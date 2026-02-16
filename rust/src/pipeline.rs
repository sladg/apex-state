//! Processing pipeline: single entry point for all state changes.
//!
//! Owns shadow state, intern table, BoolLogic registry, and reverse
//! dependency index. Processes a batch of changes, updates shadow state,
//! evaluates affected BoolLogic expressions, and returns all changes
//! (input + computed).

use crate::aggregation::{
    process_aggregation_reads, process_aggregation_writes, AggregationRegistry,
};
use crate::bool_logic::{BoolLogicNode, BoolLogicRegistry};
use crate::functions::{FunctionInput, FunctionRegistry};
use crate::graphs::Graph;
use crate::intern::InternTable;
use crate::rev_index::ReverseDependencyIndex;
use crate::router::{FullExecutionPlan, TopicRouter};
use crate::shadow::ShadowState;
use crate::validator::ValidatorInput;
use serde::{Deserialize, Serialize};
use std::collections::HashSet;

/// A single change in the input/output format.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub(crate) struct Change {
    pub path: String,
    pub value_json: String,
}

/// Validator dispatch info for JS-side execution.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub(crate) struct ValidatorDispatch {
    pub validator_id: u32,
    pub output_path: String,
    pub dependency_values: std::collections::HashMap<String, String>,
}

/// Output wrapper for processChanges (deprecated, kept for backward compat with tests).
#[derive(Serialize, Deserialize, Debug)]
#[allow(dead_code)]
pub(crate) struct ProcessResult {
    /// All changes including state and concerns (concern paths have _concerns. prefix).
    pub changes: Vec<Change>,
    /// Validators to run on JS side with their dependency values.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub validators_to_run: Vec<ValidatorDispatch>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub execution_plan: Option<FullExecutionPlan>,
}

/// Result from processChanges: orchestrates pipeline, buffers concern results for finalization.
#[derive(Serialize, Deserialize, Debug)]
pub(crate) struct PrepareResult {
    /// State changes (readonly context for JS listener execution, not for applying to valtio yet).
    pub state_changes: Vec<Change>,
    /// Validators to run on JS side with their dependency values.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub validators_to_run: Vec<ValidatorDispatch>,
    /// Pre-computed execution plan for listener dispatch.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub execution_plan: Option<FullExecutionPlan>,
    /// Whether there's work to do (validators, listeners, or concern changes to apply).
    /// If false, JS can return early without calling pipeline_finalize.
    pub has_work: bool,
}

/// Finalize result: merged changes, diffed, ready for valtio application.
#[derive(Serialize, Deserialize, Debug)]
pub(crate) struct FinalizeResult {
    /// All changes including state and concerns (concern paths have _concerns. prefix).
    pub state_changes: Vec<Change>,
}

/// Owns all WASM-internal state and orchestrates change processing.
pub(crate) struct ProcessingPipeline {
    shadow: ShadowState,
    intern: InternTable,
    registry: BoolLogicRegistry,
    rev_index: ReverseDependencyIndex,
    function_registry: FunctionRegistry,
    function_rev_index: ReverseDependencyIndex,
    aggregations: AggregationRegistry,
    sync_graph: Graph,
    flip_graph: Graph,
    router: TopicRouter,
    // Pre-allocated buffers for hot-path processing (reused across calls)
    buf_output: Vec<Change>,
    buf_sync: Vec<Change>,
    buf_flip: Vec<Change>,
    buf_affected_ids: HashSet<u32>,
    buf_concern_changes: Vec<Change>,
    buf_affected_validators: HashSet<u32>,
    // Pending changes for round-trip refactor (EP5)
    buf_pending_state_changes: Vec<Change>,
    buf_pending_concern_changes: Vec<Change>,
}

impl ProcessingPipeline {
    pub(crate) fn new() -> Self {
        Self {
            shadow: ShadowState::new(),
            intern: InternTable::new(),
            registry: BoolLogicRegistry::new(),
            rev_index: ReverseDependencyIndex::new(),
            function_registry: FunctionRegistry::new(),
            function_rev_index: ReverseDependencyIndex::new(),
            aggregations: AggregationRegistry::new(),
            sync_graph: Graph::new(),
            flip_graph: Graph::new(),
            router: TopicRouter::new(),
            buf_output: Vec::with_capacity(64),
            buf_sync: Vec::with_capacity(16),
            buf_flip: Vec::with_capacity(16),
            buf_affected_ids: HashSet::with_capacity(16),
            buf_concern_changes: Vec::with_capacity(16),
            buf_affected_validators: HashSet::with_capacity(16),
            buf_pending_state_changes: Vec::with_capacity(64),
            buf_pending_concern_changes: Vec::with_capacity(16),
        }
    }

    /// Reset the entire pipeline to a fresh state.
    pub(crate) fn reset(&mut self) {
        *self = Self::new();
    }

    /// Initialize shadow state from a JSON string.
    pub(crate) fn shadow_init(&mut self, state_json: &str) -> Result<(), String> {
        self.shadow.init(state_json)
    }

    /// Initialize shadow state from a pre-parsed serde_json::Value (no string intermediary).
    pub(crate) fn shadow_init_value(&mut self, value: serde_json::Value) -> Result<(), String> {
        self.shadow.init_value(value)
    }

    /// Register a BoolLogic expression. Returns logic_id for later cleanup.
    pub(crate) fn register_boollogic(
        &mut self,
        output_path: &str,
        tree_json: &str,
    ) -> Result<u32, String> {
        let tree: BoolLogicNode =
            serde_json::from_str(tree_json).map_err(|e| format!("BoolLogic parse error: {}", e))?;
        let id = self.registry.register(
            output_path.to_owned(),
            tree,
            &mut self.intern,
            &mut self.rev_index,
        );
        Ok(id)
    }

    /// Unregister a BoolLogic expression.
    pub(crate) fn unregister_boollogic(&mut self, logic_id: u32) {
        self.registry.unregister(logic_id, &mut self.rev_index);
    }

    /// Register a batch of aggregations.
    ///
    /// Input: JSON array of `{ "target": "...", "sources": [...] }`
    /// Output: JSON array of changes to initialize target values (read direction)
    /// Register aggregations from raw [target, source] pairs.
    ///
    /// Rust handles:
    /// - Circular dependency validation
    /// - Grouping by target for multi-source aggregations
    /// - Computing initial target values
    ///
    /// Input: JSON array of [target, source] pairs
    /// Output: JSON array of initial changes
    pub(crate) fn register_aggregation_batch(
        &mut self,
        pairs_json: &str,
    ) -> Result<String, String> {
        // Parse raw pairs
        let pairs: Vec<[String; 2]> = serde_json::from_str(pairs_json)
            .map_err(|e| format!("Aggregation pairs parse error: {}", e))?;

        // Collect all targets and sources for validation
        let mut targets = std::collections::HashSet::new();
        let mut sources = std::collections::HashSet::new();

        for [target, source] in &pairs {
            targets.insert(target.clone());
            sources.insert(source.clone());
        }

        // Validate no circular dependencies
        for target in &targets {
            if sources.contains(target) {
                return Err(format!(
                    "Circular aggregation: \"{}\" cannot be both target and source",
                    target
                ));
            }
        }

        // Group by target for multi-source aggregations
        let mut by_target: std::collections::HashMap<String, Vec<String>> =
            std::collections::HashMap::new();

        for [target, source] in pairs {
            by_target.entry(target).or_default().push(source);
        }

        // Collect all source paths for initial value computation
        let all_sources: Vec<String> = by_target
            .values()
            .flat_map(|sources| sources.iter().cloned())
            .collect();

        // Register all aggregations
        for (target, sources) in by_target {
            self.aggregations.register(target, sources);
        }

        // Compute initial values using reactive logic
        let initial_changes =
            process_aggregation_reads(&self.aggregations, &self.shadow, &all_sources);

        // Return changes as JSON
        serde_json::to_string(&initial_changes)
            .map_err(|e| format!("Failed to serialize initial changes: {}", e))
    }

    /// Unregister a batch of aggregations.
    ///
    /// Input: JSON array of target paths
    pub(crate) fn unregister_aggregation_batch(
        &mut self,
        targets_json: &str,
    ) -> Result<(), String> {
        let targets: Vec<String> = serde_json::from_str(targets_json)
            .map_err(|e| format!("Targets parse error: {}", e))?;

        for target in targets {
            self.aggregations.unregister(&target);
        }

        Ok(())
    }

    /// Register a batch of sync pairs.
    ///
    /// Registers pairs in sync graph, computes initial sync changes from shadow state,
    /// updates shadow with those changes, and returns them for valtio application.
    ///
    /// Input: JSON array of path pairs
    /// Example: `[["user.name", "profile.name"], ["user.email", "profile.email"]]`
    ///
    /// Output: JSON array of initial changes to sync all connected components
    /// Example: `[{ "path": "profile.name", "value_json": "\"alice\"" }]`
    pub(crate) fn register_sync_batch(&mut self, pairs_json: &str) -> Result<String, String> {
        let pairs: Vec<[String; 2]> = serde_json::from_str(pairs_json)
            .map_err(|e| format!("Sync pairs parse error: {}", e))?;

        // Register pairs in graph
        for pair in &pairs {
            let id1 = self.intern.intern(&pair[0]);
            let id2 = self.intern.intern(&pair[1]);
            self.sync_graph.add_edge_public(id1, id2);
        }

        // Compute initial sync changes from shadow state
        let initial_changes = self.compute_sync_initial_changes(&pairs)?;

        // Update shadow state with sync changes
        for change in &initial_changes {
            self.shadow
                .set(&change.path, &change.value_json)
                .map_err(|e| format!("Shadow update failed: {}", e))?;
        }

        // Return changes as JSON
        serde_json::to_string(&initial_changes)
            .map_err(|e| format!("Failed to serialize initial changes: {}", e))
    }

    /// Compute initial sync changes for newly registered pairs.
    ///
    /// For each connected component, finds the most common value (excluding null/undefined)
    /// and generates changes to sync all paths to that value.
    fn compute_sync_initial_changes(&self, pairs: &[[String; 2]]) -> Result<Vec<Change>, String> {
        use std::collections::{HashMap, HashSet};

        // Collect all unique paths involved
        let all_paths: HashSet<String> = pairs
            .iter()
            .flat_map(|[p1, p2]| vec![p1.clone(), p2.clone()])
            .collect();

        // Group paths by connected component
        let mut processed = HashSet::new();
        let mut components: Vec<Vec<String>> = Vec::new();

        for path in &all_paths {
            if processed.contains(path) {
                continue;
            }

            // Get component for this path
            let path_id = self
                .intern
                .get_id(path)
                .ok_or_else(|| format!("Path not interned (should not happen): {}", path))?;
            let component_ids = self.sync_graph.get_component_paths_public(path_id);

            if component_ids.is_empty() {
                continue;
            }

            // Convert IDs back to paths
            let component_paths: Vec<String> = component_ids
                .iter()
                .filter_map(|id| self.intern.get_path(*id))
                .map(|s| s.to_string())
                .collect();

            // Mark all paths in component as processed
            for p in &component_paths {
                processed.insert(p.clone());
            }

            components.push(component_paths);
        }

        // For each component, find most common value and generate sync changes
        let mut changes = Vec::new();

        for component in components {
            // Count value occurrences (excluding null/undefined)
            // Also track which path provided each value (for tie-breaking)
            let mut value_counts: HashMap<String, usize> = HashMap::new();
            let mut value_to_path: HashMap<String, String> = HashMap::new();

            for path in &component {
                if let Some(value_repr) = self.shadow.get(path) {
                    // Serialize to JSON string
                    let value_json = serde_json::to_string(&value_repr.to_json_value())
                        .unwrap_or_else(|_| "null".to_string());

                    // Skip null and undefined
                    if value_json != "null" && value_json != "undefined" {
                        *value_counts.entry(value_json.clone()).or_insert(0) += 1;

                        // Track path for this value (prefer shallowest path in case of tie)
                        value_to_path
                            .entry(value_json.clone())
                            .or_insert_with(|| path.clone());

                        // If we find a shallower path for this value, use it instead
                        if let Some(existing_path) = value_to_path.get(&value_json) {
                            let existing_depth = existing_path.matches('.').count();
                            let new_depth = path.matches('.').count();
                            if new_depth < existing_depth {
                                value_to_path.insert(value_json, path.clone());
                            }
                        }
                    }
                }
            }

            // Find most common value (with tie-breaking)
            let most_common = value_counts
                .iter()
                .max_by(|(value_a, count_a), (value_b, count_b)| {
                    // First: compare counts (higher is better)
                    match count_a.cmp(count_b) {
                        std::cmp::Ordering::Equal => {
                            // Tie: prefer value from shallowest path
                            let depth_a = value_to_path
                                .get(*value_a)
                                .map(|p| p.matches('.').count())
                                .unwrap_or(usize::MAX);
                            let depth_b = value_to_path
                                .get(*value_b)
                                .map(|p| p.matches('.').count())
                                .unwrap_or(usize::MAX);

                            // Reverse: shallower (lower depth) is better
                            depth_b.cmp(&depth_a)
                        }
                        other => other,
                    }
                })
                .map(|(value, _)| value.clone());

            if let Some(target_value) = most_common {
                // Generate changes for paths that differ from the most common value
                for path in &component {
                    let current_value = self
                        .shadow
                        .get(path)
                        .map(|v| {
                            serde_json::to_string(&v.to_json_value())
                                .unwrap_or_else(|_| "null".to_string())
                        })
                        .unwrap_or_else(|| "null".to_string());

                    if current_value != target_value {
                        changes.push(Change {
                            path: path.clone(),
                            value_json: target_value.clone(),
                        });
                    }
                }
            }
        }

        Ok(changes)
    }

    /// Unregister a batch of sync pairs.
    ///
    /// Input: JSON array of path pairs to remove
    pub(crate) fn unregister_sync_batch(&mut self, pairs_json: &str) -> Result<(), String> {
        let pairs: Vec<[String; 2]> = serde_json::from_str(pairs_json)
            .map_err(|e| format!("Sync pairs parse error: {}", e))?;

        for pair in pairs {
            let id1 = self.intern.intern(&pair[0]);
            let id2 = self.intern.intern(&pair[1]);
            self.sync_graph.remove_edge_public(id1, id2);
        }

        Ok(())
    }

    /// Register a batch of flip pairs.
    ///
    /// Input: JSON array of path pairs that should stay inverted
    /// Example: `[["checkbox1", "checkbox2"], ["toggle1", "toggle2"]]`
    pub(crate) fn register_flip_batch(&mut self, pairs_json: &str) -> Result<(), String> {
        let pairs: Vec<[String; 2]> = serde_json::from_str(pairs_json)
            .map_err(|e| format!("Flip pairs parse error: {}", e))?;

        for pair in pairs {
            let id1 = self.intern.intern(&pair[0]);
            let id2 = self.intern.intern(&pair[1]);
            self.flip_graph.add_edge_public(id1, id2);
        }

        Ok(())
    }

    /// Unregister a batch of flip pairs.
    ///
    /// Input: JSON array of path pairs to remove from flip
    pub(crate) fn unregister_flip_batch(&mut self, pairs_json: &str) -> Result<(), String> {
        let pairs: Vec<[String; 2]> = serde_json::from_str(pairs_json)
            .map_err(|e| format!("Flip pairs parse error: {}", e))?;

        for pair in pairs {
            let id1 = self.intern.intern(&pair[0]);
            let id2 = self.intern.intern(&pair[1]);
            self.flip_graph.remove_edge_public(id1, id2);
        }

        Ok(())
    }

    /// Register a batch of validators.
    ///
    /// Validators are now registered via the generic function registry.
    /// Input: JSON array of `{ "validator_id": N, "output_path": "...", "dependency_paths": [...] }`
    /// Returns: Validators to run against initial state (with dependency values from shadow).
    pub(crate) fn register_validators_batch(
        &mut self,
        validators_json: &str,
    ) -> Result<Vec<ValidatorDispatch>, String> {
        let validators: Vec<ValidatorInput> = serde_json::from_str(validators_json)
            .map_err(|e| format!("Validators parse error: {}", e))?;

        let mut initial_validators = Vec::new();

        for validator in validators {
            // Register via generic function registry
            // Validators use empty scope (they validate at the dependency path level)
            self.function_registry.register(
                validator.validator_id,
                validator.dependency_paths.clone(),
                String::new(), // Empty scope = full state
                Some(validator.output_path.clone()),
                &mut self.intern,
                &mut self.function_rev_index,
            );

            // Build initial validator dispatch by reading dependency values from shadow
            let mut dependency_values = std::collections::HashMap::new();
            for dep_path in &validator.dependency_paths {
                if let Some(value) = self.shadow.get(dep_path) {
                    let value_json = serde_json::to_string(&value.to_json_value())
                        .unwrap_or_else(|_| "null".to_owned());
                    dependency_values.insert(dep_path.clone(), value_json);
                } else {
                    dependency_values.insert(dep_path.clone(), "null".to_owned());
                }
            }

            initial_validators.push(ValidatorDispatch {
                validator_id: validator.validator_id,
                output_path: validator.output_path,
                dependency_values,
            });
        }

        Ok(initial_validators)
    }

    /// Unregister a batch of validators.
    ///
    /// Validators are now unregistered via the generic function registry.
    /// Input: JSON array of validator IDs
    pub(crate) fn unregister_validators_batch(
        &mut self,
        validator_ids_json: &str,
    ) -> Result<(), String> {
        let validator_ids: Vec<u32> = serde_json::from_str(validator_ids_json)
            .map_err(|e| format!("Validator IDs parse error: {}", e))?;

        for validator_id in validator_ids {
            self.function_registry
                .unregister(validator_id, &mut self.function_rev_index);
        }

        Ok(())
    }

    /// Register a batch of generic functions.
    ///
    /// Input: JSON array of `{ "function_id": N, "dependency_paths": [...], "scope": "...", "output_path": "..." }`
    pub(crate) fn register_functions_batch(&mut self, functions_json: &str) -> Result<(), String> {
        let functions: Vec<FunctionInput> = serde_json::from_str(functions_json)
            .map_err(|e| format!("Functions parse error: {}", e))?;

        for function in functions {
            self.function_registry.register(
                function.function_id,
                function.dependency_paths,
                function.scope,
                function.output_path,
                &mut self.intern,
                &mut self.function_rev_index,
            );
        }

        Ok(())
    }

    /// Unregister a batch of functions.
    ///
    /// Input: JSON array of function IDs
    pub(crate) fn unregister_functions_batch(
        &mut self,
        function_ids_json: &str,
    ) -> Result<(), String> {
        let function_ids: Vec<u32> = serde_json::from_str(function_ids_json)
            .map_err(|e| format!("Function IDs parse error: {}", e))?;

        for function_id in function_ids {
            self.function_registry
                .unregister(function_id, &mut self.function_rev_index);
        }

        Ok(())
    }

    /// Process sync paths into buf_sync (pre-allocated buffer).
    /// Only adds genuine changes by diffing against shadow state before pushing.
    fn process_sync_paths_into(&mut self, changes: &[Change]) {
        self.buf_sync.clear();
        for change in changes {
            let path_id = self.intern.intern(&change.path);
            let peer_ids = self.sync_graph.get_component_paths_public(path_id);
            for peer_id in peer_ids {
                if peer_id != path_id {
                    if let Some(peer_path) = self.intern.resolve(peer_id) {
                        // Check if this sync change is a no-op against current shadow state
                        let current = self.shadow.get(peer_path);
                        let new_value: crate::shadow::ValueRepr =
                            match serde_json::from_str(&change.value_json) {
                                Ok(v) => v,
                                Err(_) => {
                                    // Can't parse → treat as genuine change
                                    self.buf_sync.push(Change {
                                        path: peer_path.to_owned(),
                                        value_json: change.value_json.clone(),
                                    });
                                    continue;
                                }
                            };

                        // Only add if different from current shadow value
                        if crate::diff::is_different(&current, &new_value) {
                            self.buf_sync.push(Change {
                                path: peer_path.to_owned(),
                                value_json: change.value_json.clone(),
                            });
                        }
                    }
                }
            }
        }
    }

    /// Process flip paths into buf_flip (pre-allocated buffer).
    /// Only adds genuine changes by diffing against shadow state before pushing.
    fn process_flip_paths_into(&mut self, changes: &[Change]) {
        self.buf_flip.clear();
        for change in changes {
            let path_id = self.intern.intern(&change.path);
            let peer_ids = self.flip_graph.get_component_paths_public(path_id);
            if peer_ids.is_empty() {
                continue;
            }
            let inverted_value = match change.value_json.trim() {
                "true" => Some("false".to_owned()),
                "false" => Some("true".to_owned()),
                _ => None,
            };
            if let Some(inverted) = inverted_value {
                for peer_id in peer_ids {
                    if peer_id != path_id {
                        if let Some(peer_path) = self.intern.resolve(peer_id) {
                            // Check if this flip change is a no-op against current shadow state
                            let current = self.shadow.get(peer_path);
                            let inverted_bool = inverted == "true";
                            let new_value = crate::shadow::ValueRepr::Bool(inverted_bool);

                            // Only add if different from current shadow value
                            if crate::diff::is_different(&current, &new_value) {
                                self.buf_flip.push(Change {
                                    path: peer_path.to_owned(),
                                    value_json: inverted.clone(),
                                });
                            }
                        }
                    }
                }
            }
        }
    }

    /// Register a batch of listeners for topic-based dispatch.
    pub(crate) fn register_listeners_batch(&mut self, listeners_json: &str) -> Result<(), String> {
        self.router.register_listeners_batch(listeners_json)
    }

    /// Unregister a batch of listeners by subscriber IDs.
    pub(crate) fn unregister_listeners_batch(
        &mut self,
        subscriber_ids_json: &str,
    ) -> Result<(), String> {
        self.router.unregister_listeners_batch(subscriber_ids_json)
    }

    /// Create a dispatch plan for changes (JSON string path, used by Rust tests).
    pub(crate) fn create_dispatch_plan(&self, changes_json: &str) -> Result<String, String> {
        self.router.create_dispatch_plan_json(changes_json)
    }

    /// Create a dispatch plan from a pre-parsed Vec (serde-wasm-bindgen path).
    pub(crate) fn create_dispatch_plan_vec(
        &self,
        changes: &[Change],
    ) -> crate::router::DispatchPlan {
        self.router.create_dispatch_plan(changes)
    }

    /// Route produced changes from a depth level to downstream topics (JSON string path).
    pub(crate) fn route_produced_changes(
        &self,
        depth: u32,
        produced_changes_json: &str,
    ) -> Result<String, String> {
        self.router
            .route_produced_changes_json(depth, produced_changes_json)
    }

    /// Route produced changes from a pre-parsed Vec (serde-wasm-bindgen path).
    pub(crate) fn route_produced_changes_vec(
        &self,
        depth: u32,
        produced_changes: &[Change],
    ) -> crate::router::DispatchPlan {
        self.router.route_produced_changes(depth, produced_changes)
    }

    /// Process a batch of changes from a JSON string (legacy path, used by Rust tests).
    pub(crate) fn process_changes(&mut self, changes_json: &str) -> Result<String, String> {
        let input_changes: Vec<Change> = serde_json::from_str(changes_json)
            .map_err(|e| format!("Changes parse error: {}", e))?;
        let result = self.process_changes_vec(input_changes)?;
        serde_json::to_string(&result).map_err(|e| format!("Serialize error: {}", e))
    }

    /// Process a batch of changes from a pre-parsed Vec (serde-wasm-bindgen path).
    ///
    /// Uses pre-allocated buffers to avoid per-call allocations.
    ///
    /// Three-checkpoint diff strategy:
    /// - Checkpoint 1: Diff input changes before pipeline (early exit if all no-ops)
    /// - Checkpoint 2: Inline filtering during aggregation/sync/flip (only add genuine changes to buffers)
    /// - Checkpoint 3: JS-side diff of final queue before applyBatch (including listener changes)
    pub(crate) fn process_changes_vec(
        &mut self,
        input_changes: Vec<Change>,
    ) -> Result<ProcessResult, String> {
        // Clear pre-allocated buffers
        self.buf_output.clear();
        self.buf_sync.clear();
        self.buf_flip.clear();
        self.buf_affected_ids.clear();
        self.buf_concern_changes.clear();
        self.buf_affected_validators.clear();

        // Step 0: Diff pre-pass (always-on)
        let input_changes: Vec<Change> = {
            // Use diff engine to filter out no-op changes
            let diffed = self.diff_changes(&input_changes);
            // Early exit if all changes are no-ops
            if diffed.is_empty() {
                return Ok(ProcessResult {
                    changes: Vec::new(),
                    validators_to_run: Vec::new(),
                    execution_plan: None,
                });
            }
            diffed
        };

        // Step 1-2: Process aggregation writes (distribute target → sources)
        let changes = process_aggregation_writes(&self.aggregations, input_changes);

        // Step 3: Apply aggregated changes to shadow state and collect affected paths
        // Only process genuine changes (diff against shadow before applying)
        let mut genuine_changes = Vec::new();
        for change in &changes {
            // Check if this aggregated change is a no-op against current shadow state
            let current = self.shadow.get(&change.path);
            let new_value: crate::shadow::ValueRepr = match serde_json::from_str(&change.value_json)
            {
                Ok(v) => v,
                Err(_) => {
                    // Can't parse → treat as genuine change
                    self.shadow.set(&change.path, &change.value_json)?;
                    self.buf_output.push(change.clone());
                    genuine_changes.push(change.clone());
                    self.mark_affected_logic(&change.path);
                    continue;
                }
            };

            // Only apply and track if different from current shadow value
            if crate::diff::is_different(&current, &new_value) {
                self.shadow.set(&change.path, &change.value_json)?;
                self.buf_output.push(change.clone());
                genuine_changes.push(change.clone());
                self.mark_affected_logic(&change.path);
            }
        }

        // Step 4-5: Process sync paths and update shadow state
        // Must process ALL aggregated changes (not just genuine ones) because even if
        // a change is a no-op for path A, it might need to sync to path B that differs
        self.process_sync_paths_into(&changes);
        // Drain buf_sync to avoid borrow conflict with self.shadow/self.mark_affected_logic
        let sync_changes: Vec<Change> = self.buf_sync.drain(..).collect();
        for change in &sync_changes {
            self.shadow.set(&change.path, &change.value_json)?;
            self.mark_affected_logic(&change.path);
        }
        self.buf_output.extend_from_slice(&sync_changes);

        // Step 6-7: Process flip paths and update shadow state
        // Must process ALL aggregated changes + sync outputs (not just genuine ones)
        // because even if a change is a no-op for path A, it might need to flip to path B that differs
        let mut changes_for_flip = changes.clone();
        changes_for_flip.extend(sync_changes.clone());
        self.process_flip_paths_into(&changes_for_flip);
        let flip_changes: Vec<Change> = self.buf_flip.drain(..).collect();
        for change in &flip_changes {
            self.shadow.set(&change.path, &change.value_json)?;
            self.mark_affected_logic(&change.path);
        }
        self.buf_output.extend(flip_changes.clone());

        // Step 7.5: Process aggregation reads (sources → target recomputation)
        // After sync/flip, check if any aggregation sources changed and recompute targets
        let all_changed_paths: Vec<String> =
            self.buf_output.iter().map(|c| c.path.clone()).collect();
        let aggregation_reads =
            process_aggregation_reads(&self.aggregations, &self.shadow, &all_changed_paths);
        for change in &aggregation_reads {
            self.shadow.set(&change.path, &change.value_json)?;
            self.mark_affected_logic(&change.path);
        }
        self.buf_output.extend(aggregation_reads);

        // Step 8-9: Evaluate affected BoolLogic expressions
        for logic_id in &self.buf_affected_ids {
            if let Some(meta) = self.registry.get(*logic_id) {
                let result = meta.tree.evaluate(&self.shadow);
                self.buf_concern_changes.push(Change {
                    path: meta.output_path.clone(),
                    value_json: if result {
                        "true".to_owned()
                    } else {
                        "false".to_owned()
                    },
                });
            }
        }

        // Step 10: Collect affected validators with their dependency values
        let validators_to_run: Vec<ValidatorDispatch> = self
            .buf_affected_validators
            .iter()
            .filter_map(|&validator_id| {
                let meta = self.function_registry.get(validator_id)?;

                // Gather dependency values from shadow state
                let mut dependency_values = std::collections::HashMap::new();
                for dep_path in &meta.dependency_paths {
                    if let Some(value) = self.shadow.get(dep_path) {
                        let value_json = serde_json::to_string(&value.to_json_value())
                            .unwrap_or_else(|_| "null".to_owned());
                        dependency_values.insert(dep_path.clone(), value_json);
                    } else {
                        dependency_values.insert(dep_path.clone(), "null".to_owned());
                    }
                }

                Some(ValidatorDispatch {
                    validator_id,
                    output_path: meta.output_path.clone().unwrap_or_default(),
                    dependency_values,
                })
            })
            .collect();

        // Step 11: Create full execution plan if listeners are registered
        // Only genuine changes in buf_output (filtered inline during sync/flip/aggregation)
        let execution_plan = if self.router.has_listeners() {
            let plan = self.router.create_full_execution_plan(&self.buf_output);
            if plan.groups.is_empty() {
                None
            } else {
                Some(plan)
            }
        } else {
            None
        };

        // Move buffers into result (swap with empty vecs to avoid cloning)
        // Merge concern changes into output_changes (keep _concerns. prefix)
        let mut all_changes = std::mem::take(&mut self.buf_output);
        all_changes.extend(std::mem::take(&mut self.buf_concern_changes));

        Ok(ProcessResult {
            changes: all_changes,
            validators_to_run,
            execution_plan,
        })
    }

    /// Mark all BoolLogic expressions and validators affected by a change at the given path.
    fn mark_affected_logic(&mut self, path: &str) {
        let affected_paths = self.shadow.affected_paths(path);
        for affected_path in &affected_paths {
            let path_id = self.intern.intern(affected_path);
            // Mark affected BoolLogic
            for logic_id in self.rev_index.affected_by_path(path_id) {
                self.buf_affected_ids.insert(logic_id);
            }
            // Mark affected validators (now in function registry)
            for validator_id in self.function_rev_index.affected_by_path(path_id) {
                self.buf_affected_validators.insert(validator_id);
            }
        }
        let path_id = self.intern.intern(path);
        // Mark affected BoolLogic
        for logic_id in self.rev_index.affected_by_path(path_id) {
            self.buf_affected_ids.insert(logic_id);
        }
        // Mark affected validators (now in function registry)
        for validator_id in self.function_rev_index.affected_by_path(path_id) {
            self.buf_affected_validators.insert(validator_id);
        }
    }

    /// Dump shadow state as JSON (debug/testing).
    pub(crate) fn shadow_dump(&self) -> String {
        self.shadow.dump()
    }

    /// Get a value from shadow state at path (debug/testing).
    pub(crate) fn shadow_get(&self, path: &str) -> Option<String> {
        self.shadow.get(path).map(|v| {
            serde_json::to_string(&v.to_json_value()).unwrap_or_else(|_| "null".to_owned())
        })
    }

    /// Number of interned paths (debug/testing).
    pub(crate) fn intern_count(&self) -> u32 {
        self.intern.count()
    }

    /// Diff a batch of changes against shadow state (WASM-028).
    ///
    /// Returns only changes where the value differs from the current shadow state.
    /// Primitives are compared by value, objects/arrays always pass through.
    pub(crate) fn diff_changes(&self, changes: &[Change]) -> Vec<Change> {
        crate::diff::diff_changes(&self.shadow, changes)
    }

    /// Process changes through pipeline: aggregation → sync → flip → BoolLogic → validators.
    ///
    /// Updates shadow state during processing (needed for BoolLogic evaluation).
    /// Buffers BoolLogic concern results in buf_pending_concern_changes.
    /// Creates execution plan for listeners and collects validators to run.
    /// Returns: { state_changes, validators_to_run, execution_plan, has_work }
    ///
    /// After JS executes listeners/validators, call pipeline_finalize() with their results.
    pub(crate) fn prepare_changes(
        &mut self,
        input_changes: Vec<Change>,
    ) -> Result<PrepareResult, String> {
        // Clear buffers
        self.buf_output.clear();
        self.buf_sync.clear();
        self.buf_flip.clear();
        self.buf_affected_ids.clear();
        self.buf_concern_changes.clear();
        self.buf_affected_validators.clear();
        self.buf_pending_state_changes.clear();
        self.buf_pending_concern_changes.clear();

        // Step 0: Diff pre-pass (always-on)
        let input_changes: Vec<Change> = {
            let diffed = self.diff_changes(&input_changes);
            if diffed.is_empty() {
                return Ok(PrepareResult {
                    state_changes: Vec::new(),
                    validators_to_run: Vec::new(),
                    execution_plan: None,
                    has_work: false,
                });
            }
            diffed
        };

        // Step 1-2: Process aggregation writes
        let changes = process_aggregation_writes(&self.aggregations, input_changes);

        // Step 3: Apply aggregated changes to shadow state
        for change in &changes {
            let current = self.shadow.get(&change.path);
            let new_value: crate::shadow::ValueRepr = match serde_json::from_str(&change.value_json)
            {
                Ok(v) => v,
                Err(_) => {
                    self.shadow.set(&change.path, &change.value_json)?;
                    self.buf_output.push(change.clone());
                    self.mark_affected_logic(&change.path);
                    continue;
                }
            };

            if crate::diff::is_different(&current, &new_value) {
                self.shadow.set(&change.path, &change.value_json)?;
                self.buf_output.push(change.clone());
                self.mark_affected_logic(&change.path);
            }
        }

        // Step 4-5: Process sync paths
        self.process_sync_paths_into(&changes);
        let sync_changes: Vec<Change> = self.buf_sync.drain(..).collect();
        for change in &sync_changes {
            self.shadow.set(&change.path, &change.value_json)?;
            self.mark_affected_logic(&change.path);
        }
        self.buf_output.extend_from_slice(&sync_changes);

        // Step 6-7: Process flip paths
        let mut changes_for_flip = changes.clone();
        changes_for_flip.extend(sync_changes.clone());
        self.process_flip_paths_into(&changes_for_flip);
        let flip_changes: Vec<Change> = self.buf_flip.drain(..).collect();
        for change in &flip_changes {
            self.shadow.set(&change.path, &change.value_json)?;
            self.mark_affected_logic(&change.path);
        }
        self.buf_output.extend(flip_changes);

        // Step 8-9: Evaluate affected BoolLogic expressions
        for logic_id in &self.buf_affected_ids {
            if let Some(meta) = self.registry.get(*logic_id) {
                let result = meta.tree.evaluate(&self.shadow);
                // Keep full path with _concerns. prefix (strip happens in finalize)
                self.buf_concern_changes.push(Change {
                    path: meta.output_path.clone(),
                    value_json: if result {
                        "true".to_owned()
                    } else {
                        "false".to_owned()
                    },
                });
            }
        }

        // Step 10: Collect affected validators
        let validators_to_run: Vec<ValidatorDispatch> = self
            .buf_affected_validators
            .iter()
            .filter_map(|&validator_id| {
                let meta = self.function_registry.get(validator_id)?;

                let mut dependency_values = std::collections::HashMap::new();
                for dep_path in &meta.dependency_paths {
                    if let Some(value) = self.shadow.get(dep_path) {
                        let value_json = serde_json::to_string(&value.to_json_value())
                            .unwrap_or_else(|_| "null".to_owned());
                        dependency_values.insert(dep_path.clone(), value_json);
                    } else {
                        dependency_values.insert(dep_path.clone(), "null".to_owned());
                    }
                }

                // Keep full path with _concerns. prefix (strip happens in finalize)
                Some(ValidatorDispatch {
                    validator_id,
                    output_path: meta.output_path.clone().unwrap_or_default(),
                    dependency_values,
                })
            })
            .collect();

        // Step 11: Create execution plan
        let execution_plan = if self.router.has_listeners() {
            let plan = self.router.create_full_execution_plan(&self.buf_output);
            if plan.groups.is_empty() {
                None
            } else {
                Some(plan)
            }
        } else {
            None
        };

        // Store pending changes for finalize
        self.buf_pending_state_changes = std::mem::take(&mut self.buf_output);
        self.buf_pending_concern_changes = std::mem::take(&mut self.buf_concern_changes);

        // Determine if there's work to do:
        // - If there are validators or listeners, JS needs to execute them
        // - If there are concern changes (BoolLogic results), they need to be applied
        // - If there are state changes, they need to be finalized
        let has_work = !validators_to_run.is_empty()
            || execution_plan.is_some()
            || !self.buf_pending_concern_changes.is_empty()
            || !self.buf_pending_state_changes.is_empty();

        // Return readonly context for JS listener execution
        Ok(PrepareResult {
            state_changes: self.buf_pending_state_changes.clone(),
            validators_to_run,
            execution_plan,
            has_work,
        })
    }

    /// Finalize pipeline by merging JS-produced changes with buffered results.
    ///
    /// Accepts js_changes from listeners and validators (state + concern paths mixed).
    /// Partitions by _concerns. prefix, merges with buffered BoolLogic results,
    /// diffs all changes against shadow state (filters no-ops), updates shadow.
    /// Returns: { state_changes, concern_changes } ready for valtio application.
    pub(crate) fn pipeline_finalize(
        &mut self,
        js_changes: Vec<Change>,
    ) -> Result<FinalizeResult, String> {
        // Partition js_changes: paths starting with _concerns. go to concern bucket, rest to state
        let mut js_state_changes = Vec::new();
        let mut js_concern_changes = Vec::new();

        for change in js_changes {
            if change.path.starts_with("_concerns.") {
                // Strip prefix for concern bucket
                let stripped_path = change.path["_concerns.".len()..].to_owned();
                js_concern_changes.push(Change {
                    path: stripped_path,
                    value_json: change.value_json,
                });
            } else {
                js_state_changes.push(change);
            }
        }

        // --- State changes ---
        // Phase-1 buffered changes were already diffed and applied to shadow in phase 1.
        // Re-diffing would drop them as no-ops. Pass through directly.
        let mut genuine_state = std::mem::take(&mut self.buf_pending_state_changes);

        // Only diff JS-produced state changes (from listeners) against current shadow.
        let diffed_js_state = self.diff_changes(&js_state_changes);
        for change in &diffed_js_state {
            self.shadow.set(&change.path, &change.value_json)?;
        }
        genuine_state.extend(diffed_js_state);

        // --- Concern changes ---
        // Phase-1 BoolLogic results: already evaluated, pass through.
        let pending_concerns = std::mem::take(&mut self.buf_pending_concern_changes);
        let mut genuine_concerns: Vec<Change> = pending_concerns
            .into_iter()
            .map(|c| {
                if c.path.starts_with("_concerns.") {
                    c
                } else {
                    Change {
                        path: format!("_concerns.{}", c.path),
                        value_json: c.value_json,
                    }
                }
            })
            .collect();

        // Update shadow for phase-1 concern changes
        for change in &genuine_concerns {
            self.shadow.set(&change.path, &change.value_json)?;
        }

        // Only diff JS-produced concern changes (from validators) against current shadow.
        let js_concerns_prefixed: Vec<Change> = js_concern_changes
            .into_iter()
            .map(|c| Change {
                path: format!("_concerns.{}", c.path),
                value_json: c.value_json,
            })
            .collect();
        let diffed_js_concerns = self.diff_changes(&js_concerns_prefixed);
        for change in &diffed_js_concerns {
            self.shadow.set(&change.path, &change.value_json)?;
        }
        genuine_concerns.extend(diffed_js_concerns);

        // Merge state and concern changes into single array (keep _concerns. prefix)
        let mut all_changes = genuine_state;
        all_changes.extend(genuine_concerns);

        Ok(FinalizeResult {
            state_changes: all_changes,
        })
    }
}

impl Default for ProcessingPipeline {
    fn default() -> Self {
        Self::new()
    }
}

// Re-export for lib.rs boundary
impl Change {
    pub fn new(path: String, value_json: String) -> Self {
        Self { path, value_json }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_pipeline() -> ProcessingPipeline {
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"user": {"role": "guest", "age": 20, "email": "test@test.com"}}"#)
            .unwrap();
        p
    }

    // --- basic change processing ---

    #[test]
    fn process_echoes_input_changes() {
        let mut p = make_pipeline();
        let result = p
            .process_changes(r#"[{"path": "user.role", "value_json": "\"admin\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();
        assert_eq!(parsed.changes.len(), 1);
        assert_eq!(parsed.changes[0].path, "user.role");
        assert_eq!(parsed.changes[0].value_json, r#""admin""#);
    }

    #[test]
    fn process_updates_shadow_state() {
        let mut p = make_pipeline();
        p.process_changes(r#"[{"path": "user.role", "value_json": "\"admin\""}]"#)
            .unwrap();
        assert_eq!(p.shadow_get("user.role").unwrap(), r#""admin""#);
    }

    #[test]
    fn process_multiple_changes() {
        let mut p = make_pipeline();
        let result = p
            .process_changes(
                r#"[
                    {"path": "user.role", "value_json": "\"admin\""},
                    {"path": "user.age", "value_json": "30"}
                ]"#,
            )
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();
        assert_eq!(parsed.changes.len(), 2);
        assert_eq!(p.shadow_get("user.age").unwrap(), "30");
    }

    #[test]
    fn process_empty_changes() {
        let mut p = make_pipeline();
        let result = p.process_changes("[]").unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();
        assert!(parsed.changes.is_empty());
    }

    // --- BoolLogic evaluation ---

    #[test]
    fn process_evaluates_affected_boollogic() {
        let mut p = make_pipeline();

        // Register: disable email field when role is admin
        p.register_boollogic(
            "_concerns.user.email.disabledWhen",
            r#"{"IS_EQUAL": ["user.role", "admin"]}"#,
        )
        .unwrap();

        // Change role to admin
        let result = p
            .process_changes(r#"[{"path": "user.role", "value_json": "\"admin\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should have 1 state change + 1 concern change
        assert_eq!(parsed.changes.len(), 1);
        assert_eq!(parsed.changes[0].path, "user.role");

        assert_eq!(parsed.concern_changes.len(), 1);
        let bl_change = &parsed.concern_changes[0];
        assert_eq!(bl_change.path, "_concerns.user.email.disabledWhen");
        assert_eq!(bl_change.value_json, "true");
    }

    #[test]
    fn process_boollogic_evaluates_false() {
        let mut p = make_pipeline();

        p.register_boollogic(
            "_concerns.user.email.disabledWhen",
            r#"{"IS_EQUAL": ["user.role", "admin"]}"#,
        )
        .unwrap();

        // Change role to editor (not admin)
        let result = p
            .process_changes(r#"[{"path": "user.role", "value_json": "\"editor\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        assert_eq!(parsed.changes.len(), 1);
        assert_eq!(parsed.concern_changes.len(), 1);
        let bl_change = &parsed.concern_changes[0];
        assert_eq!(bl_change.path, "_concerns.user.email.disabledWhen");
        assert_eq!(bl_change.value_json, "false");
    }

    #[test]
    fn process_unrelated_change_no_boollogic() {
        let mut p = make_pipeline();

        p.register_boollogic(
            "_concerns.user.email.disabledWhen",
            r#"{"IS_EQUAL": ["user.role", "admin"]}"#,
        )
        .unwrap();

        // Change age (unrelated to the BoolLogic)
        let result = p
            .process_changes(r#"[{"path": "user.age", "value_json": "25"}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Only the echoed input, no BoolLogic evaluation
        assert_eq!(parsed.changes.len(), 1);
        assert_eq!(parsed.changes[0].path, "user.age");
    }

    #[test]
    fn process_multiple_concerns_same_dependency() {
        let mut p = make_pipeline();

        p.register_boollogic(
            "_concerns.user.email.disabledWhen",
            r#"{"IS_EQUAL": ["user.role", "admin"]}"#,
        )
        .unwrap();
        p.register_boollogic(
            "_concerns.user.email.readonlyWhen",
            r#"{"IS_EQUAL": ["user.role", "admin"]}"#,
        )
        .unwrap();
        p.register_boollogic(
            "_concerns.user.name.visibleWhen",
            r#"{"EXISTS": "user.role"}"#,
        )
        .unwrap();

        let result = p
            .process_changes(r#"[{"path": "user.role", "value_json": "\"admin\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // 1 input change + 3 concern changes
        assert_eq!(parsed.changes.len(), 1);
        assert_eq!(parsed.concern_changes.len(), 3);

        let bl_paths: Vec<&str> = parsed
            .concern_changes
            .iter()
            .map(|c| c.path.as_str())
            .collect();
        assert!(bl_paths.contains(&"_concerns.user.email.disabledWhen"));
        assert!(bl_paths.contains(&"_concerns.user.email.readonlyWhen"));
        assert!(bl_paths.contains(&"_concerns.user.name.visibleWhen"));
    }

    #[test]
    fn process_complex_boollogic() {
        let mut p = make_pipeline();

        // AND: role=admin AND age >= 18
        p.register_boollogic(
            "_concerns.user.panel.visibleWhen",
            r#"{"AND": [{"IS_EQUAL": ["user.role", "admin"]}, {"GTE": ["user.age", 18]}]}"#,
        )
        .unwrap();

        // Set role=admin (age is already 20, so both conditions met)
        let result = p
            .process_changes(r#"[{"path": "user.role", "value_json": "\"admin\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let bl = parsed
            .concern_changes
            .iter()
            .find(|c| c.path.contains("visibleWhen"))
            .unwrap();
        assert_eq!(bl.value_json, "true");
    }

    #[test]
    fn process_nested_object_update() {
        let mut p = make_pipeline();

        p.register_boollogic(
            "_concerns.user.email.disabledWhen",
            r#"{"IS_EQUAL": ["user.role", "admin"]}"#,
        )
        .unwrap();

        // Replace entire user object
        let result = p
            .process_changes(
                r#"[{"path": "user", "value_json": "{\"role\": \"admin\", \"age\": 30}"}]"#,
            )
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should trigger BoolLogic since user.role is a descendant of user
        let bl = parsed
            .concern_changes
            .iter()
            .find(|c| c.path.contains("disabledWhen"));
        assert!(bl.is_some());
        assert_eq!(bl.unwrap().value_json, "true");
    }

    // --- registration lifecycle ---

    #[test]
    fn register_and_unregister() {
        let mut p = make_pipeline();

        let id = p
            .register_boollogic(
                "_concerns.x.disabledWhen",
                r#"{"IS_EQUAL": ["user.role", "admin"]}"#,
            )
            .unwrap();

        // Should produce BoolLogic output
        let result = p
            .process_changes(r#"[{"path": "user.role", "value_json": "\"admin\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();
        assert_eq!(parsed.changes.len(), 1);
        assert_eq!(parsed.concern_changes.len(), 1);

        // Unregister
        p.unregister_boollogic(id);

        // Now no BoolLogic output
        let result = p
            .process_changes(r#"[{"path": "user.role", "value_json": "\"editor\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();
        assert_eq!(parsed.changes.len(), 1);
        assert_eq!(parsed.concern_changes.len(), 0);
    }

    #[test]
    fn register_invalid_json_fails() {
        let mut p = make_pipeline();
        assert!(p.register_boollogic("out", "not json").is_err());
    }

    #[test]
    fn process_invalid_changes_json_fails() {
        let mut p = make_pipeline();
        assert!(p.process_changes("not json").is_err());
    }

    // --- shadow state init ---

    #[test]
    fn shadow_init_and_dump() {
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"a": 1, "b": "hello"}"#).unwrap();
        let dump = p.shadow_dump();
        let v: serde_json::Value = serde_json::from_str(&dump).unwrap();
        assert_eq!(v["a"], 1);
        assert_eq!(v["b"], "hello");
    }

    #[test]
    fn shadow_get_returns_value() {
        let p = make_pipeline();
        assert_eq!(p.shadow_get("user.role").unwrap(), r#""guest""#);
        assert_eq!(p.shadow_get("user.age").unwrap(), "20");
    }

    #[test]
    fn shadow_get_missing_returns_none() {
        let p = make_pipeline();
        assert!(p.shadow_get("missing.path").is_none());
    }

    // --- aggregation writes ---

    #[test]
    fn aggregation_distributes_to_sources() {
        let mut p = make_pipeline();

        // Register aggregation: allUsers -> [user1, user2, user3]
        p.register_aggregation_batch(
            r#"[{"target": "allUsers", "sources": ["user1", "user2", "user3"]}]"#,
        )
        .unwrap();

        // Write to aggregation target
        let result = p
            .process_changes(r#"[{"path": "allUsers", "value_json": "\"alice\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should have 3 distributed changes (no original target change)
        assert_eq!(parsed.changes.len(), 3);

        let paths: Vec<&str> = parsed.changes.iter().map(|c| c.path.as_str()).collect();
        assert!(paths.contains(&"user1"));
        assert!(paths.contains(&"user2"));
        assert!(paths.contains(&"user3"));

        // All should have the same value
        for change in &parsed.changes {
            assert_eq!(change.value_json, "\"alice\"");
        }
    }

    #[test]
    fn aggregation_removes_target_change() {
        let mut p = make_pipeline();

        p.register_aggregation_batch(
            r#"[{"target": "form.allChecked", "sources": ["item1", "item2"]}]"#,
        )
        .unwrap();

        let result = p
            .process_changes(r#"[{"path": "form.allChecked", "value_json": "true"}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should have 2 distributed changes, NOT the original form.allChecked
        assert_eq!(parsed.changes.len(), 2);
        for change in &parsed.changes {
            assert_ne!(change.path, "form.allChecked");
        }
    }

    #[test]
    fn aggregation_with_child_path() {
        let mut p = make_pipeline();

        p.register_aggregation_batch(r#"[{"target": "allUsers", "sources": ["user1", "user2"]}]"#)
            .unwrap();

        // Write to a child path of the aggregation target
        let result = p
            .process_changes(
                r#"[{"path": "allUsers.email", "value_json": "\"test@example.com\""}]"#,
            )
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should distribute to both users with the child path appended
        assert_eq!(parsed.changes.len(), 2);

        let paths: Vec<&str> = parsed.changes.iter().map(|c| c.path.as_str()).collect();
        assert!(paths.contains(&"user1.email"));
        assert!(paths.contains(&"user2.email"));
    }

    #[test]
    fn aggregation_with_multiple_aggregations() {
        let mut p = make_pipeline();

        p.register_aggregation_batch(
            r#"[
                {"target": "allUsers", "sources": ["user1", "user2"]},
                {"target": "allItems", "sources": ["item1", "item2", "item3"]}
            ]"#,
        )
        .unwrap();

        let result = p
            .process_changes(
                r#"[
                    {"path": "allUsers", "value_json": "\"alice\""},
                    {"path": "allItems", "value_json": "42"}
                ]"#,
            )
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should have 2 + 3 = 5 distributed changes (no original targets)
        assert_eq!(parsed.changes.len(), 5);

        // Check users
        let user_changes: Vec<_> = parsed
            .changes
            .iter()
            .filter(|c| c.path.starts_with("user"))
            .collect();
        assert_eq!(user_changes.len(), 2);
        for change in user_changes {
            assert_eq!(change.value_json, "\"alice\"");
        }

        // Check items
        let item_changes: Vec<_> = parsed
            .changes
            .iter()
            .filter(|c| c.path.starts_with("item"))
            .collect();
        assert_eq!(item_changes.len(), 3);
        for change in item_changes {
            assert_eq!(change.value_json, "42");
        }
    }

    #[test]
    fn aggregation_unregister() {
        let mut p = make_pipeline();

        p.register_aggregation_batch(r#"[{"target": "allUsers", "sources": ["user1", "user2"]}]"#)
            .unwrap();

        // After unregister, aggregation should not apply
        p.unregister_aggregation_batch(r#"["allUsers"]"#).unwrap();

        let result = p
            .process_changes(r#"[{"path": "allUsers", "value_json": "\"alice\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should have the original change, not distributed
        assert_eq!(parsed.changes.len(), 1);
        assert_eq!(parsed.changes[0].path, "allUsers");
    }

    // --- sync graph tests ---

    #[test]
    fn register_sync_batch() {
        let mut p = make_pipeline();
        let result = p.register_sync_batch(
            r#"[["user.name", "profile.name"], ["user.email", "profile.email"]]"#,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn register_sync_batch_invalid_json() {
        let mut p = make_pipeline();
        let result = p.register_sync_batch("not json");
        assert!(result.is_err());
    }

    #[test]
    fn unregister_sync_batch() {
        let mut p = make_pipeline();
        p.register_sync_batch(r#"[["user.name", "profile.name"]]"#)
            .unwrap();
        let result = p.unregister_sync_batch(r#"[["user.name", "profile.name"]]"#);
        assert!(result.is_ok());
    }

    // Note: sync propagation tests are in Phase 2 (WASM-015) when process_sync_paths
    // is integrated into process_changes. For now, we only test registration/unregistration.
    //
    // #[test]
    // fn sync_propagates_change_to_peers() {
    //     // TODO: Implement in WASM-015: processChanges() Phase 2
    // }
    //
    // #[test]
    // fn sync_with_multiple_peers() {
    //     // TODO: Implement in WASM-015: processChanges() Phase 2
    // }

    #[test]
    fn sync_isolated_paths_no_propagation() {
        let mut p = make_pipeline();
        p.shadow_init(r#"{"x": "X", "y": "Y"}"#).unwrap();

        // No sync registered
        let result = p
            .process_changes(r#"[{"path": "x", "value_json": "\"Z\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should only have input change, no sync propagation
        assert_eq!(parsed.changes.len(), 1);
        assert_eq!(parsed.changes[0].path, "x");
    }

    // --- flip graph tests ---

    #[test]
    fn register_flip_batch() {
        let mut p = make_pipeline();
        let result =
            p.register_flip_batch(r#"[["checkbox1", "checkbox2"], ["toggle1", "toggle2"]]"#);
        assert!(result.is_ok());
    }

    #[test]
    fn register_flip_batch_invalid_json() {
        let mut p = make_pipeline();
        let result = p.register_flip_batch("not json");
        assert!(result.is_err());
    }

    #[test]
    fn unregister_flip_batch() {
        let mut p = make_pipeline();
        p.register_flip_batch(r#"[["checkbox1", "checkbox2"]]"#)
            .unwrap();
        let result = p.unregister_flip_batch(r#"[["checkbox1", "checkbox2"]]"#);
        assert!(result.is_ok());
    }

    #[test]
    fn flip_inverts_true_to_false() {
        let mut p = make_pipeline();
        // Start with opposite values so the change is NOT a no-op
        p.shadow_init(r#"{"isVisible": false, "isHidden": true}"#)
            .unwrap();
        p.register_flip_batch(r#"[["isVisible", "isHidden"]]"#)
            .unwrap();

        let result = p
            .process_changes(r#"[{"path": "isVisible", "value_json": "true"}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should have input change + inverted flip change
        assert_eq!(parsed.changes.len(), 2);
        assert_eq!(parsed.changes[0].path, "isVisible");
        assert_eq!(parsed.changes[0].value_json, "true");
        assert_eq!(parsed.changes[1].path, "isHidden");
        assert_eq!(parsed.changes[1].value_json, "false");
    }

    #[test]
    fn flip_inverts_false_to_true() {
        let mut p = make_pipeline();
        // Start with opposite values so the change is NOT a no-op
        p.shadow_init(r#"{"isVisible": false, "isHidden": true}"#)
            .unwrap();
        p.register_flip_batch(r#"[["isVisible", "isHidden"]]"#)
            .unwrap();

        let result = p
            .process_changes(r#"[{"path": "isHidden", "value_json": "false"}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should have input change + inverted flip change
        assert_eq!(parsed.changes.len(), 2);
        assert_eq!(parsed.changes[0].path, "isHidden");
        assert_eq!(parsed.changes[0].value_json, "false");
        assert_eq!(parsed.changes[1].path, "isVisible");
        assert_eq!(parsed.changes[1].value_json, "true");
    }

    #[test]
    fn flip_with_non_boolean_passes_through() {
        let mut p = make_pipeline();
        p.shadow_init(r#"{"enabled": true, "disabled": false}"#)
            .unwrap();
        p.register_flip_batch(r#"[["enabled", "disabled"]]"#)
            .unwrap();

        // Try to set a non-boolean value (should pass through unchanged)
        let result = p
            .process_changes(r#"[{"path": "enabled", "value_json": "\"string value\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should only have the input change, no flip
        assert_eq!(parsed.changes.len(), 1);
        assert_eq!(parsed.changes[0].path, "enabled");
        assert_eq!(parsed.changes[0].value_json, "\"string value\"");
    }

    #[test]
    fn flip_multiple_peers() {
        let mut p = make_pipeline();
        // Start with a=false so changing to true is NOT a no-op
        p.shadow_init(r#"{"a": false, "b": true, "c": true}"#)
            .unwrap();
        p.register_flip_batch(r#"[["a", "b"], ["a", "c"]]"#)
            .unwrap();

        let result = p
            .process_changes(r#"[{"path": "a", "value_json": "true"}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should have input change + 2 inverted flip changes for b and c
        assert_eq!(parsed.changes.len(), 3);
        assert_eq!(parsed.changes[0].path, "a");
        assert_eq!(parsed.changes[0].value_json, "true");

        // Check that both peers got false
        let paths: Vec<&str> = parsed.changes[1..]
            .iter()
            .map(|c| c.path.as_str())
            .collect();
        assert!(paths.contains(&"b"));
        assert!(paths.contains(&"c"));
        for change in &parsed.changes[1..] {
            assert_eq!(change.value_json, "false");
        }
    }

    #[test]
    fn flip_isolated_paths_no_propagation() {
        let mut p = make_pipeline();
        // Start with x=false so changing to true is NOT a no-op
        p.shadow_init(r#"{"x": false, "y": false}"#).unwrap();

        // No flip registered
        let result = p
            .process_changes(r#"[{"path": "x", "value_json": "true"}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should only have input change, no flip propagation
        assert_eq!(parsed.changes.len(), 1);
        assert_eq!(parsed.changes[0].path, "x");
    }

    #[test]
    fn flip_with_number_value_passes_through() {
        let mut p = make_pipeline();
        p.shadow_init(r#"{"count1": 5, "count2": 10}"#).unwrap();
        p.register_flip_batch(r#"[["count1", "count2"]]"#).unwrap();

        let result = p
            .process_changes(r#"[{"path": "count1", "value_json": "42"}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should only have the input change, no flip (number is not boolean)
        assert_eq!(parsed.changes.len(), 1);
        assert_eq!(parsed.changes[0].path, "count1");
        assert_eq!(parsed.changes[0].value_json, "42");
    }

    // --- Full pipeline integration tests (WASM-015) ---

    #[test]
    fn full_pipeline_aggregation_only() {
        let mut p = make_pipeline();

        p.register_aggregation_batch(r#"[{"target": "allUsers", "sources": ["user1", "user2"]}]"#)
            .unwrap();

        let result = p
            .process_changes(r#"[{"path": "allUsers", "value_json": "\"alice\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should have 2 distributed changes (no original target)
        assert_eq!(parsed.changes.len(), 2);
        let paths: Vec<&str> = parsed.changes.iter().map(|c| c.path.as_str()).collect();
        assert!(paths.contains(&"user1"));
        assert!(paths.contains(&"user2"));
    }

    #[test]
    fn full_pipeline_sync_only() {
        let mut p = make_pipeline();
        p.shadow_init(r#"{"user.name": "alice", "profile.name": "bob"}"#)
            .unwrap();

        p.register_sync_batch(r#"[["user.name", "profile.name"]]"#)
            .unwrap();

        let result = p
            .process_changes(r#"[{"path": "user.name", "value_json": "\"charlie\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should have input change + sync propagation
        assert_eq!(parsed.changes.len(), 2);
        assert_eq!(parsed.changes[0].path, "user.name");
        assert_eq!(parsed.changes[0].value_json, r#""charlie""#);
        assert_eq!(parsed.changes[1].path, "profile.name");
        assert_eq!(parsed.changes[1].value_json, r#""charlie""#);
    }

    #[test]
    fn full_pipeline_flip_only() {
        let mut p = make_pipeline();
        // Start with opposite values so change is NOT a no-op
        p.shadow_init(r#"{"isVisible": false, "isHidden": true}"#)
            .unwrap();

        p.register_flip_batch(r#"[["isVisible", "isHidden"]]"#)
            .unwrap();

        let result = p
            .process_changes(r#"[{"path": "isVisible", "value_json": "true"}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should have input change + flip inversion
        assert_eq!(parsed.changes.len(), 2);
        assert_eq!(parsed.changes[0].path, "isVisible");
        assert_eq!(parsed.changes[0].value_json, "true");
        assert_eq!(parsed.changes[1].path, "isHidden");
        assert_eq!(parsed.changes[1].value_json, "false");
    }

    #[test]
    fn full_pipeline_aggregation_sync_flip() {
        let mut p = ProcessingPipeline::new();
        p.shadow_init(
            r#"{
                "allUsers": null,
                "user1": {"status": true},
                "user2": {"status": false},
                "profile1": {"status": null},
                "profile2": {"status": null}
            }"#,
        )
        .unwrap();

        // Register aggregation
        p.register_aggregation_batch(r#"[{"target": "allUsers", "sources": ["user1", "user2"]}]"#)
            .unwrap();

        // Register sync: user.status <-> profile.status
        p.register_sync_batch(
            r#"[["user1.status", "profile1.status"], ["user2.status", "profile2.status"]]"#,
        )
        .unwrap();

        // Register flip: user1.status <-> user2.status
        p.register_flip_batch(r#"[["user1.status", "user2.status"]]"#)
            .unwrap();

        // Write to aggregation target (will distribute to user1, user2)
        let result = p
            .process_changes(r#"[{"path": "allUsers.status", "value_json": "true"}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Expected pipeline:
        // 1. Aggregation: allUsers.status → user1.status, user2.status
        // 2. Sync: user1.status → profile1.status, user2.status → profile2.status
        // 3. Flip: user1.status true → user2.status false (overrides aggregation)
        //
        // Detailed breakdown:
        // - Input: allUsers.status = true
        // - Agg output: user1.status = true, user2.status = true
        // - Sync propagation: profile1.status = true, profile2.status = true
        // - Flip: user2.status = false (inverted from user1.status = true)
        // Total: 5 changes (2 agg + 2 sync + 1 flip override)

        // Find paths in output
        let paths: Vec<&str> = parsed.changes.iter().map(|c| c.path.as_str()).collect();

        // Check that key paths are present
        assert!(paths.contains(&"user1.status"), "Missing user1.status");
        assert!(paths.contains(&"user2.status"), "Missing user2.status");
        assert!(
            paths.contains(&"profile1.status"),
            "Missing profile1.status"
        );
        assert!(
            paths.contains(&"profile2.status"),
            "Missing profile2.status"
        );
    }

    #[test]
    fn full_pipeline_with_boollogic() {
        let mut p = make_pipeline();

        // Register sync
        p.register_sync_batch(r#"[["user.role", "profile.role"]]"#)
            .unwrap();

        // Register BoolLogic that depends on synced field
        p.register_boollogic(
            "_concerns.user.email.disabledWhen",
            r#"{"IS_EQUAL": ["profile.role", "admin"]}"#,
        )
        .unwrap();

        // Change user.role (will sync to profile.role, then trigger BoolLogic)
        let result = p
            .process_changes(r#"[{"path": "user.role", "value_json": "\"admin\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should have: input + sync (state changes) + BoolLogic (concern change)
        assert_eq!(parsed.changes.len(), 2);
        assert_eq!(parsed.changes[0].path, "user.role");
        assert_eq!(parsed.changes[1].path, "profile.role");
        assert_eq!(parsed.concern_changes.len(), 1);
        let bl = &parsed.concern_changes[0];
        assert_eq!(bl.path, "_concerns.user.email.disabledWhen");
        assert_eq!(bl.value_json, "true");
    }

    #[test]
    fn full_pipeline_multiple_boollogics_from_sync() {
        let mut p = make_pipeline();

        p.register_sync_batch(r#"[["flag1", "flag2"]]"#).unwrap();

        p.register_boollogic(
            "_concerns.field1.visibleWhen",
            r#"{"IS_EQUAL": ["flag1", true]}"#,
        )
        .unwrap();
        p.register_boollogic(
            "_concerns.field2.visibleWhen",
            r#"{"IS_EQUAL": ["flag2", true]}"#,
        )
        .unwrap();

        let result = p
            .process_changes(r#"[{"path": "flag1", "value_json": "true"}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Input + sync (state changes) + 2 BoolLogics (concern changes)
        assert_eq!(parsed.changes.len(), 2);
        assert_eq!(parsed.concern_changes.len(), 2);

        let bl_paths: Vec<&str> = parsed
            .concern_changes
            .iter()
            .map(|c| c.path.as_str())
            .collect();
        assert!(bl_paths.contains(&"_concerns.field1.visibleWhen"));
        assert!(bl_paths.contains(&"_concerns.field2.visibleWhen"));

        for change in &parsed.concern_changes {
            assert_eq!(change.value_json, "true");
        }
    }

    #[test]
    fn full_pipeline_sync_then_flip_then_boollogic() {
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"a": true, "b": true, "c": false}"#)
            .unwrap();

        // a <-> b (sync)
        p.register_sync_batch(r#"[["a", "b"]]"#).unwrap();

        // a <-> c (flip)
        p.register_flip_batch(r#"[["a", "c"]]"#).unwrap();

        // BoolLogic on c
        p.register_boollogic("_concerns.x.disabledWhen", r#"{"IS_EQUAL": ["c", false]}"#)
            .unwrap();

        // Set a = false
        let result = p
            .process_changes(r#"[{"path": "a", "value_json": "false"}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Pipeline: a=false → b=false (sync) → c=true (flip) → concern=false (BoolLogic)
        // (c flips because a changed: false -> inverted = true, but wait...)
        // Actually: a=false, sync makes b=false, flip inverts a for c: c=true becomes c=false
        // Then BoolLogic: c==false? yes → true

        let paths: Vec<&str> = parsed.changes.iter().map(|c| c.path.as_str()).collect();
        assert!(paths.contains(&"a"), "Missing a");
        assert!(paths.contains(&"b"), "Missing b (sync)");
        assert!(paths.contains(&"c"), "Missing c (flip)");

        let concern_paths: Vec<&str> = parsed
            .concern_changes
            .iter()
            .map(|c| c.path.as_str())
            .collect();
        assert!(
            concern_paths.contains(&"_concerns.x.disabledWhen"),
            "Missing BoolLogic"
        );
    }

    #[test]
    fn full_pipeline_shadow_state_kept_in_sync() {
        let mut p = make_pipeline();

        p.register_sync_batch(r#"[["x", "y"]]"#).unwrap();

        p.process_changes(r#"[{"path": "x", "value_json": "\"updated\""}]"#)
            .unwrap();

        // Check that shadow state has both x and y updated
        assert_eq!(p.shadow_get("x").unwrap(), r#""updated""#);
        assert_eq!(p.shadow_get("y").unwrap(), r#""updated""#);
    }

    #[test]
    fn full_pipeline_many_changes_with_sync_and_flip() {
        let mut p = ProcessingPipeline::new();
        p.shadow_init(
            r#"{
                "a1": 1, "a2": 1,
                "b1": true, "b2": false,
                "c1": "x", "c2": "x"
            }"#,
        )
        .unwrap();

        // Sync pairs
        p.register_sync_batch(r#"[["a1", "a2"], ["c1", "c2"]]"#)
            .unwrap();

        // Flip pair
        p.register_flip_batch(r#"[["b1", "b2"]]"#).unwrap();

        // 3 input changes
        let result = p
            .process_changes(
                r#"[
                    {"path": "a1", "value_json": "2"},
                    {"path": "b1", "value_json": "false"},
                    {"path": "c1", "value_json": "\"y\""}
                ]"#,
            )
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Expected:
        // - Input: a1=2, b1=false, c1="y"
        // - Sync: a2=2, c2="y"
        // - Flip: b2=true (inverted from b1=false)
        // Total: 6 changes

        assert_eq!(parsed.changes.len(), 6);

        let paths: Vec<&str> = parsed.changes.iter().map(|c| c.path.as_str()).collect();
        assert!(paths.contains(&"a1"));
        assert!(paths.contains(&"a2"));
        assert!(paths.contains(&"b1"));
        assert!(paths.contains(&"b2"));
        assert!(paths.contains(&"c1"));
        assert!(paths.contains(&"c2"));

        // Verify values
        let changes_map: std::collections::HashMap<&str, &str> = parsed
            .changes
            .iter()
            .map(|c| (c.path.as_str(), c.value_json.as_str()))
            .collect();

        assert_eq!(changes_map["a1"], "2");
        assert_eq!(changes_map["a2"], "2");
        assert_eq!(changes_map["b1"], "false");
        assert_eq!(changes_map["b2"], "true"); // flipped
        assert_eq!(changes_map["c1"], r#""y""#);
        assert_eq!(changes_map["c2"], r#""y""#);
    }

    #[test]
    fn full_pipeline_sync_change_triggers_affected_boollogic() {
        let mut p = make_pipeline();

        // x <-> y (sync)
        p.register_sync_batch(r#"[["x", "y"]]"#).unwrap();

        // BoolLogic depends on y
        p.register_boollogic(
            "_concerns.field.disabledWhen",
            r#"{"IS_EQUAL": ["y", "admin"]}"#,
        )
        .unwrap();

        // Change x (will sync to y, then trigger BoolLogic)
        let result = p
            .process_changes(r#"[{"path": "x", "value_json": "\"admin\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should have: input + sync (state) + BoolLogic (concern)
        assert_eq!(parsed.changes.len(), 2);
        assert_eq!(parsed.concern_changes.len(), 1);

        let bl_change = &parsed.concern_changes[0];
        assert_eq!(bl_change.path, "_concerns.field.disabledWhen");
        assert_eq!(bl_change.value_json, "true");
    }

    #[test]
    fn full_pipeline_aggregation_with_boollogic() {
        let mut p = make_pipeline();

        p.register_aggregation_batch(r#"[{"target": "allFlags", "sources": ["flag1", "flag2"]}]"#)
            .unwrap();

        p.register_boollogic(
            "_concerns.panel.visibleWhen",
            r#"{"OR": [{"IS_EQUAL": ["flag1", true]}, {"IS_EQUAL": ["flag2", true]}]}"#,
        )
        .unwrap();

        // Write to aggregation target
        let result = p
            .process_changes(r#"[{"path": "allFlags", "value_json": "true"}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should have: 2 aggregated state changes + 1 concern change
        assert_eq!(parsed.changes.len(), 2);
        assert_eq!(parsed.concern_changes.len(), 1);

        let bl = &parsed.concern_changes[0];
        assert!(bl.path.contains("visibleWhen"));
        assert_eq!(bl.value_json, "true");
    }

    // --- Validator tests (WASM-022) ---

    #[test]
    fn validator_included_in_process_changes_output() {
        let mut p = make_pipeline();

        // Register validator on user.email
        p.register_validators_batch(
            r#"[{
                "validator_id": 1,
                "output_path": "_concerns.user.email.validationState",
                "dependency_paths": ["user.email"]
            }]"#,
        )
        .unwrap();

        // Change user.email
        let result = p
            .process_changes(r#"[{"path": "user.email", "value_json": "\"new@test.com\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should have 1 state change + 1 validator in validators_to_run
        assert_eq!(parsed.changes.len(), 1);
        assert_eq!(parsed.changes[0].path, "user.email");
        assert_eq!(parsed.validators_to_run.len(), 1);

        let validator = &parsed.validators_to_run[0];
        assert_eq!(validator.validator_id, 1);
        assert_eq!(
            validator.output_path,
            "_concerns.user.email.validationState"
        );
        assert_eq!(validator.dependency_values.len(), 1);
        assert_eq!(
            validator.dependency_values.get("user.email").unwrap(),
            r#""new@test.com""#
        );
    }

    #[test]
    fn validator_unrelated_change_skips_validator() {
        let mut p = make_pipeline();

        // Register validator on user.email
        p.register_validators_batch(
            r#"[{
                "validator_id": 1,
                "output_path": "_concerns.user.email.validationState",
                "dependency_paths": ["user.email"]
            }]"#,
        )
        .unwrap();

        // Change user.age (unrelated to validator)
        let result = p
            .process_changes(r#"[{"path": "user.age", "value_json": "25"}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should have 1 state change, no validators
        assert_eq!(parsed.changes.len(), 1);
        assert_eq!(parsed.changes[0].path, "user.age");
        assert_eq!(parsed.validators_to_run.len(), 0);
    }

    #[test]
    fn validator_multi_dep_gets_all_values() {
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"order": {"amount": 100, "currency": "USD"}}"#)
            .unwrap();

        // Register validator that depends on two fields
        p.register_validators_batch(
            r#"[{
                "validator_id": 2,
                "output_path": "_concerns.order.validationState",
                "dependency_paths": ["order.amount", "order.currency"]
            }]"#,
        )
        .unwrap();

        // Change order.amount (should trigger validator with both values)
        let result = p
            .process_changes(r#"[{"path": "order.amount", "value_json": "200"}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should have 1 state change + 1 validator with both dependency values
        assert_eq!(parsed.changes.len(), 1);
        assert_eq!(parsed.validators_to_run.len(), 1);

        let validator = &parsed.validators_to_run[0];
        assert_eq!(validator.validator_id, 2);
        assert_eq!(validator.dependency_values.len(), 2);
        assert_eq!(
            validator.dependency_values.get("order.amount").unwrap(),
            "200"
        );
        assert_eq!(
            validator.dependency_values.get("order.currency").unwrap(),
            r#""USD""#
        );
    }

    #[test]
    fn validator_multiple_validators_deduped() {
        let mut p = make_pipeline();

        // Register two validators on the same dependency path
        p.register_validators_batch(
            r#"[
                {
                    "validator_id": 1,
                    "output_path": "_concerns.user.email.validationState",
                    "dependency_paths": ["user.email"]
                },
                {
                    "validator_id": 2,
                    "output_path": "_concerns.user.email.formatState",
                    "dependency_paths": ["user.email"]
                }
            ]"#,
        )
        .unwrap();

        // Change user.email (should trigger both validators)
        let result = p
            .process_changes(r#"[{"path": "user.email", "value_json": "\"test@example.com\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should have 1 state change + 2 validators (both appear once)
        assert_eq!(parsed.changes.len(), 1);
        assert_eq!(parsed.validators_to_run.len(), 2);

        let validator_ids: Vec<u32> = parsed
            .validators_to_run
            .iter()
            .map(|v| v.validator_id)
            .collect();
        assert!(validator_ids.contains(&1));
        assert!(validator_ids.contains(&2));

        // Verify both have correct dependency value
        for validator in &parsed.validators_to_run {
            assert_eq!(
                validator.dependency_values.get("user.email").unwrap(),
                r#""test@example.com""#
            );
        }
    }

    #[test]
    fn validator_boollogic_and_validators_in_same_run() {
        let mut p = make_pipeline();

        // Register BoolLogic
        p.register_boollogic(
            "_concerns.user.email.disabledWhen",
            r#"{"IS_EQUAL": ["user.role", "admin"]}"#,
        )
        .unwrap();

        // Register validator
        p.register_validators_batch(
            r#"[{
                "validator_id": 1,
                "output_path": "_concerns.user.email.validationState",
                "dependency_paths": ["user.email"]
            }]"#,
        )
        .unwrap();

        // Change user.email (triggers validator, not BoolLogic)
        let result = p
            .process_changes(r#"[{"path": "user.email", "value_json": "\"admin@example.com\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should have: 1 state change, 0 concern changes (BoolLogic not affected), 1 validator
        assert_eq!(parsed.changes.len(), 1);
        assert_eq!(parsed.concern_changes.len(), 0); // BoolLogic depends on user.role, not user.email
        assert_eq!(parsed.validators_to_run.len(), 1);

        // Now change user.role (triggers BoolLogic, not validator)
        let result = p
            .process_changes(r#"[{"path": "user.role", "value_json": "\"admin\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should have: 1 state change, 1 concern change (BoolLogic), 0 validators
        assert_eq!(parsed.changes.len(), 1);
        assert_eq!(parsed.concern_changes.len(), 1);
        assert_eq!(
            parsed.concern_changes[0].path,
            "_concerns.user.email.disabledWhen"
        );
        assert_eq!(parsed.concern_changes[0].value_json, "true");
        assert_eq!(parsed.validators_to_run.len(), 0); // Validator depends on user.email, not user.role
    }

    // --- WASM-029: process_changes_with_diff tests ---

    #[test]
    fn process_changes_diff_filters_no_ops() {
        let mut p = make_pipeline();

        // Try to set user.role to "guest" (current value is already "guest")
        let changes = vec![Change::new("user.role".to_owned(), r#""guest""#.to_owned())];
        let result = p.process_changes_vec(changes).unwrap();

        // Should be filtered out by diff engine (always-on)
        assert_eq!(result.changes.len(), 0);
        assert_eq!(result.concern_changes.len(), 0);
    }

    #[test]
    fn process_changes_diff_keeps_genuine_changes() {
        let mut p = make_pipeline();

        // Change user.role to "admin" (different from current "guest")
        let changes = vec![Change::new("user.role".to_owned(), r#""admin""#.to_owned())];
        let result = p.process_changes_vec(changes).unwrap();

        // Should pass through diff and process normally
        assert_eq!(result.changes.len(), 1);
        assert_eq!(result.changes[0].path, "user.role");
        assert_eq!(result.changes[0].value_json, r#""admin""#);
    }

    #[test]
    fn process_changes_diff_early_exit_empty() {
        let mut p = make_pipeline();

        // All changes are no-ops
        let changes = vec![
            Change::new("user.role".to_owned(), r#""guest""#.to_owned()),
            Change::new("user.age".to_owned(), "20".to_owned()),
        ];
        let result = p.process_changes_vec(changes).unwrap();

        // Should early exit with empty result (diff always-on)
        assert_eq!(result.changes.len(), 0);
        assert_eq!(result.concern_changes.len(), 0);
        assert_eq!(result.validators_to_run.len(), 0);
        assert!(result.execution_plan.is_none());
    }

    #[test]
    fn process_changes_diff_partial_filter() {
        let mut p = make_pipeline();

        // Mix of unchanged and changed values
        let changes = vec![
            Change::new("user.role".to_owned(), r#""guest""#.to_owned()), // unchanged
            Change::new("user.age".to_owned(), "25".to_owned()),          // changed
        ];
        let result = p.process_changes_vec(changes).unwrap();

        // Should keep only the changed value (diff always-on)
        assert_eq!(result.changes.len(), 1);
        assert_eq!(result.changes[0].path, "user.age");
        assert_eq!(result.changes[0].value_json, "25");
    }
}
