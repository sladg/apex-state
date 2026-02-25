//! Processing pipeline: single entry point for all state changes.
//!
//! Owns shadow state, intern table, BoolLogic registry, and reverse
//! dependency index. Processes a batch of changes, updates shadow state,
//! evaluates affected BoolLogic expressions, and returns all changes
//! (input + computed).

use crate::aggregation::{
    process_aggregation_reads, process_aggregation_writes, AggregationRegistry, AggregationSource,
};
use crate::bool_logic::{BoolLogicNode, BoolLogicRegistry};
use crate::clear_paths::ClearPathsRegistry;
use crate::computation::{
    process_computation_reads, ComputationOp, ComputationRegistry, ComputationSource,
};
use crate::functions::{FunctionInput, FunctionRegistry};
use crate::graphs::Graph;
use crate::intern::InternTable;
use crate::rev_index::ReverseDependencyIndex;
use crate::router::{FullExecutionPlan, TopicRouter};
use crate::shadow::ShadowState;
use crate::value_logic::{ValueLogicNode, ValueLogicRegistry};
use serde::{Deserialize, Serialize};
use std::collections::HashSet;

/// A single change in the input/output format.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub(crate) struct Change {
    pub path: String,
    pub value_json: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub origin: Option<String>,
}

/// Raw sentinel value for JS `undefined` (without JSON quotes).
/// Used by shadow state to recognize undefined values during traversal.
pub(crate) const UNDEFINED_SENTINEL: &str = "__APEX_UNDEFINED__";

/// JSON-encoded sentinel for JS `undefined` values crossing the WASM boundary.
/// JS sends `JSON.stringify("__APEX_UNDEFINED__")` = `"\"__APEX_UNDEFINED__\""`.
/// Treated as null-equivalent in aggregation, sync filtering, and shadow traversal.
pub(crate) const UNDEFINED_SENTINEL_JSON: &str = "\"__APEX_UNDEFINED__\"";

/// Validator dispatch info for JS-side execution.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub(crate) struct ValidatorDispatch {
    pub validator_id: u32,
    pub output_path: String,
    pub dependency_values: std::collections::HashMap<String, String>,
}

/// Output wrapper for processChanges (deprecated, kept for backward compat with tests).
#[derive(Serialize, Deserialize, Debug)]
#[allow(dead_code)] // Used via WASM exports (invisible to clippy)
pub(crate) struct ProcessResult {
    /// All changes including state and concerns (concern paths have _concerns. prefix).
    pub changes: Vec<Change>,
    /// Validators to run on JS side with their dependency values.
    pub validators_to_run: Vec<ValidatorDispatch>,
    pub execution_plan: Option<FullExecutionPlan>,
}

/// Result from processChanges: orchestrates pipeline, buffers concern results for finalization.
#[derive(Serialize, Deserialize, Debug)]
pub(crate) struct PrepareResult {
    /// State changes (readonly context for JS listener execution, not for applying to valtio yet).
    pub state_changes: Vec<Change>,
    /// Validators to run on JS side with their dependency values.
    pub validators_to_run: Vec<ValidatorDispatch>,
    /// Pre-computed execution plan for listener dispatch.
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

// ---------------------------------------------------------------------------
// Consolidated registration structs (Rust side API)
// ---------------------------------------------------------------------------

/// Input for a clear-path rule from JS.
#[derive(Deserialize, Debug)]
pub(crate) struct ClearPathInput {
    pub triggers: Vec<String>,
    pub targets: Vec<String>,
}

/// Input for consolidated side effects registration.
#[derive(Deserialize, Debug)]
pub(crate) struct SideEffectsRegistration {
    pub registration_id: String,
    #[serde(default)]
    pub sync_pairs: Vec<[String; 2]>,
    #[serde(default)]
    pub flip_pairs: Vec<[String; 2]>,
    #[serde(default)]
    pub aggregation_pairs: Vec<serde_json::Value>,
    #[serde(default)]
    pub clear_paths: Vec<ClearPathInput>,
    #[serde(default)]
    pub computation_pairs: Vec<serde_json::Value>,
    #[serde(default)]
    pub listeners: Vec<ListenerRegistration>,
}

/// Listener entry for consolidated registration.
#[derive(Deserialize, Debug)]
pub(crate) struct ListenerRegistration {
    pub subscriber_id: u32,
    pub topic_path: String,
    pub scope_path: String,
}

/// Output from consolidated side effects registration.
#[derive(Serialize, Debug)]
pub(crate) struct SideEffectsResult {
    pub sync_changes: Vec<Change>,
    pub aggregation_changes: Vec<Change>,
    pub computation_changes: Vec<Change>,
    pub registered_listener_ids: Vec<u32>,
}

/// Input for consolidated concerns registration.
#[derive(Deserialize, Debug)]
pub(crate) struct ConcernsRegistration {
    #[serde(default)]
    pub bool_logics: Vec<BoolLogicRegistration>,
    #[serde(default)]
    pub validators: Vec<ValidatorRegistration>,
    #[serde(default)]
    pub value_logics: Vec<ValueLogicRegistration>,
}

/// ValueLogic entry for consolidated registration.
#[derive(Deserialize, Debug)]
pub(crate) struct ValueLogicRegistration {
    pub output_path: String,
    pub tree_json: String,
}

/// BoolLogic entry for consolidated registration.
#[derive(Deserialize, Debug)]
pub(crate) struct BoolLogicRegistration {
    pub output_path: String,
    pub tree_json: String,
}

/// Validator entry for consolidated registration.
#[derive(Deserialize, Debug)]
pub(crate) struct ValidatorRegistration {
    pub validator_id: u32,
    pub output_path: String,
    pub dependency_paths: Vec<String>,
}

/// Input format for batch validator registration (moved from removed validator module).
#[derive(Serialize, Deserialize, Debug)]
pub(crate) struct ValidatorInput {
    pub validator_id: u32,
    pub output_path: String,
    pub dependency_paths: Vec<String>,
}

/// Output from consolidated concerns registration.
#[derive(Serialize, Debug)]
pub(crate) struct ConcernsResult {
    pub bool_logic_changes: Vec<Change>,
    pub registered_logic_ids: Vec<u32>,
    pub registered_validator_ids: Vec<u32>,
    pub value_logic_changes: Vec<Change>,
    pub registered_value_logic_ids: Vec<u32>,
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
    computations: ComputationRegistry,
    clear_registry: ClearPathsRegistry,
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
    // ValueLogic registry and reverse dependency index
    value_logic_registry: ValueLogicRegistry,
    value_logic_rev_index: ReverseDependencyIndex,
    buf_affected_value_logics: HashSet<u32>,
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
            computations: ComputationRegistry::new(),
            clear_registry: ClearPathsRegistry::new(),
            sync_graph: Graph::new(),
            flip_graph: Graph::new(),
            router: TopicRouter::new(),
            buf_output: Vec::with_capacity(64),
            buf_sync: Vec::with_capacity(16),
            buf_flip: Vec::with_capacity(16),
            buf_affected_ids: HashSet::with_capacity(16),
            buf_concern_changes: Vec::with_capacity(16),
            buf_affected_validators: HashSet::with_capacity(16),
            value_logic_registry: ValueLogicRegistry::new(),
            value_logic_rev_index: ReverseDependencyIndex::new(),
            buf_affected_value_logics: HashSet::with_capacity(16),
            buf_pending_state_changes: Vec::with_capacity(64),
            buf_pending_concern_changes: Vec::with_capacity(16),
        }
    }

    /// Reset the entire pipeline to a fresh state.
    pub(crate) fn reset(&mut self) {
        *self = Self::new();
    }

    /// Initialize shadow state from a JSON string.
    #[allow(dead_code)] // Called via WASM export chain
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
        // Parse raw pairs as JSON arrays (2 or 3 elements each)
        let raw_pairs: Vec<serde_json::Value> = serde_json::from_str(pairs_json)
            .map_err(|e| format!("Aggregation pairs parse error: {}", e))?;

        // Collect all targets and sources for validation
        let mut targets = std::collections::HashSet::new();
        let mut sources = std::collections::HashSet::new();

        // Parse each pair into (target, source, optional condition)
        let mut parsed: Vec<(String, AggregationSource)> = Vec::new();

        for pair in &raw_pairs {
            let arr = pair
                .as_array()
                .ok_or_else(|| "Aggregation pair must be an array".to_string())?;

            if arr.len() < 2 || arr.len() > 3 {
                return Err(format!(
                    "Aggregation pair must have 2 or 3 elements, got {}",
                    arr.len()
                ));
            }

            let target = arr[0]
                .as_str()
                .ok_or_else(|| "Aggregation target must be a string".to_string())?
                .to_string();
            let source_path = arr[1]
                .as_str()
                .ok_or_else(|| "Aggregation source must be a string".to_string())?
                .to_string();

            let exclude_when = if arr.len() == 3 {
                // Third element is a JSON string of BoolLogic tree
                let tree_json = arr[2]
                    .as_str()
                    .ok_or_else(|| "Aggregation condition must be a JSON string".to_string())?;
                let node: BoolLogicNode = serde_json::from_str(tree_json)
                    .map_err(|e| format!("Aggregation condition parse error: {}", e))?;
                Some(node)
            } else {
                None
            };

            targets.insert(target.clone());
            sources.insert(source_path.clone());

            parsed.push((
                target,
                AggregationSource {
                    path: source_path,
                    exclude_when,
                },
            ));
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
        let mut by_target: std::collections::HashMap<String, Vec<AggregationSource>> =
            std::collections::HashMap::new();

        for (target, source) in parsed {
            by_target.entry(target).or_default().push(source);
        }

        // Collect all source paths for initial value computation
        let all_sources: Vec<String> = by_target
            .values()
            .flat_map(|sources| sources.iter().map(|s| s.path.clone()))
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
    #[allow(dead_code)] // Called via WASM export chain
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

    /// Register a batch of computation pairs.
    ///
    /// Input: JSON array of 3 or 4 element arrays: [op, target, source] or [op, target, source, excludeWhen]
    /// Groups by target, validates no circular deps, registers, computes initial values.
    /// Returns initial computation changes.
    pub(crate) fn register_computation_batch(
        &mut self,
        raw_pairs: &[serde_json::Value],
    ) -> Result<Vec<Change>, String> {
        // Collect all targets and sources for validation
        let mut targets = std::collections::HashSet::new();
        let mut sources = std::collections::HashSet::new();

        // Parse each pair into (op, target, source, optional condition)
        let mut parsed: Vec<(ComputationOp, String, ComputationSource)> = Vec::new();

        for pair in raw_pairs {
            let arr = pair
                .as_array()
                .ok_or_else(|| "Computation pair must be an array".to_string())?;

            if arr.len() < 3 || arr.len() > 4 {
                return Err(format!(
                    "Computation pair must have 3 or 4 elements, got {}",
                    arr.len()
                ));
            }

            let op_str = arr[0]
                .as_str()
                .ok_or_else(|| "Computation operation must be a string".to_string())?;
            let op = match op_str {
                "SUM" => ComputationOp::Sum,
                "AVG" => ComputationOp::Avg,
                _ => return Err(format!("Unknown computation operation: {}", op_str)),
            };

            let target = arr[1]
                .as_str()
                .ok_or_else(|| "Computation target must be a string".to_string())?
                .to_string();
            let source_path = arr[2]
                .as_str()
                .ok_or_else(|| "Computation source must be a string".to_string())?
                .to_string();

            let exclude_when = if arr.len() == 4 {
                let tree_json = arr[3]
                    .as_str()
                    .ok_or_else(|| "Computation condition must be a JSON string".to_string())?;
                let node: BoolLogicNode = serde_json::from_str(tree_json)
                    .map_err(|e| format!("Computation condition parse error: {}", e))?;
                Some(node)
            } else {
                None
            };

            targets.insert(target.clone());
            sources.insert(source_path.clone());

            parsed.push((
                op,
                target,
                ComputationSource {
                    path: source_path,
                    exclude_when,
                },
            ));
        }

        // Validate no circular dependencies
        for target in &targets {
            if sources.contains(target) {
                return Err(format!(
                    "Circular computation: \"{}\" cannot be both target and source",
                    target
                ));
            }
        }

        // Group by target for multi-source computations
        let mut by_target: std::collections::HashMap<
            String,
            (ComputationOp, Vec<ComputationSource>),
        > = std::collections::HashMap::new();

        for (op, target, source) in parsed {
            let entry = by_target
                .entry(target)
                .or_insert_with(|| (op.clone(), Vec::new()));
            // Validate all pairs for same target use same operation
            if entry.0 != op {
                return Err(format!(
                    "Computation target has mixed operations (found both {:?} and {:?})",
                    entry.0, op
                ));
            }
            entry.1.push(source);
        }

        // Collect all source paths for initial value computation
        let all_sources: Vec<String> = by_target
            .values()
            .flat_map(|(_, sources)| sources.iter().map(|s| s.path.clone()))
            .collect();

        // Register all computations
        for (target, (op, sources)) in by_target {
            self.computations.register(op, target, sources);
        }

        // Compute initial values
        let initial_changes =
            process_computation_reads(&self.computations, &self.shadow, &all_sources);

        // Update shadow state with initial computation values
        for change in &initial_changes {
            self.shadow
                .set(&change.path, &change.value_json)
                .map_err(|e| format!("Shadow update failed for computation: {}", e))?;
        }

        Ok(initial_changes)
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

                    // Skip undefined sentinel (missing/blank) — null is a valid value
                    if value_json != UNDEFINED_SENTINEL_JSON {
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
                            // When depths are equal, prefer value from lexicographically smaller path
                            // (deterministic tie-breaking: matches legacy JS insertion-order behavior
                            //  when paths are sorted before value counting)
                            match depth_b.cmp(&depth_a) {
                                std::cmp::Ordering::Equal => {
                                    // Compare paths: prefer the one with the lexicographically smaller path
                                    let path_a = value_to_path
                                        .get(*value_a)
                                        .map(|s| s.as_str())
                                        .unwrap_or("");
                                    let path_b = value_to_path
                                        .get(*value_b)
                                        .map(|s| s.as_str())
                                        .unwrap_or("");
                                    // We want smaller path to "win" (be the max), so reverse the comparison
                                    // path_b < path_a means a comes after b alphabetically → a loses → Less
                                    path_b.cmp(path_a)
                                }
                                other => other,
                            }
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
                            origin: Some("sync".to_owned()),
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
    #[allow(dead_code)] // Called via WASM export chain
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
    #[allow(dead_code)] // Called via WASM export chain
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
    #[allow(dead_code)] // Called via WASM export chain
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
    #[allow(dead_code)] // Called via WASM export chain
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
    #[allow(dead_code)] // Called via WASM export chain
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

    // =========================================================================
    // Consolidated registration API
    // =========================================================================

    /// Register all side effects at once: sync, flip, aggregation, and listeners.
    ///
    /// Single WASM call that combines multiple registration operations.
    /// Returns initial changes and registered listener IDs for cleanup tracking.
    pub(crate) fn register_side_effects(
        &mut self,
        reg_json: &str,
    ) -> Result<SideEffectsResult, String> {
        let reg: SideEffectsRegistration = serde_json::from_str(reg_json)
            .map_err(|e| format!("Side effects registration parse error: {}", e))?;

        let mut sync_changes = Vec::new();
        let mut aggregation_changes = Vec::new();
        let mut computation_changes = Vec::new();
        let mut registered_listener_ids = Vec::new();

        // 1. Register sync pairs
        if !reg.sync_pairs.is_empty() {
            let changes = self.register_sync_batch(
                &serde_json::to_string(&reg.sync_pairs)
                    .map_err(|e| format!("Sync pairs serialization error: {}", e))?,
            )?;
            sync_changes = serde_json::from_str(&changes)
                .map_err(|e| format!("Sync changes parse error: {}", e))?;
        }

        // 2. Register flip pairs
        if !reg.flip_pairs.is_empty() {
            self.register_flip_batch(
                &serde_json::to_string(&reg.flip_pairs)
                    .map_err(|e| format!("Flip pairs serialization error: {}", e))?,
            )?;
        }

        // 3. Register aggregations
        if !reg.aggregation_pairs.is_empty() {
            let changes = self.register_aggregation_batch(
                &serde_json::to_string(&reg.aggregation_pairs)
                    .map_err(|e| format!("Aggregation pairs serialization error: {}", e))?,
            )?;
            aggregation_changes = serde_json::from_str(&changes)
                .map_err(|e| format!("Aggregation changes parse error: {}", e))?;
        }

        // 4. Register clear paths
        for cp in &reg.clear_paths {
            self.register_clear_paths(&reg.registration_id, &cp.triggers, &cp.targets)?;
        }

        // 4.5. Register computations
        if !reg.computation_pairs.is_empty() {
            computation_changes = self.register_computation_batch(&reg.computation_pairs)?;
        }

        // 5. Register listeners
        if !reg.listeners.is_empty() {
            let listener_entries: Vec<_> = reg
                .listeners
                .iter()
                .map(|l| {
                    serde_json::json!({
                        "subscriber_id": l.subscriber_id,
                        "topic_path": l.topic_path,
                        "scope_path": l.scope_path,
                    })
                })
                .collect();

            self.register_listeners_batch(
                &serde_json::to_string(&listener_entries)
                    .map_err(|e| format!("Listeners serialization error: {}", e))?,
            )?;

            registered_listener_ids = reg.listeners.iter().map(|l| l.subscriber_id).collect();
        }

        Ok(SideEffectsResult {
            sync_changes,
            aggregation_changes,
            computation_changes,
            registered_listener_ids,
        })
    }

    /// Unregister side effects by registration ID.
    ///
    /// Cleans up clear-path rules for this registration ID.
    /// Other side-effect cleanup is managed by the caller.
    pub(crate) fn unregister_side_effects(&mut self, registration_id: &str) -> Result<(), String> {
        self.unregister_clear_paths(registration_id);
        Ok(())
    }

    /// Register all concerns at once: BoolLogic and validators.
    ///
    /// Single WASM call that combines BoolLogic and validator registration.
    /// Returns initial BoolLogic changes and registered IDs for cleanup tracking.
    pub(crate) fn register_concerns(&mut self, reg_json: &str) -> Result<ConcernsResult, String> {
        let reg: ConcernsRegistration = serde_json::from_str(reg_json)
            .map_err(|e| format!("Concerns registration parse error: {}", e))?;

        let mut bool_logic_changes = Vec::new();
        let mut registered_logic_ids = Vec::new();
        let mut registered_validator_ids = Vec::new();

        // 1. Register BoolLogic expressions and compute initial values
        for bl in &reg.bool_logics {
            let logic_id = self.register_boollogic(&bl.output_path, &bl.tree_json)?;
            registered_logic_ids.push(logic_id);

            // Evaluate BoolLogic with current shadow state to get initial value
            if let Some(meta) = self.registry.get(logic_id) {
                let result = meta.tree.evaluate(&self.shadow);
                bool_logic_changes.push(Change {
                    path: bl.output_path.clone(),
                    value_json: if result {
                        "true".to_owned()
                    } else {
                        "false".to_owned()
                    },
                    origin: None,
                });
            }
        }

        // 2. Register validators
        for validator in &reg.validators {
            let _initial_dispatches = self.register_validators_batch(
                &serde_json::to_string(&[ValidatorInput {
                    validator_id: validator.validator_id,
                    output_path: validator.output_path.clone(),
                    dependency_paths: validator.dependency_paths.clone(),
                }])
                .map_err(|e| format!("Validator serialization error: {}", e))?,
            )?;

            registered_validator_ids.push(validator.validator_id);
        }

        // 3. Register ValueLogic expressions and compute initial values
        let mut value_logic_changes = Vec::new();
        let mut registered_value_logic_ids = Vec::new();

        for vl in &reg.value_logics {
            let tree: ValueLogicNode = serde_json::from_str(&vl.tree_json)
                .map_err(|e| format!("ValueLogic parse error: {}", e))?;

            let vl_id = self.value_logic_registry.register(
                vl.output_path.clone(),
                tree,
                &mut self.intern,
                &mut self.value_logic_rev_index,
            );
            registered_value_logic_ids.push(vl_id);

            // Evaluate ValueLogic with current shadow state to get initial value
            if let Some(meta) = self.value_logic_registry.get(vl_id) {
                let result = meta.tree.evaluate(&self.shadow);
                value_logic_changes.push(Change {
                    path: vl.output_path.clone(),
                    value_json: serde_json::to_string(&result).unwrap_or_else(|_| "null".into()),
                    origin: None,
                });
            }
        }

        Ok(ConcernsResult {
            bool_logic_changes,
            registered_logic_ids,
            registered_validator_ids,
            value_logic_changes,
            registered_value_logic_ids,
        })
    }

    /// Unregister concerns by registration ID.
    ///
    /// Currently a no-op placeholder for future use (registration tracking).
    /// Cleanup is done by caller via individual unregister calls.
    pub(crate) fn unregister_concerns(&mut self, _registration_id: &str) -> Result<(), String> {
        // Placeholder: registration tracking would be implemented here
        // For now, caller manages individual unregister calls
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
                        // Skip sync to paths whose parent structure no longer exists.
                        // Prevents stale sync writes when React hasn't unmounted old
                        // components yet but the target subtree was replaced.
                        if self.shadow.get(peer_path).is_none() {
                            continue;
                        }

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
                                        origin: Some("sync".to_owned()),
                                    });
                                    continue;
                                }
                            };

                        // Only add if different from current shadow value
                        if crate::diff::is_different(&current, &new_value) {
                            self.buf_sync.push(Change {
                                path: peer_path.to_owned(),
                                value_json: change.value_json.clone(),
                                origin: Some("sync".to_owned()),
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
                            // Skip flip to paths whose parent structure no longer exists.
                            // Same guard as sync: prevents writes to stale paths.
                            if self.shadow.get(peer_path).is_none() {
                                continue;
                            }

                            // Check if this flip change is a no-op against current shadow state
                            let current = self.shadow.get(peer_path);
                            let inverted_bool = inverted == "true";
                            let new_value = crate::shadow::ValueRepr::Bool(inverted_bool);

                            // Only add if different from current shadow value
                            if crate::diff::is_different(&current, &new_value) {
                                self.buf_flip.push(Change {
                                    path: peer_path.to_owned(),
                                    value_json: inverted.clone(),
                                    origin: Some("flip".to_owned()),
                                });
                            }
                        }
                    }
                }
            }
        }
    }

    /// Register clear-path rules.
    pub(crate) fn register_clear_paths(
        &mut self,
        registration_id: &str,
        triggers: &[String],
        targets: &[String],
    ) -> Result<(), String> {
        self.clear_registry
            .register(registration_id, triggers, targets, &mut self.intern)
    }

    /// Unregister clear-path rules by registration ID.
    pub(crate) fn unregister_clear_paths(&mut self, registration_id: &str) {
        self.clear_registry.unregister(registration_id);
    }

    /// Register a batch of listeners for topic-based dispatch.
    pub(crate) fn register_listeners_batch(&mut self, listeners_json: &str) -> Result<(), String> {
        self.router.register_listeners_batch(listeners_json)
    }

    /// Unregister a batch of listeners by subscriber IDs.
    #[allow(dead_code)] // Called via WASM export chain
    pub(crate) fn unregister_listeners_batch(
        &mut self,
        subscriber_ids_json: &str,
    ) -> Result<(), String> {
        self.router.unregister_listeners_batch(subscriber_ids_json)
    }

    /// Create a dispatch plan for changes (JSON string path, used by Rust tests).
    #[allow(dead_code)] // Called via WASM export chain
    pub(crate) fn create_dispatch_plan(&self, changes_json: &str) -> Result<String, String> {
        self.router.create_dispatch_plan_json(changes_json)
    }

    /// Create a dispatch plan from a pre-parsed Vec (serde-wasm-bindgen path).
    #[allow(dead_code)] // Called via WASM export chain
    pub(crate) fn create_dispatch_plan_vec(
        &self,
        changes: &[Change],
    ) -> crate::router::DispatchPlan {
        self.router.create_dispatch_plan(changes)
    }

    /// Route produced changes from a depth level to downstream topics (JSON string path).
    #[allow(dead_code)] // Called via WASM export chain
    pub(crate) fn route_produced_changes(
        &self,
        depth: u32,
        produced_changes_json: &str,
    ) -> Result<String, String> {
        self.router
            .route_produced_changes_json(depth, produced_changes_json)
    }

    /// Route produced changes from a pre-parsed Vec (serde-wasm-bindgen path).
    #[allow(dead_code)] // Called via WASM export chain
    pub(crate) fn route_produced_changes_vec(
        &self,
        depth: u32,
        produced_changes: &[Change],
    ) -> crate::router::DispatchPlan {
        self.router.route_produced_changes(depth, produced_changes)
    }

    /// Process a batch of changes from a JSON string (legacy path, used by Rust tests).
    #[allow(dead_code)] // Called via WASM export chain
    pub(crate) fn process_changes(&mut self, changes_json: &str) -> Result<String, String> {
        let input_changes: Vec<Change> = serde_json::from_str(changes_json)
            .map_err(|e| format!("Changes parse error: {}", e))?;
        let result = self.process_changes_vec(input_changes)?;
        serde_json::to_string(&result).map_err(|e| format!("Serialize error: {}", e))
    }

    /// Process a batch of changes from a pre-parsed Vec (serde-wasm-bindgen path).
    #[allow(dead_code)] // Called via WASM export chain
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
        self.buf_affected_value_logics.clear();

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
        let changes = process_aggregation_writes(&self.aggregations, input_changes, &self.shadow);

        // Step 1.5: Filter out writes to computation targets (silent no-op)
        let changes: Vec<Change> = changes
            .into_iter()
            .filter(|c| !self.computations.is_computation_target(&c.path))
            .collect();

        // Step 3: Apply aggregated changes to shadow state and collect affected paths
        // Only process genuine changes (diff against shadow before applying)
        let mut genuine_path_ids: Vec<u32> = Vec::new();
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
                    genuine_path_ids.push(self.intern.intern(&change.path));
                    self.mark_affected_logic(&change.path);
                    continue;
                }
            };

            // Only apply and track if different from current shadow value
            if crate::diff::is_different(&current, &new_value) {
                self.shadow.set(&change.path, &change.value_json)?;
                self.buf_output.push(change.clone());
                genuine_path_ids.push(self.intern.intern(&change.path));
                self.mark_affected_logic(&change.path);
            }
        }

        // Step 3.5: Clear paths — "when X changes, set Y to null"
        // Only original genuine changes (Step 3) feed into clear processing (no self-cascading)
        let clear_changes =
            self.clear_registry
                .process(&genuine_path_ids, &mut self.intern, &mut self.shadow);
        for change in &clear_changes {
            self.mark_affected_logic(&change.path);
        }
        self.buf_output.extend(clear_changes.clone());

        // Step 4-5: Process sync paths and update shadow state
        // Must process ALL aggregated changes (not just genuine ones) because even if
        // a change is a no-op for path A, it might need to sync to path B that differs
        // Include clear changes so sync sees cleared paths
        let mut changes_for_sync = changes.clone();
        changes_for_sync.extend(clear_changes.clone());
        self.process_sync_paths_into(&changes_for_sync);
        // Drain buf_sync to avoid borrow conflict with self.shadow/self.mark_affected_logic
        let sync_changes: Vec<Change> = self.buf_sync.drain(..).collect();
        for change in &sync_changes {
            self.shadow.set(&change.path, &change.value_json)?;
            self.mark_affected_logic(&change.path);
        }
        self.buf_output.extend_from_slice(&sync_changes);

        // Step 6-7: Process flip paths and update shadow state
        // Must process ALL aggregated changes + clear changes + sync outputs
        let mut changes_for_flip = changes.clone();
        changes_for_flip.extend(clear_changes);
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

        // Step 7.6: Process computation reads (sources → target recomputation)
        let all_changed_paths: Vec<String> =
            self.buf_output.iter().map(|c| c.path.clone()).collect();
        let computation_reads =
            process_computation_reads(&self.computations, &self.shadow, &all_changed_paths);
        for change in &computation_reads {
            self.shadow.set(&change.path, &change.value_json)?;
            self.mark_affected_logic(&change.path);
        }
        self.buf_output.extend(computation_reads);

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
                    origin: None,
                });
            }
        }

        // Step 8b: Evaluate affected ValueLogic expressions
        for vl_id in &self.buf_affected_value_logics {
            if let Some(meta) = self.value_logic_registry.get(*vl_id) {
                let result = meta.tree.evaluate(&self.shadow);
                self.buf_concern_changes.push(Change {
                    path: meta.output_path.clone(),
                    value_json: serde_json::to_string(&result).unwrap_or_else(|_| "null".into()),
                    origin: None,
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

    /// Mark all BoolLogic expressions, ValueLogic expressions, and validators affected by a change at the given path.
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
            // Mark affected ValueLogic
            for vl_id in self.value_logic_rev_index.affected_by_path(path_id) {
                self.buf_affected_value_logics.insert(vl_id);
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
        // Mark affected ValueLogic
        for vl_id in self.value_logic_rev_index.affected_by_path(path_id) {
            self.buf_affected_value_logics.insert(vl_id);
        }
    }

    /// Dump shadow state as JSON (debug/testing).
    pub(crate) fn shadow_dump(&self) -> String {
        self.shadow.dump()
    }

    /// Get a value from shadow state at path (debug/testing).
    #[allow(dead_code)] // Called via WASM export chain
    pub(crate) fn shadow_get(&self, path: &str) -> Option<String> {
        self.shadow.get(path).map(|v| {
            serde_json::to_string(&v.to_json_value()).unwrap_or_else(|_| "null".to_owned())
        })
    }

    /// Number of interned paths (debug/testing).
    #[allow(dead_code)] // Called via WASM export chain
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
        self.buf_affected_value_logics.clear();
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
        let changes = process_aggregation_writes(&self.aggregations, input_changes, &self.shadow);

        // Step 1.5: Filter out writes to computation targets (silent no-op)
        let changes: Vec<Change> = changes
            .into_iter()
            .filter(|c| !self.computations.is_computation_target(&c.path))
            .collect();

        // Step 3: Apply aggregated changes to shadow state
        let mut genuine_path_ids: Vec<u32> = Vec::new();
        for change in &changes {
            let current = self.shadow.get(&change.path);
            let new_value: crate::shadow::ValueRepr = match serde_json::from_str(&change.value_json)
            {
                Ok(v) => v,
                Err(_) => {
                    self.shadow.set(&change.path, &change.value_json)?;
                    self.buf_output.push(change.clone());
                    genuine_path_ids.push(self.intern.intern(&change.path));
                    self.mark_affected_logic(&change.path);
                    continue;
                }
            };

            if crate::diff::is_different(&current, &new_value) {
                self.shadow.set(&change.path, &change.value_json)?;
                self.buf_output.push(change.clone());
                genuine_path_ids.push(self.intern.intern(&change.path));
                self.mark_affected_logic(&change.path);
            }
        }

        // Step 3.5: Clear paths — "when X changes, set Y to null"
        // Only original genuine changes (Step 3) feed into clear processing (no self-cascading)
        let clear_changes =
            self.clear_registry
                .process(&genuine_path_ids, &mut self.intern, &mut self.shadow);
        for change in &clear_changes {
            self.mark_affected_logic(&change.path);
        }
        self.buf_output.extend(clear_changes.clone());

        // Step 4-5: Process sync paths
        // Include clear changes so sync sees cleared paths
        let mut changes_for_sync = changes.clone();
        changes_for_sync.extend(clear_changes.clone());
        self.process_sync_paths_into(&changes_for_sync);
        let sync_changes: Vec<Change> = self.buf_sync.drain(..).collect();
        for change in &sync_changes {
            self.shadow.set(&change.path, &change.value_json)?;
            self.mark_affected_logic(&change.path);
        }
        self.buf_output.extend_from_slice(&sync_changes);

        // Step 6-7: Process flip paths
        // Include clear changes + sync changes
        let mut changes_for_flip = changes.clone();
        changes_for_flip.extend(clear_changes);
        changes_for_flip.extend(sync_changes.clone());
        self.process_flip_paths_into(&changes_for_flip);
        let flip_changes: Vec<Change> = self.buf_flip.drain(..).collect();
        for change in &flip_changes {
            self.shadow.set(&change.path, &change.value_json)?;
            self.mark_affected_logic(&change.path);
        }
        self.buf_output.extend(flip_changes);

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

        // Step 7.6: Process computation reads (sources → target recomputation)
        // After aggregation reads, check if any computation sources changed and recompute targets
        let all_changed_paths: Vec<String> =
            self.buf_output.iter().map(|c| c.path.clone()).collect();
        let computation_reads =
            process_computation_reads(&self.computations, &self.shadow, &all_changed_paths);
        for change in &computation_reads {
            self.shadow.set(&change.path, &change.value_json)?;
            self.mark_affected_logic(&change.path);
        }
        self.buf_output.extend(computation_reads);

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
                    origin: None,
                });
            }
        }

        // Step 8b: Evaluate affected ValueLogic expressions
        for vl_id in &self.buf_affected_value_logics {
            if let Some(meta) = self.value_logic_registry.get(*vl_id) {
                let result = meta.tree.evaluate(&self.shadow);
                self.buf_concern_changes.push(Change {
                    path: meta.output_path.clone(),
                    value_json: serde_json::to_string(&result).unwrap_or_else(|_| "null".into()),
                    origin: None,
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
                    origin: change.origin,
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
                        path: crate::join_path("_concerns", &c.path),
                        value_json: c.value_json,
                        origin: c.origin,
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
                path: crate::join_path("_concerns", &c.path),
                value_json: c.value_json,
                origin: c.origin,
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
    #[allow(dead_code)] // Called via WASM export chain
    pub fn new(path: String, value_json: String) -> Self {
        Self {
            path,
            value_json,
            origin: None,
        }
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

    /// Helper to extract concern changes from ProcessResult (those starting with _concerns.)
    fn get_concern_changes(result: &ProcessResult) -> Vec<&Change> {
        result
            .changes
            .iter()
            .filter(|c| c.path.starts_with("_concerns."))
            .collect()
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

        // Should have 1 state change + 1 concern change (merged)
        assert_eq!(parsed.changes.len(), 2);

        let concern_changes = get_concern_changes(&parsed);
        assert_eq!(concern_changes.len(), 1);
        let bl_change = concern_changes[0];
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

        assert_eq!(parsed.changes.len(), 2);
        let concern_changes = get_concern_changes(&parsed);
        assert_eq!(concern_changes.len(), 1);
        let bl_change = concern_changes[0];
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

        // 1 input change + 3 concern changes (merged)
        assert_eq!(parsed.changes.len(), 4);
        let concern_changes = get_concern_changes(&parsed);
        assert_eq!(concern_changes.len(), 3);

        let bl_paths: Vec<&str> = concern_changes.iter().map(|c| c.path.as_str()).collect();
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

        let concern_changes = get_concern_changes(&parsed);
        let bl = concern_changes
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
        let concern_changes = get_concern_changes(&parsed);
        let bl = concern_changes
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

        // Should produce BoolLogic output (1 state + 1 concern, merged)
        let result = p
            .process_changes(r#"[{"path": "user.role", "value_json": "\"admin\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();
        assert_eq!(parsed.changes.len(), 2);
        let concern_changes = get_concern_changes(&parsed);
        assert_eq!(concern_changes.len(), 1);

        // Unregister
        p.unregister_boollogic(id);

        // Now no BoolLogic output
        let result = p
            .process_changes(r#"[{"path": "user.role", "value_json": "\"editor\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();
        assert_eq!(parsed.changes.len(), 1);
        let concern_changes = get_concern_changes(&parsed);
        assert_eq!(concern_changes.len(), 0);
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
            r#"[["allUsers", "user1"], ["allUsers", "user2"], ["allUsers", "user3"]]"#,
        )
        .unwrap();

        // Write to aggregation target
        let result = p
            .process_changes(r#"[{"path": "allUsers", "value_json": "\"alice\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should have 3 distributed changes + 1 aggregation read (target recomputed from sources)
        assert_eq!(parsed.changes.len(), 4);

        let paths: Vec<&str> = parsed.changes.iter().map(|c| c.path.as_str()).collect();
        assert!(paths.contains(&"user1"));
        assert!(paths.contains(&"user2"));
        assert!(paths.contains(&"user3"));
        assert!(paths.contains(&"allUsers")); // target recomputed (all sources equal)

        // All should have the same value
        for change in &parsed.changes {
            assert_eq!(change.value_json, "\"alice\"");
        }
    }

    #[test]
    fn aggregation_removes_target_change() {
        let mut p = make_pipeline();

        p.register_aggregation_batch(
            r#"[["form.allChecked", "item1"], ["form.allChecked", "item2"]]"#,
        )
        .unwrap();

        let result = p
            .process_changes(r#"[{"path": "form.allChecked", "value_json": "true"}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should have 2 distributed changes + 1 aggregation read (target recomputed)
        assert_eq!(parsed.changes.len(), 3);
        let paths: Vec<&str> = parsed.changes.iter().map(|c| c.path.as_str()).collect();
        assert!(paths.contains(&"item1"));
        assert!(paths.contains(&"item2"));
        assert!(paths.contains(&"form.allChecked")); // target recomputed from sources
    }

    #[test]
    fn aggregation_with_child_path() {
        let mut p = make_pipeline();

        p.register_aggregation_batch(r#"[["allUsers", "user1"], ["allUsers", "user2"]]"#)
            .unwrap();

        // Write to a child path of the aggregation target
        let result = p
            .process_changes(
                r#"[{"path": "allUsers.email", "value_json": "\"test@example.com\""}]"#,
            )
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should distribute to both users with the child path appended
        // + aggregation read recomputes target (sources missing → undefined sentinel)
        let paths: Vec<&str> = parsed.changes.iter().map(|c| c.path.as_str()).collect();
        assert!(paths.contains(&"user1.email"));
        assert!(paths.contains(&"user2.email"));
    }

    #[test]
    fn aggregation_with_multiple_aggregations() {
        let mut p = make_pipeline();

        p.register_aggregation_batch(
            r#"[
                ["allUsers", "user1"], ["allUsers", "user2"],
                ["allItems", "item1"], ["allItems", "item2"], ["allItems", "item3"]
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

        // Should have 2 + 3 = 5 distributed + 2 aggregation reads (targets recomputed)
        assert_eq!(parsed.changes.len(), 7);

        let paths: Vec<&str> = parsed.changes.iter().map(|c| c.path.as_str()).collect();
        assert!(paths.contains(&"user1"));
        assert!(paths.contains(&"user2"));
        assert!(paths.contains(&"item1"));
        assert!(paths.contains(&"item2"));
        assert!(paths.contains(&"item3"));
        assert!(paths.contains(&"allUsers")); // target recomputed
        assert!(paths.contains(&"allItems")); // target recomputed
    }

    #[test]
    fn aggregation_unregister() {
        let mut p = make_pipeline();

        p.register_aggregation_batch(r#"[["allUsers", "user1"], ["allUsers", "user2"]]"#)
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

        p.register_aggregation_batch(r#"[["allUsers", "user1"], ["allUsers", "user2"]]"#)
            .unwrap();

        let result = p
            .process_changes(r#"[{"path": "allUsers", "value_json": "\"alice\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should have 2 distributed changes + 1 aggregation read (target recomputed)
        assert_eq!(parsed.changes.len(), 3);
        let paths: Vec<&str> = parsed.changes.iter().map(|c| c.path.as_str()).collect();
        assert!(paths.contains(&"user1"));
        assert!(paths.contains(&"user2"));
        assert!(paths.contains(&"allUsers")); // target recomputed (all sources equal)
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
        // Use false for profile statuses (not null) to avoid sync majority tie
        // with the new null-is-valid semantics
        p.shadow_init(
            r#"{
                "allUsers": null,
                "user1": {"status": false},
                "user2": {"status": false},
                "profile1": {"status": false},
                "profile2": {"status": false}
            }"#,
        )
        .unwrap();

        // Register aggregation
        p.register_aggregation_batch(r#"[["allUsers", "user1"], ["allUsers", "user2"]]"#)
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
        // 1. Aggregation: allUsers.status → user1.status=true, user2.status=true
        // 2. Sync: user1.status → profile1.status=true, user2.status → profile2.status=true
        // 3. Flip: user1.status=true → user2.status=false, user2.status=true → user1.status=false
        // 4. Aggregation read: sources now differ → allUsers recomputed

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

        // Should have: input + sync (state changes) + BoolLogic (concern change, merged)
        assert_eq!(parsed.changes.len(), 3);
        let concern_changes = get_concern_changes(&parsed);
        assert_eq!(concern_changes.len(), 1);
        let bl = concern_changes[0];
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

        // Input + sync (state changes) + 2 BoolLogics (concern changes, merged)
        assert_eq!(parsed.changes.len(), 4);
        let concern_changes = get_concern_changes(&parsed);
        assert_eq!(concern_changes.len(), 2);

        let bl_paths: Vec<&str> = concern_changes.iter().map(|c| c.path.as_str()).collect();
        assert!(bl_paths.contains(&"_concerns.field1.visibleWhen"));
        assert!(bl_paths.contains(&"_concerns.field2.visibleWhen"));

        for change in &concern_changes {
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

        let concern_changes = get_concern_changes(&parsed);
        let concern_paths: Vec<&str> = concern_changes.iter().map(|c| c.path.as_str()).collect();
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

        // Should have: input + sync (state) + BoolLogic (concern, merged)
        assert_eq!(parsed.changes.len(), 3);
        let concern_changes = get_concern_changes(&parsed);
        assert_eq!(concern_changes.len(), 1);

        let bl_change = concern_changes[0];
        assert_eq!(bl_change.path, "_concerns.field.disabledWhen");
        assert_eq!(bl_change.value_json, "true");
    }

    #[test]
    fn full_pipeline_aggregation_with_boollogic() {
        let mut p = make_pipeline();

        p.register_aggregation_batch(r#"[["allFlags", "flag1"], ["allFlags", "flag2"]]"#)
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

        // Should have: 2 aggregated state changes + 1 aggregation read + 1 concern change (merged)
        assert_eq!(parsed.changes.len(), 4);
        let concern_changes = get_concern_changes(&parsed);
        assert_eq!(concern_changes.len(), 1);

        let bl = concern_changes[0];
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
        let concern_changes = get_concern_changes(&parsed);
        assert_eq!(concern_changes.len(), 0); // BoolLogic depends on user.role, not user.email
        assert_eq!(parsed.validators_to_run.len(), 1);

        // Now change user.role (triggers BoolLogic, not validator)
        let result = p
            .process_changes(r#"[{"path": "user.role", "value_json": "\"admin\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should have: 1 state change + 1 concern change (BoolLogic, merged), 0 validators
        assert_eq!(parsed.changes.len(), 2);
        let concern_changes = get_concern_changes(&parsed);
        assert_eq!(concern_changes.len(), 1);
        assert_eq!(concern_changes[0].path, "_concerns.user.email.disabledWhen");
        assert_eq!(concern_changes[0].value_json, "true");
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
        let concern_changes = get_concern_changes(&result);
        assert_eq!(concern_changes.len(), 0);
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
        let concern_changes = get_concern_changes(&result);
        assert_eq!(concern_changes.len(), 0);
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

    // --- ValidatorDispatch serialization ---

    #[test]
    fn validator_dispatch_serializes_dependency_values() {
        let mut deps = std::collections::HashMap::new();
        deps.insert("user.email".to_string(), "\"test@test.com\"".to_string());
        deps.insert("user.name".to_string(), "\"Alice\"".to_string());

        let dispatch = ValidatorDispatch {
            validator_id: 1,
            output_path: "_concerns.user.email.validationState".to_string(),
            dependency_values: deps,
        };

        let json = serde_json::to_value(&dispatch).unwrap();
        let dep_vals = json.get("dependency_values").unwrap().as_object().unwrap();
        assert_eq!(dep_vals.get("user.email").unwrap(), "\"test@test.com\"");
        assert_eq!(dep_vals.get("user.name").unwrap(), "\"Alice\"");
    }

    // --- Consolidated registration API (register_side_effects + register_concerns) ---

    #[test]
    fn register_side_effects_sync_paths_work_correctly() {
        let mut p = ProcessingPipeline::new();
        // Initialize with specific values
        p.shadow_init(
            r#"{"user": {"email": "user@test.com"}, "profile": {"email": "profile@test.com"}}"#,
        )
        .unwrap();

        // Register sync: user.email <-> profile.email
        let reg = r#"{
            "registration_id": "sync-test",
            "sync_pairs": [["user.email", "profile.email"]],
            "flip_pairs": [],
            "aggregation_pairs": [],
            "listeners": []
        }"#;

        let result = p.register_side_effects(reg).unwrap();

        // Since both values exist in shadow state, sync should pick the most common value
        // and return changes to sync the other
        assert!(
            !result.sync_changes.is_empty(),
            "sync_changes should be computed"
        );

        // Verify sync graph was updated by trying to sync again
        let sync_test = p
            .register_sync_batch(r#"[["user.email", "profile.email"]]"#)
            .unwrap();
        let _: Vec<Change> = serde_json::from_str(&sync_test).unwrap();
    }

    #[test]
    fn register_side_effects_aggregation_reads_shadow_state() {
        let mut p = ProcessingPipeline::new();
        // Initialize with items that will be aggregated
        p.shadow_init(
            r#"{
            "items": [
                {"price": 10},
                {"price": 20},
                {"price": 30}
            ],
            "totals": {}
        }"#,
        )
        .unwrap();

        // Register aggregation: totals.sum <- items[].price
        let reg = r#"{
            "registration_id": "agg-test",
            "sync_pairs": [],
            "flip_pairs": [],
            "aggregation_pairs": [
                ["totals.sum", "items.0.price"],
                ["totals.sum", "items.1.price"],
                ["totals.sum", "items.2.price"]
            ],
            "listeners": []
        }"#;

        let result = p.register_side_effects(reg).unwrap();

        // Aggregation should compute initial values from shadow state (read direction)
        // totals.sum should be set based on the aggregation logic
        // The aggregation reads from items[].price and produces initial values
        assert!(
            !result.aggregation_changes.is_empty(),
            "aggregation should compute initial values from shadow state"
        );

        // Verify the aggregation change has the correct path
        assert_eq!(result.aggregation_changes[0].path, "totals.sum");

        // Sources have different prices (10, 20, 30) → all-equal fails → undefined sentinel
        let value_str = &result.aggregation_changes[0].value_json;
        assert_eq!(
            value_str, "\"__APEX_UNDEFINED__\"",
            "sources disagree → aggregation result is undefined sentinel"
        );
    }

    #[test]
    fn register_side_effects_listener_registration_completes() {
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"data": {"value": 42}}"#).unwrap();

        // Register listeners with the consolidated API
        let reg = r#"{
            "registration_id": "listeners-test",
            "sync_pairs": [],
            "flip_pairs": [],
            "aggregation_pairs": [],
            "listeners": [
                {"subscriber_id": 100, "topic_path": "data", "scope_path": ""},
                {"subscriber_id": 101, "topic_path": "data.value", "scope_path": "data"}
            ]
        }"#;

        let result = p.register_side_effects(reg).unwrap();

        // Verify both listener IDs are returned
        assert_eq!(result.registered_listener_ids.len(), 2);
        assert_eq!(result.registered_listener_ids[0], 100);
        assert_eq!(result.registered_listener_ids[1], 101);

        // Verify listeners were actually registered in the router
        // by trying to create a dispatch plan (should use the registered listeners)
        let plan =
            p.create_dispatch_plan_vec(&[Change::new("data.value".to_string(), "99".to_string())]);
        // Plan should be created successfully (may have groups or not depending on matching)
        let _ = plan;
    }

    #[test]
    fn register_side_effects_complex_scenario() {
        let mut p = ProcessingPipeline::new();
        // Complex shadow state with multiple features
        p.shadow_init(
            r#"{
            "user": {"name": "Alice", "email": "alice@test.com", "role": "guest"},
            "settings": {"darkMode": true, "notifications": true},
            "items": [
                {"price": 100, "quantity": 2},
                {"price": 50, "quantity": 1}
            ],
            "totals": {},
            "sync": {}
        }"#,
        )
        .unwrap();

        // Complex registration: sync + flip + aggregation + listeners
        let reg = r#"{
            "registration_id": "complex-test",
            "sync_pairs": [
                ["user.name", "sync.userName"],
                ["user.email", "sync.userEmail"]
            ],
            "flip_pairs": [
                ["settings.darkMode", "settings.lightMode"],
                ["settings.notifications", "settings.silent"]
            ],
            "aggregation_pairs": [
                ["totals.itemCount", "items.0.quantity"],
                ["totals.itemCount", "items.1.quantity"]
            ],
            "listeners": [
                {"subscriber_id": 200, "topic_path": "user", "scope_path": "user"},
                {"subscriber_id": 201, "topic_path": "settings.darkMode", "scope_path": "settings"}
            ]
        }"#;

        let result = p.register_side_effects(reg).unwrap();

        // Verify sync was processed
        assert!(
            !result.sync_changes.is_empty(),
            "sync_changes should be computed"
        );

        // Verify aggregation was processed
        assert!(
            !result.aggregation_changes.is_empty(),
            "aggregation_changes should be computed"
        );

        // Verify both listeners were registered
        assert_eq!(result.registered_listener_ids.len(), 2);
        assert_eq!(result.registered_listener_ids[0], 200);
        assert_eq!(result.registered_listener_ids[1], 201);

        // Verify sync changes have correct paths
        let sync_paths: Vec<&str> = result
            .sync_changes
            .iter()
            .map(|c| c.path.as_str())
            .collect();
        assert!(
            sync_paths.contains(&"sync.userName") || sync_paths.contains(&"sync.userEmail"),
            "sync should create changes for registered pairs"
        );
    }

    #[test]
    fn register_concerns_validators_with_multiple_dependencies() {
        let mut p = ProcessingPipeline::new();
        p.shadow_init(
            r#"{
            "form": {
                "email": "test@test.com",
                "name": "Alice",
                "password": "secret123"
            }
        }"#,
        )
        .unwrap();

        // Register validators with multiple dependencies each
        let reg = r#"{
            "registration_id": "concerns-test",
            "bool_logics": [],
            "validators": [
                {
                    "validator_id": 300,
                    "output_path": "_concerns.form.validationState",
                    "dependency_paths": ["form.email", "form.name", "form.password"],
                    "scope": "form"
                },
                {
                    "validator_id": 301,
                    "output_path": "_concerns.form.emailState",
                    "dependency_paths": ["form.email"],
                    "scope": "form"
                }
            ]
        }"#;

        let result = p.register_concerns(reg).unwrap();

        // Verify both validators were registered
        assert_eq!(result.registered_validator_ids.len(), 2);
        assert_eq!(result.registered_validator_ids[0], 300);
        assert_eq!(result.registered_validator_ids[1], 301);

        // No bool logics
        assert_eq!(result.registered_logic_ids.len(), 0);
        assert_eq!(result.bool_logic_changes.len(), 0);
    }

    // --- WASM-032: prepare_changes + pipeline_finalize round-trip tests ---

    mod finalize_tests {
        use super::*;

        fn make_pipeline() -> ProcessingPipeline {
            let mut p = ProcessingPipeline::new();
            p.shadow_init(r#"{"user": {"role": "guest", "age": 20, "email": "test@test.com"}}"#)
                .unwrap();
            p
        }

        // --- Basic round-trip ---

        #[test]
        fn prepare_then_finalize_basic_state_change() {
            // prepare returns state_changes; finalize returns the same changes ready for valtio
            let mut p = make_pipeline();
            let changes = vec![Change::new("user.role".to_owned(), r#""admin""#.to_owned())];

            let prepare = p.prepare_changes(changes).unwrap();
            assert_eq!(prepare.state_changes.len(), 1);
            assert_eq!(prepare.state_changes[0].path, "user.role");

            let finalize = p.pipeline_finalize(vec![]).unwrap();
            assert_eq!(finalize.state_changes.len(), 1);
            assert_eq!(finalize.state_changes[0].path, "user.role");
            assert_eq!(finalize.state_changes[0].value_json, r#""admin""#);
        }

        #[test]
        fn prepare_all_noop_changes_has_work_false() {
            // All input changes equal current shadow state → diff-filtered → early exit
            let mut p = make_pipeline();
            // user.role is already "guest"
            let changes = vec![Change::new("user.role".to_owned(), r#""guest""#.to_owned())];

            let prepare = p.prepare_changes(changes).unwrap();
            assert!(!prepare.has_work);
            assert!(prepare.state_changes.is_empty());
            assert!(prepare.validators_to_run.is_empty());
            assert!(prepare.execution_plan.is_none());
        }

        #[test]
        fn finalize_empty_js_changes_passes_through_buffered_state() {
            // Finalize with no JS input still returns buffered phase-1 state changes
            let mut p = make_pipeline();
            let changes = vec![Change::new("user.age".to_owned(), "30".to_owned())];
            let _prepare = p.prepare_changes(changes).unwrap();

            let finalize = p.pipeline_finalize(vec![]).unwrap();
            assert_eq!(finalize.state_changes.len(), 1);
            assert_eq!(finalize.state_changes[0].path, "user.age");
            assert_eq!(finalize.state_changes[0].value_json, "30");
        }

        // --- BoolLogic in round-trip ---

        #[test]
        fn prepare_finalize_boollogic_concern_changes_in_output() {
            // BoolLogic result buffered in prepare → returned in finalize with _concerns. prefix
            let mut p = make_pipeline();
            p.register_boollogic(
                "_concerns.user.email.disabledWhen",
                r#"{"IS_EQUAL": ["user.role", "admin"]}"#,
            )
            .unwrap();

            let changes = vec![Change::new("user.role".to_owned(), r#""admin""#.to_owned())];
            let prepare = p.prepare_changes(changes).unwrap();

            // prepare exposes state changes and signals pending concern work
            assert!(!prepare.state_changes.is_empty());
            assert!(prepare.has_work);

            // finalize merges buffered BoolLogic results into state_changes
            let finalize = p.pipeline_finalize(vec![]).unwrap();

            let concern_changes: Vec<&Change> = finalize
                .state_changes
                .iter()
                .filter(|c| c.path.starts_with("_concerns."))
                .collect();
            assert_eq!(concern_changes.len(), 1);
            assert_eq!(concern_changes[0].path, "_concerns.user.email.disabledWhen");
            assert_eq!(concern_changes[0].value_json, "true");
        }

        #[test]
        fn finalize_concern_prefix_preserved_in_output() {
            // All concern paths in finalize.state_changes must keep _concerns. prefix
            let mut p = make_pipeline();
            p.register_boollogic(
                "_concerns.user.email.disabledWhen",
                r#"{"IS_EQUAL": ["user.role", "admin"]}"#,
            )
            .unwrap();

            let changes = vec![Change::new("user.role".to_owned(), r#""admin""#.to_owned())];
            let _prepare = p.prepare_changes(changes).unwrap();
            let finalize = p.pipeline_finalize(vec![]).unwrap();

            // Every concern path must have the full _concerns. prefix, not a stripped version
            for change in &finalize.state_changes {
                if change.path.starts_with("_concerns.") {
                    assert!(
                        change.path.len() > "_concerns.".len(),
                        "concern path must not be bare prefix: {}",
                        change.path
                    );
                }
            }
        }

        // --- Validator dispatch in round-trip ---

        #[test]
        fn prepare_includes_validators_to_run() {
            // Validator triggered by change appears in prepare.validators_to_run with dep values
            let mut p = make_pipeline();
            p.register_validators_batch(
                r#"[{
                    "validator_id": 1,
                    "output_path": "_concerns.user.email.validationState",
                    "dependency_paths": ["user.email"]
                }]"#,
            )
            .unwrap();

            let changes = vec![Change::new(
                "user.email".to_owned(),
                r#""new@test.com""#.to_owned(),
            )];
            let prepare = p.prepare_changes(changes).unwrap();

            assert_eq!(prepare.validators_to_run.len(), 1);
            assert_eq!(prepare.validators_to_run[0].validator_id, 1);
            assert_eq!(
                prepare.validators_to_run[0].output_path,
                "_concerns.user.email.validationState"
            );
            assert!(prepare.has_work);
        }

        #[test]
        fn finalize_routes_js_concern_path_to_concern_bucket() {
            // JS passes _concerns.* path in js_changes → partitioned internally →
            // re-prefixed in finalize.state_changes
            let mut p = make_pipeline();
            let changes = vec![Change::new(
                "user.email".to_owned(),
                r#""new@test.com""#.to_owned(),
            )];
            let _prepare = p.prepare_changes(changes).unwrap();

            // Simulate JS validator writing to a concern path
            let js_changes = vec![Change::new(
                "_concerns.user.email.validationState".to_owned(),
                r#"{"valid": true}"#.to_owned(),
            )];
            let finalize = p.pipeline_finalize(js_changes).unwrap();

            let concern_changes: Vec<&Change> = finalize
                .state_changes
                .iter()
                .filter(|c| c.path.starts_with("_concerns."))
                .collect();
            assert_eq!(concern_changes.len(), 1);
            assert_eq!(
                concern_changes[0].path,
                "_concerns.user.email.validationState"
            );
            assert_eq!(concern_changes[0].value_json, r#"{"valid": true}"#);
        }

        #[test]
        fn finalize_js_state_changes_merged_with_buffered() {
            // JS listener produces additional state changes → merged with phase-1 state changes
            let mut p = make_pipeline();
            let changes = vec![Change::new("user.role".to_owned(), r#""admin""#.to_owned())];
            let _prepare = p.prepare_changes(changes).unwrap();

            // Simulate JS listener producing an additional change
            let js_changes = vec![Change::new("user.age".to_owned(), "99".to_owned())];
            let finalize = p.pipeline_finalize(js_changes).unwrap();

            let paths: Vec<&str> = finalize
                .state_changes
                .iter()
                .map(|c| c.path.as_str())
                .collect();
            assert!(paths.contains(&"user.role"), "Missing phase-1 state change");
            assert!(paths.contains(&"user.age"), "Missing JS listener change");
        }

        // --- Combined BoolLogic + validator in same round-trip ---

        #[test]
        fn boollogic_and_validator_results_both_appear_in_finalize() {
            // BoolLogic (from role change) and validator result (from JS) both present in output
            let mut p = make_pipeline();
            p.register_boollogic(
                "_concerns.user.email.disabledWhen",
                r#"{"IS_EQUAL": ["user.role", "admin"]}"#,
            )
            .unwrap();
            p.register_validators_batch(
                r#"[{
                    "validator_id": 2,
                    "output_path": "_concerns.user.email.validationState",
                    "dependency_paths": ["user.email"]
                }]"#,
            )
            .unwrap();

            // Trigger both: role change (BoolLogic) + email change (validator)
            let changes = vec![
                Change::new("user.role".to_owned(), r#""admin""#.to_owned()),
                Change::new("user.email".to_owned(), r#""admin@test.com""#.to_owned()),
            ];
            let prepare = p.prepare_changes(changes).unwrap();
            assert!(prepare.has_work);
            assert_eq!(prepare.validators_to_run.len(), 1);

            // Simulate JS validator writing its result
            let js_changes = vec![Change::new(
                "_concerns.user.email.validationState".to_owned(),
                r#"{"valid": true}"#.to_owned(),
            )];
            let finalize = p.pipeline_finalize(js_changes).unwrap();

            let concern_paths: Vec<&str> = finalize
                .state_changes
                .iter()
                .filter(|c| c.path.starts_with("_concerns."))
                .map(|c| c.path.as_str())
                .collect();
            assert!(
                concern_paths.contains(&"_concerns.user.email.disabledWhen"),
                "Missing BoolLogic concern change"
            );
            assert!(
                concern_paths.contains(&"_concerns.user.email.validationState"),
                "Missing validator concern change"
            );
        }
    }
}
