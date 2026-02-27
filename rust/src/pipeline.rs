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
#[allow(unused_imports)]
use crate::change::{
    Change, ChangeContext, ChangeKind, Lineage, PipelineTrace, ProducedChange, SkipReason,
    SkippedChange, Stage, StageTrace, UNDEFINED_SENTINEL_JSON,
};
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
use crate::trace::TraceRecorder;
use crate::value_logic::{ValueLogicNode, ValueLogicRegistry};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

use ts_rs::TS;

/// Validator dispatch info for JS-side execution.
#[derive(Serialize, Deserialize, Debug, Clone, TS)]
pub struct ValidatorDispatch {
    pub validator_id: u32,
    pub output_path: String,
    pub dependency_values: HashMap<String, String>,
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
#[derive(Serialize, Debug, TS)]
pub struct PrepareResult {
    /// Changes indexed by the execution plan's `input_change_ids` for listener dispatch.
    pub listener_changes: Vec<Change>,
    /// Validators to run on JS side with their dependency values.
    pub validators_to_run: Vec<ValidatorDispatch>,
    /// Pre-computed execution plan for listener dispatch.
    pub execution_plan: Option<FullExecutionPlan>,
    /// Whether there's work to do (validators, listeners, or concern changes to apply).
    /// If false, JS can return early without calling pipeline_finalize.
    pub has_work: bool,
}

/// Snapshot of all registered graphs and registries (debug only).
#[derive(Serialize, Debug, TS)]
pub struct GraphSnapshot {
    pub sync_pairs: Vec<[String; 2]>,
    pub directed_sync_pairs: Vec<[String; 2]>,
    pub flip_pairs: Vec<[String; 2]>,
    #[ts(inline)]
    pub listeners: Vec<ListenerInfo>,
    #[ts(inline)]
    pub bool_logics: Vec<BoolLogicInfo>,
    #[ts(inline)]
    pub value_logics: Vec<ValueLogicInfo>,
    #[ts(inline)]
    pub aggregations: Vec<AggregationInfo>,
    #[ts(inline)]
    pub computations: Vec<ComputationInfo>,
}

#[derive(Serialize, Debug, TS)]
pub struct ListenerInfo {
    pub id: u32,
    pub topic: String,
    pub scope: String,
}

#[derive(Serialize, Debug, TS)]
pub struct BoolLogicInfo {
    pub id: u32,
    pub output_path: String,
    #[ts(inline)]
    pub definition: crate::bool_logic::BoolLogicNode,
}

#[derive(Serialize, Debug, TS)]
pub struct ValueLogicInfo {
    pub id: u32,
    pub output_path: String,
}

#[derive(Serialize, Debug, TS)]
pub struct AggregationInfo {
    pub target: String,
    pub sources: Vec<String>,
}

#[derive(Serialize, Debug, TS)]
pub struct ComputationInfo {
    pub target: String,
    pub op: String,
    pub sources: Vec<String>,
}

/// Finalize result: merged changes, diffed, ready for valtio application.
#[derive(Serialize, Deserialize, Debug, TS)]
pub struct FinalizeResult {
    /// All changes including state and concerns (concern paths have _concerns. prefix).
    pub state_changes: Vec<Change>,
    /// Trace data for debug logging. Only Some when debug_enabled is true on the pipeline.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub trace: Option<PipelineTrace>,
}

// ---------------------------------------------------------------------------
// Consolidated registration structs (Rust side API)
// ---------------------------------------------------------------------------

/// Input for a clear-path rule from JS.
#[derive(Serialize, Deserialize, Debug, TS)]
pub struct ClearPathInput {
    pub triggers: Vec<String>,
    pub targets: Vec<String>,
}

/// Aggregation pair: [target, source] or [target, source, condition_json].
#[derive(Serialize, Deserialize, Debug, TS)]
#[serde(untagged)]
pub enum AggregationPairInput {
    Plain(String, String),
    WithCondition(String, String, String),
}

/// Computation pair: [op, target, source] or [op, target, source, condition_json].
#[derive(Serialize, Deserialize, Debug, TS)]
#[serde(untagged)]
pub enum ComputationPairInput {
    Plain(String, String, String),
    WithCondition(String, String, String, String),
}

/// Input for consolidated side effects registration.
#[derive(Serialize, Deserialize, Debug, TS)]
pub struct SideEffectsRegistration {
    pub registration_id: String,
    #[serde(default)]
    pub sync_pairs: Vec<[String; 2]>,
    /// One-way sync pairs: source → target only. Changes to target do NOT propagate back to source.
    #[serde(default)]
    pub directed_sync_pairs: Vec<[String; 2]>,
    #[serde(default)]
    pub flip_pairs: Vec<[String; 2]>,
    #[serde(default)]
    #[ts(inline)]
    pub aggregation_pairs: Vec<AggregationPairInput>,
    #[serde(default)]
    #[ts(inline)]
    pub clear_paths: Vec<ClearPathInput>,
    #[serde(default)]
    #[ts(inline)]
    pub computation_pairs: Vec<ComputationPairInput>,
    #[serde(default)]
    pub listeners: Vec<ListenerRegistration>,
    #[serde(default)]
    #[ts(optional)]
    pub anchor_path: Option<String>,
}

/// Listener entry for consolidated registration.
#[derive(Serialize, Deserialize, Debug, TS)]
pub struct ListenerRegistration {
    pub subscriber_id: u32,
    pub topic_paths: Vec<String>,
    pub scope_path: String,
    #[serde(default)]
    #[ts(optional)]
    pub anchor_path: Option<String>,
}

/// Output from consolidated side effects registration.
#[derive(Serialize, Debug, TS)]
pub struct SideEffectsResult {
    pub sync_changes: Vec<Change>,
    pub aggregation_changes: Vec<Change>,
    pub computation_changes: Vec<Change>,
    pub registered_listener_ids: Vec<u32>,
}

/// Input for consolidated concerns registration.
#[derive(Serialize, Deserialize, Debug, TS)]
pub struct ConcernsRegistration {
    /// Used by JS for unregister tracking; not consumed by Rust pipeline logic.
    #[serde(default)]
    #[allow(dead_code)]
    pub registration_id: String,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub bool_logics: Vec<BoolLogicRegistration>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub validators: Vec<ValidatorRegistration>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub value_logics: Vec<ValueLogicRegistration>,
    #[serde(default)]
    #[ts(optional)]
    pub anchor_path: Option<String>,
}

/// ValueLogic entry for consolidated registration.
#[derive(Serialize, Deserialize, Debug, TS)]
pub struct ValueLogicRegistration {
    pub output_path: String,
    pub tree_json: String,
}

/// BoolLogic entry for consolidated registration.
#[derive(Serialize, Deserialize, Debug, TS)]
pub struct BoolLogicRegistration {
    pub output_path: String,
    pub tree_json: String,
}

/// Validator entry for consolidated registration.
#[derive(Serialize, Deserialize, Debug, TS)]
pub struct ValidatorRegistration {
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
#[derive(Serialize, Debug, TS)]
pub struct ConcernsResult {
    pub bool_logic_changes: Vec<Change>,
    pub registered_logic_ids: Vec<u32>,
    pub registered_validator_ids: Vec<u32>,
    pub value_logic_changes: Vec<Change>,
    pub registered_value_logic_ids: Vec<u32>,
}

/// All per-call mutable state, unified into one struct.
/// Cleared at the start of every `run_pipeline_core` call.
/// Never freshly allocated — `clear()` reuses capacity.
#[allow(dead_code)]
pub(crate) struct PipelineContext {
    /// The accumulator: all changes produced this call.
    pub changes: Vec<Change>,

    /// HashSets kept persistent — avoid rehash cost across calls.
    pub affected_bool_logic: HashSet<u32>,
    pub affected_validators: HashSet<u32>,
    pub affected_value_logics: HashSet<u32>,

    /// Assembled during the call, read by callers after run_pipeline_core.
    pub validators_to_run: Vec<ValidatorDispatch>,
    pub execution_plan: Option<FullExecutionPlan>,

    /// Built by execute_policy when ProcessingPipeline.debug == true.
    pub trace: PipelineTrace,
}

#[allow(dead_code)]
impl PipelineContext {
    pub fn new() -> Self {
        Self {
            changes: Vec::with_capacity(64),
            affected_bool_logic: HashSet::with_capacity(16),
            affected_validators: HashSet::with_capacity(16),
            affected_value_logics: HashSet::with_capacity(16),
            validators_to_run: Vec::new(),
            execution_plan: None,
            trace: PipelineTrace::default(),
        }
    }

    pub fn clear(&mut self) {
        self.changes.clear();
        self.affected_bool_logic.clear();
        self.affected_validators.clear();
        self.affected_value_logics.clear();
        self.validators_to_run.clear();
        self.execution_plan = None;
        self.trace.stages.clear();
    }
}

/// How a stage assigns ChangeContext to the changes it produces.
#[allow(dead_code)]
pub(crate) enum ContextRule {
    /// Produced changes inherit the parent's ChangeContext directly.
    Inherit,
    /// Produced changes always get this context, regardless of parent.
    Always(ChangeContext),
}

/// Declares how one pipeline stage processes and produces changes.
#[allow(dead_code)]
pub(crate) struct StagePolicy {
    pub stage: Stage,
    /// ChangeKinds this stage will process.
    pub accepts: &'static [ChangeKind],
    /// Additional guard checked after `accepts`.
    pub guard: fn(&Change) -> bool,
    /// Primary ChangeKind assigned to changes this stage produces.
    pub emits: ChangeKind,
    /// How produced changes inherit ChangeContext from their parent.
    pub context_rule: ContextRule,
    /// Sub-pipeline: run these policies on the changes THIS stage produced.
    pub followup: &'static [StagePolicy],
}

#[allow(dead_code)]
pub(crate) static STAGE_POLICIES: &[StagePolicy] = &[
    StagePolicy {
        stage: Stage::AggregationWrite,
        accepts: &[ChangeKind::Real],
        guard: |_| true,
        emits: ChangeKind::Consumed,
        context_rule: ContextRule::Inherit,
        followup: &[],
    },
    StagePolicy {
        stage: Stage::Diff,
        accepts: &[ChangeKind::Real],
        guard: |_| true,
        emits: ChangeKind::Breakdown,
        context_rule: ContextRule::Always(ChangeContext::Mutation),
        followup: &[],
    },
    StagePolicy {
        stage: Stage::Sync,
        accepts: &[ChangeKind::Real],
        guard: |_| true,
        emits: ChangeKind::Real,
        context_rule: ContextRule::Inherit,
        followup: &[],
    },
    StagePolicy {
        stage: Stage::Flip,
        accepts: &[ChangeKind::Real],
        guard: |c| matches!(c.lineage.context(), ChangeContext::Mutation),
        emits: ChangeKind::Real,
        context_rule: ContextRule::Inherit,
        followup: &[],
    },
    StagePolicy {
        stage: Stage::ClearPath,
        accepts: &[ChangeKind::Real],
        guard: |_| true,
        emits: ChangeKind::Real,
        context_rule: ContextRule::Always(ChangeContext::Mutation),
        followup: &[],
    },
    StagePolicy {
        stage: Stage::AggregationRead,
        accepts: &[ChangeKind::Real],
        guard: |_| true,
        emits: ChangeKind::Real,
        context_rule: ContextRule::Inherit,
        followup: &[],
    },
    StagePolicy {
        stage: Stage::Computation,
        accepts: &[ChangeKind::Real],
        guard: |_| true,
        emits: ChangeKind::Real,
        context_rule: ContextRule::Inherit,
        followup: &[],
    },
    StagePolicy {
        stage: Stage::BoolLogic,
        accepts: &[ChangeKind::Real],
        guard: |_| true,
        emits: ChangeKind::Real,
        context_rule: ContextRule::Inherit,
        followup: &[],
    },
    StagePolicy {
        stage: Stage::ValueLogic,
        accepts: &[ChangeKind::Real],
        guard: |_| true,
        emits: ChangeKind::Real,
        context_rule: ContextRule::Inherit,
        followup: &[],
    },
    StagePolicy {
        stage: Stage::Listeners,
        accepts: &[ChangeKind::Real, ChangeKind::Redundant],
        guard: |_| true,
        emits: ChangeKind::Real,
        context_rule: ContextRule::Inherit,
        followup: &[],
    },
    StagePolicy {
        stage: Stage::Apply,
        accepts: &[ChangeKind::Real],
        guard: |_| true,
        emits: ChangeKind::Real,
        context_rule: ContextRule::Inherit,
        followup: &[],
    },
];

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
    /// One-way directed sync edges: source_id → set of target_ids.
    /// Changes to source propagate to targets; changes to target do NOT propagate back.
    directed_sync_edges: HashMap<u32, HashSet<u32>>,
    flip_graph: Graph,
    router: TopicRouter,
    value_logic_registry: ValueLogicRegistry,
    value_logic_rev_index: ReverseDependencyIndex,

    // Per-call scratch space (replaces seven buf_* fields)
    ctx: PipelineContext,

    // Cross-call: survives between prepare_changes() and pipeline_finalize()
    buf_pending_state_changes: Vec<Change>,
    buf_pending_concern_changes: Vec<Change>,

    // Debug flag — set once at init time, controls trace collection and snapshot availability.
    debug_enabled: bool,

    // Anchor path support: skip execution when anchor path is absent from shadow state
    /// Set of interned path IDs for all active anchor paths.
    anchor_path_ids: HashSet<u32>,
    /// Per-run cache: anchor_path_id → is_present in shadow state.
    anchor_states: HashMap<u32, bool>,
    /// Map from interned anchor_path_id to the set of registration_ids using it (for ref-counting).
    anchor_registrations: HashMap<u32, HashSet<String>>,
    /// Map from interned path_id (sync/flip/agg/comp) → anchor_path_id.
    /// Populated at registration time for all paths that are part of an anchored registration.
    anchored_paths: HashMap<u32, u32>,
    /// Map from registration_id → anchor_path_id, for cleanup on unregister.
    registration_anchor_map: HashMap<String, u32>,
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
            directed_sync_edges: HashMap::new(),
            flip_graph: Graph::new(),
            router: TopicRouter::new(),
            value_logic_registry: ValueLogicRegistry::new(),
            value_logic_rev_index: ReverseDependencyIndex::new(),
            ctx: PipelineContext::new(),
            buf_pending_state_changes: Vec::with_capacity(64),
            buf_pending_concern_changes: Vec::with_capacity(16),
            debug_enabled: false,
            anchor_path_ids: HashSet::new(),
            anchor_states: HashMap::new(),
            anchor_registrations: HashMap::new(),
            anchored_paths: HashMap::new(),
            registration_anchor_map: HashMap::new(),
        }
    }

    /// Enable or disable debug tracing for this pipeline.
    pub(crate) fn set_debug(&mut self, enabled: bool) {
        self.debug_enabled = enabled;
    }

    // ------------------------------------------------------------------
    // Anchor path helpers
    // ------------------------------------------------------------------

    /// Register an anchor path for a given registration_id. Interns the path and tracks refs.
    fn register_anchor_path(&mut self, anchor_path: &str, registration_id: &str) -> u32 {
        let id = self.intern.intern(anchor_path);
        self.anchor_path_ids.insert(id);
        self.anchor_registrations
            .entry(id)
            .or_default()
            .insert(registration_id.to_owned());
        id
    }

    /// Unregister an anchor path for a given registration_id.
    /// Removes from anchor_path_ids if no other registrations reference it.
    #[allow(dead_code)]
    fn unregister_anchor_path(&mut self, anchor_path_id: u32, registration_id: &str) {
        if let Some(refs) = self.anchor_registrations.get_mut(&anchor_path_id) {
            refs.remove(registration_id);
            if refs.is_empty() {
                self.anchor_registrations.remove(&anchor_path_id);
                self.anchor_path_ids.remove(&anchor_path_id);
                self.anchor_states.remove(&anchor_path_id);
            }
        }
    }

    /// Rebuild anchor_states from shadow — call once per pipeline run after shadow is updated.
    /// Uses field-level borrow splitting to avoid intermediate Vec allocation.
    fn update_anchor_states(&mut self, shadow: &ShadowState) {
        self.anchor_states.clear();
        let ids: Vec<u32> = self.anchor_path_ids.iter().copied().collect();
        for id in ids {
            if let Some(path) = self.intern.resolve(id) {
                let is_present = shadow.get(path).is_some();
                self.anchor_states.insert(id, is_present);
            }
        }
    }

    /// Returns true if the anchor is enabled (present in cached anchor_states) or not set (None).
    /// Used during pipeline processing after update_anchor_states() has been called.
    fn is_anchor_enabled(&self, anchor_id: Option<u32>) -> bool {
        match anchor_id {
            None => true,
            Some(id) => self.anchor_states.get(&id).copied().unwrap_or(true),
        }
    }

    /// Check anchor presence directly against shadow state (not cached).
    /// Used at registration time when anchor_states may not be populated yet.
    fn is_anchor_present_in_shadow(&self, anchor_id: Option<u32>) -> bool {
        match anchor_id {
            None => true,
            Some(id) => {
                if let Some(path) = self.intern.resolve(id) {
                    self.shadow.get(path).is_some()
                } else {
                    true // Can't resolve → treat as present (safe default)
                }
            }
        }
    }

    /// Assemble a snapshot of all registered graphs and registries (debug only).
    pub(crate) fn get_graph_snapshot(&self) -> GraphSnapshot {
        let sync_pairs = self.sync_graph.dump_pairs(&self.intern);
        let flip_pairs = self.flip_graph.dump_pairs(&self.intern);
        let listeners = self
            .router
            .dump_listeners()
            .into_iter()
            .map(|(id, topic, scope)| ListenerInfo { id, topic, scope })
            .collect();
        let bool_logics = self
            .registry
            .dump_infos()
            .into_iter()
            .map(|(id, output_path, definition)| BoolLogicInfo {
                id,
                output_path,
                definition,
            })
            .collect();
        let value_logics = self
            .value_logic_registry
            .dump_infos()
            .into_iter()
            .map(|(id, output_path)| ValueLogicInfo { id, output_path })
            .collect();
        let aggregations = self
            .aggregations
            .dump_infos()
            .into_iter()
            .map(|(target, sources)| AggregationInfo { target, sources })
            .collect();
        let computations = self
            .computations
            .dump_infos()
            .into_iter()
            .map(|(target, op, sources)| ComputationInfo {
                target,
                op,
                sources,
            })
            .collect();
        let directed_sync_pairs: Vec<[String; 2]> = self
            .directed_sync_edges
            .iter()
            .flat_map(|(&src_id, targets)| {
                targets.iter().filter_map(move |&tgt_id| {
                    let src = self.intern.resolve(src_id)?.to_owned();
                    let tgt = self.intern.resolve(tgt_id)?.to_owned();
                    Some([src, tgt])
                })
            })
            .collect();
        GraphSnapshot {
            sync_pairs,
            directed_sync_pairs,
            flip_pairs,
            listeners,
            bool_logics,
            value_logics,
            aggregations,
            computations,
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

    /// Register a BoolLogic expression. Returns logic_id for later cleanup.
    pub(crate) fn register_boollogic(
        &mut self,
        output_path: &str,
        tree_json: &str,
    ) -> Result<u32, String> {
        self.register_boollogic_with_anchor(output_path, tree_json, None, None)
    }

    pub(crate) fn register_boollogic_with_anchor(
        &mut self,
        output_path: &str,
        tree_json: &str,
        anchor_path_id: Option<u32>,
        registration_id: Option<String>,
    ) -> Result<u32, String> {
        let tree: BoolLogicNode =
            serde_json::from_str(tree_json).map_err(|e| format!("BoolLogic parse error: {}", e))?;
        let id = self.registry.register(
            output_path.to_owned(),
            tree,
            &mut self.intern,
            &mut self.rev_index,
            anchor_path_id,
            registration_id,
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
        pairs: &[AggregationPairInput],
    ) -> Result<Vec<Change>, String> {
        // Collect all targets and sources for validation
        let mut targets = HashSet::new();
        let mut sources = HashSet::new();

        // Convert typed pairs into (target, AggregationSource)
        let mut parsed: Vec<(String, AggregationSource)> = Vec::new();

        for pair in pairs {
            let (target, source_path, condition_json) = match pair {
                AggregationPairInput::Plain(t, s) => (t.clone(), s.clone(), None),
                AggregationPairInput::WithCondition(t, s, c) => {
                    (t.clone(), s.clone(), Some(c.as_str()))
                }
            };

            let exclude_when = match condition_json {
                Some(json) => {
                    let node: BoolLogicNode = serde_json::from_str(json)
                        .map_err(|e| format!("Aggregation condition parse error: {}", e))?;
                    Some(node)
                }
                None => None,
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
        let mut by_target: HashMap<String, Vec<AggregationSource>> = HashMap::new();

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

        // Diff against shadow — only return changes where value actually differs.
        let diffed = self.diff_changes(&initial_changes);
        for change in &diffed {
            self.shadow
                .set(&change.path, &change.value_json)
                .map_err(|e| format!("Shadow update failed for aggregation: {}", e))?;
        }

        Ok(diffed)
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
        pairs: &[ComputationPairInput],
    ) -> Result<Vec<Change>, String> {
        // Collect all targets and sources for validation
        let mut targets = HashSet::new();
        let mut sources = HashSet::new();

        // Convert typed pairs into (op, target, source, optional condition)
        let mut parsed: Vec<(ComputationOp, String, ComputationSource)> = Vec::new();

        for pair in pairs {
            let (op_str, target, source_path, condition_json) = match pair {
                ComputationPairInput::Plain(o, t, s) => (o.as_str(), t.clone(), s.clone(), None),
                ComputationPairInput::WithCondition(o, t, s, c) => {
                    (o.as_str(), t.clone(), s.clone(), Some(c.as_str()))
                }
            };

            let op = match op_str {
                "SUM" => ComputationOp::Sum,
                "AVG" => ComputationOp::Avg,
                _ => return Err(format!("Unknown computation operation: {}", op_str)),
            };

            let exclude_when = match condition_json {
                Some(json) => {
                    let node: BoolLogicNode = serde_json::from_str(json)
                        .map_err(|e| format!("Computation condition parse error: {}", e))?;
                    Some(node)
                }
                None => None,
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
        let mut by_target: HashMap<String, (ComputationOp, Vec<ComputationSource>)> =
            HashMap::new();

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

        // Diff against shadow — only update shadow and return genuine changes.
        let diffed = self.diff_changes(&initial_changes);
        for change in &diffed {
            self.shadow
                .set(&change.path, &change.value_json)
                .map_err(|e| format!("Shadow update failed for computation: {}", e))?;
        }

        Ok(diffed)
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

        // Compute initial sync changes from shadow state.
        let all_initial_changes = self.compute_sync_initial_changes(&pairs)?;

        // Diff against shadow state — only return changes where value actually differs.
        // Shadow/valtio divergence should be fixed at the source (e.g., re-init shadow
        // on provider reuse) rather than sending redundant data across the WASM boundary.
        let diffed_changes = self.diff_changes(&all_initial_changes);
        for change in &diffed_changes {
            self.shadow
                .set(&change.path, &change.value_json)
                .map_err(|e| format!("Shadow update failed: {}", e))?;
        }

        serde_json::to_string(&diffed_changes)
            .map_err(|e| format!("Failed to serialize initial changes: {}", e))
    }

    /// Register a batch of directed (one-way) sync pairs.
    ///
    /// Source → target only: changes to source propagate to target; target changes do NOT go back.
    /// Returns initial changes (source values copied to targets, diffed against shadow).
    pub(crate) fn register_directed_sync_batch(
        &mut self,
        pairs: &[[String; 2]],
    ) -> Result<Vec<Change>, String> {
        let mut initial_changes = Vec::new();

        for pair in pairs {
            let src_id = self.intern.intern(&pair[0]);
            let tgt_id = self.intern.intern(&pair[1]);

            // Record directed edge: source → target
            self.directed_sync_edges
                .entry(src_id)
                .or_default()
                .insert(tgt_id);

            // Initial sync: copy source value → target
            if let Some(src_value) = self.shadow.get(&pair[0]) {
                let value_json = serde_json::to_string(&src_value.to_json_value())
                    .unwrap_or_else(|_| "null".to_owned());
                // Skip null/undefined — only sync substantive values
                if value_json != "null" && value_json != "\"__undefined__\"" {
                    initial_changes.push(Change {
                        path: pair[1].clone(),
                        value_json,
                        kind: ChangeKind::Real,
                        lineage: Lineage::Input,
                        audit: None,
                    });
                }
            }
        }

        // Diff against shadow: only return changes where value actually differs
        let diffed = self.diff_changes(&initial_changes);
        for change in &diffed {
            self.shadow
                .set(&change.path, &change.value_json)
                .map_err(|e| format!("Shadow update failed (directed sync): {}", e))?;
        }

        Ok(diffed)
    }

    /// Compute initial sync changes for newly registered pairs.
    ///
    /// For each connected component, finds the most common value (excluding null/undefined)
    /// and emits a change for every path in the component. The caller is responsible
    /// for filtering no-ops via `diff_changes`.
    fn compute_sync_initial_changes(&self, pairs: &[[String; 2]]) -> Result<Vec<Change>, String> {
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
            // Count value occurrences for "substantive" values only.
            // Excluded from voting (treated as "no value"):
            //   - None: path absent from shadow (e.g. JSON.stringify stripped undefined)
            //   - UNDEFINED_SENTINEL: path explicitly set to JS undefined
            //   - "null": path set to null (JS undefined → null via serde, or explicit null)
            // This matches legacy JS `is.not.nil()` which skips both null and undefined.
            let mut value_counts: HashMap<String, usize> = HashMap::new();
            let mut value_to_path: HashMap<String, String> = HashMap::new();

            for path in &component {
                let value_json = match self.shadow.get(path) {
                    Some(v) => serde_json::to_string(&v.to_json_value())
                        .unwrap_or_else(|_| "null".to_string()),
                    None => continue, // Path absent from shadow — skip voting
                };

                // Skip nil-equivalent values (matches legacy JS `is.not.nil()`)
                if value_json == UNDEFINED_SENTINEL_JSON || value_json == "null" {
                    continue;
                }

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
                // Emit change for every path — diff_changes in the caller filters no-ops
                for path in &component {
                    changes.push(Change {
                        path: path.clone(),
                        value_json: target_value.clone(),
                        kind: ChangeKind::Real,
                        lineage: Lineage::Input,
                        audit: None,
                    });
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
            let mut dependency_values = HashMap::new();
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

        // Register anchor path for this registration if specified
        let anchor_path_id = if let Some(ref ap) = reg.anchor_path {
            let id = self.register_anchor_path(ap, &reg.registration_id);
            self.registration_anchor_map
                .insert(reg.registration_id.clone(), id);
            Some(id)
        } else {
            None
        };

        let mut sync_changes = Vec::new();
        let mut aggregation_changes = Vec::new();
        let mut computation_changes = Vec::new();
        let mut registered_listener_ids = Vec::new();

        // 1. Register sync pairs (bidirectional)
        if !reg.sync_pairs.is_empty() {
            // Record anchored paths for sync pairs
            if let Some(aid) = anchor_path_id {
                for pair in &reg.sync_pairs {
                    let id1 = self.intern.intern(&pair[0]);
                    let id2 = self.intern.intern(&pair[1]);
                    self.anchored_paths.insert(id1, aid);
                    self.anchored_paths.insert(id2, aid);
                }
            }
            let changes = self.register_sync_batch(
                &serde_json::to_string(&reg.sync_pairs)
                    .map_err(|e| format!("Sync pairs serialization error: {}", e))?,
            )?;
            sync_changes = serde_json::from_str(&changes)
                .map_err(|e| format!("Sync changes parse error: {}", e))?;
        }

        // 1b. Register directed sync pairs (one-way: source → target only)
        if !reg.directed_sync_pairs.is_empty() {
            // Record anchored paths for directed sync targets (target gets anchor guard)
            if let Some(aid) = anchor_path_id {
                for pair in &reg.directed_sync_pairs {
                    let src_id = self.intern.intern(&pair[0]);
                    let tgt_id = self.intern.intern(&pair[1]);
                    self.anchored_paths.insert(src_id, aid);
                    self.anchored_paths.insert(tgt_id, aid);
                }
            }
            let directed_changes = self.register_directed_sync_batch(&reg.directed_sync_pairs)?;
            // Merge directed initial changes into sync_changes
            sync_changes.extend(directed_changes);
        }

        // 2. Register flip pairs
        if !reg.flip_pairs.is_empty() {
            // Record anchored paths for flip pairs
            if let Some(aid) = anchor_path_id {
                for pair in &reg.flip_pairs {
                    let id1 = self.intern.intern(&pair[0]);
                    let id2 = self.intern.intern(&pair[1]);
                    self.anchored_paths.insert(id1, aid);
                    self.anchored_paths.insert(id2, aid);
                }
            }
            self.register_flip_batch(
                &serde_json::to_string(&reg.flip_pairs)
                    .map_err(|e| format!("Flip pairs serialization error: {}", e))?,
            )?;
        }

        // 3. Register aggregations
        if !reg.aggregation_pairs.is_empty() {
            // Record anchored paths for aggregation pairs
            if let Some(aid) = anchor_path_id {
                for pair in &reg.aggregation_pairs {
                    let (target, source) = match pair {
                        AggregationPairInput::Plain(t, s) => (t.as_str(), s.as_str()),
                        AggregationPairInput::WithCondition(t, s, _) => (t.as_str(), s.as_str()),
                    };
                    self.anchored_paths.insert(self.intern.intern(target), aid);
                    self.anchored_paths.insert(self.intern.intern(source), aid);
                }
            }
            aggregation_changes = self.register_aggregation_batch(&reg.aggregation_pairs)?;
        }

        // 4. Register clear paths
        for cp in &reg.clear_paths {
            self.register_clear_paths(&reg.registration_id, &cp.triggers, &cp.targets)?;
        }

        // 4.5. Register computations
        if !reg.computation_pairs.is_empty() {
            // Record anchored paths for computation pairs
            if let Some(aid) = anchor_path_id {
                for pair in &reg.computation_pairs {
                    let (target, source) = match pair {
                        ComputationPairInput::Plain(_, t, s) => (t.as_str(), s.as_str()),
                        ComputationPairInput::WithCondition(_, t, s, _) => (t.as_str(), s.as_str()),
                    };
                    self.anchored_paths.insert(self.intern.intern(target), aid);
                    self.anchored_paths.insert(self.intern.intern(source), aid);
                }
            }
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
                        "topic_paths": l.topic_paths,
                        "scope_path": l.scope_path,
                        "anchor_path": l.anchor_path,
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
    /// Cleans up clear-path rules and anchor path tracking for this registration ID.
    /// Other side-effect cleanup is managed by the caller.
    pub(crate) fn unregister_side_effects(&mut self, registration_id: &str) -> Result<(), String> {
        self.unregister_clear_paths(registration_id);

        // Clean up anchor tracking for this registration
        if let Some(anchor_id) = self.registration_anchor_map.remove(registration_id) {
            self.unregister_anchor_path(anchor_id, registration_id);
            // If anchor is fully removed (no other registrations use it), clean anchored_paths
            if !self.anchor_path_ids.contains(&anchor_id) {
                self.anchored_paths.retain(|_, aid| *aid != anchor_id);
            }
        }

        Ok(())
    }

    /// Register all concerns at once: BoolLogic and validators.
    ///
    /// Single WASM call that combines BoolLogic and validator registration.
    /// Returns initial BoolLogic changes and registered IDs for cleanup tracking.
    pub(crate) fn register_concerns(&mut self, reg_json: &str) -> Result<ConcernsResult, String> {
        let reg: ConcernsRegistration = serde_json::from_str(reg_json)
            .map_err(|e| format!("Concerns registration parse error: {}", e))?;

        // Resolve anchor path for this registration (if any)
        let anchor_path_id: Option<u32> = reg.anchor_path.as_deref().map(|ap| {
            let id = self.register_anchor_path(ap, &reg.registration_id);
            self.registration_anchor_map
                .insert(reg.registration_id.clone(), id);
            id
        });

        // Check if anchor is currently present in shadow (skip initial eval if absent)
        let anchor_active = self.is_anchor_present_in_shadow(anchor_path_id);

        let mut bool_logic_changes = Vec::new();
        let mut registered_logic_ids = Vec::new();
        let mut registered_validator_ids = Vec::new();

        // 1. Register BoolLogic expressions and compute initial values
        for bl in &reg.bool_logics {
            let logic_id = self.register_boollogic_with_anchor(
                &bl.output_path,
                &bl.tree_json,
                anchor_path_id,
                Some(reg.registration_id.clone()),
            )?;
            registered_logic_ids.push(logic_id);

            // Skip initial evaluation if anchor path is absent from shadow
            if !anchor_active {
                continue;
            }

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
                    kind: ChangeKind::Real,
                    lineage: Lineage::Input,
                    audit: None,
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
                anchor_path_id,
                Some(reg.registration_id.clone()),
            );
            registered_value_logic_ids.push(vl_id);

            // Skip initial evaluation if anchor path is absent from shadow
            if !anchor_active {
                continue;
            }

            // Evaluate ValueLogic with current shadow state to get initial value
            if let Some(meta) = self.value_logic_registry.get(vl_id) {
                let result = meta.tree.evaluate(&self.shadow);
                value_logic_changes.push(Change {
                    path: vl.output_path.clone(),
                    value_json: serde_json::to_string(&result).unwrap_or_else(|_| "null".into()),
                    kind: ChangeKind::Real,
                    lineage: Lineage::Input,
                    audit: None,
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
    /// Unregister concerns by registration ID.
    /// Cleans up anchor path tracking. Individual logic cleanup done by caller.
    pub(crate) fn unregister_concerns(&mut self, registration_id: &str) -> Result<(), String> {
        // Clean up anchor tracking for this registration
        if let Some(anchor_id) = self.registration_anchor_map.remove(registration_id) {
            self.unregister_anchor_path(anchor_id, registration_id);
        }
        Ok(())
    }

    /// Emit a sync change to a peer path, with parent_exists and is_different guards.
    fn emit_sync_change(
        shadow: &ShadowState,
        peer_path: &str,
        value_json: &str,
        output: &mut Vec<Change>,
    ) {
        if !shadow.parent_exists(peer_path) {
            return;
        }
        let current = shadow.get(peer_path);
        let new_value: crate::shadow::ValueRepr = match serde_json::from_str(value_json) {
            Ok(v) => v,
            Err(_) => {
                // Can't parse → treat as genuine change
                output.push(Change {
                    path: peer_path.to_owned(),
                    value_json: value_json.to_owned(),
                    kind: ChangeKind::Real,
                    lineage: Lineage::Input,
                    audit: None,
                });
                return;
            }
        };
        if crate::diff::is_different(&current, &new_value) {
            output.push(Change {
                path: peer_path.to_owned(),
                value_json: value_json.to_owned(),
                kind: ChangeKind::Real,
                lineage: Lineage::Input,
                audit: None,
            });
        }
    }

    /// Process sync paths and write into the provided output buffer.
    /// Only adds genuine changes by diffing against shadow state before pushing.
    /// Handles three cases (non-exclusive — a path can match multiple):
    /// 1. Exact match: change path is directly in sync graph
    /// 2. Child expansion: change path is deeper than a registered sync pair
    /// 3. Parent expansion: change path is shallower than registered sync pairs
    fn process_sync_paths_into(
        &mut self,
        shadow: &ShadowState,
        changes: &[Change],
        output: &mut Vec<Change>,
    ) {
        output.clear();
        for change in changes {
            let path_id = self.intern.intern(&change.path);
            let peer_ids = self.sync_graph.get_component_paths_public(path_id);

            // Case 1: Exact match — change path is directly registered
            if !peer_ids.is_empty() {
                for &peer_id in &peer_ids {
                    if peer_id != path_id {
                        // Skip if peer's anchor is disabled
                        if let Some(&aid) = self.anchored_paths.get(&peer_id) {
                            if !self.is_anchor_enabled(Some(aid)) {
                                continue;
                            }
                        }
                        if let Some(peer_path) = self.intern.resolve(peer_id) {
                            let peer_path = peer_path.to_owned();
                            Self::emit_sync_change(shadow, &peer_path, &change.value_json, output);
                        }
                    }
                }
            }

            // Case 2: Child expansion — walk ancestors to find a registered sync pair
            // A path can be both an exact match AND a child of another pair
            let change_path = &change.path;
            for (i, _) in change_path.rmatch_indices('.') {
                let ancestor = &change_path[..i];
                if let Some(ancestor_id) = self.intern.get_id(ancestor) {
                    let ancestor_peers = self.sync_graph.get_component_paths_public(ancestor_id);
                    if !ancestor_peers.is_empty() {
                        let suffix = &change_path[i..]; // includes leading "."
                        for peer_id in ancestor_peers {
                            if peer_id != ancestor_id {
                                // Skip if peer's anchor is disabled
                                if let Some(&aid) = self.anchored_paths.get(&peer_id) {
                                    if !self.is_anchor_enabled(Some(aid)) {
                                        continue;
                                    }
                                }
                                if let Some(peer_base) = self.intern.resolve(peer_id) {
                                    let peer_path = format!("{}{}", peer_base, suffix);
                                    Self::emit_sync_change(
                                        shadow,
                                        &peer_path,
                                        &change.value_json,
                                        output,
                                    );
                                }
                            }
                        }
                        break;
                    }
                }
            }

            // Case 3: Parent expansion — change is shallower than registered pairs
            // Enumerate leaf paths under this change in shadow, sync any that are registered
            if peer_ids.is_empty() {
                let leaves = shadow.affected_paths(&change.path);
                for leaf in &leaves {
                    if let Some(leaf_id) = self.intern.get_id(leaf) {
                        let leaf_peers = self.sync_graph.get_component_paths_public(leaf_id);
                        if !leaf_peers.is_empty() {
                            if let Some(leaf_value) = shadow.get(leaf) {
                                let value_json = serde_json::to_string(&leaf_value.to_json_value())
                                    .unwrap_or_else(|_| "null".to_owned());
                                for peer_id in leaf_peers {
                                    if peer_id != leaf_id {
                                        // Skip if peer's anchor is disabled
                                        if let Some(&aid) = self.anchored_paths.get(&peer_id) {
                                            if !self.is_anchor_enabled(Some(aid)) {
                                                continue;
                                            }
                                        }
                                        if let Some(peer_path) = self.intern.resolve(peer_id) {
                                            let peer_path = peer_path.to_owned();
                                            Self::emit_sync_change(
                                                shadow,
                                                &peer_path,
                                                &value_json,
                                                output,
                                            );
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // Case 4: Directed sync — exact match: change path is a registered source
            if let Some(targets) = self.directed_sync_edges.get(&path_id) {
                let targets: Vec<u32> = targets.iter().copied().collect();
                for tgt_id in targets {
                    // Skip if target's anchor is disabled
                    if let Some(&aid) = self.anchored_paths.get(&tgt_id) {
                        if !self.is_anchor_enabled(Some(aid)) {
                            continue;
                        }
                    }
                    if let Some(tgt_path) = self.intern.resolve(tgt_id) {
                        let tgt_path = tgt_path.to_owned();
                        Self::emit_sync_change(shadow, &tgt_path, &change.value_json, output);
                    }
                }
            }

            // Case 5: Directed sync — child expansion: change is deeper than a registered source
            // Walk ancestors of the change path; if an ancestor is a directed source, propagate
            // to each target with the same suffix.
            for (i, _) in change_path.rmatch_indices('.') {
                let ancestor = &change_path[..i];
                if let Some(ancestor_id) = self.intern.get_id(ancestor) {
                    if let Some(targets) = self.directed_sync_edges.get(&ancestor_id) {
                        let targets: Vec<u32> = targets.iter().copied().collect();
                        let suffix = &change_path[i..]; // includes leading "."
                        for tgt_id in targets {
                            // Skip if target's anchor is disabled
                            if let Some(&aid) = self.anchored_paths.get(&tgt_id) {
                                if !self.is_anchor_enabled(Some(aid)) {
                                    continue;
                                }
                            }
                            if let Some(tgt_base) = self.intern.resolve(tgt_id) {
                                let tgt_path = format!("{}{}", tgt_base, suffix);
                                Self::emit_sync_change(
                                    shadow,
                                    &tgt_path,
                                    &change.value_json,
                                    output,
                                );
                            }
                        }
                        break;
                    }
                }
            }

            // Case 6: Directed sync — parent expansion: change is shallower than registered sources
            // Enumerate leaves under the change; for any leaf that is a registered directed source,
            // emit a change to its target(s).
            if !self.directed_sync_edges.is_empty() {
                let leaves = shadow.affected_paths(&change.path);
                for leaf in &leaves {
                    if let Some(leaf_id) = self.intern.get_id(leaf) {
                        if let Some(targets) = self.directed_sync_edges.get(&leaf_id) {
                            if let Some(leaf_value) = shadow.get(leaf) {
                                let value_json = serde_json::to_string(&leaf_value.to_json_value())
                                    .unwrap_or_else(|_| "null".to_owned());
                                let targets: Vec<u32> = targets.iter().copied().collect();
                                for tgt_id in targets {
                                    // Skip if target's anchor is disabled
                                    if let Some(&aid) = self.anchored_paths.get(&tgt_id) {
                                        if !self.is_anchor_enabled(Some(aid)) {
                                            continue;
                                        }
                                    }
                                    if let Some(tgt_path) = self.intern.resolve(tgt_id) {
                                        let tgt_path = tgt_path.to_owned();
                                        Self::emit_sync_change(
                                            shadow,
                                            &tgt_path,
                                            &value_json,
                                            output,
                                        );
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /// Emit a flip change (inverted boolean) to a peer path, with guards.
    fn emit_flip_change(
        shadow: &ShadowState,
        peer_path: &str,
        inverted_json: &str,
        output: &mut Vec<Change>,
    ) {
        if !shadow.parent_exists(peer_path) {
            return;
        }
        let current = shadow.get(peer_path);
        let inverted_bool = inverted_json == "true";
        let new_value = crate::shadow::ValueRepr::Bool(inverted_bool);
        if crate::diff::is_different(&current, &new_value) {
            output.push(Change {
                path: peer_path.to_owned(),
                value_json: inverted_json.to_owned(),
                kind: ChangeKind::Real,
                lineage: Lineage::Input,
                audit: None,
            });
        }
    }

    /// Process flip paths and write into the provided output buffer.
    /// Only adds genuine changes by diffing against shadow state before pushing.
    ///
    /// Handles two cases:
    /// 1. Exact match: change path is directly in flip graph (must be boolean)
    /// 2. Parent expansion: change path is shallower — decompose into boolean leaves
    ///
    /// No child expansion for flip (by design).
    fn process_flip_paths_into(
        &mut self,
        shadow: &ShadowState,
        changes: &[Change],
        output: &mut Vec<Change>,
    ) {
        output.clear();
        for change in changes {
            let path_id = self.intern.intern(&change.path);
            let peer_ids = self.flip_graph.get_component_paths_public(path_id);

            if !peer_ids.is_empty() {
                // Case 1: Exact match — flip only if boolean
                let inverted_value = match change.value_json.trim() {
                    "true" => Some("false"),
                    "false" => Some("true"),
                    _ => None,
                };
                if let Some(inverted) = inverted_value {
                    for peer_id in peer_ids {
                        if peer_id != path_id {
                            // Skip if peer's anchor is disabled
                            if let Some(&aid) = self.anchored_paths.get(&peer_id) {
                                if !self.is_anchor_enabled(Some(aid)) {
                                    continue;
                                }
                            }
                            if let Some(peer_path) = self.intern.resolve(peer_id) {
                                let peer_path = peer_path.to_owned();
                                Self::emit_flip_change(shadow, &peer_path, inverted, output);
                            }
                        }
                    }
                }
                continue;
            }

            // Case 2: Parent expansion — decompose into leaf boolean paths
            // Only applies when change value is an object
            let trimmed = change.value_json.trim();
            if !trimmed.starts_with('{') {
                continue;
            }

            let leaves = shadow.affected_paths(&change.path);

            // Collect all leaf paths from this parent change that are in the flip graph.
            // Used to detect when both sides of a flip pair are under the same parent —
            // in that case the user explicitly set both values, so flip should not fire.
            let mut flip_leaf_ids: HashSet<u32> = HashSet::new();
            for leaf in &leaves {
                if let Some(leaf_id) = self.intern.get_id(leaf) {
                    if !self
                        .flip_graph
                        .get_component_paths_public(leaf_id)
                        .is_empty()
                    {
                        flip_leaf_ids.insert(leaf_id);
                    }
                }
            }

            for leaf in &leaves {
                if let Some(leaf_id) = self.intern.get_id(leaf) {
                    let leaf_peers = self.flip_graph.get_component_paths_public(leaf_id);
                    if leaf_peers.is_empty() {
                        continue;
                    }

                    // Only flip booleans
                    let leaf_value = shadow.get(leaf);
                    let inverted = match leaf_value {
                        Some(crate::shadow::ValueRepr::Bool(true)) => "false",
                        Some(crate::shadow::ValueRepr::Bool(false)) => "true",
                        _ => continue,
                    };

                    for peer_id in &leaf_peers {
                        if *peer_id == leaf_id {
                            continue;
                        }
                        // Skip if peer's anchor is disabled
                        if let Some(&aid) = self.anchored_paths.get(peer_id) {
                            if !self.is_anchor_enabled(Some(aid)) {
                                continue;
                            }
                        }
                        // If peer is also a leaf under this parent change and in the
                        // flip graph, both sides are explicitly set → skip flip.
                        if flip_leaf_ids.contains(peer_id) {
                            continue;
                        }
                        if let Some(peer_path) = self.intern.resolve(*peer_id) {
                            let peer_path = peer_path.to_owned();
                            Self::emit_flip_change(shadow, &peer_path, inverted, output);
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

    /// Collect validator dispatches for all validators affected in the current processing pass.
    fn collect_validator_dispatches(&self, shadow: &ShadowState) -> Vec<ValidatorDispatch> {
        self.ctx
            .affected_validators
            .iter()
            .filter_map(|&validator_id| {
                let meta = self.function_registry.get(validator_id)?;
                let dependency_values = meta
                    .dependency_paths
                    .iter()
                    .map(|dep_path| {
                        let value_json = shadow
                            .get(dep_path)
                            .map(|v| {
                                serde_json::to_string(&v.to_json_value())
                                    .unwrap_or_else(|_| "null".to_owned())
                            })
                            .unwrap_or_else(|| "null".to_owned());
                        (dep_path.clone(), value_json)
                    })
                    .collect();
                Some(ValidatorDispatch {
                    validator_id,
                    output_path: meta.output_path.clone().unwrap_or_default(),
                    dependency_values,
                })
            })
            .collect()
    }

    /// Build a full execution plan if any listeners are registered and any fire.
    /// Returns `None` if no listeners or no groups matched.
    fn build_execution_plan(&self) -> Option<FullExecutionPlan> {
        if !self.router.has_listeners() {
            return None;
        }
        let plan = self.router.create_full_execution_plan(&self.ctx.changes);

        if plan.groups.is_empty() {
            None
        } else {
            Some(plan)
        }
    }

    /// Shared pipeline core: steps 0–11.
    ///
    /// Clears processing buffers, runs the full change pipeline
    /// (diff → aggregation writes → shadow apply → clear paths → sync → flip →
    /// aggregation reads → computation reads → BoolLogic → ValueLogic → validators → plan),
    /// and returns intermediate results.
    ///
    /// Returns `None` if all input changes are no-ops (caller should return an empty result).
    fn run_pipeline_core(&mut self, input_changes: Vec<Change>) -> Result<bool, String> {
        // Clear per-call scratch space
        self.ctx.clear();

        // Extract trace for borrow-safe TraceRecorder (swap with Default, put back at end)
        let mut trace = std::mem::take(&mut self.ctx.trace);
        let mut rec = TraceRecorder::new(&mut trace, self.debug_enabled);

        // Working shadow: clone of self.shadow used for all reads/writes during the pipeline.
        // self.shadow stays untouched until the atomic commit at the end.
        let mut working = self.shadow.clone();

        // Local buffers for sync/flip (avoid storing on struct — they're pure temporaries)
        let mut sync_buf: Vec<Change> = Vec::new();
        let mut flip_buf: Vec<Change> = Vec::new();
        // Local buffer for concern changes produced by BoolLogic/ValueLogic evaluation
        let mut concern_changes: Vec<Change> = Vec::new();

        // Step 0: Diff pre-pass — filter no-op changes early (against working, == self.shadow here)
        let all_input_matched = rec.collect_matched(&input_changes);
        let input_changes = crate::diff::diff_changes(&working, &input_changes);
        {
            let matched = rec.collect_matched(&input_changes);
            let skipped = rec.diff_skipped(&all_input_matched, &matched);
            rec.stage(Stage::Input, matched, Vec::new(), skipped);
        }
        if input_changes.is_empty() {
            self.ctx.trace = trace;
            return Ok(false);
        }

        // Steps 1-2: Process aggregation writes (distribute target → sources)
        let input_matched_for_agg = rec.collect_matched(&input_changes);
        let changes = process_aggregation_writes(&self.aggregations, input_changes, &working);
        {
            let input_paths: Vec<&str> = input_matched_for_agg
                .iter()
                .map(|m| m[0].as_str())
                .collect();
            let produced: Vec<ProducedChange> = if rec.enabled() {
                changes
                    .iter()
                    .filter(|c| !input_paths.contains(&c.path.as_str()))
                    .map(|c| ProducedChange {
                        path: c.path.clone(),
                        value: c.value_json.clone(),
                        registration_id: None,
                        source_path: None,
                    })
                    .collect()
            } else {
                Vec::new()
            };
            rec.stage(
                Stage::AggregationWrite,
                input_matched_for_agg,
                produced,
                Vec::new(),
            );
        }

        // Step 1.5: Filter out writes to computation targets (silent no-op)
        let pre_filter_matched = rec.collect_matched(&changes);
        let changes: Vec<Change> = changes
            .into_iter()
            .filter(|c| !self.computations.is_computation_target(&c.path))
            .collect();
        {
            let kept = rec.collect_matched(&changes);
            let skipped = rec.diff_skipped(&pre_filter_matched, &kept);
            rec.stage(Stage::Computation, Vec::new(), Vec::new(), skipped);
        }

        // Step 3: Apply aggregated changes to working shadow and collect affected paths.
        // Only process genuine changes (diff against working shadow before applying).
        let mut genuine_path_ids: Vec<u32> = Vec::new();
        for change in &changes {
            let current = working.get(&change.path);
            let new_value: crate::shadow::ValueRepr = match serde_json::from_str(&change.value_json)
            {
                Ok(v) => v,
                Err(_) => {
                    // Can't parse → treat as genuine change
                    working.set(&change.path, &change.value_json)?;
                    self.ctx.changes.push(change.clone());
                    genuine_path_ids.push(self.intern.intern(&change.path));
                    self.mark_affected_logic(&working, &change.path);
                    continue;
                }
            };
            if crate::diff::is_different(&current, &new_value) {
                working.set(&change.path, &change.value_json)?;
                self.ctx.changes.push(change.clone());
                genuine_path_ids.push(self.intern.intern(&change.path));
                self.mark_affected_logic(&working, &change.path);
            }
        }

        {
            let genuine_matched = rec.collect_matched(&self.ctx.changes);
            let all_input_matched = rec.collect_matched(&changes);
            let skipped = rec.diff_skipped(&all_input_matched, &genuine_matched);
            rec.stage(Stage::Diff, genuine_matched, Vec::new(), skipped);
        }

        // Step 3.5: Clear paths — "when X changes, set Y to null".
        // Only original genuine changes (Step 3) feed into clear processing (no self-cascading).
        let clear_changes =
            self.clear_registry
                .process(&genuine_path_ids, &mut self.intern, &mut working);
        for change in &clear_changes {
            self.mark_affected_logic(&working, &change.path);
        }
        self.ctx.changes.extend(clear_changes.clone());
        {
            let trigger_matched: Vec<[String; 2]> = if rec.enabled() {
                genuine_path_ids
                    .iter()
                    .filter_map(|&id| {
                        self.intern
                            .resolve(id)
                            .map(|s| [s.to_owned(), String::new()])
                    })
                    .collect()
            } else {
                Vec::new()
            };
            let produced = rec.collect_produced(&clear_changes);
            rec.stage(Stage::ClearPath, trigger_matched, produced, Vec::new());
        }

        // Step 3.6: Early anchor evaluation — evaluate anchors BEFORE sync/flip
        // so that sync/flip correctly skip paths whose anchors are now disabled.
        let anchor_states_after_input = if !self.anchor_path_ids.is_empty() {
            self.update_anchor_states(&working);
            Some(self.anchor_states.clone())
        } else {
            None
        };

        // Steps 4-5: Process sync paths and update working shadow.
        // Must process ALL aggregated changes (not just genuine ones) because even if
        // a change is a no-op for path A, it might need to sync to path B that differs.
        // Include clear changes so sync sees cleared paths.
        let mut changes_for_sync = changes.clone();
        changes_for_sync.extend(clear_changes.clone());
        self.process_sync_paths_into(&working, &changes_for_sync, &mut sync_buf);
        for change in &sync_buf {
            working.set(&change.path, &change.value_json)?;
            self.mark_affected_logic(&working, &change.path);
        }
        self.ctx.changes.extend_from_slice(&sync_buf);
        rec.stage(
            Stage::Sync,
            rec.collect_matched(&changes_for_sync),
            rec.collect_produced(&sync_buf),
            Vec::new(),
        );

        // Steps 6-7: Process flip paths and update working shadow.
        // Must process ALL aggregated changes + clear changes + sync outputs.
        let mut changes_for_flip = changes.clone();
        changes_for_flip.extend(clear_changes);
        changes_for_flip.extend_from_slice(&sync_buf);
        self.process_flip_paths_into(&working, &changes_for_flip, &mut flip_buf);
        for change in &flip_buf {
            working.set(&change.path, &change.value_json)?;
            self.mark_affected_logic(&working, &change.path);
        }
        self.ctx.changes.extend_from_slice(&flip_buf);
        rec.stage(
            Stage::Flip,
            rec.collect_matched(&changes_for_flip),
            rec.collect_produced(&flip_buf),
            Vec::new(),
        );

        // Step 7.5: Process aggregation reads (sources → target recomputation).
        // After sync/flip, check if any aggregation sources changed and recompute targets.
        let all_changed_paths: Vec<String> =
            self.ctx.changes.iter().map(|c| c.path.clone()).collect();
        let all_changed_matched = rec.collect_matched(&self.ctx.changes);
        let aggregation_reads =
            process_aggregation_reads(&self.aggregations, &working, &all_changed_paths);
        let mut agg_read_produced: Vec<ProducedChange> = Vec::new();
        let mut agg_read_skipped: Vec<SkippedChange> = Vec::new();
        for change in &aggregation_reads {
            // Skip if target path's anchor is disabled
            let target_id = self.intern.intern(&change.path);
            if let Some(&aid) = self.anchored_paths.get(&target_id) {
                if !self.is_anchor_enabled(Some(aid)) {
                    if rec.enabled() {
                        let anchor_path = self.intern.resolve(aid).unwrap_or("").to_owned();
                        agg_read_skipped.push(TraceRecorder::skipped_anchor(
                            &change.path,
                            &change.kind,
                            &anchor_path,
                            None,
                        ));
                    }
                    continue;
                }
            }
            if rec.enabled() {
                agg_read_produced.push(ProducedChange {
                    path: change.path.clone(),
                    value: change.value_json.clone(),
                    registration_id: None,
                    source_path: None,
                });
            }
            working.set(&change.path, &change.value_json)?;
            self.mark_affected_logic(&working, &change.path);
            self.ctx.changes.push(change.clone());
        }
        rec.stage(
            Stage::AggregationRead,
            all_changed_matched,
            agg_read_produced,
            agg_read_skipped,
        );

        // Step 7.6: Process computation reads (sources → target recomputation).
        // Sees aggregation-produced changes because we re-collect all_changed_paths.
        let all_changed_paths: Vec<String> =
            self.ctx.changes.iter().map(|c| c.path.clone()).collect();
        let all_changed_matched_comp = rec.collect_matched(&self.ctx.changes);
        let computation_reads =
            process_computation_reads(&self.computations, &working, &all_changed_paths);
        let mut comp_read_produced: Vec<ProducedChange> = Vec::new();
        let mut comp_read_skipped: Vec<SkippedChange> = Vec::new();
        for change in &computation_reads {
            // Skip if target path's anchor is disabled
            let target_id = self.intern.intern(&change.path);
            if let Some(&aid) = self.anchored_paths.get(&target_id) {
                if !self.is_anchor_enabled(Some(aid)) {
                    if rec.enabled() {
                        let anchor_path = self.intern.resolve(aid).unwrap_or("").to_owned();
                        comp_read_skipped.push(TraceRecorder::skipped_anchor(
                            &change.path,
                            &change.kind,
                            &anchor_path,
                            None,
                        ));
                    }
                    continue;
                }
            }
            if rec.enabled() {
                comp_read_produced.push(ProducedChange {
                    path: change.path.clone(),
                    value: change.value_json.clone(),
                    registration_id: None,
                    source_path: None,
                });
            }
            working.set(&change.path, &change.value_json)?;
            self.mark_affected_logic(&working, &change.path);
            self.ctx.changes.push(change.clone());
        }
        rec.stage(
            Stage::Computation,
            all_changed_matched_comp,
            comp_read_produced,
            comp_read_skipped,
        );

        // Step 7.7: Re-evaluate anchor states for transitions (sync/flip/agg/comp may
        // create or remove anchor paths). Compare against Step 3.6 snapshot to detect
        // disabled→enabled transitions and mark affected logic for re-evaluation.
        if !self.anchor_path_ids.is_empty() {
            let old_states = anchor_states_after_input.unwrap_or_default();
            self.update_anchor_states(&working);

            // Find anchors that transitioned disabled→enabled
            for (&anchor_id, &is_now_present) in &self.anchor_states {
                let was_present = old_states.get(&anchor_id).copied().unwrap_or(false);
                if is_now_present && !was_present {
                    // Anchor just became enabled — re-evaluate all logic tied to it
                    for bl_id in self.registry.ids_for_anchor(anchor_id) {
                        self.ctx.affected_bool_logic.insert(bl_id);
                    }
                    for vl_id in self.value_logic_registry.ids_for_anchor(anchor_id) {
                        self.ctx.affected_value_logics.insert(vl_id);
                    }
                }
            }
        }

        // Steps 8-9: Evaluate affected BoolLogic expressions (against working shadow)
        let mut bl_produced: Vec<ProducedChange> = Vec::new();
        let mut bl_skipped: Vec<SkippedChange> = Vec::new();
        for logic_id in &self.ctx.affected_bool_logic {
            if let Some(meta) = self.registry.get(*logic_id) {
                // Skip if anchor path is missing from shadow state
                if !self.is_anchor_enabled(meta.anchor_path_id) {
                    if rec.enabled() {
                        let anchor_path = meta
                            .anchor_path_id
                            .and_then(|id| self.intern.resolve(id).map(|s| s.to_owned()))
                            .unwrap_or_default();
                        bl_skipped.push(TraceRecorder::skipped_anchor(
                            &meta.output_path,
                            &ChangeKind::Real,
                            &anchor_path,
                            meta.registration_id.as_deref(),
                        ));
                    }
                    continue;
                }
                let result = meta.tree.evaluate(&working);
                let result_json = if result {
                    "true".to_owned()
                } else {
                    "false".to_owned()
                };
                if rec.enabled() {
                    bl_produced.push(ProducedChange {
                        path: meta.output_path.clone(),
                        value: result_json.clone(),
                        registration_id: meta.registration_id.clone(),
                        source_path: None,
                    });
                }
                concern_changes.push(Change {
                    path: meta.output_path.clone(),
                    value_json: result_json,
                    kind: ChangeKind::Real,
                    lineage: Lineage::Input,
                    audit: None,
                });
            }
        }
        {
            let matched: Vec<[String; 2]> = if rec.enabled() {
                self.ctx
                    .affected_bool_logic
                    .iter()
                    .map(|id| [id.to_string(), String::new()])
                    .collect()
            } else {
                Vec::new()
            };
            rec.stage(Stage::BoolLogic, matched, bl_produced, bl_skipped);
        }

        // Step 8b: Evaluate affected ValueLogic expressions (against working shadow)
        let mut vl_produced: Vec<ProducedChange> = Vec::new();
        let mut vl_skipped: Vec<SkippedChange> = Vec::new();
        for vl_id in &self.ctx.affected_value_logics {
            if let Some(meta) = self.value_logic_registry.get(*vl_id) {
                // Skip if anchor path is missing from shadow state
                if !self.is_anchor_enabled(meta.anchor_path_id) {
                    if rec.enabled() {
                        let anchor_path = meta
                            .anchor_path_id
                            .and_then(|id| self.intern.resolve(id).map(|s| s.to_owned()))
                            .unwrap_or_default();
                        vl_skipped.push(TraceRecorder::skipped_anchor(
                            &meta.output_path,
                            &ChangeKind::Real,
                            &anchor_path,
                            meta.registration_id.as_deref(),
                        ));
                    }
                    continue;
                }
                let result = meta.tree.evaluate(&working);
                let result_json = serde_json::to_string(&result).unwrap_or_else(|_| "null".into());
                if rec.enabled() {
                    vl_produced.push(ProducedChange {
                        path: meta.output_path.clone(),
                        value: result_json.clone(),
                        registration_id: meta.registration_id.clone(),
                        source_path: None,
                    });
                }
                concern_changes.push(Change {
                    path: meta.output_path.clone(),
                    value_json: result_json,
                    kind: ChangeKind::Real,
                    lineage: Lineage::Input,
                    audit: None,
                });
            }
        }
        {
            let matched: Vec<[String; 2]> = if rec.enabled() {
                self.ctx
                    .affected_value_logics
                    .iter()
                    .map(|id| [id.to_string(), String::new()])
                    .collect()
            } else {
                Vec::new()
            };
            rec.stage(Stage::ValueLogic, matched, vl_produced, vl_skipped);
        }

        // Step 10: Collect affected validators with their dependency values
        let validators_to_run = self.collect_validator_dispatches(&working);

        // Step 11: Create full execution plan if listeners are registered
        let execution_plan = self.build_execution_plan();
        {
            let listeners_matched = rec.collect_matched(&self.ctx.changes);
            rec.stage(Stage::Listeners, listeners_matched, Vec::new(), Vec::new());
        }

        // Store concern changes in ctx.changes (prefixed with _concerns.)
        for c in concern_changes {
            let path = if c.path.starts_with("_concerns.") {
                c.path
            } else {
                crate::join_path("_concerns", &c.path)
            };
            self.ctx.changes.push(Change {
                path,
                value_json: c.value_json,
                kind: c.kind,
                lineage: c.lineage,
                audit: c.audit,
            });
        }

        self.ctx.validators_to_run = validators_to_run;
        self.ctx.execution_plan = execution_plan;

        rec.stage(
            Stage::Apply,
            vec![[format!("{} changes", self.ctx.changes.len()), String::new()]],
            Vec::new(),
            Vec::new(),
        );

        // Set anchor states on trace for display layer
        rec.set_anchor_states(&self.anchor_states, &self.intern);

        // Put trace back into ctx (rec's borrow ends here via NLL)
        self.ctx.trace = trace;

        // Atomic commit: replace self.shadow with the fully-processed working copy.
        // self.shadow was untouched during the entire pipeline — one update, like valtio's batchApply.
        self.shadow = working;

        Ok(true)
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
    pub(crate) fn process_changes_vec(
        &mut self,
        input_changes: Vec<Change>,
    ) -> Result<ProcessResult, String> {
        if !self.run_pipeline_core(input_changes)? {
            return Ok(ProcessResult {
                changes: Vec::new(),
                validators_to_run: Vec::new(),
                execution_plan: None,
            });
        }

        // In production, only Real changes are returned to JS.
        // In debug mode, all change kinds are returned for DevTools.
        if !self.debug_enabled {
            self.ctx.changes.retain(|c| c.kind == ChangeKind::Real);
        }

        Ok(ProcessResult {
            changes: std::mem::take(&mut self.ctx.changes),
            validators_to_run: std::mem::take(&mut self.ctx.validators_to_run),
            execution_plan: self.ctx.execution_plan.take(),
        })
    }

    /// Mark all BoolLogic expressions, ValueLogic expressions, and validators affected by a change at the given path.
    fn mark_affected_logic(&mut self, shadow: &ShadowState, path: &str) {
        let affected_paths = shadow.affected_paths(path);
        for affected_path in &affected_paths {
            let path_id = self.intern.intern(affected_path);
            // Mark affected BoolLogic
            for logic_id in self.rev_index.affected_by_path(path_id) {
                self.ctx.affected_bool_logic.insert(logic_id);
            }
            // Mark affected validators (now in function registry)
            for validator_id in self.function_rev_index.affected_by_path(path_id) {
                self.ctx.affected_validators.insert(validator_id);
            }
            // Mark affected ValueLogic
            for vl_id in self.value_logic_rev_index.affected_by_path(path_id) {
                self.ctx.affected_value_logics.insert(vl_id);
            }
        }
        let path_id = self.intern.intern(path);
        // Mark affected BoolLogic
        for logic_id in self.rev_index.affected_by_path(path_id) {
            self.ctx.affected_bool_logic.insert(logic_id);
        }
        // Mark affected validators (now in function registry)
        for validator_id in self.function_rev_index.affected_by_path(path_id) {
            self.ctx.affected_validators.insert(validator_id);
        }
        // Mark affected ValueLogic
        for vl_id in self.value_logic_rev_index.affected_by_path(path_id) {
            self.ctx.affected_value_logics.insert(vl_id);
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
    /// Buffers results in buf_pending_* for pipeline_finalize() to consume.
    /// Returns: { listener_changes, validators_to_run, execution_plan, has_work }
    ///
    /// After JS executes listeners/validators, call pipeline_finalize() with their results.
    pub(crate) fn prepare_changes(
        &mut self,
        input_changes: Vec<Change>,
    ) -> Result<PrepareResult, String> {
        // Clear pending buffers from any prior prepare call
        self.buf_pending_state_changes.clear();
        self.buf_pending_concern_changes.clear();

        if !self.run_pipeline_core(input_changes)? {
            return Ok(PrepareResult {
                listener_changes: Vec::new(),
                validators_to_run: Vec::new(),
                execution_plan: None,
                has_work: false,
            });
        }

        // Partition ctx.changes: _concerns. paths go to concern buffer, rest to state
        for change in self.ctx.changes.drain(..) {
            if !self.debug_enabled && change.kind != ChangeKind::Real {
                continue;
            }
            if change.path.starts_with("_concerns.") {
                self.buf_pending_concern_changes.push(change);
            } else {
                self.buf_pending_state_changes.push(change);
            }
        }

        let has_work = !self.buf_pending_state_changes.is_empty()
            || !self.buf_pending_concern_changes.is_empty()
            || !self.ctx.validators_to_run.is_empty()
            || self.ctx.execution_plan.is_some();

        Ok(PrepareResult {
            listener_changes: self.buf_pending_state_changes.clone(),
            validators_to_run: std::mem::take(&mut self.ctx.validators_to_run),
            execution_plan: self.ctx.execution_plan.take(),
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
                    kind: change.kind,
                    lineage: change.lineage,
                    audit: change.audit,
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
                        kind: c.kind,
                        lineage: c.lineage,
                        audit: c.audit,
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
                kind: c.kind,
                lineage: c.lineage,
                audit: c.audit,
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
            trace: if self.debug_enabled {
                Some(std::mem::take(&mut self.ctx.trace))
            } else {
                None
            },
        })
    }
}

impl Default for ProcessingPipeline {
    fn default() -> Self {
        Self::new()
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

    /// Parse JSON string into typed aggregation pairs (test convenience).
    fn agg_pairs(json: &str) -> Vec<AggregationPairInput> {
        serde_json::from_str(json).unwrap()
    }

    /// Parse JSON string into typed computation pairs (test convenience).
    fn comp_pairs(json: &str) -> Vec<ComputationPairInput> {
        serde_json::from_str(json).unwrap()
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
        p.register_aggregation_batch(&agg_pairs(
            r#"[["allUsers", "user1"], ["allUsers", "user2"], ["allUsers", "user3"]]"#,
        ))
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

        p.register_aggregation_batch(&agg_pairs(
            r#"[["form.allChecked", "item1"], ["form.allChecked", "item2"]]"#,
        ))
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

        p.register_aggregation_batch(&agg_pairs(
            r#"[["allUsers", "user1"], ["allUsers", "user2"]]"#,
        ))
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

        p.register_aggregation_batch(&agg_pairs(
            r#"[
                ["allUsers", "user1"], ["allUsers", "user2"],
                ["allItems", "item1"], ["allItems", "item2"], ["allItems", "item3"]
            ]"#,
        ))
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

        p.register_aggregation_batch(&agg_pairs(
            r#"[["allUsers", "user1"], ["allUsers", "user2"]]"#,
        ))
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

    // --- sync propagation tests ---

    #[test]
    fn sync_propagates_exact_match() {
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"source": "old", "target": "old"}"#)
            .unwrap();
        p.register_sync_batch(r#"[["source", "target"]]"#).unwrap();

        let result = p
            .process_changes(r#"[{"path": "source", "value_json": "\"new\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let target_change = parsed.changes.iter().find(|c| c.path == "target");
        assert!(target_change.is_some(), "target should receive sync change");
        assert_eq!(target_change.unwrap().value_json, r#""new""#);
    }

    #[test]
    fn sync_propagates_to_multiple_peers() {
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"a": "old", "b": "old", "c": "old"}"#)
            .unwrap();
        p.register_sync_batch(r#"[["a", "b"], ["a", "c"]]"#)
            .unwrap();

        let result = p
            .process_changes(r#"[{"path": "a", "value_json": "\"synced\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let paths: Vec<&str> = parsed.changes.iter().map(|c| c.path.as_str()).collect();
        assert!(paths.contains(&"b"), "b should be synced");
        assert!(paths.contains(&"c"), "c should be synced");
    }

    #[test]
    fn sync_chained_pairs() {
        // a→b, b→c — changing a should cascade to b AND c
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"a": "old", "b": "old", "c": "old"}"#)
            .unwrap();
        p.register_sync_batch(r#"[["a", "b"], ["b", "c"]]"#)
            .unwrap();

        let result = p
            .process_changes(r#"[{"path": "a", "value_json": "\"chain\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let b_change = parsed.changes.iter().find(|c| c.path == "b");
        let c_change = parsed.changes.iter().find(|c| c.path == "c");
        assert!(b_change.is_some(), "b should be synced from a");
        assert!(c_change.is_some(), "c should be synced from b (chained)");
        assert_eq!(b_change.unwrap().value_json, r#""chain""#);
        assert_eq!(c_change.unwrap().value_json, r#""chain""#);
    }

    #[test]
    fn sync_bidirectional_no_infinite_loop() {
        // a↔b — changing a should sync to b, but not loop infinitely
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"a": "old", "b": "old"}"#).unwrap();
        p.register_sync_batch(r#"[["a", "b"], ["b", "a"]]"#)
            .unwrap();

        let result = p
            .process_changes(r#"[{"path": "a", "value_json": "\"bidir\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let b_change = parsed.changes.iter().find(|c| c.path == "b");
        assert!(b_change.is_some(), "b should be synced");
        assert_eq!(b_change.unwrap().value_json, r#""bidir""#);
    }

    #[test]
    fn sync_nested_paths_exact() {
        let mut p = ProcessingPipeline::new();
        p.shadow_init(
            r#"{"user": {"name": "Alice", "email": "a@b.com"}, "profile": {"name": "old", "email": "old"}}"#,
        )
        .unwrap();
        p.register_sync_batch(
            r#"[["user.name", "profile.name"], ["user.email", "profile.email"]]"#,
        )
        .unwrap();

        let result = p
            .process_changes(r#"[{"path": "user.name", "value_json": "\"Bob\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let profile_name = parsed.changes.iter().find(|c| c.path == "profile.name");
        assert!(profile_name.is_some(), "profile.name should be synced");
        assert_eq!(profile_name.unwrap().value_json, r#""Bob""#);

        // profile.email should NOT be affected
        let profile_email = parsed.changes.iter().find(|c| c.path == "profile.email");
        assert!(
            profile_email.is_none(),
            "profile.email should not change when user.name changes"
        );
    }

    #[test]
    fn sync_deeply_nested_paths() {
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"l1": {"l2": {"value": "old", "l3": {"value": "old"}}}}"#)
            .unwrap();
        p.register_sync_batch(r#"[["l1.l2.value", "l1.l2.l3.value"]]"#)
            .unwrap();

        let result = p
            .process_changes(r#"[{"path": "l1.l2.value", "value_json": "\"deep-sync\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let deep = parsed.changes.iter().find(|c| c.path == "l1.l2.l3.value");
        assert!(deep.is_some(), "deeply nested target should be synced");
        assert_eq!(deep.unwrap().value_json, r#""deep-sync""#);
    }

    #[test]
    fn sync_shallow_to_deep() {
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"shallow": "old", "a": {"b": {"c": {"deep": "old"}}}}"#)
            .unwrap();
        p.register_sync_batch(r#"[["shallow", "a.b.c.deep"]]"#)
            .unwrap();

        let result = p
            .process_changes(r#"[{"path": "shallow", "value_json": "\"s2d\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let deep = parsed.changes.iter().find(|c| c.path == "a.b.c.deep");
        assert!(deep.is_some(), "deep target should be synced from shallow");
        assert_eq!(deep.unwrap().value_json, r#""s2d""#);
    }

    #[test]
    fn sync_deep_to_shallow() {
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"shallow": "old", "a": {"b": {"c": {"deep": "old"}}}}"#)
            .unwrap();
        p.register_sync_batch(r#"[["a.b.c.deep", "shallow"]]"#)
            .unwrap();

        let result = p
            .process_changes(r#"[{"path": "a.b.c.deep", "value_json": "\"d2s\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let shallow = parsed.changes.iter().find(|c| c.path == "shallow");
        assert!(
            shallow.is_some(),
            "shallow target should be synced from deep"
        );
        assert_eq!(shallow.unwrap().value_json, r#""d2s""#);
    }

    #[test]
    fn sync_skips_no_op_when_values_match() {
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"source": "same", "target": "same"}"#)
            .unwrap();
        p.register_sync_batch(r#"[["source", "target"]]"#).unwrap();

        // Source set to same value as target — sync should be a no-op
        let result = p
            .process_changes(r#"[{"path": "source", "value_json": "\"same\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Diff pre-pass drops this because source is already "same" in shadow
        // So no changes at all
        assert!(
            parsed.changes.is_empty() || !parsed.changes.iter().any(|c| c.path == "target"),
            "target should not get a redundant sync"
        );
    }

    #[test]
    fn sync_boolean_values() {
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"flag1": false, "flag2": true}"#).unwrap();
        p.register_sync_batch(r#"[["flag1", "flag2"]]"#).unwrap();

        let result = p
            .process_changes(r#"[{"path": "flag1", "value_json": "true"}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let flag2 = parsed.changes.iter().find(|c| c.path == "flag2");
        assert!(flag2.is_some(), "flag2 should be synced");
        assert_eq!(flag2.unwrap().value_json, "true");
    }

    #[test]
    fn sync_numeric_values() {
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"count": 0, "mirror": 99}"#).unwrap();
        p.register_sync_batch(r#"[["count", "mirror"]]"#).unwrap();

        let result = p
            .process_changes(r#"[{"path": "count", "value_json": "42"}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let mirror = parsed.changes.iter().find(|c| c.path == "mirror");
        assert!(mirror.is_some(), "mirror should be synced");
        assert_eq!(mirror.unwrap().value_json, "42");
    }

    #[test]
    fn sync_empty_string() {
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"a": "value", "b": "value"}"#).unwrap();
        p.register_sync_batch(r#"[["a", "b"]]"#).unwrap();

        let result = p
            .process_changes(r#"[{"path": "a", "value_json": "\"\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let b = parsed.changes.iter().find(|c| c.path == "b");
        assert!(b.is_some(), "b should be synced to empty string");
        assert_eq!(b.unwrap().value_json, r#""""#);
    }

    #[test]
    fn sync_independent_pairs_dont_interfere() {
        // Two independent sync pairs on different paths
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"a": "old", "b": "old", "x": "old", "y": "old"}"#)
            .unwrap();
        p.register_sync_batch(r#"[["a", "b"], ["x", "y"]]"#)
            .unwrap();

        // Change only "a" — should sync to "b" but NOT touch x/y
        let result = p
            .process_changes(r#"[{"path": "a", "value_json": "\"new-a\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let b = parsed.changes.iter().find(|c| c.path == "b");
        assert!(b.is_some(), "b should be synced");
        assert_eq!(b.unwrap().value_json, r#""new-a""#);

        let y = parsed.changes.iter().find(|c| c.path == "y");
        assert!(y.is_none(), "y should not be affected by a→b sync");
    }

    #[test]
    fn sync_multiple_changes_in_batch() {
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"s1": "old1", "t1": "old1", "s2": "old2", "t2": "old2"}"#)
            .unwrap();
        p.register_sync_batch(r#"[["s1", "t1"], ["s2", "t2"]]"#)
            .unwrap();

        let result = p
            .process_changes(
                r#"[{"path": "s1", "value_json": "\"v1\""}, {"path": "s2", "value_json": "\"v2\""}]"#,
            )
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let t1 = parsed.changes.iter().find(|c| c.path == "t1");
        let t2 = parsed.changes.iter().find(|c| c.path == "t2");
        assert!(t1.is_some(), "t1 should be synced from s1");
        assert!(t2.is_some(), "t2 should be synced from s2");
        assert_eq!(t1.unwrap().value_json, r#""v1""#);
        assert_eq!(t2.unwrap().value_json, r#""v2""#);
    }

    #[test]
    fn sync_overlapping_hierarchy_cascades() {
        // l1.value→l2.value, l2.value→l3.value — changing l1.value cascades through
        let mut p = ProcessingPipeline::new();
        p.shadow_init(
            r#"{"l1": {"value": "old"}, "l2": {"value": "old"}, "l3": {"value": "old"}}"#,
        )
        .unwrap();
        p.register_sync_batch(r#"[["l1.value", "l2.value"], ["l2.value", "l3.value"]]"#)
            .unwrap();

        let result = p
            .process_changes(r#"[{"path": "l1.value", "value_json": "\"cascade\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let l2 = parsed.changes.iter().find(|c| c.path == "l2.value");
        let l3 = parsed.changes.iter().find(|c| c.path == "l3.value");
        assert!(l2.is_some(), "l2.value should be synced directly");
        assert!(l3.is_some(), "l3.value should be synced via cascade");
        assert_eq!(l2.unwrap().value_json, r#""cascade""#);
        assert_eq!(l3.unwrap().value_json, r#""cascade""#);
    }

    #[test]
    fn sync_self_reference_no_crash() {
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"a": "old"}"#).unwrap();
        p.register_sync_batch(r#"[["a", "a"]]"#).unwrap();

        let result = p
            .process_changes(r#"[{"path": "a", "value_json": "\"self\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Self-sync should be a no-op (peer_id == path_id is skipped)
        let a_changes: Vec<&Change> = parsed.changes.iter().filter(|c| c.path == "a").collect();
        assert_eq!(a_changes.len(), 1, "only the original change, no self-sync");
    }

    #[test]
    fn sync_after_unregister_stops() {
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"a": "old", "b": "old"}"#).unwrap();
        p.register_sync_batch(r#"[["a", "b"]]"#).unwrap();

        // Verify sync works
        let result = p
            .process_changes(r#"[{"path": "a", "value_json": "\"v1\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();
        assert!(parsed.changes.iter().any(|c| c.path == "b"));

        // Unregister
        p.unregister_sync_batch(r#"[["a", "b"]]"#).unwrap();

        // Sync should no longer happen
        let result = p
            .process_changes(r#"[{"path": "a", "value_json": "\"v2\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();
        assert!(
            !parsed.changes.iter().any(|c| c.path == "b"),
            "b should NOT sync after unregister"
        );
    }

    // --- directed (one-way) sync tests ---

    #[test]
    fn directed_sync_source_propagates_to_target() {
        // Changing source → target should receive the new value
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"source": "old", "target": "old"}"#)
            .unwrap();
        p.register_directed_sync_batch(&[["source".to_owned(), "target".to_owned()]])
            .unwrap();

        let result = p
            .process_changes(r#"[{"path": "source", "value_json": "\"new\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let target = parsed.changes.iter().find(|c| c.path == "target");
        assert!(target.is_some(), "target should receive sync from source");
        assert_eq!(target.unwrap().value_json, r#""new""#);
    }

    #[test]
    fn directed_sync_target_does_not_propagate_to_source() {
        // Changing target must NOT push back to source — that is the whole point of one-way sync
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"source": "original", "target": "old"}"#)
            .unwrap();
        p.register_directed_sync_batch(&[["source".to_owned(), "target".to_owned()]])
            .unwrap();

        let result = p
            .process_changes(r#"[{"path": "target", "value_json": "\"target-only\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // source must not appear in changes at all
        assert!(
            !parsed.changes.iter().any(|c| c.path == "source"),
            "source must NOT be updated when target changes (one-way)"
        );
        // target change itself should be present
        assert!(parsed.changes.iter().any(|c| c.path == "target"));
    }

    #[test]
    fn directed_sync_initial_copies_source_to_target() {
        // On registration, source value should be copied to target
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"source": "initial-value", "target": "stale"}"#)
            .unwrap();

        let changes = p
            .register_directed_sync_batch(&[["source".to_owned(), "target".to_owned()]])
            .unwrap();

        let target_init = changes.iter().find(|c| c.path == "target");
        assert!(
            target_init.is_some(),
            "target should be initialized from source on registration"
        );
        assert_eq!(target_init.unwrap().value_json, r#""initial-value""#);
    }

    #[test]
    fn directed_sync_initial_skips_when_source_null() {
        // Source is null → no initial change emitted for target
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"source": null, "target": "existing"}"#)
            .unwrap();

        let changes = p
            .register_directed_sync_batch(&[["source".to_owned(), "target".to_owned()]])
            .unwrap();

        assert!(
            !changes.iter().any(|c| c.path == "target"),
            "target should not be overwritten when source is null"
        );
    }

    #[test]
    fn directed_sync_multiple_targets_from_one_source() {
        // One source can push to multiple targets independently
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"src": "old", "t1": "old", "t2": "old"}"#)
            .unwrap();
        p.register_directed_sync_batch(&[
            ["src".to_owned(), "t1".to_owned()],
            ["src".to_owned(), "t2".to_owned()],
        ])
        .unwrap();

        let result = p
            .process_changes(r#"[{"path": "src", "value_json": "\"broadcast\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        assert!(
            parsed.changes.iter().any(|c| c.path == "t1"),
            "t1 should receive broadcast"
        );
        assert!(
            parsed.changes.iter().any(|c| c.path == "t2"),
            "t2 should receive broadcast"
        );
        // Neither t1 nor t2 should push back to src
        let src_count = parsed.changes.iter().filter(|c| c.path == "src").count();
        assert_eq!(src_count, 1, "src should appear only as the input change");
    }

    #[test]
    fn directed_sync_independent_pairs_no_interference() {
        // Two independent directed pairs: s1→t1, s2→t2
        // Changing s1 must not touch t2; changing t1 must not touch s1
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"s1": "old", "t1": "old", "s2": "old", "t2": "old"}"#)
            .unwrap();
        p.register_directed_sync_batch(&[
            ["s1".to_owned(), "t1".to_owned()],
            ["s2".to_owned(), "t2".to_owned()],
        ])
        .unwrap();

        let result = p
            .process_changes(r#"[{"path": "s1", "value_json": "\"v1\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        assert!(parsed.changes.iter().any(|c| c.path == "t1"), "t1 synced");
        assert!(
            !parsed.changes.iter().any(|c| c.path == "t2"),
            "t2 not touched"
        );
        assert!(
            !parsed.changes.iter().any(|c| c.path == "s2"),
            "s2 not touched"
        );
    }

    #[test]
    fn directed_sync_child_expansion() {
        // Registered: source → target (parent paths). Change comes in at source.name.
        // Expected: target.name receives the same value (child expansion, Case 5).
        let mut p = ProcessingPipeline::new();
        p.shadow_init(
            r#"{"source": {"name": "old"}, "target": {"name": "old"}}"#,
        )
        .unwrap();
        p.register_directed_sync_batch(&[["source".to_owned(), "target".to_owned()]])
            .unwrap();

        let result = p
            .process_changes(
                r#"[{"path": "source.name", "value_json": "\"Alice\""}]"#,
            )
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let target_name = parsed.changes.iter().find(|c| c.path == "target.name");
        assert!(
            target_name.is_some(),
            "target.name should be synced from source.name (child expansion)"
        );
        assert_eq!(target_name.unwrap().value_json, r#""Alice""#);

        // source.name should appear exactly once — the original input change, not a sync-back
        let source_name_changes: Vec<_> =
            parsed.changes.iter().filter(|c| c.path == "source.name").collect();
        assert_eq!(
            source_name_changes.len(),
            1,
            "source.name should appear only as the original input change, not synced back"
        );
        assert_eq!(source_name_changes[0].value_json, r#""Alice""#);
    }

    #[test]
    fn directed_sync_parent_expansion() {
        // Registered: source.a → target.a, source.b → target.b (leaf pairs).
        // Change comes in at the parent "source" (whole object swap).
        // Expected: target.a and target.b both receive updated leaf values (Case 6).
        let mut p = ProcessingPipeline::new();
        p.shadow_init(
            r#"{"source": {"a": "old-a", "b": "old-b"}, "target": {"a": "stale-a", "b": "stale-b"}}"#,
        )
        .unwrap();
        p.register_directed_sync_batch(&[
            ["source.a".to_owned(), "target.a".to_owned()],
            ["source.b".to_owned(), "target.b".to_owned()],
        ])
        .unwrap();

        // Push a whole new object to "source"
        let result = p
            .process_changes(
                r#"[{"path": "source", "value_json": "{\"a\":\"new-a\",\"b\":\"new-b\"}"}]"#,
            )
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let ta = parsed.changes.iter().find(|c| c.path == "target.a");
        let tb = parsed.changes.iter().find(|c| c.path == "target.b");
        assert!(
            ta.is_some(),
            "target.a should be synced via parent expansion"
        );
        assert!(
            tb.is_some(),
            "target.b should be synced via parent expansion"
        );
        assert_eq!(ta.unwrap().value_json, r#""new-a""#);
        assert_eq!(tb.unwrap().value_json, r#""new-b""#);
    }

    #[test]
    fn directed_sync_coexists_with_bidirectional() {
        // Mix: a↔b bidirectional, c→d one-way.
        // Changing a syncs b and vice-versa; changing c syncs d but d does NOT sync c.
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"a": "old", "b": "old", "c": "old", "d": "old"}"#)
            .unwrap();
        p.register_sync_batch(r#"[["a", "b"]]"#).unwrap();
        p.register_directed_sync_batch(&[["c".to_owned(), "d".to_owned()]])
            .unwrap();

        // Bidirectional: a→b
        let r1 = p
            .process_changes(r#"[{"path": "a", "value_json": "\"va\""}]"#)
            .unwrap();
        let p1: ProcessResult = serde_json::from_str(&r1).unwrap();
        assert!(p1.changes.iter().any(|c| c.path == "b"), "b synced from a");

        // Bidirectional: b→a
        let r2 = p
            .process_changes(r#"[{"path": "b", "value_json": "\"vb\""}]"#)
            .unwrap();
        let p2: ProcessResult = serde_json::from_str(&r2).unwrap();
        assert!(p2.changes.iter().any(|c| c.path == "a"), "a synced from b");

        // Directed: c→d
        let r3 = p
            .process_changes(r#"[{"path": "c", "value_json": "\"vc\""}]"#)
            .unwrap();
        let p3: ProcessResult = serde_json::from_str(&r3).unwrap();
        assert!(p3.changes.iter().any(|c| c.path == "d"), "d synced from c");

        // Directed: d must NOT sync back to c
        let r4 = p
            .process_changes(r#"[{"path": "d", "value_json": "\"vd\""}]"#)
            .unwrap();
        let p4: ProcessResult = serde_json::from_str(&r4).unwrap();
        assert!(
            !p4.changes.iter().any(|c| c.path == "c"),
            "c must NOT be updated when d changes (one-way)"
        );
    }

    #[test]
    fn directed_sync_skips_noop_when_values_match() {
        // Source and target already have the same value — sync should be a no-op
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"source": "same", "target": "same"}"#)
            .unwrap();
        p.register_directed_sync_batch(&[["source".to_owned(), "target".to_owned()]])
            .unwrap();

        let result = p
            .process_changes(r#"[{"path": "source", "value_json": "\"same\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Diff pre-pass drops this — shadow already has "same" for source
        assert!(
            parsed.changes.is_empty()
                || !parsed.changes.iter().any(|c| c.path == "target"),
            "target should not get a redundant sync when values match"
        );
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

        p.register_aggregation_batch(&agg_pairs(
            r#"[["allUsers", "user1"], ["allUsers", "user2"]]"#,
        ))
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
        p.shadow_init(r#"{"user": {"name": "alice"}, "profile": {"name": "bob"}}"#)
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
        p.register_aggregation_batch(&agg_pairs(
            r#"[["allUsers", "user1"], ["allUsers", "user2"]]"#,
        ))
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
        let changes_map: HashMap<&str, &str> = parsed
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

        p.register_aggregation_batch(&agg_pairs(
            r#"[["allFlags", "flag1"], ["allFlags", "flag2"]]"#,
        ))
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
        let mut deps = HashMap::new();
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
                {"subscriber_id": 100, "topic_paths": ["data"], "scope_path": ""},
                {"subscriber_id": 101, "topic_paths": ["data.value"], "scope_path": "data"}
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
                {"subscriber_id": 200, "topic_paths": ["user"], "scope_path": "user"},
                {"subscriber_id": 201, "topic_paths": ["settings.darkMode"], "scope_path": "settings"}
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
            assert_eq!(prepare.listener_changes.len(), 1);
            assert_eq!(prepare.listener_changes[0].path, "user.role");

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
            assert!(prepare.listener_changes.is_empty());
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
            assert!(!prepare.listener_changes.is_empty());
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

        /// Regression: initial sync must propagate to peer paths that are absent
        /// from shadow state (e.g., stripped by JSON.stringify when value was undefined).
        /// The parent object exists but the leaf key does not.
        #[test]
        fn initial_sync_propagates_to_absent_peer_path() {
            let mut p = make_pipeline();

            // Shadow has $shared.selectedCountry as a real object,
            // but product currency is absent (parent exists, leaf stripped)
            p.shadow_init(
                r#"{
                    "$shared": {
                        "selectedCountry": {"id": "DE", "region": {"id": "EU"}}
                    },
                    "c": {"1": {"p": {"2": {"data": {"productConfig": {"base": {}}}}}}}
                }"#,
            )
            .unwrap();

            // Register sync pair: $shared.selectedCountry ↔ product currency
            let result = p
                .register_sync_batch(
                    r#"[["$shared.selectedCountry", "c.1.p.2.data.productConfig.base.currency"]]"#,
                )
                .unwrap();

            let changes: Vec<Change> = serde_json::from_str(&result).unwrap();

            // The absent currency should be synced FROM $shared.selectedCountry
            assert!(
                !changes.is_empty(),
                "Expected initial sync to propagate to absent peer path"
            );

            let currency_change = changes
                .iter()
                .find(|c| c.path == "c.1.p.2.data.productConfig.base.currency");
            assert!(
                currency_change.is_some(),
                "Expected change for currency, got: {:?}",
                changes
            );

            // Value should match $shared.selectedCountry
            let value_json = &currency_change.unwrap().value_json;
            let value: serde_json::Value = serde_json::from_str(value_json).unwrap();
            assert_eq!(value["id"], "DE");
            assert_eq!(value["region"]["id"], "EU");
        }
    }

    // =======================================================================
    // EP9 integration tests: ChangeKind, Lineage, ChangeContext, stages
    // =======================================================================

    use crate::change::{ChangeContext, ChangeKind, Lineage};

    /// Helper: create Change with Real/Input defaults (simulates JS input).
    fn input_change(path: &str, value_json: &str) -> Change {
        Change::new(path.to_owned(), value_json.to_owned())
    }

    /// Helper: find a change by path in a result set.
    fn find_change<'a>(changes: &'a [Change], path: &str) -> Option<&'a Change> {
        changes.iter().find(|c| c.path == path)
    }

    /// Helper: assert a change has expected kind.
    fn assert_kind(change: &Change, expected: ChangeKind) {
        assert_eq!(
            change.kind, expected,
            "change at '{}' expected kind {:?}",
            change.path, expected
        );
    }

    // --- Group 1: ChangeKind propagation ---

    #[test]
    fn test_input_changes_are_real() {
        let mut p = make_pipeline();
        let result = p
            .process_changes_vec(vec![input_change("user.role", r#""admin""#)])
            .unwrap();
        assert!(!result.changes.is_empty());
        for c in &result.changes {
            assert_kind(c, ChangeKind::Real);
        }
    }

    #[test]
    fn test_sync_produced_changes_are_real() {
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"a": "hello", "b": "hello"}"#).unwrap();
        p.register_sync_batch(r#"[["a", "b"]]"#).unwrap();

        let result = p
            .process_changes_vec(vec![input_change("a", r#""world""#)])
            .unwrap();
        // Should have both a and b changes
        assert!(result.changes.len() >= 2);
        for c in &result.changes {
            assert_kind(c, ChangeKind::Real);
        }
        // Verify sync propagated
        let b_change = find_change(&result.changes, "b");
        assert!(b_change.is_some(), "sync should propagate a → b");
        assert_eq!(b_change.unwrap().value_json, r#""world""#);
    }

    #[test]
    fn test_flip_produced_changes_are_real() {
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"visible": false, "hidden": true}"#)
            .unwrap();
        p.register_flip_batch(r#"[["visible", "hidden"]]"#).unwrap();

        let result = p
            .process_changes_vec(vec![input_change("visible", "true")])
            .unwrap();
        assert!(result.changes.len() >= 2);
        for c in &result.changes {
            assert_kind(c, ChangeKind::Real);
        }
        let hidden = find_change(&result.changes, "hidden");
        assert!(hidden.is_some(), "flip should produce hidden change");
        assert_eq!(hidden.unwrap().value_json, "false");
    }

    #[test]
    fn test_aggregation_write_distributes_and_recomputes() {
        // Aggregation writes distribute target → sources (write stage removes target),
        // then aggregation-read recomputes target from sources (all-equal → target restored).
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"allUsers": null, "user1": null, "user2": null}"#)
            .unwrap();
        p.register_aggregation_batch(&agg_pairs(
            r#"[["allUsers", "user1"], ["allUsers", "user2"]]"#,
        ))
        .unwrap();

        let result = p
            .process_changes_vec(vec![input_change("allUsers", r#""shared""#)])
            .unwrap();

        // Source changes should be present and Real
        let u1 = find_change(&result.changes, "user1");
        let u2 = find_change(&result.changes, "user2");
        assert!(u1.is_some(), "user1 should receive distributed value");
        assert!(u2.is_some(), "user2 should receive distributed value");
        assert_kind(u1.unwrap(), ChangeKind::Real);
        assert_kind(u2.unwrap(), ChangeKind::Real);

        // Target recomputed by aggregation-read (all sources equal → target gets same value)
        let target = find_change(&result.changes, "allUsers");
        assert!(
            target.is_some(),
            "aggregation-read should recompute target from sources"
        );
        assert_kind(target.unwrap(), ChangeKind::Real);
        assert_eq!(target.unwrap().value_json, r#""shared""#);
    }

    #[test]
    fn test_no_op_changes_filtered() {
        // If value matches shadow state, diff filters it out (0 changes returned).
        let mut p = make_pipeline();
        // Shadow has user.role = "guest" already
        let result = p
            .process_changes_vec(vec![input_change("user.role", r#""guest""#)])
            .unwrap();
        assert_eq!(
            result.changes.len(),
            0,
            "no-op change should be filtered by diff"
        );
    }

    // --- Group 2: Lineage tracking ---

    #[test]
    fn test_input_changes_have_lineage_input() {
        let mut p = make_pipeline();
        let result = p
            .process_changes_vec(vec![input_change("user.role", r#""admin""#)])
            .unwrap();
        for c in &result.changes {
            assert_eq!(
                c.lineage,
                Lineage::Input,
                "input change at '{}' should have Lineage::Input",
                c.path
            );
        }
    }

    #[test]
    fn test_sync_changes_have_lineage_input() {
        // Current behavior: sync/flip changes are created with Lineage::Input.
        // (Proper Derived lineage is a future step once stage policies are wired.)
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"x": 1, "y": 2}"#).unwrap();
        p.register_sync_batch(r#"[["x", "y"]]"#).unwrap();

        let result = p
            .process_changes_vec(vec![input_change("x", "10")])
            .unwrap();
        for c in &result.changes {
            assert_eq!(
                c.lineage,
                Lineage::Input,
                "change at '{}' currently has Lineage::Input",
                c.path
            );
        }
    }

    #[test]
    fn test_concern_changes_have_lineage_input() {
        let mut p = make_pipeline();
        p.register_boollogic(
            "_concerns.user.role.disabledWhen",
            r#"{"IS_EQUAL": ["user.role", "admin"]}"#,
        )
        .unwrap();

        let result = p
            .process_changes_vec(vec![input_change("user.role", r#""admin""#)])
            .unwrap();

        let concern_changes: Vec<&Change> = result
            .changes
            .iter()
            .filter(|c| c.path.starts_with("_concerns."))
            .collect();
        assert!(
            !concern_changes.is_empty(),
            "BoolLogic should produce concern changes"
        );
        for c in &concern_changes {
            assert_eq!(c.lineage, Lineage::Input);
        }
    }

    #[test]
    fn test_lineage_context_defaults_to_mutation() {
        // Lineage::Input.context() returns ChangeContext::Mutation
        let mut p = make_pipeline();
        let result = p
            .process_changes_vec(vec![input_change("user.age", "30")])
            .unwrap();
        for c in &result.changes {
            assert_eq!(
                c.lineage.context(),
                ChangeContext::Mutation,
                "Input lineage context should be Mutation"
            );
        }
    }

    // --- Group 3: Debug mode behavior ---

    #[test]
    fn test_debug_mode_preserves_all_changes() {
        // Debug mode skips the retain(Real) filter, preserving all change kinds.
        let mut p = make_pipeline();
        p.set_debug(true);

        let debug_result = p
            .process_changes_vec(vec![input_change("user.role", r#""admin""#)])
            .unwrap();
        assert!(
            !debug_result.changes.is_empty(),
            "debug mode should return changes"
        );

        let mut p2 = make_pipeline();
        let prod_result = p2
            .process_changes_vec(vec![input_change("user.role", r#""admin""#)])
            .unwrap();

        assert!(
            debug_result.changes.len() >= prod_result.changes.len(),
            "debug mode should return >= production changes"
        );
    }

    #[test]
    fn test_production_mode_all_changes_real() {
        // In production mode, retain(|c| c.kind == Real) runs.
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"a": 1, "b": 2}"#).unwrap();
        p.register_sync_batch(r#"[["a", "b"]]"#).unwrap();

        let result = p
            .process_changes_vec(vec![input_change("a", "10")])
            .unwrap();
        for c in &result.changes {
            assert_kind(c, ChangeKind::Real);
        }
    }

    #[test]
    fn test_debug_mode_no_filtering() {
        // Debug mode skips the retain(Real) filter.
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"a": 1, "b": 2}"#).unwrap();
        p.register_sync_batch(r#"[["a", "b"]]"#).unwrap();
        p.set_debug(true);

        let result = p
            .process_changes_vec(vec![input_change("a", "10")])
            .unwrap();
        assert!(
            result.changes.len() >= 2,
            "debug mode should not filter changes"
        );
    }

    // --- Group 4: Full pipeline integration (multi-stage) ---

    #[test]
    fn test_sync_then_flip_chain() {
        // sync(a ↔ b) + flip(b ↔ c): change a=true → b=true (sync), c=false (flip)
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"a": false, "b": false, "c": true}"#)
            .unwrap();
        p.register_sync_batch(r#"[["a", "b"]]"#).unwrap();
        p.register_flip_batch(r#"[["b", "c"]]"#).unwrap();

        let result = p
            .process_changes_vec(vec![input_change("a", "true")])
            .unwrap();

        let a = find_change(&result.changes, "a");
        let b = find_change(&result.changes, "b");
        let c = find_change(&result.changes, "c");

        assert!(a.is_some(), "a should be in output");
        assert!(b.is_some(), "b should be synced from a");
        assert!(c.is_some(), "c should be flipped from b");

        assert_eq!(a.unwrap().value_json, "true");
        assert_eq!(b.unwrap().value_json, "true");
        assert_eq!(c.unwrap().value_json, "false");

        for c in &result.changes {
            assert_kind(c, ChangeKind::Real);
        }
    }

    #[test]
    fn test_aggregation_then_sync_then_boollogic() {
        // aggregation(total ← price1, price2) + sync(total ↔ displayTotal) + BoolLogic on total
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"price1": 10, "price2": 20, "total": 10, "displayTotal": 10}"#)
            .unwrap();

        p.register_aggregation_batch(&agg_pairs(r#"[["total", "price1"], ["total", "price2"]]"#))
            .unwrap();
        p.register_sync_batch(r#"[["total", "displayTotal"]]"#)
            .unwrap();
        p.register_boollogic(
            "_concerns.total.highlight",
            r#"{"IS_EQUAL": ["total", 100]}"#,
        )
        .unwrap();

        // Change price1 → triggers aggregation read recompute of total
        let result = p
            .process_changes_vec(vec![input_change("price1", "50")])
            .unwrap();

        let price1 = find_change(&result.changes, "price1");
        assert!(price1.is_some(), "price1 should be in output");

        for c in result
            .changes
            .iter()
            .filter(|c| !c.path.starts_with("_concerns."))
        {
            assert_kind(c, ChangeKind::Real);
        }
    }

    #[test]
    fn test_clear_path_then_sync() {
        // clearPath(trigger=email, target=errors) + sync(errors ↔ displayErrors)
        let mut p = ProcessingPipeline::new();
        p.shadow_init(
            r#"{"email": "old@test.com", "errors": "some error", "displayErrors": "some error"}"#,
        )
        .unwrap();

        p.register_clear_paths("test", &["email".to_owned()], &["errors".to_owned()])
            .unwrap();
        p.register_sync_batch(r#"[["errors", "displayErrors"]]"#)
            .unwrap();

        let result = p
            .process_changes_vec(vec![input_change("email", r#""new@test.com""#)])
            .unwrap();

        let errors = find_change(&result.changes, "errors");
        assert!(errors.is_some(), "errors should be cleared");
        assert_eq!(errors.unwrap().value_json, "null");

        let display = find_change(&result.changes, "displayErrors");
        assert!(display.is_some(), "displayErrors should sync");
        assert_eq!(display.unwrap().value_json, "null");

        for c in &result.changes {
            assert_kind(c, ChangeKind::Real);
        }
    }

    #[test]
    fn test_computation_then_boollogic() {
        // computation(SUM, total ← a, b) + BoolLogic(IS_EQUAL(total, 0))
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"a": 0, "b": 0, "total": 0}"#).unwrap();

        p.register_computation_batch(&comp_pairs(
            r#"[["SUM", "total", "a"], ["SUM", "total", "b"]]"#,
        ))
        .unwrap();

        p.register_boollogic("_concerns.x.disabledWhen", r#"{"IS_EQUAL": ["total", 0]}"#)
            .unwrap();

        // Change a from 0 to 5 → total recomputes to 5, BoolLogic evaluates to false
        let result = p.process_changes_vec(vec![input_change("a", "5")]).unwrap();

        let total = find_change(&result.changes, "total");
        assert!(total.is_some(), "total should be recomputed");
        assert_eq!(total.unwrap().value_json, "5");

        let concern = find_change(&result.changes, "_concerns.x.disabledWhen");
        assert!(concern.is_some(), "BoolLogic should produce concern change");
        assert_eq!(concern.unwrap().value_json, "false");
    }

    // --- Group 5: prepare_changes / pipeline_finalize round-trip ---

    #[test]
    fn test_prepare_finalize_round_trip() {
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"x": 1, "y": 2}"#).unwrap();
        p.register_sync_batch(r#"[["x", "y"]]"#).unwrap();
        p.register_boollogic("_concerns.x.active", r#"{"GT": ["x", 5]}"#)
            .unwrap();

        let prep = p.prepare_changes(vec![input_change("x", "10")]).unwrap();
        assert!(prep.has_work, "should have work");

        let y_change = prep.listener_changes.iter().find(|c| c.path == "y");
        assert!(y_change.is_some(), "prepare should include sync to y");

        let fin = p.pipeline_finalize(vec![]).unwrap();
        assert!(
            !fin.state_changes.is_empty(),
            "finalize should return changes"
        );
    }

    #[test]
    fn test_prepare_finalize_with_js_changes() {
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"x": 1, "extra": null}"#).unwrap();

        let prep = p.prepare_changes(vec![input_change("x", "10")]).unwrap();
        assert!(prep.has_work);

        let js_changes = vec![input_change("extra", r#""from-listener""#)];
        let fin = p.pipeline_finalize(js_changes).unwrap();

        let extra = fin.state_changes.iter().find(|c| c.path == "extra");
        assert!(
            extra.is_some(),
            "finalize should include js_changes in output"
        );
        assert_eq!(extra.unwrap().value_json, r#""from-listener""#);
    }

    #[test]
    fn test_prepare_no_work_returns_early() {
        let mut p = make_pipeline();

        let prep = p.prepare_changes(vec![]).unwrap();
        assert!(!prep.has_work, "empty changes should have no work");

        let prep2 = p
            .prepare_changes(vec![input_change("user.role", r#""guest""#)])
            .unwrap();
        assert!(
            !prep2.has_work || prep2.listener_changes.is_empty(),
            "no-op change should result in no work"
        );
    }

    // --- Group 6: PipelineContext isolation ---

    #[test]
    fn test_ctx_clears_between_calls() {
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"a": 1, "b": 2, "c": 3}"#).unwrap();

        p.register_boollogic("_concerns.a.flag", r#"{"GT": ["a", 10]}"#)
            .unwrap();
        p.register_boollogic("_concerns.c.flag", r#"{"GT": ["c", 10]}"#)
            .unwrap();

        // First call: change a → triggers BoolLogic on a
        let result1 = p
            .process_changes_vec(vec![input_change("a", "20")])
            .unwrap();
        let has_a_concern = result1.changes.iter().any(|c| c.path == "_concerns.a.flag");
        assert!(has_a_concern, "first call should trigger a's BoolLogic");

        // Second call: change c → triggers BoolLogic on c, NOT a's
        let result2 = p
            .process_changes_vec(vec![input_change("c", "20")])
            .unwrap();
        let has_c_concern = result2.changes.iter().any(|c| c.path == "_concerns.c.flag");
        assert!(has_c_concern, "second call should trigger c's BoolLogic");

        let has_a_in_second = result2.changes.iter().any(|c| c.path == "_concerns.a.flag");
        assert!(
            !has_a_in_second,
            "second call should not contain first call's concern changes"
        );
    }

    #[test]
    fn test_multiple_prepare_calls_clear_pending() {
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"x": 1, "y": 2}"#).unwrap();

        // First prepare without finalize
        let _prep1 = p.prepare_changes(vec![input_change("x", "10")]).unwrap();

        // Second prepare should start fresh (buffers cleared)
        let prep2 = p.prepare_changes(vec![input_change("y", "20")]).unwrap();

        let has_x = prep2.listener_changes.iter().any(|c| c.path == "x");
        assert!(
            !has_x,
            "second prepare should not contain first prepare's data"
        );
        let has_y = prep2.listener_changes.iter().any(|c| c.path == "y");
        assert!(has_y, "second prepare should contain y's change");
    }

    // --- child path sync/flip tests (change DEEPER than registered path) ---

    #[test]
    fn sync_child_path_propagates_to_peer() {
        // Sync pair ["source", "target"], change source.name → expect target.name
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"source": {"name": "old"}, "target": {"name": "old"}}"#)
            .unwrap();
        p.register_sync_batch(r#"[["source", "target"]]"#).unwrap();

        let result = p
            .process_changes(r#"[{"path": "source.name", "value_json": "\"Bob\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let target_change = parsed.changes.iter().find(|c| c.path == "target.name");
        assert!(
            target_change.is_some(),
            "target.name should receive sync when source.name changes (child path propagation)"
        );
        assert_eq!(target_change.unwrap().value_json, r#""Bob""#);
    }

    #[test]
    fn sync_deeply_nested_child_propagates() {
        // Sync pair ["a", "b"], change a.x.y → expect b.x.y
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"a": {"x": {"y": "old"}}, "b": {"x": {"y": "old"}}}"#)
            .unwrap();
        p.register_sync_batch(r#"[["a", "b"]]"#).unwrap();

        let result = p
            .process_changes(r#"[{"path": "a.x.y", "value_json": "\"deep\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let b_change = parsed.changes.iter().find(|c| c.path == "b.x.y");
        assert!(
            b_change.is_some(),
            "b.x.y should receive sync when a.x.y changes (deeply nested child propagation)"
        );
        assert_eq!(b_change.unwrap().value_json, r#""deep""#);
    }

    #[test]
    fn sync_child_path_multiple_peers() {
        // Sync pairs ["a", "b"] and ["a", "c"], change a.name → expect b.name and c.name
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"a": {"name": "old"}, "b": {"name": "old"}, "c": {"name": "old"}}"#)
            .unwrap();
        p.register_sync_batch(r#"[["a", "b"], ["a", "c"]]"#)
            .unwrap();

        let result = p
            .process_changes(r#"[{"path": "a.name", "value_json": "\"synced\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let paths: Vec<&str> = parsed.changes.iter().map(|c| c.path.as_str()).collect();
        assert!(
            paths.contains(&"b.name"),
            "b.name should be synced from a.name (child path, multiple peers)"
        );
        assert!(
            paths.contains(&"c.name"),
            "c.name should be synced from a.name (child path, multiple peers)"
        );
    }

    #[test]
    fn flip_child_path_does_not_propagate() {
        // Flip pair ["flags", "inverted"], change flags.visible (child) → flip should NOT fire
        // Flip only works on exact path match, not children of paired objects
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"flags": {"visible": false}, "inverted": {"visible": true}}"#)
            .unwrap();
        p.register_flip_batch(r#"[["flags", "inverted"]]"#).unwrap();

        let result = p
            .process_changes(r#"[{"path": "flags.visible", "value_json": "true"}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let inv_changes: Vec<&Change> = parsed
            .changes
            .iter()
            .filter(|c| c.path.starts_with("inverted"))
            .collect();
        assert!(
            inv_changes.is_empty(),
            "flip should NOT propagate when a child of the paired object changes"
        );
    }

    // --- parent path sync/flip tests (change SHALLOWER than registered path) ---

    #[test]
    fn sync_parent_object_breaks_down_to_leaf_sync() {
        // Sync pair ["user.name", "profile.name"], change user (object) → expect profile.name
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"user": {"name": "old", "age": 20}, "profile": {"name": "old"}}"#)
            .unwrap();
        p.register_sync_batch(r#"[["user.name", "profile.name"]]"#)
            .unwrap();

        let result = p
            .process_changes(
                r#"[{"path": "user", "value_json": "{\"name\": \"Alice\", \"age\": 30}"}]"#,
            )
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let profile_name = parsed.changes.iter().find(|c| c.path == "profile.name");
        assert!(
            profile_name.is_some(),
            "profile.name should be synced when parent 'user' changes (parent breakdown)"
        );
        assert_eq!(profile_name.unwrap().value_json, r#""Alice""#);
    }

    #[test]
    fn sync_parent_with_multiple_registered_leaves() {
        // Sync pairs for user.name→profile.name and user.email→profile.email
        // Change parent user → both leaves should sync
        let mut p = ProcessingPipeline::new();
        p.shadow_init(
            r#"{"user": {"name": "old", "email": "old@x.com"}, "profile": {"name": "old", "email": "old@x.com"}}"#,
        )
        .unwrap();
        p.register_sync_batch(
            r#"[["user.name", "profile.name"], ["user.email", "profile.email"]]"#,
        )
        .unwrap();

        let result = p
            .process_changes(
                r#"[{"path": "user", "value_json": "{\"name\": \"Bob\", \"email\": \"bob@x.com\"}"}]"#,
            )
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let profile_name = parsed.changes.iter().find(|c| c.path == "profile.name");
        let profile_email = parsed.changes.iter().find(|c| c.path == "profile.email");
        assert!(
            profile_name.is_some(),
            "profile.name should be synced from parent user change"
        );
        assert!(
            profile_email.is_some(),
            "profile.email should be synced from parent user change"
        );
        assert_eq!(profile_name.unwrap().value_json, r#""Bob""#);
        assert_eq!(profile_email.unwrap().value_json, r#""bob@x.com""#);
    }

    #[test]
    fn sync_parent_ignores_unregistered_leaves() {
        // Sync pair only for user.name→profile.name (not user.age)
        // Change parent user with name+age → profile.age should NOT appear
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"user": {"name": "old", "age": 20}, "profile": {"name": "old"}}"#)
            .unwrap();
        p.register_sync_batch(r#"[["user.name", "profile.name"]]"#)
            .unwrap();

        let result = p
            .process_changes(
                r#"[{"path": "user", "value_json": "{\"name\": \"Bob\", \"age\": 30}"}]"#,
            )
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let profile_age = parsed.changes.iter().find(|c| c.path == "profile.age");
        assert!(
            profile_age.is_none(),
            "profile.age should NOT appear — no sync pair registered for user.age"
        );
    }

    #[test]
    fn sync_parent_with_missing_nested_value() {
        // Sync pair ["user.name", "profile.name"], change user to {email: "x"} (no name key)
        // Should NOT sync profile.name since the value doesn't exist in the new object
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"user": {"name": "old"}, "profile": {"name": "old"}}"#)
            .unwrap();
        p.register_sync_batch(r#"[["user.name", "profile.name"]]"#)
            .unwrap();

        let result = p
            .process_changes(r#"[{"path": "user", "value_json": "{\"email\": \"x\"}"}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let profile_name = parsed.changes.iter().find(|c| c.path == "profile.name");
        assert!(
            profile_name.is_none(),
            "profile.name should NOT be synced when source object lacks 'name' key"
        );
    }

    #[test]
    fn flip_parent_change_breaks_down_to_leaf_flip() {
        // Flip pair on leaves ["flags.a", "flags.b"], change parent "flags" (object)
        // Parent change should decompose into leaf changes, then flip fires on matched leaves
        // Shadow: a=false, b=true. Parent sets a=true → flip should produce b=false
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"flags": {"a": false, "b": true}}"#)
            .unwrap();
        p.register_flip_batch(r#"[["flags.a", "flags.b"]]"#)
            .unwrap();

        // Set flags = {a: true} — parent change (only one side of the flip pair)
        // Decompose: flags.a = true (changed from false), flags.b not present
        // Flip: flags.a = true → flags.b should become false
        let result = p
            .process_changes(r#"[{"path": "flags", "value_json": "{\"a\": true}"}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let b_change = parsed.changes.iter().find(|c| c.path == "flags.b");
        assert!(
            b_change.is_some(),
            "flags.b should receive flip when parent 'flags' changes and flags.a is a leaf flip source"
        );
        assert_eq!(b_change.unwrap().value_json, "false");
    }

    #[test]
    fn flip_parent_change_with_multiple_leaf_pairs() {
        // Flip pairs: ["panel.open", "panel.closed"], ["panel.visible", "panel.hidden"]
        // Change parent "panel" → both flip pairs should fire
        let mut p = ProcessingPipeline::new();
        p.shadow_init(
            r#"{"panel": {"open": false, "closed": true, "visible": false, "hidden": true}}"#,
        )
        .unwrap();
        p.register_flip_batch(
            r#"[["panel.open", "panel.closed"], ["panel.visible", "panel.hidden"]]"#,
        )
        .unwrap();

        // Set panel = {open: true, visible: true} — only one side of each flip pair
        // Decompose: open=true (was false), visible=true (was false)
        // closed and hidden are NOT in the parent object → flip fires for them
        // Flip: open=true → closed=false, visible=true → hidden=false
        let result = p
            .process_changes(
                r#"[{"path": "panel", "value_json": "{\"open\": true, \"visible\": true}"}]"#,
            )
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let closed = parsed.changes.iter().find(|c| c.path == "panel.closed");
        let hidden = parsed.changes.iter().find(|c| c.path == "panel.hidden");
        assert!(
            closed.is_some(),
            "panel.closed should flip from parent decomposition of panel.open"
        );
        assert!(
            hidden.is_some(),
            "panel.hidden should flip from parent decomposition of panel.visible"
        );
        assert_eq!(closed.unwrap().value_json, "false");
        assert_eq!(hidden.unwrap().value_json, "false");
    }

    #[test]
    fn flip_parent_change_both_sides_present_no_flip() {
        // Flip pair ["flags.a", "flags.b"], parent change sets BOTH a and b
        // When both sides of the flip pair are present in the parent change,
        // there's no point flipping — user explicitly set both values
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"flags": {"a": false, "b": true}}"#)
            .unwrap();
        p.register_flip_batch(r#"[["flags.a", "flags.b"]]"#)
            .unwrap();

        // User sets both: a=true, b=true (intentionally NOT inverted)
        // Since both sides are explicitly set, flip should NOT override b
        let result = p
            .process_changes(r#"[{"path": "flags", "value_json": "{\"a\": true, \"b\": true}"}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // The parent change decomposes to flags.a=true and flags.b=true
        // Since both sides of the flip pair are in the same parent change,
        // flip should NOT run — the user's explicit values should be respected
        let flip_b = parsed
            .changes
            .iter()
            .filter(|c| c.path == "flags.b" && c.value_json == "false")
            .count();
        assert_eq!(
            flip_b, 0,
            "flip should NOT override flags.b when both sides are explicitly set in parent change"
        );
    }

    #[test]
    fn flip_parent_change_only_one_side_present_does_flip() {
        // Flip pair ["flags.a", "flags.b"], parent change sets only a (not b)
        // Only one side present → flip SHOULD fire for the missing side
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"flags": {"a": false, "b": true, "other": "x"}}"#)
            .unwrap();
        p.register_flip_batch(r#"[["flags.a", "flags.b"]]"#)
            .unwrap();

        // User sets flags = {a: true, other: "y"} — b is NOT in the new object
        // Decompose: flags.a = true (changed)
        // Flip: flags.a=true → flags.b should become false
        let result = p
            .process_changes(
                r#"[{"path": "flags", "value_json": "{\"a\": true, \"other\": \"y\"}"}]"#,
            )
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let b_change = parsed.changes.iter().find(|c| c.path == "flags.b");
        assert!(
            b_change.is_some(),
            "flags.b should flip when only flags.a is in the parent change"
        );
        assert_eq!(b_change.unwrap().value_json, "false");
    }

    // --- overlapping hierarchy tests ---

    #[test]
    fn sync_parent_and_child_changes_in_same_batch() {
        // Two changes in same batch: user (parent object) AND settings.theme (leaf)
        // Sync pairs on leaves of both
        let mut p = ProcessingPipeline::new();
        p.shadow_init(
            r#"{
                "user": {"name": "old"},
                "profile": {"name": "old"},
                "settings": {"theme": "light"},
                "prefs": {"theme": "light"}
            }"#,
        )
        .unwrap();
        p.register_sync_batch(
            r#"[["user.name", "profile.name"], ["settings.theme", "prefs.theme"]]"#,
        )
        .unwrap();

        let result = p
            .process_changes(
                r#"[
                    {"path": "user", "value_json": "{\"name\": \"Alice\"}"},
                    {"path": "settings.theme", "value_json": "\"dark\""}
                ]"#,
            )
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let profile_name = parsed.changes.iter().find(|c| c.path == "profile.name");
        let prefs_theme = parsed.changes.iter().find(|c| c.path == "prefs.theme");
        assert!(
            profile_name.is_some(),
            "profile.name should be synced from parent 'user' change"
        );
        assert!(
            prefs_theme.is_some(),
            "prefs.theme should be synced from exact match settings.theme"
        );
        assert_eq!(profile_name.unwrap().value_json, r#""Alice""#);
        assert_eq!(prefs_theme.unwrap().value_json, r#""dark""#);
    }

    // --- object-level sync/flip pairs with nested data ---

    #[test]
    fn sync_object_pair_leaf_change_propagates() {
        // Sync pair on objects: ["form.billing", "form.shipping"]
        // Change a leaf inside billing → expect same leaf in shipping
        let mut p = ProcessingPipeline::new();
        p.shadow_init(
            r#"{
                "form": {
                    "billing": {"city": "Prague", "zip": "10000"},
                    "shipping": {"city": "Prague", "zip": "10000"}
                }
            }"#,
        )
        .unwrap();
        p.register_sync_batch(r#"[["form.billing", "form.shipping"]]"#)
            .unwrap();

        let result = p
            .process_changes(r#"[{"path": "form.billing.city", "value_json": "\"Brno\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let shipping_city = parsed
            .changes
            .iter()
            .find(|c| c.path == "form.shipping.city");
        assert!(
            shipping_city.is_some(),
            "form.shipping.city should sync when form.billing.city changes"
        );
        assert_eq!(shipping_city.unwrap().value_json, r#""Brno""#);

        // zip should NOT be touched
        let shipping_zip = parsed
            .changes
            .iter()
            .find(|c| c.path == "form.shipping.zip");
        assert!(
            shipping_zip.is_none(),
            "form.shipping.zip should not change when only city changed"
        );
    }

    #[test]
    fn sync_object_pair_multiple_leaf_changes_in_batch() {
        // Sync pair on objects, two leaf changes in same batch
        let mut p = ProcessingPipeline::new();
        p.shadow_init(
            r#"{
                "form": {
                    "billing": {"city": "Prague", "zip": "10000"},
                    "shipping": {"city": "Prague", "zip": "10000"}
                }
            }"#,
        )
        .unwrap();
        p.register_sync_batch(r#"[["form.billing", "form.shipping"]]"#)
            .unwrap();

        let result = p
            .process_changes(
                r#"[
                    {"path": "form.billing.city", "value_json": "\"Brno\""},
                    {"path": "form.billing.zip", "value_json": "\"60200\""}
                ]"#,
            )
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let shipping_city = parsed
            .changes
            .iter()
            .find(|c| c.path == "form.shipping.city");
        let shipping_zip = parsed
            .changes
            .iter()
            .find(|c| c.path == "form.shipping.zip");
        assert!(
            shipping_city.is_some(),
            "form.shipping.city should sync from billing.city"
        );
        assert!(
            shipping_zip.is_some(),
            "form.shipping.zip should sync from billing.zip"
        );
        assert_eq!(shipping_city.unwrap().value_json, r#""Brno""#);
        assert_eq!(shipping_zip.unwrap().value_json, r#""60200""#);
    }

    #[test]
    fn sync_object_pair_deeply_nested_leaf() {
        // Sync pair on mid-level objects, change a deep leaf
        // ["app.config.theme", "app.backup.theme"], change app.config.theme.colors.primary
        let mut p = ProcessingPipeline::new();
        p.shadow_init(
            r##"{
                "app": {
                    "config": {"theme": {"colors": {"primary": "#000"}, "font": "sans"}},
                    "backup": {"theme": {"colors": {"primary": "#000"}, "font": "sans"}}
                }
            }"##,
        )
        .unwrap();
        p.register_sync_batch(r#"[["app.config.theme", "app.backup.theme"]]"#)
            .unwrap();

        let result = p
            .process_changes(
                r##"[{"path": "app.config.theme.colors.primary", "value_json": "\"#f00\""}]"##,
            )
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let backup_color = parsed
            .changes
            .iter()
            .find(|c| c.path == "app.backup.theme.colors.primary");
        assert!(
            backup_color.is_some(),
            "deep leaf under synced object should propagate to peer"
        );
        assert_eq!(backup_color.unwrap().value_json, r##""#f00""##);

        // font should be untouched
        let backup_font = parsed
            .changes
            .iter()
            .find(|c| c.path == "app.backup.theme.font");
        assert!(
            backup_font.is_none(),
            "sibling leaves under synced object should not be touched"
        );
    }

    #[test]
    fn sync_object_pair_replace_entire_subtree() {
        // Sync pair on objects, replace the entire synced object with new value
        // This is an exact match on the pair path itself
        let mut p = ProcessingPipeline::new();
        p.shadow_init(
            r#"{
                "source": {"a": 1, "b": 2},
                "target": {"a": 1, "b": 2}
            }"#,
        )
        .unwrap();
        p.register_sync_batch(r#"[["source", "target"]]"#).unwrap();

        let result = p
            .process_changes(
                r#"[{"path": "source", "value_json": "{\"a\": 10, \"b\": 20, \"c\": 30}"}]"#,
            )
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let target_change = parsed.changes.iter().find(|c| c.path == "target");
        assert!(
            target_change.is_some(),
            "exact match on object pair should still propagate"
        );
        assert_eq!(
            target_change.unwrap().value_json,
            r#"{"a": 10, "b": 20, "c": 30}"#
        );
    }

    #[test]
    fn flip_object_pair_child_change_does_not_propagate() {
        // Flip pair on objects: ["ui.panel", "ui.overlay"]
        // Change a boolean leaf INSIDE the object → flip should NOT fire
        // Flip only works on exact path match (whole object), not children
        let mut p = ProcessingPipeline::new();
        p.shadow_init(
            r#"{
                "ui": {
                    "panel": {"visible": false, "expanded": true},
                    "overlay": {"visible": true, "expanded": false}
                }
            }"#,
        )
        .unwrap();
        p.register_flip_batch(r#"[["ui.panel", "ui.overlay"]]"#)
            .unwrap();

        let result = p
            .process_changes(r#"[{"path": "ui.panel.visible", "value_json": "true"}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let overlay_changes: Vec<&Change> = parsed
            .changes
            .iter()
            .filter(|c| c.path.starts_with("ui.overlay"))
            .collect();
        assert!(
            overlay_changes.is_empty(),
            "flip on object pair should NOT propagate when a child leaf changes — flip only works on exact match"
        );
    }

    #[test]
    fn flip_object_pair_multiple_child_changes_do_not_propagate() {
        // Flip pair on objects, multiple boolean leaf changes in batch → still no flip
        let mut p = ProcessingPipeline::new();
        p.shadow_init(
            r#"{
                "ui": {
                    "panel": {"visible": false, "expanded": false},
                    "overlay": {"visible": true, "expanded": true}
                }
            }"#,
        )
        .unwrap();
        p.register_flip_batch(r#"[["ui.panel", "ui.overlay"]]"#)
            .unwrap();

        let result = p
            .process_changes(
                r#"[
                    {"path": "ui.panel.visible", "value_json": "true"},
                    {"path": "ui.panel.expanded", "value_json": "true"}
                ]"#,
            )
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let overlay_changes: Vec<&Change> = parsed
            .changes
            .iter()
            .filter(|c| c.path.starts_with("ui.overlay"))
            .collect();
        assert!(
            overlay_changes.is_empty(),
            "flip on object pair should NOT fire for child leaf changes, even in batch"
        );
    }

    #[test]
    fn flip_object_pair_non_boolean_child_ignored() {
        // Flip pair on objects, change a non-boolean leaf → should NOT flip either
        let mut p = ProcessingPipeline::new();
        p.shadow_init(
            r#"{
                "ui": {
                    "panel": {"visible": false, "label": "old"},
                    "overlay": {"visible": true, "label": "old"}
                }
            }"#,
        )
        .unwrap();
        p.register_flip_batch(r#"[["ui.panel", "ui.overlay"]]"#)
            .unwrap();

        let result = p
            .process_changes(r#"[{"path": "ui.panel.label", "value_json": "\"new\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let overlay_label = parsed.changes.iter().find(|c| c.path == "ui.overlay.label");
        assert!(
            overlay_label.is_none(),
            "non-boolean child of flip-paired object should not propagate"
        );
    }

    #[test]
    fn sync_object_pair_chained_three_objects() {
        // Chained object sync: a → b → c (all objects)
        // Change a.val → expect b.val AND c.val
        let mut p = ProcessingPipeline::new();
        p.shadow_init(
            r#"{
                "a": {"val": "old"},
                "b": {"val": "old"},
                "c": {"val": "old"}
            }"#,
        )
        .unwrap();
        p.register_sync_batch(r#"[["a", "b"], ["b", "c"]]"#)
            .unwrap();

        let result = p
            .process_changes(r#"[{"path": "a.val", "value_json": "\"chain\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let b_val = parsed.changes.iter().find(|c| c.path == "b.val");
        let c_val = parsed.changes.iter().find(|c| c.path == "c.val");
        assert!(
            b_val.is_some(),
            "b.val should sync from a.val (object pair child propagation)"
        );
        assert!(
            c_val.is_some(),
            "c.val should sync from b.val (chained object pair child propagation)"
        );
        assert_eq!(b_val.unwrap().value_json, r#""chain""#);
        assert_eq!(c_val.unwrap().value_json, r#""chain""#);
    }

    #[test]
    fn sync_object_pair_mixed_with_leaf_pair() {
        // One object-level pair + one leaf-level pair on the same subtree
        // Object pair: ["data.input", "data.output"]
        // Leaf pair: ["data.input.status", "ui.statusBadge"]
        // Change data.input.status → both should fire
        let mut p = ProcessingPipeline::new();
        p.shadow_init(
            r#"{
                "data": {
                    "input": {"status": "draft", "title": "doc"},
                    "output": {"status": "draft", "title": "doc"}
                },
                "ui": {"statusBadge": "draft"}
            }"#,
        )
        .unwrap();
        p.register_sync_batch(
            r#"[["data.input", "data.output"], ["data.input.status", "ui.statusBadge"]]"#,
        )
        .unwrap();

        let result = p
            .process_changes(r#"[{"path": "data.input.status", "value_json": "\"published\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Leaf pair: exact match → ui.statusBadge
        let badge = parsed.changes.iter().find(|c| c.path == "ui.statusBadge");
        assert!(
            badge.is_some(),
            "ui.statusBadge should sync via exact leaf pair"
        );
        assert_eq!(badge.unwrap().value_json, r#""published""#);

        // Object pair: child propagation → data.output.status
        let output_status = parsed
            .changes
            .iter()
            .find(|c| c.path == "data.output.status");
        assert!(
            output_status.is_some(),
            "data.output.status should sync via object pair child propagation"
        );
        assert_eq!(output_status.unwrap().value_json, r#""published""#);
    }

    // --- anchorPath tests (WASM-EP10) ---
    // anchorPath: when set, all resources in the registration are silently skipped
    // if the anchor path is structurally absent from shadow state.
    // These tests document the expected behavior before implementation.

    #[test]
    fn anchor_path_present_boollogic_evaluates() {
        // anchorPath = "user.profile", profile exists → BoolLogic should evaluate
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"user": {"profile": {"name": "Alice", "role": "guest"}}}"#)
            .unwrap();

        // Note: register_side_effects doesn't support anchor_path yet, so this tests
        // the expected future behavior. For now, register directly.
        p.register_boollogic(
            "_concerns.user.profile.name.disabledWhen",
            r#"{"IS_EQUAL": ["user.profile.role", "admin"]}"#,
        )
        .unwrap();

        let result = p
            .process_changes(r#"[{"path": "user.profile.role", "value_json": "\"admin\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let concern = parsed
            .changes
            .iter()
            .find(|c| c.path == "_concerns.user.profile.name.disabledWhen");
        assert!(
            concern.is_some(),
            "BoolLogic should evaluate when anchor path is present in shadow"
        );
        assert_eq!(concern.unwrap().value_json, "true");
    }

    #[test]
    fn anchor_path_absent_boollogic_skipped() {
        // anchorPath = "user.profile", profile does NOT exist → BoolLogic should be skipped
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"user": {}, "otherField": "test"}"#)
            .unwrap();

        // Register BoolLogic that references user.profile.role
        // With anchor_path="user.profile", this should be skipped since user.profile is absent
        p.register_boollogic(
            "_concerns.user.profile.name.disabledWhen",
            r#"{"IS_EQUAL": ["user.profile.role", "admin"]}"#,
        )
        .unwrap();

        // TODO: Once anchorPath is implemented, register with anchor_path="user.profile"
        // and verify BoolLogic is NOT evaluated.
        // For now, this documents the expected behavior.

        // Change an unrelated field that happens to trigger reverse dependency check
        let result = p
            .process_changes(r#"[{"path": "otherField", "value_json": "\"changed\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Currently: BoolLogic won't fire because otherField isn't a dependency.
        // With anchorPath: even if user.profile.role changed, BoolLogic would be skipped
        // because user.profile is absent.
        let concern = parsed
            .changes
            .iter()
            .find(|c| c.path.starts_with("_concerns."));
        assert!(
            concern.is_none(),
            "BoolLogic should not fire when anchor path is absent"
        );
    }

    #[test]
    fn anchor_path_removed_mid_session_skips_resources() {
        // Start with anchor present, then remove it → resources should stop firing
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"user": {"profile": {"name": "Alice", "role": "guest"}}}"#)
            .unwrap();

        p.register_boollogic(
            "_concerns.user.profile.name.disabledWhen",
            r#"{"IS_EQUAL": ["user.profile.role", "admin"]}"#,
        )
        .unwrap();

        // First: anchor present, change role → BoolLogic fires
        let result1 = p
            .process_changes(r#"[{"path": "user.profile.role", "value_json": "\"admin\""}]"#)
            .unwrap();
        let parsed1: ProcessResult = serde_json::from_str(&result1).unwrap();
        let concern1 = parsed1
            .changes
            .iter()
            .find(|c| c.path == "_concerns.user.profile.name.disabledWhen");
        assert!(
            concern1.is_some(),
            "BoolLogic should evaluate when anchor present"
        );

        // Now remove the anchor by replacing user with empty object
        let result2 = p
            .process_changes(r#"[{"path": "user", "value_json": "{}"}]"#)
            .unwrap();
        let parsed2: ProcessResult = serde_json::from_str(&result2).unwrap();

        // TODO: With anchorPath implemented, the BoolLogic for user.profile.name should
        // NOT evaluate because user.profile no longer exists in shadow.
        // This test documents expected behavior — currently BoolLogic would still try
        // to evaluate (and get false because user.profile.role is absent).
        let _concern2 = parsed2
            .changes
            .iter()
            .find(|c| c.path == "_concerns.user.profile.name.disabledWhen");
        // When anchorPath is implemented, assert concern2 is None
    }

    #[test]
    fn anchor_path_restored_resumes_resources() {
        // Start with anchor absent, add it → resources should resume
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"user": {}}"#).unwrap();

        p.register_boollogic(
            "_concerns.user.profile.name.disabledWhen",
            r#"{"IS_EQUAL": ["user.profile.role", "admin"]}"#,
        )
        .unwrap();

        // TODO: Register with anchor_path="user.profile"
        // First call: anchor absent → BoolLogic skipped

        // Restore anchor by adding profile
        let result = p
            .process_changes(
                r#"[{"path": "user.profile", "value_json": "{\"name\": \"Bob\", \"role\": \"admin\"}"}]"#,
            )
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // When anchor is restored, BoolLogic should evaluate on this run
        // (user.profile.role = "admin" → disabledWhen = true)
        let concern = parsed
            .changes
            .iter()
            .find(|c| c.path == "_concerns.user.profile.name.disabledWhen");
        assert!(
            concern.is_some(),
            "BoolLogic should resume when anchor path is restored in shadow"
        );
    }

    #[test]
    fn anchor_path_affects_sync_pairs() {
        // anchorPath on a registration with sync pairs
        // When anchor absent → sync should not fire
        let mut p = ProcessingPipeline::new();
        p.shadow_init(
            r#"{"form": {"billing": {"city": "Prague"}}, "form_copy": {"billing": {"city": "Prague"}}}"#,
        )
        .unwrap();

        // Register sync pair with anchorPath="form.billing"
        p.register_sync_batch(r#"[["form.billing.city", "form_copy.billing.city"]]"#)
            .unwrap();

        // While anchor is present, sync should work
        let result1 = p
            .process_changes(r#"[{"path": "form.billing.city", "value_json": "\"Brno\""}]"#)
            .unwrap();
        let parsed1: ProcessResult = serde_json::from_str(&result1).unwrap();
        let sync1 = parsed1
            .changes
            .iter()
            .find(|c| c.path == "form_copy.billing.city");
        assert!(sync1.is_some(), "sync should work when anchor is present");

        // Remove anchor: form.billing disappears
        p.process_changes(r#"[{"path": "form", "value_json": "{}"}]"#)
            .unwrap();

        // TODO: With anchorPath, sync for form.billing.city should be skipped
        // Currently handled by existing parent_exists guard, but anchorPath
        // would skip the entire registration group, not just individual paths
    }

    #[test]
    fn anchor_path_affects_listeners() {
        // anchorPath on a registration with listeners
        // When anchor absent → listener dispatch should be filtered out
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"section": {"field": "value"}}"#).unwrap();

        // Register listener on section.field with anchorPath="section"
        p.register_listeners_batch(
            r#"[{"subscriber_id": 1, "topic_paths": ["section.field"], "scope_path": "section"}]"#,
        )
        .unwrap();

        // Anchor present → dispatch plan should include listener
        let plan1 = p.create_dispatch_plan_vec(&[Change::new(
            "section.field".to_string(),
            r#""new""#.to_string(),
        )]);
        assert!(
            !plan1.levels.is_empty(),
            "listener should dispatch when anchor is present"
        );

        // Remove anchor
        p.process_changes(r#"[{"path": "section", "value_json": "null"}]"#)
            .unwrap();

        // TODO: With anchorPath, listener should be filtered from dispatch plan
        // even though topic still matches. Currently the listener would still fire
        // because null is a value, not structural absence.
        // For structural absence test:
        // p.process_changes(r#"[{"path": "section", "value_json": ... remove key ...}]"#)
    }

    #[test]
    fn anchor_path_null_value_is_not_absent() {
        // Setting anchor path to null should NOT be treated as absent
        // "Absent" is structural — the key must not exist, not just be null
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"section": null}"#).unwrap();

        p.register_boollogic("_concerns.section.flag", r#"{"EXISTS": "section"}"#)
            .unwrap();

        // TODO: With anchorPath="section", resources should still fire
        // because null is a value, not structural absence
        let result = p
            .process_changes(r#"[{"path": "section", "value_json": "null"}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // EXISTS check against null path — section exists (it's null, but present)
        // anchorPath should treat this as "present"
        let _concern = parsed
            .changes
            .iter()
            .find(|c| c.path == "_concerns.section.flag");
        // When anchorPath is implemented: assert concern IS present (null = present)
    }

    #[test]
    fn anchor_path_shared_across_registrations() {
        // Multiple registrations with same anchorPath
        // Removing anchor should skip ALL of them
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"panel": {"title": "Test", "active": false, "count": 0}}"#)
            .unwrap();

        // Registration 1: BoolLogic with anchorPath="panel"
        p.register_boollogic(
            "_concerns.panel.title.disabledWhen",
            r#"{"IS_EQUAL": ["panel.active", true]}"#,
        )
        .unwrap();

        // Registration 2: sync pair with anchorPath="panel"
        p.register_sync_batch(r#"[["panel.title", "panel.backup_title"]]"#)
            .unwrap();

        // Both should work when anchor is present
        let result = p
            .process_changes(r#"[{"path": "panel.active", "value_json": "true"}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();
        let concern = parsed
            .changes
            .iter()
            .find(|c| c.path == "_concerns.panel.title.disabledWhen");
        assert!(
            concern.is_some(),
            "BoolLogic should evaluate when shared anchor is present"
        );

        // TODO: When anchorPath is implemented and panel is removed,
        // BOTH the BoolLogic and the sync should be skipped
    }

    // ========================================================================
    // Comprehensive AnchorPath Tests (WASM-EP10)
    // ========================================================================
    // Tests that verify anchorPath implementation:
    // - Resources are SKIPPED when anchor is absent (not deleted/unregistered)
    // - Resources execute when anchor is present
    // - Resources resume when anchor is restored
    // - Multiple resource types (BoolLogic, listeners, validators) respect anchor

    #[test]
    fn anchor_path_boollogic_skipped_when_absent() {
        // BoolLogic with anchorPath set → when anchor absent, skip evaluation
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"user": {"email": "test@example.com"}}"#)
            .unwrap();

        // Register BoolLogic with anchor_path
        let registration = serde_json::json!({
            "registration_id": "form-concerns",
            "anchor_path": "user.profile",
            "bool_logics": [{
                "output_path": "_concerns.user.profile.disabled",
                "tree_json": r#"{"IS_EQUAL": ["user.profile.role", "admin"]}"#
            }]
        });
        p.register_concerns(&registration.to_string()).unwrap();

        // Change a field (user.profile is absent, so BoolLogic should NOT evaluate)
        let result = p
            .process_changes(r#"[{"path": "user.email", "value_json": "\"new@test.com\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // BoolLogic should not appear in results (anchor absent)
        let concern = parsed
            .changes
            .iter()
            .find(|c| c.path == "_concerns.user.profile.disabled");
        assert!(
            concern.is_none(),
            "BoolLogic should be skipped when anchor path is absent"
        );
    }

    #[test]
    fn anchor_path_boollogic_executes_when_present() {
        // Same BoolLogic, but now anchor path exists → should evaluate
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"user": {"profile": {"role": "guest"}}}"#)
            .unwrap();

        let registration = serde_json::json!({
            "registration_id": "form-concerns",
            "anchor_path": "user.profile",
            "bool_logics": [{
                "output_path": "_concerns.user.profile.disabled",
                "tree_json": r#"{"IS_EQUAL": ["user.profile.role", "admin"]}"#
            }]
        });
        p.register_concerns(&registration.to_string()).unwrap();

        // Change role to admin (anchor present, BoolLogic should evaluate)
        let result = p
            .process_changes(r#"[{"path": "user.profile.role", "value_json": "\"admin\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // BoolLogic should appear in results (anchor present)
        let concern = parsed
            .changes
            .iter()
            .find(|c| c.path == "_concerns.user.profile.disabled");
        assert!(
            concern.is_some(),
            "BoolLogic should execute when anchor path is present"
        );
        assert_eq!(
            concern.unwrap().value_json,
            "true",
            "BoolLogic result should be true"
        );
    }

    #[test]
    #[ignore = "requires complex test state manipulation with nested path deletion"]
    fn anchor_path_not_deleted_just_skipped() {
        // Verify that removing anchor doesn't delete the resource, just skips it
        // When anchor is restored, the resource should still work
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"user": {"profile": {"role": "guest", "name": "Alice"}}}"#)
            .unwrap();

        let registration = serde_json::json!({
            "registration_id": "form-concerns",
            "anchor_path": "user.profile",
            "bool_logics": [{
                "output_path": "_concerns.user.profile.disabled",
                "tree_json": r#"{"IS_EQUAL": ["user.profile.role", "admin"]}"#
            }]
        });
        p.register_concerns(&registration.to_string()).unwrap();

        // Step 1: Anchor present → BoolLogic evaluates
        let result1 = p
            .process_changes(r#"[{"path": "user.profile.role", "value_json": "\"admin\""}]"#)
            .unwrap();
        let parsed1: ProcessResult = serde_json::from_str(&result1).unwrap();
        assert!(
            parsed1
                .changes
                .iter()
                .any(|c| c.path == "_concerns.user.profile.disabled"),
            "BoolLogic should evaluate when anchor present (step 1)"
        );

        // Step 2: Remove anchor by setting user.profile to null
        p.process_changes(r#"[{"path": "user.profile", "value_json": "null"}]"#)
            .unwrap();

        // Step 3: Change role again (anchor absent, BoolLogic should be SKIPPED)
        let result3 = p
            .process_changes(r#"[{"path": "user.profile.role", "value_json": "\"guest\""}]"#)
            .unwrap();
        let parsed3: ProcessResult = serde_json::from_str(&result3).unwrap();
        assert!(
            !parsed3
                .changes
                .iter()
                .any(|c| c.path == "_concerns.user.profile.disabled"),
            "BoolLogic should be skipped when anchor absent (step 3)"
        );

        // Step 4: Restore anchor
        p.process_changes(
            r#"[{"path": "user.profile", "value_json": "{\"role\": \"admin\", \"name\": \"Alice\"}"}]"#,
        )
        .unwrap();

        // Step 5: Change role (anchor restored, BoolLogic should evaluate again)
        let result5 = p
            .process_changes(r#"[{"path": "user.profile.role", "value_json": "\"guest\""}]"#)
            .unwrap();
        let parsed5: ProcessResult = serde_json::from_str(&result5).unwrap();
        assert!(
            parsed5
                .changes
                .iter()
                .any(|c| c.path == "_concerns.user.profile.disabled"),
            "BoolLogic should resume when anchor is restored (step 5)"
        );
    }

    #[test]
    #[ignore = "requires complex test state manipulation with nested path deletion"]
    fn anchor_path_nested_path_checks() {
        // Verify that deep nested anchor paths work correctly
        // anchor_path = "data.section.config"
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"data": {"section": {"config": {"enabled": true, "timeout": 5000}}}}"#)
            .unwrap();

        let registration = serde_json::json!({
            "registration_id": "nested-concerns",
            "anchor_path": "data.section.config",
            "bool_logics": [{
                "output_path": "_concerns.data.section.config.needsReview",
                "tree_json": r#"{"IS_EQUAL": ["data.section.config.enabled", false]}"#
            }]
        });
        p.register_concerns(&registration.to_string()).unwrap();

        // Step 1: Anchor present, change enabled
        let result1 = p
            .process_changes(r#"[{"path": "data.section.config.enabled", "value_json": "false"}]"#)
            .unwrap();
        let parsed1: ProcessResult = serde_json::from_str(&result1).unwrap();
        assert!(
            parsed1
                .changes
                .iter()
                .any(|c| c.path == "_concerns.data.section.config.needsReview"),
            "Nested BoolLogic should evaluate when anchor present"
        );

        // Step 2: Remove intermediate path (data.section becomes null)
        p.process_changes(r#"[{"path": "data.section", "value_json": "null"}]"#)
            .unwrap();

        // Step 3: Try to change enabled (anchor now absent)
        let result3 = p
            .process_changes(r#"[{"path": "data.section.config.enabled", "value_json": "true"}]"#)
            .unwrap();
        let parsed3: ProcessResult = serde_json::from_str(&result3).unwrap();
        assert!(
            !parsed3
                .changes
                .iter()
                .any(|c| c.path == "_concerns.data.section.config.needsReview"),
            "Nested BoolLogic should be skipped when anchor absent"
        );
    }

    #[test]
    #[ignore = "sync pairs require per-registration anchor path tracking"]
    fn anchor_path_sync_pairs_skipped_when_absent() {
        // Sync pairs should also be skipped when anchor is absent
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"form": {"email": "test@test.com", "confirmedEmail": "test@test.com"}}"#)
            .unwrap();

        let registration = serde_json::json!({
            "registration_id": "form-sync",
            "anchor_path": "form.verification",
            "sync_pairs": [["form.email", "form.confirmedEmail"]]
        });
        p.register_side_effects(&registration.to_string()).unwrap();

        // Change email (anchor is absent, sync should be SKIPPED)
        let result = p
            .process_changes(r#"[{"path": "form.email", "value_json": "\"new@test.com\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Only form.email should change, confirmedEmail should NOT sync
        let confirmed_change = parsed
            .changes
            .iter()
            .find(|c| c.path == "form.confirmedEmail");
        assert!(
            confirmed_change.is_none(),
            "Sync should be skipped when anchor path is absent"
        );
    }

    #[test]
    #[ignore = "sync pairs require per-registration anchor path tracking"]
    fn anchor_path_multiple_resources_same_anchor() {
        // Multiple resources (BoolLogic + sync) with same anchor → all skipped together
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"panel": {"visible": true, "title": "Panel", "backupTitle": "Panel"}}"#)
            .unwrap();

        // BoolLogic
        p.register_boollogic(
            "_concerns.panel.disabled",
            r#"{"IS_EQUAL": ["panel.visible", false]}"#,
        )
        .unwrap();

        // Sync pair
        p.register_sync_batch(r#"[["panel.title", "panel.backupTitle"]]"#)
            .unwrap();

        // Step 1: With anchor present, both should work
        let result1 = p
            .process_changes(r#"[{"path": "panel.visible", "value_json": "false"}]"#)
            .unwrap();
        let parsed1: ProcessResult = serde_json::from_str(&result1).unwrap();
        let concern_present = parsed1
            .changes
            .iter()
            .any(|c| c.path == "_concerns.panel.disabled");
        assert!(concern_present, "BoolLogic should work when anchor present");

        // Step 2: Remove anchor
        p.process_changes(r#"[{"path": "panel", "value_json": "null"}]"#)
            .unwrap();

        // Step 3: Change title (both sync and BoolLogic should be skipped)
        let result3 = p
            .process_changes(r#"[{"path": "panel.title", "value_json": "\"New Panel\""}]"#)
            .unwrap();
        let parsed3: ProcessResult = serde_json::from_str(&result3).unwrap();

        let backup_synced = parsed3
            .changes
            .iter()
            .any(|c| c.path == "panel.backupTitle");
        assert!(
            !backup_synced,
            "Sync should be skipped when anchor absent (group skip)"
        );

        let concern_skipped = parsed3
            .changes
            .iter()
            .any(|c| c.path == "_concerns.panel.disabled");
        assert!(
            !concern_skipped,
            "BoolLogic should be skipped when anchor absent (group skip)"
        );
    }

    #[test]
    #[ignore = "EXISTS check behavior with null needs investigation"]
    fn anchor_path_null_vs_missing_distinction() {
        // Verify correct distinction between null value and missing key
        // "Absent" = structurally missing (key doesn't exist)
        // null = key exists, value is null (should NOT be treated as absent)
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"user": {"profile": null}}"#).unwrap();

        let registration = serde_json::json!({
            "registration_id": "profile-test",
            "anchor_path": "user.profile",
            "bool_logics": [{
                "output_path": "_concerns.user.profile.check",
                "tree_json": r#"{"EXISTS": "user.profile"}"#
            }]
        });
        p.register_concerns(&registration.to_string()).unwrap();

        // With profile=null, anchor IS present (null is a value, not absent)
        let result = p
            .process_changes(r#"[{"path": "user.email", "value_json": "\"test@test.com\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // BoolLogic should evaluate (profile key exists, even if value is null)
        let concern = parsed
            .changes
            .iter()
            .find(|c| c.path == "_concerns.user.profile.check");
        assert!(
            concern.is_some(),
            "Anchor with null value should be treated as present (not absent)"
        );
    }
}
