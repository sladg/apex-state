use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use ts_rs::TS;

/// Canonical names for every pipeline stage.
#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[serde(rename_all = "snake_case")]
#[derive(TS)]
pub enum Stage {
    Input,
    AggregationWrite,
    Diff,
    Sync,
    Flip,
    ClearPath,
    AggregationRead,
    Computation,
    BoolLogic,
    ValueLogic,
    Listeners,
    Apply,
}

/// Coarse classification: should this change be written to valtio?
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Default)]
#[serde(rename_all = "snake_case")]
#[derive(TS)]
pub enum ChangeKind {
    /// Written to valtio. The default for pipeline outputs.
    #[default]
    Real,
    /// Value matches current shadow state. Not written to valtio, but passed to listener dispatch.
    Redundant,
    /// Leaf-level expansion of a complex parent change. Used for listener routing, BoolLogic evaluation, and sync/flip only.
    Breakdown,
    /// An aggregation-write target split into source field updates and scrapped. Never written to valtio.
    Consumed,
}

/// Whether this change is establishing a value for the first time, or transitioning from a known prior state.
#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, TS)]
pub enum ChangeContext {
    /// No prior value existed at the root of this causal chain.
    Initial,
    /// Transitioning from a known prior value. All stages apply.
    Mutation,
}

/// The causal chain of a change.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Default, TS)]
pub enum Lineage {
    /// Came directly from JS. No pipeline parent.
    #[default]
    Input,
    /// Produced by a stage from a parent change.
    Derived {
        parent_id: u32,
        #[ts(inline)]
        via: Stage,
        #[ts(inline)]
        context: ChangeContext,
    },
}

impl Lineage {
    #[allow(dead_code)]
    pub fn context(&self) -> ChangeContext {
        match self {
            Lineage::Input => ChangeContext::Mutation,
            Lineage::Derived { context, .. } => *context,
        }
    }
}

/// Debug-mode only audit information.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, TS)]
pub struct ChangeAudit {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub source_path: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub logic_id: Option<u32>,
}

/// A single change in the input/output format.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, TS)]
pub struct Change {
    pub path: String,
    pub value_json: String,
    /// Coarse classification: Real | Redundant | Breakdown | Consumed.
    #[serde(default)]
    #[ts(inline)]
    pub kind: ChangeKind,
    /// Causal chain. Always set. Carries ChangeContext for stage routing.
    #[serde(default)]
    #[ts(inline)]
    pub lineage: Lineage,
    /// Debug-mode only. None in production.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[ts(inline)]
    pub audit: Option<ChangeAudit>,
}

impl Change {
    #[allow(dead_code)]
    pub fn new(path: String, value_json: String) -> Self {
        Self {
            path,
            value_json,
            kind: ChangeKind::Real,
            lineage: Lineage::Input,
            audit: None,
        }
    }
}

/// Why a change was not processed by a stage.
#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "snake_case")]
#[derive(TS)]
pub enum SkipReason {
    /// Change kind doesn't match what this stage processes.
    WrongKind,
    /// Stage guard condition was not met.
    GuardFailed,
    /// Value matches current shadow state — no-op.
    Redundant,
    /// Anchor path not present in state — resource skipped.
    AnchorMissing,
}

/// A change that was rejected by a stage (captured in StageTrace only).
#[derive(Serialize, Deserialize, Debug, Clone, TS)]
pub struct SkippedChange {
    pub path: String,
    #[ts(inline)]
    pub kind: ChangeKind,
    #[ts(inline)]
    pub reason: SkipReason,
    /// Human-readable explanation of why this change was skipped.
    #[serde(default, skip_serializing_if = "String::is_empty")]
    pub detail: String,
    /// Which registration owns the resource that was skipped (if applicable).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub registration_id: Option<String>,
    /// Anchor path that caused the skip (if applicable).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub anchor_path: Option<String>,
}

/// A change produced by a pipeline stage (richer than a simple path-value pair).
#[derive(Serialize, Deserialize, Debug, Clone, TS)]
pub struct ProducedChange {
    pub path: String,
    /// JSON-encoded value.
    pub value: String,
    /// Which registration produced this change (if traceable).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub registration_id: Option<String>,
    /// What input path caused this (e.g. sync source path).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub source_path: Option<String>,
}

/// Execution record for one StagePolicy within a pipeline call.
#[derive(Serialize, Deserialize, Debug, Clone, TS)]
pub struct StageTrace {
    #[ts(inline)]
    pub stage: Stage,
    pub duration_us: u64,
    /// Paths (with JSON-encoded values) that this stage processed.
    pub matched: Vec<[String; 2]>,
    #[ts(inline)]
    pub skipped: Vec<SkippedChange>,
    /// Changes produced by this stage.
    #[ts(inline)]
    pub produced: Vec<ProducedChange>,
    pub followup: Vec<StageTrace>,
}

/// Full trace for one run_pipeline_core call.
#[derive(Serialize, Deserialize, Debug, Clone, Default, TS)]
pub struct PipelineTrace {
    pub total_duration_us: u64,
    pub stages: Vec<StageTrace>,
    /// Anchor path → present? Gives display layer full context on anchor-dependent resources.
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub anchor_states: HashMap<String, bool>,
}

/// Raw sentinel value for JS `undefined` (without JSON quotes).
pub(crate) const UNDEFINED_SENTINEL: &str = "__APEX_UNDEFINED__";

/// JSON-encoded sentinel for JS `undefined` values crossing the WASM boundary.
pub(crate) const UNDEFINED_SENTINEL_JSON: &str = "\"__APEX_UNDEFINED__\"";
