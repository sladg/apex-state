# WASM-EP9: Pipeline Context Refactor

**Type**: Epic
**Priority**: P2
**Depends on**: WASM-EP5 (prepare/finalize split already done)
**Goal**: Eliminate duplication between `process_changes_vec` and `prepare_changes`, unify all per-call mutable state into one `PipelineContext` type, replace scattered stage gates with a declarative `STAGE_POLICIES` table, introduce structured change lineage and classification, and add a debug audit trail.

---

## Problems

### 1. `process_changes_vec` and `prepare_changes` are ~80 lines of duplicated pipeline logic

Steps 0–11 (diff → aggregation → shadow apply → clear paths → sync → flip → aggregation reads → computation reads → BoolLogic → ValueLogic → validators → execution plan) are duplicated verbatim. Changes to pipeline logic must be made in two places.

### 2. Per-call state is scattered across seven `buf_*` fields on `ProcessingPipeline`

```rust
buf_output: Vec<Change>,
buf_sync: Vec<Change>,
buf_flip: Vec<Change>,
buf_affected_ids: HashSet<u32>,
buf_concern_changes: Vec<Change>,
buf_affected_validators: HashSet<u32>,
buf_affected_value_logics: HashSet<u32>,
```

These live on the struct alongside permanent state (registries, shadow, intern), making the distinction between "per-call scratch space" and "persistent state" invisible.

### 3. `origin: Option<String>` is incomplete and untyped

Only `"sync"` and `"flip"` are ever set. All other pipeline steps produce changes with `origin: None`. No structured way to audit why a change was produced or what caused it.

### 4. No change classification — all changes look the same

The pipeline produces fundamentally different kinds of changes that require different treatment:

- **Real** — output written to valtio
- **Redundant** — value matches shadow state; not written to valtio, but listener-observable
- **Breakdown** — leaf-level expansions of a complex parent change, used only for internal pipeline routing. The parent real change covers valtio.
- **Consumed** — an aggregation-write target change split into source field updates and scrapped. Aggregation recomputes the target from sources. Never applied.

### 5. No change lineage — routing rules are scattered and implicit

Stage gates are buried inside individual step functions as ad-hoc checks (e.g. "skip if no prior value"). There is no central declaration of what stages a change can enter, or why. Adding a new routing rule requires hunting through code.

### 6. `Change` is defined in `pipeline.rs` but used by subsystems

`aggregation.rs`, `clear_paths.rs`, and `router.rs` all import `Change` from `pipeline.rs`. This is a wrong dependency direction — subsystems importing from their orchestrator. `Change` is a foundational cross-cutting type.

---

## Solution Overview

The refactor introduces five concepts that work together:

1. **`change.rs`** — extracts `Change` and all related types into their own module, fixing the dependency direction.
2. **`Stage` enum** — canonical names for every pipeline stage, used for routing, lineage, and audit.
3. **`Lineage` + `ChangeContext`** — every change carries a structured causal chain. Context (`Initial` / `Mutation`) propagates through derived changes and drives stage gate decisions.
4. **`StagePolicy` + `STAGE_POLICIES`** — the pipeline is declared as a static table of policies. Each policy defines what a stage accepts, what it emits, how context inherits, and whether it has nested followup stages. Stage gates are declared here, not scattered in code.
5. **`PipelineContext`** — all per-call scratch state in one struct, separate from permanent pipeline state.

---

## A. File Structure Change

Extract all change-related types into `rust/src/change.rs`. This fixes the dependency direction — subsystems import from `change`, not from `pipeline`.

```
rust/src/
├── change.rs          ← NEW: Stage, ChangeKind, ChangeContext, Lineage,
│                               Change, ChangeAudit, UNDEFINED_SENTINEL_JSON
├── pipeline.rs        ← ProcessingPipeline, PipelineContext, StagePolicy,
│                          STAGE_POLICIES, executor, run_pipeline_core
├── aggregation.rs     ← imports from crate::change (was crate::pipeline)
├── clear_paths.rs     ← imports from crate::change (was crate::pipeline)
├── router.rs          ← imports from crate::change (was crate::pipeline)
└── ...
```

---

## B. `Stage` enum

Canonical names for every pipeline stage. Used in `StagePolicy`, `Lineage`, and `ChangeAudit`. Declared in `change.rs`.

```rust
#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[serde(rename_all = "snake_case")]
pub(crate) enum Stage {
    Input,             // entered from JS — not a processing stage, marks origin
    AggregationWrite,  // distribute write target to sources (produces Consumed)
    Diff,              // shadow compare, breakdown expansion
    Sync,              // sync graph propagation
    Flip,              // flip graph propagation
    ClearPath,         // trigger-based path clearing
    AggregationRead,   // recompute aggregation targets from sources
    Computation,       // SUM/AVG recomputation
    BoolLogic,         // BoolLogic expression re-evaluation (produces Concern)
    ValueLogic,        // ValueLogic expression re-evaluation (produces Concern)
    Listeners,         // route to listener dispatch
    Apply,             // write to valtio (skipped if Redundant)
}
```

---

## C. `ChangeKind` enum

Coarse classification answering **"should this change be written to valtio?"**. Declared in `change.rs`.

```rust
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "snake_case")]
pub(crate) enum ChangeKind {
    /// Written to valtio. The default for pipeline outputs.
    Real,

    /// Value matches current shadow state. Not written to valtio,
    /// but passed to listener dispatch — listeners may still care.
    Redundant,

    /// Leaf-level expansion of a complex parent change (e.g. user = {...} →
    /// user.name, user.address.city, ...). Used for listener routing, BoolLogic
    /// evaluation, and sync/flip only. The parent Real change covers valtio.
    Breakdown,

    /// An aggregation-write target split into source field updates and scrapped.
    /// Aggregation will recompute the target from its sources.
    /// Never written to valtio.
    Consumed,
}
```

---

## D. `ChangeContext` and `Lineage`

Every change carries structured lineage. `ChangeContext` propagates forward through derived changes and is the mechanism by which stage gate decisions (e.g. "skip flip for initial values") are made without scattered ad-hoc checks. Declared in `change.rs`.

```rust
/// Whether this change (or the root of its causal chain) is establishing
/// a value for the first time, or transitioning from a known prior state.
/// Propagated through Lineage::Derived — if the root was Initial, every
/// derived change in the chain is also Initial.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub(crate) enum ChangeContext {
    /// No prior value existed at the root of this causal chain.
    /// Flip is meaningless: there is no prior boolean to invert.
    Initial,

    /// Transitioning from a known prior value. All stages apply.
    Mutation,
}

/// The causal chain of a change — where it came from and what produced it.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub(crate) enum Lineage {
    /// Came directly from JS. No pipeline parent.
    Input,

    /// Produced by a stage from a parent change.
    Derived {
        /// Interned path ID of the change that caused this one.
        /// Resolve via intern table for display/debug.
        parent_id: u32,

        /// Which stage produced this change from the parent.
        via: Stage,

        /// Inherited from parent. If the root input was Initial,
        /// every change in the causal chain is also Initial.
        context: ChangeContext,
    },
}

impl Lineage {
    pub fn context(&self) -> ChangeContext {
        match self {
            Lineage::Input => ChangeContext::Mutation, // default: assume prior existed
            Lineage::Derived { context, .. } => *context,
        }
    }
}
```

---

## E. `ChangeAudit` — debug-mode only

Collected only when `pipeline.debug == true`. Provides human-readable context for DevTools. Declared in `change.rs`.

```rust
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub(crate) struct ChangeAudit {
    /// Resolved path string of the parent change (from Lineage.parent_id).
    /// Populated in debug mode to avoid intern lookup in production.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub source_path: Option<String>,

    /// For BoolLogic/ValueLogic: the registered logic ID that fired.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub logic_id: Option<u32>,
}
```

---

## F. `Change` struct

`origin: Option<String>` is removed. `Lineage` replaces it. Declared in `change.rs`.

```rust
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub(crate) struct Change {
    pub path: String,
    pub value_json: String,

    /// Coarse classification: Real | Redundant | Breakdown | Consumed.
    pub kind: ChangeKind,

    /// Causal chain. Always set. Carries ChangeContext for stage routing.
    pub lineage: Lineage,

    /// Debug-mode only. None in production.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub audit: Option<ChangeAudit>,
}
```

**`meta` is intentionally absent from this struct.** WASM has no use for it — meta doesn't affect any pipeline computation. Propagating opaque user data through every derived change would cross the JS↔WASM boundary unnecessarily. Meta is attached to output changes by JS after the pipeline returns. See section Q.

---

## G. `StagePolicy` and `ContextRule`

The policy table declares how each stage routes changes. Declared in `pipeline.rs` alongside the executor. This replaces all scattered stage gate checks in individual step functions.

### Cross-stage lineage: `accepts` as the routing mechanism

`accepts` solves the cross-stage lineage problem: different downstream stages need different answers for the same change. A `Sync`-produced `Redundant` change (peer already had that value) should **not** enter `Flip` (nothing changed, nothing to flip) but **should** reach `Listeners` (a listener may care that sync was attempted).

This is declared entirely in the policy table — no new types, no extra flags on the change:

```
Sync      → emits Real or Redundant (depending on shadow comparison)
Flip      → accepts: [Real]            ← Redundant never enters Flip
Listeners → accepts: [Real, Redundant] ← Redundant reaches Listeners
Apply     → accepts: [Real]            ← Redundant never written to valtio
```

The `accepts` array IS the cross-stage routing declaration. Every inter-stage lineage rule lives here and nowhere else.

```rust
/// How a stage assigns ChangeContext to the changes it produces.
pub(crate) enum ContextRule {
    /// Produced changes inherit the parent's ChangeContext directly.
    /// An Initial parent produces Initial children — restrictions propagate.
    Inherit,

    /// Produced changes always get this context, regardless of parent.
    Always(ChangeContext),
}

/// Declares how one pipeline stage processes and produces changes.
pub(crate) struct StagePolicy {
    pub stage: Stage,

    /// ChangeKinds this stage will process. Changes with other kinds are skipped.
    /// This is the primary cross-stage routing mechanism — stages that should
    /// not receive Redundant changes simply omit it from this list.
    pub accepts: &'static [ChangeKind],

    /// Additional guard checked after `accepts`. Return false to skip this change.
    /// Common use: `|c| matches!(c.lineage.context(), ChangeContext::Mutation)`
    pub guard: fn(&Change) -> bool,

    /// Primary ChangeKind assigned to changes this stage produces.
    /// A stage may produce Redundant instead of Real when shadow comparison
    /// reveals the value already matches (e.g. Sync peer already had the value).
    pub emits: ChangeKind,

    /// How produced changes inherit ChangeContext from their parent.
    pub context_rule: ContextRule,

    /// Sub-pipeline: run these policies on the changes THIS stage produced —
    /// not on all accumulated changes. Produced output joins the main accumulator.
    /// Enables scoped followup: e.g. run Sync after BoolLogic, but only on
    /// concern changes.
    pub followup: &'static [StagePolicy],
}
```

---

## H. `STAGE_POLICIES` — the pipeline declaration

The complete pipeline, declared as data. Reading this top-to-bottom tells you the entire pipeline. Adding a new stage means adding one entry here.

```rust
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
        emits: ChangeKind::Breakdown,           // complex values → leaf breakdowns
        context_rule: ContextRule::Always(ChangeContext::Mutation),
        followup: &[],
    },
    StagePolicy {
        stage: Stage::Sync,
        accepts: &[ChangeKind::Real],
        guard: |_| true,
        // Emits Real when the sync peer's value actually changed.
        // Emits Redundant when the peer already had the incoming value —
        // the change is preserved for Listeners but excluded from Flip and Apply
        // via their accepts arrays.
        emits: ChangeKind::Real,
        context_rule: ContextRule::Inherit,     // Initial input → sync inherits Initial
        followup: &[],
    },
    StagePolicy {
        stage: Stage::Flip,
        // accepts: [Real] only — Redundant sync changes never enter Flip.
        // A sync change that found the peer already had the value did nothing;
        // there is no state transition to invert.
        accepts: &[ChangeKind::Real],
        // Second gate: no prior value = no flip. Initial changes (and all their
        // descendants) are excluded automatically via inherited ChangeContext.
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
        emits: ChangeKind::Real,                // concern path updates are Real
        context_rule: ContextRule::Inherit,
        // Example of nested followup: run Sync on concern changes only.
        // Sync here only sees BoolLogic output, not all accumulated changes.
        followup: &[
            // StagePolicy { stage: Stage::Sync, accepts: &[ChangeKind::Real], ... }
            // Uncomment when sync-after-concern is needed.
        ],
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
        // accepts: [Real, Redundant] — listeners see both.
        // Real: the value changed, listener reacts to the new value.
        // Redundant: the sync/flip attempt found no change, but a listener
        // may still want to know the attempt was made (e.g. for auditing,
        // conditional logic based on sync events, or idempotency checks).
        accepts: &[ChangeKind::Real, ChangeKind::Redundant],
        guard: |_| true,
        emits: ChangeKind::Real,
        context_rule: ContextRule::Inherit,
        followup: &[],
    },
    StagePolicy {
        stage: Stage::Apply,
        // accepts: [Real] only — Redundant changes are never written to valtio.
        // They passed through Listeners above but stop here.
        accepts: &[ChangeKind::Real],
        guard: |_| true,
        emits: ChangeKind::Real,
        context_rule: ContextRule::Inherit,
        followup: &[],
    },
];
```

---

## I. Nested followup pipelines

`followup: &'static [StagePolicy]` enables scoped sub-pipelines. The key semantic:

- **Scoped input**: followup stages receive **only the parent stage's output** as their frontier, not all accumulated changes.
- **Shared output**: followup output joins the **main accumulator** and is visible to subsequent outer stages.
- **Arbitrary depth**: each `StagePolicy` can have its own `followup`, enabling nesting to any depth.

Example — sync pairs after BoolLogic, only on concern changes; with flip only on those sync outputs that had prior values:

```rust
// In STAGE_POLICIES:
StagePolicy {
    stage: Stage::BoolLogic,
    // ...
    followup: &[
        StagePolicy {
            stage: Stage::Sync,
            accepts: &[ChangeKind::Real],   // only concern changes from BoolLogic
            // ...
            followup: &[
                StagePolicy {
                    stage: Stage::Flip,
                    accepts: &[ChangeKind::Real],
                    guard: |c| matches!(c.lineage.context(), ChangeContext::Mutation),
                    // ...
                    followup: &[],
                },
            ],
        },
    ],
},
```

---

## J. Executor model — frontier + accumulator

Two concepts drive change routing:

- **Accumulator** (`ctx.changes`) — all changes produced this call, in order. The final output.
- **Frontier** — changes produced by the immediately preceding stage. Input to the next stage.

Followup stages receive a **scoped frontier** (parent's output only). Their output joins the shared accumulator.

`execute_policy` returns `Option<StageTrace>` — `Some` when `self.debug == true`, `None` in production. The trace tree builds itself through recursive return values. No prop drilling, no tracer parameter, no struct-level tracer field — `self.debug` is already accessible.

```rust
fn execute_policy(
    &mut self,
    policy: &StagePolicy,
    frontier: &[Change],        // scoped: only parent stage's output
    accumulator: &mut Vec<Change>,
) -> Option<StageTrace> {
    let mut trace = self.debug.then(|| StageTrace {
        stage: policy.stage,
        accepted: Vec::new(),
        skipped: Vec::new(),
        produced: Vec::new(),
        followup: Vec::new(),
    });

    let eligible: Vec<&Change> = frontier.iter()
        .filter(|c| {
            let kind_ok = policy.accepts.contains(&c.kind);
            let guard_ok = kind_ok && (policy.guard)(c);
            if let Some(t) = &mut trace {
                if guard_ok {
                    t.accepted.push(c.path.clone());
                } else {
                    t.skipped.push(SkippedChange {
                        path: c.path.clone(),
                        kind: c.kind.clone(),
                        reason: if kind_ok { SkipReason::GuardFailed } else { SkipReason::WrongKind },
                    });
                }
            }
            guard_ok
        })
        .collect();

    let produced = self.run_stage(policy.stage, &eligible);
    let produced = apply_context_rule(produced, &eligible, policy.context_rule);

    if let Some(t) = &mut trace {
        t.produced.extend(produced.iter().map(|c| c.path.clone()));
    }

    // followup sees only what THIS stage produced (scoped frontier)
    for followup in policy.followup {
        let followup_trace = self.execute_policy(followup, &produced, accumulator);
        if let (Some(t), Some(ft)) = (&mut trace, followup_trace) {
            t.followup.push(ft);  // trace tree mirrors the policy tree
        }
    }

    // output joins the shared accumulator
    accumulator.extend(produced);
    trace
}

fn run_pipeline_core(&mut self, input: Vec<Change>) -> Result<bool, String> {
    self.ctx.clear();

    if input.is_empty() { return Ok(false); }

    self.ctx.changes.extend(input.iter().cloned());
    let mut frontier = input;

    for policy in STAGE_POLICIES {
        let before = self.ctx.changes.len();
        let stage_trace = self.execute_policy(policy, &frontier, &mut self.ctx.changes);
        if let Some(t) = stage_trace {
            self.ctx.trace.stages.push(t);
        }
        // frontier for next outer stage = what was just added
        frontier = self.ctx.changes[before..].to_vec();
    }

    Ok(true)
}
```

---

## K. `PipelineContext` — all per-call state unified

One type, lives on `ProcessingPipeline` as `ctx`. Cleared at the start of every `run_pipeline_core` call. Never freshly allocated — `clear()` reuses capacity.

```rust
pub(crate) struct PipelineContext {
    /// The accumulator: all changes produced this call (Real + Redundant +
    /// Breakdown + Consumed), in order of production. Filtered at output.
    pub changes: Vec<Change>,

    /// HashSets kept persistent — avoid rehash cost across calls.
    pub affected_bool_logic: HashSet<u32>,
    pub affected_validators: HashSet<u32>,
    pub affected_value_logics: HashSet<u32>,

    /// Assembled during the call, read by callers after run_pipeline_core.
    pub validators_to_run: Vec<ValidatorDispatch>,
    pub execution_plan: Option<FullExecutionPlan>,

    /// Built by execute_policy when ProcessingPipeline.debug == true.
    /// Empty (no allocation beyond the Vec itself) when debug == false.
    /// Cleared via stages.clear() on each call — reuses Vec capacity.
    pub trace: PipelineTrace,
}

impl PipelineContext {
    pub fn new() -> Self { /* with_capacity hints */ }

    pub fn clear(&mut self) {
        self.changes.clear();
        self.affected_bool_logic.clear();
        self.affected_validators.clear();
        self.affected_value_logics.clear();
        self.validators_to_run.clear();
        self.execution_plan = None;
        self.trace.stages.clear();  // reuse Vec capacity
    }
}
```

---

## L. `ProcessingPipeline` struct

```rust
pub(crate) struct ProcessingPipeline {
    // Permanent state (registries, graphs, routers)
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
    value_logic_registry: ValueLogicRegistry,
    value_logic_rev_index: ReverseDependencyIndex,

    // Per-call scratch space (replaces seven buf_* fields)
    ctx: PipelineContext,

    // Cross-call: survives between prepare_changes() and pipeline_finalize()
    buf_pending_state_changes: Vec<Change>,
    buf_pending_concern_changes: Vec<Change>,

    /// When true: builds StageTrace in execute_policy, populates ChangeAudit on
    /// every Change, and includes non-Real changes in output for DevTools.
    /// Passed in from JS store config as `debug: true`.
    pub debug: bool,
}
```

**Removed from struct**: `buf_output`, `buf_sync`, `buf_flip`, `buf_affected_ids`, `buf_concern_changes`, `buf_affected_validators`, `buf_affected_value_logics` — 7 fields → 1 (`ctx`).

---

## M. Callers after refactor

**`process_changes_vec`** — simple path, no prepare/finalize split:

```rust
pub(crate) fn process_changes_vec(&mut self, input: Vec<Change>) -> Result<ProcessResult, String> {
    if !self.run_pipeline_core(input)? {
        return Ok(ProcessResult::empty());
    }
    if !self.debug {
        self.ctx.changes.retain(|c| c.kind == ChangeKind::Real);
    }
    Ok(ProcessResult {
        changes: std::mem::take(&mut self.ctx.changes),
        validators_to_run: std::mem::take(&mut self.ctx.validators_to_run),
        execution_plan: self.ctx.execution_plan.take(),
    })
}
```

**`prepare_changes`** — partitions accumulator into state vs concern by stage, after filtering:

```rust
pub(crate) fn prepare_changes(&mut self, input: Vec<Change>) -> Result<PrepareResult, String> {
    self.buf_pending_state_changes.clear();
    self.buf_pending_concern_changes.clear();

    if !self.run_pipeline_core(input)? {
        return Ok(PrepareResult::empty());
    }

    for change in self.ctx.changes.drain(..) {
        if !self.debug && change.kind != ChangeKind::Real { continue; }
        match change.lineage {
            // Concern stages: BoolLogic and ValueLogic outputs → concern buffer
            Lineage::Derived { via: Stage::BoolLogic, .. }
            | Lineage::Derived { via: Stage::ValueLogic, .. } => {
                self.buf_pending_concern_changes.push(change);
            }
            _ => self.buf_pending_state_changes.push(change),
        }
    }

    Ok(PrepareResult {
        state_changes: self.buf_pending_state_changes.clone(),
        validators_to_run: std::mem::take(&mut self.ctx.validators_to_run),
        execution_plan: self.ctx.execution_plan.take(),
        has_work: !self.buf_pending_state_changes.is_empty()
            || !self.buf_pending_concern_changes.is_empty()
            || !self.ctx.validators_to_run.is_empty()
            || self.ctx.execution_plan.is_some(),
    })
}
```

---

## N. Tracing types — Rust side (`change.rs`)

`SkipReason`, `SkippedChange`, `StageTrace`, and `PipelineTrace` are declared in `change.rs` alongside `Change`. They are serialized to JSON and returned to JS when `debug == true`.

```rust
/// Why a change was not processed by a stage.
#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "snake_case")]
pub(crate) enum SkipReason {
    /// change.kind not in StagePolicy.accepts
    WrongKind,
    /// change.kind matched but StagePolicy.guard returned false
    GuardFailed,
}

/// A change that entered a stage but was rejected. Captured in StageTrace
/// only — never in the accumulator. This is the key gap Lineage can't fill:
/// rejected changes don't exist as Change values.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub(crate) struct SkippedChange {
    pub path: String,
    pub kind: ChangeKind,
    pub reason: SkipReason,
}

/// Execution record for one StagePolicy within a pipeline call.
/// Structure mirrors StagePolicy — nested followup traces match nested policies.
/// All Vec fields are empty (not None) when debug == false, so the type is
/// always valid but carries no data in production.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub(crate) struct StageTrace {
    pub stage: Stage,
    pub duration_us: u64,               // wall time this stage took (including followup)
    pub accepted: Vec<String>,          // paths of changes that passed the guard
    pub skipped: Vec<SkippedChange>,    // changes that entered but were rejected
    pub produced: Vec<String>,          // paths of changes this stage created
    pub followup: Vec<StageTrace>,      // traces for each nested followup policy
}

/// Full trace for one run_pipeline_core call.
/// Returned to JS as part of ProcessResult / PrepareResult when debug == true.
#[derive(Serialize, Deserialize, Debug, Clone, Default)]
pub(crate) struct PipelineTrace {
    /// Wall time for the entire run_pipeline_core call (all stages combined).
    pub total_duration_us: u64,
    /// One entry per top-level StagePolicy in STAGE_POLICIES (in execution order).
    pub stages: Vec<StageTrace>,
}
```

---

## O. Debug flags — two levels, two concerns

Two separate flags control observability. They are independent.

| Flag | Where | What it controls |
|------|-------|-----------------|
| `debug: bool` | WASM `ProcessingPipeline` | Builds `StageTrace` in `execute_policy`. Populates `ChangeAudit` on every `Change`. Includes non-`Real` changes in output. |
| `verbose: boolean` | JS store config | Whether `logPipelineTrace()` is called automatically. Does not affect WASM. |

`debug` is a WASM concern — it determines what data is produced. `verbose` is a JS concern — it determines whether that data is logged to the console.

Redux DevTools always receives the trace when `debug` is on. Console logging is opt-in via `verbose`.

```typescript
// Store config — both are optional, both default to false
type StoreConfig<T> = {
  // ...
  debug?: boolean    // → passed to WASM, enables trace + ChangeAudit + all change kinds in output
  verbose?: boolean  // → JS only, enables automatic console.group logging per pipeline call
}
```

---

## P. Visualization — JS side

### P1. Console groups

`logPipelineTrace()` renders the `PipelineTrace` as nested `console.group` calls, color-coded by stage, collapsed by default for no-op stages. Called automatically when `verbose: true`.

**Visual output:**

```
▼ apex:pipeline  user.name, user.role  [2.3ms]
  ▶ AggregationWrite  0 accepted  (collapsed — nothing happened)
  ▼ Sync  2 accepted · 1 skipped
      ✓  user.name  →  profile.name
      ✓  user.email  →  profile.email
      ✗  user.age  [guard_failed: Initial context]
  ▼ BoolLogic  1 accepted
      ✓  user.role  →  _concerns.email.disabledWhen
      ▼ followup: Sync  1 accepted
          ✓  _concerns.email.disabledWhen  →  _concerns.name.disabledWhen
```

**Color scheme:**

| Element | Color |
|---------|-------|
| Stage name (outer) | `#6366f1` indigo, bold |
| Stage name (followup) | `#a78bfa` lighter indigo |
| Accepted / produced | `#22c55e` green |
| Skipped | `#94a3b8` slate grey |
| Skip reason | `#64748b` muted |
| Input paths | `#f59e0b` amber |
| Timing | `#64748b` muted |

**Key behaviors:**

- Stages with zero accepted and zero produced are `console.groupCollapsed` — zero noise
- Skipped entries show the reason inline (`guard_failed`, `wrong_kind`)
- Followup stages are indented and labeled `followup: StageName`
- The top-level group label includes timing and input paths for quick scanning

```typescript
const STAGE_COLOR    = 'color:#6366f1;font-weight:bold'
const FOLLOWUP_COLOR = 'color:#a78bfa;font-weight:bold'
const ACCEPTED       = 'color:#22c55e'
const SKIPPED        = 'color:#94a3b8'
const MUTED          = 'color:#64748b'
const TIMING         = 'color:#64748b;font-style:italic'

const logStageTrace = (trace: StageTrace, depth = 0): void => {
  const color = depth === 0 ? STAGE_COLOR : FOLLOWUP_COLOR
  const prefix = depth > 0 ? 'followup: ' : ''
  const timing = trace.duration_us > 0 ? ` [${(trace.duration_us / 1000).toFixed(2)}ms]` : ''
  const label = `${prefix}%c${trace.stage}%c  ${trace.accepted.length} accepted · ${trace.skipped.length} skipped%c${timing}`
  const isEmpty = trace.accepted.length === 0 && trace.produced.length === 0

  isEmpty
    ? console.groupCollapsed(label, color, '', TIMING)
    : console.group(label, color, '', TIMING)

  trace.skipped.forEach(s =>
    console.log(`%c✗  ${s.path}  %c[${s.reason}]`, SKIPPED, MUTED)
  )
  trace.produced.forEach(p =>
    console.log(`%c✓  ${p}`, ACCEPTED)
  )
  trace.followup.forEach(f => logStageTrace(f, depth + 1))
  console.groupEnd()
}

export const logCallTrace = (call: CallTrace): void => {
  const inputPaths = call.inputs.map(i => i.path).join(', ')
  const wasmMs = (call.pipeline.total_duration_us / 1000).toFixed(1)
  const jsMs = call.functions.reduce((s, f) => s + f.duration_ms, 0).toFixed(1)
  console.group(
    `%capex:pipeline%c  ${inputPaths}  [wasm ${wasmMs}ms · js ${jsMs}ms]`,
    'color:#f59e0b;font-weight:bold', ''
  )
  call.pipeline.stages.forEach(s => logStageTrace(s))
  if (call.functions.length > 0) {
    console.groupCollapsed('%cfunctions', 'color:#8b5cf6;font-weight:bold')
    call.functions.forEach(f =>
      console.log(`%c${f.kind}%c  ${f.id}  %c[${f.duration_ms.toFixed(2)}ms]`,
        'color:#8b5cf6', '', TIMING)
    )
    console.groupEnd()
  }
  console.groupEnd()
}
```

### P2. JS function timing — `FunctionTrace`

Listener handlers, validators, and `evaluate()` callbacks execute in JS *after* WASM returns. The calling code wraps each invocation with `performance.now()` to capture duration. These are collected alongside the WASM `PipelineTrace` into a single `CallTrace` before Redux DevTools dispatch.

No changes to listener/validator function signatures — timing is measured transparently by the executor wrapping each call.

**`performance.now()` precision caveat**: browsers reduce precision to 1ms by default (Spectre/Meltdown mitigation). Sub-millisecond listener timing requires cross-origin isolation headers (`Cross-Origin-Opener-Policy: same-origin` + `Cross-Origin-Embedder-Policy: require-corp`). Without them, a 0.3ms listener reads as either 0ms or 1ms. In Node.js (tests, SSR) precision is ~1µs regardless.

**Timing overhead**: two `performance.now()` calls per function cost ~0.1µs each. For 200 listeners the total measurement overhead is ~0.02ms — negligible against any realistic frame budget.

**Listener execution model**: listener handlers are CPU-bound (path checks, object traversal, small changeset production). `Promise.all` over a stage's nodes does not provide CPU parallelism — JavaScript is single-threaded. The staged execution plan's value is **correct dependency ordering** (group listeners see complete product outputs; root listeners accumulate in declared order), not concurrent CPU execution.

```typescript
type FunctionKind = 'listener' | 'validator' | 'evaluate'

type FunctionTrace = {
  kind: FunctionKind
  id: string             // registered listener / validator / concern ID
  path: string           // path it was registered on
  duration_ms: number
  triggered_by: string[] // which change paths caused this function to fire
  exec_id: string        // execution plan node ID — links this trace to the dispatch plan (e.g. "P0_L1", "G0_L0", "R3")
  stage_level: number    // which stage level the node ran in: 0 = product, 1 = group, 2 = root (sequential)
  meta: unknown          // propagated from the root Input change(s) that triggered this function
                         // undefined when no meta was provided; may be an array if triggered by
                         // multiple inputs with different metas
}

/// The complete trace for one pipeline call — both phases combined.
/// Phase 1: WASM processing  → pipeline: PipelineTrace
/// Phase 2: JS execution     → functions: FunctionTrace[]
/// Assembled in JS after all listeners / validators complete.
/// This is what gets dispatched to Redux DevTools and logged to console.
type CallTrace = {
  inputs: Change[]          // original input changes (for labelling)
  pipeline: PipelineTrace   // WASM trace with per-stage timing
  functions: FunctionTrace[] // JS-side execution records, in call order
}
```

The executor wraps every JS function call:

```typescript
const traceFunction = <T>(
  record: FunctionTrace[],
  meta: Omit<FunctionTrace, 'duration_ms'>,
  fn: () => T,
): T => {
  const start = performance.now()
  const result = fn()
  record.push({ ...meta, duration_ms: performance.now() - start })
  return result
}

// Usage in the JS pipeline executor:
const result = traceFunction(callTrace.functions, {
  kind: 'listener',
  id: listenerId,
  path: listenerPath,
  triggered_by: triggeringPaths,
  exec_id: node.execId,
  stage_level: stage.level,
}, () => listenerHandler(changes))
```

### P3. Redux DevTools

Each complete pipeline call (WASM + JS) dispatches one action. The `CallTrace` is the full payload.

**Action shape:**

```typescript
type PipelineAction = {
  type: string      // input paths joined — label shown in the DevTools timeline
  callTrace: CallTrace
}
```

**Integration** — dispatched after all listeners/validators complete for this call:

```typescript
// One-time setup (store init):
const devTools = window.__REDUX_DEVTOOLS_EXTENSION__?.connect({ name: 'apex-state' })

// After all JS functions complete:
if (debug && devTools) {
  devTools.send(
    {
      type: callTrace.inputs.map(c => c.path).join(', '),
      callTrace,
    },
    { state: snapshot(state), _concerns: snapshot(_concerns) }
  )
}
```

**What Redux DevTools shows:**

- **Timeline** — every complete pipeline call (wasm + js), labelled by input paths
- **Diff tab** — exactly what changed in `state` and `_concerns`
- **Action tab** — full `callTrace`: WASM stages with per-stage timing, then JS functions with per-function timing. Skipped entries explain why paths did not change.
- **State tab** — full snapshot at that point; enables time-travel debugging

### P4. Performance marks (optional, zero-config)

Two levels of marks — WASM stages and JS listener functions — both show as color bars in the browser Performance tab without any additional tooling.

**WASM stage marks** — called once per stage after `logCallTrace`:

```typescript
const markStageTrace = (trace: StageTrace): void => {
  performance.mark(`apex:${trace.stage}:start`)
  trace.followup.forEach(markStageTrace)
  performance.measure(`apex:${trace.stage}`, `apex:${trace.stage}:start`)
}
```

**JS listener marks** — integrated into `traceFunction` when `marks: true`:

```typescript
const traceFunction = <T>(
  record: FunctionTrace[],
  meta: Omit<FunctionTrace, 'duration_ms'>,
  fn: () => T,
  marks = false,
): T => {
  const markId = `apex:${meta.kind}:${meta.exec_id}`
  if (marks) performance.mark(`${markId}:start`)
  const start = performance.now()
  const result = fn()
  const duration_ms = performance.now() - start
  if (marks) performance.measure(markId, `${markId}:start`)
  record.push({ ...meta, duration_ms })
  return result
}
```

In the Performance tab: WASM stages appear as one contiguous block (the WASM call), followed by individual listener bars — one per node in the dispatch plan. Stage level is visible from `exec_id` prefix (`P` = product, `G` = group, `R` = root).

**Precision note**: `performance.mark` / `performance.measure` use the same clock as `performance.now()` — sub-millisecond bars only appear with COOP+COEP headers active.

---

## Q. Meta — JS-only, attached post-pipeline

User-provided metadata does not cross the WASM boundary. WASM has no use for it — meta is opaque to the pipeline and affects no computation. It is attached to output changes by JS after `processChanges` / `pipelineFinalize` returns, before changes reach listeners.

### JS API — third parameter

```typescript
// Single change with meta
store.setValue('user.name', 'Alice', { requestId: 'abc-123', source: 'profile-form' })

// Batch with per-change meta
store.applyChanges([
  { path: 'user.name', value: 'Alice', meta: { requestId: 'abc-123' } },
  { path: 'user.age',  value: 30,      meta: { requestId: 'abc-123' } },
])
```

### Post-pipeline attachment

JS tracks the inputs it sent in a given call alongside their metas. After WASM returns output changes, `attachMeta` stamps every output before it reaches listeners or DevTools:

```typescript
const attachMeta = (outputs: Change[], inputs: InputChange[]): Change[] => {
  // Common case: single input or all inputs share the same meta.
  // All outputs get that meta — no path matching needed.
  const uniqueMetas = [...new Set(inputs.map(i => i.meta))]
  if (uniqueMetas.length === 1) {
    return outputs.map(c => ({ ...c, meta: uniqueMetas[0] }))
  }

  // Multi-meta batch: match by path for direct inputs (the input path itself),
  // fall back to the first input's meta for derived changes (sync, flip, BoolLogic, etc.)
  // This is best-effort for the rare case where a batch has heterogeneous metas.
  const metaByPath = new Map(inputs.map(i => [i.path, i.meta]))
  return outputs.map(c => ({
    ...c,
    meta: metaByPath.get(c.path) ?? inputs[0]?.meta,
  }))
}
```

**Coverage:**
- Single input / uniform-meta batch → perfect attribution (the common case)
- Multi-meta batch, output path matches an input → perfect attribution
- Multi-meta batch, derived change (sync peer, BoolLogic output, etc.) → best-effort (first input's meta)

The degraded case is rare in practice. Batches that warrant different metas per change are unusual; most real batches represent one logical action with one meta.

### What meta enables for listeners

Every change a listener receives carries the meta of the Input that triggered it. Use cases:

- **Request correlation** — `{ requestId }` links a listener's output back to the originating API call
- **Source tracking** — `{ source: 'profile-form' }` tells listeners where the mutation came from
- **Optimistic update IDs** — correlate provisional changes with confirmed server responses
- **Undo/redo** — tag changes with a transaction ID; listeners can group or reverse them

---

## Change Classification Reference

| Scenario | `kind` | `lineage.via` | `lineage.context` |
|----------|--------|---------------|-------------------|
| User sets `user.name = "Alice"` | `Real` | `Input` | `Mutation` |
| New product set for first time | `Real` | `Input` | `Initial` |
| `user = {...}` expanded to `user.name` | `Breakdown` | `Diff` | `Mutation` |
| `total = 150` split to sources | `Consumed` | `AggregationWrite` | — |
| Source `item.price` → `total` recomputed | `Real` | `AggregationRead` | `Mutation` |
| `user.name` synced to `profile.name` — value changed | `Real` | `Sync` | `Mutation` |
| `user.name` synced to `profile.name` — peer already matched | `Redundant` | `Sync` | `Mutation` |
| `user.name` synced to `profile.name` (initial set) | `Real` | `Sync` | `Initial` |
| `checkbox1` flipped to `checkbox2` | `Real` | `Flip` | `Mutation` |
| `checkbox1` flip skipped — initial value | *(not produced)* | — | `Initial` → guard rejects |
| Redundant sync change reaches listener | *(same Redundant change)* | — | Listeners `accepts` includes `Redundant` |
| Redundant sync change skipped by Flip | *(not processed)* | — | Flip `accepts` excludes `Redundant` |
| `user.role` triggers `disabledWhen` | `Real` | `BoolLogic` | `Mutation` |
| Sync attempted but value already matched | `Redundant` | `Sync` | `Mutation` |
| Trigger path fires → `user.token` cleared | `Real` | `ClearPath` | `Mutation` |

---

## What Is Removed

| Removed | Replaced by |
|---------|-------------|
| `buf_output` | `ctx.changes` (accumulator) |
| `buf_sync` | frontier pattern in executor |
| `buf_flip` | frontier pattern in executor |
| `buf_affected_ids` | `ctx.affected_bool_logic` |
| `buf_concern_changes` | `ctx.changes` partitioned by `lineage.via` |
| `buf_affected_validators` | `ctx.affected_validators` |
| `buf_affected_value_logics` | `ctx.affected_value_logics` |
| `origin: Option<String>` | `lineage: Lineage` (always set, typed) |
| `ChangeStep` enum | `Stage` enum (used in `Lineage.via`) |
| Scattered stage gate checks | `StagePolicy.guard` + `StagePolicy.accepts` in `STAGE_POLICIES` |
| `PipelineCoreResult` struct | callers read `self.ctx` directly |
| Duplicated steps 0–11 in `prepare_changes` | `run_pipeline_core` via `execute_policy` |
| `Change` defined in `pipeline.rs` | `change.rs` — correct dependency direction |

---

## Implementation Order

1. **Create `rust/src/change.rs`** — declare `Stage`, `ChangeKind`, `ChangeContext`, `Lineage`, `ChangeAudit`, `SkipReason`, `SkippedChange`, `StageTrace`, `PipelineTrace`, `Change`, `UNDEFINED_SENTINEL_JSON`. Update `lib.rs` module declaration.
2. **Update all `use crate::pipeline::Change` imports** — in `aggregation.rs`, `clear_paths.rs`, `router.rs` → `use crate::change::Change` (and other types from `change.rs`).
3. **Update all `Change { ... }` construction sites** — remove `origin`, add `kind` and `lineage`. Use `grep -n "Change {" rust/src/` to find all sites.
4. **Add `PipelineContext` to `pipeline.rs`** — with `clear()` and `new()`. Include `trace: PipelineTrace` field.
5. **Add `StagePolicy`, `ContextRule`, `STAGE_POLICIES` to `pipeline.rs`**.
6. **Add `execute_policy` method to `ProcessingPipeline`** — returns `Option<StageTrace>`, builds trace when `self.debug == true`.
7. **Update `ProcessingPipeline`** — replace seven `buf_*` fields with `ctx: PipelineContext`, add `debug: bool`.
8. **Refactor `run_pipeline_core`** — delegate to `execute_policy` + `STAGE_POLICIES`. Collect `StageTrace` into `ctx.trace`. Return `bool`. Remove `PipelineCoreResult`.
9. **Remove individual stage gate checks** — `STAGE_POLICIES.guard` now owns all routing decisions. Remove ad-hoc `if change.kind != ...` checks inside `process_sync_paths`, `process_flip_paths`, etc.
10. **Refactor callers** — `process_changes_vec` and `prepare_changes` read from `self.ctx`. Partition by `lineage.via` instead of `step`. Include `ctx.trace` in result types when `debug` is on.
11. **Add JS-side trace types** — `StageTrace`, `PipelineTrace`, `SkippedChange`, `SkipReason`, `FunctionTrace`, `CallTrace` TypeScript types. `StageTrace` / `PipelineTrace` must match Rust serialization exactly.
12. **Add `attachMeta()`** — post-pipeline JS step that stamps `meta` onto all output changes before they reach listeners. Accepts inputs with optional `meta` third argument.
13. **Add `traceFunction()` wrapper** — wraps listener / validator / evaluate() calls with `performance.now()` timing. Collects into `CallTrace.functions`.
14. **Add `logCallTrace()`** — console group renderer showing WASM stages + JS functions with timing. Called automatically when `verbose: true`.
15. **Add Redux DevTools dispatch** — send `CallTrace` action after all JS functions complete for the call, when `debug: true`.
16. **Run checks** — `npm run wasm:fmt && npm run wasm:lint && npm run wasm:check`.
17. **Run tests** — verify all existing tests pass. Update test assertions for new `Change` shape (`kind` + `lineage` instead of `step` + `origin`).

---

## Acceptance Criteria

### Rust / WASM

- [ ] `rust/src/change.rs` exists; `aggregation.rs`, `clear_paths.rs`, `router.rs` import from `crate::change`
- [ ] `Stage` enum declared in `change.rs` covering all pipeline stages
- [ ] `Change.origin` removed; `Change.kind: ChangeKind` and `Change.lineage: Lineage` always set
- [ ] `ChangeContext::Initial` propagates through derived changes via `ContextRule::Inherit`
- [ ] `Change.audit: Option<ChangeAudit>` is `None` when `debug = false`
- [ ] `STAGE_POLICIES` is the sole declaration of pipeline stage order and routing rules
- [ ] `StagePolicy.guard` and `StagePolicy.accepts` are where all stage gate logic lives — no scattered checks in step functions
- [ ] `Flip` accepts only `[Real]` — `Redundant` sync changes never enter Flip
- [ ] `Listeners` accepts `[Real, Redundant]` — listeners see both actual changes and no-op sync attempts
- [ ] Nested `followup` pipelines work: followup frontier is scoped to parent output only
- [ ] Flip stage correctly skips changes with `ChangeContext::Initial` via its `guard`
- [ ] Redundant changes (value matches shadow) are `ChangeKind::Redundant` — not written to valtio but visible to listeners
- [ ] `ProcessingPipeline` has `ctx: PipelineContext` instead of seven `buf_*` fields
- [ ] `ctx: PipelineContext` includes `trace: PipelineTrace`, cleared on each call
- [ ] `execute_policy` returns `Option<StageTrace>` — `Some` when `debug == true`, `None` in production
- [ ] `SkippedChange` entries are captured in `StageTrace.skipped` for every guard rejection
- [ ] `PipelineCoreResult` struct is removed
- [ ] `process_changes_vec` and `prepare_changes` share zero duplicated pipeline logic
- [ ] `PipelineTrace` is included in `ProcessResult` / `PrepareResult` when `debug == true`
- [ ] In production, only `Real` changes returned to JS
- [ ] In debug mode, all change kinds returned with `audit.source_path` resolved
- [ ] All existing Rust tests pass
- [ ] `wasm:lint` passes with zero warnings

### JS / TypeScript

- [ ] `StageTrace`, `PipelineTrace`, `SkippedChange`, `SkipReason` TypeScript types exist and match Rust serialization
- [ ] `StageTrace.duration_us` and `PipelineTrace.total_duration_us` are present and non-zero in debug mode
- [ ] `FunctionTrace` and `CallTrace` types exist
- [ ] `traceFunction()` wrapper measures listener / validator / evaluate() timing without changing their signatures
- [ ] `CallTrace` combines WASM `PipelineTrace` + `FunctionTrace[]` in one structure
- [ ] `logCallTrace()` renders nested `console.group` output: WASM stages with timing, then JS functions
- [ ] No-op stages (zero accepted, zero produced) render as `console.groupCollapsed`
- [ ] Console output shows `[wasm Xms · js Xms]` split in the top-level group label
- [ ] `verbose: true` in store config triggers automatic `logCallTrace()` per pipeline call
- [ ] `debug: true` in store config dispatches `CallTrace` to Redux DevTools after all JS functions complete
- [ ] Redux DevTools action includes `type` (input paths) and full `callTrace`
- [ ] Redux DevTools state snapshot includes `state` and `_concerns`
- [ ] `meta` is NOT present on the Rust `Change` struct — WASM has no knowledge of it
- [ ] `attachMeta()` runs in JS after every pipeline call, before changes reach listeners or DevTools
- [ ] Single-input and uniform-meta batch cases achieve perfect meta attribution
- [ ] Multi-meta batch falls back to best-effort (first input's meta for derived changes)
- [ ] Every `Change` arriving at a listener carries a `meta` field (may be `undefined` if user provided none)
- [ ] `FunctionTrace.meta` reflects the meta of the changes that triggered that function
