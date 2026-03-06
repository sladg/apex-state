# Reactor Architecture Proposal

**Status:** Draft for review
**Date:** 2026-03-05
**Scope:** Rust/WASM pipeline restructuring

---

## Problem Statement

`ProcessingPipeline` is a 8450-line god object with 27 fields. It directly owns 6 registries, 3 reverse indices, 2 graphs, a topic router, and 6 anchor-tracking fields. The `run_pipeline_core` method is 550 lines of interleaved stage logic and trace recording. Adding a new pipeline stage requires modifying the god object directly.

## Rejected Alternative: Embedded-Meta ReactiveNode

An approach embedding behavioral metadata directly into shadow state tree nodes was evaluated and rejected. Critical review identified:

- **Borrow-checker conflict**: Reading node A's meta while writing to node B in the same tree requires `RefCell`, `unsafe`, or copying meta out (negating the benefit).
- **`shadow_init` destroys meta**: Replacing the tree root loses all registrations done before init.
- **Dirty flags incompatible with batch processing**: Push-pull (Leptos-style) doesn't work when multiple changes arrive simultaneously with stage-ordering constraints.
- **Cache performance regression**: Tree traversal O(depth) with pointer chasing vs current O(1) flat HashMap lookups.
- **Transitive sync needs union-find**: Graph component semantics can't be replicated by per-node peer lists without reimplementing BFS reachability.

## Proposed Architecture

### Core Concepts

| Concept | Role | Owns state? | Does work? |
|---------|------|-------------|------------|
| **Stage** | Identity label + ordering key | No | No |
| **Reactor** | Self-contained processing unit | Yes (registrations, graphs, indices) | Yes (processes changes, produces output) |
| **Pipeline** | Orchestrator — runs reactors in order, applies changes to shadow | Yes (shadow, intern, ctx) | Coordination only |

### The Reactor Trait

```rust
/// What a reactor produces when it processes changes
enum ReactorOutput {
    /// New changes (state or concern — no distinction)
    Changes(Vec<Change>),
    /// Execution plan for JS listener dispatch
    ExecutionPlan(FullExecutionPlan),
    /// Validator dispatches for JS execution
    ValidatorDispatches(Vec<ValidatorDispatch>),
    /// No output (inactive or no matches)
    None,
}

/// Context provided to each reactor during processing.
/// Carries both shared state (shadow, intern) and accumulated side-channel data
/// (affected ID sets, anchor state) that earlier reactors populate and later reactors consume.
struct ReactorCtx<'a> {
    /// Post-mutation shadow (the "working" copy)
    shadow: &'a ShadowState,
    /// Pre-mutation shadow (only meaningful for FlipReactor)
    pre_shadow: Option<&'a ShadowState>,
    /// Shared intern table
    intern: &'a mut InternTable,
    /// Whether debug tracing is enabled (reactors skip trace construction when false)
    debug: bool,

    /// Anchor/dormancy system — has mutable state that changes mid-pipeline.
    /// Evaluated at two checkpoints: before sync/flip and after agg/comp.
    /// Reactors call ctx.anchor.is_dormant(path_id) to check dormancy.
    anchor: &'a mut AnchorManager,

    /// Accumulated affected logic IDs — populated by the pipeline loop
    /// via ctx.mark_affected() after each reactor produces changes.
    /// Consumed by LogicReactor instead of &changes.
    /// Lives on PipelineContext (persistent, cleared per call) — ReactorCtx borrows it.
    affected_logic_ids: &'a mut HashSet<u32>,
    /// Accumulated affected validator IDs — consumed by ValidatorReactor.
    /// Lives on PipelineContext (persistent, cleared per call) — ReactorCtx borrows it.
    affected_validator_ids: &'a mut HashSet<u32>,
}

/// What a reactor returns: output + optional trace
struct ReactorResult {
    output: ReactorOutput,
    /// Populated only when ctx.debug is true.
    /// Each reactor computes its own matched/produced/skipped semantics.
    trace: Option<StageTrace>,
}

/// A modular, self-contained pipeline stage
trait Reactor {
    /// Stage identity (for ordering and tracing)
    fn stage(&self) -> Stage;

    /// Does this reactor have any active registrations?
    fn is_active(&self) -> bool;

    /// Process a batch of changes, produce output + optional trace.
    /// Trace is reactor-owned because each stage defines matched/produced/skipped differently.
    fn process(&mut self, changes: &[Change], ctx: &mut ReactorCtx) -> ReactorResult;

    /// Debug snapshot for get_graph_snapshot
    fn snapshot(&self, intern: &InternTable) -> serde_json::Value;

    /// Remove all registrations associated with the given registration_id.
    /// Each reactor handles its own cleanup logic.
    fn unregister(&mut self, registration_id: &str, intern: &mut InternTable);
}
```

Registration is deliberately NOT on the trait. Each reactor has its own registration shape (sync takes pairs, aggregation takes target+sources, listeners take topic+scope+subscriber_id). Registration stays as typed methods on each concrete struct.

### AnyReactor Enum Dispatch

The reactor set is fixed at compile time in WASM — no need for `Box<dyn Reactor>` vtable overhead. Use an enum that wraps each concrete reactor and delegates trait methods:

```rust
enum AnyReactor {
    AnchorEval(AnchorEvalReactor),             // evaluate + snapshot before sync/flip
    Sync(SyncReactor),                          // steps 4-5
    Flip(FlipReactor),                          // steps 6-7
    Derived(DerivedReactor),                    // steps 7.5-7.6
    AnchorTransitions(AnchorTransitionReactor), // compare + re-mark before logic
    Logic(LogicReactor),                        // steps 8-9
    Listener(ListenerReactor),                  // step 11
    Validator(ValidatorReactor),                // step 10
    Clear(ClearReactor),                        // cleanup — wipe stale data after all processing
}

impl AnyReactor {
    fn stage(&self) -> Stage {
        match self {
            Self::Sync(r) => r.stage(),
            Self::Flip(r) => r.stage(),
            Self::Clear(r) => r.stage(),
            Self::Derived(r) => r.stage(),
            Self::Logic(r) => r.stage(),
            Self::Listener(r) => r.stage(),
            Self::Validator(r) => r.stage(),
        }
    }

    fn is_active(&self) -> bool { /* same match delegation */ }
    fn process(&mut self, changes: &[Change], ctx: &mut ReactorCtx) -> ReactorResult { /* ... */ }
    fn snapshot(&self, intern: &InternTable) -> serde_json::Value { /* ... */ }
    fn unregister(&mut self, registration_id: &str, intern: &mut InternTable) { /* ... */ }
}
```

**Why enum over `Box<dyn Reactor>`:**

- Zero vtable overhead — direct dispatch via match
- Fixed reactor set is known at compile time (WASM target)
- Each variant is a concrete type — registration methods accessible via pattern match
- Adding a new reactor = add an enum variant + match arms (compile error guides you)

**Why not named struct fields** (e.g., `pipeline.sync: SyncReactor, pipeline.flip: FlipReactor`):

- Loses the loop — orchestration becomes per-field imperative code
- Adding a reactor requires changing the pipeline struct AND the orchestration function
- The clean `for reactor in &mut self.reactors` pattern disappears

### Concrete Reactors

Listed in execution order (matches current pipeline steps):

```
AnchorEvalReactor (step 3.6)
  State: none (operates on ctx.anchor)
  Reads: ctx.shadow — evaluates which anchor paths exist
  Output: None (stores snapshot in AnchorManager for later comparison)

SyncReactor (steps 4-5)
  State: Graph (union-find) + directed edges HashMap
  Reads: &changes — filters to paths matching registered sync graph nodes
  Output: Changes (copy value to peers)

FlipReactor (steps 6-7)
  State: Graph (union-find)
  Reads: &changes — filters to paths matching registered flip graph nodes
  Uses: ctx.pre_shadow for pre-mutation values
  Output: Changes (peer gets OLD value from pre-mutation shadow)

DerivedReactor (merged aggregation + computation)
  State: PathIndexedRegistry<DerivedEntry>
  Reads: &changes — filters to source paths matching registered targets
  Output: Changes (recomputed target values: Mirror/Sum/Avg)

AnchorTransitionReactor (step 7.7)
  State: none (operates on ctx.anchor)
  Reads: ctx.shadow — re-evaluates anchor states after sync/flip/derived
  Detects: disabled→enabled transitions, re-marks ctx.affected_logic_ids
  Output: None

LogicReactor (unified BoolLogic + ValueLogic + Expressions)
  State: LogicRegistry + ReverseDependencyIndex
  Reads: ctx.affected_logic_ids (NOT &changes — uses pre-computed ID set)
  Uses: ctx.shadow for evaluation
  Output: Changes — two kinds:
    1. Concern data: evaluated results to _concerns.* paths (options lists, computed values)
    2. State corrections: constrained options auto-correct to state.* paths
  Corrections are logged in trace (debug), not as separate concern values.
  See: docs/CONSTRAINT_EXPRESSION_DESIGN_PROMPT.md for expression language spec

ListenerReactor
  State: TopicRouter
  Reads: &changes — all accumulated changes
  Output: ExecutionPlan

ValidatorReactor
  State: FunctionRegistry + ReverseDependencyIndex
  Reads: ctx.affected_validator_ids (NOT &changes — uses pre-computed ID set)
  Output: ValidatorDispatches

ClearReactor (cleanup — runs last)
  State: ClearPathsRegistry (wildcard matching)
  Reads: &changes — filters to genuine path IDs matching trigger paths
  Output: Changes (set stale targets to null — e.g., pricing results after ccyPair change)
  Runs after listeners so reactive processing works on real data, not nulled-out stale data.
```

### Pipeline Orchestration

#### The input problem

Different reactors see different inputs today:

| Reactor | Actually receives |
|---------|-------------------|
| ClearPath | `genuine_path_ids` (post-diff path IDs) |
| Sync | `changes` + `clear_changes` |
| Flip | `changes` + `clear_changes` + `sync_buf` |
| AggRead | `ctx.changes` (all accumulated, incl. sync output) |
| CompRead | `ctx.changes` (all accumulated, incl. agg-read output) |
| BoolLogic | Not changes at all — `ctx.affected_bool_logic` (pre-computed ID set via reverse index) |
| ValueLogic | Not changes at all — `ctx.affected_value_logics` (pre-computed ID set) |
| Listeners | `ctx.changes` (all accumulated) |

A naive `reactor.process(&changes)` where `changes` is a growing accumulator handles most of these — Sync, Flip, AggRead, CompRead, and Listeners all essentially see "all changes accumulated so far". The accumulator naturally grows as each reactor's output is appended.

**BoolLogic/ValueLogic are the exception.** They don't scan changes — they use pre-computed affected ID sets built by `mark_affected_logic()`, which is called by earlier stages (Diff, ClearPath, Sync, Flip, AggRead, CompRead) whenever they produce or apply a change.

#### Solution: ReactorCtx carries the affected-ID accumulator

`ReactorCtx` already holds shared state (shadow, intern). The affected-ID sets and anchor manager are added there (see full `ReactorCtx` definition in the Reactor Trait section above).

Each reactor that produces changes triggers `ctx.mark_affected(path)` (called by the pipeline loop, not by reactors). LogicReactor reads `ctx.affected_logic_ids` instead of `&changes`. This matches the current architecture where `mark_affected_logic` is called inline by Diff, ClearPath, Sync, Flip, AggRead, and CompRead.

#### The orchestration loop

```rust
fn run_pipeline_core(&mut self, input: Vec<Change>) -> Result<bool, String> {
    self.ctx.clear();
    let mut changes = self.diff_changes(input);
    if changes.is_empty() { return Ok(false); }

    // AggregationWrite is special: rewrites changes (not a standard reactor)
    changes = self.agg_write.rewrite(changes, &ctx);

    // Apply to working shadow
    self.apply_to_shadow(&changes)?;

    // Mark initial changes as affecting logic
    for change in &changes {
        ctx.mark_affected(&change.path);
    }

    // Run reactors in order — each feeds into the next
    for reactor in &mut self.reactors {
        if !reactor.is_active() { continue; }

        let result = reactor.process(&changes, &mut ctx);

        if let Some(trace) = result.trace {
            self.ctx.trace.push(trace);
        }

        match result.output {
            ReactorOutput::Changes(new) => {
                for change in &new {
                    self.apply_to_shadow_single(change)?;
                    ctx.mark_affected(&change.path);
                }
                changes.append(&mut new); // move, don't copy
            }
            ReactorOutput::ExecutionPlan(plan) => { self.ctx.execution_plan = Some(plan); }
            ReactorOutput::ValidatorDispatches(v) => { self.ctx.validators_to_run = v; }
            // Rewrite removed — AggWrite is a pre-step
            ReactorOutput::None => {}
        }
    }

    Ok(true)
}
```

**Key details:**

- `changes` is a growing accumulator — each reactor sees everything before it
- `ctx.mark_affected()` replaces the 7 inline `mark_affected_logic()` calls scattered through the current pipeline (lines 2419, 2427, 2446, 2495, 2517, 2585, 2634). The pipeline loop centralizes this: every time a reactor produces `Changes`, the loop applies them to shadow AND marks affected logic. This is explicit in the loop body above — not hidden inside reactors.
- LogicReactor's `process()` ignores `&changes` entirely and reads `ctx.affected_logic_ids`
- ListenerReactor's `process()` reads `&changes` for the full accumulated change set
- ClearReactor reads `&changes` but filters to genuine path IDs (the subset it cares about)
- **Constrained options** (LogicReactor with `constrain`): produces state corrections alongside concern changes in a single `ReactorOutput::Changes`. The corrections flow naturally through the accumulator — ListenerReactor sees them, so listeners on corrected paths fire. State corrections do NOT re-trigger LogicReactor in the same pass (single-pass design). Re-evaluation happens on the next `processChanges` from JS.

#### Anchor/dormancy: two-phase evaluation as reactors

Anchors are evaluated **twice** per pipeline run:

1. **AnchorEvalReactor** (before Sync/Flip) — evaluates which anchor paths exist, stores snapshot
2. **AnchorTransitionReactor** (after Derived, before Logic) — re-evaluates, detects disabled→enabled transitions, re-marks affected logic IDs

Both are explicit entries in the reactor chain — no stage-name checks in the loop. `AnchorManager` lives on `ReactorCtx`, both reactors operate on it:

```rust
impl Reactor for AnchorEvalReactor {
    fn stage(&self) -> Stage { Stage::AnchorEval }
    fn is_active(&self) -> bool { !ctx.anchor.is_empty() }
    fn process(&mut self, _changes: &[Change], ctx: &mut ReactorCtx) -> ReactorResult {
        ctx.anchor.evaluate_and_snapshot(ctx.shadow, ctx.intern);
        ReactorResult { output: ReactorOutput::None, trace: None }
    }
}

impl Reactor for AnchorTransitionReactor {
    fn stage(&self) -> Stage { Stage::AnchorTransitions }
    fn is_active(&self) -> bool { !ctx.anchor.is_empty() }
    fn process(&mut self, _changes: &[Change], ctx: &mut ReactorCtx) -> ReactorResult {
        ctx.anchor.detect_transitions(ctx.shadow, ctx.intern, ctx.affected_logic_ids);
        ReactorResult { output: ReactorOutput::None, trace: None }
    }
}
```

`AnchorManager` owns `anchor_path_ids`, `anchor_states`, `anchored_paths`, `dormant_path_ids`. Reactors call `ctx.anchor.is_dormant(path_id)` to check dormancy. The pipeline loop is completely generic — no special cases.

This is ~40 lines. The current `run_pipeline_core` is 550 lines.

### Trace Recording: Reactor-Owned Traces

A generic `trace_stage` wrapper **cannot** reconstruct per-stage trace semantics. Each stage computes `matched`, `produced`, and `skipped` differently — some before processing, some after, some from reverse-index lookups, some from diff comparisons. This is not derivable from `ReactorOutput` post-hoc.

**Examples of per-stage trace differences:**

| Stage | matched | produced | skipped |
|-------|---------|----------|---------|
| Diff | genuine changes (post-diff) | empty | redundant changes (diff rejects) |
| AggregationWrite | inputs matching registered targets (pre-stage) | rewritten outputs | empty |
| Sync | changes matching sync graph nodes | sync-propagated values | empty |
| BoolLogic | affected logic IDs → dependency paths (reverse index) | evaluated results | dormant/unchanged (skipped) |
| Listeners | all accumulated changes | empty | empty |

**Solution:** Each reactor owns its trace data. The `process()` return type includes an optional `StageTrace` alongside the output:

The `Reactor` trait's `process()` method returns `ReactorResult` (see trait definition above), which pairs `ReactorOutput` with an optional `StageTrace`.

Each reactor computes its own trace internally because only it knows:

- **What it matched** (pre-processing filter against its own registrations/indices)
- **What it produced** (post-processing output changes)
- **What it skipped** (dormant paths, redundant values, filtered inputs)

The pipeline loop becomes:

```rust
for reactor in &mut self.reactors {
    if !reactor.is_active() { continue; }

    let result = reactor.process(&changes, &mut ctx);

    // Collect trace if debug is enabled (reactor returns None when debug=false)
    if let Some(trace) = result.trace {
        self.ctx.trace.push(trace);
    }

    match result.output {
        ReactorOutput::Changes(mut new) => {
            for change in &new {
                self.apply_to_shadow_single(change)?;
                ctx.mark_affected(&change.path);
            }
            changes.append(&mut new); // move, don't copy
        }
        ReactorOutput::ExecutionPlan(plan) => { self.ctx.execution_plan = Some(plan); }
        ReactorOutput::ValidatorDispatches(v) => { self.ctx.validators_to_run = v; }
        ReactorOutput::Rewrite(r) => { changes = r; }
        ReactorOutput::None => {}
    }
}
```

**Inside a concrete reactor** (e.g. `SyncReactor`):

```rust
impl Reactor for SyncReactor {
    fn process(&mut self, changes: &[Change], ctx: &mut ReactorCtx) -> ReactorResult {
        let t0 = if ctx.debug { Some(now_us()) } else { None };

        // Stage-specific "matched": filter inputs against graph nodes
        let changes_for_sync = self.filter_relevant(changes, ctx.intern);
        if changes_for_sync.is_empty() {
            return ReactorResult { output: ReactorOutput::None, trace: None };
        }

        // Do the actual work
        let produced = self.propagate(&changes_for_sync, ctx);

        // Build trace with stage-specific semantics
        let trace = t0.map(|t0| StageTrace {
            stage: Stage::Sync,
            duration_us: elapsed_us(t0),
            matched: Self::collect_matched(&changes_for_sync, ctx.intern),
            produced: Self::collect_produced(&produced, ctx.intern),
            skipped: Vec::new(), // sync doesn't skip
            followup: Vec::new(),
        });

        ReactorResult {
            output: ReactorOutput::Changes(produced),
            trace,
        }
    }
}
```

**What this eliminates:**

- ~200 lines of `if rec.enabled()` blocks interleaved with pipeline logic
- The `TraceRecorder` struct (reactors build `StageTrace` directly)
- All trace logic from `run_pipeline_core` — the pipeline just collects traces from results

**What this preserves:**

- Per-stage matched/produced/skipped semantics — each reactor defines its own
- Zero-cost when debug is disabled — `ctx.debug` short-circuits trace construction
- `StageTrace` struct is unchanged — same shape, same data, same JSON output

### Extensibility: Adding a New Reactor

Adding a new pipeline stage (e.g., a pricing engine) requires:

1. Define a struct implementing `Reactor`
2. Insert it at the desired position in the reactor vec
3. Register it from JS via a new WASM export

No existing pipeline code changes. The new reactor's output flows naturally into subsequent reactors via the shared `changes` vec.

```rust
struct PricingReactor { /* watches, model config */ }

impl Reactor for PricingReactor {
    fn stage(&self) -> Stage { Stage::DerivedRead /* or a new variant if trace distinction needed */ }
    fn is_active(&self) -> bool { !self.watches.is_empty() }
    fn process(&mut self, changes: &[Change], ctx: &mut ReactorCtx) -> ReactorResult {
        let t0 = if ctx.debug { Some(now_us()) } else { None };
        let matched_changes = self.filter_watched(changes, ctx.intern);
        let price_updates = self.run_model(&matched_changes, ctx);
        let trace = t0.map(|t0| StageTrace {
            stage: Stage::DerivedRead /* or a new variant if trace distinction needed */,
            duration_us: elapsed_us(t0),
            matched: Self::collect_matched(&matched_changes, ctx.intern),
            produced: Self::collect_produced(&price_updates, ctx.intern),
            skipped: Vec::new(),
            followup: Vec::new(),
        });
        ReactorResult { output: ReactorOutput::Changes(price_updates), trace }
    }
    fn snapshot(&self, _intern: &InternTable) -> serde_json::Value { ... }
    fn unregister(&mut self, _id: &str, _intern: &mut InternTable) { /* ... */ }
}
```

---

## Prerequisite: Unified State/Concern Changes

### Problem

Currently the pipeline maintains a hard split between state changes and concern changes:

- Separate buffers (`buf_pending_state_changes`, `buf_pending_concern_changes`)
- `_concerns.` prefix manipulation (add, strip, re-add) scattered across 3 methods
- `FinalizeResult` has two separate change lists
- `ReactorOutput` would need a `ConcernChanges` variant

### Proposal

Treat `_concerns.*` as regular paths in the shadow state tree. Shadow holds both:

```
user.email: "alice"
user.role: "admin"
_concerns.user.email.disabledWhen: false
_concerns.user.email.validationState: "valid"
```

BoolLogic/ValueLogic reactors produce changes to `_concerns.*` paths — same type, same pipeline, same diffing. JS splits by prefix when applying:

```typescript
for (const change of result.changes) {
    if (change.path.startsWith('_concerns.')) {
        applyToConcernsProxy(change.path.slice('_concerns.'.length), change.value_json);
    } else {
        applyToStateProxy(change.path, change.value_json);
    }
}
```

### What This Eliminates

Note: `FinalizeResult` already has a single `state_changes` field (merges state + concerns at line 3074-3076). The split lives in the **buffers** and the **prefix manipulation logic** in `prepare_changes` and `pipeline_finalize`.

| Area | Removed |
|------|---------|
| `run_pipeline_core` | ~30 lines: separate `concern_changes` buffer + prefix loop (lines 2795-2810) |
| `prepare_changes` | ~10 lines: partition `ctx.changes` by `_concerns.` prefix into two buffers (lines 2960-2968) |
| `pipeline_finalize` | ~55 lines: concern prefix add/strip, separate shadow updates, separate diffing, merge (lines 3028-3076) |
| `ProcessingPipeline` | 1 field: `buf_pending_concern_changes` (line 488) |
| `ReactorOutput` | 1 variant: no need for separate `ConcernChanges` |
| **Total** | **~95 lines of Rust**, one fewer concept system-wide |

### Cascade Benefit

With unified changes, concern changes can trigger other reactors:

- BoolLogic sets `_concerns.user.email.disabledWhen` to `true`
- A listener watching `_concerns.user.email` fires
- A sync pair could mirror concern values across forms

Everything is just a change flowing through the pipeline. Whether it lands in `state` or `_concerns` is a path convention, not an architectural split.

---

## Additional Improvements (Independent of Reactor Refactor)

### BoolLogic is ValueLogic — Unify at the Boundary

BoolLogic is syntactic sugar for ValueLogic. A BoolLogic expression:

```typescript
disabledWhen: { boolLogic: { IS_EQUAL: ['user.role', 'admin'] } }
```

is equivalent to:

```typescript
disabledWhen: { logic: { IF: { IS_EQUAL: ['user.role', 'admin'] }, THEN: true, ELSE: false } }
```

**TypeScript transforms at registration time** — WASM only sees `LogicNode` entries (renamed from `ValueLogicNode`). No separate `BoolLogicRegistry` exists.

#### What changes

| Layer | Before | After |
|-------|--------|-------|
| **TS registration** | `boolLogic` → `BoolLogicRegistry`, `value_logic` → `ValueLogicRegistry` | Both → `logic` key, TS wraps `boolLogic` in `{ IF: expr, THEN: true, ELSE: false }` |
| **WASM registry** | `BoolLogicRegistry` + `ValueLogicRegistry` (2 registries, 2 rev indices) | `LogicRegistry` (1 registry, 1 rev index) |
| **WASM evaluation** | `BoolLogicNode::evaluate() → bool` + `ValueLogicNode::evaluate() → Value` | `LogicNode::evaluate() → Value` (bool concerns return `Value::Bool`) |
| **Condition language** | `BoolLogicNode` (IS_EQUAL, AND, OR, NOT, etc.) | Unchanged — `BoolLogicNode` stays as the condition type inside `IF` clauses |
| **Public TS types** | `BoolLogic<STATE>`, `ValueLogic<STATE, T>` | Keep both as user-facing types. TS transforms `BoolLogic` → `Logic` at the boundary |

#### Rust side

```rust
// Renamed from ValueLogicNode — this is now the only registered/evaluated type.
// THEN/ELSE/CASES accept Expr (can be static Value OR live computation).
// See docs/CONSTRAINT_EXPRESSION_DESIGN_PROMPT.md for full Expr spec.
#[serde(untagged)]
enum LogicNode {
    IfThenElse { IF: BoolLogicNode, THEN: Expr, ELSE: Box<LogicElse> },
    Match { MATCH: String, CASES: HashMap<String, Expr>, DEFAULT: Expr },
    Compute(Expr),  // direct expression — no condition, just evaluate
}

// BoolLogicNode stays as the CONDITION language (used inside IF clauses)
// It is NOT registered separately — only used as a sub-expression inside LogicNode

// Constraint config for options concerns (co-located with options logic)
enum ConstrainConfig {
    Simple,           // deep equality match against list items
    ByKey(String),    // match current value against option[key] field
}

struct LogicMetadata {
    output_path: String,
    tree: LogicNode,
    anchor_path_id: Option<u32>,
    registration_id: Option<String>,
    /// If set, this is a constrained options concern:
    /// - Evaluates logic → options list → writes to _concerns.*.options
    /// - Checks state value against options → corrects if invalid
    /// - Writes correction metadata to _concerns.*.constrained
    constrain: Option<ConstrainConfig>,
}

struct LogicRegistry {
    entries: HashMap<u32, LogicMetadata>,
    rev_index: ReverseDependencyIndex, // single unified index
    next_id: u32,
}
```

#### TypeScript transformation (at registration boundary)

```typescript
// In registration.wasm.ts — transform boolLogic to logic before sending to WASM
if ('boolLogic' in config) {
  logics.push({
    output_path: `${path}.${concernName}`,
    tree_json: JSON.stringify({
      IF: config.boolLogic,
      THEN: true,
      ELSE: false,
    }),
  })
} else if ('logic' in config) {
  logics.push({
    output_path: `${path}.${concernName}`,
    tree_json: JSON.stringify(config.logic),
  })
}
```

#### What this eliminates

- `BoolLogicRegistry` entirely (~150 lines)
- `BoolLogicMetadata` struct
- `value_logic_registry` + `value_logic_rev_index` fields on pipeline (merged into one)
- `rev_index` for BoolLogic (merged)
- `affected_bool_logic` + `affected_value_logics` sets → single `affected_logic_ids`
- 2 evaluation loops in pipeline → 1
- 2 sets of `ids_for_anchor()` calls in anchor re-evaluation → 1
- ~400 lines of duplicate registry + pipeline code total

#### What survives

- `BoolLogicNode` enum — condition evaluator (`evaluate() → bool`), used inside `LogicNode::IfThenElse.IF`
- `BoolLogic<STATE>` TypeScript type — public API unchanged, users keep writing `{ boolLogic: ... }`
- `ValueLogic<STATE, T>` TypeScript type — renamed to `Logic<STATE, T>` in the public API (or kept as alias)
- All existing tests — BoolLogic tests become LogicNode tests with `{ IF: ..., THEN: true, ELSE: false }` wrapper

#### Expression language extension

`LogicNode` value positions (`THEN`, `ELSE`, `CASES`, `DEFAULT`) accept `Expr` instead of static `Value`. `Expr` can be a literal, a live path read (`{ PATH: "..." }`), or a composable function (GET, COALESCE, arithmetic, etc.). This enables reactive computations (currency conversion, table lookups, clamping) alongside simple condition-based selection.

Options concerns can declare `constrain` to auto-correct state values when the options list changes. The constraint evaluation is co-located with the options logic — no cross-concern references needed.

Full spec: `docs/CONSTRAINT_EXPRESSION_DESIGN_PROMPT.md`

#### Naming

| Old | New |
|-----|-----|
| `ValueLogicNode` | `LogicNode` |
| `ValueLogicRegistry` | `LogicRegistry` |
| `ValueLogicMetadata` | `LogicMetadata` |
| `BoolLogicNode` | `BoolLogicNode` (unchanged — it's a condition, not a logic entry) |
| `value_logic` (TS config key) | `logic` |
| `boolLogic` (TS config key) | Kept for backward compat, TS transforms to `logic` |
| `ValueLogic<STATE, T>` (TS type) | `Logic<STATE, T>` |
| `BoolLogic<STATE>` (TS type) | Kept as-is (it's a condition type, used in `Logic`'s `IF` clause) |

### Merge Aggregation + Computation

Unified `DerivedRegistry` with pluggable operators:

```rust
enum DerivedOp {
    Mirror,   // all-equal check (was: Aggregation)
    Sum,      // (was: Computation::Sum)
    Avg,      // (was: Computation::Avg)
}
```

Adding new operators (Min, Max, Count) = add an enum variant + a compute function.

### Extract AnchorManager

Move 6 anchor-related fields from `ProcessingPipeline` into a single struct:

```rust
struct AnchorManager {
    anchor_path_ids: HashSet<u32>,
    anchor_states: HashMap<u32, bool>,
    anchor_registrations: HashMap<u32, HashSet<String>>,
    anchored_paths: HashMap<u32, u32>,
    registration_anchor_map: HashMap<String, u32>,
    dormant_path_ids: HashSet<u32>,
}
```

### Shadow State: Intern-Keyed Access

Current `shadow.set()` re-parses the dot-separated path string on every call (`path.split('.').collect()` → recursive tree walk). This is O(depth) per call with string allocation. When SyncReactor propagates to 20 peers, that's 20 segment-split traversals.

Since we're redesigning the pipeline, shadow should accept intern IDs instead of strings:

```rust
impl ShadowState {
    /// Set a value using a pre-interned path ID chain.
    /// Avoids per-call string splitting — segments are already resolved.
    fn set_by_ids(
        &mut self,
        segment_ids: &[u32],
        value: ValueRepr,
    ) -> Result<(), String> {
        Self::set_at_by_ids(&mut self.root, segment_ids, value)
    }
}
```

**Why this matters for the reactor model:**

- The pipeline loop calls `apply_to_shadow_single` + `mark_affected` per produced change
- `mark_affected` already uses `affected_path_ids` (intern-based) — shadow access is the remaining string-based bottleneck
- Reactors already have `ctx.intern` — they can pre-intern paths during `process()` and pass ID chains
- `Change` could carry an optional `path_ids: Option<Vec<u32>>` for pre-interned paths, avoiding re-intern at the pipeline level

**Implementation:** This is independent of the reactor refactor but benefits from it. The reactor model centralizes all shadow writes in one loop body, making it easy to switch from `set(path_str)` to `set_by_ids(ids)` in one place.

### File Splitting

Split `pipeline.rs` (8450 lines) into:

- `pipeline/mod.rs` — ProcessingPipeline struct, run_pipeline_core, prepare_changes, pipeline_finalize
- `pipeline/registration.rs` — register_side_effects, register_concerns, unregister_*
- `pipeline/sync.rs` — SyncReactor (or process_sync_paths_into until reactor refactor)
- `pipeline/flip.rs` — FlipReactor
- `pipeline/snapshot.rs` — get_graph_snapshot, debug helpers

---

## ProcessingPipeline: Before and After

### Before (27 fields)

```rust
struct ProcessingPipeline {
    shadow, intern,
    registry, rev_index,                          // BoolLogic
    function_registry, function_rev_index,         // Validators
    aggregations, computations,                    // Derived values
    clear_registry,                                // Clear paths
    sync_graph, directed_sync_edges, flip_graph,   // Graphs
    router,                                        // Listeners
    value_logic_registry, value_logic_rev_index,   // ValueLogic
    ctx,                                           // Per-call scratch
    buf_pending_state_changes,                     // Cross-call buffer
    buf_pending_concern_changes,                   // Cross-call buffer (removed)
    debug_enabled,                                 // Flag
    anchor_path_ids, anchor_states,                // Anchor tracking
    anchor_registrations, anchored_paths,          // (6 fields -> AnchorManager)
    registration_anchor_map, dormant_path_ids,
    registration_map,                              // Cleanup tracking
}
```

### After (~12 fields)

```rust
struct ProcessingPipeline {
    shadow: ShadowState,
    intern: InternTable,

    // Pre-step (not a reactor — transforms input before shadow apply)
    agg_write: AggWriteStep,
    // Reactor chain (ordered)
    reactors: Vec<AnyReactor>,

    // Rev indices — pipeline-level for cross-reactor mark_affected
    logic_rev_index: ReverseDependencyIndex,
    validator_rev_index: ReverseDependencyIndex,

    // Per-call scratch (persistent, cleared per call — avoids per-call allocation)
    ctx: PipelineContext, // includes affected_logic_ids, affected_validator_ids, changes, trace
    buf_pending_changes: Vec<Change>,

    // Operational
    debug_enabled: bool,
    anchor: AnchorManager,
}
```

---

## Stage Enum

```rust
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Stage {
    // Pipeline bookkeeping (not reactors)
    Input,
    AggregationWrite,
    Diff,

    // Reactor stages (executed in this order)
    AnchorEval,        // evaluate + snapshot before sync/flip
    Sync,              // steps 4-5
    Flip,              // steps 6-7
    DerivedRead,       // merged agg-read (7.5) + computation (7.6)
    AnchorTransitions, // compare + re-mark before logic
    Logic,             // merged bool + value logic
    Listeners,
    Validators,
    ClearPath,         // cleanup — wipe stale data after all processing

    // Pipeline bookkeeping
    Apply,
}
```

**No `Custom` variant.** Stage is an identity label for tracing and anchor checkpoints, not an ordering mechanism. Reactor execution order is determined by position in `Vec<AnyReactor>` — inserting a custom reactor between Sync and Flip means placing it at index 1 in the vec. No enum variant needed.

Custom reactors use `Stage::Custom("pricing")` or similar only if we need trace labels. But ordering is always positional:

```rust
// Insert pricing reactor between Derived and Logic (index 4)
pipeline.reactors.insert(4, AnyReactor::Pricing(PricingReactor::new()));
```

---

## Implementation Order

| Step | Change | Risk | Dependencies |
|------|--------|------|--------------|
| 1 | Unify state/concern changes | Low | None — can be done on current architecture |
| 2 | Move trace recording into reactor `process()` return | Low | Step 7 (happens naturally during reactor conversion) |
| 3 | Unify BoolLogic + ValueLogic — TS wraps boolLogic as LogicNode, delete BoolLogicRegistry | Medium | None |
| 4 | Merge Aggregation + Computation into DerivedRegistry | Medium | None |
| 5 | Extract AnchorManager | Low | None |
| 6 | File split pipeline.rs | Low | None |
| 7 | Define Reactor trait + ReactorCtx + ReactorOutput | Low | Steps 1-2 make this cleaner |
| 8 | Convert each stage to a Reactor impl | Medium | Step 7 |
| 9 | Rewrite run_pipeline_core as reactor loop | Medium | Step 8 |
| 10 | Shadow intern-keyed access (`set_by_ids`) | Medium | Step 9 (reactor loop centralizes shadow writes) |

Steps 1-6 are independent and can be done in any order. Steps 7-9 are sequential and benefit from 1-6 being done first. Step 10 is an optimization pass that benefits from the reactor loop centralizing all shadow writes in one place.

---

## Testing Strategy

Each reactor is a self-contained struct with no dependency on `ProcessingPipeline`. This enables proper unit testing at the reactor level — a significant improvement over the current architecture where testing sync behavior requires constructing the entire 27-field pipeline.

### Reactor-Level Unit Tests

Each reactor can be tested in isolation with a minimal `ReactorCtx`:

```rust
#[test]
fn sync_exact_match_propagates() {
    let mut shadow = ShadowState::new();
    let mut intern = InternTable::new();
    shadow.init(r#"{"a": 1, "b": 0}"#, &mut intern).unwrap();

    let mut reactor = SyncReactor::new();
    reactor.register_pair("a", "b", &mut intern);

    let changes = vec![Change::new("a", "42")];
    let ctx = ReactorCtx {
        shadow: &shadow,
        pre_shadow: None,
        intern: &mut intern,
        debug: false,
        anchor: &mut AnchorManager::new(),
        affected_logic_ids: HashSet::new(),
        affected_validator_ids: HashSet::new(),
    };

    let result = reactor.process(&changes, &mut ctx);
    match result.output {
        ReactorOutput::Changes(produced) => {
            assert_eq!(produced.len(), 1);
            assert_eq!(produced[0].path, "b");
            assert_eq!(produced[0].value_json, "42");
        }
        _ => panic!("Expected Changes"),
    }
    // Trace is None when ctx.debug = false
    assert!(result.trace.is_none());
}
```

No pipeline construction. No shadow_init with full state. No registration JSON. Just the reactor, a shadow, and an intern table.

### Integration Tests

The existing 180 pipeline tests (~5350 lines) are mostly behavioral: register → processChanges → assert output. These survive the refactor because the WASM API (`process_changes`, `pipeline_finalize`, `register_side_effects`) stays the same. The internal routing changes but the contract doesn't.

Tests that access internal state (`shadow_get`, `shadow_dump`, `intern_count`) continue working since those are on `ProcessingPipeline`, not on individual reactors.

### Test Migration Path

| Test category | Count (est.) | Migration |
|---------------|-------------|-----------|
| Pipeline input→output (behavioral) | ~120 | No change — same API |
| Shadow state assertions | ~20 | No change — shadow stays on pipeline |
| Individual registry tests (aggregation.rs, bool_logic.rs, etc.) | ~40 | Move to reactor-level tests |
| Graph tests (graphs.rs) | ~10 | Stay with SyncReactor/FlipReactor |
| Clear paths tests (clear_paths.rs) | ~30 | Move to ClearReactor tests |

---

## Registration Cleanup / Unregister Flow

### Current Design

`unregister_side_effects` (95 lines) retrieves `RegistrationCleanupData` from `registration_map` and calls individual unregister methods on each registry:

```rust
// Current: pipeline reaches into each registry directly
fn unregister_side_effects(&mut self, registration_id: &str) {
    let cleanup = self.registration_map.remove(registration_id);
    if let Some(data) = cleanup {
        for pair in &data.sync_pairs { self.sync_graph.remove_edge(...) }
        for pair in &data.flip_pairs { self.flip_graph.remove_edge(...) }
        for target in &data.aggregation_targets { self.aggregations.unregister(target) }
        // ... etc for each registry
    }
}
```

### Proposed Design

Each reactor exposes an `unregister` method. The pipeline dispatches cleanup by reactor type using the cleanup data:

The `unregister` method is part of the `Reactor` trait (see trait definition above):

The pipeline's `unregister_side_effects` becomes a dispatcher:

```rust
fn unregister_side_effects(&mut self, registration_id: &str) -> Result<(), String> {
    // Each reactor cleans up its own state
    self.agg_write.unregister(registration_id, &mut self.intern);
    for reactor in &mut self.reactors {
        reactor.unregister(registration_id, &mut self.intern);
    }
    self.anchor.unregister(registration_id);
    self.registration_map.remove(registration_id);
    Ok(())
}
```

### What each reactor stores for cleanup

Each reactor must track which registrations it holds. Two approaches:

**Option A: Reactor stores registration_id → data mapping internally**

```rust
struct SyncReactor {
    graph: Graph,
    directed_edges: HashMap<u32, HashSet<u32>>,
    // Cleanup: which pairs belong to which registration
    registration_pairs: HashMap<String, Vec<[u32; 2]>>,
    registration_directed: HashMap<String, Vec<[u32; 2]>>,
}

impl Reactor for SyncReactor {
    fn unregister(&mut self, registration_id: &str, intern: &mut InternTable) {
        if let Some(pairs) = self.registration_pairs.remove(registration_id) {
            for [a, b] in pairs {
                self.graph.remove_edge_public(a, b);
            }
        }
        // ... directed edges
    }
}
```

**Option B: Pipeline still stores `RegistrationCleanupData`, passes relevant slices to reactors**

```rust
fn unregister_side_effects(&mut self, registration_id: &str) -> Result<(), String> {
    if let Some(data) = self.registration_map.remove(registration_id) {
        self.sync_reactor().unregister_pairs(&data.sync_pairs, &mut self.intern);
        self.flip_reactor().unregister_pairs(&data.flip_pairs, &mut self.intern);
        self.derived_reactor().unregister_targets(&data.derived_targets);
        // ...
    }
    Ok(())
}
```

**Recommendation:** Option A — each reactor owns its cleanup data. This keeps each reactor fully self-contained and eliminates `RegistrationCleanupData` from the pipeline entirely. The pipeline just broadcasts "unregister this ID" and each reactor handles it.

With Option A, the `registration_map` field on `ProcessingPipeline` is eliminated. Each reactor is responsible for its own registration tracking.

---

## Listener Round-Trip: Two-Phase Protocol

### How it works today

The listener flow is a **two-phase protocol** between JS and WASM. There is no re-entry into the reactor loop.

```
Phase 1: prepare_changes (WASM)
  JS calls: wasm.process_changes(pipeline_id, changes)
  WASM runs: run_pipeline_core → produces state changes + concern changes
  WASM returns: {
    listener_changes: [...],        // state changes for listener scope filtering
    validators_to_run: [...],       // validator dispatches with dependency values
    execution_plan: { groups, propagation_map, cascade_map },
    has_work: bool,
  }

  -- Changes are BUFFERED in buf_pending_state_changes / buf_pending_concern_changes --
  -- Shadow state is already updated --

JS executes listeners + validators:
  For each group in execution_plan:
    For each dispatch in group:
      Call listener handler(scope_changes)
      Collect produced changes from handler
  For each validator:
    Call Zod schema / custom validator
    Collect validation results

Phase 2: pipeline_finalize (WASM)
  JS calls: wasm.pipeline_finalize(pipeline_id, js_produced_changes)
  WASM: merges JS-produced changes with buffered phase-1 results
  WASM: diffs JS changes against shadow, applies, returns final list
  WASM returns: {
    changes: [...],   // all changes (state + concern) ready for valtio
    trace: {...},     // debug trace if enabled
  }

JS applies:
  For each change in result.changes:
    Apply to state proxy or _concerns proxy
```

### How the reactor model handles this

The two-phase protocol is **pipeline-level orchestration**, not reactor-level. The reactor loop runs entirely within phase 1. Phase 2 (`pipeline_finalize`) is a post-processing step that merges external (JS-produced) changes — no reactors run in phase 2.

```rust
/// Phase 1: run reactors, buffer results, return plan for JS
fn prepare_changes(&mut self, input: Vec<Change>) -> Result<PrepareResult, String> {
    self.buf_pending_changes.clear();

    if !self.run_pipeline_core(input)? {
        return Ok(PrepareResult { has_work: false, .. });
    }

    // Extract execution plan and validators from ctx (produced by reactors)
    let execution_plan = self.ctx.execution_plan.take();
    let validators_to_run = std::mem::take(&mut self.ctx.validators_to_run);

    // Buffer ALL changes (state + concern, unified) for phase 2
    self.buf_pending_changes = std::mem::take(&mut self.ctx.changes);
    let listener_changes = self.buf_pending_changes
        .iter()
        .filter(|c| !c.path.starts_with("_concerns."))
        .cloned()
        .collect();

    Ok(PrepareResult {
        listener_changes,
        validators_to_run,
        execution_plan,
        has_work: true,
    })
}

/// Phase 2: merge JS-produced changes with buffered phase-1 results
fn pipeline_finalize(&mut self, js_changes: Vec<Change>) -> Result<FinalizeResult, String> {
    // Phase-1 changes already applied to shadow — pass through
    let mut all_changes = std::mem::take(&mut self.buf_pending_changes);

    // Diff JS-produced changes against current shadow, apply genuine ones
    let diffed = self.diff_changes(&js_changes);
    for change in &diffed {
        self.shadow.set(&change.path, &change.value_json, &mut self.intern)?;
    }
    all_changes.extend(diffed);

    Ok(FinalizeResult {
        changes: all_changes,
        trace: if self.debug_enabled { Some(std::mem::take(&mut self.ctx.trace)) } else { None },
    })
}
```

Key points:

- **Reactors only run in phase 1** (`run_pipeline_core`). They are never re-entered.
- **Phase 2 is simple**: diff + merge + apply. No reactor involvement.
- **JS listener handlers** execute between phases, outside WASM entirely.
- **Listener-produced changes** come back as raw changes in phase 2 — they don't re-trigger the reactor loop. This is intentional: listener handlers are JS-side effects, and their output is treated as "new input" that gets diffed and applied, not re-processed through sync/flip/logic.

### If listener-produced changes SHOULD trigger reactors

The current architecture deliberately does NOT re-process listener-produced changes through the pipeline. If a future requirement needs this (e.g., a listener sets a value that should trigger sync propagation), it would require:

1. Phase 2 runs `run_pipeline_core` on JS-produced changes (instead of just diffing)
2. This creates a potential for infinite loops (listener produces change → triggers listener → produces change → ...)
3. The current design avoids this by design: listeners are terminal — their output is applied directly

This is a deliberate architectural decision, not an oversight. The reactor model preserves it.

---

## LogicReactor: Design Decisions

### No result caching

The reverse dependency index already provides selectivity — only logic IDs whose dependency
paths appear in the current change set are evaluated. For a market data tick affecting all
50 products, all logic IDs are re-evaluated regardless; result caching would add hash
overhead without reducing evaluation count in that case. For single-field user edits, the
reverse index already skips the unaffected entries.

BoolLogic trees are cheap to evaluate for simple expressions (`IS_LESS_THAN`, `IS_EQUAL`,
`EXISTS`). The diff step that follows already suppresses no-op concern changes. Profile
first; add result caching only if BoolLogic evaluation shows up as a meaningful share of
pipeline time at actual production scale.

### Pre-compiled path segment IDs (post-registration transform)

`BoolLogicNode` currently stores string paths (e.g., `IsEqual("user.profile.spot", v)`).
`get_path_value` splits the path on `.` and calls `intern.get_id(seg)` — a string HashMap
lookup — for every segment on every evaluation. For `"user.profile.spot"` that's 3 string
HashMap lookups per eval, per logic entry, per tick.

**Proposed transform at registration time**: after deserializing `BoolLogicNode`, walk the
tree and replace each `String` path with a pre-resolved `Vec<u32>` of segment IDs:

```rust
// Current (stored at registration, looked up at every eval)
BoolLogicNode::IsEqual(String, Value)

// After compile_tree(node, intern):
CompiledBoolLogicNode::IsEqual(Vec<u32>, Value)  // segment IDs resolved once
```

Evaluation then becomes direct `map.get(&seg_id)` array walks — no string splitting, no
intern lookups. The `intern: &InternTable` parameter can be dropped from `evaluate_value`
entirely. The compiled tree carries all the information it needs.

The transform happens inside `LogicRegistry::register()` immediately after deserialization.
No API change, no behavior change — a cheaper internal representation. Same approach applies
to `ValueLogicNode` which has the same string-path storage pattern.

See **WASM-049** in `tasks/WASM-EP12-PERF-MANY-PIPELINES.md`.

---

## Resolved: Critical Integration Issues

### 1. Borrow-checker: `mark_affected` needs rev indices inside reactors

**Problem:** The pipeline loop iterates `&mut self.reactors`. After each reactor produces changes, `mark_affected` must query reverse dependency indices to populate `affected_logic_ids`. But those rev indices live inside `LogicReactor` (and `ValidatorReactor`), which are inside `self.reactors` — already mutably borrowed by the loop.

**Solution:** Reverse dependency indices stay on the pipeline, not inside reactors. This is a practical compromise on self-containment.

```rust
struct ProcessingPipeline {
    shadow: ShadowState,
    intern: InternTable,

    agg_write: AggWriteStep,          // pre-step (not a reactor)
    reactors: Vec<AnyReactor>,

    // Rev indices are pipeline-level — needed for cross-reactor mark_affected
    logic_rev_index: ReverseDependencyIndex,
    validator_rev_index: ReverseDependencyIndex,

    ctx: PipelineContext,
    // ...
}
```

The loop creates a block-scoped `ReactorCtx` per reactor call. After `process()` returns, the ctx is dropped and borrows are released. The pipeline then accesses `self.intern`, `self.shadow`, and the rev indices directly:

```rust
for i in 0..self.reactors.len() {
    let result = {
        let ctx = ReactorCtx {
            shadow: &working,
            intern: &mut self.intern,
            anchor: &mut self.anchor,
            affected_logic_ids: &mut self.ctx.affected_logic_ids,
            affected_validator_ids: &mut self.ctx.affected_validator_ids,
            debug: self.debug_enabled,
            pre_shadow: pre_shadow.as_ref(),
        };
        self.reactors[i].process(&changes, &mut ctx)
    }; // ctx dropped — all borrows released

    if let Some(trace) = result.trace {
        self.ctx.trace.push(trace);
    }

    match result.output {
        ReactorOutput::Changes(mut new) => {
            for change in &new {
                working.set(&change.path, &change.value_json, &mut self.intern)?;
                // mark_affected uses pipeline-level rev indices (not inside reactors)
                let affected_ids = working.affected_path_ids(&change.path, &mut self.intern);
                for pid in &affected_ids {
                    for lid in self.logic_rev_index.affected_by_path(*pid) {
                        self.ctx.affected_logic_ids.insert(lid);
                    }
                    for vid in self.validator_rev_index.affected_by_path(*pid) {
                        self.ctx.affected_validator_ids.insert(vid);
                    }
                }
            }
            changes.append(&mut new);
        }
        ReactorOutput::ExecutionPlan(plan) => { self.ctx.execution_plan = Some(plan); }
        ReactorOutput::ValidatorDispatches(v) => { self.ctx.validators_to_run = v; }
        ReactorOutput::Rewrite(r) => { changes = r; }
        ReactorOutput::None => {}
    }

    // No anchor checkpoints here — they're AnchorEval/AnchorTransitions reactors in the chain
}
```

**What reactors own vs pipeline owns:**

| Component | Lives on | Why |
|-----------|----------|-----|
| Registry entries (logics map, sync graph, etc.) | Reactor | Self-contained processing |
| Reverse dependency indices | Pipeline | Needed by `mark_affected` between reactor calls |
| Affected ID sets | PipelineContext (borrowed by ReactorCtx) | Populated between reactors, consumed by LogicReactor |
| Shadow, intern | Pipeline | Shared across all reactors, mutated between calls |
| Anchor manager | Pipeline | Evaluated at checkpoints between reactors |

### 2. AggregationWrite: pre-step, not a reactor

**Decision:** AggregationWrite stays as a **pre-step**, not a reactor.

**Why it can't be a reactor:** AggWrite runs BEFORE shadow apply. It transforms input changes (distributes target writes to source writes). The rewritten changes are what get applied to shadow. All subsequent reactors see the rewritten changes.

If AggWrite were a reactor inside the loop, the loop would need special handling: "if first reactor produces Rewrite, apply those to shadow instead of the originals." This adds complexity without benefit — AggWrite is architecturally different from reactors (it transforms input, not processes changes).

```rust
fn run_pipeline_core(&mut self, input: Vec<Change>) -> Result<bool, String> {
    self.ctx.clear();
    let mut changes = self.diff_changes(input);
    if changes.is_empty() { return Ok(false); }

    // Pre-step: AggWrite rewrites input changes (not a reactor)
    changes = self.agg_write.rewrite(changes, &working, &self.intern);

    // Apply rewritten changes to working shadow
    for change in &changes {
        working.set(&change.path, &change.value_json, &mut self.intern)?;
        // mark_affected for initial changes
        // ...
    }

    // Reactor loop (all reactors see rewritten changes)
    for i in 0..self.reactors.len() {
        // ...
    }

    Ok(true)
}
```

**AggWrite's output DOES trigger other reactors** — it replaces the input changes, and those replacements flow through the entire reactor chain. There's no "silently fails" problem because it runs before the loop, not inside it.

### 3. Listener-produced changes: terminal by design

**Decision:** Listener-produced changes do NOT re-trigger reactors. This is deliberate, not a limitation.

**Current behavior (preserved):**
1. Phase 1: `run_pipeline_core` → reactors run → produces execution plan
2. JS executes listeners → listeners produce changes
3. Phase 2: `pipeline_finalize` → diffs listener changes against shadow → applies directly → returns

Listener-produced changes skip all reactors (sync, flip, logic, etc.). If a listener sets `state.a = 5` and there's a sync pair `a ↔ b`, the sync won't fire.

**Why this is correct:**
- **Prevents infinite loops**: listener produces change → triggers reactor → reactor produces change → triggers listener → ...
- **Predictable**: listeners are terminal — their output is the final word
- **JS controls it**: if a listener needs to trigger sync, it can explicitly set both `a` and `b`

**If this needs to change in the future** (listener changes triggering reactors):
1. Phase 2 runs `run_pipeline_core` on listener-produced changes instead of just diffing
2. Add cycle detection: max N re-entries (e.g., 3), error if exceeded
3. Each re-entry produces a new execution plan — listeners from the re-entry are NOT executed (only the original listeners fire)

This is a future extension, not part of the current reactor refactor. Document the limitation explicitly and move on.

**Enforcement option:** If we want to catch mistakes, WASM can warn (not error) when `pipeline_finalize` receives a change to a path that has sync/flip/logic registrations. The warning goes in the trace log, helping developers spot cases where they should be setting both sides explicitly.

---

## Resolved: Major Warnings

### 1. ReactorOutput::Rewrite — removed

With AggregationWrite as a pre-step (not a reactor), no reactor produces `Rewrite`. The variant is dead code — remove it from the enum.

```rust
enum ReactorOutput {
    Changes(Vec<Change>),
    ExecutionPlan(FullExecutionPlan),
    ValidatorDispatches(Vec<ValidatorDispatch>),
    None,
    // Rewrite removed — AggWrite is a pre-step, not a reactor
}
```

The "lossy accumulator replacement" concern is eliminated — no reactor ever replaces the accumulator.

### 2. FlipReactor pre-shadow — conditional clone with lifecycle

**Current behavior:** The pipeline clones shadow BEFORE applying changes, only when flip pairs are registered. FlipReactor reads old (pre-mutation) values from `self.shadow` while working mutations go to the `working` clone.

**In the reactor model:** The pipeline creates `pre_shadow` conditionally and passes it to FlipReactor via `ReactorCtx`:

```rust
fn run_pipeline_core(&mut self, input: Vec<Change>) -> Result<bool, String> {
    // ...
    let uses_flip = self.reactors.iter().any(|r|
        matches!(r, AnyReactor::Flip(_)) && r.is_active()
    );

    // Clone only when flip pairs exist (O(N) clone vs O(1) mem::take)
    let mut working = if uses_flip {
        self.shadow.clone()
    } else {
        std::mem::take(&mut self.shadow)
    };

    // pre_shadow = original self.shadow (only meaningful when uses_flip)
    // FlipReactor reads old values from here
    let pre_shadow = if uses_flip { Some(&self.shadow) } else { None };

    // Apply changes to working shadow
    for change in &changes {
        working.set(&change.path, &change.value_json, &mut self.intern)?;
    }

    // Reactor loop — pre_shadow passed via ReactorCtx
    for i in 0..self.reactors.len() {
        let result = {
            let ctx = ReactorCtx {
                shadow: &working,
                pre_shadow,  // FlipReactor reads from here
                // ...
            };
            self.reactors[i].process(&changes, &mut ctx)
        };
        // ...
    }

    // Restore shadow (for non-flip case, put working back)
    if !uses_flip {
        self.shadow = working;
    } else {
        self.shadow = working; // working is the post-mutation state
    }

    Ok(true)
}
```

**FlipReactor reads `ctx.pre_shadow`** for old values. All other reactors ignore it. The clone only happens when flip is active — common case (no flip) uses `mem::take` (O(1)).

### 3. LogicReactor silent misses — registration-time path validation

**Problem:** If a logic expression references `"user.rol"` (typo), the rev_index maps that path → logic_id. When `user.role` changes, `mark_affected` finds nothing for `user.role`. The logic never evaluates. Silent no-op.

**This is the current behavior** — not a new issue introduced by the reactor model. But the reactor refactor is a good time to add validation.

**Solution:** At registration time, `LogicRegistry::register()` validates extracted paths against the current shadow state. If a path doesn't exist in shadow, emit a warning (not an error — the path might be set later by `shadow_init`):

```rust
impl LogicRegistry {
    fn register(
        &mut self,
        tree: LogicNode,
        output_path: String,
        shadow: &ShadowState,
        intern: &mut InternTable,
        rev_index: &mut ReverseDependencyIndex,
    ) -> RegisterResult {
        let paths = tree.extract_paths();
        let mut warnings = Vec::new();

        for path in &paths {
            if shadow.get(path, intern).is_none() {
                warnings.push(format!(
                    "Logic expression references path '{}' which doesn't exist in shadow state. \
                     If this is a typo, the expression will never evaluate.",
                    path
                ));
            }
        }

        // ... register as normal ...

        RegisterResult { id, warnings }
    }
}
```

Warnings are returned to JS via the registration result — they appear in the console during development. Not errors (paths might be set later), but enough signal to catch typos.

**Additionally:** The pre-compiled path segment IDs transform (see "Pre-compiled path segment IDs" section in LogicReactor design) would catch invalid paths at registration time — if `intern.get_id("rol")` returns None, the segment doesn't exist yet. This is a natural validation point.

---

## Open Design Questions

1. **Registration dispatch**: `register_side_effects` currently deserializes JSON and dispatches to individual registration methods. With reactors, this function dispatches typed data to each reactor's registration methods. Acceptable trade-off vs a generic registration interface?
