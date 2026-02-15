# WASM-EP5: Pipeline Refactor — WASM as Single Orchestrator

**Type**: Epic
**Priority**: P1
**Depends on**: WASM-EP4
**Goal**: Move all orchestration logic into WASM. TypeScript becomes a dumb executor that only does what it *must* do in JS (call listener functions, execute Zod schemas, apply to valtio).

---

## Problem

Current `processChangesWASM()` does too much in JS:
- Manually applies concern changes to `_concerns` proxy (stripping `_concerns.` prefix)
- Manually executes validators (parsing JSON, calling Zod, writing results)
- Manually builds queue, executes listeners, queues produced changes
- Manually calls `diffChanges()` on output
- Holds intermediate state (processing.queue)

WASM should own the full pipeline. JS only crosses the boundary for things that *must* live in JS.

---

## Target Architecture

```
processChangesWASM(store, changes):

  1. JS → WASM: raw changes
     wasm.processChanges(changes)

  2. WASM → JS: phase 1 result
     {
       execution_plan,        // which listeners to call
       validators_to_run,     // which Zod schemas to execute
     }
     (concern changes like BoolLogic are already included in final output — no separate handling)

  3. JS executes listeners (can't be in WASM — user functions)
     → collects produced changes

  4. JS executes Zod validators (can't be in WASM — Zod is JS)
     → collects validation results as changes: [{ path: "_concerns.X.validationState", value: {...} }]

  5. JS → WASM: produced changes + validation results
     wasm.finalize(producedChanges, validationResults)

  6. WASM → JS: final flat change list
     {
       state_changes: Change[],    // apply to store.state
       concern_changes: Change[],  // apply to store._concerns (already prefixed)
     }
     (diffed — only genuine changes, ready for valtio)

  7. JS: apply to valtio
     applyBatch(stateChanges, store.state)
     applyBatch(concernChanges, store._concerns)
     Done.
```

---

## Story: WASM-032 — Pipeline round-trip refactor

**Points**: 5

### Rust changes (`rust/src/pipeline.rs`)

**Split `process_changes_vec()` into two phases:**

**Phase 1: `process_changes()` (existing, modified)**
- Diff input changes (checkpoint 1)
- Run aggregation → sync → flip → BoolLogic
- Diff pre-listener (checkpoint 2)
- Build execution plan for listeners
- Collect validators_to_run
- Store intermediate state internally (BoolLogic concern changes held in buffer)
- Return: `{ execution_plan, validators_to_run }`

**Phase 2: `finalize(produced_changes, validation_results)` (new)**
- Accept listener-produced changes + validation result changes
- Run produced changes through diff
- Merge all changes: state changes + concern changes (BoolLogic from phase 1 + validation results) + listener-produced
- Diff final output (checkpoint 3)
- Update shadow state
- Return: `{ state_changes, concern_changes }` — flat, diffed, ready to apply

### Rust changes (`rust/src/lib.rs`)

- Add `#[wasm_bindgen] fn pipeline_finalize(...)` export
- `process_changes()` return type changes (no more concern_changes in phase 1)

### Bridge changes (`src/wasm/bridge.ts`)

- `wasm.processChanges()` return type updated: `{ execution_plan, validators_to_run }`
- Add `wasm.pipelineFinalize(producedChanges, validationResults): { state_changes, concern_changes }`
- Remove `wasm.diffChanges()` (absorbed into pipeline)

### JS changes (`src/pipeline/processChanges.ts`)

**Simplify `processChangesWASM()` to:**

```typescript
const processChangesWASM = (store, initialChanges) => {
  // 1. Send to WASM
  const { execution_plan, validators_to_run } = wasm.processChanges(bridgeChanges)

  // 2. Early exit if nothing to do
  if (!execution_plan && validators_to_run.length === 0) {
    // Check if WASM has pending changes (BoolLogic etc)
    const final = wasm.pipelineFinalize([], [])
    if (final.state_changes.length === 0 && final.concern_changes.length === 0) return
    applyAll(store, final)
    return
  }

  // 3. Execute listeners (JS-only: user functions)
  const producedChanges = executeFullExecutionPlan(execution_plan, ...)

  // 4. Execute validators (JS-only: Zod schemas)
  const validationResults = runValidators(validators_to_run)

  // 5. Send results back to WASM
  const final = wasm.pipelineFinalize(producedChanges, validationResults)

  // 6. Apply to valtio
  applyAll(store, final)
}

const applyAll = (store, final) => {
  // State changes → store.state
  applyBatch(final.state_changes, store.state)
  // Concern changes → store._concerns (path already correct, no stripping needed)
  for (const c of final.concern_changes) {
    dot.set__unsafe(store._concerns, c.path, c.value)
  }
}
```

**Remove from JS:**
- Manual `_concerns.` prefix stripping
- Manual validator JSON parsing + Zod execution + result writing
- Manual `processing.queue` management
- Separate `diffChanges()` call

### Concern path simplification

**Registration** (`src/concerns/registration.ts`):
- BoolLogic already registers with `_concerns.${path}.${concernName}` as output_path — no change needed
- Validators already register with `_concerns.${path}.${concernName}` as output_path — no change needed

**WASM returns concern changes with paths relative to `_concerns` root** (e.g., `user.email.disabledWhen`), not with `_concerns.` prefix. JS applies directly to `store._concerns` — no prefix stripping.

OR: WASM returns full paths (`_concerns.user.email.disabledWhen`) and JS does a single prefix strip in `applyAll()`. Either way, one place, not scattered.

### Acceptance criteria

- [ ] `processChangesWASM()` is a simple 3-step loop: send → execute JS-only work → finalize
- [ ] No manual concern change handling in JS (no `_concerns.` stripping)
- [ ] No manual validator result writing in JS
- [ ] WASM owns all diffing (3 checkpoints internal)
- [ ] WASM owns shadow state updates
- [ ] All existing tests pass
- [ ] Listener execution still works (JS-side, user functions)
- [ ] Zod validation still works (JS-side, schemas)
- [ ] BoolLogic concerns still applied correctly

---

## Summary

| What | Before | After |
|------|--------|-------|
| Concern changes | JS strips prefix, applies manually | WASM includes in final output, JS applies uniformly |
| Validators | JS parses JSON, calls Zod, writes result | JS calls Zod, sends result to WASM, WASM includes in final output |
| Diffing | JS calls diffChanges() separately | WASM diffs internally at 3 checkpoints |
| Listener dispatch | JS builds queue, manages produced changes | JS executes handlers, sends results to WASM |
| Shadow state | Updated at unclear point | WASM updates during finalize() |
| processChangesWASM | ~80 lines of orchestration | ~20 lines: send → execute → finalize → apply |
