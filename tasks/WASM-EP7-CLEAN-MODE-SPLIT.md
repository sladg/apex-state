# WASM-EP7: Clean Mode Split — No Dual Registration

**Type**: Epic
**Priority**: P1
**Depends on**: WASM-EP6
**Goal**: When `useLegacyImplementation: false` (WASM mode), NO legacy JS data structures or effects should be created. Clean separation enables accurate benchmarking and eliminates wasted work.

---

## Problem

Currently, when WASM is enabled, side effect registration **still creates JS-side data structures**:

| Component | WASM registration | Redundant JS work |
|-----------|-------------------|--------------------|
| Sync | `wasm.registerSyncBatch()` | `addEdge()` to JS PathGroups graph |
| Flip | `wasm.registerFlipBatch()` | `addEdge()` to JS PathGroups graph |
| Listeners | `wasm.registerListenersBatch()` | JS `listeners` Map + `sortedListenerPaths` sort |
| Aggregation | **No WASM path at all** | `effect()` wrapper + JS `aggregations` Map |

This means:

1. **Benchmarks are unfair** — JS work inflates WASM mode timings
2. **Memory is wasted** — Duplicate graphs in JS + WASM
3. **Cleanup is complex** — Must unregister from both layers

### What's already clean

- **BoolLogic concerns**: Early return in `registration.ts:72-80` — no JS effect created
- **Schema validators**: Early return in `registration.ts:85-127` — no JS effect created
- **processChanges()**: Clean switch in `processChanges.ts:354-359` — JS vs WASM path
- **Custom concerns**: Always JS effect-based (correct, no WASM path exists)

---

## Target State

When `shouldUseWasm(store) === true`:

| Component | Registration | Runtime |
|-----------|-------------|---------|
| Sync | WASM only (`registerSyncBatch`) | WASM pipeline only |
| Flip | WASM only (`registerFlipBatch`) | WASM pipeline only |
| Listeners | WASM router + JS `listenerHandlers` Map only | WASM execution plan + JS handler calls |
| Aggregation | WASM only (`registerAggregationBatch`) | WASM pipeline only |
| BoolLogic | WASM only (already clean) | WASM pipeline only |
| Validators | WASM + JS schema Map (already clean) | WASM pipeline + JS Zod execution |
| Custom concerns | JS effect() (already clean, no WASM path) | JS effect() |

When `shouldUseWasm(store) === false`:

Everything works exactly as today — JS graphs, effects, processors. **No changes to legacy path.**

---

## Stories

### WASM-033: Clean sync registration in WASM mode

**Points**: 2 | **File**: `src/sideEffects/prebuilts/sync.ts`

**Current** (lines 110-119):

```typescript
// Always registers in WASM AND JS graph
const registeredInWasm = shouldUseWasm(store)
if (registeredInWasm) {
  wasm.registerSyncBatch(pairs)
}
// Always adds to JS graph:
addEdge(sync, path1, path2)
```

**Target**:

```typescript
if (shouldUseWasm(store)) {
  wasm.registerSyncBatch(pairs)
  // WASM owns the graph — skip JS PathGroups
  // Initial sync still needs processChanges() call (routes to WASM)
  return wasmOnlyCleanup
}
// Legacy: JS graph
addEdge(sync, path1, path2)
```

**Changes**:

- `registerSyncPairsBatch()`: Skip `addEdge()` when WASM enabled
- `registerSyncPair()`: Same treatment
- Initial sync (`collectGroupSyncChanges`): Still needed — reads current state, calls `processChanges()` which routes to WASM. But should NOT use JS graph (`getGroupPaths`) in WASM mode. Instead, the pairs are already known from the registration call — iterate those directly.
- Cleanup: Only call `wasm.unregisterSyncBatch()`, skip `removeEdge()`

**Acceptance**:

- [ ] WASM mode: No `addEdge()` calls, no JS PathGroups mutations
- [ ] WASM mode: Initial sync still works (via processChanges → WASM path)
- [ ] Legacy mode: Unchanged behavior
- [ ] Cleanup: Only unregisters from the layer that was registered

---

### WASM-034: Clean flip registration in WASM mode

**Points**: 1 | **File**: `src/sideEffects/prebuilts/flip.ts`

**Current** (lines 22-29):

```typescript
// Always registers in both
if (registeredInWasm) {
  wasm.registerFlipBatch([[path1, path2]])
}
addEdge(flip, path1, path2)  // Always
```

**Target**:

```typescript
if (shouldUseWasm(store)) {
  wasm.registerFlipBatch([[path1, path2]])
  return () => wasm.unregisterFlipBatch([[path1, path2]])
}
// Legacy: JS graph
addEdge(flip, path1, path2)
```

**Changes**:

- `registerFlipPair()`: Early return with WASM-only cleanup when WASM enabled
- No JS graph mutation in WASM mode

**Acceptance**:

- [ ] WASM mode: No `addEdge()` calls
- [ ] Legacy mode: Unchanged behavior

---

### WASM-035: Clean listener registration in WASM mode

**Points**: 2 | **File**: `src/sideEffects/prebuilts/listeners.ts`

**Current** (lines 60-94):

```typescript
// Always stores in JS listeners Map + listenerHandlers Map
listeners.set(mapKey, [...existing, registration])
listenerHandlers.set(subscriberId, { scope, fn })
updateSortedListenerPaths(graphs)

// Also registers in WASM
if (shouldUseWasm(store)) {
  wasm.registerListenersBatch([...])
}
```

**Target**:

```typescript
// Always need listenerHandlers (used by executeFullExecutionPlan for handler lookup)
listenerHandlers.set(subscriberId, { scope, fn })

if (shouldUseWasm(store)) {
  wasm.registerListenersBatch([...])
  // Skip: listeners Map, sortedListenerPaths (used only by JS processor)
  return wasmOnlyCleanup
}

// Legacy: JS listeners Map for JS processor
listeners.set(mapKey, [...existing, registration])
updateSortedListenerPaths(graphs)
```

**Key insight**: `listenerHandlers` Map is needed in BOTH modes because `executeFullExecutionPlan()` in `processChanges.ts:71` does:

```typescript
const registration = listenerHandlers.get(d.subscriber_id)
```

But `listeners` Map and `sortedListenerPaths` are only used by `processListeners()` (legacy JS processor).

**Changes**:

- `registerListener()`: Skip `listeners` Map and `sortedListenerPaths` in WASM mode
- Keep `listenerHandlers` Map in both modes
- Cleanup: Only unregister from layers that were registered

**Acceptance**:

- [ ] WASM mode: No `listeners.set()`, no `updateSortedListenerPaths()`
- [ ] WASM mode: `listenerHandlers` still populated (needed by execution plan)
- [ ] Legacy mode: Unchanged behavior

---

### WASM-036: Add WASM aggregation path (remove effect)

**Points**: 3 | **File**: `src/sideEffects/prebuilts/aggregation.ts`

**Current** (lines 52-87):

```typescript
// ALWAYS creates effect() — no WASM path exists
aggregations.set(targetPath, existing)

const dispose = effect(() => {
  const allEqual = dot.same(store.state, ...sourcePaths)
  const result = allEqual ? dot.get__unsafe(store.state, sourcePaths[0]!) : undefined
  dot.set__unsafe(store.state, targetPath, result)
})
```

**Problem**: Aggregation has NO WASM registration path at all. The `effect()` runs even in WASM mode. The write direction (target → sources) is handled by `processAggregationWrites` in the JS pipeline, which WASM's `process_changes` already handles. But the read direction (sources → target) is only in the JS `effect()`.

**Target**:

```typescript
if (shouldUseWasm(store)) {
  wasm.registerAggregationBatch([{ target: targetPath, sources: sourcePaths }])
  // WASM handles write direction in pipeline
  // Read direction: initial sync via processChanges (WASM handles)
  // No effect() needed
  return () => wasm.unregisterAggregationBatch([targetPath])
}

// Legacy: effect() for read direction + aggregations Map for write direction
aggregations.set(targetPath, existing)
const dispose = effect(() => { ... })
```

**Question for developer**: Does WASM's `process_changes` already handle the read direction (sources → target aggregation)? If not, this needs a Rust-side addition first. Check `rust/src/pipeline.rs` for `process_aggregation_writes` — does it only handle write direction?

**Changes**:

- `registerAggregations()`: Add WASM path with early return
- Skip `effect()` creation in WASM mode
- Skip `aggregations` Map storage in WASM mode (only used by JS processor)
- WASM already has `registerAggregationBatch` / `unregisterAggregationBatch` in bridge.ts

**Acceptance**:

- [ ] WASM mode: No `effect()` created for aggregations
- [ ] WASM mode: No `aggregations.set()` calls
- [ ] WASM mode: Write direction works via WASM pipeline
- [ ] WASM mode: Read direction works (may need Rust change)
- [ ] Legacy mode: Unchanged behavior
- [ ] Initial state sync works in both modes

---

### WASM-037: Verify no cross-contamination + benchmark comparison test

**Points**: 2 | **File**: New test or extend existing

**Purpose**: Verify that WASM mode creates zero JS-side effects/graphs, and that legacy mode creates zero WASM registrations.

**Tests**:

- WASM mode: After registering sync/flip/listeners/aggregation, assert JS graphs are empty (no PathGroups edges, no `listeners` Map entries, no `sortedListenerPaths`)
- WASM mode: Assert `listenerHandlers` Map IS populated (expected)
- WASM mode: Assert no `effect()` subscriptions for aggregation
- Legacy mode: After registering, assert no `wasm.*` calls made (WASM not loaded)
- Benchmark: Same operations in both modes, compare timing (no assertions on speed, just verify both paths complete correctly)

**Acceptance**:

- [ ] Clean separation verified in tests
- [ ] No regressions in either mode

---

## Summary

| Story | File | Points | What changes |
|-------|------|--------|-------------|
| WASM-033 | `sideEffects/prebuilts/sync.ts` | 2 | Skip JS PathGroups in WASM mode |
| WASM-034 | `sideEffects/prebuilts/flip.ts` | 1 | Skip JS PathGroups in WASM mode |
| WASM-035 | `sideEffects/prebuilts/listeners.ts` | 2 | Skip `listeners` Map, keep `listenerHandlers` |
| WASM-036 | `sideEffects/prebuilts/aggregation.ts` | 3 | Add WASM path, remove `effect()` |
| WASM-037 | Tests | 2 | Verify clean separation |
| **Total** | | **10** | |

---

## Dependency Graph

```
WASM-033 (sync)  ──┐
WASM-034 (flip)  ──┤
WASM-035 (listeners) ──┼── WASM-037 (verification)
WASM-036 (aggregation) ─┘
```

Stories 033-036 are independent (different files). Story 037 depends on all four.

---

## Risk: Aggregation Read Direction

WASM-036 has an open question: does the Rust pipeline handle sources → target aggregation (read direction)? If not, this story needs a Rust-side change first, making it larger (5pts). Developer should verify before implementation.
