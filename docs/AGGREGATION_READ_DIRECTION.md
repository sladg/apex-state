# Aggregation Read Direction Implementation

**Date**: 2026-02-16
**Status**: Implemented (Initial + Reactive)

## Summary

Implemented the **read direction** for aggregations in WASM:

1. **Initial computation**: On registration, computes target values from sources
2. **Reactive updates**: During `processChanges()`, recomputes targets when sources change

## Background

Previously, WASM only handled the **write direction** (target → sources):

- User writes to `allUsers` → distributes to `["user1", "user2", "user3"]`

The **read direction** (sources → target) was missing:

- When registering an aggregation, compute the initial target value from sources
- If all sources equal → target = that value
- If sources differ → target = `null`

## Implementation

### Rust Changes

#### 1. `rust/src/aggregation.rs`

Added `process_aggregation_reads()` function (unified logic for both initial and reactive):

- Takes changed paths and shadow state
- Uses reverse index to find affected targets
- Recomputes target values for those targets
- Returns changes with no-op filtering

**Logic**:

```rust
- Empty sources → target = null
- Any source missing → skip (no change)
- All sources equal → target = source_value
- Sources differ → target = null
- Target already correct → skip (no-op filter)
```

**Key insight**: This single function handles both initial computation (on registration) and reactive updates (during runtime). Registration just passes all source paths as "changed" to trigger the computation.

Added reverse index:

- `source_to_targets: HashMap<String, Vec<String>>`
- Maintained during `register()` and `unregister()`
- Enables O(1) lookup of affected targets

#### 2. `rust/src/pipeline.rs`

Modified `register_aggregation_batch()`:

- Changed return type from `Result<(), String>` to `Result<String, String>`
- Collects all source paths from registrations
- Calls `process_aggregation_reads()` with those source paths
- Returns changes as JSON array

**Simplification**: Instead of duplicating computation logic, we reuse the reactive path by treating all sources as "changed" on registration.

Added reactive processing in `process_changes_vec()`:

- Step 7.5: After sync/flip, process aggregation reads
- Collects all changed paths from output buffer
- Calls `process_aggregation_reads()` to recompute affected targets
- Applies target updates to shadow state and output

#### 3. `rust/src/lib.rs`

Updated WASM binding:

- Changed return type from `Result<(), JsValue>` to `Result<String, JsValue>`
- Updated documentation

### TypeScript Changes

#### 1. `src/wasm/bridge.ts`

Updated `registerAggregationBatch()`:

- Now parses the returned JSON string
- Converts WASM changes to JS format
- Returns `Change[]`

#### 2. `src/sideEffects/prebuilts/aggregation.ts`

Modified WASM mode registration:

- Receives initial changes from WASM
- Applies them directly using `applyBatch()` (not through pipeline)
- **Important**: Uses `applyBatch()` to avoid triggering write direction, which would overwrite sources

## Tests

### Rust Tests (`rust/src/aggregation.rs`)

**Initial computation (8 tests):**

- ✅ All sources equal → target = value
- ✅ Sources differ → target = null
- ✅ All sources null → target = null
- ✅ Missing sources → skip
- ✅ Empty sources → target = null
- ✅ String values
- ✅ No-op filter (target already correct)
- ✅ No-op filter (target already null, sources differ)

**Reactive updates (6 tests):**

- ✅ Source change makes sources equal → target updates
- ✅ Source change makes sources differ → target updates to null
- ✅ No affected aggregations → no changes
- ✅ Child path change detected
- ✅ No-op filter for reactive reads
- ✅ Multiple aggregations handled independently

### TypeScript Tests (`tests/wasm/pipeline-integration.test.ts`)

**Initial computation (8 tests):**

- ✅ Compute initial value on registration (all equal)
- ✅ Set target to null when sources differ
- ✅ Handle all sources being null
- ✅ Skip if sources don't exist
- ✅ Handle empty sources array
- ✅ Filter no-op changes (target already correct)
- ✅ Filter no-op changes (target already null, sources differ)

**Reactive updates (5 tests):**

- ✅ Reactively update target when source changes (makes equal)
- ✅ Reactively update target when source changes (makes differ)
- ✅ No reactive update if target already correct
- ✅ Handle multiple aggregations reactively
- ✅ Integration with sync/flip pipeline

## Usage Example

```typescript
// Register aggregation
const initialChanges = wasm.registerAggregationBatch([
  { target: 'allChecked', sources: ['item1', 'item2', 'item3'] }
])

// If all sources are true:
// initialChanges = [{ path: 'allChecked', value: true }]

// If sources differ:
// initialChanges = [{ path: 'allChecked', value: null }]

// Apply changes directly (not through pipeline)
applyBatch(
  initialChanges.map(c => [c.path, c.value, {}]),
  store.state
)
```

## Key Design Decisions

### 1. Direct Application vs Pipeline

**Decision**: Apply initial changes using `applyBatch()`, not `processChanges()`

**Reason**:

- `processChanges()` would trigger write direction
- Write direction distributes target → sources
- This would overwrite the source values we just read
- Direct application avoids the loop

### 2. Null vs Undefined

**Decision**: Use `null` (JSON `null`) for mismatched sources

**Reason**:

- JSON doesn't have `undefined`
- Consistent with how JavaScript undefined serializes
- Clear semantic: "no common value"

### 3. Skip Missing Sources

**Decision**: Don't create changes if any source is missing

**Reason**:

- Can't compute a meaningful aggregate
- User might register before shadow state is fully initialized
- Let subsequent updates handle it

## Comparison with Legacy Implementation

### Legacy TypeScript (`src/sideEffects/prebuilts/aggregation.ts`)

Used `effect()` to reactively compute target:

```typescript
effect(() => {
  const allEqual = dot.same(store.state, ...sourcePaths)
  const result = allEqual ? dot.get__unsafe(store.state, sourcePaths[0]) : undefined
  dot.set__unsafe(store.state, targetPath, result)
})
```

**Pros**:

- Reactive: updates automatically when sources change
- Simple, declarative

**Cons**:

- Creates ongoing reactive effects
- Harder to reason about update order
- Effect cleanup needed

### New WASM Implementation

Combines initial computation with reactive updates in pipeline:

```rust
// On registration: compute initial
let initial_changes = compute_aggregation_initial_values(&registry, &shadow)

// During processChanges: reactive recomputation
// 1. Write direction: allChecked=true → distributes to [item1, item2, item3]
// 2. Read direction: item1 changes → recomputes allChecked
```

**Pros**:

- ✅ Full feature parity with legacy
- ✅ Explicit, synchronous computation in pipeline
- ✅ No ongoing effects (computed on-demand)
- ✅ Clearer ownership (WASM owns logic)
- ✅ Integrated with sync/flip/listener pipeline

**Cons**:

- Slightly more complex pipeline logic
- Requires reverse index maintenance

## Future Considerations

### Aggregation Modes

Currently uses "all equal" logic. Future modes could include:

- `sum`: target = sum of sources
- `and`: target = all sources true
- `or`: target = any source true
- `count`: target = count of truthy sources

Would require:

1. Add `mode` field to `Aggregation` struct
2. Implement mode-specific logic in `compute_aggregation_initial_values()`
3. Update TypeScript types

### Reactive Updates

✅ **Implemented!** The system now:

1. Maintains source → target reverse index
2. On source change, identifies affected targets
3. Recomputes target values synchronously in pipeline
4. Applies changes with no-op filtering

This provides full feature parity with the legacy `effect()` behavior.

## Related Tasks

- **WASM-EP2**: Shadow State & Pipeline
- **WASM-036**: WASM aggregation path (clean mode split)
- **Task BACKLOG**: "WASM-036 depends on whether Rust pipeline handles aggregation read direction"

## Files Changed

### Rust

- `rust/src/aggregation.rs`:
  - Added `source_to_targets` reverse index
  - Added `compute_aggregation_initial_values()` (initial computation)
  - Added `process_aggregation_reads()` (reactive recomputation)
  - Added 14 unit tests (8 initial + 6 reactive)
- `rust/src/pipeline.rs`:
  - Modified `register_aggregation_batch()` to return changes
  - Integrated `process_aggregation_reads()` in Step 7.5
- `rust/src/lib.rs` — Updated WASM binding return type

### TypeScript

- `src/wasm/bridge.ts` — Updated `registerAggregationBatch()` to parse JSON
- `src/sideEffects/prebuilts/aggregation.ts` — Apply initial changes with `applyBatch()`

### Tests

- `tests/wasm/pipeline-integration.test.ts` — Added 13 integration tests (8 initial + 5 reactive)
- `rust/src/aggregation.rs` — Added 14 unit tests (8 initial + 6 reactive)
