# WASM-EP2: Shadow State & Pipeline

**Type**: Epic
**Priority**: P0
**Depends on**: WASM-EP1
**Goal**: Move the full pipeline (aggregation → sync → flip) into WASM with shadow state for value tracking.

---

## WASM-008: Shadow state in Rust

**Type**: Story | **Points**: 5 | **Priority**: P0
**Depends on**: WASM-002

### Description

Implement a shadow copy of all state values in Rust, keyed by interned path IDs. Primitives are stored directly (no serialization). Complex values (objects, arrays) are stored as slot indices pointing into a JS-owned value array.

### Data model

```rust
enum ValueRepr {
    Number(f64),
    Bool(bool),
    Str(u32),         // interned string ID
    Null,
    Undefined,
    Slot(u32),        // index into JS value slots array
}

struct ShadowState {
    values: HashMap<u32, ValueRepr>,
}
```

### Interface

```rust
fn init_shadow_state(entries: &[(u32, ValueRepr)])
fn update_shadow(path_id: u32, value: ValueRepr)
fn get_shadow(path_id: u32) -> ValueRepr
```

### Key decisions

- Getter functions are excluded (JS-only, render-time)
- Object values stored as opaque slot indices — Rust never inspects them
- Shadow state is the single source of truth for WASM-side evaluations

### Acceptance criteria

- [ ] Primitive values (number, bool, string, null, undefined) stored directly
- [ ] Complex values stored as slot indices
- [ ] Initialization from flat key-value pairs
- [ ] Update + get round-trip preserves values
- [ ] Performance: 1000 updates in < 50µs
- [ ] Memory: 10,000 entries < 1MB

---

## WASM-009: Value slot array

**Type**: Story | **Points**: 3 | **Priority**: P0
**Depends on**: WASM-008

### Description

Implement the JS-side value slot array that holds complex (non-primitive) values. WASM references these by index (u32) and never touches the actual JS values. JS resolves slot indices to real values at the boundary (applyBatch, listener dispatch).

### Interface

```typescript
// JS-side
const valueSlots: unknown[] = []
let nextSlot = 0

const allocSlot = (value: unknown): number => {
  const idx = nextSlot++
  valueSlots[idx] = value
  return idx
}

const resolveSlot = (idx: number): unknown => valueSlots[idx]
const freeSlot = (idx: number): void => { valueSlots[idx] = undefined }
```

### Acceptance criteria

- [ ] Slot allocation and resolution work correctly
- [ ] WASM can pass slot indices through pipeline without touching JS values
- [ ] Slot cleanup prevents memory leaks
- [ ] Integration with shadow state: `ValueRepr::Slot(idx)` references correct JS value

---

## WASM-010: processAggregationWrites in Rust

**Type**: Story | **Points**: 3 | **Priority**: P1
**Depends on**: WASM-008, WASM-009

### Description

Port `src/_internal/pipeline/processors/aggregationWrites.ts` to Rust. This processor intercepts writes to aggregation target paths and distributes them to all source paths.

### Current implementation

- `src/_internal/pipeline/processors/aggregationWrites.ts`
- Iterates changes in reverse, checks against registered aggregations map
- Replaces target change with distributed source changes

### Interface

```rust
fn register_aggregation(target_id: u32, source_ids: &[u32])
fn process_aggregation_writes(changes: &mut Vec<Change>) -> Vec<Change>
```

### Acceptance criteria

- [ ] Aggregation target writes are distributed to all source paths
- [ ] Original target change is removed from the batch
- [ ] Registration and unregistration work correctly
- [ ] Parity with JS implementation (same inputs → same outputs)
- [ ] Existing aggregation tests pass

---

## WASM-011: Sync graph + processSyncPaths in Rust

**Type**: Story | **Points**: 5 | **Priority**: P0
**Depends on**: WASM-008, WASM-009

### Description

Port the Graph data structure and `processSyncPaths` processor to Rust. The sync graph uses connected components for O(1) group lookup.

### Current implementation

- `src/utils/graph.ts` — Graph with nodeToComponent, componentToNodes, adjacency, edges
- `src/_internal/pipeline/processors/syncPaths.ts` — uses normalizeChangesForGroups
- Connected component tracking with O(1) lookup

### Rust data model

```rust
struct Graph {
    node_to_component: HashMap<u32, u32>,
    component_to_nodes: HashMap<u32, HashSet<u32>>,
    adjacency: HashMap<u32, HashSet<u32>>,
    edges: HashSet<(u32, u32)>,
    next_component_id: u32,
}
```

### Acceptance criteria

- [ ] Graph operations: add_edge, remove_edge, component lookup
- [ ] Component merging when adding edge between different components
- [ ] Sync processing: change on one path → synced to all peers in component
- [ ] Parent/child/exact match normalization (port normalizeChangesForGroups)
- [ ] Parity with JS implementation
- [ ] Existing sync tests pass

---

## WASM-012: Flip graph + processFlipPaths in Rust

**Type**: Story | **Points**: 3 | **Priority**: P0
**Depends on**: WASM-011

### Description

Port `processFlipPaths` to Rust. Reuses the same Graph structure as sync. When a boolean path changes, all peers in the connected component get the inverted value.

### Current implementation

- `src/_internal/pipeline/processors/flipPaths.ts`
- Same normalizeChangesForGroups logic as sync, but inverts boolean values

### Acceptance criteria

- [ ] Flip processing: boolean change → inverted on all peers
- [ ] Non-boolean values handled gracefully (no crash)
- [ ] Shares Graph implementation with sync (WASM-011)
- [ ] Parity with JS implementation
- [ ] Existing flip tests pass

---

## WASM-013: normalizeChangesForGroups in Rust

**Type**: Story | **Points**: 3 | **Priority**: P0
**Depends on**: WASM-002

### Description

Port `src/_internal/pipeline/normalizeChanges.ts` to Rust. This is the shared normalization logic used by both sync and flip processors.

### Three match modes

1. **Exact match**: `changePath === registeredPath` → value applies directly
2. **Parent change**: `registeredPath.startsWith(changePath + '.')` → extract nested value
3. **Child change**: `changePath.startsWith(registeredPath + '.')` → preserve relative path

### Acceptance criteria

- [ ] All three match modes work correctly with interned path IDs
- [ ] Path prefix operations work with string representations (resolve from intern table for prefix checks)
- [ ] Parity with JS normalizeChangesForGroups
- [ ] Edge cases: root path, single-segment paths, deeply nested paths

---

## WASM-014: JS-side pipeline bridge

**Type**: Story | **Points**: 5 | **Priority**: P0
**Depends on**: WASM-010, WASM-011, WASM-012, WASM-013

### Description

Replace the JS `processChanges` function with a thin bridge that delegates to WASM. The JS side prepares the change batch (path IDs + slot indices), calls WASM, and applies the returned results.

### Current flow (all JS)

```
processChanges → aggregation → sync → flip → listeners → applyBatch
```

### New flow

```
JS: resolve paths to IDs, allocate value slots
  → wasm.processPipeline(changes)
  → WASM runs aggregation → sync → flip (listeners handled separately)
  → returns finalChanges as [(pathId, slotIndex, metaFlags)]
JS: resolve IDs back to paths, resolve slots to values
  → applyBatch
```

### Key files

- `src/_internal/pipeline/processChanges.ts` — replace internals, keep signature

### Acceptance criteria

- [ ] `processChanges` signature unchanged (StoreInstance, ChangeTuple)
- [ ] Pipeline order preserved: aggregation → sync → flip
- [ ] Listeners still processed in JS after WASM pipeline returns
- [ ] applyBatch still applies to valtio proxy in JS
- [ ] All existing pipeline tests pass
- [ ] No public API changes

---

## WASM-015: Phase 2 integration tests

**Type**: Story | **Points**: 3 | **Priority**: P0
**Depends on**: WASM-014

### Description

End-to-end tests verifying the full WASM pipeline produces identical results to the JS pipeline for all processor combinations.

### Test scenarios

1. Aggregation-only changes
2. Sync-only changes (single and multi-component)
3. Flip-only changes
4. Combined: aggregation + sync + flip in one batch
5. Large batch: 100+ changes through full pipeline
6. Registration/unregistration lifecycle
7. Parity test: JS pipeline vs WASM pipeline on identical inputs

### Acceptance criteria

- [ ] All test scenarios pass
- [ ] Performance regression tests: WASM pipeline not slower for small batches (< 5 changes)
- [ ] Shadow state stays in sync with valtio proxy after each pipeline run
- [ ] Memory: no leaks after repeated register/process/unregister cycles
