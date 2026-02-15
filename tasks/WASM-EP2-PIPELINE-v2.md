# WASM-EP2: Shadow State & Pipeline (REVISED)

**Type**: Epic
**Priority**: P0
**Depends on**: WASM-EP1
**Goal**: Move the full pipeline (aggregation → sync → flip) into WASM with nested shadow state for value tracking.

**Key Architectural Decisions**:
- Shadow state is nested object tree (not flat HashMap)
- String paths at WASM boundary (path IDs internal only)
- Batch side effect registration (single call)
- processChanges() handles all pipeline stages
- Primitives stored directly, complex values as JSON strings

---

## WASM-010: Nested shadow state in Rust

**Type**: Story | **Points**: 5 | **Priority**: P0
**Depends on**: WASM-003

### Description

Extend the nested shadow state from EP1 to support pipeline operations. The shadow state mirrors the valtio state structure and tracks all values for evaluation purposes.

### Data model

```rust
struct ShadowState {
    root: ValueRepr
}

enum ValueRepr {
    Object(HashMap<String, Box<ValueRepr>>),  // Nested objects
    Array(Vec<Box<ValueRepr>>),               // Arrays
    String(String),
    Number(f64),
    Bool(bool),
    Null,
}
```

### Key capabilities

- Deep path traversal for reads/writes
- Partial updates preserve sibling paths
- Full subtree replacement
- Affected descendant path calculation
- Value comparison for change detection

### Interface

```rust
fn shadow_init(state_json: &str) -> Result<(), JsValue>
// Input: Nested JSON object matching valtio state

fn shadow_update(path: &str, value_json: &str) -> Result<(), JsValue>
// Update single path with JSON-encoded value

fn shadow_get(path: &str) -> Option<String>
// Get JSON-encoded value at path

fn shadow_dump() -> String  // Debug only
```

### Acceptance criteria

- [ ] Handles primitives (number, bool, string, null)
- [ ] Handles complex values (objects, arrays) as JSON strings
- [ ] Path traversal works for deeply nested structures
- [ ] Partial updates preserve other fields
- [ ] Subtree replacement calculates affected descendants
- [ ] Performance: 1000 nested updates in < 100µs

---

## WASM-011: Sync graph in Rust

**Type**: Story | **Points**: 5 | **Priority**: P0
**Depends on**: WASM-010

### Description

Port the Graph data structure and sync logic to Rust. Uses connected components for O(1) group lookup. When a path in a sync group changes, all peers receive the same value.

### Data model

```rust
struct Graph {
    node_to_component: HashMap<u32, u32>,        // path_id → component_id
    component_to_nodes: HashMap<u32, HashSet<u32>>,  // component_id → [path_ids]
    adjacency: HashMap<u32, HashSet<u32>>,       // path_id → [neighbor path_ids]
    edges: HashSet<(u32, u32)>,                  // all edges
    next_component_id: u32,
}
```

### Interface

```rust
fn register_sync_batch(pairs_json: &str) -> Result<(), JsValue>
// Input: [["user.name", "profile.name"], ["user.email", "profile.email"]]
// Registers all sync pairs in one call

fn unregister_sync_batch(pairs_json: &str) -> Result<(), JsValue>
// Cleanup batch of sync pairs
```

### Algorithm

1. **Registration**: Add edges between paths, merge components as needed
2. **Processing**: When path changes, find component, propagate value to all peers
3. **Normalization**: Handle parent/child/exact match scenarios

### Acceptance criteria

- [ ] Batch registration of sync pairs
- [ ] Connected component tracking with O(1) lookup
- [ ] Component merging when connecting different groups
- [ ] Change propagation to all peers in component
- [ ] Parent/child/exact match normalization
- [ ] Parity with JS Graph implementation
- [ ] Existing sync tests pass

---

## WASM-012: Flip graph in Rust

**Type**: Story | **Points**: 3 | **Priority**: P0
**Depends on**: WASM-011

### Description

Port flip logic to Rust. Reuses the same Graph structure as sync. When a boolean path in a flip group changes, all peers get the inverted value.

### Interface

```rust
fn register_flip_batch(pairs_json: &str) -> Result<(), JsValue>
// Input: [["checkbox1", "checkbox2"], ["toggle1", "toggle2"]]

fn unregister_flip_batch(pairs_json: &str) -> Result<(), JsValue>
```

### Algorithm

Same as sync, but:
- Only applies to boolean values
- Inverts value for peer paths
- Non-boolean values pass through unchanged

### Acceptance criteria

- [ ] Batch registration of flip pairs
- [ ] Boolean inversion for all peers
- [ ] Non-boolean values handled gracefully
- [ ] Shares Graph implementation with sync
- [ ] Parity with JS implementation
- [ ] Existing flip tests pass

---

## WASM-013: Aggregation writes in Rust

**Type**: Story | **Points**: 3 | **Priority**: P1
**Depends on**: WASM-010

### Description

Port aggregation write logic to Rust. When a write targets an aggregation path, distribute it to all source paths. Remove the original target change.

### Interface

```rust
fn register_aggregation_batch(aggregations_json: &str) -> Result<(), JsValue>
// Input: [
//   { "target": "allUsers", "sources": ["user1", "user2", "user3"] },
//   { "target": "summary.total", "sources": ["data.a", "data.b"] }
// ]

fn unregister_aggregation_batch(targets_json: &str) -> Result<(), JsValue>
// Input: ["allUsers", "summary.total"]
```

### Algorithm

1. Iterate changes in reverse order
2. Check if path matches aggregation target
3. Replace with distributed changes to all source paths
4. Remove original target change

### Acceptance criteria

- [ ] Batch registration of aggregations
- [ ] Target writes distributed to all sources
- [ ] Original target change removed
- [ ] Parity with JS implementation
- [ ] Existing aggregation tests pass

---

## WASM-014: normalizeChangesForGroups in Rust

**Type**: Story | **Points**: 3 | **Priority**: P0
**Depends on**: WASM-002

### Description

Port the shared normalization logic used by sync and flip processors. Handles three matching scenarios: exact match, parent change, child change.

### Three match modes

```rust
// 1. Exact match: changePath == registeredPath
//    → value applies directly

// 2. Parent change: registeredPath starts with changePath + '.'
//    → extract nested value from change value

// 3. Child change: changePath starts with registeredPath + '.'
//    → preserve relative path
```

### Implementation notes

- Works with string paths (resolve from intern table for comparisons)
- Handles root path edge case
- Supports deeply nested paths

### Acceptance criteria

- [ ] All three match modes work correctly
- [ ] Path prefix operations use string comparison
- [ ] Parity with JS normalizeChangesForGroups
- [ ] Edge cases: root path, single-segment, deeply nested

---

## WASM-015: processChanges() - Phase 2

**Type**: Story | **Points**: 5 | **Priority**: P0
**Depends on**: WASM-011, WASM-012, WASM-013, WASM-014

### Description

Extend processChanges() to run the full pipeline: aggregation → sync → flip → BoolLogic evaluation. Returns flat list of all changes.

### Enhanced algorithm

```rust
fn process_changes(changes_json: &str) -> Result<String, JsValue> {
    // 1. Parse input changes
    let mut changes = parse_changes(changes_json)?;

    // 2. Process aggregation writes (expand target → sources)
    changes = process_aggregation_writes(changes);

    // 3. Update shadow state
    for change in &changes {
        shadow_update(&change.path, &change.value_json)?;
    }

    // 4. Process sync paths (propagate to peers)
    let sync_changes = process_sync_paths(&changes);
    changes.extend(sync_changes);

    // 5. Update shadow state with sync changes
    for change in &sync_changes {
        shadow_update(&change.path, &change.value_json)?;
    }

    // 6. Process flip paths (invert booleans for peers)
    let flip_changes = process_flip_paths(&changes);
    changes.extend(flip_changes);

    // 7. Update shadow state with flip changes
    for change in &flip_changes {
        shadow_update(&change.path, &change.value_json)?;
    }

    // 8. Calculate affected paths (including descendants)
    let affected_paths = calculate_affected_paths(&changes);

    // 9. Find affected BoolLogic expressions
    let affected_logic_ids = find_affected_logic(affected_paths);

    // 10. Evaluate affected BoolLogic
    let logic_changes = evaluate_boollogic_batch(affected_logic_ids);
    changes.extend(logic_changes);

    // 11. Return all changes (input + pipeline + BoolLogic)
    Ok(serialize_changes(changes))
}
```

### Output format

```typescript
// Same as EP1 - flat array of changes
{
  "changes": [
    { "path": "user.role", "value_json": "\"admin\"" },          // Input
    { "path": "profile.role", "value_json": "\"admin\"" },       // Sync
    { "path": "flag1", "value_json": "true" },                   // Input
    { "path": "flag2", "value_json": "false" },                  // Flip
    { "path": "_concerns.user.email.disabledWhen", "value_json": "true" }  // BoolLogic
  ]
}
```

### Acceptance criteria

- [ ] Pipeline order: aggregation → sync → flip → BoolLogic
- [ ] Shadow state updated after each stage
- [ ] All changes returned in flat array
- [ ] No duplicate changes in output
- [ ] Performance: 10 changes with 50 paths in < 300µs

---

## WASM-016: JS-side pipeline bridge

**Type**: Story | **Points**: 3 | **Priority**: P0
**Depends on**: WASM-015

### Description

Replace existing JS processChanges with thin bridge to WASM. Handles batch registration of side effects at initialization.

### Implementation

```typescript
// src/wasm/bridge.ts

export interface Change {
  path: string
  value: unknown
}

// Initialization
export const initPipeline = (
  state: Record<string, unknown>,
  syncPairs: [string, string][],
  flipPairs: [string, string][],
  aggregations: { target: string; sources: string[] }[]
): void => {
  wasm.shadow_init(JSON.stringify(state))
  wasm.register_sync_batch(JSON.stringify(syncPairs))
  wasm.register_flip_batch(JSON.stringify(flipPairs))
  wasm.register_aggregation_batch(JSON.stringify(aggregations))
}

// Main pipeline entry point
export const processChanges = (changes: Change[]): Change[] => {
  const changesJson = JSON.stringify(
    changes.map(c => ({
      path: c.path,
      value_json: JSON.stringify(c.value)
    }))
  )

  const resultJson = wasm.process_changes(changesJson)
  const result = JSON.parse(resultJson)

  return result.changes.map((c: any) => ({
    path: c.path,
    value: JSON.parse(c.value_json)
  }))
}

// Apply changes to valtio proxy
export const applyChanges = (store: any, changes: Change[]): void => {
  for (const change of changes) {
    dot.set(store, change.path, change.value)
  }
}
```

### Integration points

- `src/_internal/pipeline/processChanges.ts` - replace with WASM bridge
- Side effect registration moves to initialization (batch call)
- Listeners still processed in JS (handled in EP3)

### Acceptance criteria

- [ ] Batch initialization of all side effects
- [ ] processChanges signature unchanged
- [ ] All pipeline stages run in WASM
- [ ] Results applied to valtio proxy in JS
- [ ] All existing pipeline tests pass
- [ ] No public API changes

---

## WASM-017: Phase 2 integration tests

**Type**: Story | **Points**: 3 | **Priority**: P0
**Depends on**: WASM-016

### Description

End-to-end tests verifying the full WASM pipeline produces identical results to the JS pipeline for all processor combinations.

### Test scenarios

1. **Aggregation-only**: Target write → distributed to sources
2. **Sync-only**: Single and multi-component sync groups
3. **Flip-only**: Boolean inversion across peers
4. **Combined pipeline**: Aggregation + sync + flip in one batch
5. **Pipeline + BoolLogic**: Changes trigger both pipeline and concern evaluation
6. **Nested object updates**: Deep changes propagate correctly
7. **Large batch**: 100+ changes through full pipeline
8. **Registration lifecycle**: Add/remove side effects dynamically
9. **Parity test**: JS vs WASM pipeline on identical inputs

### Acceptance criteria

- [ ] All test scenarios pass
- [ ] WASM pipeline matches JS pipeline output exactly
- [ ] Shadow state stays in sync with valtio proxy
- [ ] Performance: WASM not slower for small batches (< 5 changes)
- [ ] Memory: no leaks after repeated cycles
- [ ] Tests run in Node (vitest)

---

## Summary

**Phase 2 Stories**:
- WASM-010: Nested shadow state (extended from EP1)
- WASM-011: Sync graph (batch registration)
- WASM-012: Flip graph (shares graph with sync)
- WASM-013: Aggregation writes
- WASM-014: normalizeChangesForGroups
- WASM-015: processChanges() - Phase 2 (full pipeline)
- WASM-016: JS bridge (batch init, minimal)
- WASM-017: Integration tests

**Total Points**: 27

**Key Simplifications from Original**:
- Batch side effect registration (single call vs multiple)
- Nested shadow state (no value slots)
- String paths at boundary (no path ID cache in JS)
- Single processChanges() for entire pipeline
- Flat output (all changes uniform)

**Dependencies**:
- WASM-010 extends shadow state from WASM-003 (EP1)
- WASM-015 extends processChanges from WASM-006 (EP1)
- All pipeline work builds on string interning (WASM-002)
