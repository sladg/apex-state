# WASM-EP5: Streaming Data Gateway (REVISED)

**Type**: Epic
**Priority**: P1
**Depends on**: WASM-EP2
**Goal**: Use WASM nested shadow state as a change detection gateway for high-frequency external data, filtering out no-op updates before they reach the pipeline or valtio proxies.

**Key Architectural Decisions**:
- Shadow state is nested tree (matches state structure)
- Diff compares against nested shadow state
- String paths at boundary (no path IDs in JS)
- Diff + pipeline run in single processChanges() call
- Primitives diffed by value, complex values by reference

---

## WASM-028: Shadow state diff engine

**Type**: Story | **Points**: 4 | **Priority**: P1
**Depends on**: WASM-010

### Description

Implement a diff function in Rust that compares incoming changes against the nested shadow state and returns only genuine changes. Works with the nested tree structure, not flat HashMap.

### Interface

```rust
fn diff_changes(changes_json: &str) -> Result<String, JsValue>
// Input: [{ "path": "user.email", "value_json": "\"test@example.com\"" }]
// Returns: [{ "path": "user.email", "value_json": "..." }]  (only if changed)
```

### Diff algorithm

```rust
fn diff_changes(changes_json: &str) -> Result<String, JsValue> {
    let changes: Vec<Change> = serde_json::from_str(changes_json)?;
    let mut genuine_changes = Vec::new();

    for change in changes {
        // Get current value from nested shadow state
        let current = shadow_get(&change.path);

        // Parse incoming value
        let new_value: ValueRepr = parse_value_repr(&change.value_json);

        // Compare
        if is_different(&current, &new_value) {
            genuine_changes.push(change);
        }
    }

    Ok(serde_json::to_string(&genuine_changes)?)
}

fn is_different(current: &Option<ValueRepr>, new: &ValueRepr) -> bool {
    match (current, new) {
        (None, _) => true,  // First time seeing this path
        (Some(ValueRepr::Number(a)), ValueRepr::Number(b)) => {
            // Handle NaN, -0/+0 edge cases
            !floats_equal(*a, *b)
        }
        (Some(ValueRepr::Bool(a)), ValueRepr::Bool(b)) => a != b,
        (Some(ValueRepr::String(a)), ValueRepr::String(b)) => a != b,
        (Some(ValueRepr::Null), ValueRepr::Null) => false,
        (Some(ValueRepr::Object(_)), ValueRepr::Object(_)) => {
            // Objects always pass through (no deep comparison)
            true
        }
        (Some(ValueRepr::Array(_)), ValueRepr::Array(_)) => {
            // Arrays always pass through (no deep comparison)
            true
        }
        _ => true  // Type mismatch = different
    }
}
```

### Diff rules

| Type | Comparison | Result if equal |
|---|---|---|
| Number | Bitwise equal (handles NaN, -0/+0) | DROP |
| Bool | `==` | DROP |
| String | `==` | DROP |
| Null | Type match | DROP |
| Object | Reference (always different) | KEEP |
| Array | Reference (always different) | KEEP |

### Acceptance criteria

- [ ] Primitives correctly diffed (number, bool, string, null)
- [ ] Complex values (objects, arrays) always pass through
- [ ] Nested paths work correctly (traverse shadow tree)
- [ ] Edge cases: NaN, -0 vs +0, undefined
- [ ] Performance: diff 1000 changes in < 20µs
- [ ] No allocations in the no-change fast path

---

## WASM-029: Integrated diff + pipeline

**Type**: Story | **Points**: 4 | **Priority**: P1
**Depends on**: WASM-028, WASM-015

### Description

Integrate the diff engine with processChanges() so that streaming data can be diffed and processed in a single WASM call. This avoids the overhead of round-tripping through JS between diff and pipeline.

### New interface

```rust
fn process_changes_with_diff(changes_json: &str, enable_diff: bool) -> Result<String, JsValue>
// When enable_diff=true:
//   1. Diff changes against shadow state
//   2. If no genuine changes, return empty result immediately
//   3. Otherwise, run full pipeline on genuine changes
// When enable_diff=false:
//   Run pipeline directly (same as process_changes)
```

### Algorithm

```rust
fn process_changes_with_diff(changes_json: &str, enable_diff: bool) -> Result<String, JsValue> {
    let changes: Vec<Change> = serde_json::from_str(changes_json)?;

    let changes_to_process = if enable_diff {
        // Diff against shadow state
        let mut genuine = Vec::new();
        for change in changes {
            let current = shadow_get(&change.path);
            let new_value = parse_value_repr(&change.value_json);

            if is_different(&current, &new_value) {
                genuine.push(change);
            }
        }

        // Early exit if no genuine changes
        if genuine.is_empty() {
            return Ok(json!({ "changes": [] }).to_string());
        }

        genuine
    } else {
        changes
    };

    // Run full pipeline (aggregation → sync → flip → BoolLogic)
    process_changes_internal(changes_to_process)
}
```

### Key optimization

When diff is enabled and all changes are no-ops:
- Return immediately with empty result
- Skip pipeline entirely
- No shadow state updates
- No listener dispatch
- Zero valtio proxy writes

### Acceptance criteria

- [ ] Diff + pipeline run in single WASM call
- [ ] Early exit when all changes are no-ops
- [ ] Shadow state stays consistent
- [ ] Genuine changes flow through full pipeline
- [ ] Performance: diff + pipeline for 500 fields (10% changed) < 80µs

---

## WASM-030: createStreamGateway API

**Type**: Story | **Points**: 3 | **Priority**: P1
**Depends on**: WASM-029

### Description

Implement the JS-side `createStreamGateway` wrapper that provides a clean API for feeding external streaming data through the WASM diff engine and pipeline.

### API

```typescript
interface StreamGateway<DATA extends object> {
  /**
   * Diff payload against shadow state, run pipeline for genuine changes.
   * Accepts flat key-value pairs (path → value).
   */
  ingest(updates: Partial<Record<DeepKey<DATA>, unknown>>): void

  /**
   * Diff and process a raw array of changes.
   */
  ingestBatch(changes: Change[]): void

  /**
   * Cleanup. Removes gateway state, does NOT affect store.
   */
  dispose(): void
}

const createStreamGateway: <DATA extends object>(
  store: StoreInstance<DATA>
) => StreamGateway<DATA>
```

### Implementation

```typescript
// src/wasm/streamGateway.ts

export const createStreamGateway = <DATA extends object>(
  store: StoreInstance<DATA>
): StreamGateway<DATA> => {
  let disposed = false

  const ingest = (updates: Partial<Record<DeepKey<DATA>, unknown>>): void => {
    if (disposed) throw new Error('Gateway disposed')

    const changes: Change[] = Object.entries(updates).map(([path, value]) => ({
      path,
      value
    }))

    ingestBatch(changes)
  }

  const ingestBatch = (changes: Change[]): void => {
    if (disposed) throw new Error('Gateway disposed')
    if (changes.length === 0) return

    // Call WASM with diff enabled
    const changesJson = JSON.stringify(
      changes.map(c => ({
        path: c.path,
        value_json: JSON.stringify(c.value)
      }))
    )

    const resultJson = wasm.process_changes_with_diff(changesJson, true)
    const result = JSON.parse(resultJson)

    // If no genuine changes, WASM returns empty result
    if (result.changes.length === 0) {
      return  // Fast path: no work needed
    }

    // Apply genuine changes to valtio proxy
    const allChanges = result.changes.map((c: any) => ({
      path: c.path,
      value: JSON.parse(c.value_json)
    }))

    applyChanges(store, allChanges)
  }

  const dispose = (): void => {
    disposed = true
    // Gateway is stateless beyond shared shadow state
    // No cleanup needed in WASM
  }

  return { ingest, ingestBatch, dispose }
}
```

### Key decisions

- Gateway is stateless (uses shared shadow state)
- Multiple gateways can coexist on same store
- Gateway does NOT own store lifecycle
- Diff is opt-in (regular processChanges still available)

### Acceptance criteria

- [ ] `ingest()` accepts typed path-value pairs
- [ ] `ingestBatch()` accepts pre-formed changes
- [ ] Early exit when all changes are no-ops (zero proxy writes)
- [ ] Genuine changes flow through full pipeline
- [ ] `dispose()` cleans up without affecting store
- [ ] Type-safe: `ingest()` only accepts valid `DeepKey<DATA>` paths

---

## WASM-031: Phase 5 integration tests

**Type**: Story | **Points**: 3 | **Priority**: P1
**Depends on**: WASM-030

### Description

End-to-end tests for the streaming data gateway.

### Test scenarios

1. **No-op batch**
   - All values unchanged → zero proxy writes, zero re-renders
   - Verify WASM early exit path

2. **Partial update**
   - 100 fields ingested, 10 changed → only 10 applied
   - Verify diff filtering works

3. **Primitive types**
   - Number, boolean, string, null — all diffed correctly
   - Edge cases: NaN, -0 vs +0, undefined

4. **Complex values**
   - Objects always pass through (no false drops)
   - Arrays always pass through

5. **Nested paths**
   - Deep paths like "user.profile.settings.theme"
   - Verify shadow tree traversal

6. **Pipeline interaction**
   - Genuine changes trigger sync/flip/listeners correctly
   - BoolLogic re-evaluated for affected paths

7. **Concurrent sources**
   - `gateway.ingest()` + manual `setValue()` in same tick
   - Verify shadow state consistency

8. **High frequency**
   - 1000 ingests with 1% change rate
   - Verify minimal work done (diff filters most)

9. **Multiple gateways**
   - Two gateways on same store
   - Both work correctly, no interference

10. **Cleanup**
    - `dispose()` gateway
    - Verify no memory leaks

### Performance targets

- No-op batch (1000 fields): < 10µs
- 10% change rate (1000 fields): < 100µs
- 1000 ingests at 10ms intervals: < 50ms total JS thread time

### Acceptance criteria

- [ ] All scenarios pass
- [ ] Performance targets met
- [ ] Memory stable after 10,000 ingest cycles
- [ ] Gateway plays nicely with concerns (BoolLogic, validation)
- [ ] Tests run in Node (vitest)

---

## Summary

**Phase 5 Stories**:
- WASM-028: Shadow state diff engine (nested tree)
- WASM-029: Integrated diff + pipeline (single call)
- WASM-030: createStreamGateway API
- WASM-031: Integration tests

**Total Points**: 14

**Key Simplifications from Original**:
- Nested shadow state (no flat HashMap with slots)
- String paths at boundary (no path IDs in JS)
- Diff + pipeline in single call (no intermediate round trip)
- processChanges_with_diff extends existing processChanges
- Gateway is stateless (uses shared shadow state)

**Dependencies**:
- WASM-028 builds on nested shadow state from WASM-010 (EP2)
- WASM-029 extends processChanges from WASM-015 (EP2)
- Works alongside BoolLogic, validation, listeners (all use same shadow state)

**Use Cases**:
- High-frequency market data feeds
- WebSocket streams with frequent but mostly unchanged data
- Polling APIs that return full state (most fields unchanged)
- Any scenario where change detection overhead matters

**Performance Benefits**:
- No-op batches: ~99% reduction in work (10µs vs 1ms+)
- Partial updates: Only genuine changes processed
- Zero valtio proxy writes for unchanged fields
- Zero React re-renders for filtered changes
