# WASM-EP5: Streaming Data Gateway

**Type**: Epic
**Priority**: P1
**Depends on**: WASM-EP2
**Goal**: Use WASM shadow state as a change detection gateway for high-frequency external data, filtering out no-op updates before they reach the pipeline or valtio proxies.

---

## WASM-025: Shadow state diff engine

**Type**: Story | **Points**: 3 | **Priority**: P1
**Depends on**: WASM-008

### Description

Implement a diff function in Rust that compares incoming values against the shadow state and returns only genuine changes. Primitives are compared by value. Complex values (objects, arrays) always pass through (can't deep-compare cheaply).

### Interface

```rust
fn diff_against_shadow(
    updates: &[(u32, ValueRepr)]
) -> Vec<(u32, ValueRepr)>
// Returns only entries where value differs from shadow state
// Also updates shadow state with new values for changed entries
```

### Diff rules

| Type | Comparison | Result if equal |
|---|---|---|
| Number(f64) | bitwise equal (handles NaN) | DROP |
| Bool(bool) | `==` | DROP |
| Str(u32) | intern ID `==` | DROP |
| Null / Undefined | type match | DROP |
| Slot(u32) | always different (opaque) | KEEP |

### Acceptance criteria

- [ ] Primitives correctly diffed (including NaN, -0 vs +0 edge cases)
- [ ] Complex values (Slot) always pass through
- [ ] Shadow state updated atomically (only changed values)
- [ ] Performance: diff 1000 entries in < 10µs
- [ ] No allocations in the no-change fast path

---

## WASM-026: createStreamGateway API

**Type**: Story | **Points**: 3 | **Priority**: P1
**Depends on**: WASM-025

### Description

Implement the JS-side `createStreamGateway` wrapper that provides a clean API for feeding external data through the WASM diff engine and into the pipeline.

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
  ingestBatch(changes: ChangeTuple): void

  /**
   * Cleanup. Removes gateway state, does NOT affect store.
   */
  dispose(): void
}

const createStreamGateway: <DATA extends object>(
  store: StoreInstance<DATA>
) => StreamGateway<DATA>
```

### Key decisions

- Gateway is stateless beyond the WASM shadow state reference
- Multiple gateways can coexist on the same store (e.g., different subscription feeds)
- Gateway does NOT own the store lifecycle

### Acceptance criteria

- [ ] `ingest()` resolves paths to IDs, diffs in WASM, processes only genuine changes
- [ ] `ingestBatch()` accepts pre-formed change tuples
- [ ] Filtered changes flow through the standard pipeline (aggregation → sync → flip → listeners)
- [ ] `dispose()` cleans up without affecting store state
- [ ] Type-safe: `ingest()` accepts only valid `DeepKey<DATA>` paths

---

## WASM-027: Pipeline integration

**Type**: Story | **Points**: 3 | **Priority**: P1
**Depends on**: WASM-026, WASM-014

### Description

Integrate the streaming gateway with the WASM pipeline so that diffed changes flow directly into `processChanges` without redundant work. The diff and pipeline should share the same shadow state.

### Flow

```
gateway.ingest(payload)
  → WASM: diff against shadow state → genuine changes
  → WASM: pipeline (aggregation → sync → flip) on genuine changes
  → return to JS: finalChanges + dispatch plans
  → JS: listener dispatch + applyBatch
```

### Key insight

The diff step and pipeline step share the same WASM memory. No need to serialize changes back to JS between diff and pipeline — they flow directly in Rust.

### Acceptance criteria

- [ ] Diff + pipeline run in a single WASM call (no intermediate JS round trip)
- [ ] Shadow state stays consistent between gateway ingests and direct `setValue` calls
- [ ] Concurrent use: `setValue` and `gateway.ingest` don't corrupt shadow state
- [ ] Performance: diff + pipeline for 500 fields (10% changed) < 50µs

---

## WASM-028: Phase 5 integration tests

**Type**: Story | **Points**: 3 | **Priority**: P1
**Depends on**: WASM-027

### Description

End-to-end tests for the streaming data gateway.

### Test scenarios

1. **No-op batch**: all values unchanged → zero proxy writes, zero re-renders
2. **Partial update**: 100 fields ingested, 10 changed → only 10 applied
3. **Primitive types**: number, boolean, string, null — all diffed correctly
4. **Complex values**: objects always pass through (no false drops)
5. **Pipeline interaction**: genuine changes trigger sync/flip/listeners correctly
6. **Concurrent sources**: gateway.ingest + manual setValue in same tick
7. **High frequency**: 100 ingests with 1% change rate → verify minimal work done
8. **Cleanup**: dispose gateway, verify no memory leaks

### Acceptance criteria

- [ ] All scenarios pass
- [ ] Performance test: 1000 ingests at 10ms intervals, < 1ms total JS thread time
- [ ] Memory stable after 10,000 ingest cycles (no growth)
- [ ] Gateway plays nicely with existing concern evaluation (BoolLogic, validation)
