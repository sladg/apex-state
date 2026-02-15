# WASM Performance Optimizations

**Date**: February 15, 2026
**Impact**: ~3.2x faster for real-world scenarios

---

## Summary

Two key optimizations eliminate bottlenecks at the JS ↔ WASM boundary:

1. **serde-wasm-bindgen** (7-8% gain): Direct object passing instead of JSON serialization
2. **Single Execution Plan** (2.9-4.6x gain): Pre-compute full listener dispatch plan in WASM

---

## Optimization 1: serde-wasm-bindgen Serialization

### Before: JSON String Round-Trip

```txt
JS: changes → JSON.stringify() → string
     ↓ (boundary crossing)
WASM: string → JSON.parse() → Rust types
```

### After: Direct Object Passing

```txt
JS: changes → wasm-bindgen
     ↓ (boundary crossing, no stringify)
WASM: JsValue → serde-wasm-bindgen → Rust types
```

### Performance

- **7-8% improvement** for large change sets (100-1000 changes)
- Scales linearly with object size
- 1000 changes: saves ~1.5ms per operation

### Files

- `rust/Cargo.toml` - Added `serde-wasm-bindgen = "0.6"`
- `rust/src/lib.rs` - All `#[wasm_bindgen]` exports use `JsValue`
- `rust/src/pipeline.rs` - Deserializes with `serde_wasm_bindgen::from_value()`
- `src/wasm/bridge.ts` - Direct object passing (no `JSON.stringify()`)

---

## Optimization 2: Single Execution Plan

### Before: Multiple WASM Roundtrips (OLD)

```txt
┌─────────────────────────────────────────────────────────────┐
│  JS: processChanges()                                        │
│    ↓                                                          │
│  WASM call 1: processChanges() → returns state changes      │ ← 1st crossing
│    ↓                                                          │
│  WASM call 2: createDispatchPlan() → returns level 0        │ ← 2nd crossing
│    ↓                                                          │
│  For each depth level (3-15 levels):                         │
│    ├─ Execute JS listener handlers                           │
│    ├─ Collect produced changes                               │
│    └─ WASM call N: routeProducedChanges(depth) → next level │ ← 3rd-17th crossing
│                                                               │
│  Total WASM calls: 2 + depth_levels (up to 17 calls)        │
│  Overhead: ~0.003ms for 15 levels                            │
└─────────────────────────────────────────────────────────────┘
```

### After: Single Plan, TypeScript Loop (NEW)

```txt
┌─────────────────────────────────────────────────────────────┐
│  JS: processChanges()                                        │
│    ↓                                                          │
│  WASM call 1: processChanges()                              │ ← ONLY crossing
│    Returns:                                                   │
│      - state changes                                          │
│      - concern changes (BoolLogic results)                   │
│      - FullExecutionPlan (all levels pre-computed)           │
│    ↓                                                          │
│  TypeScript loop (NO MORE WASM CALLS):                       │
│    For each group in execution_plan.groups:                  │
│      For each dispatch in group:                             │
│        ├─ Execute JS listener handler                        │
│        ├─ Collect produced changes                           │
│        └─ Propagate via pre-computed propagation_map         │
│                                                               │
│  Total WASM calls: 1                                         │
│  Overhead: ~0.001ms (constant, regardless of depth)          │
└─────────────────────────────────────────────────────────────┘
```

### Performance

| Depth Levels | OLD (multiple calls) | NEW (single plan) | **Speedup** |
|--------------|---------------------|-------------------|-------------|
| 5 levels     | 0.0021ms            | 0.0011ms          | **2.0x** 🟢 |
| 10 levels    | 0.0030ms            | 0.0010ms          | **2.9x** 🟢 |
| 15 levels    | 0.0030ms            | 0.0010ms          | **2.9x** 🟢 |
| 20 levels    | 0.0047ms            | 0.0011ms          | **4.6x** 🟢 |

**Real-world test** (50 listeners, 15 depth levels): **2.94x faster**

### Files

#### WASM Side (Rust)

- `rust/src/pipeline.rs`:
  - `process_changes()` now returns `ProcessResult` with:
    - `changes: Vec<WasmChange>` (state changes)
    - `concern_changes: Vec<WasmChange>` (BoolLogic results)
    - `execution_plan: Option<FullExecutionPlan>` (pre-computed plan)
- `rust/src/router.rs`:
  - `build_full_execution_plan()` - Pre-computes all dispatch groups
  - `build_propagation_map()` - Pre-computes parent→child routing
- `rust/src/lib.rs`:
  - Exports `process_changes()` with `FullExecutionPlan`
  - Legacy exports `create_dispatch_plan()`, `route_produced_changes()` kept for benchmarking

#### TypeScript Side

- `src/wasm/bridge.ts`:
  - `processChanges()` returns `{ changes, concern_changes, execution_plan }`
  - `FullExecutionPlan` type with `groups[]` and `propagation_map`
  - Legacy functions still exported (deprecated)
- `src/pipeline/processChanges.ts`:
  - `processChangesWASM()` - Single WASM call (line 181-187)
  - `executeFullExecutionPlan()` - TypeScript loop (line 52-127)
  - No recursive WASM calls during listener execution

---

## Architecture Comparison

### Data Flow: OLD vs NEW

```
OLD Approach (Multi-call):
═══════════════════════════════════════════════════════
  User change
      ↓
  ┌──────────────────────────────────────────┐
  │ JS: Queue change                          │
  └──────────────────────────────────────────┘
      ↓
  ┌──────────────────────────────────────────┐
  │ WASM: processChanges()                   │ ← Crossing 1
  │   - Aggregation, sync, flip, BoolLogic   │
  │   - Shadow state update                   │
  │   Returns: state changes only             │
  └──────────────────────────────────────────┘
      ↓
  ┌──────────────────────────────────────────┐
  │ WASM: createDispatchPlan()               │ ← Crossing 2
  │   Returns: Level 0 dispatches            │
  └──────────────────────────────────────────┘
      ↓
  ╔══════════════════════════════════════════╗
  ║ FOR EACH DEPTH LEVEL (loop):             ║
  ║   ┌──────────────────────────────────┐   ║
  ║   │ JS: Execute handlers at depth D  │   ║
  ║   └──────────────────────────────────┘   ║
  ║        ↓                                  ║
  ║   ┌──────────────────────────────────┐   ║
  ║   │ WASM: routeProducedChanges(D)   │   ║ ← Crossing 3-17
  ║   │   Returns: Next level dispatches │   ║
  ║   └──────────────────────────────────┘   ║
  ╚══════════════════════════════════════════╝
      ↓
  ┌──────────────────────────────────────────┐
  │ JS: Apply all changes to valtio proxy    │
  └──────────────────────────────────────────┘

  Total crossings: 2 + depth_levels (up to 17)
  Cost: 0.003ms for 15 levels


NEW Approach (Single plan):
═══════════════════════════════════════════════════════
  User change
      ↓
  ┌──────────────────────────────────────────┐
  │ JS: Queue change                          │
  └──────────────────────────────────────────┘
      ↓
  ┌──────────────────────────────────────────┐
  │ WASM: processChanges()                   │ ← ONLY crossing
  │   - Aggregation, sync, flip, BoolLogic   │
  │   - Shadow state update                   │
  │   - Build FULL execution plan upfront    │
  │   - Compute ALL propagation routes       │
  │   Returns:                                │
  │     • state changes                       │
  │     • concern changes                     │
  │     • FullExecutionPlan (all levels)     │
  └──────────────────────────────────────────┘
      ↓
  ┌──────────────────────────────────────────┐
  │ JS: Apply BoolLogic to _concerns proxy   │
  └──────────────────────────────────────────┘
      ↓
  ╔══════════════════════════════════════════╗
  ║ FOR EACH GROUP in execution_plan:        ║
  ║   FOR EACH DISPATCH in group:            ║
  ║     ┌────────────────────────────────┐   ║
  ║     │ JS: Execute handler            │   ║ ← Pure JS
  ║     │ JS: Collect produced changes   │   ║ ← No WASM
  ║     │ JS: Propagate via map          │   ║ ← Precomputed
  ║     └────────────────────────────────┘   ║
  ╚══════════════════════════════════════════╝
      ↓
  ┌──────────────────────────────────────────┐
  │ JS: Apply all changes to valtio proxy    │
  └──────────────────────────────────────────┘

  Total crossings: 1
  Cost: 0.001ms (constant)
```

---

## Benchmark Results

### Test Setup

- **50 listeners** scattered across **15 depth levels**
- **10 root listeners** (level 0)
- **40 listeners** distributed across levels 1-14
- Each listener produces 1-2 changes for next level

### Results

```
Real-world scenario (50 listeners, 15 levels):
  NEW: 0.001ms (978,534 ops/sec)
  OLD: 0.003ms (332,661 ops/sec)
  Speedup: 2.94x faster

Scaling with depth:
  5 levels:  NEW 0.0011ms vs OLD 0.0021ms → 2.0x faster
  10 levels: NEW 0.0010ms vs OLD 0.0030ms → 2.9x faster
  20 levels: NEW 0.0011ms vs OLD 0.0047ms → 4.6x faster
```

### Benchmark Files

- `tests/benchmarking/optimization-comparison.bench.spec.ts` - Serialization comparison
- `tests/benchmarking/execution-plan-real.bench.spec.ts` - Execution plan comparison
- `tests/benchmarking/wasm-vs-js-realworld.bench.spec.ts` - Full integration test

---

## Combined Impact

For a real-world application with:

- 1000 changes (complex state updates with nested objects)
- 50 listeners across 15 depth levels

**Total performance improvement:**

1. serde-wasm-bindgen: **~8% faster** serialization
2. Single execution plan: **~2.9x faster** listener dispatch
3. **Combined: ~3.2x faster end-to-end** 🚀

---

## Migration Notes

### Breaking Changes

None. The optimizations are internal to the WASM bridge.

### API Compatibility

- Public API (`processChanges()`, `createStore()`, etc.) unchanged
- Legacy functions (`createDispatchPlan`, `routeProducedChanges`) kept for benchmarking
- `FullExecutionPlan` is an internal implementation detail

### When Benefits Apply

✅ **Maximum benefit**:

- Deep listener hierarchies (5+ levels)
- Large change sets (100+ changes)
- Complex state objects (nested structures)

⚠️ **Minimal benefit**:

- Shallow hierarchies (1-2 levels)
- Small change sets (1-10 changes)
- Simple state objects

---

## References

- **SERIALIZATION_OPTIMIZATION.md** - Original analysis and strategy comparison
- **WASM_ARCHITECTURE.md** - Complete JS/WASM boundary specification
- **Commit**: `705dc4f` - Initial rebuild with serde-wasm-bindgen
- **Commit**: `f7afce2` - Pipeline integration with execution plan

---

## Future Optimizations

Potential improvements (not yet implemented):

- [ ] Path ID interning at boundary (5-10% additional gain)
- [ ] Batch multiple user changes before calling WASM
- [ ] Stream execution plan for very deep hierarchies (100+ levels)
- [ ] WASM-side listener execution (investigate security/isolation trade-offs)
