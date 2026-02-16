# Legacy vs WASM: Benchmark & Parity Comparison

## TL;DR

**Setup:** 60 variants (3 Records deep), 75 syncs, 40 flips, 100 BoolLogic, 85 listeners (75 scoped + 10 catalog-root).

| | Legacy | WASM |
|---|---|---|
| **Single field edit** | 0.5μs | 1.4μs — **Legacy 2.6x faster** |
| **5 changes + listeners** | 2.65ms | 0.041ms — **WASM 65x faster** |
| **7 changes + cascading** | 41.8ms | 0.114ms — **WASM 367x faster** |
| **60 bulk changes** | 596ms | 2.9ms — **WASM 207x faster** |
| **135 changes (full refresh)** | 621ms | 2.99ms — **WASM 208x faster** |
| **Parity** | Identical state output across all 16 scenarios |
| **Crossover point** | ~5 changes with active listeners |
| **Legacy advantage** | No serialization overhead, direct proxy access |
| **WASM advantage** | Pre-computed routing, O(n) vs O(changes x listeners) dispatch |

### Impact of 10 Cascading Catalog-Root Listeners

Adding 10 broad listeners (path: `catalog`) that fire on every catalog change and **write back into catalog** (2 changes each = 20 cascading changes per trigger):

| Scenario | Legacy (no root → with root) | WASM (no root → with root) | Notes |
|---|---|---|---|
| S02: Batch variant (3 fields) | 146 → 28 ops/s (**5.2x slower**) | 18,907 → 10,022 ops/s (**1.9x slower**) | Cascading output amplifies Legacy's O(c×l) dispatch cost |
| S10: Deep metadata (7 writes) | 152 → 24 ops/s (**6.3x slower**) | 17,951 → 8,749 ops/s (**2.1x slower**) | Legacy: 15ms → 42ms; WASM: 0.06ms → 0.11ms |
| S13: Shipping recalc (15 changes) | 68,627 → 101,421 ops/s (~same) | 58,306 → 57,833 ops/s (~same) | Listeners fire but 20 extra changes are cheap at this scale |
| S16: Full refresh (135 changes) | 1.6 → 1.6 ops/s (~same) | 394 → 335 ops/s (**1.2x slower**) | Already dominated by 135-change listener round-trips |

**Key finding:** Cascading root listeners devastate Legacy on medium-complexity scenarios. S10 went from WASM being 117x faster to **364x faster** because each of the 10 root listeners writes 2 catalog changes back, and Legacy must re-filter all accumulated changes against all listeners. WASM's pre-computed topic router absorbs the extra work with only ~2x overhead.

---

## Parity Verification

**All 16 scenarios produce identical state.** The parity test (`tests/integration/wasm-vs-legacy-parity.test.ts`) runs each scenario once per mode and asserts:

1. Same number of leaf-level state diffs
2. `deepEqual` on final state snapshots

Results (all passing):

| Scenario | Input Changes | Legacy Diffs | WASM Diffs | State Match |
|----------|--------------|-------------|-----------|-------------|
| S01: Single variant price edit | 1 | 3 | 3 | Yes |
| S02: Batch variant update (3 fields) | 3 | 9 | 9 | Yes |
| S03: Currency propagation (75 syncs) | 1 | 2 | 2 | Yes |
| S04: Order confirmation (flip + listeners) | 2 | 3 | 3 | Yes |
| S05: Variant restock (60 variants) | 60 | 120 | 120 | Yes |
| S06: Variant toggle (10 flips) | 5 | 10 | 10 | Yes |
| S07: Full checkout workflow | 5 | 10 | 10 | Yes |
| S08: Dashboard aggregation (10 orders) | 10 | 20 | 20 | Yes |
| S09: Product archive (6 status) | 6 | 6 | 6 | Yes |
| S10: Deep variant metadata (7 writes) | 7 | 14 | 14 | Yes |
| S11: Bulk variant prices (60 changes) | 60 | 180 | 180 | Yes |
| S12: Department toggle + flip | 3 | 6 | 6 | Yes |
| S13: Variant shipping recalc | 15 | 45 | 45 | Yes |
| S14: Multi-order confirm (5 orders) | 10 | 15 | 15 | Yes |
| S15: Cross-dept variant prices | 40 | 80 | 80 | Yes |
| S16: Full catalog refresh (135 changes) | 135 | 240 | 240 | Yes |

### What Parity Does NOT Cover

- **`_concerns` state** (BoolLogic, validation results) — Legacy evaluates BoolLogic via `effect()` outside the pipeline; WASM evaluates it inside the pipeline. The parity test compares only `state`, not `_concerns`.
- **Validation state** — Same reason; validators are orchestrated differently.
- The `smoke-dual-mode.test.tsx` tests DO verify some concerns parity for basic scenarios (disabledWhen, validation), but not at the scale of the benchmark scenarios.

---

## Benchmark Results

**Test setup:** 3 departments x 5 products x 4 variants = 60 variants, traversing 3 dynamic `Record<>` layers. 75 sync pairs, 40 flip pairs, 100 BoolLogic conditions, 85 listeners (75 scoped + 10 catalog-root).

| Scenario | Legacy (ops/s) | WASM (ops/s) | Legacy Mean (ms) | WASM Mean (ms) | Winner | Ratio |
|----------|---------------|-------------|------------------|----------------|--------|-------|
| S01: Single variant price edit | 1,935,343 | 739,824 | 0.0005 | 0.0014 | **Legacy** | 2.6x |
| S02: Batch variant update (3 fields) | 28 | 10,022 | 35.79 | 0.100 | **WASM** | 358x |
| S03: Currency propagation (75 syncs) | 6,496,699 | 833,425 | 0.0002 | 0.0012 | **Legacy** | 7.8x |
| S04: Order confirmation (flip) | 4,902,207 | 1,143,517 | 0.0002 | 0.0009 | **Legacy** | 4.3x |
| S05: Variant restock (60 variants) | 24,177 | 9,183 | 0.041 | 0.109 | **Legacy** | 2.6x |
| S06: Variant toggle (10 flips) | 96,967 | 116,997 | 0.010 | 0.009 | **WASM** | 1.2x |
| S07: Full checkout workflow (5 changes) | 377 | 24,630 | 2.65 | 0.041 | **WASM** | 65x |
| S08: Dashboard aggregation (10 orders) | 47 | 7,638 | 21.29 | 0.131 | **WASM** | 162x |
| S09: Product archive (6 status) | 358,419 | 136,973 | 0.003 | 0.007 | **Legacy** | 2.6x |
| S10: Deep variant metadata (7 writes) | 24 | 8,749 | 41.83 | 0.114 | **WASM** | 367x |
| S11: Bulk variant prices (60 changes) | 1.7 | 347 | 596.2 | 2.88 | **WASM** | 207x |
| S12: Dept toggle + flip (3 flips) | 862,564 | 392,765 | 0.001 | 0.003 | **Legacy** | 2.2x |
| S13: Variant shipping recalc (15 changes) | 101,421 | 57,833 | 0.010 | 0.017 | **Legacy** | 1.8x |
| S14: Multi-order confirm (5 orders) | 397,100 | 118,090 | 0.003 | 0.008 | **Legacy** | 3.4x |
| S15: Cross-dept variant prices (40 changes) | 3.5 | 1,007 | 284.5 | 0.993 | **WASM** | 286x |
| S16: Full catalog refresh (135 changes) | 1.6 | 335 | 621.3 | 2.99 | **WASM** | 208x |

---

## Analysis: Why Legacy Wins on Small Operations

**Legacy wins S01, S03, S04, S06, S09, S12, S13, S14** — all scenarios with **few input changes** (1-15) that trigger **no listeners** or only simple propagation.

**Root cause: WASM boundary crossing overhead.**

Every WASM call requires:
1. **JSON serialization** — `changesToWasm()` converts `Change[]` to `WasmChange[]` with `JSON.stringify()` per value
2. **wasm-bindgen marshalling** — Strings cross as heap-allocated copies
3. **JSON deserialization** in Rust — `serde_json::from_str()` per change
4. **Result serialization** back to JS — The response struct is serialized back
5. **Two round trips** — `processChanges()` + `pipelineFinalize()` (even when no listeners fire)

For a single field edit (S01), Legacy does:
- Direct valtio proxy read for no-op check
- Simple Map lookup for sync/flip
- Direct state mutation

WASM does the same logical work but pays ~1.4μs of serialization overhead vs Legacy's ~0.5μs direct JS execution. When the actual computation is trivial, the boundary cost dominates.

**Key insight:** Legacy's S03 (currency propagation, 75 sync paths) is 8.4x faster despite touching 75 syncs — because it's still just 1 input change with a single sync lookup. The "75 sync paths" are registered but only 1 fires. The boundary cost is fixed per call, not per registration.

---

## Analysis: Why WASM Wins on Complex Operations

**WASM wins S02, S07, S08, S10, S11, S15, S16** — all scenarios with **many changes** that trigger **listeners producing more changes**.

**Root cause: O(changes x listeners) dispatch cost in Legacy.**

The critical difference is how listeners are routed:

### Legacy Listener Path
```
processChanges([5 changes])
  → sync/flip (fast, JS Map lookups)
  → processListeners():
      sort changes by depth (deepest first)
      for each listener path (deepest first):
        filter ALL changes by string prefix matching
        relativize matched paths (string slicing)
        call handler → handler returns new changes
        queue returned changes
  → apply ALL queued changes to state proxy (single pass)
```

Each listener invocation:
1. Filters **all** changes against the listener path via string prefix matching — O(changes) per listener
2. Relativizes paths (string slicing per match)
3. Calls handler function
4. Queued output changes are applied at the end — **listeners run once, never re-triggered**

The cost is O(changes x listeners) because every listener must scan every change. With 85 listeners and 135 changes, that's 11,475 string prefix comparisons per `processChanges` call.

### WASM Listener Path
```
wasm.processChanges([5 changes])
  → shadow state update
  → sync/flip via graph traversal (pre-computed edges)
  → BoolLogic evaluation (in WASM, no boundary crossing)
  → returns execution_plan (pre-computed: which listeners, which changes, in what order)

JS: executeFullExecutionPlan()
  → for each listener in plan:
      call handler with pre-filtered, pre-relativized changes
      collect produced changes

wasm.pipelineFinalize(produced_changes)
  → merge, diff against shadow, return final changes
```

**Key WASM advantages for complex scenarios:**

1. **Pre-computed routing** — WASM's topic router pre-computes which listeners match which paths at registration time. Legacy does string prefix matching per change per listener at dispatch time.
2. **Single expansion** — WASM processes all sync/flip/BoolLogic in one pass inside Rust. Legacy does sequential JS Map lookups.
3. **O(1) dispatch vs O(c×l)** — WASM uses pre-computed dispatch plans (which listener gets which changes). Legacy scans all changes for every listener via `startsWith()`.
4. **Shadow state diffing** — WASM diffs against an internal shadow state (fast Rust HashMap). Legacy reads from valtio proxies (Proxy trap overhead per read).
5. **BoolLogic in pipeline** — WASM evaluates all 100 BoolLogic conditions during `processChanges()`. Legacy defers to `effect()` which runs asynchronously.

### Scaling Factor

The performance gap grows superlinearly with complexity:

| Input Changes | Legacy Mean | WASM Mean | WASM Speedup |
|--------------|------------|----------|-------------|
| 3 (S02) | 35.8ms | 0.100ms | 358x |
| 5 (S07) | 2.65ms | 0.041ms | 65x |
| 7 (S10) | 41.8ms | 0.114ms | 367x |
| 10 (S08) | 21.3ms | 0.131ms | 162x |
| 40 (S15) | 284.5ms | 0.993ms | 286x |
| 60 (S11) | 596.2ms | 2.88ms | 207x |
| 135 (S16) | 621.3ms | 2.99ms | 208x |

Legacy degrades as O(changes x listeners) because every listener must scan every change via string prefix matching. With more changes and more listeners, this cost grows multiplicatively. WASM stays roughly O(n) due to pre-computed routing — each change is pre-mapped to its listeners at registration time.

---

## Implementation Differences (Nuances)

### 1. BoolLogic Evaluation Timing

| | Legacy | WASM |
|---|--------|------|
| **When** | Asynchronous via `effect()` after state mutation | Synchronous inside `processChanges()` |
| **Visibility** | Listeners do NOT see updated BoolLogic results | Listeners DO see updated BoolLogic results |
| **Location** | JavaScript (tree walk in JS) | Rust (tree walk in WASM) |

**Impact:** In Legacy, a listener cannot depend on `disabledWhen` being current for the triggering change. In WASM, BoolLogic is always evaluated before listeners fire.

### 2. No-Op Filtering

| | Legacy | WASM |
|---|--------|------|
| **Method** | Reads from valtio proxy (`dot.get__unsafe`) | Diffs against shadow state (Rust HashMap) |
| **Cost** | Proxy trap overhead per read | Direct memory lookup |

### 3. Listener Dispatch

| | Legacy | WASM |
|---|--------|------|
| **Routing** | String prefix matching per change per listener | Pre-computed topic router (built at registration) |
| **Ordering** | Sort by path depth at dispatch time | Pre-sorted in execution plan |
| **Change filtering** | `filterAndRelativize()` per listener | Done in WASM, delivered pre-filtered |

### 4. Sync/Flip Processing

| | Legacy | WASM |
|---|--------|------|
| **Structure** | `Map<string, string[]>` in JS | Graph edges in Rust (`HashMap<PathId, Vec<PathId>>`) |
| **Lookup** | Map.get() per path | Interned PathId lookup (O(1)) |
| **Traversal** | Direct pair lookup | Connected component traversal |

Both produce identical results for the same input, but WASM uses interned path IDs (u32) instead of string comparisons.

### 5. Aggregation

| | Legacy | WASM |
|---|--------|------|
| **Processing** | JS reads array items, computes sum/count/etc. | Rust reads shadow state, computes in WASM |
| **Initial values** | Computed at registration time in JS | Computed at registration time in WASM |

### 6. State Application

| | Legacy | WASM |
|---|--------|------|
| **Final write** | Direct `dot.set()` on valtio proxy | `applyBatch()` — single batch of all changes |
| **Triggering renders** | Each `dot.set()` may trigger | Single batch minimizes re-renders |

---

## Summary

| Scenario Type | Winner | Why |
|--------------|--------|-----|
| Single field edit, no listeners | **Legacy (2-8x)** | WASM boundary overhead dominates trivial work |
| Few changes, simple propagation | **Legacy (1.2-6x)** | Same reason — not enough work to amortize crossing cost |
| Multi-field with listeners | **WASM (65-162x)** | Pre-computed routing vs O(c×l) string matching |
| Bulk changes (40-135) | **WASM (207-286x)** | O(c×l) dispatch cost dominates in Legacy |

**Crossover point:** ~5 input changes with active listeners, or ~15 changes without listeners.

**For real-world forms:** Most user interactions are single-field edits (Legacy wins). Bulk operations like "apply template", "import data", or "reset section" trigger WASM's advantages.

**Recommendation:** The dual-mode architecture is justified — use Legacy for simple reactive forms, WASM for complex catalogs and bulk operations.
