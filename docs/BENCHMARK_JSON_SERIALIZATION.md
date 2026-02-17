# JSON Serialization Benchmark: WASM Bridge

**Date:** 2026-02-17
**Node:** v22.22.0 (V8 with optimized JSON)
**Fixture:** `tests/mocks/ecommerce-bench.ts` — 3 depts × 5 products × 4 variants + 10 orders = 1,987 leaf values

## Context

`changesToWasm` and `wasmChangesToJs` in `src/wasm/bridge.ts` serialize every change value through `JSON.stringify`/`JSON.parse`. This benchmark evaluates whether third-party libraries or alternative serialization approaches can improve throughput.

## Approaches Tested

| # | Approach | Description |
|---|---|---|
| 1 | **Current** | `JSON.stringify`/`JSON.parse` per value, `Array.map` |
| 2 | **Optimized JSON** | Type-specific fast paths: `String(n)` for numbers/booleans, skip JSON for primitives. `for`-loop with pre-allocated array |
| 3 | **fast-json-stringify** | Schema-compiled stringify (Fastify). Requires JSON Schema upfront |
| 4 | **Batch JSON** | Single `JSON.stringify(changes)` / `JSON.parse(json)` call |
| 5 | **Binary TypedArray** | Hand-pack into `ArrayBuffer` with type tags. Path strings UTF-8 encoded |
| 6 | **Binary + path interning** | Same as #5 but paths sent as `u32` IDs (pre-populated intern table) |
| 7 | **serde-wasm-bindgen** | Pass JS objects by reference; Rust walks `JsValue` tree directly |

## Results: Third-Party Libraries

**fast-json-stringify** was **3x slower** than native `JSON.stringify`. Reason: the inner `JSON.stringify(c.value)` handles arbitrary user values — no schema is possible for unknown shapes. The schema overhead on the wrapper structure adds cost.

**@jsonjoy.com/json-pack** requires non-standard encoder/decoder API — not a drop-in. Skipped from detailed benchmarking.

**Conclusion:** No third-party library helps for this use case (arbitrary value types, small payloads).

## Results: Optimized JSON Fast Paths

Type-specific fast paths bypass `JSON.stringify`/`JSON.parse` for primitives:

- Numbers/booleans: `String(value)` is **4.7x faster** than `JSON.stringify(value)`
- Parse: char-code detection (`Number()`, direct comparison) vs `JSON.parse`

### Round-Trip: Ecommerce Scenarios (100K iterations)

| Scenario | Changes | Current (ns/op) | Optimized (ns/op) | Speedup |
|---|---|---|---|---|
| S01: Single price edit | 1 (number) | 134 | 44 | **3.0x** |
| S03: Currency propagation | 1 (string) | 107 | 104 | 1.0x |
| S05: Variant restock | 60 (numbers) | 4,598 | 747 | **6.2x** |
| S10: Deep nested mixed | 7 (mixed) | 692 | 528 | **1.3x** |
| S11: Bulk variant prices | 60 (numbers) | 7,118 | 5,839 | **1.2x** |
| S16: Full catalog refresh | 135 (mixed) | 17,232 | 9,067 | **1.9x** |
| FULL: All leaf values | 1,987 | 227,912 | 145,198 | **1.6x** |

String-only payloads see no improvement (expected). Number-heavy batches see the largest gains.

### Value Type Distribution (ecommerce fixture)

| Type | Count | % |
|---|---|---|
| number | 888 | 44.7% |
| string | 762 | 38.3% |
| boolean | 277 | 13.9% |
| array | 60 | 3.0% |

**58.6% of values are number or boolean** — these are the types that benefit from fast paths.

## Results: Binary Serialization

### Round-Trip Comparison (100K iterations)

| Scenario | Current JSON | Opt. JSON | Binary TypedArray | Binary + Interning |
|---|---|---|---|---|
| S01: 1 change | 138 ns | **42 ns** | 595 ns | 233 ns |
| S05: 60 numbers | 4,665 ns | **758 ns** | 11,135 ns | 1,169 ns |
| S10: 7 mixed | 717 ns | **526 ns** | 2,055 ns | 797 ns |
| S16: 135 mixed | 16,929 ns | 8,936 ns | 29,702 ns | **6,971 ns** |
| FULL: 1987 values | 229,040 ns | **144,319 ns** | 623,712 ns | 220,802 ns |

**Binary TypedArray without interning is consistently slower than JSON.** `TextEncoder.encode()` for path strings negates all binary packing benefits. V8's native JSON is too fast for hand-packed binary to compete on the JS side.

**Binary + interning** only wins at 135+ mixed changes where u32 path IDs eliminate string encoding cost. Marginal gain, very high implementation effort.

### Payload Size

| Scenario | Current JSON | Binary + Interning | Savings |
|---|---|---|---|
| S01: 1 change | 97 B | 17 B | 82.5% |
| S05: 60 changes | 6,061 B | 784 B | 87.1% |
| S16: 135 changes | 13,681 B | 1,759 B | 87.1% |
| FULL: 1987 values | 197,387 B | 26,639 B | 86.5% |

Binary interning saves ~87% on wire size (relevant for memory pressure, not latency).

### serde-wasm-bindgen (reference passing)

JS encode cost is ~6-7 ns (just passing a reference). Real cost shifts to Rust walking the `JsValue` tree (~20-50 ns/leaf). Already used in this project for some operations. Would require changing `Change` type to pass `JsValue` directly instead of pre-stringifying.

## Recommendations

| Priority | Approach | Effort | Speedup | Notes |
|---|---|---|---|---|
| **1** | Optimized JSON fast paths | Low (1 file) | 1.3–6.2x | Zero deps, drop-in replacement |
| **2** | serde-wasm-bindgen for Changes | Medium | ~10x encode | Eliminates Rust-side JSON parse too |
| **3** | Binary + interning | Very high | Wins only at 135+ | Overkill for this codebase |
| ✗ | Binary TypedArray (no interning) | High | **Slower than JSON** | Do not pursue |
| ✗ | fast-json-stringify | Low | **3x slower** | Schemas can't handle arbitrary values |
| ✗ | Batch JSON.stringify | Low | **Slower** | Single-call overhead > per-value |
