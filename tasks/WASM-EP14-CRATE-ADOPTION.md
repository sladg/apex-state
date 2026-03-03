# WASM-EP14: Rust Third-Party Crate Adoption

**Epic Key**: WASM-EP14
**Depends On**: EP6 (Pipeline Refactor)
**Status**: ⏳ Ready
**Total Points**: 8pts

---

## Context

Several parts of the Rust codebase implement functionality that battle-tested crates already provide. Adopting these crates reduces our custom code, improves correctness, and in most cases improves performance. All candidates are WASM-compatible and actively maintained.

WASM binary size is tracked per crate — the budget target is <15KB gzipped per addition.

---

## Story Breakdown

### WASM-055: `ahash` — replace `std::HashMap` everywhere — 1pt

**Crate**: [`ahash`](https://crates.io/crates/ahash) | WASM size: ~1KB gzipped

**Problem**: `std::HashMap` uses SipHash by default, which is DoS-resistant but ~20–40% slower than non-cryptographic hashers. All our keys are internal (path strings, `u32` IDs) — no external input risk.

**Fix**: Add `type HashMap<K, V> = ahash::AHashMap<K, V>` and `type HashSet<T> = ahash::AHashSet<T>` in a `prelude.rs` or at the top of each file. Replace all `std::collections::HashMap` and `HashSet` imports.

**Files affected**: All `.rs` files using `HashMap` / `HashSet` (`pipeline.rs`, `shadow.rs`, `graphs.rs`, `router.rs`, `aggregation.rs`, `computation.rs`, `rev_index.rs`, etc.)

**Acceptance criteria**:
- No `std::collections::HashMap` or `HashSet` remaining
- All tests pass
- No behavior change

---

### WASM-056: `string-interner` — replace `interning.rs` — 2pts

**Crate**: [`string-interner`](https://crates.io/crates/string-interner) | WASM size: ~8KB gzipped

**Problem**: `rust/src/interning.rs` (~100 lines) implements a bidirectional `String ↔ u32` map. `string-interner` does exactly this — with arena allocation, zero unsafe code, O(1) both directions, and tested edge cases we haven't covered.

**Fix**: Replace `InternTable` with `string_interner::DefaultStringInterner`. Update `intern(path) -> u32` call sites to use the crate's API (`interner.get_or_intern(path) -> Symbol`). `Symbol` is a newtype over `u32`, transparent at runtime.

**Acceptance criteria**:
- `rust/src/interning.rs` deleted
- All call sites updated
- All interning tests pass (or are removed if now covered by crate's own test suite)

---

### WASM-057: `smallvec` — hot-path `Vec<u32>` collections — 2pts

**Crate**: [`smallvec`](https://crates.io/crates/smallvec) | WASM size: ~3KB gzipped

**Problem**: Many hot-path collections hold 1–4 items in the common case but use heap-allocated `Vec`. Examples: affected entities per path in `RevIndex`, subscriber lists per topic in `TopicRouter`, component node lists in `graphs.rs`.

**Fix**: Replace `Vec<u32>` with `SmallVec<[u32; 4]>` in the highest-frequency collections. The API is identical to `Vec` — no caller changes needed beyond the type.

**Target locations**:
- `rev_index.rs`: `path_to_entities` values
- `router.rs`: subscriber lists per topic
- `graphs.rs`: component node sets (if node count is typically small)

**Acceptance criteria**:
- Common case (≤4 items) allocates zero heap
- All tests pass
- Benchmark shows reduced allocation count

---

### WASM-058: `slab` — replace `InternTable` reverse vec — 1pt

**Crate**: [`slab`](https://crates.io/crates/slab) | WASM size: ~2KB gzipped

**Problem**: `InternTable` uses `Vec<String>` for the `id → string` reverse lookup. Stable slot allocation (for safe `PathId` reuse if we ever support unregistration) requires manual free-list management.

**Fix**: Replace `Vec<String>` with `Slab<String>`. `Slab` provides stable integer keys, O(1) insert/get/remove, and handles slot reuse automatically.

**Note**: If WASM-056 (`string-interner`) is adopted, this story may be redundant — evaluate during implementation.

**Acceptance criteria**:
- `InternTable` (or its replacement) uses `Slab` for reverse lookup
- All interning tests pass

---

### WASM-059: Evaluate `petgraph` for replacing `graphs.rs` — 1pt

**Crate**: [`petgraph`](https://crates.io/crates/petgraph) | WASM size: ~30KB gzipped

**This is an evaluation story, not implementation.**

**Problem**: `rust/src/graphs.rs` (~200–300 lines) implements connected components for sync/flip groups. `petgraph` has `algo::connected_components()` and full BFS/DFS support built in.

**Evaluation criteria**:
1. Does `petgraph` expose O(1) "which component does node X belong to?" — or does it require a full traversal?
2. Is the 30KB WASM size cost justified given graphs.rs is ~300 lines?
3. Can we wrap it cleanly without re-implementing the O(1) lookup index on top?

**Output**: A short decision note added to this story (adopt / do not adopt + reasoning). If adopted, create a follow-up implementation story.

---

### WASM-060: Evaluate `indexmap` for `TopicRouter` ordering — 1pt

**Crate**: [`indexmap`](https://crates.io/crates/indexmap) | WASM size: ~10KB gzipped

**This is an evaluation story, not implementation.**

**Problem**: WASM-045 (EP12) proposes pre-sorting topics at registration using `BTreeMap`. `IndexMap` is an alternative — preserves insertion order with O(1) lookups, avoiding a separate sort if registration order naturally produces the right dispatch order.

**Evaluation criteria**:
1. Is topic registration order guaranteed to be deepest-first in all call paths?
2. Does `IndexMap`'s insertion-order guarantee satisfy dispatch ordering, or does depth sorting still need to happen explicitly?
3. Size vs. benefit: 10KB for what might be a 15-line `BTreeMap` solution?

**Output**: A short decision note (adopt / do not adopt + reasoning). If adopted, fold into WASM-045 implementation.

---

## Suggested Order

1. **WASM-055** (`ahash`) — zero risk, immediate, do first
2. **WASM-057** (`smallvec`) — independent, low risk
3. **WASM-056** (`string-interner`) — deletes a whole file, do before WASM-058
4. **WASM-058** (`slab`) — may be redundant after 056, evaluate then
5. **WASM-059 + WASM-060** — evaluations, can run in parallel

---

## Cargo.toml Additions

```toml
[dependencies]
ahash = { version = "0.8", default-features = false }
string-interner = { version = "0.17", default-features = false, features = ["backends"] }
smallvec = { version = "1.13", features = ["union"] }
slab = "0.4"

# Evaluation only — do not add until WASM-059/060 decisions made
# petgraph = { version = "0.6", default-features = false }
# indexmap = "2"
```

---

## Files to Modify

- `rust/Cargo.toml` — add dependencies
- `rust/src/interning.rs` — **deleted** (WASM-056)
- `rust/src/pipeline.rs`, `shadow.rs`, `graphs.rs`, `router.rs`, `aggregation.rs`, `computation.rs`, `rev_index.rs` — HashMap/HashSet imports (WASM-055)
- `rust/src/router.rs`, `rev_index.rs`, `graphs.rs` — SmallVec adoption (WASM-057)

---

## Sizing

| Story | Points | Estimate |
|-------|--------|----------|
| WASM-055 | 1pt | < 2h — ahash swap |
| WASM-056 | 2pts | Half day — replace interning.rs |
| WASM-057 | 2pts | Half day — smallvec hot paths |
| WASM-058 | 1pt | < 2h — slab reverse vec |
| WASM-059 | 1pt | < 2h — petgraph evaluation |
| WASM-060 | 1pt | < 2h — indexmap evaluation |
| **Total** | **8pts** | **~2 days** |

---

## Dependencies

- **Requires**: EP6 (Pipeline Refactor) — ✅ Complete
- **WASM-058 may be superseded by WASM-056** — check during implementation
- **WASM-059/060 are evaluations** — produce decision notes, not code
- **Blocks**: None
