# WASM-EP11: Rust Performance — Huge Objects

**Epic Key**: WASM-EP11
**Depends On**: EP6 (Pipeline Refactor)
**Status**: 🔄 In Progress (2/4 complete)
**Total Points**: 16pts

---

## Context

When state contains large nested objects (10k+ nodes), several Rust operations become bottlenecks due to unnecessary cloning and per-leaf `String` allocations. These issues do not affect correctness but cause significant memory pressure and CPU overhead at scale.

---

## Story Breakdown

### WASM-041: Remove subtree clone in `shadow.set()` — 3pts ✅ 2026-03-04

**File**: `rust/src/shadow.rs`

**Problem**: When replacing an Object value, the entire old subtree is cloned just to collect `removed_paths`:

```rust
let old_value = self.get(path).unwrap().clone(); // deep clone of entire subtree
Self::collect_all_paths(&old_value, path, &mut removed);
```

**Fix**: Refactor `collect_all_paths()` to accept `&ValueRepr` (reference, not owned). Reorder so paths are collected from the existing tree *before* the mutation happens. Eliminates one O(|subtree|) allocation + copy on every object replacement.

**Acceptance criteria**:
- `shadow.set()` no longer clones the old subtree
- All existing `shadow.test.ts` tests pass unchanged
- No behavior change — only allocations reduced

---

### WASM-042: `affected_paths()` returns `Vec<u32>` (intern-first) — 5pts ✅ 2026-03-04

**Files**: `rust/src/shadow.rs`, `rust/src/pipeline.rs`, `rust/src/router.rs`, `rust/src/bool_logic.rs`

**Problem**: Every leaf path in a subtree is heap-allocated as a new `String`:

```rust
result.push(child_path.clone()); // one String allocation per leaf
```

For a 10k-node subtree dispatch, this is 10k heap allocations.

**Fix**: Intern all shadow state paths at write time inside `set()`. Change `affected_paths()` to return `Vec<u32>` (path IDs). All downstream consumers (BoolLogic evaluation, topic routing) already work with `u32` IDs after interning.

**Requires**:
- `ShadowState` gains a reference to `InternTable` (or path IDs are passed in and stored alongside values)
- All callers of `affected_paths()` updated to accept `Vec<u32>`

**Acceptance criteria**:
- `affected_paths()` returns `Vec<u32>` — no `String` allocated per leaf
- All callers compile and pass tests
- Benchmark shows reduced allocation count for large subtree dispatch

---

### WASM-043: Shadow state object keys stored as `u32` — 5pts

**File**: `rust/src/shadow.rs`

**Problem**: Object nodes use `HashMap<String, ValueRepr>` where keys are owned `String`s. For arrays of records sharing the same schema (e.g., 1000 users each with `name`, `email`, `id`), each instance stores separate `String` copies of the same field names.

**Fix**: Change `HashMap<String, ValueRepr>` → `HashMap<u32, ValueRepr>` for Object nodes. Field names are interned on write through `InternTable`. Reduces per-key overhead from ~40 bytes to ~8 bytes.

**Note**: Best done after WASM-042 (shared intern infrastructure).

**Acceptance criteria**:
- Object nodes use `u32` keys throughout
- `traverse()` and `get()` use interned field segments
- All shadow tests pass
- Memory usage measurably lower for wide objects (benchmark or snapshot test)

---

### WASM-044: Hash-based object diff to skip no-op object sets — 3pts

**File**: `rust/src/diff.rs`

**Problem**: Objects and arrays always pass through the diff check — there is no deep comparison:

```rust
(Some(ValueRepr::Object(_)), ValueRepr::Object(_)) => true, // always "different"
```

Setting an unchanged object still triggers the full listener fan-out.

**Fix**: Add a structural hash field to each `ValueRepr::Object` and `ValueRepr::Array` node, computed at write time (rolling hash over children). Diff compares hashes first — if equal, the change is a no-op and is filtered before listener dispatch.

**Acceptance criteria**:
- Setting an identical object no longer triggers listeners
- Hash is computed incrementally (not full traversal per check)
- Existing diff tests pass; add tests for: unchanged object, changed object, changed nested value

---

## Files to Modify

- `rust/src/shadow.rs` — WASM-041, 042, 043
- `rust/src/diff.rs` — WASM-044
- `rust/src/pipeline.rs` — WASM-042 (caller updates)
- `rust/src/router.rs` — WASM-042 (caller updates)
- `rust/src/bool_logic.rs` — WASM-042 (caller updates)

---

## Sizing

| Story | Points | Estimate |
|-------|--------|----------|
| WASM-041 | 3pts | 1 day — Shadow set refactor |
| WASM-042 | 5pts | 2–3 days — Intern-first affected_paths |
| WASM-043 | 5pts | 2–3 days — Interned object keys |
| WASM-044 | 3pts | 1 day — Hash-based diff |
| **Total** | **16pts** | **~1 week** |

---

## Dependencies

- **Requires**: EP6 (Pipeline Refactor) — ✅ Complete
- **WASM-043 should follow WASM-042** — share intern infrastructure
- **Blocks**: None
