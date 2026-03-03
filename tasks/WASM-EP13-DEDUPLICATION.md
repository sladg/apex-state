# WASM-EP13: Rust Deduplication & Abstraction

**Epic Key**: WASM-EP13
**Depends On**: EP6 (Pipeline Refactor)
**Status**: ⏳ Ready
**Total Points**: 14pts

---

## Context

`aggregation.rs`, `computation.rs`, `value_logic.rs`, and `bool_logic.rs` share large blocks of structurally identical code. Extracting shared patterns reduces total Rust LOC by ~300 lines and makes adding future registry types trivial. All changes are pure refactors — no behavior change, full test suite must remain green.

---

## Story Breakdown

### WASM-049: Generic `PathIndexedRegistry<T>` trait — 5pts

**Files**: `rust/src/aggregation.rs`, `rust/src/computation.rs`, `rust/src/value_logic.rs`

**Problem**: All three registry structs have identical field layouts and near-identical `new()`, `register()`, `unregister()` implementations — only the stored value type differs:

```rust
pub struct AggregationRegistry {
    aggregations: HashMap<String, Aggregation>,
    source_to_targets: HashMap<String, Vec<String>>,
    condition_path_to_targets: HashMap<String, Vec<String>>,
}

pub struct ComputationRegistry {
    computations: HashMap<String, Computation>,
    source_to_targets: HashMap<String, Vec<String>>, // identical
    condition_path_to_targets: HashMap<String, Vec<String>>, // identical
}
```

**Fix**: Extract a trait `PathIndexedRegistry<T>` with default implementations for the shared index management. Each concrete type only needs to implement how to extract source paths from `T` (e.g., `fn source_paths(entry: &T) -> Vec<String>`).

**Eliminates**: ~200 lines across 2–3 files.

**Acceptance criteria**:
- Trait defined in a shared module (e.g., `rust/src/registry.rs`)
- `AggregationRegistry`, `ComputationRegistry` implement the trait
- All existing tests pass with no changes to test code

---

### WASM-050: Shared condition index update helper — 2pts

**Files**: `rust/src/aggregation.rs`, `rust/src/computation.rs` (and others)

**Problem**: The condition index update pattern repeats verbatim 6+ times:

```rust
if let Some(ref condition) = source.exclude_when {
    for cond_path in condition.extract_paths() {
        self.condition_path_to_targets
            .entry(cond_path)
            .or_default()
            .push(target.clone());
    }
}
```

**Fix**: Extract to a free function in a shared module:

```rust
pub(crate) fn update_condition_index(
    condition: &Option<BoolLogicNode>,
    target: &str,
    index: &mut HashMap<String, Vec<String>>,
) { ... }
```

**Eliminates**: ~30 lines of copy-pasted logic.

**Note**: Best done alongside or after WASM-049.

**Acceptance criteria**:
- Single definition, all callers use it
- No behavior change

---

### WASM-051: Shared `get_affected_targets` helper — 2pts

**Files**: `rust/src/aggregation.rs:138-168`, `rust/src/computation.rs:140-164`

**Problem**: Identical 4-step "find affected targets" algorithm duplicated in both files:
1. Direct match on source path
2. Direct match on condition path
3. Child-of-source match
4. Child-of-condition match

**Fix**: Extract to a free function:

```rust
pub(crate) fn get_affected_from_indices(
    changed_paths: &[String],
    source_index: &HashMap<String, Vec<String>>,
    condition_index: &HashMap<String, Vec<String>>,
) -> Vec<String> { ... }
```

**Eliminates**: ~40 lines of duplicated algorithm.

**Acceptance criteria**:
- Both `AggregationRegistry` and `ComputationRegistry` call the shared function
- Results are identical to current behavior
- All pipeline integration tests pass

---

### WASM-052: Unified `collect_paths` in `shadow.rs` — 2pts

**File**: `rust/src/shadow.rs`

**Problem**: `collect_all_paths()` and `collect_leaves()` are structurally identical recursive traversals. The only difference is whether intermediate Object/Array nodes are included in the result.

**Fix**: Merge into one parameterized function:

```rust
fn collect_paths(
    value: &ValueRepr,
    prefix: &str,
    result: &mut Vec<String>,
    include_intermediate: bool,
) { ... }
```

**Eliminates**: ~25 lines.

**Acceptance criteria**:
- Both call sites use the unified function with the appropriate flag
- Shadow tests pass unchanged

---

### WASM-053: Shadow path resolution guard helper — 1pt

**File**: `rust/src/shadow.rs`

**Problem**: The empty-path guard repeats 3+ times:

```rust
if path.is_empty() {
    Some(&self.root)
} else {
    Self::traverse(&self.root, path.split('.'))
}
```

**Fix**: Extract to a private method:

```rust
fn resolve(&self, path: &str) -> Option<&ValueRepr> {
    if path.is_empty() { Some(&self.root) } else { Self::traverse(&self.root, path.split('.')) }
}
```

**Eliminates**: ~15 lines.

---

### WASM-054: Unified change iteration helper (aggregation/computation) — 2pts

**Files**: `rust/src/aggregation.rs`, `rust/src/computation.rs`

**Problem**: Both `process_aggregation_writes()` and `process_computation_writes()` share the outer loop structure: iterate changes in reverse, match on path, collect results, drain originals. ~30 lines of structural duplication.

**Fix**: Extract a shared iterator/helper that handles the reversal + drain pattern, accepting a closure for the type-specific match logic:

```rust
fn process_write_changes<F>(
    changes: &mut Vec<Change>,
    mut matcher: F,
) -> Vec<Change>
where F: FnMut(&str) -> Option<Change> { ... }
```

**Eliminates**: ~30 lines.

**Acceptance criteria**:
- Both registries call the shared helper
- Output identical to current behavior
- All pipeline tests pass

---

## Files to Modify

- `rust/src/aggregation.rs` — WASM-049, 050, 051, 054
- `rust/src/computation.rs` — WASM-049, 050, 051, 054
- `rust/src/value_logic.rs` — WASM-049 (partial)
- `rust/src/shadow.rs` — WASM-052, 053
- `rust/src/registry.rs` *(new)* — WASM-049: shared trait + helpers

---

## Sizing

| Story | Points | Estimate |
|-------|--------|----------|
| WASM-049 | 5pts | 2–3 days — Generic trait |
| WASM-050 | 2pts | Half day — Condition index helper |
| WASM-051 | 2pts | Half day — Affected targets helper |
| WASM-052 | 2pts | Half day — Unified collect_paths |
| WASM-053 | 1pt | < 2h — Path guard helper |
| WASM-054 | 2pts | Half day — Change iteration helper |
| **Total** | **14pts** | **~5 days** |

---

## Suggested Order

1. **WASM-049** first — establishes the shared module and trait
2. **WASM-050 + WASM-051** immediately after (natural follow-on in same files)
3. **WASM-052 + WASM-053** independently (shadow.rs only, no cross-file deps)
4. **WASM-054** last (requires 049 structure to be in place)

---

## Dependencies

- **Requires**: EP6 (Pipeline Refactor) — ✅ Complete
- **Blocks**: None
- **Note**: These are pure refactors. The test suite is the acceptance gate — all tests must pass without modification.
