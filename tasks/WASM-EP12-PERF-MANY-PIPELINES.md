# WASM-EP12: Rust Performance — Many Pipelines

**Epic Key**: WASM-EP12
**Depends On**: EP6 (Pipeline Refactor)
**Status**: ⏳ Ready
**Total Points**: 6pts

---

## Context

When many `createStore()` instances exist or many listeners/BoolLogics are registered, per-`processChanges()` overhead accumulates in routing and allocation. These are lower-effort, high-leverage fixes — all four stories are independent.

---

## Story Breakdown

### WASM-045: Pre-sort `TopicRouter` at registration time — 2pts

**File**: `rust/src/router.rs`

**Problem**: Topic dispatch re-sorts matched topics by depth on every `processChanges()` call — O(m log m) per call, where m = number of matched topics.

**Fix**: Maintain a `BTreeMap<(depth_desc, topic_id), Vec<u32>>` at registration time, keeping topics in deepest-first order. Dispatch iterates the map in pre-sorted order with zero sort cost at runtime.

**Acceptance criteria**:

- No sort operation during `processChanges()`
- Dispatch order is identical to current behavior (deepest-first)
- All listener dispatch tests pass

---

### WASM-046: Arc-based `RevIndex` — remove `HashSet` clone at registration — 2pts

**File**: `rust/src/rev_index.rs`

**Problem**: `entity_to_paths` clones the input `HashSet` on every `insert()`:

```rust
self.entity_to_paths.insert(entity_id, path_ids.clone());
```

**Fix**: Switch to `Arc<HashSet<u32>>` for shared ownership between `path_to_entities` and `entity_to_paths`. The clone becomes a cheap `Arc::clone()` (reference count increment only).

**Acceptance criteria**:

- No `HashSet::clone()` during registration
- Cleanup (`unregister`) still works correctly — `Arc` ownership drops when both sides release
- All rev_index tests pass

---

### WASM-047: Configurable `PipelineContext` capacity hints — 1pt

**File**: `rust/src/pipeline.rs`

**Problem**: Pre-allocation sizes are hardcoded regardless of registration count:

```rust
changes: Vec::with_capacity(64),
affected_bool_logic: HashSet::with_capacity(16),
```

For large pipelines (100+ BoolLogics), these sizes are too small and trigger reallocation. For tiny pipelines, they over-allocate.

**Fix**: After `initPipeline()` completes all registrations, compute capacity hints from actual registration counts (e.g., `bool_logic_count * 2`). Pass hints into `PipelineContext::new(hints)`.

**Acceptance criteria**:

- Capacity hints derived from registered entity counts
- No behavior change
- Benchmark shows fewer reallocations for large pipelines

---

### WASM-048: Graph component merge — drain `HashSet` instead of tmp `Vec` — 1pt

**File**: `rust/src/graphs.rs`

**Problem**: Component merge collects into a temporary `Vec` just to iterate:

```rust
let smaller_nodes: Vec<u32> = self.component_to_nodes[&smaller_comp]
    .iter()
    .copied()
    .collect(); // unnecessary allocation
```

**Fix**: Drain the `HashSet` directly from `component_to_nodes` using `.remove()` + iterate, avoiding the intermediary `Vec`.

**Acceptance criteria**:

- No temporary `Vec` allocated during component merge
- Graph connectivity tests pass unchanged

---

### WASM-049: Pre-compile BoolLogic/ValueLogic paths to segment ID arrays — 2pts

**Files**: `rust/src/bool_logic.rs`, `rust/src/value_logic.rs`

**Problem**: `BoolLogicNode` and `ValueLogicNode` store string paths (e.g.,
`IsEqual("user.profile.spot", v)`). On every `evaluate_value()` call, `get_path_value`
does `path.split('.')` and one `intern.get_id(seg)` string-HashMap lookup per segment.
For `"user.profile.spot"`: 3 string lookups per eval. At 200 logic entries per tick,
that's hundreds of avoidable string HashMap lookups per `processChanges` call.

**Fix**: At registration time, walk the deserialized tree and replace every `String` path
with a pre-resolved `Vec<u32>` of segment IDs. Introduce a compiled node type:

```rust
enum CompiledBoolLogicNode {
    IsEqual(Vec<u32>, Value),
    Exists(Vec<u32>),
    IsEmpty(Vec<u32>),
    Lt(Vec<u32>, f64),
    Gt(Vec<u32>, f64),
    // ... all leaf variants use Vec<u32> instead of String
    And(Vec<CompiledBoolLogicNode>),
    Or(Vec<CompiledBoolLogicNode>),
    Not(Box<CompiledBoolLogicNode>),
}
```

`LogicRegistry::register()` calls `compile_tree(node, intern)` after deserialization.
`evaluate_value` drops the `intern: &InternTable` parameter — the compiled tree has no
string lookups. Same transform applies to `ValueLogicNode`.

**Acceptance criteria**:

- `BoolLogicNode` and `ValueLogicNode` no longer stored in registries — only compiled forms
- `evaluate_value` / `evaluate` take no `intern` parameter
- `get_path_value` replaced with segment-ID traversal in hot path
- All existing bool_logic and value_logic tests pass unchanged

---

## Files to Modify

- `rust/src/router.rs` — WASM-045
- `rust/src/rev_index.rs` — WASM-046
- `rust/src/pipeline.rs` — WASM-047
- `rust/src/graphs.rs` — WASM-048
- `rust/src/bool_logic.rs`, `rust/src/value_logic.rs` — WASM-049

---

## Sizing

| Story | Points | Estimate |
|-------|--------|----------|
| WASM-045 | 2pts | Half day — Router pre-sort |
| WASM-046 | 2pts | Half day — Arc RevIndex |
| WASM-047 | 1pt | < 2h — Capacity hints |
| WASM-048 | 1pt | < 2h — Graph drain |
| WASM-049 | 2pts | Half day — Pre-compiled path segment IDs |
| **Total** | **8pts** | **~2.5 days** |

---

## Dependencies

- **Requires**: EP6 (Pipeline Refactor) — ✅ Complete
- All four stories are **independent** — can be parallelized
- **Blocks**: None
