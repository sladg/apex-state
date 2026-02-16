# WASM-EP8: Recency-Based Sync Path Prioritization

**Epic Key**: WASM-EP8
**Depends On**: EP6 (Pipeline Refactor)
**Status**: ⏳ Ready
**Total Points**: 5pts

---

## Context

Currently, sync path registration uses **majority voting** to determine initial sync values:

- Most common non-null value wins
- Tie-breaking: shallowest path (fewer dots in path name)
- Final tie: first value encountered

**Problem**: This doesn't reflect user intent. If a user just changed `profile.name`, syncing `user.name` → `profile.name` should use `profile.name`'s value (most recent), not the majority value.

**Goal**: Track which paths were changed recently and prioritize the **most recently changed** value when registering sync paths.

---

## Current vs. Desired Behavior

### Current (Majority Voting)

```rust
// State: { user.name: "Alice", profile.name: "Bob", settings.name: "Alice" }
// Registering sync: ["user.name", "profile.name", "settings.name"]
// Result: All sync to "Alice" (majority: 2/3)
```

### Desired (Recency-Based)

```rust
// State: { user.name: "Alice", profile.name: "Bob", settings.name: "Alice" }
// Last changed: profile.name (counter: 42)
// Registering sync: ["user.name", "profile.name", "settings.name"]
// Result: All sync to "Bob" (most recent change)
```

---

## Implementation Design

### 1. Add Change Tracking to Pipeline (`rust/src/pipeline.rs`)

```rust
pub(crate) struct ProcessingPipeline {
    // ... existing fields ...

    /// Global counter, increments on every processChanges() call
    change_counter: u64,

    /// Tracks when each path was last modified
    /// Maps: path (String) → counter value when last changed (u64)
    path_recency: HashMap<String, u64>,
}
```

**Memory overhead**: One `u64` counter + one `HashMap` entry per changed path (minimal).

### 2. Update Change Processing

In `prepare_changes()`:

```rust
// Increment counter at start of processing
self.change_counter += 1;

// After diffing, record recency for all changes
for change in &changes_after_diff {
    self.path_recency.insert(change.path.clone(), self.change_counter);
}
```

### 3. Update Sync Initial Changes

In `compute_sync_initial_changes()`:

```rust
for component in components {
    // Find path with highest recency (most recently changed)
    let most_recent_path = component_paths
        .iter()
        .max_by_key(|path| {
            self.path_recency.get(*path).unwrap_or(&0)
        })
        .unwrap();

    // Use that path's value as target
    let target_value = self.shadow.get(most_recent_path)
        .map(|v| serde_json::to_string(&v.to_json_value()).unwrap_or("null".to_string()))
        .unwrap_or("null".to_string());

    // Sync all paths in component to this value
    for path in &component_paths {
        let current = self.shadow.get(path)...;
        if current != target_value {
            changes.push(Change { path: path.clone(), value_json: target_value.clone() });
        }
    }
}
```

### 4. Fallback Logic

When no recency data exists (fresh page load, never changed):

- **Fallback**: Use shallowest path (current tie-breaker logic)
- If all paths have same depth: first non-null value

---

## Story Breakdown

### WASM-038: Add recency tracking infrastructure (2pts)

- Add `change_counter` and `path_recency` fields to `ProcessingPipeline`
- Update `prepare_changes()` to increment counter and record recency
- Add `reset()` method to clear recency data (for testing)
- Unit tests: verify counter increments, recency map updates correctly

### WASM-039: Update sync registration to use recency (2pts)

- Modify `compute_sync_initial_changes()` to prioritize most recent path
- Implement fallback logic (shallowest path when no history)
- Update documentation comments
- Unit tests: recency prioritization, fallback behavior

### WASM-040: Integration tests (1pt)

- Test: Recent change wins over majority
- Test: Fallback to shallowest path when no history
- Test: Multiple sync registrations respect latest changes
- Test: Recency persists across multiple `processChanges()` calls

---

## Acceptance Criteria

✅ **Given**: State `{ user.name: "Alice", profile.name: "Bob" }`
✅ **And**: `profile.name` was changed most recently (counter: 42)
✅ **When**: Registering sync `["user.name", "profile.name"]`
✅ **Then**: Both sync to `"Bob"` (most recent), not `"Alice"`

✅ **Given**: Fresh page load (no recency data)
✅ **When**: Registering sync `["user.profile.name", "settings.name"]`
✅ **Then**: Both sync to `settings.name` value (shallowest path fallback)

---

## Benefits

- ✅ **Better UX**: Syncing reflects user's most recent action
- ✅ **Intuitive**: "Last touched wins" is easier to reason about than "majority wins"
- ✅ **Minimal overhead**: Single counter + HashMap (O(1) lookups)
- ✅ **Backward compatible**: Fallback preserves existing behavior when no history

---

## Limitations

- ⚠️ **Session-only**: Recency data lost on page reload
- ⚠️ **Not persisted**: Only tracks changes during current session
- ⚠️ **Memory**: Grows with number of unique paths changed (acceptable for most apps)

---

## Files to Modify

- `rust/src/pipeline.rs` — Add fields, update logic
- `rust/src/lib.rs` — Initialize new fields in `ProcessingPipeline::new()`
- Tests:
  - `tests/wasm/pipeline.test.ts` — Unit tests for recency tracking
  - `tests/wasm/pipeline-integration.test.ts` — Integration tests

---

## Dependencies

- **Requires**: EP6 (Pipeline Refactor) — ✅ Complete
- **Blocks**: None

---

## Sizing

| Story | Points | Estimate |
|-------|--------|----------|
| WASM-038 | 2pts | Half day — Add infrastructure |
| WASM-039 | 2pts | Half day — Update sync logic |
| WASM-040 | 1pt | < 2h — Integration tests |
| **Total** | **5pts** | **~2 days** |

---

## Notes

- Implementation should NOT affect aggregation logic (aggregations use "all equal" logic, not recency)
- Recency tracking is ONLY for sync paths
- Consider adding a `clearRecency()` method for testing/debugging
