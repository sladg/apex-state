---
created: unstaged
updated: unstaged
status: active
---

# Task: Change Deduplication (A-B-A Collapse)

**Priority**: P2
**Type**: Feature
**Effort**: Medium

## Goal

When a batch of changes contains oscillating values (A→B→A), collapse them so the net result is no change. This reduces unnecessary listener triggers and re-renders.

## Current behavior

The pipeline (`src/pipeline/applyBatch.ts`) only checks `current !== value` at application time. It does not look at the batch as a whole to eliminate redundant changes.

Example: If a batch contains `[set foo=1, set foo=2, set foo=1]` and `foo` was already `1`, the net effect is zero — but currently all three changes may propagate.

## Design questions (need decision)

1. **Scope**: Should dedup happen within a single flush cycle only, or across multiple?
   - Recommended: single flush cycle only (simpler, predictable)
2. **Granularity**: Per-path last-write-wins, or full sequence analysis?
   - Recommended: per-path last-write-wins within a batch (compare final value to pre-batch value)
3. **Where**: New processor in `src/pipeline/processors/`, or inline in `processChanges.ts`?

## Key files

- `src/pipeline/applyBatch.ts` - current change application
- `src/pipeline/processChanges.ts` - change processing orchestration
- `src/pipeline/queue.ts` - change queue

## Acceptance criteria

- Batch `[A→B, B→A]` on a path produces zero net changes
- Batch `[A→B, B→C]` on a path produces one change (A→C)
- Existing tests still pass
- New tests cover dedup scenarios
