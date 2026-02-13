---
created: unstaged
updated: unstaged
status: active
---

# Task: Listener Spillage Test

**Priority**: P1
**Type**: Test only
**Effort**: Low

## Goal

Add tests verifying listener isolation and ordering when multiple listeners watch the same path.

## What to test

1. **No spillage between listeners**: When listener A triggers on path `foo.bar` and produces a new change, listener B (also on `foo.bar`) should NOT see that new change in the same flush cycle.
2. **Ordering**: Listeners fire in registration order. Verify this is stable.
3. **Cross-listener trigger**: If listener A's side-effect changes `foo.bar`, listener B should be called in the _next_ flush cycle (not the current one).

## Key files

- `src/pipeline/processors/listeners.ts` - listener execution logic
- `tests/integration/side-effects.test.tsx` - existing listener tests (add here or create new file)

## Acceptance criteria

- Tests pass with `npm test`
- At least 3 test cases covering the scenarios above
- No changes to source code (test-only)
