---
created: unstaged
updated: unstaged
status: active
---

# Task: Null-Path Listener Test

**Priority**: P1
**Type**: Test only
**Effort**: Low

## Goal

Add tests verifying that listeners registered with `path: null` (root listeners) receive all changes from previously run listeners.

## Context

The implementation already exists in `src/pipeline/processors/listeners.ts` (lines 55-61) where `listenerPath === ''` handles root listeners. The type definition in `src/core/types.ts` (line 104) shows `path: DeepKey<DATA> | null`. What's missing is test coverage.

## What to test

1. **Root listener receives all changes**: A `path: null` listener is called for any state change regardless of path.
2. **Accumulation**: Root listener sees changes produced by previously executed path-specific listeners.
3. **Multiple root listeners**: Multiple `path: null` listeners each receive the full change set.

## Key files

- `src/pipeline/processors/listeners.ts` - implementation
- `src/core/types.ts` - type definition for `path: null`
- `tests/integration/side-effects.test.tsx` - existing listener tests

## Acceptance criteria

- Tests pass with `npm test`
- At least 3 test cases covering the scenarios above
- No changes to source code (test-only)
