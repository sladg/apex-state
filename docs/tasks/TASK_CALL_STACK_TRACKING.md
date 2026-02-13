---
created: unstaged
updated: unstaged
status: active
---

# Task: Call-Stack Tracking for Debugging

**Priority**: P3
**Type**: Feature
**Effort**: Medium

## Goal

Allow developers to see WHERE store functions (setValue, etc.) were called from — file and line number — for easier debugging.

## Current state

A timing/debug system exists (`src/utils/timing.ts`, config in `src/core/types.ts` lines 21-28) that tracks operation duration. It does NOT capture call origin (file/line).

## Proposed approach

1. Add a `debug.traceOrigin` config flag (default: `false`)
2. When enabled, capture `new Error().stack` at the call site of store mutation functions
3. Attach the parsed file/line to the change object or log it
4. Gate behind the flag to avoid perf cost in production (stack capture is expensive)

## Key files

- `src/utils/timing.ts` - existing debug infrastructure
- `src/core/types.ts` - debug config types
- `src/store/createStore.ts` - store function call sites

## Design questions

1. Should the trace be attached to each change object, or just logged?
2. Should it integrate with the existing timing system or be separate?
3. What's the output format? (console.log, structured data, callback?)

## Acceptance criteria

- Config flag `debug.traceOrigin` enables/disables the feature
- When enabled, store mutations include file/line info
- Zero overhead when disabled
- Works in both Node.js and browser environments
