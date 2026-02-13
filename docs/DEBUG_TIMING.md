---
title: Debug & Timing Guide
audience: contributors debugging performance
created: 2026-02-05 (c2c957e)
updated: 2026-02-05 (d221b60)
status: active
---

# Debug & Timing

Apex-state includes built-in timing instrumentation for detecting slow concerns and listeners during development.

## Enabling Debug Timing

Pass `debug` config when creating the store:

```typescript
const store = createGenericStore<MyState>({
  debug: {
    timing: true, // enable measurement (default: false)
    timingThreshold: 16, // ms threshold for "slow" warnings (default: 16)
  },
})
```

When `timing` is enabled, every concern evaluation, listener execution, and registration call is wrapped in `performance.now()` measurements. Operations exceeding the threshold trigger a `console.warn`.

## What Gets Measured

| Operation          | When it fires                                         | Threshold applies to                      |
| ------------------ | ----------------------------------------------------- | ----------------------------------------- |
| Concern evaluation | Each `evaluate()` call inside `effect()`              | Single concern on a single path           |
| Listener execution | Each listener `fn()` call during pipeline             | Single listener invocation                |
| Registration       | Each `registerConcernEffects` / `registerSideEffects` | Full registration batch for one component |

## Console Output

When a slow operation is detected:

```
[apex-state] Slow concerns: user.email/validationState took 18.42ms (threshold: 16ms)
[apex-state] Slow registration: my-form-id/sideEffects took 12.50ms (threshold: 16ms)
```

After a batch of operations, a summary is logged if any were slow or total time exceeded 16ms:

```
[apex-state] concerns: 12 ops in 24.50ms (1 slow)
```

## API Reference

All utilities live in `src/utils/timing.ts`.

### Types

```typescript
interface TimingEvent {
  type: "concerns" | "listeners" | "registration"
  path: string
  name: string
  duration: number
  threshold: number
}

interface TimingSummary {
  type: "concerns" | "listeners" | "registration"
  totalDuration: number
  operationCount: number
  slowOperations: TimingEvent[]
}

type OnSlowOperation = (event: TimingEvent) => void
type OnTimingSummary = (summary: TimingSummary) => void
```

### Functions

**`measureTiming(fn, debug, event, context?, onSlowOperation?)`**

Wraps a function call with timing measurement. If `debug.timing` is `false`, the function runs without overhead.

- `fn` — function to measure
- `debug` — resolved debug config
- `event` — metadata (type, path, name, threshold) without duration
- `context` — optional `TimingContext` to accumulate results across a batch
- `onSlowOperation` — callback when threshold exceeded (defaults to `console.warn`)

**`createTimingContext(type)`**

Creates a context for accumulating measurements across multiple operations in a single batch.

**`reportTimingSummary(context, onSummary?)`**

Reports the accumulated timing summary from a context.

## StoreConfig Reference

```typescript
interface DebugConfig {
  timing?: boolean // default: false
  timingThreshold?: number // default: 16 (ms)
}

interface StoreConfig {
  maxIterations?: number // default: 100
  debug?: DebugConfig
}
```

## Key Files

| File                              | Role                                                      |
| --------------------------------- | --------------------------------------------------------- |
| `src/utils/timing.ts`             | Timing utilities, types, default handlers                 |
| `src/core/types.ts`               | `DebugConfig`, `ResolvedDebugConfig`, `StoreConfig`       |
| `src/concerns/registration.ts`    | Uses `timing.run` for concern evaluation and registration |
| `src/sideEffects/registration.ts` | Uses `timing.run` for side effect registration            |

## Testing

- `tests/utils/timing.test.ts` — unit tests for timing utilities
