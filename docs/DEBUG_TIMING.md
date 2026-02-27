---
title: Debug & Logging Guide
updated: 2026-02-27
audience: contributors debugging performance and understanding pipeline behavior
---

# Debug & Logging

Apex-state includes built-in console logging for pipeline runs, registrations, and slow-operation warnings during development.

## Enabling Debug Options

Pass `debug` config when creating the store:

```typescript
const store = createGenericStore<MyState>({
  debug: {
    log: true,            // enable console logging (default: false)
    timing: true,         // enable slow-operation warnings (default: false)
    timingThreshold: 16,  // ms threshold for "slow" warnings (default: 16)
    track: true,          // enable debug tracking for tests/tooling (default: false)
    devtools: true,       // enable Redux DevTools integration (default: false)
  },
})
```

All flags are optional and default to `false`.

## Console Logging (`debug.log`)

When `log: true`, every pipeline run and registration emits a grouped `console.log` entry.

### Pipeline log (per `processChanges` call)

A single collapsed group shows:

```
▶ apex-state · user.email, user.name  4.21ms
```

Expanding it reveals:

- **Input changes** — what triggered this run
- **WASM pipeline stages** — each stage (sync, flip, listeners, etc.) with duration, matched paths, produced/skipped changes
- **Listener dispatches** — which listeners fired, their input/output/state, duration
- **Applied changes** — final set written to valtio
- **State snapshot** — frozen state after apply

### Registration log (per register/unregister call)

Emitted after each `useSideEffects` or `useConcerns` mount/unmount:

```
▶ apex-state · [register] sideEffects-my-form  2.10ms
```

Expanding shows the full graph snapshot:

- **syncPairs** — all registered sync edges (bidirectional shown as `↔`, directed as `→`)
- **flipPairs** — all flip edges
- **listeners** — registered listener topics and scopes
- **boolLogics** — registered BoolLogic output paths
- **valueLogics** — registered ValueLogic output paths

## Timing Warnings (`debug.timing`)

When `timing: true`, listener execution is measured. If a listener exceeds `timingThreshold` (default 16ms), a warning is emitted inside the pipeline log group.

The `slow: true` flag is also set on the corresponding `ListenerDispatchTrace` entry.

## Debug Tracking (`debug.track`)

When `track: true`, the store exposes `store._debug` with an append-only log of all `processChanges` calls and their applied changes. Useful for assertions in tests:

```typescript
const changes = store._debug?.changes
store._debug?.reset()
```

## Logger API

The logger is created once per store via `createLogger(config)` in `src/utils/log.ts`. It returns an `ApexLogger` with two methods:

```typescript
interface ApexLogger {
  logPipeline: (data: PipelineLogData) => void
  logRegistration: (
    type: 'register' | 'unregister',
    id: string,
    snapshot: GraphSnapshot,
    data?: RegistrationLogData,
  ) => void
  destroy: () => void
}
```

When `log: false`, `createLogger` returns a zero-overhead no-op singleton — no closures, no work.

## `DebugConfig` Reference

```typescript
interface DebugConfig {
  log?: boolean              // console logging (default: false)
  timing?: boolean           // slow-operation warnings (default: false)
  timingThreshold?: number   // ms threshold (default: 16)
  track?: boolean            // debug tracking via store._debug (default: false)
  devtools?: boolean         // Redux DevTools integration (default: false)
}
```

## Key Files

| File                              | Role                                                           |
| --------------------------------- | -------------------------------------------------------------- |
| `src/utils/log.ts`                | `createLogger()`, `ApexLogger`, `PipelineLogData`, trace types |
| `src/core/types.ts`               | `DebugConfig`, `DebugTrack`, `StoreInstance._debug`            |
| `src/pipeline/process-changes.wasm-impl.ts` | Calls `logger.logPipeline` after each `processChanges`  |
| `src/sideEffects/registration.wasm-impl.ts` | Calls `logger.logRegistration` after register/unregister |

## Testing

- Logging integration tests: `tests/integrations_v2/logging.test.tsx`
