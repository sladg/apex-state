# Testing Mock — `@sladg/apex-state/testing`

Lightweight mock of `@sladg/apex-state` for consumer tests. No real pipeline, no WASM, no concerns evaluation — just bare-bones reactive state with call tracking.

## Setup

### 1. Create the mock file

```
your-project/
  __mocks__/
    @sladg/
      apex-state.ts     <-- this file
  src/
  tests/
```

```ts
// __mocks__/@sladg/apex-state.ts
export * from '@sladg/apex-state/testing'
```

### 2. Enable the mock in your test

```ts
import { vi } from 'vitest'

vi.mock('@sladg/apex-state')
```

Vitest automatically picks up the `__mocks__` file. Types are resolved from the real module's `.d.ts`.

---

## API Reference

### `__mocked`

| Method / Property | Description |
|---|---|
| `__mocked.set<T>(data?)` | Seed state + establish type. Returns typed chainable. |
| `__mocked.state` | Reactive proxy — `.value`, `.calls`, `.effects` |
| `__mocked.getState()` | Immutable snapshot of current state |
| `__mocked.flush()` | Flush valtio-triggered React re-renders (async) |
| `__mocked.reset()` | Clear all state, calls, and effects |

### Typed chainable (returned by `.set<T>()`)

| Method / Property | Description |
|---|---|
| `.set(path, value)` | Type-safe set — `DeepKey<T>` path, `DeepValue<T, P>` value. Chainable. |
| `.state` | Same reactive proxy as `__mocked.state` |
| `.getState()` | Typed immutable snapshot (`T`) |
| `.flush()` | Same as `__mocked.flush()` |
| `.reset()` | Same as `__mocked.reset()` |

---

## Usage Examples

See `examples/testing-mock.ts` for complete examples of:

- Seeding mock state with type-safe chaining
- Tracking `setValue` calls from `useStore` hooks
- Asserting `useConcerns` and `useSideEffects` registrations

---

## What's Mocked, What's Real

| | Mock | Real |
|---|---|---|
| `createGenericStore` | Returns mock hooks backed by shared `__state` proxy | - |
| `Provider` | Seeds `__state.value` from `initialState`, renders children | - |
| `useStore` / `useFieldStore` | Reads from `__state` via `useSnapshot`, tracks calls | - |
| `useJitStore` | Same, with `setChanges` batch support | - |
| `useConcerns` / `useSideEffects` | Logs to `__state.effects` (no evaluation) | - |
| `withConcerns` | Returns `useFieldStore` (no concern filtering) | - |
| All other exports | - | Re-exported from the real module (`dot`, `evaluateBoolLogic`, `prebuilts`, etc.) |
| Types | - | Resolved from real module `.d.ts` |

---

## Isolation

Each vitest test file gets its own module instance (`isolate: true`). Call `__mocked.reset()` in `beforeEach` to clean up between tests within a file.

## Peer Dependencies

The testing subpath requires `@testing-library/react` (optional peer dependency) for `act()` used by `flush()`. If you're only using `__mocked.set()` / `__mocked.state` / `__mocked.reset()` without React rendering, it's not needed.
