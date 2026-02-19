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

That's it. Vitest automatically picks up the `__mocks__` file and replaces all imports from `@sladg/apex-state` with the mock.

> **Note**: Types are resolved from the real module's `.d.ts` — `vi.mock` only replaces runtime, not type resolution. Your component code stays fully typed.

---

## API Reference

### `__mocked`

Import from the testing module:

```ts
import { __mocked } from '@sladg/apex-state/testing'
```

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

## Usage Patterns

### Basic: seed state and assert

```ts
import { vi, describe, it, expect, beforeEach } from 'vitest'
import { __mocked, createGenericStore } from '@sladg/apex-state/testing'

vi.mock('@sladg/apex-state')

interface FormState {
  user: {
    email: string
    name: string
  }
  count: number
}

beforeEach(() => __mocked.reset())

it('seeds state with type-safe chaining', () => {
  __mocked
    .set<FormState>({ user: { email: '', name: '' }, count: 0 })
    .set('user.email', 'alice@test.com')
    .set('count', 42)

  const snap = __mocked.getState()
  expect(snap).toEqual({
    user: { email: 'alice@test.com', name: '' },
    count: 42,
  })
})
```

### Assert setValue calls from hooks

```ts
import { renderHook } from '@testing-library/react'

it('tracks setValue calls', () => {
  const store = createGenericStore<FormState>()
  const hook = renderHook(() => store.useStore('user.email'))

  // Simulate user interaction
  hook.result.current[1]('bob@test.com')

  // Assert the call was tracked
  expect(__mocked.state.calls).toContainEqual({
    path: 'user.email',
    value: 'bob@test.com',
    meta: undefined,
  })
})
```

### Assert multiple calls in order

```ts
it('tracks call sequence', () => {
  const store = createGenericStore<FormState>()
  const hookEmail = renderHook(() => store.useStore('user.email'))
  const hookName = renderHook(() => store.useStore('user.name'))

  hookEmail.result.current[1]('a@b.com')
  hookName.result.current[1]('Alice')
  hookEmail.result.current[1]('updated@b.com')

  expect(__mocked.state.calls).toHaveLength(3)
  expect(__mocked.state.calls[0]).toEqual({
    path: 'user.email',
    value: 'a@b.com',
    meta: undefined,
  })
  expect(__mocked.state.calls[2]).toEqual({
    path: 'user.email',
    value: 'updated@b.com',
    meta: undefined,
  })
})
```

### React component rendering with flush

Hooks use `useSnapshot` internally, so valtio triggers re-renders automatically. Call `flush()` to let the microtask queue propagate.

```ts
import React from 'react'
import { render, screen } from '@testing-library/react'

const store = createGenericStore<FormState>()

const EmailDisplay = () => {
  const [email] = store.useStore('user.email')
  return React.createElement('span', { 'data-testid': 'email' }, String(email ?? ''))
}

it('renders initial state', async () => {
  render(
    React.createElement(
      store.Provider,
      { initialState: { user: { email: 'init@test.com', name: '' }, count: 0 }, children: null },
      React.createElement(EmailDisplay),
    ),
  )

  await __mocked.flush()
  expect(screen.getByTestId('email').textContent).toBe('init@test.com')
})

it('re-renders after state change', async () => {
  render(
    React.createElement(
      store.Provider,
      { initialState: { user: { email: '', name: '' }, count: 0 }, children: null },
      React.createElement(EmailDisplay),
    ),
  )

  await __mocked.flush()
  expect(screen.getByTestId('email').textContent).toBe('')

  // Mutate state externally
  __mocked.set<FormState>().set('user.email', 'updated@test.com')
  await __mocked.flush()

  expect(screen.getByTestId('email').textContent).toBe('updated@test.com')
})
```

### useFieldStore

```ts
it('reads value and setValue from useFieldStore', () => {
  __mocked.set<FormState>().set('user.name', 'Alice')
  const store = createGenericStore<FormState>()
  const hook = renderHook(() => store.useFieldStore('user.name'))

  expect(hook.result.current.value).toBe('Alice')

  hook.result.current.setValue('Bob')
  expect(__mocked.state.value['user']).toEqual({ name: 'Bob' })
})
```

### useJitStore — batch changes

```ts
it('applies batch changes', () => {
  const store = createGenericStore<FormState>()
  const hook = renderHook(() => store.useJitStore())

  hook.result.current.setChanges([
    ['user.email', 'batch@test.com', {}],
    ['count', 5, {}],
  ])

  expect(__mocked.state.value).toEqual({
    user: { email: 'batch@test.com' },
    count: 5,
  })
  expect(__mocked.state.calls).toHaveLength(2)
})
```

### Effect registration tracking

`useConcerns` and `useSideEffects` are plain functions (no `useSnapshot`) — call them directly.

```ts
it('tracks concern registrations', () => {
  const store = createGenericStore<FormState>()
  const registration = {
    'user.email': { disabledWhen: { boolLogic: { AND: [] } } },
  }

  store.useConcerns('form-concerns', registration)

  expect(__mocked.state.effects).toContainEqual({
    id: 'form-concerns',
    type: 'concerns',
    registration,
  })
})

it('tracks side effect registrations', () => {
  const store = createGenericStore<FormState>()
  store.useSideEffects('my-effects', { listeners: [] })

  expect(__mocked.state.effects).toHaveLength(1)
  expect(__mocked.state.effects[0]?.type).toBe('sideEffects')
})
```

### withConcerns — scoped field store

```ts
it('returns scoped useFieldStore', () => {
  __mocked.set<FormState>().set('count', 100)
  const store = createGenericStore<FormState>()
  const scoped = store.withConcerns({})
  const hook = renderHook(() => scoped.useFieldStore('count'))

  expect(hook.result.current.value).toBe(100)
})
```

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

Each vitest test file gets its own module instance (vitest default: `isolate: true`). The singleton `__state` proxy is safe across parallel test files. Call `__mocked.reset()` in `beforeEach` to clean up between tests within a file.

---

## Peer Dependencies

The testing subpath requires `@testing-library/react` (optional peer dependency) for `act()` used by `flush()`. If you're only using `__mocked.set()` / `__mocked.state` / `__mocked.reset()` without React rendering, it's not needed.
