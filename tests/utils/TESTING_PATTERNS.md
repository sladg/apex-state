# Testing Patterns for apex-state

## Overview

This guide documents the **correct** patterns for testing apex-state stores. These patterns mirror production code behavior and avoid common pitfalls.

---

## Core Principle: Use Real Store, Not Mocks

❌ **Don't create custom test stores**
✅ **Always use real `createGenericStore`**

The test utilities provide helpers that wrap the real store, ensuring tests run against production code paths.

---

## Pattern 1: Basic Store Testing with `mountStore`

### Usage

```typescript
import { mountStore } from '../utils/react'
import { createGenericStore } from '~/store/createStore'

it('should update state correctly', async () => {
  const store = createGenericStore<{ count: number }>()

  const { storeInstance, setValue } = mountStore(
    store,
    { count: 0 }  // Initial state
  )

  // Make a change
  setValue('count', 42)
  await flushSync()

  // Verify
  expect(storeInstance.state.count).toBe(42)
})
```

### With Concerns Registration

```typescript
const { storeInstance, setValue } = mountStore(
  store,
  { email: '' },
  {
    concerns: {
      email: {
        validationState: { schema: z.string().email() }
      }
    },
    concernsId: 'test'  // Optional, defaults to 'test'
  }
)

setValue('email', 'invalid')
await flushSync()

expect(storeInstance._concerns['email']?.validationState).toMatchObject({
  isValid: false
})
```

### Dual-Mode Testing (Legacy vs WASM)

```typescript
const MODES = [
  { name: 'Legacy', config: { useLegacyImplementation: true } },
  { name: 'WASM', config: { useLegacyImplementation: false } }
]

describe.each(MODES)('[$name] Store behavior', ({ config }) => {
  it('should work in both modes', async () => {
    const store = createGenericStore<State>(config)
    const { storeInstance, setValue } = mountStore(store, initialState)

    // Test runs twice: once in Legacy, once in WASM
    setValue('field', 'value')
    expect(storeInstance.state.field).toBe('value')
  })
})
```

---

## Pattern 2: Using `setValue` Helper

### What It Does

`setValue` mimics the production `setValue` implementation:

1. Wraps change in React's `act()`
2. Creates an `ArrayOfChanges` tuple
3. Calls `processChanges` directly (Legacy or WASM based on config)

### Why This Pattern Is Correct

**Production code** (from `create-store.ts`):

```typescript
const setValue = (newValue, meta) => {
  const changes = [[path, newValue, meta]]
  processChanges(store, changes)
}
```

**Test helper** (from `tests/utils/react.tsx`):

```typescript
const setValue = (path, value, meta) => {
  act(() => {
    const changes = [[path, value, meta]]
    const processChanges = config.useLegacyImplementation
      ? processChangesLegacy
      : processChangesWasm
    processChanges(storeInstance, changes)
  })
}
```

Both call `processChanges` directly. This is **intentional** - the store doesn't rely on Proxy traps for programmatic updates.

### When NOT to Use `setValue`

If you're testing React components that update via user interactions:

```typescript
// ✅ Good - tests real user flow
import { fireEvent } from '../utils/react'

const input = screen.getByRole('textbox')
fireEvent.change(input, { target: { value: 'new value' } })

// ❌ Bad - bypasses component logic
setValue('field', 'new value')
```

---

## Pattern 3: Debug Tracking (`_debug`)

Enable `debug.track: true` on the store to record all `processChanges` calls and their effects.

### Setup

```typescript
const store = createGenericStore<State>({ debug: { track: true } })
const { storeInstance, setValue } = mountStore(store, initialState)
```

### Verify processChanges Calls

```typescript
it('should track processChanges calls', async () => {
  setValue('source', 'updated')
  await flushSync()

  const lastCall = storeInstance._debug!.calls.at(-1)!
  expect(lastCall.input).toContainEqual(['source', 'updated', expect.anything()])
  expect(lastCall.applied).toContainEqual({ path: 'source', value: 'updated' })
})
```

### Verify Concern Changes

```typescript
it('should track concern changes', async () => {
  setValue('email', 'invalid')
  await flushSync()

  const lastCall = storeInstance._debug!.calls.at(-1)!
  expect(lastCall.appliedConcerns).toContainEqual(
    expect.objectContaining({ path: expect.stringContaining('email') })
  )
})
```

### Reset Between Assertions

```typescript
storeInstance._debug!.clear()
// ... next interaction ...
expect(storeInstance._debug!.calls).toHaveLength(1) // only the new call
```

---

## Pattern 4: Side Effects Testing

### Sync Paths

```typescript
it('should synchronize source to target', async () => {
  const store = createGenericStore<SyncFlipState>()

  const Component = () => {
    store.useSideEffects('sync-test', {
      syncPaths: [['source', 'target']]
    })
    return null
  }

  const { storeInstance, setValue } = mountStore(
    <Component />,
    store,
    { source: 'initial', target: '' }
  )

  await flushSync()  // Let initial sync run

  setValue('source', 'updated')
  await flushSync()

  expect(storeInstance.state.target).toBe('updated')
})
```

### Listeners

```typescript
it('should trigger listener on change', async () => {
  const handler = vi.fn()
  const store = createGenericStore<State>()

  const Component = () => {
    store.useSideEffects('listener-test', {
      listeners: [
        {
          topic: 'user.*',
          handler: (_, { changes }) => {
            handler(changes)
          }
        }
      ]
    })
    return null
  }

  const { setValue } = mountStore(<Component />, store, initialState)

  setValue('user.name', 'Alice')
  await flushSync()

  expect(handler).toHaveBeenCalledWith(
    expect.arrayContaining([
      expect.objectContaining({ path: 'user.name', value: 'Alice' })
    ])
  )
})
```

---

## Pattern 5: Async Operations with `flushSync`

Always use `flushSync()` to wait for React and store updates:

```typescript
import { flushSync } from '../utils/react'

it('should handle async updates', async () => {
  const { storeInstance, setValue } = mountStore(/* ... */)

  setValue('field', 'value')
  await flushSync()  // ✅ Wait for effects, re-renders, etc.

  expect(storeInstance.state.field).toBe('value')
})
```

### Why `flushSync` Is Needed

1. **React batching** - React batches updates, `flushSync` forces immediate flush
2. **Effect cleanup** - Valtio effects need time to run
3. **Listener dispatch** - Side effects are async
4. **Concern evaluation** - BoolLogic/validation happens in effects

---

## Test Data: Single Shared Fixture, No Inline Builders

### Rule: One `mocks.ts`, One Big Object

All test state lives in a single file: `tests/mocks/`. It exports one typed constant per domain shape — no inline state builders, no `buildFlatState(N)`, no `buildOrders()` functions scattered across test files.

```typescript
// tests/mocks/

export interface EcommerceState {
  orders: Record<string, Order>
  invoices: Record<string, Invoice>
}

export const ECOMMERCE_STATE: EcommerceState = {
  orders: {
    order_0: { currency: 'USD', confirmed: false, status: 'pending', subtotal: 100, tax: 20, total: 120 },
    order_1: { currency: 'USD', confirmed: false, status: 'pending', subtotal: 110, tax: 22, total: 132 },
    // ... all orders defined statically
  },
  invoices: {
    inv_0: { pending: true },
    inv_1: { pending: true },
  },
}
```

### Why?

- **Readable** — you see the exact data shape at a glance, no runtime logic to trace
- **Type-safe** — the constant is typed, so tests get autocomplete and compile-time checks
- **Single source of truth** — one place to update when the domain model changes
- **No `as any`** — if the data doesn't fit the type, fix the type or the data, don't cast

### Anti-Patterns

```typescript
// ❌ Bad - inline builder function, hides data shape
const buildOrders = (): Record<string, unknown> => {
  const orders: Record<string, unknown> = {}
  for (let i = 0; i < ORDER_COUNT; i++) {
    orders[`order_${i}`] = { /* ... */ }
  }
  return { orders }
}

// ❌ Bad - dynamic generation, can't see actual values
const state = buildFlatState(N * 2)
const changes: ArrayOfChanges<BenchState, GenericMeta> = [
  ['field_0', 'trigger', {}],
] as any  // type escape!

// ❌ Bad - vague describe name
describe('S11: E-commerce — 15 orders (~256 total changes)', () => { ... })
```

```typescript
// ✅ Good - import shared constant, no builders
import { ECOMMERCE_STATE } from '../utils/mocks'

const store = createGenericStore<EcommerceState>({ debug: { track: true } })
const { storeInstance, setValue } = mountStore(store, ECOMMERCE_STATE)
```

### Guidelines

1. **All test state in `tests/mocks/`** — typed constants, no functions
2. **No `as any` casts on test data** — if types don't match, fix them
3. **No dynamic state builders** — static data is easier to reason about and debug
4. **If a test needs a variation**, spread from the base: `{ ...ECOMMERCE_STATE, orders: { ...ECOMMERCE_STATE.orders, order_0: { ...overrides } } }`

---

## Test Naming: Describe What Is Being Verified

Test names should describe the **behavior being verified**, not implementation details or scenario numbers.

```typescript
// ❌ Bad - scenario number, implementation detail, no "why"
describe('S11: E-commerce — 15 orders (~256 total changes)', () => { ... })

// ✅ Good - describes the behavior
describe('order confirmation syncs invoice status and updates audit trail', () => { ... })
```

```typescript
// ❌ Bad - describes setup, not behavior
it('should process 100 sync pairs', () => { ... })

// ✅ Good - describes what the test proves
it('synced paths propagate value changes bidirectionally', () => { ... })
```

### Naming Rules

1. `describe` blocks name the **feature or behavior** being tested
2. `it` blocks name the **specific assertion** — what should happen
3. No scenario numbers (`S1`, `S11`) — use descriptive names
4. No metrics in names (`~256 changes`) — that's a comment at best

---

## No `as any` in Tests

Type casts hide bugs. If test data doesn't match the expected type, the test is wrong or the type needs updating.

```typescript
// ❌ Bad - hides type mismatch
const changes: ArrayOfChanges<BenchState, GenericMeta> = [
  ['field_0', 'trigger', {}],
] as any

// ✅ Good - properly typed
const changes: ArrayOfChanges<BenchState, GenericMeta> = [
  ['field_0', 'trigger', {} as GenericMeta],
]
```

If you find yourself reaching for `as any`, stop and ask:

- Is the type wrong? Fix the type.
- Is the data wrong? Fix the data.
- Is the test testing something impossible? Reconsider the test.

---

## Dynamic Paths: Use `_()` for Record/HashMap Keys

When paths index into `Record<string, T>` (e.g., `orders.order_0.status`), use the `_()` hashKey mapper to produce type-safe paths. Never cast with `as any` or `as unknown`.

### How `_()` Works

`_()` takes a concrete ID string and returns it typed as `HASH_KEY`, which satisfies `DeepKey<T>` for Record-indexed paths.

```typescript
import { _ } from '~/utils/hashKey'
```

### Correct Usage

```typescript
interface EcommerceState {
  orders: Record<string, Order>
}

// ✅ Good - type-safe dynamic path
setValue(`orders.${_('order_0')}.status`, 'confirmed')

// ✅ Good - in ArrayOfChanges
const changes: ArrayOfChanges<EcommerceState, GenericMeta> = [
  [`orders.${_('order_0')}.status`, 'confirmed', {}],
  [`orders.${_('order_0')}.lastModified`, Date.now(), {}],
]

// ✅ Good - in listener return values
const listener = (): ArrayOfChanges<EcommerceState, GenericMeta> => [
  [`orders.${_('order_0')}.updateCount`, 1, {}],
  [`orders.${_('order_0')}.audit.updatedBy`, 'system', {}],
]
```

### What NOT to Do

```typescript
// ❌ Bad - string literal, bypasses type safety
const changes = [
  ['orders.order_0.status', 'confirmed', {}],
] as unknown as ArrayOfChanges<EcommerceState, GenericMeta>

// ❌ Bad - factory function with as unknown cast
const makeListener = (idx: number) => (): ArrayOfChanges<State, GenericMeta> =>
  [
    [`orders.order_${idx}.status`, 'updated', {}],
  ] as unknown as ArrayOfChanges<State, GenericMeta>
```

### Tip: Reusable Path Helpers

If many tests need the same path prefix, create a typed helper in `mocks.ts`:

```typescript
// tests/mocks/
export const orderPath = (id: string, field: string) =>
  `orders.${_(id)}.${field}` as const
```

---

## Anti-Patterns to Avoid

### ❌ Don't Reimplement Store Logic

```typescript
// ❌ Bad - custom store implementation
const testStore = {
  state: proxy({}),
  setValue: (path, value) => {
    dot.set(testStore.state, path, value)  // Bypasses processChanges!
  }
}
```

```typescript
// ✅ Good - use real store
const store = createGenericStore<State>()
const { storeInstance, setValue } = mountStore(store, initialState)
```

### ❌ Don't Set Values via Proxy Directly

```typescript
// ❌ Bad - bypasses change processing
storeInstance.state.field = 'value'
```

```typescript
// ✅ Good - use setValue helper
setValue('field', 'value')
```

### ❌ Don't Put Scaffolding in Test Files

Test files should contain **only tests**. Interfaces, constants, helper functions, builder functions, and fixtures belong in shared files under `tests/mocks/` or `tests/utils/`.

```typescript
// ❌ Bad - test file defines its own interfaces, constants, and helpers
// tests/benchmarking/wasm-vs-js.bench.spec.ts

interface BenchState {
  orders: Record<string, Order>
  invoices: Record<string, Invoice>
}

const ORDER_COUNT = 15

const buildOrders = (): Record<string, unknown> => {
  const orders: Record<string, unknown> = {}
  for (let i = 0; i < ORDER_COUNT; i++) { /* 20 lines of setup */ }
  return { orders }
}

const makeOrderListener = (idx: number) => () => [ /* ... */ ]

const buildFlatState = (n: number) => { /* ... */ }

// Then finally the actual tests start at line 150...
```

```typescript
// ✅ Good - test file imports everything, contains only tests
// tests/benchmarking/wasm-vs-js.bench.spec.ts

import { ECOMMERCE_STATE, FLAT_STATE } from '../mocks/states'
import type { EcommerceState, FlatState } from '../mocks/types'

describe('order confirmation propagates through sync and listener pipeline', () => {
  it('applies changes to all synced fields', async () => {
    const store = createGenericStore<EcommerceState>({ debug: { track: true } })
    const { storeInstance, setValue } = mountStore(store, ECOMMERCE_STATE)
    // ... actual test logic
  })
})
```

**Rule of thumb:** If a test file has more than ~5 lines of setup before the first `describe`/`it`, the scaffolding should be extracted to shared fixtures.

**Where things go:**

- **Interfaces and types** → `tests/mocks/types.ts`
- **State constants/fixtures** → `tests/mocks/states.ts` (or domain-specific files like `tests/mocks/ecommerce.ts`)
- **Reusable path helpers** → `tests/mocks/paths.ts`
- **Test utilities (render, flush, etc.)** → `tests/utils/`

### ❌ Don't Mock `processChanges`

```typescript
// ❌ Bad - defeats the purpose of integration tests
vi.mock('../../src/pipeline/processChanges', () => ({
  processChanges: vi.fn()
}))
```

```typescript
// ✅ Good - use _debug tracking to verify calls
const store = createGenericStore<State>({ debug: { track: true } })
// ... run test ...
expect(storeInstance._debug!.calls).toHaveLength(1)
```

---

## Common Gotchas

### 1. Forgetting `await flushSync()`

```typescript
// ❌ Fails intermittently
setValue('count', 42)
expect(storeInstance.state.count).toBe(42)  // Might not have updated yet!

// ✅ Reliable
setValue('count', 42)
await flushSync()
expect(storeInstance.state.count).toBe(42)
```

### 2. Using Wrong Config for Mode

```typescript
// ❌ Creates store with default config, test says "WASM" but runs Legacy
const store = createGenericStore()  // Defaults to Legacy!

// ✅ Explicit config
const store = createGenericStore({ useLegacyImplementation: false })
```

### 3. Testing Concerns Before Registration

```typescript
// ❌ Concerns not registered yet
const { storeInstance } = mountStore(store, initialState)
expect(storeInstance._concerns['field']?.validationState).toBeDefined()

// ✅ Register concerns first
const { storeInstance } = mountStore(
  store,
  initialState,
  { concerns: { field: { validationState: { schema } } } }
)
await flushSync()  // Let concerns evaluate
expect(storeInstance._concerns['field']?.validationState).toBeDefined()
```

---

## Summary: Testing Checklist

When writing store tests:

- ✅ Use real `createGenericStore`, not custom implementations
- ✅ Use `mountStore` for React integration
- ✅ Use `setValue` helper for programmatic updates
- ✅ Always `await flushSync()` after changes
- ✅ Use `_debug` tracking to verify processChanges calls and applied changes
- ✅ Test both Legacy and WASM modes with `.each(MODES)`
- ✅ All test state in `tests/mocks/` — typed constants, no inline builders
- ✅ Use `_()` hashKey mapper for dynamic paths — never `as any` or `as unknown`
- ✅ Descriptive test names — behavior, not scenario numbers
- ❌ Don't set state via Proxy directly
- ❌ Don't mock `processChanges` in integration tests
- ❌ Don't create custom test stores
- ❌ Don't use `as any` or `as unknown` casts on test data
- ❌ Don't create inline state builder functions (`buildState()`, `buildOrders()`)
- ❌ Don't use string literals for dynamic paths — use `_()`
- ❌ Don't define interfaces, constants, or helpers in test files — extract to `tests/mocks/` or `tests/utils/`

---

## Further Reading

- `tests/smoke-dual-mode.test.tsx` - Comprehensive examples
- `tests/utils/react.tsx` - Test utility implementations
- `src/store/create-store.ts` - Production `setValue` implementation
- `src/core/types.ts` - `DebugTrack` and `DebugTrackEntry` type definitions
