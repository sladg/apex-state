# @sladg/apex-state

Reactive state management for React built on [Valtio](https://github.com/pmndrs/valtio). Declare what your fields need — validation, conditional UI, sync, listeners — and the store handles the rest. Optional Rust/WASM accelerator for complex workloads (up to 367x faster).

## Quick Start

```bash
npm install @sladg/apex-state valtio zod react
```

```tsx
import { createGenericStore } from '@sladg/apex-state'
import { z } from 'zod'

// 1. Define your state shape
type FormState = {
  user: { name: string; email: string; age: number }
  preferences: { newsletter: boolean; theme: 'light' | 'dark' }
}

// 2. Create a typed store (WASM-accelerated by default)
const store = createGenericStore<FormState>()

// 3. Wrap your app with Provider
const App = () => (
  <store.Provider initialState={{
    user: { name: '', email: '', age: 0 },
    preferences: { newsletter: false, theme: 'light' },
  }}>
    <UserForm />
  </store.Provider>
)

// 4. Use hooks to read/write state and declare concerns
const UserForm = () => {
  store.useConcerns('user-form', {
    'user.email': {
      validationState: { schema: z.string().email('Invalid email') },
    },
  })

  const { value, setValue, validationState } = store.useFieldStore('user.email')

  return (
    <div>
      <input
        value={value}
        onChange={(e) => setValue(e.target.value)}
        className={validationState?.isError ? 'error' : ''}
      />
      {validationState?.isError && <span>{validationState.errors[0]}</span>}
    </div>
  )
}
```

## Features

| Feature | Description | Details |
|---|---|---|
| **Type-safe paths** | `DeepKey<T>` / `DeepValue<T, P>` — compile-time path safety | |
| **Concerns** | Validation (Zod), BoolLogic conditions, dynamic text | [Concerns Guide](docs/guides/CONCERNS_GUIDE.md) |
| **Side effects** | Sync paths, flip paths, aggregations, listeners | [Side Effects Guide](docs/SIDE_EFFECTS_GUIDE.md) |
| **WASM mode** | Rust-powered pipeline for bulk operations (up to 367x faster) | [Architecture](docs/WASM_ARCHITECTURE.md) |
| **Composable hooks** | Buffered, throttled, transformed field wrappers | [Store & Hooks](docs/guides/STORE_HOOKS.md) |
| **Record/wildcard** | `Record<string, V>` with `_()` hash key paths | [Wildcard Guide](docs/WILD_FUNCTION_GUIDE.md) |
| **Testing mock** | Drop-in `vi.mock` replacement with call tracking | [Testing Mock](docs/TESTING_MOCK.md) |

## Full Example

```tsx
import { createGenericStore } from '@sladg/apex-state'
import { z } from 'zod'

type OrderState = {
  product: { name: string; quantity: number; price: number }
  shipping: { address: string; express: boolean; standard: boolean }
  payment: { method: 'card' | 'cash'; cardNumber: string }
  status: 'draft' | 'submitted'
}

const store = createGenericStore<OrderState>()

const OrderForm = () => {
  // Side effects: auto-flip booleans
  store.useSideEffects('order', {
    flipPaths: [['shipping.express', 'shipping.standard']],
  })

  // Concerns: declarative validation and conditional UI
  store.useConcerns('order', {
    'product.quantity': {
      validationState: { schema: z.number().min(1).max(100) },
      disabledWhen: { boolLogic: { IS_EQUAL: ['status', 'submitted'] } },
    },
    'payment.cardNumber': {
      validationState: { schema: z.string().regex(/^\d{16}$/) },
      visibleWhen: { boolLogic: { IS_EQUAL: ['payment.method', 'card'] } },
    },
  })

  const { value, setValue, validationState, disabledWhen } =
    store.useFieldStore('product.quantity')

  return (
    <input
      type="number"
      value={value}
      onChange={(e) => setValue(Number(e.target.value))}
      disabled={disabledWhen}
      className={validationState?.isError ? 'error' : ''}
    />
  )
}

const App = () => (
  <store.Provider initialState={{
    product: { name: 'Widget', quantity: 1, price: 29.99 },
    shipping: { address: '', express: false, standard: true },
    payment: { method: 'card', cardNumber: '' },
    status: 'draft',
  }}>
    <OrderForm />
  </store.Provider>
)
```

## Reading and Writing State

```tsx
const store = createGenericStore<MyState>()

// useStore — simple [value, setter] tuple (like useState)
const NameInput = () => {
  const [name, setName] = store.useStore('user.name')
  return <input value={name} onChange={(e) => setName(e.target.value)} />
}

// useFieldStore — object API with concerns merged in
const EmailInput = () => {
  const { value, setValue, validationState, disabledWhen } =
    store.useFieldStore('user.email')
  return (
    <input
      value={value}
      onChange={(e) => setValue(e.target.value)}
      disabled={disabledWhen}
      className={validationState?.isError ? 'error' : ''}
    />
  )
}

// useJitStore — bulk operations and non-reactive reads
const ImportButton = () => {
  const { setChanges, getState } = store.useJitStore()

  const handleImport = (data: Record<string, unknown>) => {
    setChanges([
      ['user.name', data.name, {}],
      ['user.email', data.email, {}],
      ['user.age', data.age, {}],
    ])
  }

  return <button onClick={() => handleImport({ name: 'Alice', email: 'a@b.com', age: 30 })}>Import</button>
}
```

## Validation with Zod

```tsx
import { createGenericStore } from '@sladg/apex-state'
import { z } from 'zod'

type ProfileState = {
  user: { name: string; email: string; age: number; bio: string }
}

const store = createGenericStore<ProfileState>()

const ProfileForm = () => {
  // Register validation schemas for multiple fields at once
  store.useConcerns('profile-validation', {
    'user.name': {
      validationState: { schema: z.string().min(2, 'Name too short').max(50) },
    },
    'user.email': {
      validationState: { schema: z.string().email('Enter a valid email') },
    },
    'user.age': {
      validationState: { schema: z.number().min(18, 'Must be 18+').max(120) },
    },
    'user.bio': {
      validationState: { schema: z.string().max(500, 'Bio too long') },
    },
  })

  const email = store.useFieldStore('user.email')
  const age = store.useFieldStore('user.age')

  return (
    <form>
      <div>
        <input value={email.value} onChange={(e) => email.setValue(e.target.value)} />
        {email.validationState?.isError && (
          <ul>{email.validationState.errors.map((err, i) => <li key={i}>{err}</li>)}</ul>
        )}
      </div>
      <div>
        <input
          type="number"
          value={age.value}
          onChange={(e) => age.setValue(Number(e.target.value))}
        />
        {age.validationState?.isError && <span>{age.validationState.errors[0]}</span>}
      </div>
    </form>
  )
}
```

## Conditional UI with BoolLogic

```tsx
store.useConcerns('conditional-ui', {
  // Disable when another field has a specific value
  'user.email': {
    disabledWhen: { boolLogic: { IS_EQUAL: ['status', 'submitted'] } },
  },

  // Show only when multiple conditions are true
  'payment.cardNumber': {
    visibleWhen: {
      boolLogic: {
        AND: [
          { IS_EQUAL: ['payment.method', 'card'] },
          { EXISTS: 'user.email' },
        ],
      },
    },
  },

  // Make readonly when value exceeds threshold
  'order.total': {
    readonlyWhen: { boolLogic: { GT: ['order.total', 10000] } },
  },

  // Complex nested logic with OR, NOT
  'shipping.express': {
    disabledWhen: {
      boolLogic: {
        OR: [
          { IS_EQUAL: ['status', 'shipped'] },
          { NOT: { EXISTS: 'shipping.address' } },
        ],
      },
    },
  },
})

// Available BoolLogic operators:
// IS_EQUAL, EXISTS, IS_EMPTY, GT, LT, GTE, LTE, IN, AND, OR, NOT
```

## Side Effects

### Sync Paths

```tsx
store.useSideEffects('sync', {
  syncPaths: [
    ['billing.email', 'shipping.email'],   // changing one updates the other
    ['billing.phone', 'shipping.phone'],
  ],
})
// When user types in billing.email, shipping.email updates automatically
```

### Flip Paths

```tsx
store.useSideEffects('flips', {
  flipPaths: [
    ['isActive', 'isInactive'],       // setting isActive=true -> isInactive=false
    ['isExpanded', 'isCollapsed'],
  ],
})
```

### Aggregations

```tsx
store.useSideEffects('agg', {
  aggregations: [
    // Target is ALWAYS first. Multiple pairs with same target form a group.
    ['summary.price', 'legs.0.price'],
    ['summary.price', 'legs.1.price'],
    ['summary.price', 'legs.2.price'],
  ],
})
// summary.price = leg price if ALL legs match, undefined if they differ
```

### Listeners

```tsx
store.useSideEffects('listeners', {
  listeners: [
    {
      path: 'user.profile',       // watch changes under this path
      scope: 'user.profile',      // receive scoped state and relative paths
      fn: (changes, state) => {
        // changes: [['name', 'Alice', {}]] — paths relative to scope
        // state: { name: 'Alice', email: '...' } — user.profile sub-object
        return [['audit.lastEdit', Date.now(), {}]] // return FULL paths for new changes
      },
    },
  ],
})
```

## Dynamic Text

```tsx
store.useConcerns('dynamic-text', {
  'legs.0.strike': {
    dynamicTooltip: { template: 'Current strike: {{legs.0.strike}}' },
    dynamicLabel: { template: 'Strike for {{legs.0.product}}' },
    dynamicPlaceholder: { template: 'Enter value (min {{legs.0.minStrike}})' },
  },
})

const { dynamicTooltip, dynamicLabel } = store.useFieldConcerns('legs.0.strike')
// dynamicTooltip -> "Current strike: 105"
// dynamicLabel -> "Strike for AAPL"
```

## Composable Field Hooks

```tsx
import { useBufferedField, useThrottledField, useTransformedField } from '@sladg/apex-state'

// Buffer edits locally, commit/cancel explicitly
const PriceEditor = () => {
  const raw = store.useFieldStore('product.price')
  const buffered = useBufferedField(raw)

  return (
    <div>
      <input value={buffered.value} onChange={(e) => buffered.setValue(Number(e.target.value))} />
      <button onClick={buffered.commit} disabled={!buffered.isDirty}>Save</button>
      <button onClick={buffered.cancel}>Cancel</button>
    </div>
  )
}

// Throttle rapid setValue calls (e.g., slider input)
const VolumeSlider = () => {
  const raw = store.useFieldStore('audio.volume')
  const throttled = useThrottledField(raw, { ms: 100 })
  return <input type="range" value={throttled.value} onChange={(e) => throttled.setValue(Number(e.target.value))} />
}

// Transform display format (cents <-> dollars)
const CurrencyInput = () => {
  const raw = store.useFieldStore('price')
  const formatted = useTransformedField(raw, {
    to: (cents: number) => (cents / 100).toFixed(2),    // store -> display
    from: (dollars: string) => Math.round(parseFloat(dollars) * 100), // display -> store
  })
  return <input value={formatted.value} onChange={(e) => formatted.setValue(e.target.value)} />
}

// Chain them: buffered + transformed
const raw = store.useFieldStore('price')
const buffered = useBufferedField(raw)
const display = useTransformedField(buffered, {
  to: (cents) => (cents / 100).toFixed(2),
  from: (dollars) => Math.round(parseFloat(dollars) * 100),
})
// display has: value, setValue, commit, cancel, isDirty
```

## Hash Key Paths for Record Types

```tsx
import { _ } from '@sladg/apex-state'

// _() marks a segment as a hash key for Record-typed paths
const strikePath = `portfolio.legs.${_('l1')}.strike`
// -> typed string containing HASH_KEY marker

// Use with concerns — applies to ALL keys in the Record
store.useConcerns('wildcards', {
  [strikePath]: {
    validationState: { schema: z.number().min(0) },
    disabledWhen: { boolLogic: { IS_EQUAL: ['status', 'locked'] } },
  },
})

// Multiple hash keys for deeply nested Records
const nestedPath = `books.${_('b1')}.products.${_('p1')}.legs.${_('l1')}.notional`
```

## Testing with the Mock Module

```tsx
// __mocks__/@sladg/apex-state.ts
export * from '@sladg/apex-state/testing'

// your-test.test.ts
import { vi, describe, it, expect, beforeEach } from 'vitest'
import { __mocked, createGenericStore } from '@sladg/apex-state/testing'
import { renderHook } from '@testing-library/react'

vi.mock('@sladg/apex-state')

type FormState = { user: { email: string; name: string }; count: number }

beforeEach(() => __mocked.reset())

it('seeds state with type-safe chaining', () => {
  __mocked
    .set<FormState>({ user: { email: '', name: '' }, count: 0 })
    .set('user.email', 'alice@test.com')
    .set('count', 42)

  expect(__mocked.getState()).toEqual({
    user: { email: 'alice@test.com', name: '' },
    count: 42,
  })
})

it('tracks setValue calls from hooks', () => {
  const store = createGenericStore<FormState>()
  const hook = renderHook(() => store.useStore('user.email'))

  hook.result.current[1]('bob@test.com')

  expect(__mocked.state.calls).toContainEqual({
    path: 'user.email',
    value: 'bob@test.com',
    meta: undefined,
  })
})

it('tracks concern and side effect registrations', () => {
  const store = createGenericStore<FormState>()
  store.useConcerns('form', { 'user.email': { disabledWhen: { boolLogic: { AND: [] } } } })
  store.useSideEffects('effects', { listeners: [] })

  expect(__mocked.state.effects).toHaveLength(2)
  expect(__mocked.state.effects[0]?.type).toBe('concerns')
  expect(__mocked.state.effects[1]?.type).toBe('sideEffects')
})
```

## WASM vs Legacy Mode

WASM is the default. Pass `{ useLegacyImplementation: true }` for pure JS:

```tsx
// WASM (default) — Rust-powered pipeline, faster for complex state
const wasmStore = createGenericStore<MyState>()

// Legacy JS — pure JavaScript, no WASM binary needed
const legacyStore = createGenericStore<MyState>({ useLegacyImplementation: true })
```

### Performance

Benchmarked with 60 variants across 3 Record layers, 75 syncs, 40 flips, 100 BoolLogic conditions, 85 listeners:

| Operation | Legacy | WASM | Winner |
|---|---|---|---|
| Single field edit | **0.5us** | 1.4us | Legacy 2.6x |
| 7 changes + cascading listeners | 41.8ms | **0.11ms** | WASM 367x |
| 60 bulk price changes | 596ms | **2.9ms** | WASM 207x |
| 135 changes (full catalog refresh) | 621ms | **2.99ms** | WASM 208x |

Both modes produce **identical state** — verified across all 16 benchmark scenarios. See [Benchmark Comparison](docs/BENCHMARK_COMPARISON.md) for the full analysis.

## API Overview

### Store Creation

`createGenericStore<T>(config?)` returns all hooks:

| Hook | Returns | Use case |
|---|---|---|
| `Provider` | React context | Wraps component tree with `initialState` |
| `useStore(path)` | `[value, setValue]` | Simple read/write (like `useState`) |
| `useFieldStore(path)` | `{ value, setValue, ...concerns }` | Form fields with merged concerns |
| `useJitStore()` | `{ proxyValue, setChanges, getState }` | Bulk updates, non-reactive reads |
| `useConcerns(id, config)` | `void` | Register validation/BoolLogic/dynamic text |
| `useSideEffects(id, config)` | `void` | Register sync/flip/aggregation/listeners |
| `useFieldConcerns(path)` | `EvaluatedConcerns` | Read concern results for a path |
| `withConcerns(selection)` | `{ useFieldStore }` | Scoped field store with selected concerns |

### Built-in Concerns

| Concern | Returns | Config |
|---|---|---|
| `validationState` | `{ isError, errors[] }` | `{ schema: ZodSchema }` |
| `disabledWhen` | `boolean` | `{ boolLogic: BoolLogic }` |
| `visibleWhen` | `boolean` | `{ boolLogic: BoolLogic }` |
| `readonlyWhen` | `boolean` | `{ boolLogic: BoolLogic }` |
| `dynamicLabel` | `string` | `{ template: '{{path}}' }` |
| `dynamicTooltip` | `string` | `{ template: '{{path}}' }` |
| `dynamicPlaceholder` | `string` | `{ template: '{{path}}' }` |

### BoolLogic Operators

`IS_EQUAL`, `EXISTS`, `IS_EMPTY`, `GT`, `LT`, `GTE`, `LTE`, `IN`, `AND`, `OR`, `NOT`

## Architecture

```
setValue("email", "alice@example.com")
  |
  +--[WASM/Rust]--> shadow state + sync + flip + BoolLogic (Rust)
  |                   |
  |                   v
  |                 execute listeners + Zod validators (JS)
  |                   |
  |                   v
  |                 pipelineFinalize -> diff -> final changes (Rust)
  |
  +--[Legacy JS]--> sync -> flip -> listeners -> applyBatch
  |
  v
valtio proxy -> React re-render
```

**Dual-layer design:** JS/React owns reactivity and rendering. Rust/WASM owns heavy computation (graphs, diffing, pipeline orchestration). The boundary is thin: paths cross as strings, values as JSON. WASM decides the execution plan, JS executes user functions.

See [WASM Architecture](docs/WASM_ARCHITECTURE.md) for the full specification.

## Development

```bash
npm install            # Install dependencies
npm run wasm:build     # Compile Rust -> WASM
npm run build          # Bundle TypeScript + WASM
npm run test           # Run tests
npm run code:check     # Lint + type check
npm run wasm:check     # Rust lint + check
```

### WASM Prerequisites

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
rustup target add wasm32-unknown-unknown
cargo install wasm-pack
```

## Documentation

| Document | Covers |
|---|---|
| [Store & Hooks](docs/guides/STORE_HOOKS.md) | Hook reference, composable hooks, patterns |
| [Concerns Guide](docs/guides/CONCERNS_GUIDE.md) | Concern lifecycle, built-ins, custom concerns |
| [Side Effects Guide](docs/SIDE_EFFECTS_GUIDE.md) | Sync, flip, aggregation, listener API |
| [WASM Architecture](docs/WASM_ARCHITECTURE.md) | JS/WASM boundary, data flow, ownership model |
| [Benchmark Comparison](docs/BENCHMARK_COMPARISON.md) | Legacy vs WASM across 16 scenarios |
| [Wildcard Paths](docs/WILD_FUNCTION_GUIDE.md) | `_()` hash key utility for Record types |
| [String Interpolation](docs/INTERPOLATION.md) | Template helpers for dynamic text concerns |
| [Testing Mock](docs/TESTING_MOCK.md) | Mock module for consumer tests (`vi.mock`) |
| [Record Migration](docs/RECORD_MIGRATION.md) | Migration patterns for dynamic Record types |
| [Debug Timing](docs/DEBUG_TIMING.md) | Performance debugging utilities |
| [Full Index](docs/README.md) | Complete documentation index |

## Roadmap

- **Aggregation modes** — Planned: `SUM`, `AVG`, `COUNT`, `MIN`, `MAX`, and custom reducer functions (currently consensus mode only).
- **Nested sub-stores** — Component-level state that participates in the parent's pipeline.

## License

MIT
