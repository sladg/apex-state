# @sladg/apex-state

Reactive state management for React built on [Valtio](https://github.com/pmndrs/valtio). Declare what your fields need — validation, conditional UI, sync, listeners — and the store handles the rest. Optional Rust/WASM accelerator for complex workloads (up to 367x faster).

```bash
npm install @sladg/apex-state valtio zod react
```

## Example

```tsx
import { createGenericStore } from '@sladg/apex-state'
import { z } from 'zod'

type OrderState = {
  product: { name: string; quantity: number; price: number }
  shipping: { address: string; express: boolean }
  payment: { method: 'card' | 'cash'; cardNumber: string }
  status: 'draft' | 'submitted'
}

const store = createGenericStore<OrderState>()

const OrderForm = () => {
  // Declare side effects
  store.useSideEffects('order', {
    syncPaths: [['product.price', 'shipping.basePrice']],
    flipPaths: [['shipping.express', 'shipping.standard']],
  })

  // Declare concerns — just data, no logic to test
  store.useConcerns('order', {
    'product.quantity': {
      validationState: { schema: z.number().min(1).max(100) },
      disabledWhen: { condition: { IS_EQUAL: ['status', 'submitted'] } },
    },
    'payment.cardNumber': {
      validationState: { schema: z.string().regex(/^\d{16}$/) },
      visibleWhen: { condition: { IS_EQUAL: ['payment.method', 'card'] } },
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
    shipping: { address: '', express: false },
    payment: { method: 'card', cardNumber: '' },
    status: 'draft',
  }}>
    <OrderForm />
  </store.Provider>
)
```

## Features

| Feature | Description | Details |
|---|---|---|
| **Type-safe paths** | `DeepKey<T>` / `DeepValue<T, P>` — compile-time path safety | |
| **Concerns** | Validation, BoolLogic conditions, dynamic text | [Concerns Guide](docs/guides/CONCERNS_GUIDE.md) |
| **Side effects** | Sync paths, flip paths, aggregations, listeners | [Side Effects Guide](docs/SIDE_EFFECTS_GUIDE.md) |
| **WASM mode** | Rust-powered pipeline for bulk operations | [Architecture](docs/WASM_ARCHITECTURE.md) |
| **Composable hooks** | Buffered, throttled, transformed field wrappers | [Store & Hooks](docs/guides/STORE_HOOKS.md) |
| **Record/wildcard** | `Record<string, V>` with `[*]` wildcard paths | [Wildcard Guide](docs/WILD_FUNCTION_GUIDE.md) |

## Architecture

```
setValue("email", "alice@example.com")
  │
  ├─[Legacy JS]──▶ sync → flip → listeners → applyBatch
  │
  └─[WASM/Rust]──▶ shadow state + sync + flip + BoolLogic (Rust)
                      │
                      ▼
                    execute listeners + Zod validators (JS)
                      │
                      ▼
                    pipelineFinalize → diff → final changes (Rust)
  │
  ▼
valtio proxy → React re-render
```

**Dual-layer design:** JS/React owns reactivity and rendering. Rust/WASM owns heavy computation (graphs, diffing, pipeline orchestration). The boundary is thin: paths cross as strings, values as JSON. WASM decides the execution plan, JS executes user functions.

See [docs/WASM_ARCHITECTURE.md](docs/WASM_ARCHITECTURE.md) for the full specification.

## WASM Mode

WASM is the default. Pass `{ useLegacyImplementation: true }` for pure JS:

```tsx
const store = createGenericStore<MyState>()                                // WASM (default)
const store = createGenericStore<MyState>({ useLegacyImplementation: true }) // Legacy JS
```

### Performance

Benchmarked with 60 variants across 3 Record layers, 75 syncs, 40 flips, 100 BoolLogic conditions, 85 listeners:

| Operation | Legacy | WASM | Winner |
|---|---|---|---|
| Single field edit | **0.5us** | 1.4us | Legacy 2.6x |
| 7 changes + cascading listeners | 41.8ms | **0.11ms** | WASM 367x |
| 60 bulk price changes | 596ms | **2.9ms** | WASM 207x |
| 135 changes (full catalog refresh) | 621ms | **2.99ms** | WASM 208x |

Both modes produce **identical state** — verified across all 16 benchmark scenarios. See [docs/BENCHMARK_COMPARISON.md](docs/BENCHMARK_COMPARISON.md) for the full analysis.

### Why WASM is faster

- **Pre-computed topic routing** — listener dispatch is O(1) lookup vs O(changes x listeners) string matching
- **Shadow state diffing** — fast Rust HashMap vs valtio Proxy trap overhead
- **Single-pass pipeline** — aggregation + sync + flip + BoolLogic in one Rust call
- **BoolLogic in pipeline** — evaluated in Rust before listeners fire; Legacy defers to async `effect()`

### Why Legacy is faster for small ops

Every WASM call pays a fixed cost: JSON serialization, wasm-bindgen marshalling, and two round trips (`processChanges` + `pipelineFinalize`). When the actual work is trivial, this ~1us overhead dominates.

## API Quick Reference

### Store

```tsx
const {
  Provider,        // React context — accepts initialState
  useFieldStore,   // { value, setValue, ...concerns } for a path
  useStore,        // [value, setValue] tuple for a path
  useJitStore,     // { proxyValue, setChanges, getState } for bulk ops
  useSideEffects,  // register sync/flip/aggregation/listeners
  useConcerns,     // register validation/BoolLogic/custom concerns
  withConcerns,    // typed concern selection
} = createGenericStore<MyState>(config?)
```

### Concerns

```tsx
useConcerns('id', {
  'user.email': {
    validationState: { schema: z.string().email() },
    disabledWhen:    { condition: { IS_EQUAL: ['tosAccepted', false] } },
    visibleWhen:     { condition: { AND: [{ EXISTS: 'user.name' }, { IS_EQUAL: ['step', 2] }] } },
  },
})
```

Built-in concerns: `validationState`, `disabledWhen`, `readonlyWhen`, `visibleWhen`, `dynamicLabel`, `dynamicTooltip`, `dynamicPlaceholder`.

BoolLogic operators: `IS_EQUAL`, `EXISTS`, `IS_EMPTY`, `GT`, `LT`, `GTE`, `LTE`, `IN`, `AND`, `OR`, `NOT`.

See [Concerns Guide](docs/guides/CONCERNS_GUIDE.md) for lifecycle, custom concerns, and testing.

### Side Effects

```tsx
useSideEffects('id', {
  syncPaths:    [['source', 'target']],
  flipPaths:    [['active', 'inactive']],
  // Aggregation: target gets the common value when all sources agree, undefined otherwise
  // Multiple pairs with the same target form a group
  aggregations: [['summary.price', 'legs.0.price'], ['summary.price', 'legs.1.price']],
  listeners:    [{ path: 'orders', scope: 'orders', fn: handler }],
})
```

See [Side Effects Guide](docs/SIDE_EFFECTS_GUIDE.md) for the full API.

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
# Rust toolchain
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
rustup target add wasm32-unknown-unknown
cargo install wasm-pack
```

## Documentation

| Document | Covers |
|---|---|
| [WASM Architecture](docs/WASM_ARCHITECTURE.md) | JS/WASM boundary, data flow, ownership model |
| [Benchmark Comparison](docs/BENCHMARK_COMPARISON.md) | Legacy vs WASM performance with 16 scenarios |
| [Concerns Guide](docs/guides/CONCERNS_GUIDE.md) | Concern lifecycle, built-ins, custom concerns |
| [Side Effects Guide](docs/SIDE_EFFECTS_GUIDE.md) | Sync, flip, aggregation, listener API |
| [Store & Hooks](docs/guides/STORE_HOOKS.md) | Hook reference and patterns |
| [Debug Timing](docs/DEBUG_TIMING.md) | Performance debugging utilities |
| [Wildcard Paths](docs/WILD_FUNCTION_GUIDE.md) | `Wild()` template utility for Record types |
| [Record Migration](docs/RECORD_MIGRATION.md) | Migration patterns for dynamic Record types |
| [Full Index](docs/README.md) | Complete documentation index |

## Roadmap

- **Multiple store instances** — WASM currently uses global state, limiting to one store per page. Moving to instance-scoped WASM will allow parallel independent stores.
- **Nested sub-stores** — Allow a parent store to contain child stores, enabling component-level state that participates in the parent's pipeline (concerns, listeners, sync).
- **Technical debt resolution** — See [TECHNICAL_DEBT.md](TECHNICAL_DEBT.md) for tracked items.

## License

MIT
