# @sladg/apex-state

---

_Built with AI._

---

Reactive state management for React where you declare what your fields need (validation, visibility, labels) and the store handles the rest.

## Why

Complex form state typically means scattered validation logic, conditional rendering spread across components, and hundreds of unit tests covering every edge case.

Apex-state inverts this: **define behavior as static configuration, not imperative code**.

The core functions (`evaluateBoolLogic`, `validationState`, etc.) are tested once. Your application config is just constants - Lego bricks snapped together. You test the bricks, not every possible tower.

## How

Built on [valtio](https://github.com/pmndrs/valtio) for proxy-based state and [valtio-reactive](https://github.com/jotaijs/valtio-reactive) for automatic dependency tracking.

- **valtio**: Mutable-style API, immutable under the hood. Write `state.count++`, get efficient React updates.
- **valtio-reactive**: Effects that auto-track which state paths they read. Change `state.user.name` -> only effects reading that path re-run.

Concerns wrap this: you declare _what_ to compute, the library handles _when_ to recompute.

## Installation

```bash
npm install @sladg/apex-state valtio zod react
```

## Example

```typescript
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
  // Register concerns - validation, conditional UI, dynamic text
  // This is just data - a constant. No logic to test here.
  store.useConcerns('order-form', {
    'product.quantity': {
      validationState: { schema: z.number().min(1).max(100) },
      disabledWhen: { condition: { IS_EQUAL: ['status', 'submitted'] } },
    },
    'shipping.address': {
      validationState: { schema: z.string().min(10) },
      visibleWhen: { condition: { EXISTS: 'shipping.express' } },
      dynamicLabel: { template: 'Address ({{shipping.express}} ? Express : Standard)' },
    },
    'payment.cardNumber': {
      validationState: { schema: z.string().regex(/^\d{16}$/) },
      visibleWhen: { condition: { IS_EQUAL: ['payment.method', 'card'] } },
      disabledWhen: {
        condition: {
          OR: [
            { IS_EQUAL: ['status', 'submitted'] },
            { LT: ['product.quantity', 1] },
          ],
        },
      },
      dynamicTooltip: { template: 'Pay ${{product.price}} for {{product.quantity}} items' },
    },
  })

  // Access state with hooks
  const [quantity, setQuantity] = store.useStore('product.quantity')
  const address = store.useFieldStore('shipping.address')
  const cardNumber = store.useFieldStore('payment.cardNumber')

  // Access evaluated concerns
  const quantityConcerns = store.useFieldConcerns('product.quantity')
  const addressConcerns = store.useFieldConcerns('shipping.address')
  const cardConcerns = store.useFieldConcerns('payment.cardNumber')

  return (
    <form>
      <input
        type="number"
        value={quantity}
        onChange={(e) => setQuantity(Number(e.target.value))}
        disabled={quantityConcerns.disabledWhen}
        className={quantityConcerns.validationState?.isError ? 'invalid' : 'valid'}
      />

      {addressConcerns.visibleWhen && (
        <input
          value={address.value}
          onChange={(e) => address.setValue(e.target.value)}
          placeholder={addressConcerns.dynamicLabel}
        />
      )}

      {cardConcerns.visibleWhen && (
        <input
          value={cardNumber.value}
          onChange={(e) => cardNumber.setValue(e.target.value)}
          disabled={cardConcerns.disabledWhen}
          title={cardConcerns.dynamicTooltip}
        />
      )}
    </form>
  )
}

const App = () => {
  return (
    <store.Provider
      initialState={{
        product: { name: 'Widget', quantity: 1, price: 29.99 },
        shipping: { address: '', express: false },
        payment: { method: 'card', cardNumber: '' },
        status: 'draft',
      }}
    >
      <OrderForm />
    </store.Provider>
  )
}
```

## Concerns

Reactive computations that automatically track dependencies and re-evaluate when state changes.

| Concern              | Props                      | Returns               | Description              |
| -------------------- | -------------------------- | --------------------- | ------------------------ |
| `validationState`    | `{ schema: ZodSchema }`    | `{ isError, errors }` | Full validation state    |
| `disabledWhen`       | `{ condition: BoolLogic }` | `boolean`             | Conditional disable      |
| `readonlyWhen`       | `{ condition: BoolLogic }` | `boolean`             | Conditional readonly     |
| `visibleWhen`        | `{ condition: BoolLogic }` | `boolean`             | Conditional visibility   |
| `dynamicLabel`       | `{ template: string }`     | `string`              | Interpolated label       |
| `dynamicTooltip`     | `{ template: string }`     | `string`              | Interpolated tooltip     |
| `dynamicPlaceholder` | `{ template: string }`     | `string`              | Interpolated placeholder |

## BoolLogic

Declarative conditions for `disabledWhen`, `readonlyWhen`, `visibleWhen`:

```typescript
{
  IS_EQUAL: ["path", value]
} // path === value
{
  EXISTS: "path"
} // path !== undefined && path !== null
{
  IS_EMPTY: "path"
} // !path or empty string/array
{
  GT: ["path", number]
} // path > number
{
  LT: ["path", number]
} // path < number
{
  GTE: ["path", number]
} // path >= number
{
  LTE: ["path", number]
} // path <= number
{
  IN: ["path", [values]]
} // values.includes(path)
{
  AND: [conditions]
} // all conditions true
{
  OR: [conditions]
} // any condition true
{
  NOT: condition
} // invert condition
```

## Hooks

### Store Hooks

```typescript
// useState-like access
const [value, setValue] = store.useStore("path.to.field")

// Object API for forms
const field = store.useFieldStore("path.to.field")
field.value // current value
field.setValue(v) // update value

// Evaluated concerns for a path
const concerns = store.useFieldConcerns("path.to.field")
concerns.validationState // { isError: boolean, errors: ValidationError[] }
concerns.disabledWhen // boolean
concerns.dynamicTooltip // string

// Bulk operations (non-reactive)
const jit = store.useJitStore()
jit.setChanges([
  ["a", 1, {}],
  ["b", 2, {}],
])
jit.getState() // snapshot
```

### Composable Field Hooks

Standalone hooks that wrap any field hook to add behavior. They compose with each other.

```typescript
import { useBufferedField, useThrottledField, useTransformedField, useKeyboardSelect } from "@sladg/apex-state"

// Buffered editing - hold changes locally until commit
const field = store.useFieldStore("user.name")
const buffered = useBufferedField(field)
buffered.setValue("draft") // local only
buffered.commit() // push to store
buffered.cancel() // revert to stored value
buffered.isDirty // true if local !== stored

// Throttled updates - rate-limit setValue calls
const price = store.useFieldStore("spotPrice")
const throttled = useThrottledField(price, { ms: 100 })
throttled.setValue(1.234) // immediate
throttled.setValue(1.235) // buffered, last value wins
// After 100ms: 1.235 is applied

// Value transformation - convert between storage and display formats
const date = store.useFieldStore("user.birthdate")
const formatted = useTransformedField(date, {
  to: (iso) => format(new Date(iso), "MM/dd/yyyy"),
  from: (display) => parse(display, "MM/dd/yyyy").toISOString(),
})
// formatted.value is "01/15/2024", store holds ISO string

// Keyboard selection - type-ahead for select inputs
const country = store.useFieldStore("user.country")
const { onKeyDown, ...rest } = useKeyboardSelect(country, {
  options: [
    { value: "us", label: "United States" },
    { value: "uk", label: "United Kingdom" },
  ],
})
// User types "u" -> selects "United States"

// Compose them together
const raw = store.useFieldStore("price")
const transformed = useTransformedField(raw, {
  to: (cents) => (cents / 100).toFixed(2),
  from: (dollars) => Math.round(parseFloat(dollars) * 100),
})
const throttled2 = useThrottledField(transformed, { ms: 50 })
```

## Side Effects

Side effects react to state changes synchronously during the change processing pipeline. They run before concerns re-evaluate.

```typescript
// Register via hook
store.useSideEffects("my-effects", {
  // Sync paths: keep two paths in sync
  syncPaths: [["billing.email", "shipping.email"]],

  // Flip paths: inverse boolean pairs
  flipPaths: [["isActive", "isInactive"]],

  // Aggregations: derive target from sources
  // Target is always first. Value = common value if all sources agree, else undefined.
  aggregations: [
    ["summary.price", "legs.0.price"],
    ["summary.price", "legs.1.price"],
  ],

  // Listeners: react to changes under a path
  listeners: [
    {
      path: "user.profile", // watch changes under this path
      scope: "user.profile", // receive scoped state
      fn: (changes, state) => {
        // changes: [['name', 'Alice', {}]]  -- relative to scope
        // state: user.profile sub-object
        return [["audit.lastEdit", Date.now(), {}]] // return full paths
      },
    },
  ],
})
```

See `docs/SIDE_EFFECTS_GUIDE.md` for the full API reference.

## Store Configuration

```typescript
const store = createGenericStore<MyState>({
  // Max iterations for the change processing loop (default: 100)
  maxIterations: 100,

  // Debug tooling
  debug: {
    timing: true, // measure concern/listener execution time
    timingThreshold: 16, // warn when an operation exceeds this (ms)
  },
})
```

## Record and Wildcard Paths

Types with `Record<string, V>` are supported. Dynamic keys use `[*]` wildcard notation in paths:

```typescript
type State = {
  users: Record<string, { name: string; age: number }>
}

// Type-safe paths include wildcards
type Paths = DeepKey<State>
// 'users' | 'users.[*]' | 'users.[*].name' | 'users.[*].age'

// Access with concrete keys at runtime
const [name, setName] = store.useStore("users.alice.name")
```

See `docs/WILDCARD_UTILITIES_EXAMPLE.md` and `docs/WILD_FUNCTION_GUIDE.md` for utility helpers.

## Advanced Utilities

```typescript
import { dot, is, applyChangesToObject, evaluateBoolLogic } from "@sladg/apex-state"

// Deep path access (for performance-critical code outside React)
dot.get(state, "user.address.street")
dot.set(state, "user.address.street", "New Street")

// Type predicates
is.object(value)
is.array(value)
is.not.nil(value)

// Apply changes immutably (returns new object)
const newState = applyChangesToObject(state, [
  ["user.name", "Bob"],
  ["user.age", 31],
])

// Evaluate BoolLogic outside concerns
const disabled = evaluateBoolLogic({ IS_EQUAL: ["status", "submitted"] }, state)
```

## WASM Development

This project includes Rust-to-WebAssembly compilation for performance-critical features.

### Prerequisites

1. **Rust Toolchain** (≥1.93.0):
   ```bash
   curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
   ```

2. **WASM Target**:
   ```bash
   rustup target add wasm32-unknown-unknown
   ```

3. **wasm-pack** (≥0.11.0):
   ```bash
   cargo install wasm-pack
   # OR on macOS:
   brew install wasm-pack
   ```

### Build Workflow

```bash
# Navigate to Rust project
cd rust

# Development build (fast, includes debug info)
wasm-pack build --target bundler --dev

# Production build (optimized)
wasm-pack build --target bundler --release

# Run Rust tests
cargo test
```

Build output goes to `pkg/` directory with:
- `*.wasm` - WebAssembly binary
- `*.js` - JavaScript bindings
- `*.d.ts` - TypeScript definitions
- `package.json` - Package metadata

### Development Cycle

1. **Write Rust Code** (`rust/src/lib.rs`):
   ```rust
   use wasm_bindgen::prelude::*;

   #[wasm_bindgen]
   pub fn my_function(input: &str) -> String {
       format!("Processed: {}", input)
   }
   ```

2. **Build WASM Module**:
   ```bash
   cd rust && wasm-pack build --target bundler --dev
   ```

3. **Import in TypeScript**:
   ```typescript
   import init, { my_function } from '../rust/pkg/apex_state_wasm';

   // Initialize WASM module
   await init();

   // Call exported functions
   const result = my_function('hello');
   ```

4. **Test in Browser**:
   ```bash
   npm run dev
   ```

### Troubleshooting

**Version Mismatch Errors**:
- Use wasm-pack (not manual wasm-bindgen CLI) to ensure version alignment

**Slow Builds on Linux**:
- wasm-opt can be slow. Use development builds without optimization:
  ```bash
  wasm-pack build --target bundler -- --no-opt
  ```

**Missing LLD Linker**:
- Install Rust via rustup (not package managers) to ensure wasm-ld is available

**Large WASM Files**:
- Development builds include debug symbols. Use `--release` for production:
  ```bash
  wasm-pack build --target bundler --release
  ```

**TypeScript Import Errors**:
- Ensure `pkg/` is generated before importing
- Check that Vite config supports WASM imports (already configured via `esbuild-plugin-wasm`)

## Documentation

See `docs/README.md` for a full index. Key guides:

| Guide                           | Audience                        |
| ------------------------------- | ------------------------------- |
| `docs/guides/ARCHITECTURE.md`   | Core architecture and data flow |
| `docs/guides/CONCERNS_GUIDE.md` | Building and testing concerns   |
| `docs/guides/STORE_HOOKS.md`    | Hook reference and patterns     |
| `docs/SIDE_EFFECTS_GUIDE.md`    | Side effects API reference      |
| `docs/DEBUG_TIMING.md`          | Debug and performance tooling   |
