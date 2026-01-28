# @sladg/apex-state

---

_Built with AI._

---

Reactive state management for React where you declare what your fields need (validation, visibility, labels) and the store handles the rest.

## Why

Complex form state typically means scattered validation logic, conditional rendering spread across components, and hundreds of unit tests covering every edge case.

Apex-state inverts this: **define behavior as static configuration, not imperative code**.

The core functions (`evaluateBoolLogic`, `zodValidation`, etc.) are tested once. Your application config is just constants - Lego bricks snapped together. You test the bricks, not every possible tower.

## How

Built on [valtio](https://github.com/pmndrs/valtio) for proxy-based state and [valtio-reactive](https://github.com/jotaijs/valtio-reactive) for automatic dependency tracking.

- **valtio**: Mutable-style API, immutable under the hood. Write `state.count++`, get efficient React updates.
- **valtio-reactive**: Effects that auto-track which state paths they read. Change `state.user.name` → only effects reading that path re-run.

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
      zodValidation: { schema: z.number().min(1).max(100) },
      disabledWhen: { condition: { IS_EQUAL: ['status', 'submitted'] } },
    },
    'shipping.address': {
      zodValidation: { schema: z.string().min(10) },
      visibleWhen: { condition: { EXISTS: 'shipping.express' } },
      dynamicLabel: { template: 'Address ({{shipping.express}} ? Express : Standard)' },
    },
    'payment.cardNumber': {
      zodValidation: { schema: z.string().regex(/^\d{16}$/) },
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
        className={quantityConcerns.zodValidation ? 'valid' : 'invalid'}
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

| Concern              | Props                      | Returns             | Description              |
| -------------------- | -------------------------- | ------------------- | ------------------------ |
| `zodValidation`      | `{ schema: ZodSchema }`    | `boolean`           | Schema validation        |
| `validationState`    | `{ schema: ZodSchema }`    | `{ valid, errors }` | Full validation state    |
| `disabledWhen`       | `{ condition: BoolLogic }` | `boolean`           | Conditional disable      |
| `readonlyWhen`       | `{ condition: BoolLogic }` | `boolean`           | Conditional readonly     |
| `visibleWhen`        | `{ condition: BoolLogic }` | `boolean`           | Conditional visibility   |
| `dynamicLabel`       | `{ template: string }`     | `string`            | Interpolated label       |
| `dynamicTooltip`     | `{ template: string }`     | `string`            | Interpolated tooltip     |
| `dynamicPlaceholder` | `{ template: string }`     | `string`            | Interpolated placeholder |

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

```typescript
// useState-like access
const [value, setValue] = store.useStore("path.to.field")

// Object API for forms
const field = store.useFieldStore("path.to.field")
field.value // current value
field.setValue(v) // update value

// Evaluated concerns for a path
const concerns = store.useFieldConcerns("path.to.field")
concerns.zodValidation // boolean
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
