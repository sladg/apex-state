/**
 * Basic store usage examples — Provider, hooks, and core API.
 *
 * These examples type-check against the real @sladg/apex-state exports.
 * If the API changes, `tsc --noEmit` will catch breakage here.
 */

import { createGenericStore } from '@sladg/apex-state'
import { z } from 'zod'

// ---------------------------------------------------------------------------
// Shared state type used across examples
// ---------------------------------------------------------------------------

interface OrderState {
  product: { name: string; quantity: number; price: number }
  shipping: { address: string; express: boolean; standard: boolean }
  payment: { method: 'card' | 'cash'; cardNumber: string }
  status: 'draft' | 'submitted'
}

const store = createGenericStore<OrderState>()

// @llms-example: Order form with Provider, validation, BoolLogic concerns, and flip side effects
const OrderForm = () => {
  // Declare side effects
  store.useSideEffects('order', {
    flipPaths: [['shipping.express', 'shipping.standard']],
  })

  // Declare concerns — just data, no logic to test
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
      disabled={disabledWhen as boolean | undefined}
      className={
        (validationState as { isError?: boolean } | undefined)?.isError
          ? 'error'
          : ''
      }
    />
  )
}

const App = () => (
  <store.Provider
    initialState={{
      product: { name: 'Widget', quantity: 1, price: 29.99 },
      shipping: { address: '', express: false, standard: true },
      payment: { method: 'card', cardNumber: '' },
      status: 'draft',
    }}
  >
    <OrderForm />
  </store.Provider>
)
// @llms-example-end

// @llms-example: createGenericStore returns Provider, useStore, useFieldStore, useJitStore, and more
const {
  Provider, // React context — accepts initialState
  useFieldStore, // { value, setValue, ...concerns } for a path
  useStore, // [value, setValue] tuple for a path
  useJitStore, // { proxyValue, setChanges, getState } for bulk ops
  useSideEffects, // register sync/flip/aggregation/listeners
  useConcerns, // register validation/BoolLogic/custom concerns
  withConcerns, // typed concern selection
} = createGenericStore<OrderState>()
// @llms-example-end

// Suppress unused variable warnings — these are documentation-only examples
void OrderForm
void App
void Provider
void useFieldStore
void useStore
void useJitStore
void useSideEffects
void useConcerns
void withConcerns
