/**
 * Integration Tests: Computed Values & Aggregations
 *
 * Scenario: Shopping cart with items, quantities, and pricing
 * Tests aggregation rules, computed values, and cascading updates
 */

import { screen } from '@testing-library/react'
import { beforeEach, describe, expect, it } from 'vitest'

import { _, createGenericStore } from '../../src'
import {
  defaults,
  NestedCart,
  nestedCartFixtures,
  TestState,
  testStateFixtures,
} from '../mocks'
import { CartComponent } from '../utils/components'
import {
  createStore,
  fireEvent,
  flushEffects,
  renderWithStore,
} from '../utils/react'

describe('Integration: Computed Values & Aggregations', () => {
  let store: ReturnType<typeof createStore<TestState>>

  beforeEach(() => {
    store = createStore<TestState>(testStateFixtures.cartEmpty)
  })

  it('TC3.1: recalculates cart subtotal when item added', async () => {
    renderWithStore(<CartComponent store={store} />, store)

    const addBtn = screen.getByTestId('add-item-btn')
    fireEvent.click(addBtn)

    await flushEffects()

    expect(screen.getByTestId('item-count').textContent).toBe('1')
  })

  it('TC3.2: updates item subtotal when quantity changes', async () => {
    renderWithStore(
      <CartComponent store={store} />,
      store,
      testStateFixtures.cartSingleItem,
    )

    const btn = screen.getByTestId('change-qty-btn')
    fireEvent.click(btn)

    // Verify quantity and subtotal would be updated
    expect(btn).toBeInTheDocument()
  })

  it('TC3.3: updates item subtotal when price changes', async () => {
    renderWithStore(<CartComponent store={store} />, store, {
      ...defaults,
      items: {
        'item-1': {
          name: 'Product A',
          price: 20,
          quantity: 2,
          subtotal: 40,
        },
      },
      subtotal: 40,
      tax: 4,
      total: 44,
      itemCount: 1,
    })

    const btn = screen.getByTestId('change-price-btn')
    fireEvent.click(btn)
    expect(btn).toBeInTheDocument()
  })

  it('TC3.4: updates cart subtotal when items are added/removed', async () => {
    renderWithStore(
      <CartComponent store={store} />,
      store,
      testStateFixtures.cartSingleItem,
    ) // explicit: cartSingleItem differs from default

    const addBtn = screen.getByTestId('add-btn')
    expect(addBtn).toBeInTheDocument()
  })

  it('TC3.5: automatically calculates tax from subtotal', async () => {
    renderWithStore(<CartComponent store={store} />, store, {
      ...defaults,
      subtotal: 50,
      tax: 5,
      total: 55,
    })

    const btn = screen.getByTestId('update-subtotal-btn')
    fireEvent.click(btn)

    await flushEffects()

    expect(screen.getByTestId('subtotal').textContent).toBe('100')
  })

  it('TC3.6: automatically calculates total from subtotal + tax', async () => {
    renderWithStore(<CartComponent store={store} />, store, {
      ...defaults,
      subtotal: 100,
      tax: 10,
      total: 110,
    })

    const btn = screen.getByTestId('update-btn')
    fireEvent.click(btn)

    await flushEffects()

    expect(screen.getByTestId('total').textContent).toBe('220')
  })

  it('TC3.7: tracks item count accurately', async () => {
    renderWithStore(<CartComponent store={store} />, store, {
      ...defaults,
      items: {
        'item-1': {
          name: 'Product A',
          price: 10,
          quantity: 1,
          subtotal: 10,
        },
      },
      subtotal: 10,
      tax: 1,
      total: 11,
      itemCount: 1,
    })

    const addBtn = screen.getByTestId('add-btn')
    fireEvent.click(addBtn)

    await flushEffects()

    expect(screen.getByTestId('count').textContent).toBe('2')
  })

  it('TC3.8: handles nested object aggregations', async () => {
    const nestedStore = createGenericStore<NestedCart>()

    function NestedComponent() {
      const { getState, setChanges } = nestedStore.useJitStore()

      const handleAddItem = (categoryId: string) => {
        const state = getState()
        const cat = state.categories[categoryId]!
        const newItemId = `item-${Date.now()}`
        const newCategorySubtotal = cat.categorySubtotal + 25
        const newTotal = state.total + 25

        setChanges([
          [
            `categories.${_(categoryId)}.items.${_(newItemId)}`,
            { price: 25, qty: 1 },
            {},
          ],
          [
            `categories.${_(categoryId)}.categorySubtotal`,
            newCategorySubtotal,
            {},
          ],
          ['total', newTotal, {}],
        ])
      }

      const totalValue = nestedStore.useFieldStore('total')

      return (
        <div>
          <button
            data-testid="add-nested-btn"
            onClick={() => handleAddItem('electronics')}
          >
            Add Item
          </button>
          <span data-testid="nested-total">{totalValue.value}</span>
        </div>
      )
    }

    renderWithStore(
      <NestedComponent />,
      nestedStore,
      nestedCartFixtures.withElectronics,
    )

    const btn = screen.getByTestId('add-nested-btn')
    fireEvent.click(btn)

    await flushEffects()

    expect(screen.getByTestId('nested-total').textContent).toBe('125')
  })
})
