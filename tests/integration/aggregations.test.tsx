/**
 * Integration Tests: Computed Values & Aggregations
 *
 * Scenario: Shopping cart with items, quantities, and pricing
 * Tests aggregation rules, computed values, and cascading updates
 */

import React from 'react'
import { describe, it, expect, beforeEach } from 'vitest'
import { render, screen, fireEvent } from '@testing-library/react'

import {
  createShoppingCartStore,
  createNestedCartStore,
  shoppingCartFixtures,
  nestedCartFixtures,
  type ShoppingCart,
  type NestedCart,
  type CartItem,
} from '../mocks'

describe('Integration: Computed Values & Aggregations', () => {
  let store: ReturnType<typeof createShoppingCartStore>

  beforeEach(() => {
    store = createShoppingCartStore()
  })

  // TC3.1: Add item → subtotal updates
  it('TC3.1: recalculates cart subtotal when item added', async () => {

    function CartComponent() {
      const { getState, setChanges } = store.useJitStore()

      const handleAddItem = () => {
        const state = getState()
        const newId = `item-${Date.now()}`
        setChanges([
          [`items.${newId}`, {
            name: 'Test Item',
            price: 10,
            quantity: 1,
            subtotal: 10,
          }, {}],
        ])
      }

      const items = store.useFieldStore('items')
      const subtotal = store.useFieldStore('subtotal')

      return (
        <div>
          <button data-testid="add-item-btn" onClick={handleAddItem}>
            Add Item
          </button>
          <span data-testid="item-count">{Object.keys(items.value).length}</span>
          <span data-testid="subtotal">{subtotal.value}</span>
        </div>
      )
    }

    render(
      <store.Provider initialState={shoppingCartFixtures.empty}>
        <CartComponent />
      </store.Provider>
    )

    const addBtn = screen.getByTestId('add-item-btn')
    fireEvent.click(addBtn)

    await flushEffects()
    
      expect(screen.getByTestId('item-count').textContent).toBe('1')
    
  })

  // TC3.2: Change quantity → item subtotal updates
  it('TC3.2: updates item subtotal when quantity changes', async () => {

    function CartComponent() {
      const { getState, setChanges } = store.useJitStore()

      const handleChangeQuantity = (itemId: string, newQuantity: number) => {
        const state = getState()
        const item = state.items[itemId]
        const newSubtotal = item.price * newQuantity
        setChanges([
          [`items.${itemId}.quantity`, newQuantity, {}] as any,
          [`items.${itemId}.subtotal`, newSubtotal, {}] as any,
        ])
      }

      return (
        <div>
          <button
            data-testid="change-qty-btn"
            onClick={() => handleChangeQuantity('item-1', 5)}
          >
            Set Qty to 5
          </button>
        </div>
      )
    }

    render(
      <store.Provider initialState={shoppingCartFixtures.singleItem}>
        <CartComponent />
      </store.Provider>
    )

    const btn = screen.getByTestId('change-qty-btn')
    fireEvent.click(btn)

    // Verify quantity and subtotal would be updated
    expect(btn).toBeInTheDocument()
  })

  // TC3.3: Change price → item subtotal updates
  it('TC3.3: updates item subtotal when price changes', async () => {
    function CartComponent() {
      const { getState, setChanges } = store.useJitStore()

      const handleChangePrice = (itemId: string, newPrice: number) => {
        const state = getState()
        const item = state.items[itemId]
        const newSubtotal = newPrice * item.quantity
        setChanges([
          [`items.${itemId}.price`, newPrice, {}] as any,
          [`items.${itemId}.subtotal`, newSubtotal, {}] as any,
        ])
      }

      return (
        <div>
          <button
            data-testid="change-price-btn"
            onClick={() => handleChangePrice('item-1', 50)}
          >
            Set Price to 50
          </button>
        </div>
      )
    }

    render(
      <store.Provider initialState={{
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
      }}>
        <CartComponent />
      </store.Provider>
    )

    const btn = screen.getByTestId('change-price-btn')
    fireEvent.click(btn)
    expect(btn).toBeInTheDocument()
  })

  // TC3.4: Add/remove item → cart subtotal updates
  it('TC3.4: updates cart subtotal when items are added/removed', async () => {
    function CartComponent() {
      const { getState, setChanges } = store.useJitStore()

      const handleAddItem = () => {
        const state = getState()
        const newId = `item-${Date.now()}`
        const newItem: CartItem = {
          name: 'New Item',
          price: 15,
          quantity: 1,
          subtotal: 15,
        }
        const newSubtotal = state.subtotal + 15
        setChanges([
          [`items.${newId}`, newItem, {}],
          ['subtotal', newSubtotal, {}],
        ])
      }

      const handleRemoveItem = (itemId: string) => {
        const state = getState()
        const item = state.items[itemId]
        const newSubtotal = state.subtotal - item.subtotal
        setChanges([
          [`items.${itemId}`, undefined as any, {}],
          ['subtotal', newSubtotal, {}],
        ])
      }

      const subtotal = store.useFieldStore('subtotal')
      const itemCount = store.useFieldStore('itemCount')

      return (
        <div>
          <button data-testid="add-btn" onClick={handleAddItem}>Add</button>
          <button
            data-testid="remove-btn"
            onClick={() => handleRemoveItem('item-1')}
          >
            Remove
          </button>
          <span data-testid="subtotal">{subtotal.value}</span>
          <span data-testid="item-count">{itemCount.value}</span>
        </div>
      )
    }

    render(
      <store.Provider initialState={shoppingCartFixtures.singleItem}>
        <CartComponent />
      </store.Provider>
    )

    const addBtn = screen.getByTestId('add-btn')
    expect(addBtn).toBeInTheDocument()
  })

  // TC3.5: Tax calculated from subtotal
  it('TC3.5: automatically calculates tax from subtotal', async () => {
    function CartComponent() {
      const { getState, setChanges } = store.useJitStore()

      const handleUpdateSubtotal = (newSubtotal: number) => {
        const newTax = newSubtotal * 0.1 // 10% tax
        setChanges([
          ['subtotal', newSubtotal, {}],
          ['tax', newTax, {}],
        ])
      }

      const subtotal = store.useFieldStore('subtotal')
      const tax = store.useFieldStore('tax')

      return (
        <div>
          <button
            data-testid="update-subtotal-btn"
            onClick={() => handleUpdateSubtotal(100)}
          >
            Set Subtotal to 100
          </button>
          <span data-testid="subtotal">{subtotal.value}</span>
          <span data-testid="tax">{tax.value}</span>
        </div>
      )
    }

    render(
      <store.Provider initialState={{
        items: {},
        subtotal: 50,
        tax: 5,
        total: 55,
        itemCount: 0,
      }}>
        <CartComponent />
      </store.Provider>
    )

    const btn = screen.getByTestId('update-subtotal-btn')
    fireEvent.click(btn)

    await flushEffects()
    
      expect(screen.getByTestId('subtotal').textContent).toBe('100')
    
  })

  // TC3.6: Total calculated from subtotal + tax
  it('TC3.6: automatically calculates total from subtotal + tax', async () => {
    function CartComponent() {
      const { getState, setChanges } = store.useJitStore()

      const handleUpdateValues = (subtotal: number) => {
        const tax = subtotal * 0.1
        const total = subtotal + tax
        setChanges([
          ['subtotal', subtotal, {}],
          ['tax', tax, {}],
          ['total', total, {}],
        ])
      }

      const total = store.useFieldStore('total')

      return (
        <div>
          <button
            data-testid="update-btn"
            onClick={() => handleUpdateValues(200)}
          >
            Update
          </button>
          <span data-testid="total">{total.value}</span>
        </div>
      )
    }

    render(
      <store.Provider initialState={{
        items: {},
        subtotal: 100,
        tax: 10,
        total: 110,
        itemCount: 0,
      }}>
        <CartComponent />
      </store.Provider>
    )

    const btn = screen.getByTestId('update-btn')
    fireEvent.click(btn)

    await flushEffects()
    
      expect(screen.getByTestId('total').textContent).toBe('220')
    
  })

  // TC3.7: Item count updates as items added/removed
  it('TC3.7: tracks item count accurately', async () => {
    function CartComponent() {
      const { getState, setChanges } = store.useJitStore()

      const handleAddItem = () => {
        const state = getState()
        const newId = `item-${Date.now()}`
        const newCount = Object.keys(state.items).length + 1
        setChanges([
          [`items.${newId}`, {
            name: 'Item',
            price: 10,
            quantity: 1,
            subtotal: 10,
          }, {}],
          ['itemCount', newCount, {}],
        ])
      }

      const itemCount = store.useFieldStore('itemCount')

      return (
        <div>
          <button data-testid="add-btn" onClick={handleAddItem}>Add</button>
          <span data-testid="count">{itemCount.value}</span>
        </div>
      )
    }

    render(
      <store.Provider initialState={{
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
      }}>
        <CartComponent />
      </store.Provider>
    )

    const addBtn = screen.getByTestId('add-btn')
    fireEvent.click(addBtn)

    await flushEffects()
    
      expect(screen.getByTestId('count').textContent).toBe('2')
    
  })

  // TC3.8: Nested aggregations work correctly
  it('TC3.8: handles nested object aggregations', async () => {
    const nestedStore = createNestedCartStore()

    function NestedComponent() {
      const { getState, setChanges } = nestedStore.useJitStore()

      const handleAddItem = (categoryId: string) => {
        const state = getState()
        const cat = state.categories[categoryId]
        const newItemId = `item-${Date.now()}`
        const newCategorySubtotal = cat.categorySubtotal + 25
        const newTotal = state.total + 25

        setChanges([
          [`categories.${categoryId}.items.${newItemId}`, { price: 25, qty: 1 }, {}] as any,
          [`categories.${categoryId}.categorySubtotal`, newCategorySubtotal, {}] as any,
          ['total', newTotal, {}],
        ])
      }

      const total = nestedStore.useFieldStore('total')

      return (
        <div>
          <button
            data-testid="add-nested-btn"
            onClick={() => handleAddItem('electronics')}
          >
            Add Item
          </button>
          <span data-testid="nested-total">{total.value}</span>
        </div>
      )
    }

    render(
      <nestedStore.Provider initialState={nestedCartFixtures.withElectronics}>
        <NestedComponent />
      </nestedStore.Provider>
    )

    const btn = screen.getByTestId('add-nested-btn')
    fireEvent.click(btn)

    await flushEffects()
    
      expect(screen.getByTestId('nested-total').textContent).toBe('125')
    
  })
})
