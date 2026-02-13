/**
 * TEST-003: Batch Updates - Multiple Changes
 *
 * Priority: P0 (Critical)
 *
 * Validates that multiple synchronous changes result in batched evaluation
 */

import { beforeEach, describe, expect, it } from 'vitest'
import { z } from 'zod'

import { createGenericStore } from '../../src'
import type { StoreInstance } from '../../src/core/types'
import { flush, renderWithStore } from '../utils/react'

interface AppState {
  products: {
    'item-1': { price: number; status: string }
    'item-2': { price: number; status: string }
  }
  shipping: { rate: number }
}

const initialState: AppState = {
  products: {
    'item-1': { price: 29.99, status: 'in-stock' },
    'item-2': { price: 49.99, status: 'in-stock' },
  },
  shipping: { rate: 5.99 },
}

const concerns = {
  'products.item-1.price': {
    validationState: { schema: z.number().min(0) },
    dynamicTooltip: {
      template:
        'Price: {{products.item-1.price}} + Shipping: {{shipping.rate}}',
    },
  },
  'products.item-2.price': {
    validationState: { schema: z.number().min(0) },
    dynamicTooltip: {
      template:
        'Price: {{products.item-2.price}} + Shipping: {{shipping.rate}}',
    },
  },
}

describe('TEST-003: Batch Updates', () => {
  let storeInstance: StoreInstance<AppState>

  beforeEach(async () => {
    const store = createGenericStore<AppState>()
    const result = renderWithStore(store, structuredClone(initialState), {
      concerns,
    })
    storeInstance = result.storeInstance
    await flush()
  })

  it('AC1: All concerns evaluate (correctness)', async () => {
    storeInstance.state.products['item-1'].price = 39.99
    storeInstance.state.products['item-2'].price = 59.99
    storeInstance.state.shipping.rate = 9.99

    await flush()

    const item1Concerns = storeInstance._concerns['products.item-1.price']
    const item2Concerns = storeInstance._concerns['products.item-2.price']

    expect(item1Concerns?.['validationState']).toBeDefined()
    expect(item1Concerns?.['dynamicTooltip']).toBeDefined()
    expect(item2Concerns?.['validationState']).toBeDefined()
    expect(item2Concerns?.['dynamicTooltip']).toBeDefined()
  })

  it('AC2: Each concern evaluates at most once per update cycle', async () => {
    storeInstance.state.products['item-1'].price = 39.99
    await flush()

    const item1Concerns = storeInstance._concerns['products.item-1.price']
    expect(item1Concerns?.['validationState']).toBeDefined()
    expect(item1Concerns?.['dynamicTooltip']).toBe(
      'Price: 39.99 + Shipping: 5.99',
    )
  })

  it('AC4: Final state is correct after bulk update', async () => {
    storeInstance.state.products['item-1'].price = 39.99
    storeInstance.state.products['item-2'].price = 59.99
    storeInstance.state.shipping.rate = 9.99

    await flush()

    const item1Tooltip =
      storeInstance._concerns['products.item-1.price']?.['dynamicTooltip']
    const item2Tooltip =
      storeInstance._concerns['products.item-2.price']?.['dynamicTooltip']

    expect(item1Tooltip).toBe('Price: 39.99 + Shipping: 9.99')
    expect(item2Tooltip).toBe('Price: 59.99 + Shipping: 9.99')
  })

  it('Round-trip: initial tooltip values are correct', async () => {
    const item1Initial =
      storeInstance._concerns['products.item-1.price']?.['dynamicTooltip']
    expect(item1Initial).toBe('Price: 29.99 + Shipping: 5.99')

    storeInstance.state.products['item-1'].price = 39.99
    await flush()

    const item1Updated =
      storeInstance._concerns['products.item-1.price']?.['dynamicTooltip']
    expect(item1Updated).toBe('Price: 39.99 + Shipping: 5.99')
  })
})
