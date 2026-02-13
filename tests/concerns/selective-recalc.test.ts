/**
 * TEST-001: Single Field Change - Selective Re-calculation
 *
 * Priority: P0 (Critical)
 *
 * Validates that only relevant concerns recalculate when state changes
 */

import { beforeEach, describe, expect, it } from 'vitest'
import { z } from 'zod'

import { createGenericStore } from '../../src'
import type { StoreInstance } from '../../src/core/types'
import { flush, renderWithStore } from '../utils/react'

interface AppState {
  products: {
    'item-1': {
      price: number
      sku: string
      quantity: number | null
      status: string
    }
    'item-2': {
      price: number
      sku: string
      quantity: number | null
      status: string
    }
  }
  shipping: { rate: number; method: string }
}

const initialState: AppState = {
  products: {
    'item-1': {
      price: 29.99,
      sku: 'SHIRT-BLU-M',
      quantity: null,
      status: 'in-stock',
    },
    'item-2': {
      price: 49.99,
      sku: 'PANTS-BLK-L',
      quantity: null,
      status: 'in-stock',
    },
  },
  shipping: { rate: 5.99, method: 'standard' },
}

const concerns = {
  'products.item-1.price': {
    validationState: { schema: z.number().min(0).max(999) },
    disabledWhen: {
      condition: { IS_EQUAL: ['products.item-1.status', 'out-of-stock'] },
    },
    dynamicTooltip: {
      template: 'Item 1 Price: {{products.item-1.price}}',
    },
  },
  'products.item-2.price': {
    validationState: { schema: z.number().min(0).max(999) },
    disabledWhen: {
      condition: { IS_EQUAL: ['products.item-2.status', 'out-of-stock'] },
    },
    dynamicTooltip: {
      template: 'Item 2 Price: {{products.item-2.price}}',
    },
  },
}

describe('TEST-001: Selective Re-calculation', () => {
  let storeInstance: StoreInstance<AppState>

  beforeEach(async () => {
    const store = createGenericStore<AppState>()
    const result = renderWithStore(store, structuredClone(initialState), {
      concerns,
    })
    storeInstance = result.storeInstance
    await flush()
  })

  it('AC1: Only item-1 concerns recalculate when item-1 price changes', async () => {
    storeInstance.state.products['item-1'].price = 39.99
    await flush()

    const item1Concerns = storeInstance._concerns['products.item-1.price']
    const item2Concerns = storeInstance._concerns['products.item-2.price']

    expect(item1Concerns?.['validationState']).toBeDefined()
    expect(item1Concerns?.['disabledWhen']).toBeDefined()
    expect(item1Concerns?.['dynamicTooltip']).toBe('Item 1 Price: 39.99')

    // item-2 remains at initial values
    expect(item2Concerns?.['dynamicTooltip']).toBe('Item 2 Price: 49.99')
  })

  it('AC2: All item-1 concerns recalculate', async () => {
    storeInstance.state.products['item-1'].price = 39.99
    await flush()

    const item1Concerns = storeInstance._concerns['products.item-1.price']

    expect(item1Concerns?.['validationState']).toBeDefined()
    expect(item1Concerns?.['disabledWhen']).toBeDefined()
    expect(item1Concerns?.['dynamicTooltip']).toBeDefined()
  })

  it('AC3: Correct values after recalculation', async () => {
    storeInstance.state.products['item-1'].price = 39.99
    await flush()

    const item1Concerns = storeInstance._concerns['products.item-1.price']
    const validation = item1Concerns?.['validationState'] as
      | { isError: boolean; errors: unknown[] }
      | undefined
    const tooltip = item1Concerns?.['dynamicTooltip']

    expect(validation?.isError).toBe(false)
    expect(tooltip).toBe('Item 1 Price: 39.99')
  })

  it('Round-trip: price change → concern value available', async () => {
    const initialTooltip =
      storeInstance._concerns['products.item-1.price']?.['dynamicTooltip']
    expect(initialTooltip).toBe('Item 1 Price: 29.99')

    storeInstance.state.products['item-1'].price = 39.99
    await flush()

    const updatedTooltip =
      storeInstance._concerns['products.item-1.price']?.['dynamicTooltip']
    expect(updatedTooltip).toBe('Item 1 Price: 39.99')
  })
})
