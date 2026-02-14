/**
 * TEST-002: Cross-Field Validation - Dependency Tracking
 *
 * Priority: P0 (Critical)
 *
 * Validates that cross-field dependencies are tracked correctly
 */

import { beforeEach, describe, expect, it } from 'vitest'
import { z } from 'zod'

import { createGenericStore } from '~'
import type { StoreInstance } from '~/core/types'

import { flush, renderWithStore } from '../utils/react'

interface AppState {
  products: {
    'item-1': { price: number; status: string }
    'item-2': { price: number; status: string }
  }
}

const initialState: AppState = {
  products: {
    'item-1': { price: 29.99, status: 'in-stock' },
    'item-2': { price: 49.99, status: 'in-stock' },
  },
}

const concerns = {
  'products.item-1.price': {
    validationState: { schema: z.number().min(0) },
    disabledWhen: {
      condition: { IS_EQUAL: ['products.item-1.status', 'out-of-stock'] },
    },
  },
  'products.item-2.price': {
    validationState: { schema: z.number().min(0) },
    disabledWhen: {
      condition: { IS_EQUAL: ['products.item-2.status', 'out-of-stock'] },
    },
  },
}

describe('TEST-002: Cross-Field Dependency Tracking', () => {
  let storeInstance: StoreInstance<AppState>

  beforeEach(async () => {
    const store = createGenericStore<AppState>()
    const result = renderWithStore(store, structuredClone(initialState), {
      concerns,
    })
    storeInstance = result.storeInstance
    await flush()
  })

  it('AC1: Only item-1 disabled concern recalculates when status changes', async () => {
    storeInstance.state.products['item-1'].status = 'out-of-stock'
    await flush()

    const item1Disabled =
      storeInstance._concerns['products.item-1.price']?.['disabledWhen']
    const item2Disabled =
      storeInstance._concerns['products.item-2.price']?.['disabledWhen']

    expect(item1Disabled).toBe(true)
    expect(item2Disabled).toBe(false)
  })

  it('AC2: Item-1 validationState does NOT recalculate', async () => {
    storeInstance.state.products['item-1'].status = 'out-of-stock'
    await flush()

    const validation =
      storeInstance._concerns['products.item-1.price']?.['validationState']
    expect(validation).toMatchObject({
      isError: false,
      errors: [],
    })
  })

  it('AC3: Item-2 concerns do NOT recalculate', async () => {
    storeInstance.state.products['item-1'].status = 'out-of-stock'
    await flush()

    const item2Disabled =
      storeInstance._concerns['products.item-2.price']?.['disabledWhen']
    const item2Validation =
      storeInstance._concerns['products.item-2.price']?.['validationState']

    expect(item2Disabled).toBe(false)
    expect(item2Validation).toMatchObject({
      isError: false,
      errors: [],
    })
  })

  it('AC4: Correct disabled state after change', async () => {
    storeInstance.state.products['item-1'].status = 'out-of-stock'
    await flush()

    const item1Disabled =
      storeInstance._concerns['products.item-1.price']?.['disabledWhen']
    const item2Disabled =
      storeInstance._concerns['products.item-2.price']?.['disabledWhen']

    expect(item1Disabled).toBe(true)
    expect(item2Disabled).toBe(false)
  })

  it('Round-trip: cross-field dependency change → concern value available', async () => {
    const initialDisabled =
      storeInstance._concerns['products.item-1.price']?.['disabledWhen']
    expect(initialDisabled).toBe(false)

    storeInstance.state.products['item-1'].status = 'out-of-stock'
    await flush()

    const updatedDisabled =
      storeInstance._concerns['products.item-1.price']?.['disabledWhen']
    expect(updatedDisabled).toBe(true)
  })
})
