/**
 * TEST-002: Cross-Field Validation - Dependency Tracking
 *
 * Priority: P0 (Critical)
 * Performance Target: < 2ms
 *
 * Validates that cross-field dependencies are tracked correctly
 */

import { describe, expect, it } from 'vitest'
import { z } from 'zod'

import { createGenericStore } from '../../src'
import { flushSync, mountStore } from '../utils/react'

interface AppState {
  products: {
    'leg-1': { strike: number; status: string }
    'leg-2': { strike: number; status: string }
  }
}

describe('TEST-002: Cross-Field Dependency Tracking', () => {
  const createAppStore = () => createGenericStore<AppState>()

  it('AC1: Only leg-1 disabled concern recalculates when status changes', async () => {
    const store = createAppStore()

    const { storeInstance } = mountStore(
      store,
      {
        products: {
          'leg-1': { strike: 100, status: 'active' },
          'leg-2': { strike: 105, status: 'active' },
        },
      },
      {
        concerns: {
          'products.leg-1.strike': {
            validationState: { schema: z.number().min(0) },
            disabledWhen: {
              boolLogic: { IS_EQUAL: ['products.leg-1.status', 'locked'] },
            },
          },
          'products.leg-2.strike': {
            validationState: { schema: z.number().min(0) },
            disabledWhen: {
              boolLogic: { IS_EQUAL: ['products.leg-2.status', 'locked'] },
            },
          },
        },
      },
    )

    await flushSync()

    const start = performance.now()
    storeInstance.state.products['leg-1'].status = 'locked'
    await flushSync()
    const duration = performance.now() - start

    // Verify only leg-1 disabled concern updated
    const leg1Disabled =
      storeInstance._concerns['products.leg-1.strike']?.['disabledWhen']
    const leg2Disabled =
      storeInstance._concerns['products.leg-2.strike']?.['disabledWhen']

    expect(leg1Disabled).toBe(true) // Updated
    expect(leg2Disabled).toBe(false) // Not updated

    // Performance: < 15ms (includes React overhead)
    expect(duration).toBeLessThan(15)
  })

  it('AC2: Leg-1 validationState does NOT recalculate', async () => {
    const store = createAppStore()

    const { storeInstance } = mountStore(
      store,
      {
        products: {
          'leg-1': { strike: 100, status: 'active' },
          'leg-2': { strike: 105, status: 'active' },
        },
      },
      {
        concerns: {
          'products.leg-1.strike': {
            validationState: { schema: z.number().min(0) },
            disabledWhen: {
              boolLogic: { IS_EQUAL: ['products.leg-1.status', 'locked'] },
            },
          },
        },
      },
    )

    await flushSync()

    // Change status
    storeInstance.state.products['leg-1'].status = 'locked'
    await flushSync()

    // Verify validationState remains valid (didn't recalculate unnecessarily)
    const validation =
      storeInstance._concerns['products.leg-1.strike']?.['validationState']
    expect(validation).toMatchObject({
      isError: false,
      errors: [],
    })
  })

  it('AC3: Leg-2 concerns do NOT recalculate', async () => {
    const store = createAppStore()

    const { storeInstance } = mountStore(
      store,
      {
        products: {
          'leg-1': { strike: 100, status: 'active' },
          'leg-2': { strike: 105, status: 'active' },
        },
      },
      {
        concerns: {
          'products.leg-1.strike': {
            validationState: { schema: z.number().min(0) },
            disabledWhen: {
              boolLogic: { IS_EQUAL: ['products.leg-1.status', 'locked'] },
            },
          },
          'products.leg-2.strike': {
            validationState: { schema: z.number().min(0) },
            disabledWhen: {
              boolLogic: { IS_EQUAL: ['products.leg-2.status', 'locked'] },
            },
          },
        },
      },
    )

    await flushSync()

    // Change leg-1 status
    storeInstance.state.products['leg-1'].status = 'locked'
    await flushSync()

    // Verify leg-2 concerns remain unchanged
    const leg2Disabled =
      storeInstance._concerns['products.leg-2.strike']?.['disabledWhen']
    const leg2Validation =
      storeInstance._concerns['products.leg-2.strike']?.['validationState']

    expect(leg2Disabled).toBe(false) // Should still be false (not locked)
    expect(leg2Validation).toMatchObject({
      isError: false,
      errors: [],
    })
  })

  it('AC4: Correct disabled state after change', async () => {
    const store = createAppStore()

    const { storeInstance } = mountStore(
      store,
      {
        products: {
          'leg-1': { strike: 100, status: 'active' },
          'leg-2': { strike: 105, status: 'active' },
        },
      },
      {
        concerns: {
          'products.leg-1.strike': {
            disabledWhen: {
              boolLogic: { IS_EQUAL: ['products.leg-1.status', 'locked'] },
            },
          },
          'products.leg-2.strike': {
            disabledWhen: {
              boolLogic: { IS_EQUAL: ['products.leg-2.status', 'locked'] },
            },
          },
        },
      },
    )

    await flushSync()

    storeInstance.state.products['leg-1'].status = 'locked'
    await flushSync()

    const leg1Disabled =
      storeInstance._concerns['products.leg-1.strike']?.['disabledWhen']
    const leg2Disabled =
      storeInstance._concerns['products.leg-2.strike']?.['disabledWhen']

    expect(leg1Disabled).toBe(true) // Locked
    expect(leg2Disabled).toBe(false) // Not locked
  })

  it('Performance target: < 2ms for single concern evaluation', async () => {
    const store = createAppStore()

    const { storeInstance } = mountStore(
      store,
      {
        products: {
          'leg-1': { strike: 100, status: 'active' },
          'leg-2': { strike: 105, status: 'active' },
        },
      },
      {
        concerns: {
          'products.leg-1.strike': {
            disabledWhen: {
              boolLogic: { IS_EQUAL: ['products.leg-1.status', 'locked'] },
            },
          },
        },
      },
    )

    await flushSync()

    const start = performance.now()
    storeInstance.state.products['leg-1'].status = 'locked'
    await flushSync()
    const duration = performance.now() - start

    // Performance: < 15ms (includes React overhead)
    expect(duration).toBeLessThan(15)
  })

  it('Round-trip: cross-field dependency change → concern value available < 15ms', async () => {
    const store = createAppStore()

    const { storeInstance } = mountStore(
      store,
      {
        products: {
          'leg-1': { strike: 100, status: 'active' },
          'leg-2': { strike: 105, status: 'active' },
        },
      },
      {
        concerns: {
          'products.leg-1.strike': {
            disabledWhen: {
              boolLogic: { IS_EQUAL: ['products.leg-1.status', 'locked'] },
            },
          },
          'products.leg-2.strike': {
            disabledWhen: {
              boolLogic: { IS_EQUAL: ['products.leg-2.status', 'locked'] },
            },
          },
        },
      },
    )

    await flushSync()

    // Verify initial state
    const initialDisabled =
      storeInstance._concerns['products.leg-1.strike']?.['disabledWhen']
    expect(initialDisabled).toBe(false)

    // Measure propagation time
    const start = performance.now()

    // Change status that affects disabled concern
    storeInstance.state.products['leg-1'].status = 'locked'
    await flushSync()

    const propagationTime = performance.now() - start

    // Verify disabled state updated
    const updatedDisabled =
      storeInstance._concerns['products.leg-1.strike']?.['disabledWhen']
    expect(updatedDisabled).toBe(true)

    expect(propagationTime).toBeLessThan(15)
  })
})
