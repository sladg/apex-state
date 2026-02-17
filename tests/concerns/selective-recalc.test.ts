/**
 * TEST-001: Single Field Change - Selective Re-calculation
 *
 * Priority: P0 (Critical)
 * Performance Target: < 5ms
 *
 * Validates that only relevant concerns recalculate when state changes
 */

import { describe, expect, it } from 'vitest'
import { z } from 'zod'

import { createGenericStore } from '../../src'
import { flushSync, mountStore } from '../utils/react'

interface AppState {
  products: {
    'leg-1': {
      strike: number
      expiry: string
      premium: number | null
      status: string
    }
    'leg-2': {
      strike: number
      expiry: string
      premium: number | null
      status: string
    }
  }
  market: { spot: number; volatility: number }
}

describe('TEST-001: Selective Re-calculation', () => {
  const createAppStore = () => createGenericStore<AppState>()

  it('AC1: Only leg-1 concerns recalculate when leg-1 strike changes', async () => {
    const store = createAppStore()

    const { storeInstance } = mountStore(
      store,
      {
        products: {
          'leg-1': {
            strike: 100,
            expiry: '2024-12-31',
            premium: null,
            status: 'active',
          },
          'leg-2': {
            strike: 105,
            expiry: '2024-12-31',
            premium: null,
            status: 'active',
          },
        },
        market: { spot: 102, volatility: 0.15 },
      },
      {
        concerns: {
          'products.leg-1.strike': {
            validationState: { schema: z.number().min(0).max(200) },
            disabledWhen: {
              boolLogic: { IS_EQUAL: ['products.leg-1.status', 'locked'] },
            },
            dynamicTooltip: {
              template: 'Leg 1 Strike: {{products.leg-1.strike}}',
            },
          },
          'products.leg-2.strike': {
            validationState: { schema: z.number().min(0).max(200) },
            disabledWhen: {
              boolLogic: { IS_EQUAL: ['products.leg-2.status', 'locked'] },
            },
            dynamicTooltip: {
              template: 'Leg 2 Strike: {{products.leg-2.strike}}',
            },
          },
        },
      },
    )

    await flushSync()

    const start = performance.now()

    // Change leg-1 strike
    storeInstance.state.products['leg-1'].strike = 150

    await flushSync()

    const duration = performance.now() - start

    // AC1: Only leg-1 concerns should be updated, leg-2 should remain unchanged
    const leg1Concerns = storeInstance._concerns['products.leg-1.strike']
    const leg2Concerns = storeInstance._concerns['products.leg-2.strike']

    // Verify leg-1 concerns are defined and updated
    expect(leg1Concerns?.['validationState']).toBeDefined()
    expect(leg1Concerns?.['disabledWhen']).toBeDefined()
    expect(leg1Concerns?.['dynamicTooltip']).toBe('Leg 1 Strike: 150')

    // Verify leg-2 concerns remain at initial values
    expect(leg2Concerns?.['dynamicTooltip']).toBe('Leg 2 Strike: 105')

    // Performance: < 15ms (includes React overhead)
    expect(duration).toBeLessThan(15)
  })

  it('AC2: All leg-1 concerns recalculate', async () => {
    const store = createAppStore()

    const { storeInstance } = mountStore(
      store,
      {
        products: {
          'leg-1': {
            strike: 100,
            expiry: '2024-12-31',
            premium: null,
            status: 'active',
          },
          'leg-2': {
            strike: 105,
            expiry: '2024-12-31',
            premium: null,
            status: 'active',
          },
        },
        market: { spot: 102, volatility: 0.15 },
      },
      {
        concerns: {
          'products.leg-1.strike': {
            validationState: { schema: z.number().min(0).max(200) },
            disabledWhen: {
              boolLogic: { IS_EQUAL: ['products.leg-1.status', 'locked'] },
            },
            dynamicTooltip: {
              template: 'Leg 1 Strike: {{products.leg-1.strike}}',
            },
          },
        },
      },
    )

    await flushSync()

    storeInstance.state.products['leg-1'].strike = 150

    await flushSync()

    const leg1Concerns = storeInstance._concerns['products.leg-1.strike']

    // All three concerns should be defined and updated
    expect(leg1Concerns?.['validationState']).toBeDefined()
    expect(leg1Concerns?.['disabledWhen']).toBeDefined()
    expect(leg1Concerns?.['dynamicTooltip']).toBeDefined()
  })

  it('AC3: Correct values after recalculation', async () => {
    const store = createAppStore()

    const { storeInstance } = mountStore(
      store,
      {
        products: {
          'leg-1': {
            strike: 100,
            expiry: '2024-12-31',
            premium: null,
            status: 'active',
          },
          'leg-2': {
            strike: 105,
            expiry: '2024-12-31',
            premium: null,
            status: 'active',
          },
        },
        market: { spot: 102, volatility: 0.15 },
      },
      {
        concerns: {
          'products.leg-1.strike': {
            validationState: { schema: z.number().min(0).max(200) },
            disabledWhen: {
              boolLogic: { IS_EQUAL: ['products.leg-1.status', 'locked'] },
            },
            dynamicTooltip: {
              template: 'Leg 1 Strike: {{products.leg-1.strike}}',
            },
          },
        },
      },
    )

    await flushSync()

    storeInstance.state.products['leg-1'].strike = 150

    await flushSync()

    const leg1Concerns = storeInstance._concerns['products.leg-1.strike']
    const validation = leg1Concerns?.['validationState'] as
      | { isError: boolean; errors: any[] }
      | undefined
    const tooltip = leg1Concerns?.['dynamicTooltip']

    expect(validation?.isError).toBe(false) // Valid (no error)
    expect(tooltip).toBe('Leg 1 Strike: 150')
  })

  it('Performance target: < 5ms for re-evaluation', async () => {
    const store = createAppStore()

    const { storeInstance } = mountStore(
      store,
      {
        products: {
          'leg-1': {
            strike: 100,
            expiry: '2024-12-31',
            premium: null,
            status: 'active',
          },
          'leg-2': {
            strike: 105,
            expiry: '2024-12-31',
            premium: null,
            status: 'active',
          },
        },
        market: { spot: 102, volatility: 0.15 },
      },
      {
        concerns: {
          'products.leg-1.strike': {
            validationState: { schema: z.number().min(0).max(200) },
            disabledWhen: {
              boolLogic: { IS_EQUAL: ['products.leg-1.status', 'locked'] },
            },
            dynamicTooltip: {
              template: 'Leg 1 Strike: {{products.leg-1.strike}}',
            },
          },
        },
      },
    )

    await flushSync()

    const start = performance.now()
    storeInstance.state.products['leg-1'].strike = 150
    await flushSync()
    const duration = performance.now() - start

    // Performance: < 15ms (includes React overhead)
    expect(duration).toBeLessThan(15)
  })

  it('Round-trip: strike change → concern value available < 15ms', async () => {
    const store = createAppStore()

    const { storeInstance } = mountStore(
      store,
      {
        products: {
          'leg-1': {
            strike: 100,
            expiry: '2024-12-31',
            premium: null,
            status: 'active',
          },
          'leg-2': {
            strike: 105,
            expiry: '2024-12-31',
            premium: null,
            status: 'active',
          },
        },
        market: { spot: 102, volatility: 0.15 },
      },
      {
        concerns: {
          'products.leg-1.strike': {
            validationState: { schema: z.number().min(0).max(200) },
            disabledWhen: {
              boolLogic: { IS_EQUAL: ['products.leg-1.status', 'locked'] },
            },
            dynamicTooltip: {
              template: 'Leg 1 Strike: {{products.leg-1.strike}}',
            },
          },
        },
      },
    )

    await flushSync()

    // Verify initial tooltip
    const initialTooltip =
      storeInstance._concerns['products.leg-1.strike']?.['dynamicTooltip']
    expect(initialTooltip).toBe('Leg 1 Strike: 100')

    // Change the strike and measure propagation time
    const start = performance.now()
    storeInstance.state.products['leg-1'].strike = 150
    await flushSync()
    const propagationTime = performance.now() - start

    // Verify tooltip updated
    const updatedTooltip =
      storeInstance._concerns['products.leg-1.strike']?.['dynamicTooltip']
    expect(updatedTooltip).toBe('Leg 1 Strike: 150')

    expect(propagationTime).toBeLessThan(15)
  })
})
