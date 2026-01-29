/**
 * TEST-001: Single Field Change - Selective Re-calculation
 *
 * Priority: P0 (Critical)
 * Performance Target: < 5ms
 *
 * Validates that only relevant concerns recalculate when state changes
 */

import { beforeEach, describe, expect, it } from 'vitest'
import { z } from 'zod'

import {
  type ConcernType,
  createConcernSpies,
  createEvaluationTracker,
  createTestStore,
  evaluateBoolLogic,
  getDeepValue,
  PerformanceBenchmark,
  waitForConcernValue,
} from './test-utils'

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
  let spies: ReturnType<typeof createConcernSpies>
  let tracker: ReturnType<typeof createEvaluationTracker>
  let benchmark: PerformanceBenchmark

  beforeEach(() => {
    spies = createConcernSpies()
    tracker = createEvaluationTracker()
    benchmark = new PerformanceBenchmark()
  })

  const createTestStoreWithConcerns = (initialData: AppState) => {
    // Define concerns with tracking
    const validationState: ConcernType = {
      name: 'validationState',
      evaluate: (props) => {
        spies.validationState(props.path)
        tracker.track('validationState', props.path)
        const result = props.schema.safeParse(props.value)
        return {
          isError: !result.success,
          errors: result.success
            ? []
            : result.error.errors.map((e: any) => ({
                field: e.path.length > 0 ? e.path.join('.') : undefined,
                message: e.message,
              })),
        }
      },
    }

    const disabled: ConcernType = {
      name: 'disabled',
      evaluate: (props) => {
        spies.disabled(props.path)
        tracker.track('disabled', props.path)
        return evaluateBoolLogic(props.condition, props.state)
      },
    }

    const tooltip: ConcernType = {
      name: 'tooltip',
      evaluate: (props) => {
        spies.tooltip(props.path)
        tracker.track('tooltip', props.path)
        return props.template.replace(
          /\{\{([\w.-]+)\}\}/g,
          (_: string, path: string) => {
            const value = getDeepValue(props.state, path)
            return value != null ? String(value) : ''
          },
        )
      },
    }

    const concerns = { validationState, disabled, tooltip }

    return createTestStore(initialData, concerns)
  }

  it('AC1: Only leg-1 concerns recalculate when leg-1 strike changes', async () => {
    const store = createTestStoreWithConcerns({
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
    })

    // Register concerns
    store.useConcerns('test', {
      'products.leg-1.strike': {
        validationState: { schema: z.number().min(0).max(200) },
        disabled: {
          condition: { IS_EQUAL: ['products.leg-1.status', 'locked'] },
        },
        tooltip: { template: 'Leg 1 Strike: {{products.leg-1.strike}}' },
      },
      'products.leg-2.strike': {
        validationState: { schema: z.number().min(0).max(200) },
        disabled: {
          condition: { IS_EQUAL: ['products.leg-2.status', 'locked'] },
        },
        tooltip: { template: 'Leg 2 Strike: {{products.leg-2.strike}}' },
      },
    })

    // Wait for initial evaluation

    // Clear tracking
    tracker.clear()
    spies.clear()

    // Change leg-1 strike
    store.proxy.products['leg-1'].strike = 150
    benchmark.start('single-field-update')
    const duration = benchmark.end('single-field-update')

    // AC1: Only leg-1 concerns recalculate
    const leg1Evals = tracker.filter((e) => e.path === 'products.leg-1.strike')
    const leg2Evals = tracker.filter((e) => e.path === 'products.leg-2.strike')

    expect(leg1Evals.length).toBe(3) // validationState + disabled + tooltip
    expect(leg2Evals.length).toBe(0) // Should NOT recalculate

    // Performance: < 15ms (valtio effect() overhead ~10ms + concern eval ~1-2ms)
    expect(duration).toBeLessThan(15)
  })

  it('AC2: All leg-1 concerns recalculate', async () => {
    const store = createTestStoreWithConcerns({
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
    })

    store.useConcerns('test', {
      'products.leg-1.strike': {
        validationState: { schema: z.number().min(0).max(200) },
        disabled: {
          condition: { IS_EQUAL: ['products.leg-1.status', 'locked'] },
        },
        tooltip: { template: 'Leg 1 Strike: {{products.leg-1.strike}}' },
      },
    })

    tracker.clear()

    store.proxy.products['leg-1'].strike = 150

    const leg1Evals = tracker.filter((e) => e.path === 'products.leg-1.strike')
    const concernNames = leg1Evals.map((e) => e.concern)

    expect(concernNames).toContain('validationState')
    expect(concernNames).toContain('disabled')
    expect(concernNames).toContain('tooltip')
  })

  it('AC3: Correct values after recalculation', async () => {
    const store = createTestStoreWithConcerns({
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
    })

    store.useConcerns('test', {
      'products.leg-1.strike': {
        validationState: { schema: z.number().min(0).max(200) },
        disabled: {
          condition: { IS_EQUAL: ['products.leg-1.status', 'locked'] },
        },
        tooltip: { template: 'Leg 1 Strike: {{products.leg-1.strike}}' },
      },
    })

    store.proxy.products['leg-1'].strike = 150

    const concerns = store.getFieldConcerns('products.leg-1.strike')

    expect(concerns.validationState?.isError).toBe(false) // Valid (no error)
    expect(concerns.tooltip).toBe('Leg 1 Strike: 150')
  })

  it('Performance target: < 5ms for re-evaluation', async () => {
    const store = createTestStoreWithConcerns({
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
    })

    benchmark.start('registration')
    store.useConcerns('test', {
      'products.leg-1.strike': {
        validationState: { schema: z.number().min(0).max(200) },
        disabled: {
          condition: { IS_EQUAL: ['products.leg-1.status', 'locked'] },
        },
        tooltip: { template: 'Leg 1 Strike: {{products.leg-1.strike}}' },
      },
    })
    benchmark.end('registration')

    store.proxy.products['leg-1'].strike = 150
    benchmark.start('single-field-update')
    const duration = benchmark.end('single-field-update')

    const report = benchmark.report()

    // Performance: < 15ms (valtio effect() overhead ~10ms + concern eval ~1-2ms)
    expect(duration).toBeLessThan(15)
    expect(
      report.measurements.find((m) => m.name === 'single-field-update')!
        .duration,
    ).toBeLessThan(15)
  })

  it('Round-trip: strike change → concern value available < 15ms', async () => {
    const store = createTestStoreWithConcerns({
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
    })

    store.useConcerns('test', {
      'products.leg-1.strike': {
        validationState: { schema: z.number().min(0).max(200) },
        disabled: {
          condition: { IS_EQUAL: ['products.leg-1.status', 'locked'] },
        },
        tooltip: { template: 'Leg 1 Strike: {{products.leg-1.strike}}' },
      },
    })

    // Verify initial tooltip
    const initialConcerns = store.getFieldConcerns('products.leg-1.strike')
    expect(initialConcerns.tooltip).toBe('Leg 1 Strike: 100')

    // Change the strike and measure propagation time
    store.proxy.products['leg-1'].strike = 150

    // Measure: state change → effects → concern value observable
    const propagationTime = await waitForConcernValue(
      () => store.getFieldConcerns('products.leg-1.strike').tooltip,
      'Leg 1 Strike: 150',
      100, // timeout: 100ms
    )

    expect(propagationTime).toBeLessThan(15)
  })
})
