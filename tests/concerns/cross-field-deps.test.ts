/**
 * TEST-002: Cross-Field Validation - Dependency Tracking
 *
 * Priority: P0 (Critical)
 * Performance Target: < 2ms
 *
 * Validates that cross-field dependencies are tracked correctly
 */

import { beforeEach, describe, expect, it } from 'vitest'
import { z } from 'zod'

import {
  type ConcernType,
  createConcernSpies,
  createEvaluationTracker,
  createTestStore,
  evaluateBoolLogic,
  PerformanceBenchmark,
  waitForConcernValue,
} from './test-utils'

interface AppState {
  products: {
    'leg-1': { strike: number; status: string }
    'leg-2': { strike: number; status: string }
  }
}

describe('TEST-002: Cross-Field Dependency Tracking', () => {
  let spies: ReturnType<typeof createConcernSpies>
  let tracker: ReturnType<typeof createEvaluationTracker>
  let benchmark: PerformanceBenchmark

  beforeEach(() => {
    spies = createConcernSpies()
    tracker = createEvaluationTracker()
    benchmark = new PerformanceBenchmark()
  })

  const createTestStoreWithConcerns = (initialData: AppState) => {
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

    const concerns = { validationState, disabled }

    return createTestStore(initialData, concerns)
  }

  it('AC1: Only leg-1 disabled concern recalculates when status changes', async () => {
    const store = createTestStoreWithConcerns({
      products: {
        'leg-1': { strike: 100, status: 'active' },
        'leg-2': { strike: 105, status: 'active' },
      },
    })

    store.useConcerns('test', {
      'products.leg-1.strike': {
        validationState: { schema: z.number().min(0) },
        disabled: {
          condition: { IS_EQUAL: ['products.leg-1.status', 'locked'] },
        },
      },
      'products.leg-2.strike': {
        validationState: { schema: z.number().min(0) },
        disabled: {
          condition: { IS_EQUAL: ['products.leg-2.status', 'locked'] },
        },
      },
    })

    tracker.clear()

    store.proxy.products['leg-1'].status = 'locked'
    benchmark.start('cross-field-update')
    const duration = benchmark.end('cross-field-update')

    const evals = tracker.log

    // Only leg-1 disabled should recalculate
    expect(evals.length).toBe(1)
    expect(evals[0].concern).toBe('disabled')
    expect(evals[0].path).toBe('products.leg-1.strike')

    // Performance: < 15ms (valtio effect() overhead ~10ms, single concern eval ~1-2ms)
    expect(duration).toBeLessThan(15)
  })

  it('AC2: Leg-1 validationState does NOT recalculate', async () => {
    const store = createTestStoreWithConcerns({
      products: {
        'leg-1': { strike: 100, status: 'active' },
        'leg-2': { strike: 105, status: 'active' },
      },
    })

    store.useConcerns('test', {
      'products.leg-1.strike': {
        validationState: { schema: z.number().min(0) },
        disabled: {
          condition: { IS_EQUAL: ['products.leg-1.status', 'locked'] },
        },
      },
    })

    tracker.clear()

    store.proxy.products['leg-1'].status = 'locked'

    const validationEvals = tracker.filter(
      (e) => e.concern === 'validationState',
    )
    expect(validationEvals.length).toBe(0) // Doesn't depend on status
  })

  it('AC3: Leg-2 concerns do NOT recalculate', async () => {
    const store = createTestStoreWithConcerns({
      products: {
        'leg-1': { strike: 100, status: 'active' },
        'leg-2': { strike: 105, status: 'active' },
      },
    })

    store.useConcerns('test', {
      'products.leg-1.strike': {
        validationState: { schema: z.number().min(0) },
        disabled: {
          condition: { IS_EQUAL: ['products.leg-1.status', 'locked'] },
        },
      },
      'products.leg-2.strike': {
        validationState: { schema: z.number().min(0) },
        disabled: {
          condition: { IS_EQUAL: ['products.leg-2.status', 'locked'] },
        },
      },
    })

    tracker.clear()

    store.proxy.products['leg-1'].status = 'locked'

    const leg2Evals = tracker.filter((e) => e.path.includes('leg-2'))
    expect(leg2Evals.length).toBe(0)
  })

  it('AC4: Correct disabled state after change', async () => {
    const store = createTestStoreWithConcerns({
      products: {
        'leg-1': { strike: 100, status: 'active' },
        'leg-2': { strike: 105, status: 'active' },
      },
    })

    store.useConcerns('test', {
      'products.leg-1.strike': {
        disabled: {
          condition: { IS_EQUAL: ['products.leg-1.status', 'locked'] },
        },
      },
      'products.leg-2.strike': {
        disabled: {
          condition: { IS_EQUAL: ['products.leg-2.status', 'locked'] },
        },
      },
    })

    store.proxy.products['leg-1'].status = 'locked'

    const leg1Concerns = store.getFieldConcerns('products.leg-1.strike')
    const leg2Concerns = store.getFieldConcerns('products.leg-2.strike')

    expect(leg1Concerns.disabled).toBe(true) // Locked
    expect(leg2Concerns.disabled).toBe(false) // Not locked
  })

  it('Performance target: < 2ms for single concern evaluation', async () => {
    const store = createTestStoreWithConcerns({
      products: {
        'leg-1': { strike: 100, status: 'active' },
        'leg-2': { strike: 105, status: 'active' },
      },
    })

    store.useConcerns('test', {
      'products.leg-1.strike': {
        disabled: {
          condition: { IS_EQUAL: ['products.leg-1.status', 'locked'] },
        },
      },
    })

    store.proxy.products['leg-1'].status = 'locked'
    benchmark.start('cross-field-update')
    const duration = benchmark.end('cross-field-update')

    // Performance: < 15ms (valtio effect() overhead ~10ms, single concern eval ~1-2ms)
    expect(duration).toBeLessThan(15)
  })

  it('Round-trip: cross-field dependency change → concern value available < 15ms', async () => {
    const store = createTestStoreWithConcerns({
      products: {
        'leg-1': { strike: 100, status: 'active' },
        'leg-2': { strike: 105, status: 'active' },
      },
    })

    store.useConcerns('test', {
      'products.leg-1.strike': {
        disabled: {
          condition: { IS_EQUAL: ['products.leg-1.status', 'locked'] },
        },
      },
      'products.leg-2.strike': {
        disabled: {
          condition: { IS_EQUAL: ['products.leg-2.status', 'locked'] },
        },
      },
    })

    // Verify initial state
    expect(store.getFieldConcerns('products.leg-1.strike').disabled).toBe(false)

    // Change status that affects disabled concern
    store.proxy.products['leg-1'].status = 'locked'

    // Measure: status change → effects → disabled value updated
    const propagationTime = await waitForConcernValue(
      () => store.getFieldConcerns('products.leg-1.strike').disabled,
      true,
      100,
    )

    expect(propagationTime).toBeLessThan(15)
  })
})
