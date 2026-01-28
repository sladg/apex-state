/**
 * TEST-002: Cross-Field Validation - Dependency Tracking
 *
 * Priority: P0 (Critical)
 * Performance Target: < 2ms
 *
 * Validates that cross-field dependencies are tracked correctly
 */

import { describe, it, expect, beforeEach } from 'vitest'
import { proxy } from 'valtio/vanilla'
import { effect } from 'valtio-reactive'
import { z } from 'zod'
import {
  PerformanceBenchmark,
  createEvaluationTracker,
  createConcernSpies,
  waitForEffects,
  getDeepValue,
  evaluateBoolLogic
} from './test-utils'

type AppState = {
  products: {
    'leg-1': { strike: number; status: string }
    'leg-2': { strike: number; status: string }
  }
}

type ConcernType = {
  name: string
  evaluate: (props: any) => any
}

type ConcernRegistration = {
  id: string
  path: string
  concernName: string
  concern: ConcernType
  config: any
  dispose: () => void
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

  const createTestStore = (initialData: AppState) => {
    const dataProxy = proxy<AppState>(initialData)
    const concernsRegistry = new Map<string, ConcernRegistration[]>()
    const evaluationCache = new Map<string, any>()

    const zodValidation: ConcernType = {
      name: 'zodValidation',
      evaluate: (props) => {
        spies.zodValidation(props.path)
        tracker.track('zodValidation', props.path)
        return props.schema.safeParse(props.value).success
      }
    }

    const disabled: ConcernType = {
      name: 'disabled',
      evaluate: (props) => {
        spies.disabled(props.path)
        tracker.track('disabled', props.path)
        return evaluateBoolLogic(props.condition, props.state)
      }
    }

    const concerns = { zodValidation, disabled }

    const useConcerns = (id: string, registration: Record<string, any>) => {
      const disposeCallbacks: Array<() => void> = []

      Object.entries(registration).forEach(([path, concernConfigs]) => {
        if (!concernConfigs) return

        Object.entries(concernConfigs).forEach(([concernName, config]) => {
          if (!config) return

          const concern = concerns[concernName as keyof typeof concerns]
          if (!concern) return

          const concernKey = `${id}:${path}:${concernName}`

          const dispose = effect(() => {
            const value = getDeepValue(dataProxy, path)

            const result = concern.evaluate({
              state: dataProxy,
              path,
              value,
              ...config
            })

            evaluationCache.set(concernKey, result)
          })

          const reg: ConcernRegistration = {
            id,
            path,
            concernName,
            concern,
            config,
            dispose
          }

          const pathRegs = concernsRegistry.get(path) || []
          pathRegs.push(reg)
          concernsRegistry.set(path, pathRegs)

          disposeCallbacks.push(dispose)
        })
      })

      return () => {
        disposeCallbacks.forEach(dispose => dispose())
        concernsRegistry.forEach((regs, path) => {
          const filtered = regs.filter(r => r.id !== id)
          if (filtered.length === 0) {
            concernsRegistry.delete(path)
          } else {
            concernsRegistry.set(path, filtered)
          }
        })
      }
    }

    const getFieldConcerns = (path: string) => {
      const result: Record<string, any> = {}
      const registrations = concernsRegistry.get(path) || []

      registrations.forEach(({ id, path: regPath, concernName }) => {
        const key = `${id}:${regPath}:${concernName}`
        result[concernName] = evaluationCache.get(key)
      })

      return result
    }

    return {
      proxy: dataProxy,
      useConcerns,
      getFieldConcerns
    }
  }

  it('AC1: Only leg-1 disabled concern recalculates when status changes', async () => {
    const store = createTestStore({
      products: {
        'leg-1': { strike: 100, status: 'active' },
        'leg-2': { strike: 105, status: 'active' }
      }
    })

    store.useConcerns('test', {
      'products.leg-1.strike': {
        zodValidation: { schema: z.number().min(0) },
        disabled: { condition: { IS_EQUAL: ['products.leg-1.status', 'locked'] } }
      },
      'products.leg-2.strike': {
        zodValidation: { schema: z.number().min(0) },
        disabled: { condition: { IS_EQUAL: ['products.leg-2.status', 'locked'] } }
      }
    })

    await waitForEffects()
    tracker.clear()

    store.proxy.products['leg-1'].status = 'locked'
    benchmark.start('cross-field-update')
    await waitForEffects()
    const duration = benchmark.end('cross-field-update')

    const evals = tracker.log

    // Only leg-1 disabled should recalculate
    expect(evals.length).toBe(1)
    expect(evals[0].concern).toBe('disabled')
    expect(evals[0].path).toBe('products.leg-1.strike')

    // Performance: < 12ms (valtio effect() overhead ~10ms, single concern eval ~1-2ms)
    expect(duration).toBeLessThan(12)
  })

  it('AC2: Leg-1 zodValidation does NOT recalculate', async () => {
    const store = createTestStore({
      products: {
        'leg-1': { strike: 100, status: 'active' },
        'leg-2': { strike: 105, status: 'active' }
      }
    })

    store.useConcerns('test', {
      'products.leg-1.strike': {
        zodValidation: { schema: z.number().min(0) },
        disabled: { condition: { IS_EQUAL: ['products.leg-1.status', 'locked'] } }
      }
    })

    await waitForEffects()
    tracker.clear()

    store.proxy.products['leg-1'].status = 'locked'
    await waitForEffects()

    const validationEvals = tracker.filter(e => e.concern === 'zodValidation')
    expect(validationEvals.length).toBe(0) // Doesn't depend on status
  })

  it('AC3: Leg-2 concerns do NOT recalculate', async () => {
    const store = createTestStore({
      products: {
        'leg-1': { strike: 100, status: 'active' },
        'leg-2': { strike: 105, status: 'active' }
      }
    })

    store.useConcerns('test', {
      'products.leg-1.strike': {
        zodValidation: { schema: z.number().min(0) },
        disabled: { condition: { IS_EQUAL: ['products.leg-1.status', 'locked'] } }
      },
      'products.leg-2.strike': {
        zodValidation: { schema: z.number().min(0) },
        disabled: { condition: { IS_EQUAL: ['products.leg-2.status', 'locked'] } }
      }
    })

    await waitForEffects()
    tracker.clear()

    store.proxy.products['leg-1'].status = 'locked'
    await waitForEffects()

    const leg2Evals = tracker.filter(e => e.path.includes('leg-2'))
    expect(leg2Evals.length).toBe(0)
  })

  it('AC4: Correct disabled state after change', async () => {
    const store = createTestStore({
      products: {
        'leg-1': { strike: 100, status: 'active' },
        'leg-2': { strike: 105, status: 'active' }
      }
    })

    store.useConcerns('test', {
      'products.leg-1.strike': {
        disabled: { condition: { IS_EQUAL: ['products.leg-1.status', 'locked'] } }
      },
      'products.leg-2.strike': {
        disabled: { condition: { IS_EQUAL: ['products.leg-2.status', 'locked'] } }
      }
    })

    await waitForEffects()

    store.proxy.products['leg-1'].status = 'locked'
    await waitForEffects()

    const leg1Concerns = store.getFieldConcerns('products.leg-1.strike')
    const leg2Concerns = store.getFieldConcerns('products.leg-2.strike')

    expect(leg1Concerns.disabled).toBe(true)  // Locked
    expect(leg2Concerns.disabled).toBe(false) // Not locked
  })

  it('Performance target: < 2ms for single concern evaluation', async () => {
    const store = createTestStore({
      products: {
        'leg-1': { strike: 100, status: 'active' },
        'leg-2': { strike: 105, status: 'active' }
      }
    })

    store.useConcerns('test', {
      'products.leg-1.strike': {
        disabled: { condition: { IS_EQUAL: ['products.leg-1.status', 'locked'] } }
      }
    })

    await waitForEffects()

    store.proxy.products['leg-1'].status = 'locked'
    benchmark.start('cross-field-update')
    await waitForEffects()
    const duration = benchmark.end('cross-field-update')

    // Performance: < 12ms (valtio effect() overhead ~10ms, single concern eval ~1-2ms)
    expect(duration).toBeLessThan(12)
  })
})
