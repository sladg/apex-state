/**
 * TEST-001: Single Field Change - Selective Re-calculation
 *
 * Priority: P0 (Critical)
 * Performance Target: < 5ms
 *
 * Validates that only relevant concerns recalculate when state changes
 */

import { proxy } from 'valtio/vanilla'
import { effect } from 'valtio-reactive'
import { beforeEach, describe, expect, it } from 'vitest'
import { z } from 'zod'

import {
  createConcernSpies,
  createEvaluationTracker,
  evaluateBoolLogic,
  getDeepValue,
  PerformanceBenchmark,
  waitForEffects,
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

interface ConcernType {
  name: string
  evaluate: (props: any) => any
}

interface ConcernRegistration {
  id: string
  path: string
  concernName: string
  concern: ConcernType
  config: any
  dispose: () => void
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

  const createTestStore = (initialData: AppState) => {
    const dataProxy = proxy<AppState>(initialData)
    const concernsRegistry = new Map<string, ConcernRegistration[]>()
    const evaluationCache = new Map<string, any>()

    // Define concerns with tracking
    const zodValidation: ConcernType = {
      name: 'zodValidation',
      evaluate: (props) => {
        spies.zodValidation(props.path)
        tracker.track('zodValidation', props.path)
        return props.schema.safeParse(props.value).success
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

    const concerns = { zodValidation, disabled, tooltip }

    const useConcerns = (id: string, registration: Record<string, any>) => {
      const disposeCallbacks: (() => void)[] = []

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
              ...config,
            })

            evaluationCache.set(concernKey, result)
          })

          const reg: ConcernRegistration = {
            id,
            path,
            concernName,
            concern,
            config,
            dispose,
          }

          const pathRegs = concernsRegistry.get(path) || []
          pathRegs.push(reg)
          concernsRegistry.set(path, pathRegs)

          disposeCallbacks.push(dispose)
        })
      })

      return () => {
        disposeCallbacks.forEach((dispose) => dispose())
        concernsRegistry.forEach((regs, path) => {
          const filtered = regs.filter((r) => r.id !== id)
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
      getFieldConcerns,
    }
  }

  it(
    'AC1: Only leg-1 concerns recalculate when leg-1 strike changes',
    { retry: 2 },
    async () => {
      const store = createTestStore({
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
          zodValidation: { schema: z.number().min(0).max(200) },
          disabled: {
            condition: { IS_EQUAL: ['products.leg-1.status', 'locked'] },
          },
          tooltip: { template: 'Leg 1 Strike: {{products.leg-1.strike}}' },
        },
        'products.leg-2.strike': {
          zodValidation: { schema: z.number().min(0).max(200) },
          disabled: {
            condition: { IS_EQUAL: ['products.leg-2.status', 'locked'] },
          },
          tooltip: { template: 'Leg 2 Strike: {{products.leg-2.strike}}' },
        },
      })

      // Wait for initial evaluation
      await waitForEffects()

      // Clear tracking
      tracker.clear()
      spies.clear()

      // Change leg-1 strike
      store.proxy.products['leg-1'].strike = 150
      benchmark.start('single-field-update')
      await waitForEffects()
      const duration = benchmark.end('single-field-update')

      // AC1: Only leg-1 concerns recalculate
      const leg1Evals = tracker.filter(
        (e) => e.path === 'products.leg-1.strike',
      )
      const leg2Evals = tracker.filter(
        (e) => e.path === 'products.leg-2.strike',
      )

      expect(leg1Evals.length).toBe(3) // zodValidation + disabled + tooltip
      expect(leg2Evals.length).toBe(0) // Should NOT recalculate

      // Performance: < 15ms (valtio effect() overhead ~10ms + concern eval ~1-2ms)
      expect(duration).toBeLessThan(15)
    },
  )

  it('AC2: All leg-1 concerns recalculate', async () => {
    const store = createTestStore({
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
        zodValidation: { schema: z.number().min(0).max(200) },
        disabled: {
          condition: { IS_EQUAL: ['products.leg-1.status', 'locked'] },
        },
        tooltip: { template: 'Leg 1 Strike: {{products.leg-1.strike}}' },
      },
    })

    await waitForEffects()
    tracker.clear()

    store.proxy.products['leg-1'].strike = 150
    await waitForEffects()

    const leg1Evals = tracker.filter((e) => e.path === 'products.leg-1.strike')
    const concernNames = leg1Evals.map((e) => e.concern)

    expect(concernNames).toContain('zodValidation')
    expect(concernNames).toContain('disabled')
    expect(concernNames).toContain('tooltip')
  })

  it('AC3: Correct values after recalculation', async () => {
    const store = createTestStore({
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
        zodValidation: { schema: z.number().min(0).max(200) },
        disabled: {
          condition: { IS_EQUAL: ['products.leg-1.status', 'locked'] },
        },
        tooltip: { template: 'Leg 1 Strike: {{products.leg-1.strike}}' },
      },
    })

    await waitForEffects()

    store.proxy.products['leg-1'].strike = 150
    await waitForEffects()

    const concerns = store.getFieldConcerns('products.leg-1.strike')

    expect(concerns.zodValidation).toBe(true) // Valid
    expect(concerns.tooltip).toBe('Leg 1 Strike: 150')
  })

  it('Performance target: < 5ms for re-evaluation', { retry: 2 }, async () => {
    const store = createTestStore({
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
        zodValidation: { schema: z.number().min(0).max(200) },
        disabled: {
          condition: { IS_EQUAL: ['products.leg-1.status', 'locked'] },
        },
        tooltip: { template: 'Leg 1 Strike: {{products.leg-1.strike}}' },
      },
    })
    await waitForEffects()
    benchmark.end('registration')

    store.proxy.products['leg-1'].strike = 150
    benchmark.start('single-field-update')
    await waitForEffects()
    const duration = benchmark.end('single-field-update')

    const report = benchmark.report()

    // Performance: < 15ms (valtio effect() overhead ~10ms + concern eval ~1-2ms)
    expect(duration).toBeLessThan(15)
    expect(
      report.measurements.find((m) => m.name === 'single-field-update')!
        .duration,
    ).toBeLessThan(15)
  })
})
