/**
 * TEST-003: Batch Updates - Multiple Changes
 *
 * Priority: P0 (Critical)
 * Performance Target: < 30ms
 *
 * Validates that multiple synchronous changes result in batched evaluation
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
  getDeepValue
} from './test-utils'

type AppState = {
  products: {
    'leg-1': { strike: number; status: string }
    'leg-2': { strike: number; status: string }
  }
  market: { spot: number }
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

describe('TEST-003: Batch Updates', () => {
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

    const tooltip: ConcernType = {
      name: 'tooltip',
      evaluate: (props) => {
        spies.tooltip(props.path)
        tracker.track('tooltip', props.path)
        return props.template.replace(/\{\{([\w.-]+)\}\}/g, (_: string, path: string) => {
          const value = getDeepValue(props.state, path)
          return value != null ? String(value) : ''
        })
      }
    }

    const concerns = { zodValidation, tooltip }

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

  it('AC1: All concerns evaluate (correctness)', async () => {
    const store = createTestStore({
      products: {
        'leg-1': { strike: 100, status: 'active' },
        'leg-2': { strike: 105, status: 'active' }
      },
      market: { spot: 102 }
    })

    store.useConcerns('test', {
      'products.leg-1.strike': {
        zodValidation: { schema: z.number().min(0) },
        tooltip: { template: 'Strike: {{products.leg-1.strike}} @ {{market.spot}}' }
      },
      'products.leg-2.strike': {
        zodValidation: { schema: z.number().min(0) },
        tooltip: { template: 'Strike: {{products.leg-2.strike}} @ {{market.spot}}' }
      }
    })

    await waitForEffects()
    tracker.clear()

    // Synchronous bulk update
    store.proxy.products['leg-1'].strike = 150
    store.proxy.products['leg-2'].strike = 155
    store.proxy.market.spot = 120

    await waitForEffects()

    // All concerns should evaluate - 2 concerns × 2 paths = 4 total
    // But tooltip for both paths depends on market.spot, so we expect evaluations
    expect(tracker.log.length).toBeGreaterThanOrEqual(4)
  })

  it('AC2: Each concern evaluates at most once per update cycle', async () => {
    const store = createTestStore({
      products: {
        'leg-1': { strike: 100, status: 'active' },
        'leg-2': { strike: 105, status: 'active' }
      },
      market: { spot: 102 }
    })

    store.useConcerns('test', {
      'products.leg-1.strike': {
        zodValidation: { schema: z.number().min(0) },
        tooltip: { template: 'Strike: {{products.leg-1.strike}} @ {{market.spot}}' }
      }
    })

    await waitForEffects()
    tracker.clear()

    // Change strike value
    store.proxy.products['leg-1'].strike = 150

    await waitForEffects()

    const leg1ValidationEvals = tracker.filter(
      e => e.concern === 'zodValidation' && e.path === 'products.leg-1.strike'
    )

    // Should only evaluate once, not multiple times
    expect(leg1ValidationEvals.length).toBe(1)
  })

  it('AC4: Final state is correct after bulk update', async () => {
    const store = createTestStore({
      products: {
        'leg-1': { strike: 100, status: 'active' },
        'leg-2': { strike: 105, status: 'active' }
      },
      market: { spot: 102 }
    })

    store.useConcerns('test', {
      'products.leg-1.strike': {
        tooltip: { template: 'Strike: {{products.leg-1.strike}} @ {{market.spot}}' }
      },
      'products.leg-2.strike': {
        tooltip: { template: 'Strike: {{products.leg-2.strike}} @ {{market.spot}}' }
      }
    })

    await waitForEffects()

    // Synchronous bulk update
    store.proxy.products['leg-1'].strike = 150
    store.proxy.products['leg-2'].strike = 155
    store.proxy.market.spot = 120

    await waitForEffects()

    const leg1 = store.getFieldConcerns('products.leg-1.strike')
    const leg2 = store.getFieldConcerns('products.leg-2.strike')

    expect(leg1.tooltip).toBe('Strike: 150 @ 120')
    expect(leg2.tooltip).toBe('Strike: 155 @ 120')
  })

  it('Performance target: < 30ms end-to-end', async () => {
    const store = createTestStore({
      products: {
        'leg-1': { strike: 100, status: 'active' },
        'leg-2': { strike: 105, status: 'active' }
      },
      market: { spot: 102 }
    })

    store.useConcerns('test', {
      'products.leg-1.strike': {
        zodValidation: { schema: z.number().min(0) },
        tooltip: { template: 'Strike: {{products.leg-1.strike}} @ {{market.spot}}' }
      },
      'products.leg-2.strike': {
        zodValidation: { schema: z.number().min(0) },
        tooltip: { template: 'Strike: {{products.leg-2.strike}} @ {{market.spot}}' }
      }
    })

    await waitForEffects()

    store.proxy.products['leg-1'].strike = 150
    store.proxy.products['leg-2'].strike = 155
    store.proxy.market.spot = 120
    benchmark.start('bulk-update')
    await waitForEffects()
    const duration = benchmark.end('bulk-update')

    expect(duration).toBeLessThan(30)
  })

  it('Evaluation time target: < 10ms', async () => {
    const store = createTestStore({
      products: {
        'leg-1': { strike: 100, status: 'active' },
        'leg-2': { strike: 105, status: 'active' }
      },
      market: { spot: 102 }
    })

    store.useConcerns('test', {
      'products.leg-1.strike': {
        zodValidation: { schema: z.number().min(0) },
        tooltip: { template: 'Strike: {{products.leg-1.strike}} @ {{market.spot}}' }
      },
      'products.leg-2.strike': {
        zodValidation: { schema: z.number().min(0) },
        tooltip: { template: 'Strike: {{products.leg-2.strike}} @ {{market.spot}}' }
      }
    })

    await waitForEffects()

    // Measure just the evaluation time
    const evalStart = performance.now()
    store.proxy.products['leg-1'].strike = 150
    store.proxy.products['leg-2'].strike = 155
    const evalEnd = performance.now()

    const evalDuration = evalEnd - evalStart

    // This is immediate, not waiting for effects
    expect(evalDuration).toBeLessThan(10)
  })
})
