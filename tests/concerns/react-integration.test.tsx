/**
 * TEST-007: React Integration - useSnapshot Batching
 *
 * Priority: P0 (Critical)
 * Performance Target: < 16ms
 *
 * Validates that React re-renders are batched correctly with useSnapshot
 */

import { describe, it, expect, beforeEach } from 'vitest'
import { render, waitFor } from '@testing-library/react'
import { proxy, useSnapshot } from 'valtio'
import { effect } from 'valtio-reactive'
import { z } from 'zod'
import React from 'react'
import {
  PerformanceBenchmark,
  createRenderTracker,
  createConcernSpies,
  getDeepValue,
  evaluateBoolLogic
} from './test-utils'

type AppState = {
  products: {
    'leg-1': { strike: number; notional: number; status: string }
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

describe('TEST-007: React Integration', () => {
  let spies: ReturnType<typeof createConcernSpies>
  let benchmark: PerformanceBenchmark

  beforeEach(() => {
    spies = createConcernSpies()
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
        return props.schema.safeParse(props.value).success
      }
    }

    const disabled: ConcernType = {
      name: 'disabled',
      evaluate: (props) => {
        spies.disabled(props.path)
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

    const useFieldConcerns = (path: string) => {
      const snap = useSnapshot(dataProxy)
      const result: Record<string, any> = {}
      const registrations = concernsRegistry.get(path) || []

      registrations.forEach(({ id, path: regPath, concernName }) => {
        const key = `${id}:${regPath}:${concernName}`
        result[concernName] = evaluationCache.get(key)
      })

      return result
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
      useFieldConcerns,
      getFieldConcerns
    }
  }

  it('AC1: Single re-render with React 18 batching', async () => {
    const renderTracker = createRenderTracker()

    const store = createTestStore({
      products: {
        'leg-1': { strike: 100, notional: 1000000, status: 'active' }
      },
      market: { spot: 102 }
    })

    store.useConcerns('test', {
      'products.leg-1.strike': {
        zodValidation: { schema: z.number().min(0).max(200) },
        disabled: { condition: { IS_EQUAL: ['products.leg-1.status', 'locked'] } }
      }
    })

    const TradeForm = () => {
      const snap = useSnapshot(store.proxy)
      const strikeValue = snap.products['leg-1'].strike
      const strikeConcerns = store.useFieldConcerns('products.leg-1.strike')

      renderTracker.track({
        strike: strikeValue,
        valid: strikeConcerns.zodValidation
      })

      return (
        <input
          value={strikeValue}
          disabled={strikeConcerns.disabled}
          className={strikeConcerns.zodValidation ? '' : 'error'}
          readOnly
        />
      )
    }

    render(<TradeForm />)

    // Wait for initial render
    await waitFor(() => expect(renderTracker.log.length).toBeGreaterThan(0))

    // Clear after initial render
    renderTracker.clear()

    // Bulk update
    store.proxy.products['leg-1'].strike = 150
    store.proxy.products['leg-1'].notional = 2000000
    store.proxy.products['leg-1'].status = 'locked'

    // Wait for re-renders to settle
    await waitFor(() => expect(renderTracker.log.length).toBeGreaterThan(0))

    // React 18 automatic batching should result in a single render
    expect(renderTracker.log.length).toBe(1)
  })

  it('AC2: No intermediate states visible', async () => {
    const renderTracker = createRenderTracker()

    const store = createTestStore({
      products: {
        'leg-1': { strike: 100, notional: 1000000, status: 'active' }
      },
      market: { spot: 102 }
    })

    store.useConcerns('test', {
      'products.leg-1.strike': {
        zodValidation: { schema: z.number().min(0).max(200) }
      }
    })

    const TradeForm = () => {
      const snap = useSnapshot(store.proxy)
      const strikeValue = snap.products['leg-1'].strike

      renderTracker.track({
        strike: strikeValue
      })

      return <div>{strikeValue}</div>
    }

    render(<TradeForm />)

    await waitFor(() => expect(renderTracker.log.length).toBeGreaterThan(0))

    renderTracker.clear()

    store.proxy.products['leg-1'].strike = 150

    await waitFor(() => expect(renderTracker.log.length).toBeGreaterThan(0))

    // Should only see final state
    expect(renderTracker.log[0].strike).toBe(150)
  })

  it('AC3: Concerns reflect final state', async () => {
    const renderTracker = createRenderTracker()

    const store = createTestStore({
      products: {
        'leg-1': { strike: 100, notional: 1000000, status: 'active' }
      },
      market: { spot: 102 }
    })

    store.useConcerns('test', {
      'products.leg-1.strike': {
        zodValidation: { schema: z.number().min(0).max(200) }
      }
    })

    const TradeForm = () => {
      const snap = useSnapshot(store.proxy)
      const strikeConcerns = store.useFieldConcerns('products.leg-1.strike')

      renderTracker.track({
        valid: strikeConcerns.zodValidation
      })

      return <div>{snap.products['leg-1'].strike}</div>
    }

    render(<TradeForm />)

    await waitFor(() => expect(renderTracker.log.length).toBeGreaterThan(0))

    renderTracker.clear()

    store.proxy.products['leg-1'].strike = 150

    await waitFor(() => expect(renderTracker.log.length).toBeGreaterThan(0))

    // Validation should pass for 150
    expect(renderTracker.log[0].valid).toBe(true)
  })

  it('Performance target: < 16ms render time (60fps)', async () => {
    const renderTracker = createRenderTracker()

    const store = createTestStore({
      products: {
        'leg-1': { strike: 100, notional: 1000000, status: 'active' }
      },
      market: { spot: 102 }
    })

    store.useConcerns('test', {
      'products.leg-1.strike': {
        zodValidation: { schema: z.number().min(0).max(200) },
        disabled: { condition: { IS_EQUAL: ['products.leg-1.status', 'locked'] } }
      }
    })

    const TradeForm = () => {
      const renderStart = performance.now()

      const snap = useSnapshot(store.proxy)
      const strikeValue = snap.products['leg-1'].strike
      const strikeConcerns = store.useFieldConcerns('products.leg-1.strike')

      const renderEnd = performance.now()
      const renderDuration = renderEnd - renderStart

      renderTracker.track({
        strike: strikeValue,
        valid: strikeConcerns.zodValidation,
        renderDuration
      })

      return (
        <input
          value={strikeValue}
          disabled={strikeConcerns.disabled}
          readOnly
        />
      )
    }

    render(<TradeForm />)

    await waitFor(() => expect(renderTracker.log.length).toBeGreaterThan(0))

    renderTracker.clear()

    benchmark.start('react-render')
    store.proxy.products['leg-1'].strike = 150
    await waitFor(() => expect(renderTracker.log.length).toBeGreaterThan(0))
    const totalDuration = benchmark.end('react-render')

    // Total time should be < 16ms for 60fps
    expect(totalDuration).toBeLessThan(16)

    // Individual render should also be fast
    if (renderTracker.log.length > 0) {
      expect(renderTracker.log[0].renderDuration).toBeLessThan(16)
    }
  })

  it('No flashing or visual glitches during updates', async () => {
    const renderTracker = createRenderTracker()

    const store = createTestStore({
      products: {
        'leg-1': { strike: 100, notional: 1000000, status: 'active' }
      },
      market: { spot: 102 }
    })

    store.useConcerns('test', {
      'products.leg-1.strike': {
        zodValidation: { schema: z.number().min(0).max(200) }
      }
    })

    const TradeForm = () => {
      const snap = useSnapshot(store.proxy)
      const strikeConcerns = store.useFieldConcerns('products.leg-1.strike')
      const strikeValue = snap.products['leg-1'].strike

      renderTracker.track({
        strike: strikeValue,
        className: strikeConcerns.zodValidation ? 'valid' : 'error'
      })

      return (
        <input
          value={strikeValue}
          className={strikeConcerns.zodValidation ? 'valid' : 'error'}
          readOnly
        />
      )
    }

    render(<TradeForm />)

    await waitFor(() => expect(renderTracker.log.length).toBeGreaterThan(0))

    renderTracker.clear()

    // Multiple rapid updates
    store.proxy.products['leg-1'].strike = 150
    store.proxy.products['leg-1'].notional = 2000000

    await waitFor(() => expect(renderTracker.log.length).toBeGreaterThan(0))

    // Should render with consistent valid state (no flashing between error/valid)
    renderTracker.log.forEach(entry => {
      expect(entry.className).toBe('valid')
    })
  })
})
