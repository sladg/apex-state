/**
 * TEST-007: React Integration - useSnapshot Batching
 *
 * Priority: P0 (Critical)
 * Performance Target: < 16ms
 *
 * Validates that React re-renders are batched correctly with useSnapshot
 */

import React from 'react'

import { render } from '@testing-library/react'
import { useSnapshot } from 'valtio'
import { beforeEach, describe, expect, it } from 'vitest'
import { z } from 'zod'

import type { ConcernType } from '../utils/concerns'
import {
  createTestStore as createTestStoreReact,
  flushEffects,
  flushSync,
} from '../utils/react'
import {
  createConcernSpies,
  createRenderTracker,
  evaluateBoolLogic,
  PerformanceBenchmark,
} from './test-utils'

interface AppState {
  products: {
    'leg-1': { strike: number; notional: number; status: string }
  }
  market: { spot: number }
}

describe('TEST-007: React Integration', () => {
  let spies: ReturnType<typeof createConcernSpies>
  let benchmark: PerformanceBenchmark

  beforeEach(() => {
    spies = createConcernSpies()
    benchmark = new PerformanceBenchmark()
  })

  const createTestStore = (initialData: AppState) => {
    const validationState: ConcernType = {
      name: 'validationState',
      evaluate: (props) => {
        spies.validationState(props.path)
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
        return evaluateBoolLogic(props.condition, props.state)
      },
    }

    const concerns = { validationState, disabled }

    const store = createTestStoreReact(initialData)
    const originalUseConcerns = store.useConcerns

    const useConcerns = (id: string, registration: Record<string, any>) => {
      return originalUseConcerns(id, registration, concerns)
    }

    const useFieldConcerns = (path: string) => {
      const _snap = useSnapshot(store.proxy)
      return store.getFieldConcerns(path)
    }

    return {
      proxy: store.proxy,
      useConcerns,
      useFieldConcerns,
      getFieldConcerns: store.getFieldConcerns,
    }
  }

  it('AC1: Single re-render with React 18 batching', async () => {
    const renderTracker = createRenderTracker()

    const store = createTestStore({
      products: {
        'leg-1': { strike: 100, notional: 1000000, status: 'active' },
      },
      market: { spot: 102 },
    })

    store.useConcerns('test', {
      'products.leg-1.strike': {
        validationState: { schema: z.number().min(0).max(200) },
        disabled: {
          condition: { IS_EQUAL: ['products.leg-1.status', 'locked'] },
        },
      },
    })

    const TradeForm = () => {
      const snap = useSnapshot(store.proxy)
      const strikeValue = snap.products['leg-1'].strike
      const strikeConcerns = store.useFieldConcerns('products.leg-1.strike')

      renderTracker.track({
        strike: strikeValue,
        valid: !strikeConcerns.validationState?.isError,
      })

      return (
        <input
          value={strikeValue}
          disabled={strikeConcerns.disabled}
          className={!strikeConcerns.validationState?.isError ? '' : 'error'}
          readOnly
        />
      )
    }

    render(<TradeForm />)

    // Wait for initial render
    await flushEffects()

    // Clear after initial render
    renderTracker.clear()

    // Bulk update
    store.proxy.products['leg-1'].strike = 150
    store.proxy.products['leg-1'].notional = 2000000
    store.proxy.products['leg-1'].status = 'locked'

    // Wait for re-renders to settle
    await flushEffects()

    // React 18 automatic batching should result in a single render
    expect(renderTracker.log.length).toBe(1)
  })

  it('AC2: No intermediate states visible', async () => {
    const renderTracker = createRenderTracker()

    const store = createTestStore({
      products: {
        'leg-1': { strike: 100, notional: 1000000, status: 'active' },
      },
      market: { spot: 102 },
    })

    store.useConcerns('test', {
      'products.leg-1.strike': {
        validationState: { schema: z.number().min(0).max(200) },
      },
    })

    const TradeForm = () => {
      const snap = useSnapshot(store.proxy)
      const strikeValue = snap.products['leg-1'].strike

      renderTracker.track({
        strike: strikeValue,
      })

      return <div>{strikeValue}</div>
    }

    render(<TradeForm />)

    await flushEffects()

    renderTracker.clear()

    store.proxy.products['leg-1'].strike = 150

    await flushEffects()

    // Should only see final state
    expect(renderTracker.log[0].strike).toBe(150)
  })

  it('AC3: Concerns reflect final state', async () => {
    const renderTracker = createRenderTracker()

    const store = createTestStore({
      products: {
        'leg-1': { strike: 100, notional: 1000000, status: 'active' },
      },
      market: { spot: 102 },
    })

    store.useConcerns('test', {
      'products.leg-1.strike': {
        validationState: { schema: z.number().min(0).max(200) },
      },
    })

    const TradeForm = () => {
      const snap = useSnapshot(store.proxy)
      const strikeConcerns = store.useFieldConcerns('products.leg-1.strike')

      renderTracker.track({
        valid: !strikeConcerns.validationState?.isError,
      })

      return <div>{snap.products['leg-1'].strike}</div>
    }

    render(<TradeForm />)

    await flushEffects()

    renderTracker.clear()

    store.proxy.products['leg-1'].strike = 150

    await flushEffects()

    // Validation should pass for 150
    expect(renderTracker.log[0].valid).toBe(true)
  })

  it('Performance target: < 16ms render time (60fps)', async () => {
    const renderTracker = createRenderTracker()

    const store = createTestStore({
      products: {
        'leg-1': { strike: 100, notional: 1000000, status: 'active' },
      },
      market: { spot: 102 },
    })

    store.useConcerns('test', {
      'products.leg-1.strike': {
        validationState: { schema: z.number().min(0).max(200) },
        disabled: {
          condition: { IS_EQUAL: ['products.leg-1.status', 'locked'] },
        },
      },
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
        valid: !strikeConcerns.validationState?.isError,
        renderDuration,
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

    // Effects run synchronously, no wait needed
    renderTracker.clear()

    benchmark.start('react-render')
    store.proxy.products['leg-1'].strike = 150
    await flushSync() // Use flushSync for performance tests (no 20ms setTimeout)
    const totalDuration = benchmark.end('react-render')

    // Total time should be < 5ms for React render + synchronous effects
    // This test uses Zod validation which is synchronous, no async validators
    expect(totalDuration).toBeLessThan(5)

    // Individual render should be under 16ms (60fps target)
    if (renderTracker.log.length > 0) {
      expect(renderTracker.log[0].renderDuration).toBeLessThan(16)
    }
  })

  it('No flashing or visual glitches during updates', async () => {
    const renderTracker = createRenderTracker()

    const store = createTestStore({
      products: {
        'leg-1': { strike: 100, notional: 1000000, status: 'active' },
      },
      market: { spot: 102 },
    })

    store.useConcerns('test', {
      'products.leg-1.strike': {
        validationState: { schema: z.number().min(0).max(200) },
      },
    })

    const TradeForm = () => {
      const snap = useSnapshot(store.proxy)
      const strikeConcerns = store.useFieldConcerns('products.leg-1.strike')
      const strikeValue = snap.products['leg-1'].strike

      renderTracker.track({
        strike: strikeValue,
        className: !strikeConcerns.validationState?.isError ? 'valid' : 'error',
      })

      return (
        <input
          value={strikeValue}
          className={
            !strikeConcerns.validationState?.isError ? 'valid' : 'error'
          }
          readOnly
        />
      )
    }

    render(<TradeForm />)

    await flushEffects()

    renderTracker.clear()

    // Multiple rapid updates
    store.proxy.products['leg-1'].strike = 150
    store.proxy.products['leg-1'].notional = 2000000

    await flushEffects()

    // Should render with consistent valid state (no flashing between error/valid)
    renderTracker.log.forEach((entry) => {
      expect(entry.className).toBe('valid')
    })
  })
})
