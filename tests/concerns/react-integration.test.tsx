/**
 * TEST-007: React Integration - useSnapshot Batching
 *
 * Priority: P0 (Critical)
 * Performance Target: < 16ms
 *
 * Validates that React re-renders are batched correctly with useSnapshot
 */

import { useSnapshot } from 'valtio'
import { beforeEach, describe, expect, it } from 'vitest'
import { z } from 'zod'

import type { ValidationStateResult } from '../../src/concerns/prebuilts'
import { useStoreContext } from '../../src/core/context'
import type { StoreInstance } from '../../src/core/types'
import { createGenericStore } from '../../src/store/createStore'
import { _ } from '../../src/utils/hashKey'
import { TradeFormComponent, useTradeStoreInstance } from '../utils/components'
import { flushEffects, flushSync, renderWithStore } from '../utils/react'
import { createRenderTracker, PerformanceBenchmark } from './test-utils'

interface AppState {
  products: {
    'leg-1': { strike: number; notional: number; status: string }
  }
  market: { spot: number }
}

describe('TEST-007: React Integration', () => {
  let benchmark: PerformanceBenchmark

  beforeEach(() => {
    benchmark = new PerformanceBenchmark()
  })

  const initialState: AppState = {
    products: {
      'leg-1': { strike: 100, notional: 1000000, status: 'active' },
    },
    market: { spot: 102 },
  }

  const fullConcerns = {
    [`products.${_('leg-1')}.strike`]: {
      validationState: { schema: z.number().min(0).max(200) },
      disabledWhen: {
        condition: {
          IS_EQUAL: [`products.${_('leg-1')}.status`, 'locked'],
        },
      },
    },
  }

  const validationOnlyConcerns = {
    'products.leg-1.strike': {
      validationState: { schema: z.number().min(0).max(200) },
    },
  }

  it('AC1: Single re-render with React 18 batching', async () => {
    const renderTracker = createRenderTracker()
    const store = createGenericStore<AppState>()

    let storeInstance: StoreInstance<AppState>

    const Wrapper = () => {
      storeInstance = useTradeStoreInstance() as StoreInstance<AppState>
      return <TradeFormComponent renderTracker={renderTracker} />
    }

    renderWithStore(<Wrapper />, store, structuredClone(initialState), {
      concerns: fullConcerns,
    })

    await flushEffects()
    renderTracker.clear()

    // Bulk update
    storeInstance!.state.products['leg-1'].strike = 150
    storeInstance!.state.products['leg-1'].notional = 2000000
    storeInstance!.state.products['leg-1'].status = 'locked'

    await flushEffects()

    // React 18 automatic batching should result in a single render
    expect(renderTracker.log.length).toBe(1)
  })

  it('AC2: No intermediate states visible', async () => {
    const renderTracker = createRenderTracker()
    const store = createGenericStore<AppState>()

    let storeInstance: StoreInstance<AppState>

    const TradeForm = () => {
      storeInstance = useStoreContext<AppState>()
      const snap = useSnapshot(storeInstance.state)
      const strikeValue = snap.products['leg-1'].strike

      renderTracker.track({
        strike: strikeValue,
      })

      return <div>{strikeValue}</div>
    }

    renderWithStore(<TradeForm />, store, structuredClone(initialState), {
      concerns: validationOnlyConcerns,
    })

    await flushEffects()
    renderTracker.clear()

    storeInstance!.state.products['leg-1'].strike = 150

    await flushEffects()

    // Should only see final state
    expect(renderTracker.log[0]?.['strike']).toBe(150)
  })

  it('AC3: Concerns reflect final state', async () => {
    const renderTracker = createRenderTracker()
    const store = createGenericStore<AppState>()

    let storeInstance: StoreInstance<AppState>

    const TradeForm = () => {
      storeInstance = useStoreContext<AppState>()
      const snap = useSnapshot(storeInstance.state)
      const strikeConcerns =
        storeInstance._concerns[`products.${_('leg-1')}.strike`]

      renderTracker.track({
        valid: !(strikeConcerns?.['validationState'] as ValidationStateResult)
          ?.isError,
      })

      return <div>{snap.products['leg-1'].strike}</div>
    }

    renderWithStore(<TradeForm />, store, structuredClone(initialState), {
      concerns: validationOnlyConcerns,
    })

    await flushEffects()
    renderTracker.clear()

    storeInstance!.state.products['leg-1'].strike = 150

    await flushEffects()

    // Validation should pass for 150
    expect(renderTracker.log[0]?.['valid']).toBe(true)
  })

  it('Performance target: < 16ms render time (60fps)', async () => {
    const renderTracker = createRenderTracker()
    const store = createGenericStore<AppState>()

    let storeInstance: StoreInstance<AppState>

    const Wrapper = () => {
      storeInstance = useTradeStoreInstance() as StoreInstance<AppState>
      return <TradeFormComponent renderTracker={renderTracker} measurePerf />
    }

    renderWithStore(<Wrapper />, store, structuredClone(initialState), {
      concerns: fullConcerns,
    })

    renderTracker.clear()

    benchmark.start('react-render')
    storeInstance!.state.products['leg-1'].strike = 150
    await flushSync()
    const totalDuration = benchmark.end('react-render')

    // Total time should be < 5ms for React render + synchronous effects
    expect(totalDuration).toBeLessThan(5)

    // Individual render should be under 16ms (60fps target)
    if (renderTracker.log.length > 0) {
      expect(renderTracker.log[0]?.['renderDuration']).toBeLessThan(16)
    }
  })

  it('No flashing or visual glitches during updates', async () => {
    const renderTracker = createRenderTracker()
    const store = createGenericStore<AppState>()

    let storeInstance: StoreInstance<AppState>

    const TradeForm = () => {
      storeInstance = useStoreContext<AppState>()
      const snap = useSnapshot(storeInstance.state)
      const strikeConcerns =
        storeInstance._concerns[`products.${_('leg-1')}.strike`]
      const strikeValue = snap.products['leg-1'].strike

      renderTracker.track({
        strike: strikeValue,
        className: !(
          strikeConcerns?.['validationState'] as ValidationStateResult
        )?.isError
          ? 'valid'
          : 'error',
      })

      return (
        <input
          value={strikeValue}
          className={
            !(strikeConcerns?.['validationState'] as ValidationStateResult)
              ?.isError
              ? 'valid'
              : 'error'
          }
          readOnly
        />
      )
    }

    renderWithStore(<TradeForm />, store, structuredClone(initialState), {
      concerns: validationOnlyConcerns,
    })

    await flushEffects()
    renderTracker.clear()

    // Multiple rapid updates
    storeInstance!.state.products['leg-1'].strike = 150
    storeInstance!.state.products['leg-1'].notional = 2000000

    await flushEffects()

    // Should render with consistent valid state (no flashing between error/valid)
    renderTracker.log.forEach((entry) => {
      expect(entry['className']).toBe('valid')
    })
  })
})
