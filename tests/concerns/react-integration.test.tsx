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

import type { ValidationStateResult } from '~/concerns/prebuilts'
import { useStoreContext } from '~/core/context'
import type { StoreInstance } from '~/core/types'
import { createGenericStore } from '~/store/create-store'
import { _ } from '~/utils/hash-key'

import { flushEffects, flushSync, mountStore } from '../utils/react'
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

  const createAppStore = () => createGenericStore<AppState>()

  it('AC1: Single re-render with React 18 batching', async () => {
    const renderTracker = createRenderTracker()
    const store = createAppStore()

    const initialState = {
      products: {
        'leg-1': { strike: 100, notional: 1000000, status: 'active' },
      },
      market: { spot: 102 },
    }

    const concerns = {
      [`products.${_('leg-1')}.strike`]: {
        validationState: { schema: z.number().min(0).max(200) },
        disabledWhen: {
          boolLogic: {
            IS_EQUAL: [`products.${_('leg-1')}.status`, 'locked'],
          },
        },
      },
    }

    let storeInstance: StoreInstance<AppState>

    const TradeForm = () => {
      storeInstance = useStoreContext<AppState>()
      const snap = useSnapshot(storeInstance.state)
      const strikeValue = snap.products['leg-1'].strike
      const strikeConcerns =
        storeInstance._concerns[`products.${_('leg-1')}.strike`]

      renderTracker.track({
        strike: strikeValue,
        valid: !(strikeConcerns?.['validationState'] as ValidationStateResult)
          ?.isError,
      })

      return (
        <input
          value={strikeValue}
          disabled={strikeConcerns?.['disabledWhen'] as boolean | undefined}
          className={
            !(strikeConcerns?.['validationState'] as ValidationStateResult)
              ?.isError
              ? ''
              : 'error'
          }
          readOnly
        />
      )
    }

    mountStore(<TradeForm />, store, initialState, { concerns })

    // Wait for initial render
    await flushEffects()

    // Clear after initial render
    renderTracker.clear()

    // Bulk update
    storeInstance!.state.products['leg-1'].strike = 150
    storeInstance!.state.products['leg-1'].notional = 2000000
    storeInstance!.state.products['leg-1'].status = 'locked'

    // Wait for re-renders to settle
    await flushEffects()

    // React 18 automatic batching should result in a single render
    expect(renderTracker.log.length).toBe(1)
  })

  it('AC2: No intermediate states visible', async () => {
    const renderTracker = createRenderTracker()
    const store = createAppStore()

    const initialState = {
      products: {
        'leg-1': { strike: 100, notional: 1000000, status: 'active' },
      },
      market: { spot: 102 },
    }

    const concerns = {
      'products.leg-1.strike': {
        validationState: { schema: z.number().min(0).max(200) },
      },
    }

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

    mountStore(<TradeForm />, store, initialState, { concerns })

    await flushEffects()

    renderTracker.clear()

    storeInstance!.state.products['leg-1'].strike = 150

    await flushEffects()

    // Should only see final state
    expect(renderTracker.log[0]?.['strike']).toBe(150)
  })

  it('AC3: Concerns reflect final state', async () => {
    const renderTracker = createRenderTracker()
    const store = createAppStore()

    const initialState = {
      products: {
        'leg-1': { strike: 100, notional: 1000000, status: 'active' },
      },
      market: { spot: 102 },
    }

    const concerns = {
      'products.leg-1.strike': {
        validationState: { schema: z.number().min(0).max(200) },
      },
    }

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

    mountStore(<TradeForm />, store, initialState, { concerns })

    await flushEffects()

    renderTracker.clear()

    storeInstance!.state.products['leg-1'].strike = 150

    await flushEffects()

    // Validation should pass for 150
    expect(renderTracker.log[0]?.['valid']).toBe(true)
  })

  it('Performance target: < 16ms render time (60fps)', async () => {
    const renderTracker = createRenderTracker()
    const store = createAppStore()

    const initialState = {
      products: {
        'leg-1': { strike: 100, notional: 1000000, status: 'active' },
      },
      market: { spot: 102 },
    }

    const concerns = {
      [`products.${_('leg-1')}.strike`]: {
        validationState: { schema: z.number().min(0).max(200) },
        disabledWhen: {
          boolLogic: {
            IS_EQUAL: [`products.${_('leg-1')}.status`, 'locked'],
          },
        },
      },
    }

    let storeInstance: StoreInstance<AppState>

    const TradeForm = () => {
      const renderStart = performance.now()

      storeInstance = useStoreContext<AppState>()
      const snap = useSnapshot(storeInstance.state)
      const strikeValue = snap.products['leg-1'].strike
      const strikeConcerns =
        storeInstance._concerns[`products.${_('leg-1')}.strike`]

      const renderEnd = performance.now()
      const renderDuration = renderEnd - renderStart

      renderTracker.track({
        strike: strikeValue,
        valid: !(strikeConcerns?.['validationState'] as ValidationStateResult)
          ?.isError,
        renderDuration,
      })

      return (
        <input
          value={strikeValue}
          disabled={strikeConcerns?.['disabledWhen'] as boolean | undefined}
          readOnly
        />
      )
    }

    mountStore(<TradeForm />, store, initialState, { concerns })

    // Effects run synchronously, no wait needed
    renderTracker.clear()

    benchmark.start('react-render')
    storeInstance!.state.products['leg-1'].strike = 150
    await flushSync() // Use flushSync for performance tests (no 20ms setTimeout)
    const totalDuration = benchmark.end('react-render')

    // Total time should be < 5ms for React render + synchronous effects
    // This test uses Zod validation which is synchronous, no async validators
    expect(totalDuration).toBeLessThan(5)

    // Individual render should be under 16ms (60fps target)
    if (renderTracker.log.length > 0) {
      expect(renderTracker.log[0]?.['renderDuration']).toBeLessThan(16)
    }
  })

  it('No flashing or visual glitches during updates', async () => {
    const renderTracker = createRenderTracker()
    const store = createAppStore()

    const initialState = {
      products: {
        'leg-1': { strike: 100, notional: 1000000, status: 'active' },
      },
      market: { spot: 102 },
    }

    const concerns = {
      'products.leg-1.strike': {
        validationState: { schema: z.number().min(0).max(200) },
      },
    }

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

    mountStore(<TradeForm />, store, initialState, { concerns })

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
