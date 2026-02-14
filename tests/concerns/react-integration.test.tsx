/**
 * TEST-007: React Integration - useSnapshot Batching
 *
 * Priority: P0 (Critical)
 *
 * Validates that React re-renders are batched correctly with useSnapshot
 */

import { useSnapshot } from 'valtio'
import { describe, expect, it } from 'vitest'
import { z } from 'zod'

import type { ValidationStateResult } from '~/concerns/prebuilts'
import { useStoreContext } from '~/core/context'
import type { StoreInstance } from '~/core/types'
import { createGenericStore } from '~/store/createStore'
import { _ } from '~/utils/hashKey'

import { flush, renderWithStore } from '../utils/react'

interface AppState {
  products: {
    'item-1': { price: number; quantity: number; status: string }
  }
  shipping: { rate: number }
}

const initialState: AppState = {
  products: {
    'item-1': { price: 29.99, quantity: 1, status: 'in-stock' },
  },
  shipping: { rate: 5.99 },
}

const fullConcerns = {
  [`products.${_('item-1')}.price`]: {
    validationState: { schema: z.number().min(0).max(999) },
    disabledWhen: {
      condition: {
        IS_EQUAL: [`products.${_('item-1')}.status`, 'out-of-stock'],
      },
    },
  },
}

const validationOnlyConcerns = {
  'products.item-1.price': {
    validationState: { schema: z.number().min(0).max(999) },
  },
}

/** Simple render log — tracks what React rendered each cycle */
const createRenderLog = () => {
  const log: Record<string, unknown>[] = []
  return {
    log,
    track: (data: Record<string, unknown>) => {
      log.push(data)
    },
    clear: () => {
      log.length = 0
    },
  }
}

describe('TEST-007: React Integration', () => {
  it('AC1: Single re-render with React 18 batching', async () => {
    const renderLog = createRenderLog()
    const store = createGenericStore<AppState>()

    let storeInstance: StoreInstance<AppState>

    const CartItem = () => {
      storeInstance = useStoreContext<AppState>()
      const snap = useSnapshot(storeInstance.state)
      const priceConcerns =
        storeInstance._concerns[`products.${_('item-1')}.price`]

      renderLog.track({
        price: snap.products['item-1'].price,
        valid: !(priceConcerns?.['validationState'] as ValidationStateResult)
          ?.isError,
      })

      return <div>{snap.products['item-1'].price}</div>
    }

    renderWithStore(<CartItem />, store, structuredClone(initialState), {
      concerns: fullConcerns,
    })

    await flush()
    renderLog.clear()

    // Bulk update
    storeInstance!.state.products['item-1'].price = 39.99
    storeInstance!.state.products['item-1'].quantity = 3
    storeInstance!.state.products['item-1'].status = 'out-of-stock'

    await flush()

    // React 18 automatic batching should result in a single render
    expect(renderLog.log.length).toBe(1)
  })

  it('AC2: No intermediate states visible', async () => {
    const renderLog = createRenderLog()
    const store = createGenericStore<AppState>()

    let storeInstance: StoreInstance<AppState>

    const CartItem = () => {
      storeInstance = useStoreContext<AppState>()
      const snap = useSnapshot(storeInstance.state)

      renderLog.track({ price: snap.products['item-1'].price })

      return <div>{snap.products['item-1'].price}</div>
    }

    renderWithStore(<CartItem />, store, structuredClone(initialState), {
      concerns: validationOnlyConcerns,
    })

    await flush()
    renderLog.clear()

    storeInstance!.state.products['item-1'].price = 39.99
    await flush()

    // Should only see final state
    expect(renderLog.log[0]?.['price']).toBe(39.99)
  })

  it('AC3: Concerns reflect final state', async () => {
    const renderLog = createRenderLog()
    const store = createGenericStore<AppState>()

    let storeInstance: StoreInstance<AppState>

    const CartItem = () => {
      storeInstance = useStoreContext<AppState>()
      const snap = useSnapshot(storeInstance.state)
      const priceConcerns =
        storeInstance._concerns[`products.${_('item-1')}.price`]

      renderLog.track({
        valid: !(priceConcerns?.['validationState'] as ValidationStateResult)
          ?.isError,
      })

      return <div>{snap.products['item-1'].price}</div>
    }

    renderWithStore(<CartItem />, store, structuredClone(initialState), {
      concerns: validationOnlyConcerns,
    })

    await flush()
    renderLog.clear()

    storeInstance!.state.products['item-1'].price = 39.99
    await flush()

    // Validation should pass for 39.99
    expect(renderLog.log[0]?.['valid']).toBe(true)
  })

  it('No flashing or visual glitches during updates', async () => {
    const renderLog = createRenderLog()
    const store = createGenericStore<AppState>()

    let storeInstance: StoreInstance<AppState>

    const CartItem = () => {
      storeInstance = useStoreContext<AppState>()
      const snap = useSnapshot(storeInstance.state)
      const priceConcerns =
        storeInstance._concerns[`products.${_('item-1')}.price`]

      renderLog.track({
        price: snap.products['item-1'].price,
        className: !(
          priceConcerns?.['validationState'] as ValidationStateResult
        )?.isError
          ? 'valid'
          : 'error',
      })

      return <div>{snap.products['item-1'].price}</div>
    }

    renderWithStore(<CartItem />, store, structuredClone(initialState), {
      concerns: validationOnlyConcerns,
    })

    await flush()
    renderLog.clear()

    // Multiple rapid updates
    storeInstance!.state.products['item-1'].price = 39.99
    storeInstance!.state.products['item-1'].quantity = 5

    await flush()

    // Should render with consistent valid state (no flashing between error/valid)
    renderLog.log.forEach((entry) => {
      expect(entry['className']).toBe('valid')
    })
  })
})
