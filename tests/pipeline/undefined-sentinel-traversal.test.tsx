/**
 * Undefined sentinel traversal
 *
 * Regression test: setting a nested path through a field that holds the
 * undefined sentinel ("__APEX_UNDEFINED__") must promote to an object,
 * not error with "Cannot traverse through primitive at <key>".
 *
 * Null values already handled this correctly; the bug was specific to
 * the sentinel string used for JS `undefined` crossing the WASM boundary.
 */

import { describe, expect, it } from 'vitest'

import type { ArrayOfChanges, GenericMeta } from '~/types'

import { createGenericStore } from '../../src'
import { flushEffects, flushSync, MODES, mountStore } from '../utils/react'

// Type uses non-optional objects so DeepKey generates nested paths like
// "shippingAddress.street". At runtime we set the parent to undefined
// via setChanges, then set a child path — triggering sentinel traversal.
interface TraversalState {
  shippingAddress: { street: string; city: string }
  order: { payment: { method: string } }
}

describe.each(MODES)('[$name] Undefined sentinel traversal', ({ config }) => {
  it('should set child path after parent becomes undefined', async () => {
    const store = createGenericStore<TraversalState>(config)
    const { storeInstance, setValue, setChanges } = mountStore(store, {
      shippingAddress: { street: '123 Main St', city: 'Portland' },
      order: { payment: { method: 'credit_card' } },
    })
    await flushEffects()

    // Set the field to undefined (creates the sentinel in shadow state)
    setChanges([
      ['shippingAddress', undefined as never, {} as GenericMeta],
    ] as ArrayOfChanges<TraversalState, GenericMeta>)
    await flushEffects()
    expect(storeInstance.state.shippingAddress).toBeUndefined()

    // Now set a child path through the undefined field — this triggers
    // shadow state traversal through the sentinel string
    setValue('shippingAddress.street', '456 Oak Ave')
    await flushEffects()

    expect(storeInstance.state.shippingAddress.street).toBe('456 Oak Ave')
  })

  it('should set deeply nested child after parent becomes undefined', async () => {
    const store = createGenericStore<TraversalState>(config)
    const { storeInstance, setValue, setChanges } = mountStore(store, {
      shippingAddress: { street: '123 Main St', city: 'Portland' },
      order: { payment: { method: 'credit_card' } },
    })
    await flushEffects()

    // Set order.payment to undefined
    setChanges([
      ['order.payment', undefined as never, {} as GenericMeta],
    ] as ArrayOfChanges<TraversalState, GenericMeta>)
    await flushEffects()
    expect(storeInstance.state.order.payment).toBeUndefined()

    // Set a child path through the undefined nested field
    setValue('order.payment.method', 'paypal')
    await flushEffects()

    expect(storeInstance.state.order.payment.method).toBe('paypal')
  })

  it('should handle field that starts as undefined', async () => {
    const store = createGenericStore<TraversalState>(config)
    const { storeInstance, setValue } = mountStore(store, {
      shippingAddress: undefined as never,
      order: { payment: undefined as never },
    })
    await flushEffects()

    // Set a child path on initially-undefined field
    setValue('shippingAddress.street', '789 Elm Blvd')
    await flushEffects()

    expect(storeInstance.state.shippingAddress.street).toBe('789 Elm Blvd')
  })
})

// ---------------------------------------------------------------------------
// Sync paths: undefined propagates via sync, then child path set through it
// ---------------------------------------------------------------------------

interface SyncTraversalState {
  billingAddress: { street: string; city: string }
  shippingAddress: { street: string; city: string }
}

describe.each(MODES)(
  '[$name] Undefined sentinel traversal with syncPaths',
  ({ config }) => {
    it('should set child path after sync propagates undefined', async () => {
      const store = createGenericStore<SyncTraversalState>(config)

      const Component = () => {
        store.useSideEffects('sync-test', {
          syncPaths: [['billingAddress', 'shippingAddress']],
        })
        return null
      }

      const { storeInstance, setValue, setChanges } = mountStore(
        <Component />,
        store,
        {
          billingAddress: { street: '100 First Ave', city: 'Boston' },
          shippingAddress: { street: '100 First Ave', city: 'Boston' },
        },
      )
      await flushSync()

      // Set billingAddress to undefined — sync propagates to shippingAddress
      setChanges([
        ['billingAddress', undefined as never, {} as GenericMeta],
      ] as ArrayOfChanges<SyncTraversalState, GenericMeta>)
      await flushSync()

      expect(storeInstance.state.billingAddress).toBeUndefined()
      expect(storeInstance.state.shippingAddress).toBeUndefined()

      // Now set a child path through the synced-to-undefined field
      setValue('shippingAddress.street', '200 Second St')
      await flushSync()

      expect(storeInstance.state.shippingAddress.street).toBe('200 Second St')
    })

    it('should recover synced field after undefined round-trip', async () => {
      const store = createGenericStore<SyncTraversalState>(config)

      const Component = () => {
        store.useSideEffects('sync-test', {
          syncPaths: [['billingAddress', 'shippingAddress']],
        })
        return null
      }

      const { storeInstance, setValue, setChanges } = mountStore(
        <Component />,
        store,
        {
          billingAddress: { street: '100 First Ave', city: 'Boston' },
          shippingAddress: { street: '100 First Ave', city: 'Boston' },
        },
      )
      await flushSync()

      // Set billing to undefined → syncs to shipping
      setChanges([
        ['billingAddress', undefined as never, {} as GenericMeta],
      ] as ArrayOfChanges<SyncTraversalState, GenericMeta>)
      await flushSync()

      // Restore billing with a full object → should sync to shipping
      setValue('billingAddress', { street: '300 Third Blvd', city: 'Chicago' })
      await flushSync()

      expect(storeInstance.state.billingAddress).toEqual({
        street: '300 Third Blvd',
        city: 'Chicago',
      })
      expect(storeInstance.state.shippingAddress).toEqual({
        street: '300 Third Blvd',
        city: 'Chicago',
      })
    })
  },
)
