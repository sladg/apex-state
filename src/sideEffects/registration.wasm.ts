/**
 * WASM Implementation - Side Effects Registration
 *
 * Consolidates all side effects (sync, flip, aggregation, listeners) into a single
 * WASM call for efficiency. No legacy fallback logic - assumes WASM is loaded.
 */

import type { StoreInstance } from '../core/types'
import { applyBatch } from '../pipeline/apply-batch'
import { processChanges } from '../pipeline/process-changes'
import type { GenericMeta } from '../types'
import type { SideEffects } from '../types/side-effects'
import { wasm } from '../wasm/bridge'

/** Auto-incrementing subscriber ID counter for O(1) handler lookup. */
let nextSubscriberId = 0

/** Reset the subscriber ID counter (testing only). */
export const resetSubscriberIdCounter = (): void => {
  nextSubscriberId = 0
}

export const registerSideEffects = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  store: StoreInstance<DATA, META>,
  id: string,
  effects: SideEffects<DATA, META>,
): (() => void) => {
  // Build consolidated side effects registration
  const syncPairs: [string, string][] = effects.syncPaths ?? []
  const flipPairs: [string, string][] = effects.flipPaths ?? []
  const aggregationPairs: [string, string][] = effects.aggregations ?? []

  // Build listeners array
  const listeners = effects.listeners?.map((listener) => {
    const { listenerHandlers } = store._internal.graphs

    // Assign unique subscriber_id for O(1) handler lookup
    const subscriberId = nextSubscriberId++

    // Wrap fn with timing measurement
    const originalFn = listener.fn
    const mapKey = listener.path ?? ''
    const wrappedFn = (changes: any, state: any) =>
      store._internal.timing.run(
        'listeners',
        () => originalFn(changes, state),
        {
          path: mapKey,
          name: 'listener',
        },
      )

    // Store in handler map for execution
    listenerHandlers.set(subscriberId, {
      scope: listener.scope,
      fn: wrappedFn,
    })

    return {
      subscriber_id: subscriberId,
      topic_path: listener.path ?? '',
      scope_path: listener.scope ?? '',
    }
  })

  // Single consolidated WASM call with all side effects
  const registrationId = `sideEffects-${id}`
  const result = wasm.registerSideEffects({
    registration_id: registrationId,
    ...(syncPairs.length > 0 && { sync_pairs: syncPairs }),
    ...(flipPairs.length > 0 && { flip_pairs: flipPairs }),
    ...(aggregationPairs.length > 0 && { aggregation_pairs: aggregationPairs }),
    ...(listeners && listeners.length > 0 && { listeners }),
  })

  // Apply sync changes to valtio
  if (result.sync_changes.length > 0) {
    const syncChanges = result.sync_changes.map((c) => [
      c.path,
      c.value,
      { isSyncPathChange: true },
    ]) as any
    processChanges(store, syncChanges)
  }

  // Apply aggregation changes directly to state
  if (result.aggregation_changes.length > 0) {
    applyBatch(
      result.aggregation_changes.map((c) => [c.path, c.value, {}]) as any,
      store.state,
    )
  }

  // Create cleanup function
  const cleanup = () => {
    wasm.unregisterSideEffects(registrationId)

    // Clean up listener handlers
    effects.listeners?.forEach((_listener, index) => {
      if (listeners && listeners[index]) {
        store._internal.graphs.listenerHandlers.delete(
          listeners[index].subscriber_id,
        )
      }
    })

    store._internal.registrations.sideEffectCleanups.delete(id)
  }

  // Store cleanup reference
  store._internal.registrations.sideEffectCleanups.set(id, cleanup)

  return cleanup
}
