/**
 * WASM Implementation - Side Effects Registration
 *
 * Consolidates all side effects (sync, flip, aggregation, listeners) into a single
 * WASM call for efficiency. No legacy fallback logic - assumes WASM is loaded.
 */

import type { StoreInstance } from '../core/types'
import { applyBatch } from '../pipeline/apply-batch'
import type { GenericMeta } from '../types'
import type { SideEffects } from '../types/side-effects'
import type { WasmAggregationPair, WasmComputationPair } from '../wasm/bridge'

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
  const aggregationPairs = (effects.aggregations ?? []).map(
    ([target, source, condition]) =>
      condition
        ? [target as string, source as string, JSON.stringify(condition)]
        : [target as string, source as string],
  ) as WasmAggregationPair[]

  // Serialize computations: [op, target, source, condition?] → WASM format
  const computationPairs = (effects.computations ?? []).map(
    ([op, target, source, condition]) =>
      condition
        ? [
            op as string,
            target as string,
            source as string,
            JSON.stringify(condition),
          ]
        : [op as string, target as string, source as string],
  ) as WasmComputationPair[]

  // Transform clearPaths: public API format → WASM format
  // When expandMatch: true, rewrite [*] → [**] in target paths
  const clearPaths = effects.clearPaths?.map(([triggers, targets, opts]) => ({
    triggers: triggers as string[],
    targets: (opts?.expandMatch
      ? (targets as string[]).map((t) => t.replace(/\[\*\]/g, '[**]'))
      : targets) as string[],
  }))

  // Build listeners array
  const listeners = effects.listeners?.map((listener) => {
    const { listenerHandlers } = store._internal.graphs

    // Assign unique subscriber_id for O(1) handler lookup
    const subscriberId = nextSubscriberId++

    // Wrap fn with timing measurement
    const originalFn = listener.fn
    // Default scope to path when omitted (undefined)
    const effectiveScope =
      listener.scope === undefined ? listener.path : listener.scope

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
      scope: effectiveScope,
      fn: wrappedFn,
      name: originalFn.name || '(anonymous)',
    })

    return {
      subscriber_id: subscriberId,
      topic_path: listener.path ?? '',
      scope_path: effectiveScope ?? '',
    }
  })

  // Single consolidated WASM call with all side effects
  const pipeline = store._internal.pipeline!
  const registrationId = `sideEffects-${id}`
  const registration = {
    registration_id: registrationId,
    ...(syncPairs.length > 0 && { sync_pairs: syncPairs }),
    ...(flipPairs.length > 0 && { flip_pairs: flipPairs }),
    ...(aggregationPairs.length > 0 && { aggregation_pairs: aggregationPairs }),
    ...(computationPairs.length > 0 && {
      computation_pairs: computationPairs,
    }),
    ...(clearPaths && clearPaths.length > 0 && { clear_paths: clearPaths }),
    ...(listeners && listeners.length > 0 && { listeners }),
  }
  const result = pipeline.registerSideEffects(registration)

  // Notify observer (DevTools + console logging)
  store._internal.observer.event('registration:add', {
    id,
    registration,
    result,
  })

  // Apply sync changes directly to valtio state.
  // IMPORTANT: Do NOT route through processChanges() here. The Rust register_sync_batch()
  // already updated shadow state for initial sync changes. Routing through processChanges()
  // would cause the diff pre-pass to see "no change" (shadow already updated), return
  // has_work=false, and skip writing to valtio. Use applyBatch() directly, same as
  // aggregation_changes below, so valtio state matches shadow state.
  if (result.sync_changes.length > 0) {
    applyBatch(
      result.sync_changes.map((c) => [c.path, c.value, {}]) as any,
      store.state,
    )
  }

  // Apply aggregation changes directly to state
  if (result.aggregation_changes.length > 0) {
    applyBatch(
      result.aggregation_changes.map((c) => [c.path, c.value, {}]) as any,
      store.state,
    )
  }

  // Apply computation changes directly to state
  if (result.computation_changes.length > 0) {
    applyBatch(
      result.computation_changes.map((c) => [c.path, c.value, {}]) as any,
      store.state,
    )
  }

  // Create cleanup function
  const cleanup = () => {
    pipeline.unregisterSideEffects(registrationId)
    store._internal.observer.event('registration:remove', id)

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
