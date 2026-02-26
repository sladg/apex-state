/**
 * WASM Implementation - Side Effects Registration
 *
 * Consolidates all side effects (sync, flip, aggregation, listeners) into a single
 * WASM call for efficiency. No legacy fallback logic - assumes WASM is loaded.
 */

import type { StoreInstance } from '../core/types'
import { applyBatch } from '../pipeline/apply-batch'
import type { GenericMeta } from '../types'
import { changes } from '../types/changes'
import { pairs } from '../types/pairs'
import type { SideEffects } from '../types/side-effects'

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
  store: StoreInstance<DATA>,
  id: string,
  effects: SideEffects<DATA, META>,
): (() => void) => {
  // Build consolidated side effects registration
  const syncPairs = effects.syncPaths ?? []
  const flipPairs = effects.flipPaths ?? []
  const aggregationPairs = pairs.aggregationToWasm(effects.aggregations ?? [])

  // Serialize computations: [op, target, source, condition?] → WASM format
  const computationPairs = pairs.computationToWasm(effects.computations ?? [])

  // Transform clearPaths: public API format → WASM format
  // When expandMatch: true, rewrite [*] → [**] in target paths
  const clearPaths = effects.clearPaths?.map(([triggers, targets, opts]) => ({
    triggers: triggers,
    targets: opts?.expandMatch
      ? targets.map((t) => t.replace(/\[\*\]/g, '[**]'))
      : targets,
  }))

  // Build listeners array
  const listeners = effects.listeners?.map((listener) => {
    const { listenerHandlers } = store._internal.registrations

    // Assign unique subscriber_id for O(1) handler lookup
    const subscriberId = nextSubscriberId++

    const originalFn = listener.fn
    // Default scope to path when omitted (undefined)
    const effectiveScope =
      listener.scope === undefined ? listener.path : listener.scope

    // Store in handler map for execution
    listenerHandlers.set(subscriberId, {
      scope: effectiveScope,
      fn: originalFn,
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
    sync_pairs: syncPairs,
    flip_pairs: flipPairs,
    aggregation_pairs: aggregationPairs,
    computation_pairs: computationPairs,
    clear_paths: clearPaths ?? [],
    listeners: listeners ?? [],
  }
  const result = pipeline.registerSideEffects(registration)

  // Log registration with graph snapshot (no-op when log is disabled)
  const snapshot = pipeline.getGraphSnapshot()
  store._internal.logger.logRegistration('register', id, snapshot)
  store._internal.devtools?.notifyRegistration('register', id, snapshot)

  // Apply sync changes directly to valtio state.
  // IMPORTANT: Do NOT route through processChanges() here. The Rust register_sync_batch()
  // already updated shadow state for initial sync changes. Routing through processChanges()
  // would cause the diff pre-pass to see "no change" (shadow already updated), return
  // has_work=false, and skip writing to valtio. Use applyBatch() directly, same as
  // aggregation_changes below, so valtio state matches shadow state.
  if (result.sync_changes.length > 0) {
    applyBatch(changes.fromWasm<DATA>(result.sync_changes), store.state)
  }

  // Apply aggregation changes directly to state
  if (result.aggregation_changes.length > 0) {
    applyBatch(changes.fromWasm<DATA>(result.aggregation_changes), store.state)
  }

  // Apply computation changes directly to state
  if (result.computation_changes.length > 0) {
    applyBatch(changes.fromWasm<DATA>(result.computation_changes), store.state)
  }

  // Create cleanup function
  const cleanup = () => {
    pipeline.unregisterSideEffects(registrationId)
    const unregSnapshot = pipeline.getGraphSnapshot()
    store._internal.logger.logRegistration('unregister', id, unregSnapshot)
    store._internal.devtools?.notifyRegistration(
      'unregister',
      id,
      unregSnapshot,
    )

    // Clean up listener handlers
    effects.listeners?.forEach((_listener, index) => {
      if (listeners && listeners[index]) {
        store._internal.registrations.listenerHandlers.delete(
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
