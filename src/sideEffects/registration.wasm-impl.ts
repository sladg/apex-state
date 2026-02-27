/**
 * WASM Implementation - Side Effects Registration
 *
 * Consolidates all side effects (sync, flip, aggregation, listeners) into a single
 * WASM call for efficiency. No legacy fallback logic - assumes WASM is loaded.
 */

import { snapshot } from 'valtio'

import type { MultiPathListener, StoreInstance } from '../core/types'
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
  const t0 = performance.now()

  // Build consolidated side effects registration
  const { bidirectional: syncPairs, directed: directedSyncPairs } =
    pairs.syncToWasm(effects.syncPaths ?? [])

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

  // Type guard to distinguish MultiPathListener from SinglePathListener
  const isMultiPath = (
    listener: unknown,
  ): listener is MultiPathListener<DATA, META> =>
    Array.isArray((listener as { path: unknown }).path)

  // Build listeners array
  const listeners = effects.listeners?.map((listener) => {
    const { listenerHandlers } = store._internal.registrations

    // Assign unique subscriber_id for O(1) handler lookup
    const subscriberId = nextSubscriberId++

    const originalFn = listener.fn

    // Discriminated union: TypeScript narrows the type based on path structure
    const topicPaths = isMultiPath(listener)
      ? listener.path
      : [listener.path ?? '']

    // For single-path: scope defaults to path when NOT explicitly set (undefined).
    // null is an explicit "no scope" value → full state is passed to the handler.
    // For multi-path: scope must be explicitly set (required in type).
    const effectiveScope = isMultiPath(listener)
      ? listener.scope
      : listener.scope === undefined
        ? listener.path
        : listener.scope

    // Store in handler map for execution
    listenerHandlers.set(subscriberId, {
      scope: effectiveScope,
      fn: originalFn,
      name: originalFn.name || '(anonymous)',
      registrationId: `sideEffects-${id}`,
    })

    return {
      subscriber_id: subscriberId,
      topic_paths: topicPaths,
      scope_path: effectiveScope ?? '',
    }
  })

  // Single consolidated WASM call with all side effects
  const pipeline = store._internal.pipeline!
  const registrationId = `sideEffects-${id}`
  const registration = {
    registration_id: registrationId,
    sync_pairs: syncPairs,
    directed_sync_pairs: directedSyncPairs,
    flip_pairs: flipPairs,
    aggregation_pairs: aggregationPairs,
    computation_pairs: computationPairs,
    clear_paths: clearPaths ?? [],
    listeners: listeners ?? [],
    anchor_path: effects.anchorPath as string,
  }

  const result = pipeline.registerSideEffects(registration)

  // Apply sync changes directly to valtio state.
  // IMPORTANT: Do NOT route through processChanges() here — it would diff against shadow,
  // see "no change" (shadow was already updated by register_sync_batch), return
  // has_work=false, and skip writing to valtio.
  //
  // Instead: register_sync_batch returns ALL initial sync changes (not just the
  // shadow-diffed subset), and applyBatch's own `current !== value` guard handles
  // valtio-level idempotency. This covers two divergence scenarios:
  //   - Direct proxy mutations (bypassing processChanges) leave shadow ahead of valtio
  //   - Navigation remount with preserved pipeline reuses stale shadow
  // In both cases, applyBatch correctly writes the synced value to valtio even
  // when shadow already has it.
  const appliedChanges = [
    ...result.sync_changes,
    ...result.aggregation_changes,
    ...result.computation_changes,
  ]

  // Apply changes to state
  if (appliedChanges.length > 0) {
    applyBatch(changes.fromWasm<DATA>(appliedChanges), store.state)
  }

  const durationMs = performance.now() - t0

  // Log registration with graph snapshot (no-op when log is disabled)
  const graphSnapshot = pipeline.getGraphSnapshot()
  store._internal.logger.logRegistration('register', id, graphSnapshot, {
    result,
    appliedChanges,
    stateSnapshot: snapshot(store.state),
    durationMs,
  })
  store._internal.devtools?.notifyRegistration('register', id, graphSnapshot)

  // Create cleanup function
  const cleanup = () => {
    const ut0 = performance.now()
    pipeline.unregisterSideEffects(registrationId)
    const unregDurationMs = performance.now() - ut0
    const unregSnapshot = pipeline.getGraphSnapshot()
    store._internal.logger.logRegistration('unregister', id, unregSnapshot, {
      stateSnapshot: snapshot(store.state),
      durationMs: unregDurationMs,
    })
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
