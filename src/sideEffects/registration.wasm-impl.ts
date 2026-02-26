/**
 * WASM Implementation - Side Effects Registration
 *
 * Consolidates all side effects (sync, flip, aggregation, listeners) into a single
 * WASM call for efficiency. No legacy fallback logic - assumes WASM is loaded.
 */

import type { MultiPathListener, StoreInstance } from '../core/types'
import { applyBatch } from '../pipeline/apply-batch'
import type { GenericMeta } from '../types'
import type { Change } from '../types/changes'
import { changes } from '../types/changes'
import { pairs } from '../types/pairs'
import type { SideEffects } from '../types/side-effects'
import type { Wasm } from '../wasm/bridge'

/** Build a StageTrace from a stage name and change array. Returns null if empty. */
const stageFromChanges = (
  stage: Wasm.StageTrace['stage'],
  ch: Change[],
): Wasm.StageTrace | null => {
  if (ch.length === 0) return null
  return {
    stage,
    duration_us: 0,
    accepted: [],
    produced: ch.map((c) => c.path),
    skipped: [],
    followup: [],
  }
}

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
    flip_pairs: flipPairs,
    aggregation_pairs: aggregationPairs,
    computation_pairs: computationPairs,
    clear_paths: clearPaths ?? [],
    listeners: listeners ?? [],
    anchor_path: effects.anchorPath as string,
  }
  const result = pipeline.registerSideEffects(registration)

  // Log registration with graph snapshot and initial stage trace (no-op when log is disabled)
  const snapshot = pipeline.getGraphSnapshot()
  const traceStages = [
    stageFromChanges('sync', result.sync_changes),
    stageFromChanges('aggregation_read', result.aggregation_changes),
    stageFromChanges('computation', result.computation_changes),
  ].filter((s): s is Wasm.StageTrace => s !== null)
  const trace: Wasm.PipelineTrace | undefined =
    traceStages.length > 0
      ? { total_duration_us: 0, stages: traceStages }
      : undefined
  store._internal.logger.logRegistration('register', id, snapshot, trace)
  store._internal.devtools?.notifyRegistration('register', id, snapshot)

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
