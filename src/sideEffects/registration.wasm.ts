/**
 * WASM Implementation - Side Effects Registration
 *
 * Registers all side effects using WASM implementations directly.
 * No legacy fallback logic - this version assumes WASM is loaded.
 */

import type { StoreInstance } from '../core/types'
import type { GenericMeta } from '../types'
import type { SideEffects } from '../types/side-effects'
import { registerAggregations as registerAggregationsWasm } from './prebuilts/aggregation.wasm'
import { registerFlipPair as registerFlipPairWasm } from './prebuilts/flip.wasm'
import { registerListener as registerListenerWasm } from './prebuilts/listeners.wasm'
import { registerSyncPairsBatch as registerSyncPairsBatchWasm } from './prebuilts/sync.wasm'

const registerSideEffectsImpl = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  store: StoreInstance<DATA, META>,
  id: string,
  effects: SideEffects<DATA, META>,
): (() => void) => {
  const cleanups: (() => void)[] = []

  // Register sync paths: [path1, path2]
  // WASM computes initial sync from shadow state and returns changes
  if (effects.syncPaths) {
    const cleanup = registerSyncPairsBatchWasm(store, effects.syncPaths)
    cleanups.push(cleanup)
  }

  // Register flip paths: [path1, path2]
  // WASM handles flip graph and initial flip computation
  if (effects.flipPaths) {
    for (const [path1, path2] of effects.flipPaths) {
      const cleanup = registerFlipPairWasm(store, path1, path2)
      cleanups.push(cleanup)
    }
  }

  // Register aggregations: [target, source] - target always first
  // WASM computes initial aggregation values from shadow state
  if (effects.aggregations) {
    const cleanup = registerAggregationsWasm(store, id, effects.aggregations)
    cleanups.push(cleanup)
  }

  // Register listeners: { path, scope, fn }
  // WASM handles topic routing and dispatch ordering
  if (effects.listeners) {
    for (const listener of effects.listeners) {
      const cleanup = registerListenerWasm(store, listener)
      cleanups.push(cleanup)
    }
  }

  // Store cleanup reference
  const combinedCleanup = () => cleanups.forEach((fn) => fn())
  store._internal.registrations.sideEffectCleanups.set(id, combinedCleanup)

  return () => {
    combinedCleanup()
    store._internal.registrations.sideEffectCleanups.delete(id)
  }
}

export const registerSideEffects = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  store: StoreInstance<DATA, META>,
  id: string,
  effects: SideEffects<DATA, META>,
): (() => void) =>
  store._internal.timing.run(
    'registration',
    () => registerSideEffectsImpl(store, id, effects),
    { path: id, name: 'sideEffects' },
  )
