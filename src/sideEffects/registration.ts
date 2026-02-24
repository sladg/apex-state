import type { StoreInstance } from '../core/types'
import type { AggregationPair, GenericMeta } from '../types'
import type { SideEffects } from '../types/side-effects'
import { registerAggregations } from './prebuilts/aggregation'
import { registerFlipPair } from './prebuilts/flip'
import { registerListenerLegacy } from './prebuilts/listeners'
import { registerSyncPairsBatch } from './prebuilts/sync'

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
  // PERF: Uses registerSyncPairsBatch (1 processChanges call) instead of per-pair loop.
  // Do NOT revert to a loop over registerSyncPair — causes N × M redundant effect evaluations.
  // See docs/SIDE_EFFECTS_GUIDE.md "Batched Registration" section.
  if (effects.syncPaths) {
    const pairs = effects.syncPaths as [string, string][]
    const cleanup = registerSyncPairsBatch(store, pairs)
    cleanups.push(cleanup)
  }

  // Register flip paths: [path1, path2]
  if (effects.flipPaths) {
    const pairs = effects.flipPaths as [string, string][]
    for (const [path1, path2] of pairs) {
      const cleanup = registerFlipPair(store, path1, path2)
      cleanups.push(cleanup)
    }
  }

  // Register aggregations: [target, source] - target always first
  if (effects.aggregations) {
    const pairs = effects.aggregations as AggregationPair<DATA>[]
    const cleanup = registerAggregations(store, id, pairs)
    cleanups.push(cleanup)
  }

  // Register clear paths: not supported in legacy mode (requires WASM)
  // clearPaths rules are silently ignored in legacy implementation

  // Register listeners: { path, scope, fn }
  if (effects.listeners) {
    for (const listener of effects.listeners) {
      const cleanup = registerListenerLegacy(store, listener)
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
