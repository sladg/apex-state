import type { StoreInstance } from '../core/types'
import type { GenericMeta } from '../types'
import type { SideEffects } from '../types/sideEffects'
import { registerAggregations } from './prebuilts/aggregation'
import { registerFlipPair } from './prebuilts/flip'
import { registerSyncPair } from './prebuilts/sync'

export const registerSideEffects = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  store: StoreInstance<DATA, META>,
  id: string,
  effects: SideEffects<DATA, META>,
): (() => void) => {
  const cleanups: (() => void)[] = []

  // Register sync paths: [path1, path2]
  if (effects.syncPaths) {
    for (const [path1, path2] of effects.syncPaths) {
      const cleanup = registerSyncPair(store, path1, path2)
      cleanups.push(cleanup)
    }
  }

  // Register flip paths: [path1, path2]
  if (effects.flipPaths) {
    for (const [path1, path2] of effects.flipPaths) {
      const cleanup = registerFlipPair(store, path1, path2)
      cleanups.push(cleanup)
    }
  }

  // Register aggregations: [target, source] - target always first
  if (effects.aggregations) {
    const cleanup = registerAggregations(store, id, effects.aggregations)
    cleanups.push(cleanup)
  }

  // Store cleanup reference
  const combinedCleanup = () => cleanups.forEach((fn) => fn())
  store._internal.registrations.sideEffectCleanups.set(id, combinedCleanup)

  return () => {
    combinedCleanup()
    store._internal.registrations.sideEffectCleanups.delete(id)
  }
}
