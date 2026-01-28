/**
 * Side effects module
 *
 * Exports registration functions for side effects:
 * - syncPaths: Keep multiple paths synchronized
 * - flipPaths: Keep paths with inverse values
 * - aggregations: Compute derived values from multiple sources
 * - listeners: React to path changes
 */

export {
  registerSideEffects,
  registerSyncPair,
  registerFlipPair,
  registerAggregation,
  registerListener,
} from './registration'
