/**
 * Side effects module
 *
 * Exports registration functions for side effects:
 * - syncPaths: Keep multiple paths synchronized
 * - flipPaths: Keep paths with inverse values
 * - aggregations: Compute derived values from multiple sources
 * - listeners: React to path changes
 */

export { registerFlipPair } from './prebuilts/flip'
export { registerListenerLegacy } from './prebuilts/listeners'
export { registerSyncPairsBatch } from './prebuilts/sync'
export { registerSideEffects } from './registration'
