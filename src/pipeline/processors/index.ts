/**
 * Pipeline Processors
 *
 * Individual processors for different types of side effects.
 * Note: Aggregations use effect() for read direction (sources → target)
 *       and pipeline preprocessing for write direction (target → sources)
 */

export { processAggregationWrites } from './aggregation-writes'
export { processFlipPaths } from './flip-paths'
export { processListeners } from './listeners'
export { processSyncPaths } from './sync-paths'
