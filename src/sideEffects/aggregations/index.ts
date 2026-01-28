/**
 * Aggregations side-effect exports
 *
 * Provides aggregation functionality where multiple source paths
 * aggregate into a single target path with cycle detection.
 */

export { createAggregationGraph } from './graph'
export type { AggregationGraph } from './graph'

export { createAggregationsRegistry } from './registry'
export type { AggregationsRegistry } from './registry'

export type { Aggregation, AggregationConfig } from './types'
