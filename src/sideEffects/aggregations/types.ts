/**
 * Type definitions for aggregations side-effect
 *
 * Defines the structure of aggregation relationships where:
 * - Multiple source paths aggregate into a single target path
 * - When all sources are equal, target receives that value
 * - When sources differ, target becomes undefined
 * - When target changes, all sources are set to that value
 */

import type { DeepKey } from '../../types'

/**
 * Single aggregation rule defining target←sources relationship
 */
export interface Aggregation<DATA extends object> {
  /**
   * Unique identifier for this aggregation
   */
  id: string

  /**
   * Target path that aggregates values from sources
   */
  targetPath: DeepKey<DATA>

  /**
   * Source paths whose values are aggregated into target
   */
  sourcePaths: DeepKey<DATA>[]
}

/**
 * Configuration for aggregations side-effect
 */
export interface AggregationConfig<DATA> {
  rules: Array<{
    id: string
    targetPath: DeepKey<DATA>
    sourcePaths: DeepKey<DATA>[]
  }>
}
