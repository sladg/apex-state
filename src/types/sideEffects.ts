/**
 * SideEffects type definition
 *
 * Configuration types for all side effects including sync paths, flip paths,
 * listeners, validators, aggregations, and clear paths.
 */

import type { DeepKey } from './deepKey'
import type { ClearPathConfig } from '../sideEffects/clearPaths/types'

/**
 * Configuration for sync paths side-effect
 */
export interface SyncPathConfig<DATA> {
  pairs: Array<{
    id: string
    path1: DeepKey<DATA>
    path2: DeepKey<DATA>
  }>
}

/**
 * Configuration for flip paths side-effect
 */
export interface FlipPathConfig<DATA> {
  pairs: Array<{
    id: string
    path1: DeepKey<DATA>
    path2: DeepKey<DATA>
  }>
}

/**
 * Configuration for clear paths side-effect
 */
export interface ClearPathsConfig<DATA> {
  rules: Array<ClearPathConfig<DATA>>
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

/**
 * Side effects configuration for a store
 */
export interface SideEffects<DATA> {
  /**
   * Sync paths configuration - keeps specified paths synchronized
   */
  syncPaths?: SyncPathConfig<DATA>

  /**
   * Flip paths configuration - keeps specified paths with opposite values
   */
  flipPaths?: FlipPathConfig<DATA>

  /**
   * Clear paths configuration - clears paths when triggers fire
   */
  clearPaths?: ClearPathsConfig<DATA>

  /**
   * Aggregations configuration - aggregates multiple sources into target
   */
  aggregations?: AggregationConfig<DATA>

  // Placeholder for future side effects
  // listeners?: ...
  // validators?: ...
  [key: string]: unknown
}
