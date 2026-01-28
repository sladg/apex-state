/**
 * SideEffects type definition
 *
 * Configuration types for side effects passed to useSideEffects hook.
 * Simple tuple-based API: [path1, path2]
 */

import type { SyncPair, FlipPair, AggregationPair } from './pathsOfSameValue'

/**
 * Side effects configuration for useSideEffects hook
 *
 * @example
 * ```typescript
 * useSideEffects('my-effects', {
 *   syncPaths: [
 *     ['user.email', 'profile.email'],
 *   ],
 *   flipPaths: [
 *     ['isActive', 'isInactive'],
 *   ],
 *   aggregations: [
 *     ['total', 'price1'],  // target <- source (target always first)
 *     ['total', 'price2'],
 *   ],
 * })
 * ```
 */
export interface SideEffects<DATA extends object> {
  /**
   * Sync paths - keeps specified paths synchronized
   * Format: [path1, path2] - both paths stay in sync
   */
  syncPaths?: Array<SyncPair<DATA>>

  /**
   * Flip paths - keeps specified paths with opposite values
   * Format: [path1, path2] - paths have inverse boolean values
   */
  flipPaths?: Array<FlipPair<DATA>>

  /**
   * Aggregations - aggregates sources into target
   * Format: [target, source] - target is ALWAYS first (left)
   * Multiple pairs can point to same target for multi-source aggregation
   */
  aggregations?: Array<AggregationPair<DATA>>

  [key: string]: unknown
}
