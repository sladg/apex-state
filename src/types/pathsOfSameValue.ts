/**
 * PathsOfSameValue - Type-safe path tuples for side effects
 *
 * Simple tuple-based API for syncPaths, flipPaths, and aggregations.
 *
 * @example
 * ```typescript
 * type State = {
 *   user: { email: string }
 *   profile: { email: string }
 *   count: number
 * }
 *
 * // ✓ Valid: both paths are string
 * const sync: SyncPair<State> = ['user.email', 'profile.email']
 *
 * // ✗ Error: 'user.email' (string) can't sync with 'count' (number)
 * const invalid: SyncPair<State> = ['user.email', 'count']
 * ```
 */

import type { DeepKey } from './deepKey'
import type { DeepValue } from './deepValue'

/**
 * Get all paths in DATA that have the same value type as the value at PATH
 */
export type PathsWithSameValueAs<
  DATA extends object,
  PATH extends DeepKey<DATA>,
> = {
  [K in DeepKey<DATA>]: DeepValue<DATA, K> extends DeepValue<DATA, PATH>
    ? DeepValue<DATA, PATH> extends DeepValue<DATA, K>
      ? K
      : never
    : never
}[DeepKey<DATA>]

/**
 * A tuple of two paths that must have the same value type.
 * Format: [path1, path2]
 *
 * @example
 * const pair: SyncPair<State> = ['user.email', 'profile.email']
 */
export type SyncPair<DATA extends object> = {
  [P1 in DeepKey<DATA>]: [P1, PathsWithSameValueAs<DATA, P1>]
}[DeepKey<DATA>]

/**
 * A tuple of two paths for flip (alias for SyncPair)
 * Format: [path1, path2]
 *
 * @example
 * const pair: FlipPair<State> = ['isActive', 'isInactive']
 */
export type FlipPair<DATA extends object> = SyncPair<DATA>

/**
 * A tuple for aggregation: [target, source]
 * First element (left) is ALWAYS the target (aggregated) path.
 * Second element is a source path.
 *
 * Multiple pairs can point to same target for multi-source aggregation.
 *
 * @example
 * // target <- source (target is always first/left)
 * const aggs: AggregationPair<State>[] = [
 *   ['total', 'price1'],
 *   ['total', 'price2'],
 * ]
 */
export type AggregationPair<DATA extends object> = SyncPair<DATA>
