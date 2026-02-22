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

import type { BoolLogic } from './bool-logic'
import type { DeepKey } from './deep-key'
import type { DeepKeyFiltered } from './deep-key-filtered'
import type { DeepValue } from './deep-value'

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
 * A tuple for aggregation: [target, source] or [target, source, excludeWhen]
 * First element (left) is ALWAYS the target (aggregated) path.
 * Second element is a source path.
 * Optional third element is a BoolLogic condition — when true, this source is excluded.
 *
 * Multiple pairs can point to same target for multi-source aggregation.
 *
 * @example
 * // target <- source (target is always first/left)
 * const aggs: AggregationPair<State>[] = [
 *   ['total', 'price1'],
 *   ['total', 'price2', { IS_EQUAL: ['price2.disabled', true] }],
 * ]
 */
export type AggregationPair<DATA extends object> = {
  [P1 in DeepKey<DATA>]:
    | [P1, PathsWithSameValueAs<DATA, P1>]
    | [P1, PathsWithSameValueAs<DATA, P1>, BoolLogic<DATA>]
}[DeepKey<DATA>]

/**
 * Supported computation operations for numeric reduction.
 */
export type ComputationOp = 'SUM' | 'AVG'

/**
 * A tuple for computation: [operation, target, source] or [operation, target, source, excludeWhen]
 * First element is the operation (SUM, AVG).
 * Second element is the target path (must be a number path).
 * Third element is a source path (must be a number path).
 * Optional fourth element is a BoolLogic condition — when true, this source is excluded.
 *
 * Multiple pairs can point to same target for multi-source computation.
 *
 * @example
 * const comps: ComputationPair<State>[] = [
 *   ['SUM', 'total', 'price1'],
 *   ['SUM', 'total', 'price2', { IS_EQUAL: ['price2.disabled', true] }],
 *   ['AVG', 'average', 'score1'],
 *   ['AVG', 'average', 'score2'],
 * ]
 */
export type ComputationPair<DATA extends object> = {
  [TARGET in DeepKeyFiltered<DATA, number>]:
    | [ComputationOp, TARGET, DeepKeyFiltered<DATA, number>]
    | [ComputationOp, TARGET, DeepKeyFiltered<DATA, number>, BoolLogic<DATA>]
}[DeepKeyFiltered<DATA, number>]
