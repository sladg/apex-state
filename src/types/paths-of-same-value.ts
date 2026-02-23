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
import type { DefaultDepth, ResolvableDeepKey } from './deep-key'
import type { DeepKeyFiltered } from './deep-key-filtered'
import type { DeepValue } from './deep-value'

/**
 * Get all paths in DATA that have the same value type as the value at PATH
 */
export type PathsWithSameValueAs<
  DATA extends object,
  PATH extends ResolvableDeepKey<DATA, Depth>,
  Depth extends number = DefaultDepth,
> = {
  [K in ResolvableDeepKey<DATA, Depth>]: NonNullable<
    DeepValue<DATA, K>
  > extends NonNullable<DeepValue<DATA, PATH>>
    ? NonNullable<DeepValue<DATA, PATH>> extends NonNullable<DeepValue<DATA, K>>
      ? K
      : never
    : never
}[ResolvableDeepKey<DATA, Depth>]

/**
 * A tuple of two paths that must have the same value type.
 * Format: [path1, path2]
 *
 * @example
 * const pair: SyncPair<State> = ['user.email', 'profile.email']
 *
 * @example Custom depth — propagates to DeepKey and PathsWithSameValueAs
 * ```typescript
 * // Limit path traversal to 10 levels
 * const shallow: SyncPair<State, 10> = ['user.email', 'profile.email']
 * ```
 */
export type SyncPair<
  DATA extends object,
  Depth extends number = DefaultDepth,
> = {
  [P1 in ResolvableDeepKey<DATA, Depth>]: [
    P1,
    PathsWithSameValueAs<DATA, P1, Depth>,
  ]
}[ResolvableDeepKey<DATA, Depth>]

/**
 * A tuple of two paths for flip (alias for SyncPair)
 * Format: [path1, path2]
 *
 * @example
 * const pair: FlipPair<State> = ['isActive', 'isInactive']
 */
export type FlipPair<
  DATA extends object,
  Depth extends number = DefaultDepth,
> = SyncPair<DATA, Depth>

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
export type AggregationPair<
  DATA extends object,
  Depth extends number = DefaultDepth,
> = {
  [P1 in ResolvableDeepKey<DATA, Depth>]:
    | [P1, PathsWithSameValueAs<DATA, P1, Depth>]
    | [P1, PathsWithSameValueAs<DATA, P1, Depth>, BoolLogic<DATA, Depth>]
}[ResolvableDeepKey<DATA, Depth>]

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
export type ComputationPair<
  DATA extends object,
  Depth extends number = DefaultDepth,
> = {
  [TARGET in DeepKeyFiltered<DATA, number, Depth>]:
    | [ComputationOp, TARGET, DeepKeyFiltered<DATA, number, Depth>]
    | [
        ComputationOp,
        TARGET,
        DeepKeyFiltered<DATA, number, Depth>,
        BoolLogic<DATA, Depth>,
      ]
}[DeepKeyFiltered<DATA, number, Depth>]
