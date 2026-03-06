/**
 * Direct pair types — O(N²) path unions for side effects
 *
 * These types enumerate all valid path combinations eagerly.
 * Simple to use as type annotations but hit TS2589 at ~1,500 paths.
 *
 * For large state types, use the curried helper functions (syncPairs, flipPairs, etc.)
 * which use lazy validators from `pair-validators.ts` instead.
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
 * Format: [path1, path2] or [path1, path2, { oneWay: '[0]->[1]' | '[1]->[0]' }]
 *
 * `oneWay` makes sync unidirectional. The value declares the direction explicitly:
 * - `'[0]->[1]'` — first path pushes to second path (left → right)
 * - `'[1]->[0]'` — second path pushes to first path (right → left)
 *
 * Without the option, sync is bidirectional.
 *
 * **Scaling note**: This type is O(N²) where N = number of paths. It hits TS2589
 * at ~1,500 paths. For large state types, use the `syncPairs()` helper function
 * or `store.syncPairs()` (pre-warmed from `createGenericStore`) instead —
 * they scale to ~50K–80K paths.
 *
 * @example Bidirectional
 * const pair: SyncPair<State> = ['user.email', 'profile.email']
 *
 * @example One-way: user.email → profile.email only
 * const pair: SyncPair<State> = ['user.email', 'profile.email', { oneWay: '[0]->[1]' }]
 *
 * @example One-way: profile.email → user.email only (reversed direction)
 * const pair: SyncPair<State> = ['user.email', 'profile.email', { oneWay: '[1]->[0]' }]
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
  [P1 in ResolvableDeepKey<DATA, Depth>]:
    | [P1, PathsWithSameValueAs<DATA, P1, Depth>]
    | [
        P1,
        PathsWithSameValueAs<DATA, P1, Depth>,
        { oneWay: '[0]->[1]' | '[1]->[0]' },
      ]
}[ResolvableDeepKey<DATA, Depth>]

/**
 * A tuple of two paths for flip. Always bidirectional: path1 ↔ path2 (inverted values).
 * Format: [path1, path2]
 *
 * **Scaling note**: O(N²) — hits TS2589 at ~1,500 paths. Use `flipPairs()` helper
 * or `store.flipPairs()` for large state types.
 *
 * @example
 * const pair: FlipPair<State> = ['isActive', 'isInactive']
 */
export type FlipPair<
  DATA extends object,
  Depth extends number = DefaultDepth,
> = {
  [P1 in ResolvableDeepKey<DATA, Depth>]: [
    P1,
    PathsWithSameValueAs<DATA, P1, Depth>,
  ]
}[ResolvableDeepKey<DATA, Depth>]

/**
 * A tuple for aggregation: [target, source] or [target, source, excludeWhen]
 * First element (left) is ALWAYS the target (aggregated) path.
 * Second element is a source path.
 * Optional third element is a BoolLogic condition — when true, this source is excluded.
 *
 * Multiple pairs can point to same target for multi-source aggregation.
 *
 * **Scaling note**: O(N²) — hits TS2589 at ~1,500 paths. Use `aggregationPairs()` helper
 * or `store.aggregationPairs()` for large state types.
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
 * **Scaling note**: O(N²) — hits TS2589 at ~1,500 paths. Use `computationPairs()` helper
 * or `store.computationPairs()` for large state types.
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
