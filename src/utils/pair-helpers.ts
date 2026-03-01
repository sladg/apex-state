/**
 * Lazy-validated pair helper functions
 *
 * These curried helpers provide O(N) base type + O(K) per-pair validation,
 * avoiding the O(N²) explosion of SyncPair/FlipPair/AggregationPair/ComputationPair
 * on large state types (1500+ paths).
 *
 * Runtime behavior: identity function (returns input as-is).
 * Type behavior: validates each pair via CheckSyncPairs / CheckAggregationPairs / CheckComputationPairs.
 *
 * **Why these exist**: The direct pair types (`SyncPair<T>`, `FlipPair<T>`, etc.) distribute
 * over all N paths to build an O(N²) union of valid pairs. This hits TS2589 at ~1,500 paths.
 * These helpers defer validation: the function constraint uses `ResolvableDeepKey<T>` (O(N))
 * for autocomplete, then `CheckPairValueMatch` validates only the K pairs you actually write
 * via `DeepValue` comparison (O(1) per pair).
 *
 * **Inference safety**: Parameter types use mapped types only (`{ [I in keyof T]: Check<T[I]> }`)
 * — never `[...T] & MappedType<T>`. This avoids the TS inference competition where `[...T]`
 * and a mapped type both try to infer T, causing tuple widening on small state types.
 *
 * **Branded returns**: Each helper returns a `Validated*` branded type so that
 * `useSideEffects({ syncPaths: result })` skips the O(N²) re-validation.
 *
 * **Scaling limits** (measured on Apple M4 Pro):
 * - ~82,500 paths (500 flat sectors, depth 4): PASS in 7.1s / 617 MB
 * - ~52,800 paths (binary 6 levels, depth 9): PASS in 4.7s / 574 MB
 * - ~105,600 paths (binary 7 levels, depth 10): TS2589
 * - Practical limit: ~50K–80K paths, bounded by `ResolvableDeepKey` union resolution
 *   at the function constraint, not by the validation logic.
 * - Old `SyncPair<T>` hit TS2589 at ~1,500 paths — a ~30–50x improvement.
 *
 * @example
 * ```typescript
 * const syncs = syncPairs<State>()([
 *   ['user.email', 'profile.email'],  // ✓ both string
 *   ['user.age', 'profile.name'],     // ✗ number vs string → type error
 * ])
 * ```
 */

import type {
  BoolLogic,
  CheckAggregationPairs,
  CheckComputationPairs,
  CheckListeners,
  CheckSyncPairs,
  ComputationOp,
  ValidatedAggregationPairs,
  ValidatedComputationPairs,
  ValidatedFlipPairs,
  ValidatedListeners,
  ValidatedSyncPairs,
} from '../types'
import type { DefaultDepth, ResolvableDeepKey } from '../types/deep-key'
import type { DeepKeyFiltered } from '../types/deep-key-filtered'
import type { GenericMeta } from '../types/meta'

/**
 * Lazy-validated sync pairs. Curried: `syncPairs<State>()(pairs)`.
 *
 * Provides autocomplete for valid paths (O(N)) and validates that both paths
 * in each pair resolve to the same value type (O(K) per pair).
 * Returns branded `ValidatedSyncPairs` — accepted by `useSideEffects` without re-validation.
 *
 * @example
 * ```typescript
 * const syncs = syncPairs<MyState>()([
 *   ['user.email', 'profile.email'],
 *   ['user.name', 'profile.name'],
 * ])
 * useSideEffects('my-syncs', { syncPaths: syncs })  // no O(N²) re-check
 * ```
 */
export const syncPairs =
  <DATA extends object, Depth extends number = DefaultDepth>() =>
  <
    T extends (
      | [ResolvableDeepKey<DATA, Depth>, ResolvableDeepKey<DATA, Depth>]
      | [
          ResolvableDeepKey<DATA, Depth>,
          ResolvableDeepKey<DATA, Depth>,
          { oneWay: '[0]->[1]' | '[1]->[0]' },
        ]
    )[],
  >(
    pairs: CheckSyncPairs<DATA, T, Depth>,
  ): ValidatedSyncPairs<DATA> =>
    pairs as ValidatedSyncPairs<DATA>

/**
 * Lazy-validated flip pairs. Curried: `flipPairs<State>()(pairs)`.
 *
 * Identical to syncPairs — validates both paths resolve to the same value type.
 * Returns branded `ValidatedFlipPairs` — accepted by `useSideEffects` without re-validation.
 *
 * @example
 * ```typescript
 * const flips = flipPairs<MyState>()([
 *   ['isActive', 'isInactive'],
 * ])
 * useSideEffects('my-flips', { flipPaths: flips })  // no O(N²) re-check
 * ```
 */
export const flipPairs =
  <DATA extends object, Depth extends number = DefaultDepth>() =>
  <
    T extends [
      ResolvableDeepKey<DATA, Depth>,
      ResolvableDeepKey<DATA, Depth>,
    ][],
  >(
    pairs: CheckSyncPairs<DATA, T, Depth>,
  ): ValidatedFlipPairs<DATA> =>
    pairs as ValidatedFlipPairs<DATA>

/**
 * Lazy-validated aggregation pairs. Curried: `aggregationPairs<State>()(pairs)`.
 *
 * Each pair is [target, source] or [target, source, BoolLogic].
 * Validates that target and source resolve to the same value type.
 * Returns branded `ValidatedAggregationPairs` — accepted by `useSideEffects` without re-validation.
 *
 * @example
 * ```typescript
 * const aggs = aggregationPairs<MyState>()([
 *   ['total', 'price1'],
 *   ['total', 'price2', { IS_EQUAL: ['price2.disabled', true] }],
 * ])
 * useSideEffects('my-aggs', { aggregations: aggs })  // no O(N²) re-check
 * ```
 */
export const aggregationPairs =
  <DATA extends object, Depth extends number = DefaultDepth>() =>
  <
    T extends (
      | [ResolvableDeepKey<DATA, Depth>, ResolvableDeepKey<DATA, Depth>]
      | [
          ResolvableDeepKey<DATA, Depth>,
          ResolvableDeepKey<DATA, Depth>,
          BoolLogic<DATA, Depth>,
        ]
    )[],
  >(
    pairs: CheckAggregationPairs<DATA, T, Depth>,
  ): ValidatedAggregationPairs<DATA> =>
    pairs as ValidatedAggregationPairs<DATA>

/**
 * Lazy-validated computation pairs. Curried: `computationPairs<State>()(pairs)`.
 *
 * Each pair is [op, target, source] or [op, target, source, BoolLogic].
 * Validates that both target and source are number paths.
 * Returns branded `ValidatedComputationPairs` — accepted by `useSideEffects` without re-validation.
 *
 * @example
 * ```typescript
 * const comps = computationPairs<MyState>()([
 *   ['SUM', 'total', 'price1'],
 *   ['AVG', 'average', 'score1'],
 * ])
 * useSideEffects('my-comps', { computations: comps })  // no O(N²) re-check
 * ```
 */
export const computationPairs =
  <DATA extends object, Depth extends number = DefaultDepth>() =>
  <
    T extends (
      | [
          ComputationOp,
          DeepKeyFiltered<DATA, number, Depth>,
          DeepKeyFiltered<DATA, number, Depth>,
        ]
      | [
          ComputationOp,
          DeepKeyFiltered<DATA, number, Depth>,
          DeepKeyFiltered<DATA, number, Depth>,
          BoolLogic<DATA, Depth>,
        ]
    )[],
  >(
    pairs: CheckComputationPairs<DATA, T, Depth>,
  ): ValidatedComputationPairs<DATA> =>
    pairs as ValidatedComputationPairs<DATA>

/**
 * Lazy-validated listeners. Curried: `listeners<State>()(items)`.
 *
 * Provides autocomplete for valid paths (O(N)) and validates each listener:
 * - path and scope are valid `ResolvableDeepKey<DATA>` or null
 * - When both non-null: scope must be a dot-separated prefix of path
 * - fn receives correctly-typed scoped state based on scope
 *
 * Returns branded `ValidatedListeners` — accepted by `useSideEffects` without re-validation.
 *
 * @example
 * ```typescript
 * const myListeners = listeners<MyState>()([
 *   {
 *     path: 'user.profile.name',
 *     scope: 'user.profile',
 *     fn: (changes, state) => {
 *       // changes: ArrayOfChanges relative to user.profile scope
 *       // state: typed as MyState['user']['profile']
 *       // return: ArrayOfChanges relative to scope, or undefined
 *       return [['name', `updated-${state.name}`, {}]]
 *     }
 *   },
 * ])
 * useSideEffects('my-listeners', { listeners: myListeners })
 * ```
 */
export const listeners =
  <
    DATA extends object,
    META extends GenericMeta = GenericMeta,
    Depth extends number = DefaultDepth,
  >() =>
  <
    T extends readonly {
      path: ResolvableDeepKey<DATA, Depth> | null
      scope?: ResolvableDeepKey<DATA, Depth> | null | undefined
      fn: (...args: any[]) => any
    }[],
  >(
    items: CheckListeners<DATA, META, T, Depth>,
  ): ValidatedListeners<DATA, META> =>
    items as ValidatedListeners<DATA, META>
