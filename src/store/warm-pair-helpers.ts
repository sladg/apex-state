/**
 * Pre-warmed pair helpers — DATA type already bound
 *
 * Returned by `createGenericStore()` so users don't need to repeat `<MyState>`
 * at every call site. Each helper is the second (inner) function of the curried
 * pair validator, with DATA already applied.
 *
 * @example Basic usage
 * ```typescript
 * const { syncPairs, flipPairs, useSideEffects } = createGenericStore<MyState>()
 *
 * // No <MyState> needed — already bound
 * const syncs = syncPairs([['user.email', 'profile.email']])
 * useSideEffects('my-syncs', { syncPaths: syncs })
 * ```
 *
 * @example All pairs in one call (preferred)
 * ```typescript
 * const { syncPairs, useSideEffects } = createGenericStore<MyState>()
 *
 * // Put all pairs in a single call — validation is O(K) per pair
 * const syncs = syncPairs([
 *   ['user.email', 'profile.email'],
 *   ['user.name', 'profile.name'],
 * ])
 *
 * useSideEffects('syncs', { syncPaths: syncs })
 * ```
 */

import type { GenericMeta } from '../types'
import {
  aggregationPairs,
  computationPairs,
  flipPairs,
  syncPairs,
} from '../utils/pair-helpers'

/**
 * Creates pre-warmed pair helpers with DATA type already bound.
 * Called once by createGenericStore, returned as part of the store API.
 */
export const createWarmPairHelpers = <
  DATA extends object,
  _META extends GenericMeta = GenericMeta,
>() => ({
  /**
   * Pre-warmed sync pair validator — DATA type is already bound.
   * Validates that both paths in each pair resolve to the same value type.
   * Pass the result to `useSideEffects` as `syncPaths`.
   *
   * @example
   * ```typescript
   * const { useSideEffects, syncPairs } = createGenericStore<MyState>()
   *
   * // Type-safe — no need to repeat <MyState>
   * const syncs = syncPairs([
   *   ['user.email', 'profile.email'],   // ✓ both string
   *   ['user.age', 'profile.age'],       // ✓ both number
   * ])
   *
   * // Inside a component:
   * useSideEffects('my-syncs', { syncPaths: syncs })
   * ```
   */
  syncPairs: syncPairs<DATA>(),

  /**
   * Pre-warmed flip pair validator — DATA type is already bound.
   * Validates that both paths in each pair resolve to the same value type.
   * Used for inverted boolean synchronization. Pass the result to `useSideEffects` as `flipPaths`.
   *
   * @example
   * ```typescript
   * const { useSideEffects, flipPairs } = createGenericStore<MyState>()
   *
   * const flips = flipPairs([
   *   ['isExpanded', 'isCollapsed'],  // ✓ both boolean
   * ])
   *
   * // Inside a component:
   * useSideEffects('my-flips', { flipPaths: flips })
   * ```
   */
  flipPairs: flipPairs<DATA>(),

  /**
   * Pre-warmed aggregation pair validator — DATA type is already bound.
   * Each pair is `[target, source]` or `[target, source, BoolLogic]`.
   * Validates that target and source resolve to the same value type.
   * Pass the result to `useSideEffects` as `aggregations`.
   *
   * @example
   * ```typescript
   * const { useSideEffects, aggregationPairs } = createGenericStore<MyState>()
   *
   * const aggs = aggregationPairs([
   *   ['total', 'price1'],
   *   ['total', 'price2', { IS_EQUAL: ['price2.disabled', true] }],
   * ])
   *
   * // Inside a component:
   * useSideEffects('my-aggs', { aggregations: aggs })
   * ```
   */
  aggregationPairs: aggregationPairs<DATA>(),

  /**
   * Pre-warmed computation pair validator — DATA type is already bound.
   * Each pair is `[op, target, source]` or `[op, target, source, BoolLogic]`.
   * Both target and source must be number paths.
   * Pass the result to `useSideEffects` as `computations`.
   *
   * @example
   * ```typescript
   * const { useSideEffects, computationPairs } = createGenericStore<MyState>()
   *
   * const comps = computationPairs([
   *   ['SUM', 'total', 'price1'],
   *   ['AVG', 'average', 'score1'],
   * ])
   *
   * // Inside a component:
   * useSideEffects('my-comps', { computations: comps })
   * ```
   */
  computationPairs: computationPairs<DATA>(),
})
