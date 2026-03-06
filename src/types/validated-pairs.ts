/**
 * Validated pair branded types
 *
 * Phantom types for pre-validated pair arrays returned by helper functions
 * (syncPairs, flipPairs, aggregationPairs, computationPairs).
 *
 * Brands serve two purposes:
 * 1. SideEffects accepts them without re-resolving the O(N²) direct pair types
 * 2. DATA generic prevents cross-store use (pairs from StoreA can't go to StoreB)
 *
 * Zero runtime cost — brands are erased at compile time.
 */

import type { ListenerRegistration } from '../core/types'
import type { GenericMeta } from './meta'
import type { ComputationOp } from './paths-of-same-value'

export declare const VALIDATED: unique symbol
export declare const STORE_DATA: unique symbol

/** Pre-validated sync pair array. Returned by `syncPairs()`. Branded with DATA to prevent cross-store use. */
export type ValidatedSyncPairs<DATA extends object = object> = (
  | [string, string]
  | [string, string, { oneWay: '[0]->[1]' | '[1]->[0]' }]
)[] & { [VALIDATED]: 'sync'; [STORE_DATA]: DATA }

/** Pre-validated flip pair array. Returned by `flipPairs()`. Branded with DATA to prevent cross-store use. */
export type ValidatedFlipPairs<DATA extends object = object> = [
  string,
  string,
][] & { [VALIDATED]: 'flip'; [STORE_DATA]: DATA }

/** Pre-validated aggregation pair array. Returned by `aggregationPairs()`. Branded with DATA to prevent cross-store use. */
export type ValidatedAggregationPairs<DATA extends object = object> = (
  | [string, string]
  | [string, string, unknown]
)[] & { [VALIDATED]: 'aggregation'; [STORE_DATA]: DATA }

/** Pre-validated computation pair array. Returned by `computationPairs()`. Branded with DATA to prevent cross-store use. */
export type ValidatedComputationPairs<DATA extends object = object> = (
  | [ComputationOp, string, string]
  | [ComputationOp, string, string, unknown]
)[] & { [VALIDATED]: 'computation'; [STORE_DATA]: DATA }

/** Pre-validated listener array. Returned by `listeners()`. Branded with DATA to prevent cross-store use.
 * Uses intersection with ListenerRegistration[] so it's assignable to the listeners field
 * without needing a union — preserving contextual typing for inline fn parameters.
 */
export type ValidatedListeners<
  DATA extends object = object,
  META extends GenericMeta = GenericMeta,
> = ListenerRegistration<DATA, META>[] & {
  [VALIDATED]: 'listeners'
  [STORE_DATA]: DATA
}
