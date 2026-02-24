/**
 * Lazy pair validators — O(N) base + O(K) per-pair DeepValue check
 *
 * These types power the helper functions (syncPairs, flipPairs, etc.)
 * that scale to large state types without hitting TS2589.
 *
 * Unlike the direct pair types (SyncPair, FlipPair, etc.) which are O(N²),
 * these validators only evaluate DeepValue for the K pairs you actually write.
 */

import type { BoolLogic } from './bool-logic'
import type { DefaultDepth, ResolvableDeepKey } from './deep-key'
import type { DeepKeyFiltered } from './deep-key-filtered'
import type { DeepValue } from './deep-value'
import type { ComputationOp } from './paths-of-same-value'

/**
 * Validates that two paths resolve to the same value type.
 * Only evaluates DeepValue when P1, P2 are concrete literals — O(1) per pair.
 * Wraps in [T] extends [U] to prevent distributive conditional.
 *
 * Returns [P1, P2] on match, [P1, never] on mismatch.
 */
export type CheckPairValueMatch<
  DATA extends object,
  P1 extends string,
  P2 extends string,
  Depth extends number = DefaultDepth,
> = [P1] extends [ResolvableDeepKey<DATA, Depth>]
  ? [P2] extends [ResolvableDeepKey<DATA, Depth>]
    ? [NonNullable<DeepValue<DATA, P1>>] extends [
        NonNullable<DeepValue<DATA, P2>>,
      ]
      ? [NonNullable<DeepValue<DATA, P2>>] extends [
          NonNullable<DeepValue<DATA, P1>>,
        ]
        ? [P1, P2]
        : [P1, never]
      : [P1, never]
    : never
  : never

/**
 * Validates an array of [P1, P2] sync/flip pairs lazily — O(K) where K = pairs written.
 */
export type CheckSyncPairs<
  DATA extends object,
  T extends readonly [string, string][],
  Depth extends number = DefaultDepth,
> = {
  [I in keyof T]: T[I] extends [
    infer P1 extends string,
    infer P2 extends string,
  ]
    ? CheckPairValueMatch<DATA, P1, P2, Depth>
    : T[I]
}

/**
 * Validates that two paths both resolve to number.
 * Returns [ComputationOp, P1, P2] or [ComputationOp, P1, P2, BoolLogic] on match.
 */
export type CheckComputationPairValueMatch<
  DATA extends object,
  P1 extends string,
  P2 extends string,
  Depth extends number = DefaultDepth,
> = [P1] extends [DeepKeyFiltered<DATA, number, Depth>]
  ? [P2] extends [DeepKeyFiltered<DATA, number, Depth>]
    ? true
    : false
  : false

/**
 * Validates an array of aggregation pairs: [target, source] or [target, source, BoolLogic].
 * Each pair checks that target and source resolve to the same value type.
 */
export type CheckAggregationPairs<
  DATA extends object,
  T extends readonly (
    | readonly [string, string]
    | readonly [string, string, BoolLogic<DATA, Depth>]
  )[],
  Depth extends number = DefaultDepth,
> = {
  [I in keyof T]: T[I] extends readonly [
    infer P1 extends string,
    infer P2 extends string,
    infer BL,
  ]
    ? CheckPairValueMatch<DATA, P1, P2, Depth> extends [P1, P2]
      ? [P1, P2, BL]
      : [P1, never, BL]
    : T[I] extends readonly [infer P1 extends string, infer P2 extends string]
      ? CheckPairValueMatch<DATA, P1, P2, Depth>
      : T[I]
}

/**
 * Validates an array of computation pairs: [op, target, source] or [op, target, source, BoolLogic].
 * Both target and source must be number paths.
 */
export type CheckComputationPairs<
  DATA extends object,
  T extends readonly (
    | readonly [ComputationOp, string, string]
    | readonly [ComputationOp, string, string, BoolLogic<DATA, Depth>]
  )[],
  Depth extends number = DefaultDepth,
> = {
  [I in keyof T]: T[I] extends readonly [
    infer Op extends ComputationOp,
    infer P1 extends string,
    infer P2 extends string,
    infer BL,
  ]
    ? CheckComputationPairValueMatch<DATA, P1, P2, Depth> extends true
      ? [Op, P1, P2, BL]
      : [Op, P1, never, BL]
    : T[I] extends readonly [
          infer Op extends ComputationOp,
          infer P1 extends string,
          infer P2 extends string,
        ]
      ? CheckComputationPairValueMatch<DATA, P1, P2, Depth> extends true
        ? [Op, P1, P2]
        : [Op, P1, never]
      : T[I]
}
