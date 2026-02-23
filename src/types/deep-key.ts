/**
 * DeepKey utility type
 *
 * Generates a union of all possible dot-notation paths for nested objects.
 * Supports nested objects up to depth 20 to handle deeply nested data structures.
 * Handles arrays and complex object hierarchies.
 *
 * Uses loop unrolling: processes 2 nesting levels per recursion call,
 * halving recursion depth while preserving bottom-up evaluation that
 * TypeScript can cache (no prefix parameter).
 *
 * Examples of supported paths:
 * - Simple: "name", "email"
 * - Nested: "user.address.street"
 * - Deep: "g.p.data.optionsCommon.base.ccyPair.forCurrency.id" (11 levels)
 *
 * @example
 * ```typescript
 * type User = {
 *   name: string
 *   address: {
 *     street: string
 *     city: string
 *   }
 * }
 *
 * // DeepKey<User> = "name" | "address" | "address.street" | "address.city"
 * ```
 *
 * @example Custom depth — reduce for faster compilation on wide types
 * ```typescript
 * // Only traverse 10 levels deep instead of the default 20
 * type ShallowPaths = DeepKey<User, 10>
 * ```
 *
 * @example Depth limit marker — `??` appears at the cutoff point
 * ```typescript
 * // If a type is nested deeper than Depth, autocomplete shows:
 * // "some.deep.path.??" — signaling you should increase depth or restructure
 * type Paths = DeepKey<VeryDeepType, 6>
 * ```
 */

import type { HASH_KEY } from './hash-key'

type Primitive = string | number | boolean | bigint | symbol | null | undefined

type IsAny<T> = 0 extends 1 & T ? true : false

/**
 * Default recursion depth for DeepKey and all types that depend on it.
 * Change this single value to adjust the depth limit across the entire type system.
 *
 * Propagates to: SyncPair, FlipPair, AggregationPair, ComputationPair,
 * BoolLogic, SideEffects, DeepKeyFiltered, ClearPathRule.
 *
 * @example Override per-call instead of changing the default
 * ```typescript
 * type Shallow = DeepKey<MyState, 10>    // reduced depth
 * type Deeper = DeepKey<MyState, 30>     // increased depth
 * ```
 */
export type DefaultDepth = 20

/**
 * Marker emitted when DeepKey reaches its depth limit on an object type,
 * or when it encounters an `any`-typed property (unknowable structure).
 * Appears as `some.deep.path.??` in autocomplete, signaling the cutoff point.
 * Users seeing this can increase depth: `DeepKey<T, 30>` or restructure their type.
 */
type DepthLimitMarker = '??'

// Public API — keeps the same signature, delegates to loop-unrolled impl
export type DeepKey<T, Depth extends number = DefaultDepth> = DeepKeyUnrolled<
  T,
  Depth
> &
  string

/**
 * DeepKey with `??` marker paths excluded. Use in types that resolve path values
 * (DeepValue, DeepKeyFiltered, PathsWithSameValueAs) where `??` would fail resolution.
 * DeepKey itself keeps `??` for IDE autocomplete visibility.
 */
export type ResolvableDeepKey<T, Depth extends number = DefaultDepth> = Exclude<
  DeepKey<T, Depth>,
  `${string}??` | '??'
>

/**
 * Loop-unrolled DeepKey: processes 2 nesting levels per recursion call.
 *
 * For each key K of T:
 *   - Emit K (level 1 path)
 *   - If T[K] is an object, inline level 2:
 *     - For each key K2 of T[K]:
 *       - Emit `${K}.${K2}` (level 2 path)
 *       - If T[K][K2] is an object, recurse with `${K}.${K2}.${DeepKeyUnrolled}`
 *
 * Key insight: DeepKeyUnrolled<T, Depth> has NO prefix parameter,
 * so identical sub-types at the same depth are cached by TypeScript.
 * Template literal distribution (`${prefix}.${union}`) is handled natively
 * by TypeScript without extra type instantiation overhead.
 */
type DeepKeyUnrolled<T, Depth extends number> = Depth extends 0
  ? // At depth limit: if T is still an object, emit marker so callers
    // produce paths like `some.deep.path.??` — showing the exact cutoff.
    T extends Primitive | readonly any[]
    ? never
    : DepthLimitMarker
  : IsAny<T> extends true
    ? never
    : T extends Primitive
      ? never
      : T extends readonly any[]
        ? never
        : string extends keyof T
          ? // T is Record<string, V> — level 1: emit HASH_KEY
              | HASH_KEY
              | (IsAny<T[string]> extends true
                  ? `${HASH_KEY}.${DepthLimitMarker}`
                  : NonNullable<T[string]> extends Primitive | readonly any[]
                    ? never
                    : RecordLevel2<NonNullable<T[string]>, HASH_KEY, Depth>)
          : // T has concrete keys — level 1: enumerate keys
            {
              [K in keyof T & (string | number)]:
                | (K & string)
                | (IsAny<T[K]> extends true
                    ? `${K & string}.${DepthLimitMarker}`
                    : NonNullable<T[K]> extends Primitive | readonly any[]
                      ? never
                      : ConcreteLevel2<NonNullable<T[K]>, K & string, Depth>)
            }[keyof T & (string | number)]

/**
 * Level 2 expansion for concrete-key objects.
 * Enumerates V's keys and uses inline template literal distribution for level 3+.
 */
type ConcreteLevel2<V, ParentKey extends string, Depth extends number> =
  IsAny<V> extends true
    ? `${ParentKey}.${DepthLimitMarker}`
    : V extends Primitive
      ? never
      : V extends readonly any[]
        ? never
        : string extends keyof V
          ? // V is Record<string, W> at level 2
              | `${ParentKey}.${HASH_KEY}`
              | (IsAny<V[string]> extends true
                  ? `${ParentKey}.${HASH_KEY}.${DepthLimitMarker}`
                  : NonNullable<V[string]> extends Primitive | readonly any[]
                    ? never
                    : `${ParentKey}.${HASH_KEY}.${DeepKeyUnrolled<NonNullable<V[string]>, Prev2<Depth>> & string}`)
          : // V has concrete keys at level 2
            {
              [K2 in keyof V & (string | number)]:
                | `${ParentKey}.${K2 & string}`
                | (IsAny<V[K2]> extends true
                    ? `${ParentKey}.${K2 & string}.${DepthLimitMarker}`
                    : NonNullable<V[K2]> extends Primitive | readonly any[]
                      ? never
                      : `${ParentKey}.${K2 & string}.${DeepKeyUnrolled<NonNullable<V[K2]>, Prev2<Depth>> & string}`)
            }[keyof V & (string | number)]

/**
 * Level 2 expansion for Record-value objects.
 * When level 1 was a Record, its value type V is expanded at level 2.
 */
type RecordLevel2<V, ParentKey extends string, Depth extends number> =
  IsAny<V> extends true
    ? `${ParentKey}.${DepthLimitMarker}`
    : V extends Primitive
      ? never
      : V extends readonly any[]
        ? never
        : string extends keyof V
          ? // V is also a Record at level 2
              | `${ParentKey}.${HASH_KEY}`
              | (IsAny<V[string]> extends true
                  ? `${ParentKey}.${HASH_KEY}.${DepthLimitMarker}`
                  : NonNullable<V[string]> extends Primitive | readonly any[]
                    ? never
                    : `${ParentKey}.${HASH_KEY}.${DeepKeyUnrolled<NonNullable<V[string]>, Prev2<Depth>> & string}`)
          : // V has concrete keys at level 2
            {
              [K2 in keyof V & (string | number)]:
                | `${ParentKey}.${K2 & string}`
                | (IsAny<V[K2]> extends true
                    ? `${ParentKey}.${K2 & string}.${DepthLimitMarker}`
                    : NonNullable<V[K2]> extends Primitive | readonly any[]
                      ? never
                      : `${ParentKey}.${K2 & string}.${DeepKeyUnrolled<NonNullable<V[K2]>, Prev2<Depth>> & string}`)
            }[keyof V & (string | number)]

// Depth counter: decrement by 2, handling odd depths
type Prev2<N extends number> = N extends 20
  ? 18
  : N extends 19
    ? 17
    : N extends 18
      ? 16
      : N extends 17
        ? 15
        : N extends 16
          ? 14
          : N extends 15
            ? 13
            : N extends 14
              ? 12
              : N extends 13
                ? 11
                : N extends 12
                  ? 10
                  : N extends 11
                    ? 9
                    : N extends 10
                      ? 8
                      : N extends 9
                        ? 7
                        : N extends 8
                          ? 6
                          : N extends 7
                            ? 5
                            : N extends 6
                              ? 4
                              : N extends 5
                                ? 3
                                : N extends 4
                                  ? 2
                                  : N extends 3
                                    ? 1
                                    : N extends 2
                                      ? 0
                                      : N extends 1
                                        ? 0
                                        : never
