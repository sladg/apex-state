/**
 * DeepKey utility type
 *
 * Generates a union of all possible dot-notation paths for nested objects.
 * Supports nested objects up to depth 20 to handle deeply nested data structures.
 * Handles arrays and complex object hierarchies.
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
 */

import type { HASH_KEY } from './hashKey'

type Primitive = string | number | boolean | bigint | symbol | null | undefined

type IsAny<T> = 0 extends 1 & T ? true : false

// Main DeepKey implementation with depth limit to prevent infinite recursion
export type DeepKey<T, Depth extends number = 20> = Depth extends 0
  ? never
  : IsAny<T> extends true
    ? never
    : T extends Primitive
      ? never
      : T extends readonly any[]
        ? never
        : string extends keyof T
          ? // T is a Record<string, V> — emit HASH_KEY and recurse into T[string]
              | HASH_KEY
              | (T[string] extends Primitive
                  ? never
                  : T[string] extends readonly any[]
                    ? never
                    : DeepKey<T[string], Prev<Depth>> extends infer DK
                      ? DK extends string
                        ? `${HASH_KEY}.${DK}`
                        : never
                      : never)
          : // T has concrete keys only
            {
              [K in keyof T & (string | number)]:
                | `${K & string}`
                | (T[K] extends Primitive
                    ? never
                    : T[K] extends readonly any[]
                      ? never
                      : DeepKey<T[K], Prev<Depth>> extends infer DK
                        ? DK extends string
                          ? `${K & string}.${DK}`
                          : never
                        : never)
            }[keyof T & (string | number)]

// Depth counter helper types (supports up to depth 20)
type Prev<N extends number> = N extends 20
  ? 19
  : N extends 19
    ? 18
    : N extends 18
      ? 17
      : N extends 17
        ? 16
        : N extends 16
          ? 15
          : N extends 15
            ? 14
            : N extends 14
              ? 13
              : N extends 13
                ? 12
                : N extends 12
                  ? 11
                  : N extends 11
                    ? 10
                    : N extends 10
                      ? 9
                      : N extends 9
                        ? 8
                        : N extends 8
                          ? 7
                          : N extends 7
                            ? 6
                            : N extends 6
                              ? 5
                              : N extends 5
                                ? 4
                                : N extends 4
                                  ? 3
                                  : N extends 3
                                    ? 2
                                    : N extends 2
                                      ? 1
                                      : N extends 1
                                        ? 0
                                        : never
