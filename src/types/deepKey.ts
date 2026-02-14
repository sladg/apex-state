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

type Primitive = string | number | boolean | bigint | symbol | null | undefined

type IsAny<T> = 0 extends 1 & T ? true : false

/** Detects if T is a union type (distributes, returns true if T has 2+ members) */
type IsUnion<T, U = T> = [T] extends [never]
  ? false
  : T extends unknown
    ? [U] extends [T]
      ? false
      : true
    : never

/**
 * Detects if T is a homogeneous record (enum-keyed) that should use `[${string}]` patterns.
 *
 * Returns true when:
 * 1. Not already Record<string, V> (handled by `string extends keyof T` branch)
 * 2. Has multiple string keys (single-key objects are structural)
 * 3. All values are the SAME type (inferred V is not a union)
 * 4. Value type is non-primitive and non-array
 */
type IsHomogeneousRecord<T> =
  IsAny<T> extends true
    ? false
    : string extends keyof T
      ? false
      : keyof T extends string
        ? true extends IsUnion<keyof T & string>
          ? T extends Record<keyof T, infer V>
            ? true extends IsUnion<V>
              ? false
              : [V] extends [Primitive]
                ? false
                : [V] extends [readonly unknown[]]
                  ? false
                  : true
            : false
          : false
        : false

/**
 * Main DeepKey implementation with depth limit to prevent infinite recursion.
 *
 * 3 branches:
 * 1. `string extends keyof T`    → Record<string, V>  → `[${string}]` patterns
 * 2. `IsHomogeneousRecord<T>`    → Record<Enum, V>    → `[${string}]` patterns (collapsed)
 * 3. else                        → structural object   → bare keys
 */
export type DeepKey<T, Depth extends number = 20> = Depth extends 0
  ? never
  : IsAny<T> extends true
    ? never
    : T extends Primitive
      ? never
      : T extends readonly unknown[]
        ? never
        : string extends keyof T
          ? // T is a Record<string, V> — emit [${string}] and recurse into T[string]
              | `[${string}]`
              | (T[string] extends Primitive
                  ? never
                  : T[string] extends readonly unknown[]
                    ? never
                    : DeepKey<T[string], Prev<Depth>> extends infer DK
                      ? DK extends string
                        ? `[${string}].${DK}`
                        : never
                      : never)
          : IsHomogeneousRecord<T> extends true
            ? // T is Record<Enum, V> — collapse to [${string}] (prevents union explosion)
                | `[${string}]`
                | (T[keyof T & string] extends Primitive
                    ? never
                    : T[keyof T & string] extends readonly unknown[]
                      ? never
                      : DeepKey<
                            T[keyof T & string],
                            Prev<Depth>
                          > extends infer DK
                        ? DK extends string
                          ? `[${string}].${DK}`
                          : never
                        : never)
            : // T has concrete keys — emit bare keys
              {
                [K in keyof T & (string | number)]:
                  | `${K & string}`
                  | (T[K] extends Primitive
                      ? never
                      : T[K] extends readonly unknown[]
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
