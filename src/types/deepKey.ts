/**
 * DeepKey utility type
 *
 * Generates a union of all possible dot-notation paths for nested objects.
 * Supports nested objects up to a reasonable depth and handles arrays.
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

// Helper to check if type is never
type IsNever<T> = [T] extends [never] ? true : false

// Helper to get array element type
type ArrayElement<T> = T extends readonly (infer E)[] ? E : never

// Exclude array methods and numeric keys
type ExcludeArrayKeys<T> = T extends readonly any[] ? never : T

// Main DeepKey implementation with depth limit to prevent infinite recursion
export type DeepKey<T, Depth extends number = 5> = Depth extends 0
  ? never
  : IsAny<T> extends true
  ? string
  : T extends Primitive
  ? never
  : T extends readonly any[]
  ? never
  : {
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

// Depth counter helper types
type Prev<N extends number> = N extends 5
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
