/**
 * DeepValue utility type
 *
 * Extracts the value type for a given dot-notation path string.
 * Handles nested objects, arrays, and optional properties.
 *
 * @example
 * ```typescript
 * type User = {
 *   address: {
 *     street: string
 *     city: string
 *   }
 * }
 *
 * // DeepValue<User, "address.street"> = string
 * // DeepValue<User, "address"> = { street: string, city: string }
 * ```
 */

import type { HASH_KEY } from './hashKey'

type IsAny<T> = 0 extends 1 & T ? true : false

type Primitive = string | number | boolean | bigint | symbol | null | undefined

/** Detects if T is a union type */
type IsUnion<T, U = T> = [T] extends [never]
  ? false
  : T extends unknown
    ? [U] extends [T]
      ? false
      : true
    : never

/**
 * Detects if T is a homogeneous record (enum-keyed).
 * Same heuristic as in DeepKey — must match to resolve values correctly.
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

// Main DeepValue implementation
export type DeepValue<T, Path extends string> =
  IsAny<T> extends true
    ? never
    : T extends readonly unknown[]
      ? T[number]
      : Path extends `${infer First}.${infer Rest}`
        ? First extends keyof T
          ? DeepValue<T[First], Rest>
          : string extends keyof T
            ? // First doesn't exist as concrete key, but T has string index (Record<string, V>)
              First extends HASH_KEY
              ? DeepValue<T[string], Rest>
              : unknown
            : // Check for homogeneous record (Record<Enum, V>) — keys are bracketed
              IsHomogeneousRecord<T> extends true
              ? First extends HASH_KEY
                ? DeepValue<T[keyof T & string], Rest>
                : unknown
              : unknown
        : Path extends HASH_KEY
          ? // Hash key path: resolve to Record value type
            string extends keyof T
            ? T[string]
            : IsHomogeneousRecord<T> extends true
              ? T[keyof T & string]
              : unknown
          : Path extends keyof T
            ? T[Path]
            : unknown
