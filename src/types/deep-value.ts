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

import type { HASH_KEY } from './hash-key'

type IsAny<T> = 0 extends 1 & T ? true : false

// Main DeepValue implementation
export type DeepValue<T, Path extends string> =
  IsAny<T> extends true
    ? never
    : T extends readonly any[]
      ? T[number]
      : Path extends `${infer First}.${infer Rest}`
        ? First extends keyof T
          ? DeepValue<T[First], Rest>
          : string extends keyof T
            ? // First doesn't exist as concrete key, but T has string index (Record)
              First extends HASH_KEY
              ? DeepValue<T[string], Rest>
              : unknown
            : unknown
        : Path extends HASH_KEY
          ? // Hash key path: resolve to Record value type
            string extends keyof T
            ? T[string]
            : unknown
          : Path extends keyof T
            ? T[Path]
            : unknown
