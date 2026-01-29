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

type IsAny<T> = 0 extends 1 & T ? true : false

// Main DeepValue implementation
export type DeepValue<T, Path extends string> =
  IsAny<T> extends true
    ? any
    : T extends readonly any[]
      ? T[number]
      : Path extends keyof T
        ? T[Path]
        : Path extends `${infer First}.${infer Rest}`
          ? First extends keyof T
            ? DeepValue<T[First], Rest>
            : unknown
          : unknown
