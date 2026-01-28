/**
 * Type-safe string interpolation with {{path}} placeholders
 *
 * Extracts paths from template strings and validates they exist in DATA.
 *
 * @example
 * ```typescript
 * type State = { user: { name: string }; order: { total: number } }
 *
 * // ✓ Valid: paths exist in State
 * type Valid = ValidatedTemplate<"Hello {{user.name}}, total: {{order.total}}", State>
 *
 * // ✗ Error: 'user.invalid' doesn't exist
 * type Invalid = ValidatedTemplate<"Hello {{user.invalid}}", State>
 * ```
 */

import type { DeepKey } from './deepKey'

/**
 * Helper: Extract path from first {{path}} in string
 */
type ExtractFirst<S extends string> =
  S extends `${string}{{${infer Path}}}${string}` ? Path : never

/**
 * Helper: Get remainder after first {{path}}
 */
type AfterFirst<S extends string> =
  S extends `${string}{{${string}}}${infer Rest}` ? Rest : never

/**
 * Extract all {{path}} placeholders from a template string
 *
 * @example
 * type Paths = ExtractPlaceholders<"Hello {{user.name}}, email: {{user.email}}">
 * // Result: "user.name" | "user.email"
 */
export type ExtractPlaceholders<S extends string> =
  ExtractFirst<S> extends never
    ? never
    : ExtractFirst<S> | ExtractPlaceholders<AfterFirst<S>>

/**
 * Validate that all {{path}} placeholders in a template are valid DeepKey<DATA>
 *
 * Returns the template string if valid, never if invalid.
 *
 * @example
 * type State = { user: { name: string } }
 *
 * // Returns "Hello {{user.name}}"
 * type Valid = ValidatedTemplate<"Hello {{user.name}}", State>
 *
 * // Returns never (invalid path)
 * type Invalid = ValidatedTemplate<"Hello {{invalid.path}}", State>
 */
export type ValidatedTemplate<
  T extends string,
  DATA extends object
> = ExtractPlaceholders<T> extends never
  ? T // No placeholders, always valid
  : ExtractPlaceholders<T> extends DeepKey<DATA>
    ? T
    : never
