/**
 * DeepKeyFiltered - Filters paths by their resolved value type
 *
 * Returns only paths from DeepKey<T> that resolve to a specific type U.
 * Useful for type-safe filtering of state paths by their value types.
 *
 * @example
 * ```typescript
 * type Data = {
 *   isActive: boolean
 *   name: string
 *   count: number
 *   user: { isAdmin: boolean }
 * }
 *
 * // Only boolean paths
 * type BoolPaths = DeepKeyFiltered<Data, boolean>
 * // Result: "isActive" | "user.isAdmin"
 *
 * // Only string paths
 * type StrPaths = DeepKeyFiltered<Data, string>
 * // Result: "name"
 * ```
 */

import type { DeepKey, DeepValue } from './index'

/**
 * Filters DeepKey<T> to only include paths that resolve to type U.
 *
 * Uses mapped type with conditional filtering - maps over all possible paths,
 * keeps those matching the target type, and extracts the union.
 */
export type DeepKeyFiltered<T, U> = {
  [K in DeepKey<T>]: DeepValue<T, K> extends U ? K : never
}[DeepKey<T>]
