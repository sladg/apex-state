/**
 * Concern type utilities
 *
 * Generic type helpers for working with concern arrays and extracting
 * their return types for proper type-safe concern registration and reading.
 */

/**
 * Extract the return type from a concern's evaluate function
 *
 * Used internally to determine what a concern evaluates to,
 * enabling type-safe concern result objects.
 *
 * @example
 * ```typescript
 * type MyConcern = { evaluate: (...args: any[]) => boolean }
 * type Result = ExtractEvaluateReturn<MyConcern>  // boolean
 * ```
 */
export type ExtractEvaluateReturn<T> = T extends {
  evaluate: (...args: any[]) => infer R
}
  ? R
  : never

/**
 * Dynamically build an evaluated concerns object from a CONCERNS array
 *
 * Maps each concern's name to its return type, creating a properly typed
 * object that represents all concerns that can be registered/evaluated.
 *
 * @example
 * ```typescript
 * const concerns = [
 *   { name: 'validationState', evaluate: () => ({ isError: boolean, errors: [] }) },
 *   { name: 'tooltip', evaluate: () => string }
 * ] as const
 *
 * type Evaluated = EvaluatedConcerns<typeof concerns>
 * // { validationState?: { isError: boolean, errors: [] }, tooltip?: string }
 * ```
 */
export type EvaluatedConcerns<CONCERNS extends readonly any[]> = {
  [K in CONCERNS[number] as K['name']]?: ExtractEvaluateReturn<K>
}
