/**
 * Concern type utilities
 *
 * Generic type helpers for working with concern arrays and extracting
 * their return types for proper type-safe concern registration and reading.
 */

import { z } from 'zod'

import type { BoolLogic } from './bool-logic'
import type { DeepKey } from './deep-key'
import type { DeepValue } from './deep-value'

/**
 * Config type for the validationState concern at a specific path.
 *
 * Defined here (not in concerns/prebuilts/validation-state.ts) to avoid
 * circular imports when used in ConcernRegistrationMap.
 *
 * Can be a schema for the path's own value type, or a scoped schema
 * targeting a different path in the same state.
 */
export type ValidationStateInput<DATA, PATH extends DeepKey<DATA>> =
  | { schema: z.ZodSchema<DeepValue<DATA, PATH>> }
  | {
      [SCOPE in DeepKey<DATA>]: {
        scope: SCOPE
        schema: z.ZodSchema<DeepValue<DATA, SCOPE>>
      }
    }[DeepKey<DATA>]

/**
 * Get the appropriate registration config type for a concern at a given path.
 *
 * - validationState: schema must match DeepValue<DATA, PATH> — path-dependent
 * - BoolLogic-based concerns: boolLogic paths are typed against DATA
 * - Template-based concerns: fixed { template: string }
 * - Custom concerns: fall back to Record<string, unknown>
 */
type ConcernConfigFor<
  C,
  DATA extends object,
  PATH extends DeepKey<DATA>,
> = C extends { name: 'validationState' }
  ? ValidationStateInput<DATA, PATH>
  : C extends { evaluate: (props: infer P) => any }
    ? P extends { boolLogic: any }
      ? { boolLogic: BoolLogic<DATA> }
      : Omit<P, 'state' | 'path' | 'value'>
    : Record<string, unknown>

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

/**
 * Maps field paths to per-concern config objects.
 *
 * When CONCERNS is provided (e.g., from createGenericStore's CONCERNS generic),
 * each concern config is type-checked against:
 * - The concern's own EXTRA_PROPS (e.g., `{ boolLogic: ... }` for disabledWhen)
 * - The path's value type for validationState (schema must match DeepValue<DATA, PATH>)
 *
 * Without CONCERNS (standalone usage), falls back to loose typing for backward compatibility.
 *
 * @example
 * ```typescript
 * const registration: ConcernRegistrationMap<MyFormState> = {
 *   email: { validationState: { schema: z.string().email() } },
 *   name: { validationState: { schema: z.string().min(1) } },
 * }
 * ```
 */
export type ConcernRegistrationMap<
  DATA extends object,
  CONCERNS extends readonly any[] = readonly any[],
> = Partial<{
  [PATH in DeepKey<DATA>]: Partial<{
    [C in CONCERNS[number] as C extends { name: string }
      ? C['name']
      : never]: ConcernConfigFor<C, DATA, PATH>
  }>
}>
