/**
 * Value logic DSL type definitions
 *
 * Type-safe conditional value selection DSL. Evaluates conditions and returns
 * arbitrary JSON values (not booleans). Used for driving options lists, labels,
 * or any concern value based on state.
 *
 * Two variants:
 * - IF/THEN/ELSE: nestable for elif chains
 * - MATCH: multi-way switch on a state path value
 *
 * @example
 * ```typescript
 * // IF/THEN/ELSE — dropdown options depend on role
 * const options: ValueLogic<State, string[]> = {
 *   IF: { IS_EQUAL: ['user.role', 'admin'] },
 *   THEN: ['create', 'read', 'update', 'delete'],
 *   ELSE: ['read'],
 * }
 *
 * // MATCH — multi-way switch
 * const label: ValueLogic<State, string> = {
 *   MATCH: 'user.role',
 *   CASES: { admin: 'Full Access', editor: 'Edit Access' },
 *   DEFAULT: 'Read Only',
 * }
 *
 * // Chained elif
 * const access: ValueLogic<State, string> = {
 *   IF: { IS_EQUAL: ['user.role', 'admin'] },
 *   THEN: 'Full',
 *   ELSE: {
 *     IF: { IS_EQUAL: ['user.role', 'editor'] },
 *     THEN: 'Edit',
 *     ELSE: 'Read',
 *   },
 * }
 * ```
 */

import type { BoolLogic } from './bool-logic'
import type { DeepKey } from './deep-key'
import type { DeepValue } from './deep-value'

/**
 * Value logic DSL for conditional value selection
 *
 * Evaluates conditions (reusing BoolLogic) and returns arbitrary JSON values.
 * THEN/ELSE/CASES values are static JSON — not derived from state.
 *
 * @typeParam STATE - The state object type for path resolution
 * @typeParam T - The return value type (defaults to unknown)
 */
export type ValueLogic<STATE, T = unknown> =
  | ValueLogicIfThenElse<STATE, T>
  | ValueLogicMatch<STATE, T>

/** IF/THEN/ELSE variant — nestable for elif chains */
export interface ValueLogicIfThenElse<STATE, T = unknown> {
  IF: BoolLogic<STATE>
  THEN: T
  ELSE: T | ValueLogic<STATE, T>
}

/** MATCH variant — multi-way switch on a state path value */
export type ValueLogicMatch<STATE, T = unknown> = {
  [P in DeepKey<STATE>]: {
    MATCH: P
    CASES: Partial<
      Record<`${Extract<DeepValue<STATE, P>, string | number>}`, T>
    >
    DEFAULT: T
  }
}[DeepKey<STATE>]
