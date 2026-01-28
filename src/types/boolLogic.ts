/**
 * Boolean logic DSL type definitions
 *
 * Type-safe conditional expression DSL used by concerns and side effects
 * for reactive condition checking against state.
 */

import type { DeepKey } from './deepKey'

/**
 * Primitive types that can be compared in BoolLogic expressions
 */
export type ComparableValue = string | number | boolean | null | undefined

/**
 * Boolean logic DSL for conditional expressions
 *
 * Provides a declarative way to express conditions against state paths.
 * Used by concerns (disabledWhen, visibleWhen) and side effects.
 *
 * Operators:
 * - IS_EQUAL: Compare path value to expected value
 * - EXISTS: Check if path value is not null/undefined
 * - IS_EMPTY: Check if path value is empty (string/array/object)
 * - AND/OR/NOT: Boolean combinators
 * - GT/LT/GTE/LTE: Numeric comparisons
 * - IN: Check if path value is in allowed list
 *
 * @example
 * ```typescript
 * // Simple equality check
 * const isAdmin: BoolLogic<State> = { IS_EQUAL: ['user.role', 'admin'] }
 *
 * // Combined conditions
 * const canEdit: BoolLogic<State> = {
 *   AND: [
 *     { IS_EQUAL: ['user.role', 'editor'] },
 *     { EXISTS: 'document.id' },
 *     { NOT: { IS_EQUAL: ['document.status', 'locked'] } }
 *   ]
 * }
 *
 * // Numeric comparison
 * const isExpensive: BoolLogic<State> = { GT: ['product.price', 100] }
 * ```
 */
export type BoolLogic<STATE> =
  | { IS_EQUAL: [DeepKey<STATE>, ComparableValue] }
  | { EXISTS: DeepKey<STATE> }
  | { IS_EMPTY: DeepKey<STATE> }
  | { AND: BoolLogic<STATE>[] }
  | { OR: BoolLogic<STATE>[] }
  | { NOT: BoolLogic<STATE> }
  | { GT: [DeepKey<STATE>, number] }
  | { LT: [DeepKey<STATE>, number] }
  | { GTE: [DeepKey<STATE>, number] }
  | { LTE: [DeepKey<STATE>, number] }
  | { IN: [DeepKey<STATE>, ComparableValue[]] }
