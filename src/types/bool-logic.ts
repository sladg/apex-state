/**
 * Boolean logic DSL type definitions
 *
 * Type-safe conditional expression DSL used by concerns and side effects
 * for reactive condition checking against state.
 */

import type { DeepKey, DefaultDepth } from './deep-key'
import type { DeepKeyFiltered } from './deep-key-filtered'
import type { DeepValue } from './deep-value'

/**
 * Path-value pair distributed over all valid paths.
 * Each path is paired with its resolved value type, ensuring type safety.
 * The distribution happens inside the tuple, not at the union level —
 * this keeps BoolLogic's top-level union flat so `in` narrowing works.
 */
type PathValuePair<STATE, Depth extends number> =
  DeepKey<STATE, Depth> extends infer P
    ? P extends string
      ? [P, DeepValue<STATE, P>]
      : never
    : never

/**
 * Path-value array pair for IN operator.
 * Same distribution as PathValuePair but with array values.
 */
type PathValueArrayPair<STATE, Depth extends number> =
  DeepKey<STATE, Depth> extends infer P
    ? P extends string
      ? [P, DeepValue<STATE, P>[]]
      : never
    : never

/**
 * Array-elements pair for CONTAINS_ANY / CONTAINS_ALL operators.
 * Same distribution as PathArrayElementPair but the second element is an array
 * of items, enabling multi-value containment checks.
 */
type PathArrayElementsPair<STATE, Depth extends number> =
  DeepKey<STATE, Depth> extends infer P
    ? P extends string
      ? NonNullable<DeepValue<STATE, P>> extends readonly (infer Item)[]
        ? [P, Item[]]
        : never
      : never
    : never

/**
 * Paths that resolve to number, plus `.length` on array-valued paths.
 * Allows GT/LT/GTE/LTE to compare against array lengths without
 * polluting DeepKey with virtual paths.
 */
type NumericPaths<STATE, Depth extends number> =
  | DeepKeyFiltered<STATE, number, Depth>
  | `${DeepKeyFiltered<STATE, readonly unknown[], Depth>}.length`

/**
 * Boolean logic DSL for conditional expressions
 *
 * Provides a declarative way to express conditions against state paths.
 * Used by concerns (disabledWhen, visibleWhen) and side effects.
 *
 * Operators:
 * - IS_EQUAL: Compare path value to expected value (value must match path type)
 * - EXISTS: Check if path value is not null/undefined
 * - IS_EMPTY: Check if path value is empty (string/array/object)
 * - AND/OR/NOT: Boolean combinators
 * - GT/LT/GTE/LTE: Numeric comparisons (only on number paths or array.length)
 * - IN: Check if path value is in allowed list (values must match path type)
 * - CONTAINS_ANY: Check if array at path contains any of the given elements
 * - CONTAINS_ALL: Check if array at path contains all of the given elements
 * - Shorthand: [path, value] tuple as shorthand for IS_EQUAL
 *
 * @example
 * ```typescript
 * // Simple equality check
 * const isAdmin: BoolLogic<State> = { IS_EQUAL: ['user.role', 'admin'] }
 *
 * // Shorthand equality (equivalent to IS_EQUAL)
 * const isAdmin2: BoolLogic<State> = ['user.role', 'admin']
 *
 * // Combined conditions with shorthand
 * const canEdit: BoolLogic<State> = {
 *   AND: [
 *     ['user.role', 'editor'],
 *     { EXISTS: 'document.id' },
 *     { NOT: ['document.status', 'locked'] }
 *   ]
 * }
 *
 * // Numeric comparison
 * const isExpensive: BoolLogic<State> = { GT: ['product.price', 100] }
 *
 * // Array length comparison
 * const hasManyItems: BoolLogic<State> = { GT: ['cart.items.length', 5] }
 * ```
 */
export type BoolLogic<STATE, Depth extends number = DefaultDepth> =
  | { IS_EQUAL: PathValuePair<STATE, Depth> }
  | { EXISTS: DeepKey<STATE, Depth> }
  | { IS_EMPTY: DeepKey<STATE, Depth> }
  | { AND: BoolLogic<STATE, Depth>[] }
  | { OR: BoolLogic<STATE, Depth>[] }
  | { NOT: BoolLogic<STATE, Depth> }
  | { GT: [NumericPaths<STATE, Depth>, number] }
  | { LT: [NumericPaths<STATE, Depth>, number] }
  | { GTE: [NumericPaths<STATE, Depth>, number] }
  | { LTE: [NumericPaths<STATE, Depth>, number] }
  | { IN: PathValueArrayPair<STATE, Depth> }
  | { CONTAINS_ANY: PathArrayElementsPair<STATE, Depth> }
  | { CONTAINS_ALL: PathArrayElementsPair<STATE, Depth> }
  | PathValuePair<STATE, Depth>
