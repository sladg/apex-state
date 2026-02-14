/**
 * Read-only condition concern
 *
 * Evaluates a boolean logic expression to determine if a field should be read-only.
 * Automatically tracks all state paths accessed in the condition.
 *
 * Returns true if read-only, false if editable.
 *
 * @example
 * ```typescript
 * store.useConcerns('my-concerns', {
 *   'productId': {
 *     readonlyWhen: { condition: { IS_EQUAL: ['order.status', 'completed'] } }
 *   }
 * })
 * // Returns: true if order status is 'completed', false otherwise
 * ```
 */

import type { BoolLogic } from '~/types'
import { evaluateBoolLogic } from '~/utils/boolLogic'

import type { BaseConcernProps, ConcernType } from '../types'

export interface ReadonlyWhenInput<SUB_STATE> {
  condition: BoolLogic<SUB_STATE>
}

export const readonlyWhen: ConcernType<
  ReadonlyWhenInput<Record<string, unknown>>,
  boolean
> = {
  name: 'readonlyWhen',
  description: 'Boolean logic for readonly state',
  evaluate: (
    props: BaseConcernProps<Record<string, unknown>, string> &
      ReadonlyWhenInput<Record<string, unknown>>,
  ) => {
    return evaluateBoolLogic(props.condition, props.state)
  },
}
