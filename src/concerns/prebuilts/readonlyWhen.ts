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

import { evaluateBoolLogic } from '../../utils/boolLogic'
import type { BoolLogic, ConcernType } from '../types'

export const readonlyWhen: ConcernType<{ condition: BoolLogic<any> }, boolean> =
  {
    name: 'readonlyWhen',
    description: 'Boolean logic for readonly state',
    evaluate: (props) => {
      return evaluateBoolLogic(props.condition, props.state)
    },
  }
