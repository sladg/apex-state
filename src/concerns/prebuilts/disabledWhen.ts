/**
 * Disabled condition concern
 *
 * Evaluates a boolean logic expression to determine if a field should be disabled.
 * Automatically tracks all state paths accessed in the condition.
 *
 * Returns true if disabled, false if enabled.
 *
 * @example
 * ```typescript
 * store.useConcerns('my-concerns', {
 *   'submitButton': {
 *     disabledWhen: { condition: { IS_EMPTY: 'form.email' } }
 *   }
 * })
 * // Returns: true if form.email is empty, false otherwise
 * ```
 */

import type { ConcernType, BoolLogic } from '../types'
import { evaluateBoolLogic } from '../../utils/boolLogic'

export const disabledWhen: ConcernType<{ condition: BoolLogic<any> }, boolean> = {
  name: 'disabledWhen',
  description: 'Boolean logic for disabled state',
  evaluate: (props) => {
    return evaluateBoolLogic(props.condition, props.state)
  }
}
