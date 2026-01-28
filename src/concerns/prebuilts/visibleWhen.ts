/**
 * Visibility condition concern
 *
 * Evaluates a boolean logic expression to determine if a field should be visible.
 * Automatically tracks all state paths accessed in the condition.
 *
 * Returns true if visible, false if hidden.
 *
 * @example
 * ```typescript
 * store.useConcerns('my-concerns', {
 *   'advancedOptions': {
 *     visibleWhen: { condition: { IS_EQUAL: ['settings.mode', 'advanced'] } }
 *   }
 * })
 * // Returns: true if settings.mode is 'advanced', false otherwise
 * ```
 */

import type { ConcernType, BoolLogic } from '../types'
import { evaluateBoolLogic } from '../../utils/boolLogic'

export const visibleWhen: ConcernType<{ condition: BoolLogic<any> }, boolean> = {
  name: 'visibleWhen',
  description: 'Boolean logic for visibility',
  evaluate: (props) => {
    return evaluateBoolLogic(props.condition, props.state)
  }
}
