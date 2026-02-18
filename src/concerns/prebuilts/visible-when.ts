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
 *     visibleWhen: { boolLogic: { IS_EQUAL: ['settings.mode', 'advanced'] } }
 *   }
 * })
 * // Returns: true if settings.mode is 'advanced', false otherwise
 * ```
 */

import { evaluateBoolLogic } from '../../utils/bool-logic'
import type { BoolLogic, ConcernType } from '../types'

export const visibleWhen: ConcernType<
  'visibleWhen',
  { boolLogic: BoolLogic<any> },
  boolean
> = {
  name: 'visibleWhen',
  description: 'Boolean logic for visibility',
  evaluate: (props) => {
    return evaluateBoolLogic(props.boolLogic, props.state)
  },
}
