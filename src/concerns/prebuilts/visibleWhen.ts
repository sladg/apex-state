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

import type { BoolLogic } from '~/types'
import { evaluateBoolLogic } from '~/utils/boolLogic'

import type { BaseConcernProps, ConcernType } from '../types'

export interface VisibleWhenInput<SUB_STATE> {
  condition: BoolLogic<SUB_STATE>
}

export const visibleWhen: ConcernType<
  VisibleWhenInput<Record<string, unknown>>,
  boolean
> = {
  name: 'visibleWhen',
  description: 'Boolean logic for visibility',
  evaluate: (
    props: BaseConcernProps<Record<string, unknown>, string> &
      VisibleWhenInput<Record<string, unknown>>,
  ) => {
    return evaluateBoolLogic(props.condition, props.state)
  },
}
