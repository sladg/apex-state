import type { BoolLogic } from '../../types'
import { evaluateBoolLogic } from '../../utils/boolLogic'
import type { BaseConcernProps, ConcernType } from '../types'

interface DisabledWhenInput<SUB_STATE> {
  condition: BoolLogic<SUB_STATE>
}

export const disabledWhen: ConcernType<
  DisabledWhenInput<Record<string, unknown>>,
  boolean
> = {
  name: 'disabledWhen',
  description: 'Boolean logic for disabled state',
  evaluate: (
    props: BaseConcernProps<Record<string, unknown>, string> &
      DisabledWhenInput<Record<string, unknown>>,
  ) => {
    return evaluateBoolLogic(props.condition, props.state)
  },
}
