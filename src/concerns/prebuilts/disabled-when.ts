import { evaluateBoolLogic } from '../../utils/bool-logic'
import type { BoolLogic, ConcernType } from '../types'

export const disabledWhen: ConcernType<{ condition: BoolLogic<any> }, boolean> =
  {
    name: 'disabledWhen',
    description: 'Boolean logic for disabled state',
    evaluate: (props) => {
      return evaluateBoolLogic(props.condition, props.state)
    },
  }
