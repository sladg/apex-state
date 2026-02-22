/**
 * Concerns system examples — validation, BoolLogic, and all operators.
 *
 * These examples type-check against the real @sladg/apex-state exports.
 */

import { createGenericStore } from '@sladg/apex-state'
import { z } from 'zod'

interface FormState {
  user: { name: string; email: string }
  tosAccepted: boolean
  step: number
}

const store = createGenericStore<FormState>()

// @llms-example: Register Zod validation, disabledWhen/visibleWhen BoolLogic, and all operators
const FormFields = () => {
  store.useConcerns('form', {
    'user.email': {
      validationState: { schema: z.string().email() },
      disabledWhen: { boolLogic: { IS_EQUAL: ['tosAccepted', false] } },
      visibleWhen: {
        boolLogic: {
          AND: [{ EXISTS: 'user.name' }, { IS_EQUAL: ['step', 2] }],
        },
      },
    },
  })

  // Built-in concerns: validationState, disabledWhen, readonlyWhen,
  //   visibleWhen, dynamicLabel, dynamicTooltip, dynamicPlaceholder
  //
  // BoolLogic operators: IS_EQUAL, EXISTS, IS_EMPTY, GT, LT, GTE, LTE, IN, AND, OR, NOT

  return null
}
// @llms-example-end

void FormFields
