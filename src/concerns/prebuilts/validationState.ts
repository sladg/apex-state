import { z } from 'zod'

import type { DeepKey, DeepValue } from '../../types'
import { deepGetUnsafe } from '../../utils/deepAccess'
import type { BaseConcernProps } from '../types'

export interface ValidationError {
  field?: string
  message: string
}

export interface ValidationStateResult {
  isError: boolean
  errors: ValidationError[]
}

type ValidationStateInput<SUB_STATE, PATH extends DeepKey<SUB_STATE>> =
  | {
      schema: z.ZodSchema<DeepValue<SUB_STATE, PATH>>
    }
  | {
      [SCOPE in DeepKey<SUB_STATE>]: {
        scope: SCOPE
        schema: z.ZodSchema<DeepValue<SUB_STATE, SCOPE>>
      }
    }[DeepKey<SUB_STATE>]

export const validationState = {
  name: 'validationState' as const,
  description: 'Zod schema validation with isError flag and detailed errors',
  evaluate: <SUB_STATE, PATH extends DeepKey<SUB_STATE>>(
    props: BaseConcernProps<any, PATH> & ValidationStateInput<SUB_STATE, PATH>,
  ): ValidationStateResult => {
    // If scope is provided, validate at scope path; otherwise validate at registration path
    // Note: props.scope is a runtime string path, use deepGetUnsafe for dynamic access
    const valueToValidate =
      'scope' in props && props.scope
        ? deepGetUnsafe(props.state, props.scope)
        : props.value

    // Run Zod validation
    const result = props.schema.safeParse(valueToValidate)

    // Success: return valid state
    if (result.success) {
      return {
        isError: false,
        errors: [],
      }
    }

    // Failure: transform Zod errors to ValidationError format
    const errors: ValidationError[] = result.error.errors.map((zodError) => ({
      field: zodError.path.length > 0 ? zodError.path.join('.') : '.',
      message: zodError.message,
    }))

    return {
      isError: true,
      errors,
    }
  },
}
