import type { DeepKey } from '../../types'
import type { ValidationStateInput } from '../../types/concerns'
import { dot } from '../../utils/dot'
import type { BaseConcernProps } from '../types'

export type { ValidationStateInput }

export interface ValidationError {
  field?: string
  message: string
}

export interface ValidationStateResult {
  isError: boolean
  errors: ValidationError[]
}

export interface ValidationStateConcern {
  name: 'validationState'
  description: string
  evaluate: <SUB_STATE, PATH extends DeepKey<SUB_STATE>>(
    props: BaseConcernProps<SUB_STATE, PATH> &
      ValidationStateInput<SUB_STATE, PATH>,
  ) => ValidationStateResult
}

export const validationState: ValidationStateConcern = {
  name: 'validationState',
  description: 'Zod schema validation with isError flag and detailed errors',
  evaluate: <SUB_STATE, PATH extends DeepKey<SUB_STATE>>(
    props: BaseConcernProps<SUB_STATE, PATH> &
      ValidationStateInput<SUB_STATE, PATH>,
  ): ValidationStateResult => {
    // If scope is provided, validate at scope path; otherwise validate at registration path
    // Note: props.scope is a runtime string path, use dot.get__unsafe for dynamic access
    const valueToValidate =
      'scope' in props && props.scope
        ? dot.get__unsafe(props.state, props.scope)
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
