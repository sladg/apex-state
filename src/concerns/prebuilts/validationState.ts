/**
 * ValidationState Concern
 *
 * Complete validation state concern that returns structured validation results
 * including isValid flag, detailed error information, summary message, and timestamp.
 *
 * Replaces the old validators + _errors pattern with a cleaner concerns-based approach.
 * Errors are stored in the concerns proxy (separate from state), preventing state pollution.
 *
 * @example
 * ```typescript
 * // Register validation concern
 * store.useConcerns('form-validation', {
 *   'user.email': {
 *     validationState: { schema: z.string().email() }
 *   },
 *   'user.password': {
 *     validationState: { schema: z.string().min(8) }
 *   }
 * })
 *
 * // Read validation results
 * const emailValidation = store.useFieldConcerns('user.email').validationState
 * // { isValid: false, errors: [{field: '', message: 'Invalid email'}], message: '...', timestamp: ... }
 * ```
 */

import { z } from 'zod'

import { deepGetUnsafe } from '../../store/utils/deepAccess'
import type { DeepKey, DeepValue } from '../../types'
import type { BaseConcernProps } from '../types'

/**
 * Single validation error with optional field/path information
 */
export interface ValidationError {
  /** Field path where error occurred (empty string for root) */
  field?: string
  /** Error message from Zod */
  message: string
}

/**
 * Complete validation state result
 * Returned by validationState concern when evaluated
 */
export interface ValidationStateResult {
  /** Whether all validations passed */
  isValid: boolean
  /** Array of validation errors (empty if isValid=true) */
  errors: ValidationError[]
  /** Concatenated error messages for simple display (empty if isValid=true) */
  message: string
  /** Timestamp when validation was last evaluated (for detecting updates) */
  timestamp: number
}

/**
 * Discriminated union for validationState input
 * Either validates at PATH, or at a SCOPE with type-safe schema
 */
type ValidationStateInput<SUB_STATE, PATH extends DeepKey<SUB_STATE>> =
  | {
      // No scope provided - validate at PATH
      schema: z.ZodSchema<DeepValue<SUB_STATE, PATH>>
    }
  | {
      // Scope provided - validate at scope with type-safe schema
      [SCOPE in DeepKey<SUB_STATE>]: {
        scope: SCOPE
        schema: z.ZodSchema<DeepValue<SUB_STATE, SCOPE>>
      }
    }[DeepKey<SUB_STATE>]

/**
 * ValidationState concern - full validation state with errors
 *
 * Evaluates a Zod schema and returns complete validation state including:
 * - isValid: boolean flag
 * - errors: array of {field, message} objects
 * - message: concatenated error messages
 * - timestamp: when validation was evaluated
 *
 * Supports optional scope parameter to validate a different path than the registration path.
 * Dependencies are automatically tracked by valtio-reactive's effect() system.
 */
export const validationState = {
  name: 'validationState' as const,
  description: 'Complete validation state with errors, isValid, and timestamp',
  evaluate: <SUB_STATE, PATH extends DeepKey<SUB_STATE>>(
    props: BaseConcernProps<any, PATH> & ValidationStateInput<SUB_STATE, PATH>,
  ): ValidationStateResult => {
    const timestamp = Date.now()

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
        isValid: true,
        errors: [],
        message: '',
        timestamp,
      }
    }

    // Failure: transform Zod errors to ValidationError format
    const errors: ValidationError[] = result.error.errors.map((zodError) => ({
      field: zodError.path.length > 0 ? zodError.path.join('.') : undefined,
      message: zodError.message,
    }))

    const message = errors.map((e) => e.message).join('; ')

    return {
      isValid: false,
      errors,
      message,
      timestamp,
    }
  },
}
