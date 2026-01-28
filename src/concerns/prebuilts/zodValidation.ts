/**
 * Zod schema validation concern
 *
 * Validates a value against a Zod schema. Optionally validates a different scope
 * (e.g., validate parent object instead of single field).
 *
 * Returns true if valid, false if invalid.
 *
 * @example
 * ```typescript
 * // Validate at the field path
 * store.useConcerns('my-concerns', {
 *   'email': {
 *     zodValidation: { schema: z.string().email() }
 *   }
 * })
 *
 * // Validate at a different scope
 * store.useConcerns('my-concerns', {
 *   'email': {
 *     zodValidation: {
 *       scope: 'user',
 *       schema: z.object({ email: z.string().email() })
 *     }
 *   }
 * })
 * ```
 */

import { z } from 'zod'
import type { DeepKey, DeepValue } from '../../types'
import type { BaseConcernProps } from '../types'
import { deepGet } from '../../store/utils/deepAccess'

/**
 * Discriminated union for zodValidation input
 * Either validates at PATH, or at a SCOPE with type-safe schema
 */
type ZodValidationInput<SUB_STATE, PATH extends DeepKey<SUB_STATE>> =
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

export const zodValidation = {
  name: 'zodValidation' as const,
  description: 'Zod schema validation with optional scope',
  evaluate: <SUB_STATE, PATH extends DeepKey<SUB_STATE>>(
    props: BaseConcernProps<any, PATH> & ZodValidationInput<SUB_STATE, PATH>
  ): boolean => {
    // If scope is provided, validate at scope, otherwise validate at path
    const valueToValidate = 'scope' in props && props.scope
      ? deepGet(props.state, props.scope )
      : props.value

    const result = props.schema.safeParse(valueToValidate)
    return result.success
  }
}
