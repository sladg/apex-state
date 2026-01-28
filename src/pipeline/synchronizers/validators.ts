/**
 * Validators Synchronizer
 *
 * Runs Zod validations when relevant data changes.
 * Stores errors in state at configured error paths.
 */

import type { Synchronizer } from '../types'
import type { ArrayOfChanges, DeepKey, GenericMeta } from '../../types'
import type { ValidatorsRegistry } from '../../sideEffects/validators/registry'
import type { StoredError } from '../../sideEffects/validators/types'
import { deepGet } from '../../store/utils/deepAccess'

/**
 * Create a synchronizer for validators side-effect.
 *
 * Runs validators when their scope changes and stores/removes errors accordingly.
 * Multiple validators can target the same error path.
 *
 * @param registry - The ValidatorsRegistry instance managing validators
 * @param errorStorePath - Root path for error storage (default: '_errors')
 * @returns A synchronizer function that validates changes and stores errors
 */
export const createValidatorsSynchronizer = <
  DATA extends object,
  META extends GenericMeta = GenericMeta
>(
  registry: ValidatorsRegistry<DATA, META>,
  errorStorePath = '_errors'
): Synchronizer<DATA, META> => {
  return (changes, state) => {
    const processedValidators = new Set<string>()
    const errorUpdates = new Map<string, StoredError[]>() // errorPath -> updated errors
    let lastMeta: META | undefined

    for (const [path, _value, meta] of changes) {
      lastMeta = meta
      // Find validators affected by this change
      const validators = registry.getValidatorsAffectedByChange(path)

      for (const validator of validators) {
        // Process each validator only once per pipeline cycle
        if (processedValidators.has(validator.id)) continue
        processedValidators.add(validator.id)

        // Get data to validate
        const dataToValidate = validator.scope === null
          ? state
          : deepGet(state, validator.scope)

        // Run validation using Zod safeParse
        const result = validator.schema.safeParse(dataToValidate)

        // Build error path (errorStorePath.validator.errorPath)
        const errorPath = `${errorStorePath}.${validator.errorPath}` as string

        // Get current errors at this path (from state or accumulated updates)
        let currentErrors: StoredError[]
        if (errorUpdates.has(errorPath)) {
          currentErrors = errorUpdates.get(errorPath)!
        } else {
          currentErrors = (deepGet(state, errorPath as DeepKey<DATA>) as StoredError[]) || []
        }

        let updatedErrors: StoredError[]

        if (result.success) {
          // Validation passed - remove this validator's errors
          updatedErrors = currentErrors.filter(e => e.id !== validator.id)
        } else {
          // Validation failed - add/update this validator's error
          const errorMessage = result.error.errors
            .map(e => e.message)
            .join(', ')

          // Remove old error from this validator, add new one
          const otherErrors = currentErrors.filter(e => e.id !== validator.id)
          updatedErrors = [
            ...otherErrors,
            { id: validator.id, message: errorMessage }
          ]
        }

        // Store updated errors for this path
        errorUpdates.set(errorPath, updatedErrors)
      }
    }

    // Convert accumulated error updates to changes
    const newChanges: ArrayOfChanges<DATA, META> = []
    for (const [errorPath, updatedErrors] of errorUpdates.entries()) {
      newChanges.push([
        errorPath as DeepKey<DATA>,
        updatedErrors.length > 0 ? updatedErrors : undefined,
        { ...lastMeta, isProgramaticChange: true } as META
      ])
    }

    return [...changes, ...newChanges]
  }
}