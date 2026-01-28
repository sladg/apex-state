/**
 * Validators Registry (Factory Pattern)
 *
 * CRITICAL ARCHITECTURE: Uses factory function, NOT classes.
 * Manages registration and lookup of Zod validators.
 */

import type { DeepKey, GenericMeta } from '../../types'
import type { ValidatorConfig } from './types'

/**
 * Registry interface for managing validators
 */
export interface ValidatorsRegistry<
  DATA extends object,
  META extends GenericMeta
> {
  /**
   * Register a validator with unique ID
   */
  register(config: ValidatorConfig<DATA>): void

  /**
   * Unregister a validator by ID
   */
  unregister(id: string): void

  /**
   * Get a specific validator by ID
   */
  getValidator(id: string): ValidatorConfig<DATA> | undefined

  /**
   * Get all validators for a specific scope
   */
  getValidatorsForScope(scope: DeepKey<DATA> | null): ValidatorConfig<DATA>[]

  /**
   * Get all registered validators
   */
  getAllValidators(): ValidatorConfig<DATA>[]

  /**
   * Get validators affected by a change at a given path
   * Includes global validators and validators with matching/parent scopes
   */
  getValidatorsAffectedByChange(path: DeepKey<DATA>): ValidatorConfig<DATA>[]
}

/**
 * Create a validators registry using factory pattern
 *
 * @example
 * ```typescript
 * const registry = createValidatorsRegistry<AppState, GenericMeta>()
 * registry.register({
 *   id: 'email-validator',
 *   scope: 'user.email',
 *   schema: z.string().email(),
 *   errorPath: 'user.email'
 * })
 * ```
 */
export const createValidatorsRegistry = <
  DATA extends object,
  META extends GenericMeta
>(): ValidatorsRegistry<DATA, META> => {
  // Private state
  const validators = new Map<string, ValidatorConfig<DATA>>()
  const scopeIndex = new Map<string, Set<string>>() // scope → Set<validatorId>

  const register = (config: ValidatorConfig<DATA>): void => {
    validators.set(config.id, config)

    // Index by scope for efficient lookup
    const scopeKey = config.scope === null ? '__global__' : (config.scope as string)
    if (!scopeIndex.has(scopeKey)) {
      scopeIndex.set(scopeKey, new Set())
    }
    scopeIndex.get(scopeKey)!.add(config.id)
  }

  const unregister = (id: string): void => {
    const config = validators.get(id)
    if (!config) return

    const scopeKey = config.scope === null ? '__global__' : (config.scope as string)
    scopeIndex.get(scopeKey)?.delete(id)

    validators.delete(id)
  }

  const getValidator = (id: string): ValidatorConfig<DATA> | undefined => {
    return validators.get(id)
  }

  const getValidatorsForScope = (
    scope: DeepKey<DATA> | null
  ): ValidatorConfig<DATA>[] => {
    const scopeKey = scope === null ? '__global__' : (scope as string)
    const ids = scopeIndex.get(scopeKey) || new Set()
    return Array.from(ids).map(id => validators.get(id)!).filter(Boolean)
  }

  const getAllValidators = (): ValidatorConfig<DATA>[] => {
    return Array.from(validators.values())
  }

  const getValidatorsAffectedByChange = (
    path: DeepKey<DATA>
  ): ValidatorConfig<DATA>[] => {
    const affected: ValidatorConfig<DATA>[] = []

    // Always include global validators
    affected.push(...getValidatorsForScope(null))

    // Check validators with scope matching or parent of this path
    for (const [scopeKey, ids] of scopeIndex.entries()) {
      if (scopeKey === '__global__') continue

      const pathStr = path as string
      // Validator scope matches or is parent of changed path
      if (pathStr === scopeKey || pathStr.startsWith(scopeKey + '.')) {
        ids.forEach(id => {
          const validator = validators.get(id)
          if (validator) affected.push(validator)
        })
      }
    }

    return affected
  }

  return {
    register,
    unregister,
    getValidator,
    getValidatorsForScope,
    getAllValidators,
    getValidatorsAffectedByChange
  }
}
