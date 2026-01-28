/**
 * Tests for validators synchronizer
 *
 * Validates Zod validation execution, error storage, and efficiency.
 */

import { describe, test, expect } from 'vitest'
import { z } from 'zod'
import { createValidatorsSynchronizer } from '../../../src/pipeline/synchronizers/validators'
import { createValidatorsRegistry } from '../../../src/sideEffects/validators/registry'
import type { ArrayOfChanges, GenericMeta } from '../../../src/types'
import type { StoredError } from '../../../src/sideEffects/validators/types'

interface TestState {
  email: string
  password: string
  user: {
    name: string
    age: number
  }
  _errors?: {
    email?: StoredError[]
    password?: StoredError[]
    'user.name'?: StoredError[]
  }
}

describe('createValidatorsSynchronizer', () => {
  describe('Basic Validation', () => {
    test('should add error when validation fails', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()
      registry.register({
        id: 'email-validator',
        scope: 'email',
        schema: z.string().email(),
        errorPath: 'email'
      })

      const synchronizer = createValidatorsSynchronizer(registry, '_errors')

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['email', 'invalid-email', {}]
      ]

      const state: TestState = {
        email: 'invalid-email',
        password: '',
        user: { name: '', age: 0 }
      }

      const result = synchronizer(changes, state)

      // Should have original change plus error change
      expect(result.length).toBe(2)

      const errorChange = result.find(c => c[0] === '_errors.email')
      expect(errorChange).toBeDefined()
      expect(Array.isArray(errorChange?.[1])).toBe(true)

      const errors = errorChange?.[1] as StoredError[]
      expect(errors).toHaveLength(1)
      expect(errors[0].id).toBe('email-validator')
      expect(errors[0].message).toContain('email')
    })

    test('should remove error when validation passes', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()
      registry.register({
        id: 'email-validator',
        scope: 'email',
        schema: z.string().email(),
        errorPath: 'email'
      })

      const synchronizer = createValidatorsSynchronizer(registry, '_errors')

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['email', 'valid@email.com', {}]
      ]

      const state: TestState = {
        email: 'valid@email.com',
        password: '',
        user: { name: '', age: 0 },
        _errors: {
          email: [{ id: 'email-validator', message: 'Invalid email' }]
        }
      }

      const result = synchronizer(changes, state)

      const errorChange = result.find(c => c[0] === '_errors.email')
      expect(errorChange).toBeDefined()
      expect(errorChange?.[1]).toBeUndefined() // Cleared errors
    })

    test('should handle global validators', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()
      registry.register({
        id: 'global-validator',
        scope: null,
        schema: z.object({
          email: z.string().email()
        }),
        errorPath: 'global'
      })

      const synchronizer = createValidatorsSynchronizer(registry, '_errors')

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['email', 'invalid', {}]
      ]

      const state: TestState = {
        email: 'invalid',
        password: '',
        user: { name: '', age: 0 }
      }

      const result = synchronizer(changes, state)

      // Global validator should run on any change
      const errorChange = result.find(c => c[0] === '_errors.global')
      expect(errorChange).toBeDefined()
    })
  })

  describe('Multiple Validators', () => {
    test('should support multiple validators for same path', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()

      registry.register({
        id: 'password-min',
        scope: 'password',
        schema: z.string().min(8),
        errorPath: 'password'
      })

      registry.register({
        id: 'password-pattern',
        scope: 'password',
        schema: z.string().regex(/[A-Z]/),
        errorPath: 'password'
      })

      const synchronizer = createValidatorsSynchronizer(registry, '_errors')

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['password', 'short', {}]
      ]

      const state: TestState = {
        email: '',
        password: 'short',
        user: { name: '', age: 0 }
      }

      const result = synchronizer(changes, state)

      const errorChange = result.find(c => c[0] === '_errors.password')
      const errors = errorChange?.[1] as StoredError[]

      // Both validators should fail
      expect(errors).toHaveLength(2)
      expect(errors.map(e => e.id)).toContain('password-min')
      expect(errors.map(e => e.id)).toContain('password-pattern')
    })

    test('should preserve errors from other validators', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()

      registry.register({
        id: 'password-min',
        scope: 'password',
        schema: z.string().min(8),
        errorPath: 'password'
      })

      registry.register({
        id: 'password-pattern',
        scope: 'password',
        schema: z.string().regex(/[A-Z]/),
        errorPath: 'password'
      })

      const synchronizer = createValidatorsSynchronizer(registry, '_errors')

      // Password passes length but not pattern
      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['password', 'longpassword', {}]
      ]

      const state: TestState = {
        email: '',
        password: 'longpassword',
        user: { name: '', age: 0 }
      }

      const result = synchronizer(changes, state)

      const errorChange = result.find(c => c[0] === '_errors.password')
      const errors = errorChange?.[1] as StoredError[]

      // Only pattern validator should fail
      expect(errors).toHaveLength(1)
      expect(errors[0].id).toBe('password-pattern')
    })

    test('should clear all errors when all validators pass', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()

      registry.register({
        id: 'password-min',
        scope: 'password',
        schema: z.string().min(8),
        errorPath: 'password'
      })

      registry.register({
        id: 'password-pattern',
        scope: 'password',
        schema: z.string().regex(/[A-Z]/),
        errorPath: 'password'
      })

      const synchronizer = createValidatorsSynchronizer(registry, '_errors')

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['password', 'ValidPassword123', {}]
      ]

      const state: TestState = {
        email: '',
        password: 'ValidPassword123',
        user: { name: '', age: 0 },
        _errors: {
          password: [
            { id: 'password-min', message: 'Too short' },
            { id: 'password-pattern', message: 'No uppercase' }
          ]
        }
      }

      const result = synchronizer(changes, state)

      const errorChange = result.find(c => c[0] === '_errors.password')
      expect(errorChange?.[1]).toBeUndefined() // All errors cleared
    })
  })

  describe('Nested Path Validation', () => {
    test('should validate nested paths', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()

      registry.register({
        id: 'user-name-validator',
        scope: 'user.name',
        schema: z.string().min(2),
        errorPath: 'user.name'
      })

      const synchronizer = createValidatorsSynchronizer(registry, '_errors')

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['user.name', 'A', {}]
      ]

      const state: TestState = {
        email: '',
        password: '',
        user: { name: 'A', age: 0 }
      }

      const result = synchronizer(changes, state)

      const errorChange = result.find(c => c[0] === '_errors.user.name')
      expect(errorChange).toBeDefined()
      const errors = errorChange?.[1] as StoredError[]
      expect(errors[0].id).toBe('user-name-validator')
    })

    test('should run parent scope validators on child changes', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()

      registry.register({
        id: 'user-validator',
        scope: 'user',
        schema: z.object({
          name: z.string().min(2),
          age: z.number().min(0)
        }),
        errorPath: 'user'
      })

      const synchronizer = createValidatorsSynchronizer(registry, '_errors')

      // Change child property
      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['user.name', 'A', {}]
      ]

      const state: TestState = {
        email: '',
        password: '',
        user: { name: 'A', age: 0 }
      }

      const result = synchronizer(changes, state)

      // Parent validator should run
      const errorChange = result.find(c => c[0] === '_errors.user')
      expect(errorChange).toBeDefined()
    })
  })

  describe('Metadata Handling', () => {
    test('should mark error changes as programmatic', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()

      registry.register({
        id: 'email-validator',
        scope: 'email',
        schema: z.string().email(),
        errorPath: 'email'
      })

      const synchronizer = createValidatorsSynchronizer(registry, '_errors')

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['email', 'invalid', {}]
      ]

      const state: TestState = {
        email: 'invalid',
        password: '',
        user: { name: '', age: 0 }
      }

      const result = synchronizer(changes, state)

      const errorChange = result.find(c => c[0] === '_errors.email')
      expect(errorChange?.[2].isProgramaticChange).toBe(true)
    })

    test('should preserve original change metadata', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()

      registry.register({
        id: 'email-validator',
        scope: 'email',
        schema: z.string().email(),
        errorPath: 'email'
      })

      const synchronizer = createValidatorsSynchronizer(registry, '_errors')

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['email', 'invalid', { sender: 'user-123' }]
      ]

      const state: TestState = {
        email: 'invalid',
        password: '',
        user: { name: '', age: 0 }
      }

      const result = synchronizer(changes, state)

      // Original change should be preserved
      expect(result[0][2].sender).toBe('user-123')
    })
  })

  describe('Efficiency', () => {
    test('should process each validator only once per cycle', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()

      let validationCount = 0
      const countingSchema = z.string().email().refine(() => {
        validationCount++
        return false
      })

      registry.register({
        id: 'email-validator',
        scope: 'email',
        schema: countingSchema,
        errorPath: 'email'
      })

      const synchronizer = createValidatorsSynchronizer(registry, '_errors')

      // Multiple changes to same path
      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['email', 'test1@example.com', {}],
        ['email', 'test2@example.com', {}]
      ]

      const state: TestState = {
        email: 'test2@example.com',
        password: '',
        user: { name: '', age: 0 }
      }

      synchronizer(changes, state)

      // Should only validate once despite multiple changes
      expect(validationCount).toBe(1)
    })

    test('should not run unaffected validators', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()

      let emailValidationCount = 0
      let passwordValidationCount = 0

      registry.register({
        id: 'email-validator',
        scope: 'email',
        schema: z.string().email().refine(() => {
          emailValidationCount++
          return true
        }),
        errorPath: 'email'
      })

      registry.register({
        id: 'password-validator',
        scope: 'password',
        schema: z.string().min(8).refine(() => {
          passwordValidationCount++
          return true
        }),
        errorPath: 'password'
      })

      const synchronizer = createValidatorsSynchronizer(registry, '_errors')

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['email', 'test@example.com', {}]
      ]

      const state: TestState = {
        email: 'test@example.com',
        password: 'password123',
        user: { name: '', age: 0 }
      }

      synchronizer(changes, state)

      // Only email validator should run
      expect(emailValidationCount).toBe(1)
      expect(passwordValidationCount).toBe(0)
    })
  })

  describe('Edge Cases', () => {
    test('should handle empty changes', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()
      const synchronizer = createValidatorsSynchronizer(registry, '_errors')

      const changes: ArrayOfChanges<TestState, GenericMeta> = []
      const state: TestState = {
        email: '',
        password: '',
        user: { name: '', age: 0 }
      }

      const result = synchronizer(changes, state)
      expect(result).toEqual([])
    })

    test('should handle changes with no matching validators', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()

      registry.register({
        id: 'email-validator',
        scope: 'email',
        schema: z.string().email(),
        errorPath: 'email'
      })

      const synchronizer = createValidatorsSynchronizer(registry, '_errors')

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['password', 'newpassword', {}]
      ]

      const state: TestState = {
        email: '',
        password: 'newpassword',
        user: { name: '', age: 0 }
      }

      const result = synchronizer(changes, state)

      // Should only return original change (no validators affected)
      expect(result.length).toBe(1)
      expect(result[0][0]).toBe('password')
    })

    test('should handle custom error store path', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()

      registry.register({
        id: 'email-validator',
        scope: 'email',
        schema: z.string().email(),
        errorPath: 'email'
      })

      const synchronizer = createValidatorsSynchronizer(registry, 'validationErrors')

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['email', 'invalid', {}]
      ]

      const state: TestState = {
        email: 'invalid',
        password: '',
        user: { name: '', age: 0 }
      }

      const result = synchronizer(changes, state)

      const errorChange = result.find(c => c[0] === 'validationErrors.email')
      expect(errorChange).toBeDefined()
    })
  })
})
