/**
 * Tests for ValidatorsRegistry
 *
 * Validates registration, unregistration, and lookup functionality.
 */

import { describe, test, expect } from 'vitest'
import { z } from 'zod'
import { createValidatorsRegistry } from '../../../src/sideEffects/validators/registry'
import type { GenericMeta } from '../../../src/types'

interface TestState {
  email: string
  password: string
  user: {
    name: string
    age: number
  }
  _errors: Record<string, Array<{ id: string; message: string }>>
  global?: Array<{ id: string; message: string }>
}

describe('ValidatorsRegistry', () => {
  describe('Registration', () => {
    test('should register validator', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()

      registry.register({
        id: 'email-validator',
        scope: 'email',
        schema: z.string().email(),
        errorPath: 'email'
      })

      const validator = registry.getValidator('email-validator')
      expect(validator).toBeDefined()
      expect(validator?.id).toBe('email-validator')
      expect(validator?.scope).toBe('email')
    })

    test('should register multiple validators', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()

      registry.register({
        id: 'email-validator',
        scope: 'email',
        schema: z.string().email(),
        errorPath: 'email'
      })

      registry.register({
        id: 'password-validator',
        scope: 'password',
        schema: z.string().min(8),
        errorPath: 'password'
      })

      expect(registry.getAllValidators()).toHaveLength(2)
    })

    test('should register global validator with null scope', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()

      registry.register({
        id: 'global-validator',
        scope: null,
        schema: z.object({ email: z.string() }),
        errorPath: 'global'
      })

      const validator = registry.getValidator('global-validator')
      expect(validator?.scope).toBeNull()
    })
  })

  describe('Unregistration', () => {
    test('should unregister validator', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()

      registry.register({
        id: 'email-validator',
        scope: 'email',
        schema: z.string().email(),
        errorPath: 'email'
      })

      registry.unregister('email-validator')

      expect(registry.getValidator('email-validator')).toBeUndefined()
    })

    test('should handle unregistering non-existent validator', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()

      expect(() => {
        registry.unregister('nonexistent')
      }).not.toThrow()
    })

    test('should maintain other validators after unregistering one', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()

      registry.register({
        id: 'email-validator',
        scope: 'email',
        schema: z.string().email(),
        errorPath: 'email'
      })

      registry.register({
        id: 'password-validator',
        scope: 'password',
        schema: z.string().min(8),
        errorPath: 'password'
      })

      registry.unregister('email-validator')

      expect(registry.getValidator('email-validator')).toBeUndefined()
      expect(registry.getValidator('password-validator')).toBeDefined()
    })
  })

  describe('Scope Queries', () => {
    test('should get validators for specific scope', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()

      registry.register({
        id: 'email-validator',
        scope: 'email',
        schema: z.string().email(),
        errorPath: 'email'
      })

      registry.register({
        id: 'password-validator',
        scope: 'password',
        schema: z.string().min(8),
        errorPath: 'password'
      })

      const emailValidators = registry.getValidatorsForScope('email')
      expect(emailValidators).toHaveLength(1)
      expect(emailValidators[0].id).toBe('email-validator')
    })

    test('should get global validators', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()

      registry.register({
        id: 'global-validator',
        scope: null,
        schema: z.object({ email: z.string() }),
        errorPath: 'global'
      })

      const globalValidators = registry.getValidatorsForScope(null)
      expect(globalValidators).toHaveLength(1)
      expect(globalValidators[0].id).toBe('global-validator')
    })
  })

  describe('Change-Affected Queries', () => {
    test('should include global validators for any change', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()

      registry.register({
        id: 'global-validator',
        scope: null,
        schema: z.object({ email: z.string() }),
        errorPath: 'global'
      })

      const affected = registry.getValidatorsAffectedByChange('email')
      expect(affected).toHaveLength(1)
      expect(affected[0].id).toBe('global-validator')
    })

    test('should include validators with matching scope', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()

      registry.register({
        id: 'email-validator',
        scope: 'email',
        schema: z.string().email(),
        errorPath: 'email'
      })

      const affected = registry.getValidatorsAffectedByChange('email')
      expect(affected.some(v => v.id === 'email-validator')).toBe(true)
    })

    test('should include validators with parent scope', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()

      registry.register({
        id: 'user-validator',
        scope: 'user',
        schema: z.object({ name: z.string(), age: z.number() }),
        errorPath: 'user'
      })

      const affected = registry.getValidatorsAffectedByChange('user.name')
      expect(affected.some(v => v.id === 'user-validator')).toBe(true)
    })

    test('should not include validators with unrelated scope', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()

      registry.register({
        id: 'email-validator',
        scope: 'email',
        schema: z.string().email(),
        errorPath: 'email'
      })

      const affected = registry.getValidatorsAffectedByChange('password')
      expect(affected.some(v => v.id === 'email-validator')).toBe(false)
    })

    test('should handle multiple validators for same path', () => {
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

      const affected = registry.getValidatorsAffectedByChange('password')
      expect(affected).toHaveLength(2)
      expect(affected.map(v => v.id)).toContain('password-min')
      expect(affected.map(v => v.id)).toContain('password-pattern')
    })
  })

  describe('Complex Scenarios', () => {
    test('should handle nested path validation', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()

      registry.register({
        id: 'user-name-validator',
        scope: 'user.name',
        schema: z.string().min(2),
        errorPath: 'user.name'
      })

      const validator = registry.getValidator('user-name-validator')
      expect(validator?.scope).toBe('user.name')

      const affected = registry.getValidatorsAffectedByChange('user.name')
      expect(affected.some(v => v.id === 'user-name-validator')).toBe(true)
    })

    test('should handle registration and unregistration sequence', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()

      registry.register({
        id: 'validator-1',
        scope: 'email',
        schema: z.string().email(),
        errorPath: 'email'
      })

      expect(registry.getAllValidators()).toHaveLength(1)

      registry.unregister('validator-1')
      expect(registry.getAllValidators()).toHaveLength(0)

      registry.register({
        id: 'validator-2',
        scope: 'email',
        schema: z.string().min(5),
        errorPath: 'email'
      })

      expect(registry.getAllValidators()).toHaveLength(1)
      expect(registry.getValidator('validator-2')).toBeDefined()
    })
  })
})
