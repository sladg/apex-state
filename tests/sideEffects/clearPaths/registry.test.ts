/**
 * Tests for ClearPathsRegistry
 *
 * Validates registration, unregistration, and trigger detection (including nested).
 */

import { describe, test, expect } from 'vitest'
import { createClearPathsRegistry } from '../../../src/sideEffects/clearPaths/registry'

interface TestState {
  selectedUser: string | null
  userData: any
  userPreferences: any
  form: {
    field1: string
    field2: string
  }
  formError: string | null
}

describe('ClearPathsRegistry', () => {
  describe('Registration', () => {
    test('should register clear rule', () => {
      const registry = createClearPathsRegistry<TestState>()
      registry.register({
        id: 'clear-user',
        triggerPath: 'selectedUser',
        clearPaths: ['userData', 'userPreferences'],
      })

      const rules = registry.getClearRulesTriggeredBy('selectedUser')
      expect(rules).toHaveLength(1)
      expect(rules[0].id).toBe('clear-user')
    })

    test('should register multiple clear rules', () => {
      const registry = createClearPathsRegistry<TestState>()
      registry.register({
        id: 'clear-user',
        triggerPath: 'selectedUser',
        clearPaths: ['userData'],
      })
      registry.register({
        id: 'clear-error',
        triggerPath: 'form',
        clearPaths: ['formError'],
        clearOnNested: true,
      })

      const rules1 = registry.getClearRulesTriggeredBy('selectedUser')
      expect(rules1).toHaveLength(1)

      const rules2 = registry.getClearRulesTriggeredBy('form')
      expect(rules2).toHaveLength(1)
    })

    test('should allow multiple rules for same trigger', () => {
      const registry = createClearPathsRegistry<TestState>()
      registry.register({
        id: 'clear-user-1',
        triggerPath: 'selectedUser',
        clearPaths: ['userData'],
      })
      registry.register({
        id: 'clear-user-2',
        triggerPath: 'selectedUser',
        clearPaths: ['userPreferences'],
      })

      const rules = registry.getClearRulesTriggeredBy('selectedUser')
      expect(rules).toHaveLength(2)
    })
  })

  describe('Unregistration', () => {
    test('should unregister clear rule', () => {
      const registry = createClearPathsRegistry<TestState>()
      registry.register({
        id: 'clear-user',
        triggerPath: 'selectedUser',
        clearPaths: ['userData'],
      })

      registry.unregister('clear-user')

      const rules = registry.getClearRulesTriggeredBy('selectedUser')
      expect(rules).toHaveLength(0)
    })

    test('should handle unregistering non-existent ID', () => {
      const registry = createClearPathsRegistry<TestState>()
      expect(() => {
        registry.unregister('nonexistent')
      }).not.toThrow()
    })

    test('should maintain other rules after unregistering one', () => {
      const registry = createClearPathsRegistry<TestState>()
      registry.register({
        id: 'clear-user',
        triggerPath: 'selectedUser',
        clearPaths: ['userData'],
      })
      registry.register({
        id: 'clear-error',
        triggerPath: 'form',
        clearPaths: ['formError'],
      })

      registry.unregister('clear-user')

      const rules1 = registry.getClearRulesTriggeredBy('selectedUser')
      expect(rules1).toHaveLength(0)

      const rules2 = registry.getClearRulesTriggeredBy('form')
      expect(rules2).toHaveLength(1)
    })
  })

  describe('Trigger Detection - Direct Match', () => {
    test('should trigger on exact path match', () => {
      const registry = createClearPathsRegistry<TestState>()
      registry.register({
        id: 'clear-user',
        triggerPath: 'selectedUser',
        clearPaths: ['userData'],
        clearOnNested: false,
      })

      const rules = registry.getClearRulesTriggeredBy('selectedUser')
      expect(rules).toHaveLength(1)
      expect(rules[0].id).toBe('clear-user')
    })

    test('should not trigger on nested path when clearOnNested=false', () => {
      const registry = createClearPathsRegistry<TestState>()
      registry.register({
        id: 'clear-error',
        triggerPath: 'form',
        clearPaths: ['formError'],
        clearOnNested: false,
      })

      const rules = registry.getClearRulesTriggeredBy('form.field1')
      expect(rules).toHaveLength(0)
    })

    test('should not trigger on unrelated path', () => {
      const registry = createClearPathsRegistry<TestState>()
      registry.register({
        id: 'clear-user',
        triggerPath: 'selectedUser',
        clearPaths: ['userData'],
      })

      const rules = registry.getClearRulesTriggeredBy('formError')
      expect(rules).toHaveLength(0)
    })
  })

  describe('Trigger Detection - Nested Match', () => {
    test('should trigger on nested path when clearOnNested=true', () => {
      const registry = createClearPathsRegistry<TestState>()
      registry.register({
        id: 'clear-error',
        triggerPath: 'form',
        clearPaths: ['formError'],
        clearOnNested: true,
      })

      const rules = registry.getClearRulesTriggeredBy('form.field1')
      expect(rules).toHaveLength(1)
      expect(rules[0].id).toBe('clear-error')
    })

    test('should trigger on deeply nested path when clearOnNested=true', () => {
      const registry = createClearPathsRegistry<TestState>()
      registry.register({
        id: 'clear-error',
        triggerPath: 'form',
        clearPaths: ['formError'],
        clearOnNested: true,
      })

      const rules = registry.getClearRulesTriggeredBy(
        'form.field1.subfield' as any
      )
      expect(rules).toHaveLength(1)
    })

    test('should not trigger on nested when clearOnNested defaults to false', () => {
      const registry = createClearPathsRegistry<TestState>()
      registry.register({
        id: 'clear-error',
        triggerPath: 'form',
        clearPaths: ['formError'],
        // clearOnNested not specified, should default to false
      })

      const rules = registry.getClearRulesTriggeredBy('form.field1')
      expect(rules).toHaveLength(0)
    })
  })

  describe('Complex Scenarios', () => {
    test('should return both direct and nested triggers', () => {
      const registry = createClearPathsRegistry<TestState>()
      registry.register({
        id: 'direct',
        triggerPath: 'form.field1',
        clearPaths: ['userData'],
        clearOnNested: false,
      })
      registry.register({
        id: 'nested',
        triggerPath: 'form',
        clearPaths: ['formError'],
        clearOnNested: true,
      })

      const rules = registry.getClearRulesTriggeredBy('form.field1')
      expect(rules).toHaveLength(2)
      expect(rules.map((r) => r.id).sort()).toEqual(['direct', 'nested'])
    })

    test('should avoid duplicate rules in result', () => {
      const registry = createClearPathsRegistry<TestState>()
      registry.register({
        id: 'clear-error',
        triggerPath: 'form',
        clearPaths: ['formError'],
        clearOnNested: true,
      })

      // Query with the exact trigger path - should only appear once
      const rules = registry.getClearRulesTriggeredBy('form')
      expect(rules).toHaveLength(1)
    })

    test('should handle multiple rules with different clearOnNested settings', () => {
      const registry = createClearPathsRegistry<TestState>()
      registry.register({
        id: 'nested-true',
        triggerPath: 'form',
        clearPaths: ['formError'],
        clearOnNested: true,
      })
      registry.register({
        id: 'nested-false',
        triggerPath: 'form',
        clearPaths: ['userData'],
        clearOnNested: false,
      })

      // Direct trigger should return both
      const rules1 = registry.getClearRulesTriggeredBy('form')
      expect(rules1).toHaveLength(2)

      // Nested trigger should only return the clearOnNested=true rule
      const rules2 = registry.getClearRulesTriggeredBy('form.field1')
      expect(rules2).toHaveLength(1)
      expect(rules2[0].id).toBe('nested-true')
    })
  })
})
