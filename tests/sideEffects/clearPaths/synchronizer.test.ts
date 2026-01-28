/**
 * Tests for clearPaths synchronizer
 *
 * Validates clearing logic, nested trigger detection, and duplicate prevention.
 */

import { describe, test, expect } from 'vitest'
import { createClearPathsSynchronizer } from '../../../src/pipeline/synchronizers/clearPaths'
import { createClearPathsRegistry } from '../../../src/sideEffects/clearPaths/registry'
import type { ArrayOfChanges, GenericMeta } from '../../../src/types'

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

describe('createClearPathsSynchronizer', () => {
  describe('Basic Clear Logic', () => {
    test('should clear paths when trigger changes', () => {
      const registry = createClearPathsRegistry<TestState>()
      registry.register({
        id: 'clear-user',
        triggerPath: 'selectedUser',
        clearPaths: ['userData', 'userPreferences'],
      })

      const synchronizer = createClearPathsSynchronizer<TestState, GenericMeta>(
        registry
      )

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['selectedUser', 'alice', {}],
      ]

      const result = synchronizer(changes, {
        selectedUser: 'bob',
        userData: { name: 'Bob' },
        userPreferences: { theme: 'dark' },
        form: { field1: '', field2: '' },
        formError: null,
      })

      // Should have original change plus two clear changes
      expect(result.length).toBe(3)
      expect(result[0]).toEqual(['selectedUser', 'alice', {}])

      const clearChanges = result.slice(1)
      expect(clearChanges[0][0]).toBe('userData')
      expect(clearChanges[0][1]).toBe(undefined)
      expect(clearChanges[0][2].isProgramaticChange).toBe(true)

      expect(clearChanges[1][0]).toBe('userPreferences')
      expect(clearChanges[1][1]).toBe(undefined)
      expect(clearChanges[1][2].isProgramaticChange).toBe(true)
    })

    test('should clear single path', () => {
      const registry = createClearPathsRegistry<TestState>()
      registry.register({
        id: 'clear-error',
        triggerPath: 'form',
        clearPaths: ['formError'],
      })

      const synchronizer = createClearPathsSynchronizer<TestState, GenericMeta>(
        registry
      )

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['form', { field1: 'test', field2: '' }, {}],
      ]

      const result = synchronizer(changes, {
        selectedUser: null,
        userData: null,
        userPreferences: null,
        form: { field1: '', field2: '' },
        formError: 'Error message',
      })

      expect(result.length).toBe(2)
      const clearChange = result.find((c) => c[0] === 'formError')
      expect(clearChange?.[1]).toBe(undefined)
      expect(clearChange?.[2].isProgramaticChange).toBe(true)
    })

    test('should handle multiple changes in one batch', () => {
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

      const synchronizer = createClearPathsSynchronizer<TestState, GenericMeta>(
        registry
      )

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['selectedUser', 'alice', {}],
        ['form', { field1: 'test', field2: '' }, {}],
      ]

      const result = synchronizer(changes, {
        selectedUser: 'bob',
        userData: { name: 'Bob' },
        userPreferences: null,
        form: { field1: '', field2: '' },
        formError: 'Error',
      })

      // Original 2 changes + 2 clear changes
      expect(result.length).toBe(4)
      const clearChanges = result.filter((c) => c[2].isProgramaticChange)
      expect(clearChanges.length).toBe(2)
    })
  })

  describe('Nested Trigger Detection', () => {
    test('should trigger on nested change when clearOnNested=true', () => {
      const registry = createClearPathsRegistry<TestState>()
      registry.register({
        id: 'clear-error',
        triggerPath: 'form',
        clearPaths: ['formError'],
        clearOnNested: true,
      })

      const synchronizer = createClearPathsSynchronizer<TestState, GenericMeta>(
        registry
      )

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['form.field1', 'test', {}],
      ]

      const result = synchronizer(changes, {
        selectedUser: null,
        userData: null,
        userPreferences: null,
        form: { field1: '', field2: '' },
        formError: 'Please fill form',
      })

      expect(result.length).toBe(2)
      const clearChange = result.find((c) => c[0] === 'formError')
      expect(clearChange?.[1]).toBe(undefined)
    })

    test('should not trigger on nested change when clearOnNested=false', () => {
      const registry = createClearPathsRegistry<TestState>()
      registry.register({
        id: 'clear-error',
        triggerPath: 'form',
        clearPaths: ['formError'],
        clearOnNested: false,
      })

      const synchronizer = createClearPathsSynchronizer<TestState, GenericMeta>(
        registry
      )

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['form.field1', 'test', {}],
      ]

      const result = synchronizer(changes, {
        selectedUser: null,
        userData: null,
        userPreferences: null,
        form: { field1: '', field2: '' },
        formError: 'Please fill form',
      })

      // No clear should happen
      expect(result.length).toBe(1)
      expect(result[0]).toEqual(['form.field1', 'test', {}])
    })

    test('should handle deeply nested paths', () => {
      const registry = createClearPathsRegistry<any>()
      registry.register({
        id: 'clear-error',
        triggerPath: 'form',
        clearPaths: ['error'],
        clearOnNested: true,
      })

      const synchronizer = createClearPathsSynchronizer(registry)

      const changes: ArrayOfChanges<any, GenericMeta> = [
        ['form.section.subsection.field', 'value', {}],
      ]

      const result = synchronizer(changes, {})

      expect(result.length).toBe(2)
      const clearChange = result.find((c) => c[0] === 'error')
      expect(clearChange?.[1]).toBe(undefined)
    })
  })

  describe('Duplicate Prevention', () => {
    test('should avoid clearing same path multiple times', () => {
      const registry = createClearPathsRegistry<TestState>()
      registry.register({
        id: 'clear-user-1',
        triggerPath: 'selectedUser',
        clearPaths: ['userData', 'userPreferences'],
      })
      registry.register({
        id: 'clear-user-2',
        triggerPath: 'selectedUser',
        clearPaths: ['userData'], // Duplicate clear path
      })

      const synchronizer = createClearPathsSynchronizer<TestState, GenericMeta>(
        registry
      )

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['selectedUser', 'alice', {}],
      ]

      const result = synchronizer(changes, {
        selectedUser: 'bob',
        userData: { name: 'Bob' },
        userPreferences: { theme: 'dark' },
        form: { field1: '', field2: '' },
        formError: null,
      })

      // Should have original + 2 clears (userData only once, userPreferences once)
      expect(result.length).toBe(3)

      const clearPaths = result.slice(1).map((c) => c[0])
      expect(clearPaths).toContain('userData')
      expect(clearPaths).toContain('userPreferences')

      // Count userData occurrences - should be 1
      const userDataClears = clearPaths.filter((p) => p === 'userData')
      expect(userDataClears.length).toBe(1)
    })

    test('should handle multiple triggers in same batch without duplicates', () => {
      const registry = createClearPathsRegistry<any>()
      registry.register({
        id: 'clear-1',
        triggerPath: 'trigger1',
        clearPaths: ['target', 'other'],
      })
      registry.register({
        id: 'clear-2',
        triggerPath: 'trigger2',
        clearPaths: ['target', 'another'],
      })

      const synchronizer = createClearPathsSynchronizer(registry)

      const changes: ArrayOfChanges<any, GenericMeta> = [
        ['trigger1', 'value1', {}],
        ['trigger2', 'value2', {}],
      ]

      const result = synchronizer(changes, {})

      // Original 2 + clears: target (1), other (1), another (1) = 5 total
      expect(result.length).toBe(5)

      const clearPaths = result.slice(2).map((c) => c[0])
      const targetClears = clearPaths.filter((p) => p === 'target')
      expect(targetClears.length).toBe(1) // Should only clear once
    })
  })

  describe('Metadata Preservation', () => {
    test('should preserve original metadata in clear changes', () => {
      const registry = createClearPathsRegistry<TestState>()
      registry.register({
        id: 'clear-user',
        triggerPath: 'selectedUser',
        clearPaths: ['userData'],
      })

      const synchronizer = createClearPathsSynchronizer<TestState, GenericMeta>(
        registry
      )

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['selectedUser', 'alice', { sender: 'user-123' }],
      ]

      const result = synchronizer(changes, {
        selectedUser: 'bob',
        userData: { name: 'Bob' },
        userPreferences: null,
        form: { field1: '', field2: '' },
        formError: null,
      })

      const clearChange = result.find((c) => c[0] === 'userData')
      expect(clearChange?.[2].sender).toBe('user-123')
      expect(clearChange?.[2].isProgramaticChange).toBe(true)
    })
  })

  describe('Edge Cases', () => {
    test('should handle empty changes', () => {
      const registry = createClearPathsRegistry<TestState>()
      const synchronizer = createClearPathsSynchronizer<TestState, GenericMeta>(
        registry
      )

      const changes: ArrayOfChanges<TestState, GenericMeta> = []
      const result = synchronizer(changes, {
        selectedUser: null,
        userData: null,
        userPreferences: null,
        form: { field1: '', field2: '' },
        formError: null,
      })

      expect(result).toEqual([])
    })

    test('should handle changes with no clear rules', () => {
      const registry = createClearPathsRegistry<TestState>()
      registry.register({
        id: 'clear-user',
        triggerPath: 'selectedUser',
        clearPaths: ['userData'],
      })

      const synchronizer = createClearPathsSynchronizer<TestState, GenericMeta>(
        registry
      )

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['formError', 'Error', {}],
      ]

      const result = synchronizer(changes, {
        selectedUser: null,
        userData: null,
        userPreferences: null,
        form: { field1: '', field2: '' },
        formError: null,
      })

      // Should return original changes only
      expect(result.length).toBe(1)
      expect(result[0]).toEqual(['formError', 'Error', {}])
    })

    test('should handle registry with no rules', () => {
      const registry = createClearPathsRegistry<TestState>()
      const synchronizer = createClearPathsSynchronizer<TestState, GenericMeta>(
        registry
      )

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['selectedUser', 'alice', {}],
      ]

      const result = synchronizer(changes, {
        selectedUser: 'bob',
        userData: null,
        userPreferences: null,
        form: { field1: '', field2: '' },
        formError: null,
      })

      expect(result.length).toBe(1)
      expect(result[0]).toEqual(['selectedUser', 'alice', {}])
    })
  })
})
