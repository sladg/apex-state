/**
 * Integration tests for clear paths side-effect
 *
 * Tests complete clear paths functionality in a React component context.
 * Note: Full integration with useSideEffects will be completed when
 * the pipeline connects to the sideEffectsRegistry.
 */

import React from 'react'
import { describe, test, expect } from 'vitest'
import { render, waitFor, fireEvent } from '@testing-library/react'
import { createGenericStore } from '../../../src/store/createStore'
import { createClearPathsRegistry } from '../../../src/sideEffects/clearPaths/registry'
import { createClearPathsSynchronizer } from '../../../src/pipeline/synchronizers/clearPaths'
import type { GenericMeta } from '../../../src/types'

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

describe('Clear Paths Integration', () => {
  describe('Manual synchronizer integration', () => {
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

      const initialState: TestState = {
        selectedUser: 'bob',
        userData: { name: 'Bob' },
        userPreferences: { theme: 'dark' },
        form: { field1: '', field2: '' },
        formError: null,
      }

      const changes = synchronizer(
        [['selectedUser', 'alice', {}]],
        initialState
      )

      // Should have original change plus clear changes
      expect(changes.length).toBe(3)
      expect(changes[0]).toEqual(['selectedUser', 'alice', {}])

      const clearChanges = changes.slice(1)
      const clearPaths = clearChanges.map((c) => c[0])
      expect(clearPaths).toContain('userData')
      expect(clearPaths).toContain('userPreferences')

      clearChanges.forEach((change) => {
        expect(change[1]).toBe(undefined)
        expect(change[2].isProgramaticChange).toBe(true)
      })
    })

    test('should handle nested triggers with clearOnNested=true', () => {
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

      const initialState: TestState = {
        selectedUser: null,
        userData: null,
        userPreferences: null,
        form: { field1: '', field2: '' },
        formError: 'Please fill form',
      }

      const changes = synchronizer([['form.field1', 'test', {}]], initialState)

      expect(changes.length).toBe(2)
      const clearChange = changes.find((c) => c[0] === 'formError')
      expect(clearChange?.[1]).toBe(undefined)
      expect(clearChange?.[2].isProgramaticChange).toBe(true)
    })

    test('should not trigger on nested when clearOnNested=false', () => {
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

      const initialState: TestState = {
        selectedUser: null,
        userData: null,
        userPreferences: null,
        form: { field1: '', field2: '' },
        formError: 'Error',
      }

      const changes = synchronizer([['form.field1', 'test', {}]], initialState)

      // Should not clear formError
      expect(changes.length).toBe(1)
      expect(changes[0]).toEqual(['form.field1', 'test', {}])
    })

    test('should prevent duplicate clears', () => {
      const registry = createClearPathsRegistry<TestState>()
      registry.register({
        id: 'clear-1',
        triggerPath: 'selectedUser',
        clearPaths: ['userData'],
      })
      registry.register({
        id: 'clear-2',
        triggerPath: 'selectedUser',
        clearPaths: ['userData'], // Duplicate
      })

      const synchronizer = createClearPathsSynchronizer<TestState, GenericMeta>(
        registry
      )

      const initialState: TestState = {
        selectedUser: 'bob',
        userData: { name: 'Bob' },
        userPreferences: null,
        form: { field1: '', field2: '' },
        formError: null,
      }

      const changes = synchronizer(
        [['selectedUser', 'alice', {}]],
        initialState
      )

      // Should only clear userData once
      const userDataClears = changes.filter((c) => c[0] === 'userData')
      expect(userDataClears.length).toBe(1)
    })
  })

  describe('Store integration', () => {
    test('basic store operations work', async () => {
      const store = createGenericStore<TestState, GenericMeta>()

      function TestComponent() {
        const [selectedUser, setSelectedUser] = store.useStore('selectedUser')
        const [userData, setUserData] = store.useStore('userData')

        return (
          <div>
            <span data-testid="user">{selectedUser || 'none'}</span>
            <span data-testid="data">
              {userData ? JSON.stringify(userData) : 'null'}
            </span>
            <button onClick={() => setSelectedUser('alice')}>
              Select Alice
            </button>
            <button onClick={() => setUserData({ name: 'Alice' })}>
              Set Data
            </button>
          </div>
        )
      }

      const { getByText, getByTestId } = render(
        <store.Provider
          initialState={{
            selectedUser: null,
            userData: null,
            userPreferences: null,
            form: { field1: '', field2: '' },
            formError: null,
          }}
        >
          <TestComponent />
        </store.Provider>
      )

      expect(getByTestId('user').textContent).toBe('none')
      expect(getByTestId('data').textContent).toBe('null')

      getByText('Set Data').click()

      await waitFor(() => {
        expect(getByTestId('data').textContent).toBe('{"name":"Alice"}')
      })

      getByText('Select Alice').click()

      await waitFor(() => {
        expect(getByTestId('user').textContent).toBe('alice')
      })
    })

    test('useSideEffects hook registration works', () => {
      const store = createGenericStore<TestState, GenericMeta>()

      function TestComponent() {
        // Register clear paths side-effect
        store.useSideEffects('test-clear', {
          clearPaths: {
            rules: [
              {
                id: 'clear-user',
                triggerPath: 'selectedUser',
                clearPaths: ['userData', 'userPreferences'],
              },
            ],
          },
        })

        return <div>Component with clear paths</div>
      }

      const { container } = render(
        <store.Provider
          initialState={{
            selectedUser: null,
            userData: null,
            userPreferences: null,
            form: { field1: '', field2: '' },
            formError: null,
          }}
        >
          <TestComponent />
        </store.Provider>
      )

      // Should render without errors
      expect(container).toBeTruthy()
    })
  })

  describe('Registry lifecycle', () => {
    test('should handle dynamic registration', () => {
      const registry = createClearPathsRegistry<TestState>()

      // Register first rule
      registry.register({
        id: 'clear-user',
        triggerPath: 'selectedUser',
        clearPaths: ['userData'],
      })

      let rules = registry.getClearRulesTriggeredBy('selectedUser')
      expect(rules.length).toBe(1)

      // Register second rule
      registry.register({
        id: 'clear-error',
        triggerPath: 'form',
        clearPaths: ['formError'],
      })

      rules = registry.getClearRulesTriggeredBy('form')
      expect(rules.length).toBe(1)

      // Unregister first rule
      registry.unregister('clear-user')
      rules = registry.getClearRulesTriggeredBy('selectedUser')
      expect(rules.length).toBe(0)
    })

    test('should handle mount/unmount scenarios', () => {
      const registry = createClearPathsRegistry<TestState>()

      // Simulate component mount
      registry.register({
        id: 'component-1',
        triggerPath: 'selectedUser',
        clearPaths: ['userData'],
      })

      // Simulate another component mount
      registry.register({
        id: 'component-2',
        triggerPath: 'form',
        clearPaths: ['formError'],
      })

      // Both should be active
      expect(
        registry.getClearRulesTriggeredBy('selectedUser').length
      ).toBeGreaterThan(0)
      expect(registry.getClearRulesTriggeredBy('form').length).toBeGreaterThan(
        0
      )

      // Simulate first component unmount
      registry.unregister('component-1')

      // Second should still be active
      expect(registry.getClearRulesTriggeredBy('selectedUser').length).toBe(0)
      expect(registry.getClearRulesTriggeredBy('form').length).toBe(1)

      // Simulate second component unmount
      registry.unregister('component-2')

      // All should be cleared
      expect(registry.getClearRulesTriggeredBy('form').length).toBe(0)
    })
  })

  describe('Complex scenarios', () => {
    test('should handle multiple rules and triggers', () => {
      const registry = createClearPathsRegistry<TestState>()
      registry.register({
        id: 'clear-user',
        triggerPath: 'selectedUser',
        clearPaths: ['userData', 'userPreferences'],
      })
      registry.register({
        id: 'clear-error',
        triggerPath: 'form',
        clearPaths: ['formError'],
        clearOnNested: true,
      })

      const synchronizer = createClearPathsSynchronizer<TestState, GenericMeta>(
        registry
      )

      const initialState: TestState = {
        selectedUser: 'bob',
        userData: { name: 'Bob' },
        userPreferences: { theme: 'dark' },
        form: { field1: '', field2: '' },
        formError: 'Error',
      }

      // Multiple changes in one batch
      const changes = synchronizer(
        [
          ['selectedUser', 'alice', {}],
          ['form.field1', 'test', {}],
        ],
        initialState
      )

      // Original 2 + userData + userPreferences + formError = 5
      expect(changes.length).toBe(5)

      const clearChanges = changes.filter((c) => c[2].isProgramaticChange)
      expect(clearChanges.length).toBe(3)
    })

    test('should handle nested and direct triggers together', () => {
      const registry = createClearPathsRegistry<TestState>()
      registry.register({
        id: 'direct',
        triggerPath: 'form.field1',
        clearPaths: ['userData'],
      })
      registry.register({
        id: 'nested',
        triggerPath: 'form',
        clearPaths: ['formError'],
        clearOnNested: true,
      })

      const synchronizer = createClearPathsSynchronizer<TestState, GenericMeta>(
        registry
      )

      const initialState: TestState = {
        selectedUser: null,
        userData: { name: 'Test' },
        userPreferences: null,
        form: { field1: '', field2: '' },
        formError: 'Error',
      }

      const changes = synchronizer([['form.field1', 'test', {}]], initialState)

      // Original + userData (direct) + formError (nested) = 3
      expect(changes.length).toBe(3)

      const clearChanges = changes.filter((c) => c[2].isProgramaticChange)
      expect(clearChanges.length).toBe(2)

      const clearedPaths = clearChanges.map((c) => c[0])
      expect(clearedPaths).toContain('userData')
      expect(clearedPaths).toContain('formError')
    })
  })

  describe('Performance scenarios', () => {
    test('should handle rapid state changes', () => {
      const registry = createClearPathsRegistry<TestState>()
      registry.register({
        id: 'clear-user',
        triggerPath: 'selectedUser',
        clearPaths: ['userData'],
      })

      const synchronizer = createClearPathsSynchronizer<TestState, GenericMeta>(
        registry
      )

      const initialState: TestState = {
        selectedUser: null,
        userData: null,
        userPreferences: null,
        form: { field1: '', field2: '' },
        formError: null,
      }

      // Simulate 100 rapid changes
      const start = performance.now()
      for (let i = 0; i < 100; i++) {
        synchronizer([['selectedUser', `user${i}`, {}]], initialState)
      }
      const duration = performance.now() - start

      // Should complete quickly (< 20ms for 100 iterations)
      expect(duration).toBeLessThan(20)
    })

    test('should handle many clear rules efficiently', () => {
      const registry = createClearPathsRegistry<any>()

      // Create many clear rules
      for (let i = 0; i < 20; i++) {
        registry.register({
          id: `clear-${i}`,
          triggerPath: 'trigger',
          clearPaths: [`target${i}`],
        })
      }

      const synchronizer = createClearPathsSynchronizer(registry)

      const start = performance.now()
      const changes = synchronizer([['trigger', 'value', {}]], {})
      const duration = performance.now() - start

      // Should complete quickly
      expect(duration).toBeLessThan(5)

      // Should clear all 20 targets
      expect(changes.length).toBe(21) // 1 original + 20 clears
    })
  })

  describe('Error handling', () => {
    test('should handle invalid paths gracefully', () => {
      const registry = createClearPathsRegistry<TestState>()
      const synchronizer = createClearPathsSynchronizer<TestState, GenericMeta>(
        registry
      )

      // Register valid rules
      registry.register({
        id: 'clear-user',
        triggerPath: 'selectedUser',
        clearPaths: ['userData'],
      })

      // Changes to non-existent path should pass through
      const initialState: TestState = {
        selectedUser: null,
        userData: null,
        userPreferences: null,
        form: { field1: '', field2: '' },
        formError: null,
      }

      const changes = synchronizer(
        [['nonexistent' as any, 'value', {}]],
        initialState
      )

      expect(changes.length).toBe(1)
      expect(changes[0][0]).toBe('nonexistent')
    })

    test('should handle unregistered triggers', () => {
      const registry = createClearPathsRegistry<TestState>()
      const synchronizer = createClearPathsSynchronizer<TestState, GenericMeta>(
        registry
      )

      const initialState: TestState = {
        selectedUser: null,
        userData: null,
        userPreferences: null,
        form: { field1: '', field2: '' },
        formError: null,
      }

      // Change to path with no clear rules
      const changes = synchronizer(
        [['userData', { name: 'Test' }, {}]],
        initialState
      )

      // Should just pass through
      expect(changes.length).toBe(1)
      expect(changes[0]).toEqual(['userData', { name: 'Test' }, {}])
    })
  })
})
