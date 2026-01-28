/**
 * Listeners Integration Tests
 *
 * Tests for real-world listener usage patterns.
 * Demonstrates global and scoped listeners working with the synchronizer.
 */

import { describe, it, expect, vi } from 'vitest'
import { proxy } from 'valtio'
import { createListenersRegistry } from '../../../src/sideEffects/listeners/registry'
import { createListenersSynchronizer } from '../../../src/pipeline/synchronizers/listeners'
import type { ArrayOfChanges, GenericMeta } from '../../../src/types'

interface TestState {
  user: {
    name: string
    age: number
    address: {
      city: string
    }
  }
  count: number
}

type TestMeta = GenericMeta

describe('Listeners Integration', () => {
  describe('global listener receives all changes', () => {
    it('tracks all state changes with global listener', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()
      const receivedChanges: any[] = []

      registry.register('global', null, (changes) => {
        receivedChanges.push(...changes)
      })

      const synchronizer = createListenersSynchronizer(registry)
      const state = proxy<TestState>({
        user: { name: 'Alice', age: 30, address: { city: 'NYC' } },
        count: 0,
      })

      const changes: ArrayOfChanges<TestState, TestMeta> = [
        ['user.name', 'Bob', {}],
        ['count', 10, {}],
      ]

      synchronizer(changes, state)

      expect(receivedChanges.length).toBeGreaterThan(0)
      expect(receivedChanges.some((c) => c[0] === 'user.name')).toBe(true)
      expect(receivedChanges.some((c) => c[0] === 'count')).toBe(true)
    })
  })

  describe('scoped listener receives only relevant changes', () => {
    it('user listener receives only user changes', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()
      const userChanges: any[] = []

      registry.register('user-listener', 'user', (changes) => {
        userChanges.push(...changes)
      })

      const synchronizer = createListenersSynchronizer(registry)
      const state = proxy<TestState>({
        user: { name: 'Alice', age: 30, address: { city: 'NYC' } },
        count: 0,
      })

      const changes: ArrayOfChanges<TestState, TestMeta> = [
        ['user.name', 'Bob', {}],
        ['count', 42, {}],
      ]

      synchronizer(changes, state)

      // Should only receive user.name change, not count
      const hasUserChange = userChanges.some((c) => c[0] === 'user.name')
      const hasCountChange = userChanges.some((c) => c[0] === 'count')
      expect(hasUserChange).toBe(true)
      expect(hasCountChange).toBe(false)
    })
  })

  describe('smart breakdown with nested listeners', () => {
    it('breaks down object changes for nested listeners', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()
      const nameChanges: any[] = []
      const ageChanges: any[] = []

      registry.register('name-listener', 'user.name', (changes, name) => {
        nameChanges.push({ changes, name })
      })
      registry.register('age-listener', 'user.age', (changes, age) => {
        ageChanges.push({ changes, age })
      })

      const synchronizer = createListenersSynchronizer(registry)
      const state = proxy<TestState>({
        user: { name: 'Alice', age: 30, address: { city: 'NYC' } },
        count: 0,
      })

      const changes: ArrayOfChanges<TestState, TestMeta> = [
        ['user', { name: 'Charlie', age: 35, address: { city: 'LA' } }, {}],
      ]

      synchronizer(changes, state)

      // Both listeners should be triggered
      expect(nameChanges.length).toBeGreaterThan(0)
      expect(ageChanges.length).toBeGreaterThan(0)
    })
  })

  describe('listener error handling', () => {
    it('continues execution after listener error', () => {
      const consoleError = vi.spyOn(console, 'error').mockImplementation(() => {})
      const registry = createListenersRegistry<TestState, TestMeta>()
      const successCalls: any[] = []

      registry.register('error-listener', null, () => {
        throw new Error('Listener error')
      })
      registry.register('success-listener', null, (changes) => {
        successCalls.push(changes)
      })

      const synchronizer = createListenersSynchronizer(registry)
      const state = proxy<TestState>({
        user: { name: 'Test', age: 30, address: { city: 'NYC' } },
        count: 0,
      })

      const changes: ArrayOfChanges<TestState, TestMeta> = [['count', 99, {}]]

      // Should not throw
      expect(() => synchronizer(changes, state)).not.toThrow()

      // Success listener should still execute
      expect(successCalls.length).toBeGreaterThan(0)

      consoleError.mockRestore()
    })
  })

  describe('multiple listeners for same path', () => {
    it('invokes all listeners for a path', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()
      const calls1: any[] = []
      const calls2: any[] = []

      registry.register('listener-1', 'count', (changes) => {
        calls1.push(changes)
      })
      registry.register('listener-2', 'count', (changes) => {
        calls2.push(changes)
      })

      const synchronizer = createListenersSynchronizer(registry)
      const state = proxy<TestState>({
        user: { name: 'Test', age: 30, address: { city: 'NYC' } },
        count: 0,
      })

      const changes: ArrayOfChanges<TestState, TestMeta> = [['count', 5, {}]]

      synchronizer(changes, state)

      expect(calls1.length).toBeGreaterThan(0)
      expect(calls2.length).toBeGreaterThan(0)
    })
  })

  describe('listener receives immutable snapshot', () => {
    it('listener receives snapshot not proxy', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()
      let receivedState: any

      registry.register('global', null, (_changes, state) => {
        receivedState = state
        // Try to mutate - should not affect original
        try {
          ;(state as any).count = 9999
        } catch {
          // Mutation might throw in strict mode
        }
      })

      const synchronizer = createListenersSynchronizer(registry)
      const state = proxy<TestState>({
        user: { name: 'Test', age: 30, address: { city: 'NYC' } },
        count: 100,
      })

      const changes: ArrayOfChanges<TestState, TestMeta> = [['count', 100, {}]]

      synchronizer(changes, state)

      expect(receivedState).toBeDefined()
      // Snapshot should be plain object with correct value
      expect(receivedState.count).toBe(100)
    })
  })

  describe('unregister listener', () => {
    it('stops invoking unregistered listener', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()
      const calls: any[] = []

      registry.register('temp-listener', 'count', (changes) => {
        calls.push(changes)
      })

      const synchronizer = createListenersSynchronizer(registry)
      const state = proxy<TestState>({
        user: { name: 'Test', age: 30, address: { city: 'NYC' } },
        count: 0,
      })

      // First call
      const changes1: ArrayOfChanges<TestState, TestMeta> = [['count', 1, {}]]
      synchronizer(changes1, state)
      expect(calls.length).toBe(1)

      // Unregister
      registry.unregister('temp-listener')

      // Second call - should not trigger listener
      const changes2: ArrayOfChanges<TestState, TestMeta> = [['count', 2, {}]]
      synchronizer(changes2, state)
      expect(calls.length).toBe(1) // Still 1, not 2
    })
  })

  describe('deeply nested listeners', () => {
    it('triggers deeply nested listeners correctly', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()
      const cityCalls: any[] = []

      registry.register('city-listener', 'user.address.city', (changes, city) => {
        cityCalls.push({ changes, city })
      })

      const synchronizer = createListenersSynchronizer(registry)
      const state = proxy<TestState>({
        user: { name: 'Alice', age: 30, address: { city: 'NYC' } },
        count: 0,
      })

      // Update state before calling synchronizer (since synchronizer reads current state)
      state.user = { name: 'Alice', age: 30, address: { city: 'LA' } }

      const changes: ArrayOfChanges<TestState, TestMeta> = [
        ['user', { name: 'Alice', age: 30, address: { city: 'LA' } }, {}],
      ]

      synchronizer(changes, state)

      expect(cityCalls.length).toBeGreaterThan(0)
      expect(cityCalls[0].city).toBe('LA')
    })
  })
})
