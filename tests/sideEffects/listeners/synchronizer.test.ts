/**
 * Listeners Synchronizer Tests (APEX-26)
 *
 * Tests listener invocation, error handling, and synchronizer integration.
 */

import { describe, it, expect, vi, beforeEach } from 'vitest'
import { proxy } from 'valtio'
import { createListenersSynchronizer } from '../../../src/pipeline/synchronizers/listeners'
import { createListenersRegistry } from '../../../src/sideEffects/listeners/registry'
import type { ArrayOfChanges, GenericMeta } from '../../../src/types'

interface TestState {
  user: {
    name: string
    age: number
    address: {
      city: string
      zip: string
    }
  }
  count: number
}

type TestMeta = GenericMeta

describe('createListenersSynchronizer', () => {
  let registry: ReturnType<typeof createListenersRegistry<TestState, TestMeta>>
  let state: TestState

  beforeEach(() => {
    registry = createListenersRegistry<TestState, TestMeta>()
    state = proxy({
      user: {
        name: 'Alice',
        age: 30,
        address: { city: 'NYC', zip: '10001' },
      },
      count: 0,
    })
  })

  describe('global listeners', () => {
    it('invokes global listener with all changes', () => {
      const receivedChanges: any[] = []
      const receivedState: any[] = []

      registry.register('global', null, (changes, state) => {
        receivedChanges.push(...changes)
        receivedState.push(state)
      })

      const synchronizer = createListenersSynchronizer(registry)
      const changes: ArrayOfChanges<TestState, TestMeta> = [
        ['user.name', 'Bob', {}],
        ['count', 10, {}],
      ]

      synchronizer(changes, state)

      expect(receivedChanges).toHaveLength(2)
      expect(receivedChanges[0][0]).toBe('user.name')
      expect(receivedChanges[1][0]).toBe('count')
      expect(receivedState).toHaveLength(1)
    })

    it('invokes multiple global listeners', () => {
      const calls1: any[] = []
      const calls2: any[] = []

      registry.register('global-1', null, (changes) => {
        calls1.push(changes.length)
      })
      registry.register('global-2', null, (changes) => {
        calls2.push(changes.length)
      })

      const synchronizer = createListenersSynchronizer(registry)
      const changes: ArrayOfChanges<TestState, TestMeta> = [
        ['count', 10, {}],
      ]

      synchronizer(changes, state)

      expect(calls1).toHaveLength(1)
      expect(calls2).toHaveLength(1)
    })

    it('passes immutable snapshot to global listeners', () => {
      let receivedState: any

      registry.register('global', null, (_changes, state) => {
        receivedState = state
      })

      const synchronizer = createListenersSynchronizer(registry)
      const changes: ArrayOfChanges<TestState, TestMeta> = [
        ['count', 10, {}],
      ]

      synchronizer(changes, state)

      // Should be snapshot, not proxy
      expect(receivedState).toBeDefined()
      expect(receivedState).not.toBe(state)
    })
  })

  describe('scoped listeners', () => {
    it('invokes scoped listener with relevant changes only', () => {
      const userChanges: any[] = []

      registry.register('user-listener', 'user', (changes) => {
        userChanges.push(...changes)
      })

      const synchronizer = createListenersSynchronizer(registry)
      const changes: ArrayOfChanges<TestState, TestMeta> = [
        ['user.name', 'Bob', {}],
        ['count', 10, {}],
      ]

      // Update state before invoking
      state.user.name = 'Bob'
      state.count = 10

      synchronizer(changes, state)

      // Should only receive user.name change, not count
      expect(userChanges).toHaveLength(1)
      expect(userChanges[0][0]).toBe('user.name')
    })

    it('passes scoped state to scoped listener', () => {
      let receivedState: any

      registry.register('user-listener', 'user', (_changes, userState) => {
        receivedState = userState
      })

      const synchronizer = createListenersSynchronizer(registry)
      const changes: ArrayOfChanges<TestState, TestMeta> = [
        ['user.name', 'Bob', {}],
      ]

      state.user.name = 'Bob'
      synchronizer(changes, state)

      expect(receivedState).toEqual({
        name: 'Bob',
        age: 30,
        address: { city: 'NYC', zip: '10001' },
      })
    })

    it('invokes nested scoped listeners correctly', () => {
      const nameChanges: any[] = []

      registry.register('name-listener', 'user.name', (changes) => {
        nameChanges.push(...changes)
      })

      const synchronizer = createListenersSynchronizer(registry)
      const changes: ArrayOfChanges<TestState, TestMeta> = [
        ['user', { name: 'Bob', age: 25, address: { city: 'LA', zip: '90001' } }, {}],
      ]

      state.user = { name: 'Bob', age: 25, address: { city: 'LA', zip: '90001' } }
      synchronizer(changes, state)

      // Should receive broken down user.name change
      expect(nameChanges.length).toBeGreaterThan(0)
      expect(nameChanges.some((c: any) => c[0] === 'user.name')).toBe(true)
    })

    it('does not invoke listener for unrelated changes', () => {
      const userCalls = vi.fn()

      registry.register('user-listener', 'user', userCalls)

      const synchronizer = createListenersSynchronizer(registry)
      const changes: ArrayOfChanges<TestState, TestMeta> = [
        ['count', 10, {}],
      ]

      state.count = 10
      synchronizer(changes, state)

      expect(userCalls).not.toHaveBeenCalled()
    })
  })

  describe('smart breakdown integration', () => {
    it('breaks down changes for listener triggering', () => {
      const nameChanges: any[] = []
      const ageChanges: any[] = []

      registry.register('name-listener', 'user.name', (changes) => {
        nameChanges.push(...changes)
      })
      registry.register('age-listener', 'user.age', (changes) => {
        ageChanges.push(...changes)
      })

      const synchronizer = createListenersSynchronizer(registry)
      const changes: ArrayOfChanges<TestState, TestMeta> = [
        ['user', { name: 'Bob', age: 25, address: { city: 'LA', zip: '90001' } }, {}],
      ]

      state.user = { name: 'Bob', age: 25, address: { city: 'LA', zip: '90001' } }
      synchronizer(changes, state)

      // Both listeners should be triggered due to breakdown
      expect(nameChanges.length).toBeGreaterThan(0)
      expect(ageChanges.length).toBeGreaterThan(0)
    })

    it('does not break down when no nested listeners', () => {
      const userCalls = vi.fn()

      registry.register('user-listener', 'user', userCalls)

      const synchronizer = createListenersSynchronizer(registry)
      const changes: ArrayOfChanges<TestState, TestMeta> = [
        ['user', { name: 'Bob', age: 25, address: { city: 'LA', zip: '90001' } }, {}],
      ]

      state.user = { name: 'Bob', age: 25, address: { city: 'LA', zip: '90001' } }
      synchronizer(changes, state)

      // Should be called once with original change
      expect(userCalls).toHaveBeenCalledTimes(1)
    })
  })

  describe('error handling', () => {
    it('catches and logs global listener errors', () => {
      const consoleError = vi.spyOn(console, 'error').mockImplementation(() => {})
      const errorListener = vi.fn().mockImplementation(() => {
        throw new Error('Listener error')
      })
      const successListener = vi.fn()

      registry.register('error-listener', null, errorListener)
      registry.register('success-listener', null, successListener)

      const synchronizer = createListenersSynchronizer(registry)
      const changes: ArrayOfChanges<TestState, TestMeta> = [
        ['count', 10, {}],
      ]

      // Should not throw - error should be caught
      expect(() => synchronizer(changes, state)).not.toThrow()

      expect(errorListener).toHaveBeenCalled()
      expect(successListener).toHaveBeenCalled()
      expect(consoleError).toHaveBeenCalled()

      consoleError.mockRestore()
    })

    it('catches and logs scoped listener errors', () => {
      const consoleError = vi.spyOn(console, 'error').mockImplementation(() => {})
      const errorListener = vi.fn().mockImplementation(() => {
        throw new Error('Scoped listener error')
      })

      registry.register('error-listener', 'user', errorListener)

      const synchronizer = createListenersSynchronizer(registry)
      const changes: ArrayOfChanges<TestState, TestMeta> = [
        ['user.name', 'Bob', {}],
      ]

      state.user.name = 'Bob'

      // Should not throw
      expect(() => synchronizer(changes, state)).not.toThrow()

      expect(errorListener).toHaveBeenCalled()
      expect(consoleError).toHaveBeenCalled()

      consoleError.mockRestore()
    })

    it('continues pipeline after listener error', () => {
      const consoleError = vi.spyOn(console, 'error').mockImplementation(() => {})
      const errorListener = vi.fn().mockImplementation(() => {
        throw new Error('Error')
      })

      registry.register('error-listener', null, errorListener)

      const synchronizer = createListenersSynchronizer(registry)
      const changes: ArrayOfChanges<TestState, TestMeta> = [
        ['count', 10, {}],
      ]

      const result = synchronizer(changes, state)

      // Should return changes unchanged
      expect(result).toBe(changes)

      consoleError.mockRestore()
    })
  })

  describe('synchronizer behavior', () => {
    it('returns original changes unchanged', () => {
      registry.register('global', null, vi.fn())

      const synchronizer = createListenersSynchronizer(registry)
      const changes: ArrayOfChanges<TestState, TestMeta> = [
        ['count', 10, {}],
      ]

      const result = synchronizer(changes, state)

      // Listeners do not add changes
      expect(result).toBe(changes)
    })

    it('works with empty changes array', () => {
      registry.register('global', null, vi.fn())

      const synchronizer = createListenersSynchronizer(registry)
      const changes: ArrayOfChanges<TestState, TestMeta> = []

      const result = synchronizer(changes, state)

      expect(result).toEqual([])
    })
  })

  describe('invocation order', () => {
    it('invokes listeners in registration order', () => {
      const order: number[] = []

      registry.register('listener-1', null, () => order.push(1))
      registry.register('listener-2', null, () => order.push(2))
      registry.register('listener-3', null, () => order.push(3))

      const synchronizer = createListenersSynchronizer(registry)
      const changes: ArrayOfChanges<TestState, TestMeta> = [
        ['count', 10, {}],
      ]

      synchronizer(changes, state)

      expect(order).toEqual([1, 2, 3])
    })
  })
})
