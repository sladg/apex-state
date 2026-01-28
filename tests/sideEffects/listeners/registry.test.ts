/**
 * Listeners Registry Tests (APEX-24)
 *
 * Tests listener registration, unregistration, and path lookup.
 */

import { describe, it, expect, vi } from 'vitest'
import { createListenersRegistry } from '../../../src/sideEffects/listeners/registry'
import type { GenericMeta } from '../../../src/types'

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

describe('ListenersRegistry', () => {
  describe('register', () => {
    it('registers global listener (key=null)', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()
      const fn = vi.fn()

      registry.register('global-1', null, fn)

      const globalListeners = registry.getGlobalListeners()
      expect(globalListeners).toHaveLength(1)
      expect(globalListeners[0].key).toBe(null)
      expect(globalListeners[0].fn).toBe(fn)
    })

    it('registers scoped listener', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()
      const fn = vi.fn()

      registry.register('user-listener', 'user', fn)

      const listeners = registry.getListeners('user')
      expect(listeners).toHaveLength(1)
      expect(listeners[0].key).toBe('user')
      expect(listeners[0].fn).toBe(fn)
    })

    it('registers nested scoped listener', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()
      const fn = vi.fn()

      registry.register('name-listener', 'user.name', fn)

      const listeners = registry.getListeners('user.name')
      expect(listeners).toHaveLength(1)
    })

    it('registers multiple listeners for same path', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()
      const fn1 = vi.fn()
      const fn2 = vi.fn()

      registry.register('listener-1', 'user', fn1)
      registry.register('listener-2', 'user', fn2)

      const listeners = registry.getListeners('user')
      expect(listeners).toHaveLength(2)
    })

    it('registers multiple global listeners', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()
      const fn1 = vi.fn()
      const fn2 = vi.fn()

      registry.register('global-1', null, fn1)
      registry.register('global-2', null, fn2)

      const globalListeners = registry.getGlobalListeners()
      expect(globalListeners).toHaveLength(2)
    })
  })

  describe('unregister', () => {
    it('unregisters global listener', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()
      const fn = vi.fn()

      registry.register('global-1', null, fn)
      expect(registry.getGlobalListeners()).toHaveLength(1)

      registry.unregister('global-1')
      expect(registry.getGlobalListeners()).toHaveLength(0)
    })

    it('unregisters scoped listener', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()
      const fn = vi.fn()

      registry.register('user-listener', 'user', fn)
      expect(registry.getListeners('user')).toHaveLength(1)

      registry.unregister('user-listener')
      expect(registry.getListeners('user')).toHaveLength(0)
    })

    it('unregisters one of multiple listeners', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()
      const fn1 = vi.fn()
      const fn2 = vi.fn()

      registry.register('listener-1', 'user', fn1)
      registry.register('listener-2', 'user', fn2)
      expect(registry.getListeners('user')).toHaveLength(2)

      registry.unregister('listener-1')
      expect(registry.getListeners('user')).toHaveLength(1)
    })

    it('handles unregister of non-existent id', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()

      // Should not throw
      expect(() => registry.unregister('non-existent')).not.toThrow()
    })
  })

  describe('getListeners', () => {
    it('returns empty array for path with no listeners', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()

      const listeners = registry.getListeners('user')
      expect(listeners).toEqual([])
    })

    it('returns listeners for specific path', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()
      const fn1 = vi.fn()
      const fn2 = vi.fn()

      registry.register('listener-1', 'user', fn1)
      registry.register('listener-2', 'user.name', fn2)

      const userListeners = registry.getListeners('user')
      expect(userListeners).toHaveLength(1)
      expect(userListeners[0].fn).toBe(fn1)

      const nameListeners = registry.getListeners('user.name')
      expect(nameListeners).toHaveLength(1)
      expect(nameListeners[0].fn).toBe(fn2)
    })
  })

  describe('getGlobalListeners', () => {
    it('returns empty array when no global listeners', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()

      expect(registry.getGlobalListeners()).toEqual([])
    })

    it('returns all global listeners', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()
      const fn1 = vi.fn()
      const fn2 = vi.fn()

      registry.register('global-1', null, fn1)
      registry.register('global-2', null, fn2)

      const globalListeners = registry.getGlobalListeners()
      expect(globalListeners).toHaveLength(2)
    })
  })

  describe('hasListenerForPath', () => {
    it('returns false for path with no listeners', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()

      expect(registry.hasListenerForPath('user')).toBe(false)
    })

    it('returns true for path with listener', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()

      registry.register('user-listener', 'user', vi.fn())

      expect(registry.hasListenerForPath('user')).toBe(true)
    })

    it('returns true for nested path with listener', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()

      registry.register('name-listener', 'user.name', vi.fn())

      expect(registry.hasListenerForPath('user.name')).toBe(true)
      expect(registry.hasListenerForPath('user')).toBe(false)
    })
  })

  describe('hasListenerForNestedPath', () => {
    it('returns false when no nested listeners exist', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()

      registry.register('user-listener', 'user', vi.fn())

      expect(registry.hasListenerForNestedPath('user')).toBe(false)
    })

    it('returns true when nested listener exists', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()

      registry.register('name-listener', 'user.name', vi.fn())

      expect(registry.hasListenerForNestedPath('user')).toBe(true)
    })

    it('returns true for deeply nested listeners', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()

      registry.register('city-listener', 'user.address.city', vi.fn())

      expect(registry.hasListenerForNestedPath('user')).toBe(true)
      expect(registry.hasListenerForNestedPath('user.address')).toBe(true)
      expect(registry.hasListenerForNestedPath('user.address.city')).toBe(false)
    })

    it('does not match sibling paths', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()

      registry.register('name-listener', 'user.name', vi.fn())

      expect(registry.hasListenerForNestedPath('count')).toBe(false)
    })
  })

  describe('path indexing', () => {
    it('maintains path index correctly', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()

      registry.register('name-listener', 'user.name', vi.fn())
      registry.register('age-listener', 'user.age', vi.fn())

      expect(registry.hasListenerForPath('user.name')).toBe(true)
      expect(registry.hasListenerForPath('user.age')).toBe(true)

      registry.unregister('name-listener')

      expect(registry.hasListenerForPath('user.name')).toBe(false)
      expect(registry.hasListenerForPath('user.age')).toBe(true)
    })
  })
})
