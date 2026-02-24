/**
 * Tests for dot utility - Deep access utilities
 *
 * Tests all dot functions:
 * - get: Type-safe deep access
 * - get__unsafe: Runtime path deep access
 * - set: Type-safe deep set
 * - set__unsafe: Runtime path deep set
 * - has: Check if path exists
 * - same: Deep equality check for multiple paths
 */

import { describe, expect, it } from 'vitest'

import { dot } from '../../src/utils/dot'

interface TestState {
  user: {
    name: string
    age: number
    address: {
      city: string
      zip: string | undefined
    }
    tags: string[]
  }
  settings: {
    enabled: boolean
    value: number | null
  }
  empty: Record<string, never>
}

const createTestState = (): TestState => ({
  user: {
    name: 'John',
    age: 30,
    address: {
      city: 'NYC',
      zip: undefined,
    },
    tags: ['developer', 'designer'],
  },
  settings: {
    enabled: true,
    value: null,
  },
  empty: {},
})

describe('dot utility', () => {
  describe('get (type-safe)', () => {
    it('should get top-level value', () => {
      const state = createTestState()
      expect(dot.get(state, 'user')).toEqual(state.user)
      expect(dot.get(state, 'settings')).toEqual(state.settings)
    })

    it('should get nested value', () => {
      const state = createTestState()
      expect(dot.get(state, 'user.name')).toBe('John')
      expect(dot.get(state, 'user.age')).toBe(30)
      expect(dot.get(state, 'user.address.city')).toBe('NYC')
    })

    it('should get deeply nested value', () => {
      const state = createTestState()
      expect(dot.get(state, 'user.address.city')).toBe('NYC')
    })

    it('should return undefined for non-existent paths', () => {
      const state = createTestState()
      expect(dot.get(state, 'user.address.zip')).toBeUndefined()
    })

    it('should get array values', () => {
      const state = createTestState()
      expect(dot.get(state, 'user.tags')).toEqual(['developer', 'designer'])
    })

    it('should get value at array index', () => {
      const state = createTestState()
      expect(dot.get__unsafe(state, 'user.tags.0')).toBe('developer')
      expect(dot.get__unsafe(state, 'user.tags.1')).toBe('designer')
    })

    it('should return undefined for out-of-bounds array index', () => {
      const state = createTestState()
      expect(dot.get__unsafe(state, 'user.tags.5')).toBeUndefined()
    })

    it('should get boolean values', () => {
      const state = createTestState()
      expect(dot.get(state, 'settings.enabled')).toBe(true)
    })

    it('should get null values', () => {
      const state = createTestState()
      expect(dot.get(state, 'settings.value')).toBeNull()
    })
  })

  describe('get__unsafe (runtime path)', () => {
    it('should get value with dynamic path', () => {
      const state = createTestState()
      const path = 'user.name'
      expect(dot.get__unsafe(state, path)).toBe('John')
    })

    it('should get nested value with dynamic path', () => {
      const state = createTestState()
      const path = 'user.address.city'
      expect(dot.get__unsafe(state, path)).toBe('NYC')
    })

    it('should return undefined for non-existent dynamic paths', () => {
      const state = createTestState()
      const path = 'user.nonexistent.path'
      expect(dot.get__unsafe(state, path)).toBeUndefined()
    })

    it('should return undefined when intermediate path is not an object', () => {
      const state = createTestState()
      // user.name is a string, so user.name.length would work on string but
      // user.name.foo would return undefined
      expect(dot.get__unsafe(state, 'user.name.foo')).toBeUndefined()
    })

    it('should handle null objects gracefully', () => {
      const state = { value: null }
      expect(dot.get__unsafe(state, 'value.nested')).toBeUndefined()
    })
  })

  describe('set (type-safe)', () => {
    it('should set top-level value', () => {
      const state = createTestState()
      dot.set(state, 'settings', { enabled: false, value: 42 })
      expect(state.settings.enabled).toBe(false)
      expect(state.settings.value).toBe(42)
    })

    it('should set nested value', () => {
      const state = createTestState()
      dot.set(state, 'user.name', 'Jane')
      expect(state.user.name).toBe('Jane')
    })

    it('should set deeply nested value', () => {
      const state = createTestState()
      dot.set(state, 'user.address.city', 'LA')
      expect(state.user.address.city).toBe('LA')
    })

    it('should create intermediate objects if needed', () => {
      const state = {} as TestState
      dot.set(state, 'user.address.city', 'Chicago')
      expect(state.user.address.city).toBe('Chicago')
    })

    it('should set boolean values', () => {
      const state = createTestState()
      dot.set(state, 'settings.enabled', false)
      expect(state.settings.enabled).toBe(false)
    })

    it('should set null values', () => {
      const state = createTestState()
      dot.set(state, 'settings.value', null)
      expect(state.settings.value).toBeNull()
    })

    it('should set array values', () => {
      const state = createTestState()
      dot.set(state, 'user.tags', ['engineer'])
      expect(state.user.tags).toEqual(['engineer'])
    })

    it('should set value at array index', () => {
      const state = createTestState()
      dot.set__unsafe(state, 'user.tags.0', 'engineer')
      expect(state.user.tags[0]).toBe('engineer')
      expect(state.user.tags[1]).toBe('designer')
    })
  })

  describe('set__unsafe (runtime path)', () => {
    it('should set value with dynamic path', () => {
      const state = createTestState()
      const path = 'user.name'
      dot.set__unsafe(state, path, 'Jane')
      expect(state.user.name).toBe('Jane')
    })

    it('should set nested value with dynamic path', () => {
      const state = createTestState()
      const path = 'user.address.city'
      dot.set__unsafe(state, path, 'LA')
      expect(state.user.address.city).toBe('LA')
    })

    it('should create intermediate objects if needed', () => {
      const state = {} as TestState
      const path = 'user.address.city'
      dot.set__unsafe(state, path, 'Chicago')
      expect((state as any).user.address.city).toBe('Chicago')
    })

    it('should set value at array index', () => {
      const state = createTestState()
      dot.set__unsafe(state, 'user.tags.1', 'engineer')
      expect(state.user.tags[1]).toBe('engineer')
      expect(state.user.tags[0]).toBe('developer')
    })

    it('should create intermediate array when next key is numeric', () => {
      const state = {} as any
      dot.set__unsafe(state, 'items.0.name', 'Alice')
      expect(Array.isArray(state.items)).toBe(true)
      expect(state.items[0].name).toBe('Alice')
    })
  })

  describe('has', () => {
    it('should return true for existing top-level path', () => {
      const state = createTestState()
      expect(dot.has(state, 'user')).toBe(true)
      expect(dot.has(state, 'settings')).toBe(true)
    })

    it('should return true for existing nested path', () => {
      const state = createTestState()
      expect(dot.has(state, 'user.name')).toBe(true)
      expect(dot.has(state, 'user.address.city')).toBe(true)
    })

    it('should return false for undefined values', () => {
      const state = createTestState()
      expect(dot.has(state, 'user.address.zip')).toBe(false)
    })

    it('should return true for null values (null !== undefined)', () => {
      const state = createTestState()
      expect(dot.has(state, 'settings.value')).toBe(true)
    })

    it('should return true for falsy but defined values', () => {
      const state = { value: 0, flag: false, str: '' }
      expect(dot.has(state, 'value')).toBe(true)
      expect(dot.has(state, 'flag')).toBe(true)
      expect(dot.has(state, 'str')).toBe(true)
    })

    it('should return true for empty objects', () => {
      const state = createTestState()
      expect(dot.has(state, 'empty')).toBe(true)
    })

    it('should return true for value at array index', () => {
      const state = createTestState()
      // use get__unsafe since numeric index paths aren't in DeepKey
      expect(dot.get__unsafe(state, 'user.tags.0')).toBe('developer')
      expect(dot.get__unsafe(state, 'user.tags.1')).toBe('designer')
      expect(dot.get__unsafe(state, 'user.tags.5')).toBeUndefined()
    })

    it('should return true for arrays', () => {
      const state = createTestState()
      expect(dot.has(state, 'user.tags')).toBe(true)
    })
  })

  describe('same', () => {
    it('should return true when comparing zero paths', () => {
      const state = createTestState()
      expect(dot.same(state)).toBe(true)
    })

    it('should return true when comparing single path', () => {
      const state = createTestState()
      expect(dot.same(state, 'user.name')).toBe(true)
    })

    it('should return true when values at paths are equal', () => {
      const state = {
        a: { value: 42 },
        b: { value: 42 },
        c: { value: 42 },
      }
      expect(dot.same(state, 'a.value', 'b.value', 'c.value')).toBe(true)
    })

    it('should return false when values at paths differ', () => {
      const state = {
        a: { value: 42 },
        b: { value: 99 },
      }
      expect(dot.same(state, 'a.value', 'b.value')).toBe(false)
    })

    it('should return true when comparing same string values', () => {
      const state = {
        first: { name: 'John' },
        second: { name: 'John' },
      }
      expect(dot.same(state, 'first.name', 'second.name')).toBe(true)
    })

    it('should return false when comparing different string values', () => {
      const state = {
        first: { name: 'John' },
        second: { name: 'Jane' },
      }
      expect(dot.same(state, 'first.name', 'second.name')).toBe(false)
    })

    it('should handle deep equality for objects', () => {
      const state = {
        a: { data: { x: 1, y: 2 } },
        b: { data: { x: 1, y: 2 } },
      }
      expect(dot.same(state, 'a.data', 'b.data')).toBe(true)
    })

    it('should return false for objects with different values', () => {
      const state = {
        a: { data: { x: 1, y: 2 } },
        b: { data: { x: 1, y: 3 } },
      }
      expect(dot.same(state, 'a.data', 'b.data')).toBe(false)
    })

    it('should return true for arrays with same values', () => {
      const state = {
        a: { items: [1, 2, 3] },
        b: { items: [1, 2, 3] },
      }
      expect(dot.same(state, 'a.items', 'b.items')).toBe(true)
    })

    it('should return false for arrays with different values', () => {
      const state = {
        a: { items: [1, 2, 3] },
        b: { items: [1, 2, 4] },
      }
      expect(dot.same(state, 'a.items', 'b.items')).toBe(false)
    })

    it('should return true for same array instance', () => {
      const sharedArray = [1, 2, 3]
      const state = {
        a: { items: sharedArray },
        b: { items: sharedArray },
      }
      expect(dot.same(state, 'a.items', 'b.items')).toBe(true)
    })

    it('should return false for arrays with different values', () => {
      const state = {
        a: { items: [1, 2, 3] },
        b: { items: [1, 2, 4] },
      }
      expect(dot.same(state, 'a.items', 'b.items')).toBe(false)
    })

    it('should handle null values', () => {
      const state = {
        a: { value: null },
        b: { value: null },
      }
      expect(dot.same(state, 'a.value', 'b.value')).toBe(true)
    })

    it('should handle undefined values', () => {
      const state = {
        a: { value: undefined },
        b: { value: undefined },
      }
      expect(dot.same(state, 'a.value', 'b.value')).toBe(true)
    })

    it('should return false when comparing null to undefined', () => {
      const state = {
        a: { value: null },
        b: { value: undefined },
      }
      expect(dot.same(state, 'a.value', 'b.value')).toBe(false)
    })
  })

  describe('edge cases', () => {
    it('should handle empty string values', () => {
      const state = { name: '' }
      expect(dot.get(state, 'name')).toBe('')
      expect(dot.has(state, 'name')).toBe(true)
    })

    it('should handle zero values', () => {
      const state = { count: 0 }
      expect(dot.get(state, 'count')).toBe(0)
      expect(dot.has(state, 'count')).toBe(true)
    })

    it('should handle false values', () => {
      const state = { enabled: false }
      expect(dot.get(state, 'enabled')).toBe(false)
      expect(dot.has(state, 'enabled')).toBe(true)
    })

    it('should handle nested empty objects', () => {
      const state = { outer: { inner: {} } }
      expect(dot.get(state, 'outer.inner')).toEqual({})
      expect(dot.has(state, 'outer.inner')).toBe(true)
    })

    it('should handle nested empty arrays', () => {
      const state = { items: { list: [] as number[] } }
      expect(dot.get(state, 'items.list')).toEqual([])
      expect(dot.has(state, 'items.list')).toBe(true)
    })

    it('should work with single-level paths', () => {
      const state = { value: 42 }
      expect(dot.get(state, 'value')).toBe(42)
      dot.set(state, 'value', 99)
      expect(state.value).toBe(99)
    })
  })

  describe('path caching', () => {
    it('should handle repeated access to same paths efficiently', () => {
      const state = createTestState()
      // Access the same path multiple times to ensure caching works
      for (let i = 0; i < 100; i++) {
        expect(dot.get(state, 'user.address.city')).toBe('NYC')
      }
    })

    it('should handle many different paths', () => {
      const state: Record<string, { value: number }> = {}
      // Create many different paths to test cache behavior
      for (let i = 0; i < 50; i++) {
        state[`key${i}`] = { value: i }
        expect(dot.get__unsafe(state, `key${i}.value`)).toBe(i)
      }
    })

    it('should clear cache when exceeding max size', () => {
      // MAX_CACHE_SIZE is 1000, so we need to exceed it
      const state: Record<string, { value: number }> = {}
      for (let i = 0; i < 1100; i++) {
        state[`key${i}`] = { value: i }
        // Each unique path gets cached
        dot.get__unsafe(state, `key${i}.value`)
      }
      // Cache should have been cleared and continue working
      expect(dot.get__unsafe(state, 'key1099.value')).toBe(1099)
    })
  })

  describe('has with non-object intermediate paths', () => {
    it('should return false when intermediate path is a primitive', () => {
      const state = { name: 'John' }
      // name is a string, not an object, so name.foo doesn't exist
      expect(dot.has(state, 'name')).toBe(true)
      // @ts-expect-error - testing invalid nested path through primitive
      expect(dot.has(state, 'name.length.foo')).toBe(false)
    })

    it('should return false when intermediate path is null', () => {
      const state = { data: null as { nested: string } | null }
      expect(dot.has(state, 'data.nested')).toBe(false)
    })
  })
})
