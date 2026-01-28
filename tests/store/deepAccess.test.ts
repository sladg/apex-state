/**
 * Tests for deep access utilities
 *
 * Verifies deepGet, deepSet, and deepHas functions work correctly
 * with type-safe paths
 */

import { describe, expect, it } from 'vitest'

import { deepGet, deepHas, deepSet } from '../../src/store/utils/deepAccess'

describe('Deep Access Utilities', () => {
  describe('deepGet', () => {
    it('should get top-level property', () => {
      const obj = { name: 'John', age: 30 }
      expect(deepGet(obj, 'name')).toBe('John')
      expect(deepGet(obj, 'age')).toBe(30)
    })

    it('should get nested property', () => {
      const obj = {
        user: {
          profile: {
            name: 'Alice',
          },
        },
      }
      expect(deepGet(obj, 'user.profile.name')).toBe('Alice')
    })

    it('should return undefined for missing property', () => {
      const obj = { a: 1 }
      // @ts-expect-error - testing runtime behavior with invalid path
      expect(deepGet(obj, 'b')).toBeUndefined()
      // @ts-expect-error - testing runtime behavior with invalid path
      expect(deepGet(obj, 'a.b.c')).toBeUndefined()
    })

    it('should handle deeply nested objects', () => {
      const obj = {
        level1: {
          level2: {
            level3: {
              level4: {
                value: 'deep',
              },
            },
          },
        },
      }
      expect(deepGet(obj, 'level1.level2.level3.level4.value')).toBe('deep')
    })

    it('should handle objects with null values', () => {
      const obj = { data: null as any }
      expect(deepGet(obj, 'data')).toBeNull()
    })
  })

  describe('deepSet', () => {
    it('should set top-level property', () => {
      const obj = { name: 'John', age: 30 }
      deepSet(obj, 'name', 'Jane')
      expect(obj.name).toBe('Jane')
    })

    it('should set nested property', () => {
      const obj = {
        user: {
          profile: {
            name: 'Alice',
          },
        },
      }
      deepSet(obj, 'user.profile.name', 'Bob')
      expect(obj.user.profile.name).toBe('Bob')
    })

    it('should create intermediate objects if missing', () => {
      const obj: any = {}
      deepSet(obj, 'a.b.c', 'value')
      expect(obj.a.b.c).toBe('value')
    })

    it('should handle deeply nested paths', () => {
      const obj: any = {}
      deepSet(obj, 'level1.level2.level3.level4.value', 'deep')
      expect(obj.level1.level2.level3.level4.value).toBe('deep')
    })

    it('should overwrite existing values', () => {
      const obj = {
        data: {
          value: 'old',
        },
      }
      deepSet(obj, 'data.value', 'new')
      expect(obj.data.value).toBe('new')
    })

    it('should set numeric values', () => {
      const obj = { count: 0 }
      deepSet(obj, 'count', 42)
      expect(obj.count).toBe(42)
    })

    it('should set object values', () => {
      const obj: any = {}
      const newValue = { nested: 'object' }
      deepSet(obj, 'data', newValue)
      expect(obj.data).toEqual(newValue)
    })
  })

  describe('deepHas', () => {
    it('should return true for existing top-level property', () => {
      const obj = { name: 'John', age: 30 }
      expect(deepHas(obj, 'name')).toBe(true)
      expect(deepHas(obj, 'age')).toBe(true)
    })

    it('should return false for missing top-level property', () => {
      const obj = { name: 'John' }
      // @ts-expect-error - testing runtime behavior
      expect(deepHas(obj, 'age')).toBe(false)
    })

    it('should return true for existing nested property', () => {
      const obj = {
        user: {
          profile: {
            name: 'Alice',
          },
        },
      }
      expect(deepHas(obj, 'user.profile.name')).toBe(true)
    })

    it('should return false for missing nested property', () => {
      const obj = {
        user: {
          profile: {
            name: 'Alice',
          },
        },
      }
      // @ts-expect-error - testing runtime behavior
      expect(deepHas(obj, 'user.profile.age')).toBe(false)
      // @ts-expect-error - testing runtime behavior
      expect(deepHas(obj, 'user.settings')).toBe(false)
    })

    it('should handle null and undefined values', () => {
      const obj = { nullValue: null, undefinedValue: undefined }
      // null is actually returned by lodash get (not undefined)
      expect(deepHas(obj, 'nullValue')).toBe(true)
      expect(deepHas(obj, 'undefinedValue')).toBe(false)
    })

    it('should work with deeply nested paths', () => {
      const obj = {
        a: {
          b: {
            c: {
              d: 'value',
            },
          },
        },
      }
      expect(deepHas(obj, 'a.b.c.d')).toBe(true)
      // @ts-expect-error - testing runtime behavior
      expect(deepHas(obj, 'a.b.c.e')).toBe(false)
    })
  })

  describe('Integration with valtio proxy', () => {
    it('should work with proxy objects', () => {
      // This test verifies the utilities work with proxy-like objects
      // In real usage, these would be valtio proxies
      const obj = new Proxy(
        { value: 'original' },
        {
          get: (target, prop) => target[prop as keyof typeof target],
          set: (target, prop, value) => {
            target[prop as keyof typeof target] = value
            return true
          },
        },
      )

      expect(deepGet(obj, 'value')).toBe('original')
      deepSet(obj, 'value', 'updated')
      expect(obj.value).toBe('updated')
      expect(deepHas(obj, 'value')).toBe(true)
    })
  })
})
