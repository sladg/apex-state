/**
 * Tests for deep access utilities
 *
 * Verifies dot.get, dot.set, and dot.has functions work correctly
 * with type-safe paths
 */

import { describe, expect, it } from 'vitest'

import { dot } from '~/utils/dot'
import { guard } from '~/utils/guards'
import { _ } from '~/utils/hashKey'

describe('Deep Access Utilities', () => {
  describe('dot.get', () => {
    it('should get top-level property', () => {
      const obj = { name: 'John', age: 30 }
      expect(dot.get(obj, 'name')).toBe('John')
      expect(dot.get(obj, 'age')).toBe(30)
    })

    it('should get nested property', () => {
      const obj = {
        user: {
          profile: {
            name: 'Alice',
          },
        },
      }
      expect(dot.get(obj, 'user.profile.name')).toBe('Alice')
    })

    it('should return undefined for missing property', () => {
      const obj = { a: 1 }
      // @ts-expect-error - testing runtime behavior with invalid path
      expect(dot.get(obj, 'b')).toBeUndefined()
      // @ts-expect-error - testing runtime behavior with invalid path
      expect(dot.get(obj, 'a.b.c')).toBeUndefined()
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
      expect(dot.get(obj, 'level1.level2.level3.level4.value')).toBe('deep')
    })

    it('should handle objects with null values', () => {
      const obj = { data: null }
      expect(dot.get(obj, 'data')).toBeNull()
    })
  })

  describe('dot.set', () => {
    it('should set top-level property', () => {
      const obj = { name: 'John', age: 30 }
      dot.set(obj, 'name', 'Jane')
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
      dot.set(obj, 'user.profile.name', 'Bob')
      expect(obj.user.profile.name).toBe('Bob')
    })

    it('should create intermediate objects if missing', () => {
      const obj: { user?: { profile?: { name?: string } } } = {}
      dot.set(obj, 'user.profile.name', 'value')
      expect(obj.user?.profile?.name).toBe('value')
    })

    it('should handle deeply nested paths', () => {
      const obj: {
        level1?: {
          level2?: { level3?: { level4?: { value?: string } } }
        }
      } = {}
      dot.set(obj, 'level1.level2.level3.level4.value', 'deep')
      expect(obj.level1?.level2?.level3?.level4?.value).toBe('deep')
    })

    it('should overwrite existing values', () => {
      const obj = {
        data: {
          value: 'old',
        },
      }
      dot.set(obj, 'data.value', 'new')
      expect(obj.data.value).toBe('new')
    })

    it('should set numeric values', () => {
      const obj = { count: 0 }
      dot.set(obj, 'count', 42)
      expect(obj.count).toBe(42)
    })

    it('should set object values', () => {
      const obj: { data?: { nested: string } } = {}
      const newValue = { nested: 'object' }
      dot.set(obj, 'data', newValue)
      expect(obj.data).toEqual(newValue)
    })
  })

  describe('dot.has', () => {
    it('should return true for existing top-level property', () => {
      const obj = { name: 'John', age: 30 }
      expect(dot.has(obj, 'name')).toBe(true)
      expect(dot.has(obj, 'age')).toBe(true)
    })

    it('should return false for missing top-level property', () => {
      const obj = { name: 'John' }
      // @ts-expect-error - testing runtime behavior
      expect(dot.has(obj, 'age')).toBe(false)
    })

    it('should return true for existing nested property', () => {
      const obj = {
        user: {
          profile: {
            name: 'Alice',
          },
        },
      }
      expect(dot.has(obj, 'user.profile.name')).toBe(true)
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
      expect(dot.has(obj, 'user.profile.age')).toBe(false)
      // @ts-expect-error - testing runtime behavior
      expect(dot.has(obj, 'user.settings')).toBe(false)
    })

    it('should handle null and undefined values', () => {
      const obj = { nullValue: null, undefinedValue: undefined }
      // null is actually returned by lodash get (not undefined)
      expect(dot.has(obj, 'nullValue')).toBe(true)
      expect(dot.has(obj, 'undefinedValue')).toBe(false)
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
      expect(dot.has(obj, 'a.b.c.d')).toBe(true)
      // @ts-expect-error - testing runtime behavior
      expect(dot.has(obj, 'a.b.c.e')).toBe(false)
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

      expect(dot.get(obj, 'value')).toBe('original')
      dot.set(obj, 'value', 'updated')
      expect(obj.value).toBe('updated')
      expect(dot.has(obj, 'value')).toBe(true)
    })
  })

  describe('guard.dynamicPath (dynamic key validation)', () => {
    it('throws error when bracket notation is passed to guard.dynamicPath', () => {
      expect(() => guard.dynamicPath('nested.[*].value')).toThrow(
        /bracket notation/,
      )
    })

    it('allows normal paths without hash keys', () => {
      expect(() => guard.dynamicPath('nested.value')).not.toThrow()
      expect(() => guard.dynamicPath('user.profile.name')).not.toThrow()
    })

    it('works with dot.get/set/has when paths are valid', () => {
      const obj = { nested: { value: 'test' } }
      expect(dot.get(obj, 'nested.value')).toBe('test')
      expect(() => dot.set(obj, 'nested.value', 'updated')).not.toThrow()
      expect(dot.has(obj, 'nested.value')).toBe(true)
    })
  })

  describe('_ (hash key function)', () => {
    it('returns the concrete ID typed as HASH_KEY', () => {
      const result = _('l1')
      expect(result).toBe('l1')
    })

    it('returns the input parameter unchanged', () => {
      expect(_('l1')).toBe('l1')
      expect(_('any-string')).toBe('any-string')
      expect(_('123')).toBe('123')
      expect(_('')).toBe('')
    })

    it('works in template strings with concrete IDs', () => {
      const path = `portfolio.books.b1.legs.${_('l1')}.notional`
      expect(path).toBe('portfolio.books.b1.legs.l1.notional')
    })

    it('works with multiple hash keys in template', () => {
      const path = `users.${_('u1')}.posts.${_('p1')}.comments.${_('c1')}.text`
      expect(path).toBe('users.u1.posts.p1.comments.c1.text')
    })

    it('works with complex nested paths', () => {
      const path = `portfolio.books.${_('b1')}.products.${_('p1')}.legGroups.${_('g1')}.legs.${_('l1')}.notional`
      expect(path).toBe(
        'portfolio.books.b1.products.p1.legGroups.g1.legs.l1.notional',
      )
    })
  })
})
