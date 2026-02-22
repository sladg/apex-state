import { describe, expect, it } from 'vitest'

import { deepClone } from '../../src/utils/deep-clone'

describe('deepClone', () => {
  describe('basic cloning', () => {
    it('should clone primitives', () => {
      expect(deepClone(42)).toBe(42)
      expect(deepClone('hello')).toBe('hello')
      expect(deepClone(true)).toBe(true)
      expect(deepClone(null)).toBe(null)
    })

    it('should clone flat objects', () => {
      const original = { a: 1, b: 'two', c: true }
      const cloned = deepClone(original)

      expect(cloned).toEqual(original)
      expect(cloned).not.toBe(original)
    })

    it('should clone arrays', () => {
      const original = [1, 2, { nested: true }]
      const cloned = deepClone(original)

      expect(cloned).toEqual(original)
      expect(cloned).not.toBe(original)
      expect(cloned[2]).not.toBe(original[2])
    })
  })

  describe('deep cloning — no mutation of original', () => {
    it('should not mutate original when cloned flat object is modified', () => {
      const original = { name: 'Alice', age: 30 }
      const cloned = deepClone(original)

      cloned.name = 'Bob'
      cloned.age = 25

      expect(original.name).toBe('Alice')
      expect(original.age).toBe(30)
    })

    it('should not mutate original when nested object is modified', () => {
      const original = {
        user: {
          profile: {
            bio: 'hello',
            settings: { theme: 'dark' },
          },
        },
      }
      const cloned = deepClone(original)

      cloned.user.profile.bio = 'mutated'
      cloned.user.profile.settings.theme = 'light'

      expect(original.user.profile.bio).toBe('hello')
      expect(original.user.profile.settings.theme).toBe('dark')
    })

    it('should not mutate original when array elements are modified', () => {
      const original = {
        items: [
          { id: 1, value: 'a' },
          { id: 2, value: 'b' },
        ],
      }
      const cloned = deepClone(original)

      cloned.items[0]!.value = 'mutated'
      cloned.items.push({ id: 3, value: 'c' })

      expect(original.items[0]!.value).toBe('a')
      expect(original.items).toHaveLength(2)
    })

    it('should not share nested references between original and clone', () => {
      const shared = { key: 'shared' }
      const original = { a: shared, b: shared }
      const cloned = deepClone(original)

      cloned.a.key = 'mutated'

      expect(original.a.key).toBe('shared')
      expect(original.b.key).toBe('shared')
    })
  })

  describe('getter preservation', () => {
    it('should preserve top-level getters', () => {
      const original = {
        firstName: 'John',
        lastName: 'Doe',
        get fullName() {
          return `${this.firstName} ${this.lastName}`
        },
      }
      const cloned = deepClone(original)

      expect(cloned.fullName).toBe('John Doe')

      // Getter is reactive — changing deps updates the computed value
      cloned.firstName = 'Jane'
      expect(cloned.fullName).toBe('Jane Doe')

      // Original is not affected
      expect(original.firstName).toBe('John')
      expect(original.fullName).toBe('John Doe')
    })

    it('should preserve nested getters', () => {
      const original = {
        stats: {
          score1: 10,
          score2: 20,
          get total() {
            return this.score1 + this.score2
          },
        },
      }
      const cloned = deepClone(original)

      expect(cloned.stats.total).toBe(30)

      cloned.stats.score1 = 100
      expect(cloned.stats.total).toBe(120)

      // Original untouched
      expect(original.stats.total).toBe(30)
    })

    it('should preserve getter as a descriptor, not a static value', () => {
      const original = {
        x: 1,
        get doubled() {
          return this.x * 2
        },
      }
      const cloned = deepClone(original)

      const descriptor = Object.getOwnPropertyDescriptor(cloned, 'doubled')
      expect(descriptor?.get).toBeDefined()
      expect(typeof descriptor?.get).toBe('function')
    })
  })

  describe('circular reference detection', () => {
    it('should throw on direct self-reference', () => {
      const obj: Record<string, unknown> = { a: 1 }
      obj['self'] = obj

      expect(() => deepClone(obj)).toThrow('[deepClone] Circular reference')
    })

    it('should throw on indirect circular reference', () => {
      const a: Record<string, unknown> = { name: 'a' }
      const b: Record<string, unknown> = { name: 'b', parent: a }
      a['child'] = b

      expect(() => deepClone(a)).toThrow('[deepClone] Circular reference')
    })

    it('should throw on deeply nested circular reference', () => {
      const root: Record<string, unknown> = {
        level1: { level2: { level3: {} } },
      }
      const l1 = root['level1'] as Record<
        string,
        Record<string, Record<string, unknown>>
      >
      l1['level2']!['level3']!['backToRoot'] = root

      expect(() => deepClone(root)).toThrow('[deepClone] Circular reference')
    })

    it('should include path in error message', () => {
      const deep: Record<string, unknown> = {}
      const obj: Record<string, unknown> = { nested: { deep } }
      deep['ref'] = obj

      expect(() => deepClone(obj)).toThrow('nested.deep.ref')
    })

    it('should allow non-circular shared references', () => {
      const shared = { key: 'shared' }
      const obj = { a: shared, b: shared }

      // Same object referenced twice (diamond) is NOT circular — should work
      expect(() => deepClone(obj)).not.toThrow()
    })
  })
})
