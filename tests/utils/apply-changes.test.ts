/**
 * Tests for applyChangesToObject utility
 *
 * Tests functionality and measures performance for reference.
 */

import { describe, expect, it } from 'vitest'

import type { ArrayOfChanges } from '~/types'
import { applyChangesToObject } from '~/utils/apply-changes'

describe('applyChangesToObject', () => {
  it('should apply single change and return new object', () => {
    const obj = { name: 'Alice' }
    const changes: ArrayOfChanges<typeof obj> = [['name', 'Bob', {}]]

    const result = applyChangesToObject(obj, changes)

    expect(result.name).toBe('Bob')
    expect(result).not.toBe(obj) // Different reference
    expect(obj.name).toBe('Alice') // Original unchanged
  })

  it('should apply multiple changes', () => {
    const obj = { a: 1, b: 2, c: 3 }
    const changes: ArrayOfChanges<typeof obj> = [
      ['a', 10, {}],
      ['b', 20, {}],
      ['c', 30, {}],
    ]

    const result = applyChangesToObject(obj, changes)

    expect(result).toEqual({ a: 10, b: 20, c: 30 })
    expect(obj).toEqual({ a: 1, b: 2, c: 3 }) // Original unchanged
  })

  it('should apply nested path changes', () => {
    const obj = {
      user: {
        profile: {
          name: 'Alice',
          age: 30,
        },
      },
    }
    const changes: ArrayOfChanges<typeof obj> = [
      ['user.profile.name', 'Bob', {}],
      ['user.profile.age', 31, {}],
    ]

    const result = applyChangesToObject(obj, changes)

    expect(result.user.profile.name).toBe('Bob')
    expect(result.user.profile.age).toBe(31)
    expect(obj.user.profile.name).toBe('Alice') // Original unchanged
  })

  it('should handle empty changes array', () => {
    const obj = { value: 42 }
    const changes: ArrayOfChanges<typeof obj> = []

    const result = applyChangesToObject(obj, changes)

    expect(result).toEqual({ value: 42 })
    expect(result).not.toBe(obj) // Still a new object
  })

  it('should handle object values', () => {
    const obj = { data: { nested: 'old', extra: false } }
    const changes: ArrayOfChanges<typeof obj> = [
      ['data', { nested: 'new', extra: true }, {}],
    ]

    const result = applyChangesToObject(obj, changes)

    expect(result.data).toEqual({ nested: 'new', extra: true })
  })

  it('should handle array values', () => {
    const obj = { items: [1, 2, 3] }
    const changes: ArrayOfChanges<typeof obj> = [['items', [4, 5, 6], {}]]

    const result = applyChangesToObject(obj, changes)

    expect(result.items).toEqual([4, 5, 6])
  })

  it('should handle nested path changes at depth', () => {
    const obj = {
      level1: {
        level2: {
          level3: {
            value: 0,
          },
        },
      },
    }
    const changes: ArrayOfChanges<typeof obj> = [
      ['level1.level2.level3.value', 999, {}],
    ]

    const result = applyChangesToObject(obj, changes)

    expect(result.level1.level2.level3.value).toBe(999)
    expect(obj.level1.level2.level3.value).toBe(0) // Original unchanged
  })

  // ── Proxy compatibility (structuredClone cannot clone Proxy objects) ──────

  it('should work when the root object is a Proxy', () => {
    const inner = { name: 'Alice', score: 10 }
    const proxy = new Proxy(inner, {})
    const changes: ArrayOfChanges<typeof inner> = [['name', 'Bob', {}]]

    // structuredClone(proxy) throws — applyChangesToObject must handle this
    expect(() => applyChangesToObject(proxy, changes)).not.toThrow()
    const result = applyChangesToObject(proxy, changes)
    expect(result.name).toBe('Bob')
    expect(result.score).toBe(10)
  })

  it('should work when a nested array is a Proxy', () => {
    const obj = { items: new Proxy([1, 2, 3], {}), label: 'list' }
    const changes: ArrayOfChanges<typeof obj> = [['label', 'updated', {}]]

    // structuredClone throws when encountering a nested Proxy-wrapped array
    expect(() => applyChangesToObject(obj, changes)).not.toThrow()
    const result = applyChangesToObject(obj, changes)
    expect(result.label).toBe('updated')
    expect(result.items).toEqual([1, 2, 3])
  })

  it('should work when a nested object is a Proxy', () => {
    const obj = {
      user: new Proxy({ name: 'Alice', age: 30 }, {}),
      active: true,
    }
    const changes: ArrayOfChanges<typeof obj> = [['active', false, {}]]

    expect(() => applyChangesToObject(obj, changes)).not.toThrow()
    const result = applyChangesToObject(obj, changes)
    expect(result.active).toBe(false)
    expect(result.user.name).toBe('Alice')
  })
})
