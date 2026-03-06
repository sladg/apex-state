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

  // ── Parent-path replacement (replaces entire subtree, no merge) ─────────

  it('should replace entire subtree when change targets a parent path', () => {
    // Setting g.123 = { p: { xyz: { data: 'new' } } } should replace the whole
    // g.123 node — g.123.p.abc must NOT survive in the result.
    const obj = {
      g: {
        '123': {
          p: {
            abc: { data: 'old-abc' },
            keep: 'should-be-gone',
            xyz: undefined as unknown as { data: string },
          },
        },
      },
    }
    const changes: ArrayOfChanges<typeof obj> = [
      ['g.123', { p: { xyz: { data: 'new-xyz' } } as never }, {}],
    ]

    const result = applyChangesToObject(obj, changes)

    // New key is present
    expect(result.g['123'].p['xyz']).toEqual({
      data: 'new-xyz',
    })
    // Old sibling key is gone — full replacement, not a merge
    expect(result.g['123'].p['abc']).toBeUndefined()
    expect(result.g['123'].p['keep']).toBeUndefined()
    // Original is unchanged
    expect(obj.g['123'].p.abc.data).toBe('old-abc')
  })

  it('should remove old keys when object is replaced with entirely different keys', () => {
    // g starts with numeric-like keys 1, 2, 3.
    // Replacing g with { a, b, c } must DELETE 1, 2, 3 — not merge them.
    const obj = {
      g: { '1': 'one', '2': 'two', '3': 'three' },
    }
    const changes: ArrayOfChanges<typeof obj> = [
      ['g', { a: 'alpha', b: 'beta', c: 'gamma' } as never, {}],
    ]

    const result = applyChangesToObject(obj, changes)

    // New keys are present
    const g = result.g as Record<string, unknown>
    expect(g['a']).toBe('alpha')
    expect(g['b']).toBe('beta')
    expect(g['c']).toBe('gamma')
    // Old keys are gone — full replacement, not merge
    expect(g['1']).toBeUndefined()
    expect(g['2']).toBeUndefined()
    expect(g['3']).toBeUndefined()
    // Original unchanged
    expect(obj.g['1']).toBe('one')
  })

  it('should remove old keys at 30 levels deep when replaced with entirely different keys', () => {
    // Build a 30-level deep object: root.g.1.2.3...  (alternating 1,2,3 keys)
    // Replace the deepest node with { a, b, c } and verify old keys are gone at every level.
    const DEPTH = 30

    // Build deep object: each level has key matching its depth index (1-based, cycling 1/2/3)
    type DeepObj = Record<string, unknown>
    const buildDeep = (depth: number): DeepObj => {
      if (depth === 0) return { '1': 'leaf-1', '2': 'leaf-2', '3': 'leaf-3' }
      const key = String((depth % 3) + 1)
      return { [key]: buildDeep(depth - 1) }
    }

    const obj = { g: buildDeep(DEPTH) }

    // Build the change path: g.2.1.3.2.1.3... (following the same cycling key pattern)
    const pathSegments = ['g']
    for (let d = DEPTH; d >= 1; d--) {
      pathSegments.push(String((d % 3) + 1))
    }
    const changePath = pathSegments.join('.')

    const changes: ArrayOfChanges<typeof obj> = [
      [changePath as never, { a: 'alpha', b: 'beta', c: 'gamma' } as never, {}],
    ]

    const result = applyChangesToObject(obj, changes)

    // Navigate to the replaced node
    const navigate = (node: unknown, segs: string[]): unknown =>
      segs.reduce(
        (cur, k) =>
          cur != null && typeof cur === 'object'
            ? (cur as Record<string, unknown>)[k]
            : undefined,
        node,
      )

    const replaced = navigate(result, pathSegments) as Record<string, unknown>

    // New keys present
    expect(replaced['a']).toBe('alpha')
    expect(replaced['b']).toBe('beta')
    expect(replaced['c']).toBe('gamma')
    // Old keys gone
    expect(replaced['1']).toBeUndefined()
    expect(replaced['2']).toBeUndefined()
    expect(replaced['3']).toBeUndefined()

    // Original is unchanged — leaf of original still has old keys
    const originalLeaf = navigate(obj, pathSegments) as Record<string, unknown>
    expect(originalLeaf['1']).toBe('leaf-1')
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
