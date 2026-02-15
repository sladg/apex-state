/**
 * WASM-028: Shadow state diff engine tests
 *
 * Tests the diff_changes() function that compares incoming changes against
 * shadow state and returns only genuine changes.
 */

import { beforeEach, describe, expect, it } from 'vitest'

import { type Change, wasm } from '../../src/wasm/bridge'

describe('WASM-028: diff_changes', () => {
  beforeEach(() => {
    wasm.pipelineReset()
  })

  describe('Primitives', () => {
    it('drops unchanged numbers', () => {
      wasm.shadowInit({ a: 42, b: 3.14 })

      const changes: Change[] = [
        { path: 'a', value: 42 },
        { path: 'b', value: 3.14 },
      ]

      const result = wasm.diffChanges(changes)
      expect(result).toEqual([])
    })

    it('keeps changed numbers', () => {
      wasm.shadowInit({ a: 42 })

      const changes: Change[] = [{ path: 'a', value: 99 }]

      const result = wasm.diffChanges(changes)
      expect(result).toEqual([{ path: 'a', value: 99 }])
    })

    it('drops unchanged booleans', () => {
      wasm.shadowInit({ flag: true })

      const changes: Change[] = [{ path: 'flag', value: true }]

      const result = wasm.diffChanges(changes)
      expect(result).toEqual([])
    })

    it('keeps changed booleans', () => {
      wasm.shadowInit({ flag: true })

      const changes: Change[] = [{ path: 'flag', value: false }]

      const result = wasm.diffChanges(changes)
      expect(result).toEqual([{ path: 'flag', value: false }])
    })

    it('drops unchanged strings', () => {
      wasm.shadowInit({ name: 'Alice' })

      const changes: Change[] = [{ path: 'name', value: 'Alice' }]

      const result = wasm.diffChanges(changes)
      expect(result).toEqual([])
    })

    it('keeps changed strings', () => {
      wasm.shadowInit({ name: 'Alice' })

      const changes: Change[] = [{ path: 'name', value: 'Bob' }]

      const result = wasm.diffChanges(changes)
      expect(result).toEqual([{ path: 'name', value: 'Bob' }])
    })

    it('drops unchanged null', () => {
      wasm.shadowInit({ val: null })

      const changes: Change[] = [{ path: 'val', value: null }]

      const result = wasm.diffChanges(changes)
      expect(result).toEqual([])
    })

    it('keeps type mismatch (null → number)', () => {
      wasm.shadowInit({ val: null })

      const changes: Change[] = [{ path: 'val', value: 0 }]

      const result = wasm.diffChanges(changes)
      expect(result).toEqual([{ path: 'val', value: 0 }])
    })
  })

  describe('Edge cases', () => {
    it('handles -0 vs +0 as different', () => {
      wasm.shadowInit({ x: 0 })

      const changes: Change[] = [{ path: 'x', value: -0 }]

      // In JavaScript, 0 === -0, but we want bitwise comparison
      // Rust diff should detect this difference
      const result = wasm.diffChanges(changes)
      expect(result).toEqual([{ path: 'x', value: -0 }])
    })

    it('handles NaN as always different', () => {
      // Even NaN != NaN in IEEE 754
      wasm.shadowInit({ x: NaN })

      const changes: Change[] = [{ path: 'x', value: NaN }]

      const result = wasm.diffChanges(changes)
      expect(result).toEqual([{ path: 'x', value: NaN }])
    })
  })

  describe('Objects and Arrays', () => {
    it('always keeps objects (no deep comparison)', () => {
      wasm.shadowInit({ obj: { a: 1, b: 2 } })

      // Same content, but should still be kept
      const changes: Change[] = [{ path: 'obj', value: { a: 1, b: 2 } }]

      const result = wasm.diffChanges(changes)
      expect(result).toEqual([{ path: 'obj', value: { a: 1, b: 2 } }])
    })

    it('always keeps arrays (no deep comparison)', () => {
      wasm.shadowInit({ arr: [1, 2, 3] })

      // Same content, but should still be kept
      const changes: Change[] = [{ path: 'arr', value: [1, 2, 3] }]

      const result = wasm.diffChanges(changes)
      expect(result).toEqual([{ path: 'arr', value: [1, 2, 3] }])
    })
  })

  describe('Nested paths', () => {
    it('drops unchanged nested values', () => {
      wasm.shadowInit({ user: { name: 'Alice', age: 30 } })

      const changes: Change[] = [
        { path: 'user.name', value: 'Alice' },
        { path: 'user.age', value: 30 },
      ]

      const result = wasm.diffChanges(changes)
      expect(result).toEqual([])
    })

    it('keeps changed nested values', () => {
      wasm.shadowInit({ user: { name: 'Alice', age: 30 } })

      const changes: Change[] = [{ path: 'user.age', value: 31 }]

      const result = wasm.diffChanges(changes)
      expect(result).toEqual([{ path: 'user.age', value: 31 }])
    })

    it('handles partial batch (some changed, some unchanged)', () => {
      wasm.shadowInit({ user: { name: 'Alice', age: 30, email: 'a@a.com' } })

      const changes: Change[] = [
        { path: 'user.name', value: 'Alice' }, // unchanged → drop
        { path: 'user.age', value: 31 }, // changed → keep
        { path: 'user.email', value: 'a@a.com' }, // unchanged → drop
      ]

      const result = wasm.diffChanges(changes)
      expect(result).toEqual([{ path: 'user.age', value: 31 }])
    })
  })

  describe('New paths', () => {
    it('keeps new paths (not in shadow state)', () => {
      wasm.shadowInit({ a: 1 })

      const changes: Change[] = [{ path: 'b', value: 2 }]

      const result = wasm.diffChanges(changes)
      expect(result).toEqual([{ path: 'b', value: 2 }])
    })

    it('keeps new nested paths', () => {
      wasm.shadowInit({ user: { name: 'Alice' } })

      const changes: Change[] = [
        { path: 'user.email', value: 'alice@example.com' },
      ]

      const result = wasm.diffChanges(changes)
      expect(result).toEqual([
        { path: 'user.email', value: 'alice@example.com' },
      ])
    })
  })

  describe('Performance: fast path', () => {
    it('returns empty array when all changes are no-ops', () => {
      const data: Record<string, unknown> = {}
      for (let i = 0; i < 1000; i++) {
        data[`field${i}`] = i
      }
      wasm.shadowInit(data)

      const changes: Change[] = Object.entries(data).map(([path, value]) => ({
        path,
        value,
      }))

      const result = wasm.diffChanges(changes)
      expect(result).toEqual([])
    })

    it('filters correctly with 10% change rate', () => {
      const data: Record<string, unknown> = {}
      for (let i = 0; i < 1000; i++) {
        data[`field${i}`] = i
      }
      wasm.shadowInit(data)

      const changes: Change[] = []
      for (let i = 0; i < 1000; i++) {
        const value = i % 10 === 0 ? i + 1 : i // 10% changed
        changes.push({ path: `field${i}`, value })
      }

      const result = wasm.diffChanges(changes)
      expect(result.length).toBe(100) // 10% of 1000
    })
  })

  describe('Empty input', () => {
    it('returns empty array for empty input', () => {
      wasm.shadowInit({ a: 1 })

      const result = wasm.diffChanges([])
      expect(result).toEqual([])
    })
  })

  describe('Integration with existing shadow state', () => {
    it('works after processChanges updates shadow state', () => {
      wasm.shadowInit({ counter: 0 })

      // Process a change (updates shadow state)
      wasm.processChanges([{ path: 'counter', value: 1 }])

      // Now diff against updated shadow state
      const result = wasm.diffChanges([{ path: 'counter', value: 1 }])

      // Should be dropped (matches updated shadow state)
      expect(result).toEqual([])
    })

    it('detects changes after shadow state update', () => {
      wasm.shadowInit({ counter: 0 })

      // Process a change (updates shadow state to 1)
      wasm.processChanges([{ path: 'counter', value: 1 }])

      // Now diff a different value
      const result = wasm.diffChanges([{ path: 'counter', value: 2 }])

      // Should be kept (different from shadow state)
      expect(result).toEqual([{ path: 'counter', value: 2 }])
    })
  })
})
