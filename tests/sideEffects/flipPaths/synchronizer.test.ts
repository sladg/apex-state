/**
 * Tests for flipPaths synchronizer
 *
 * Validates change propagation, value flipping (boolean/enum), metadata flags,
 * and infinite loop prevention.
 */

import { describe, test, expect } from 'vitest'
import { createFlipPathsSynchronizer } from '../../../src/pipeline/synchronizers/flipPaths'
import { createFlipPathsRegistry } from '../../../src/sideEffects/flipPaths/registry'
import type { ArrayOfChanges, GenericMeta } from '../../../src/types'

interface BooleanState {
  a: boolean
  b: boolean
  c: boolean
  d: boolean
}

type ThemeMode = 'light' | 'dark'

interface EnumState {
  mode: ThemeMode
  theme: ThemeMode
  background: ThemeMode
}

describe('createFlipPathsSynchronizer', () => {
  describe('Boolean Flipping', () => {
    test('should flip boolean true to false', () => {
      const registry = createFlipPathsRegistry<BooleanState>()
      registry.register('ab', 'a', 'b')

      const synchronizer = createFlipPathsSynchronizer<
        BooleanState,
        GenericMeta
      >(registry)

      const changes: ArrayOfChanges<BooleanState, GenericMeta> = [
        ['a', true, {}],
      ]

      const result = synchronizer(changes, { a: false, b: true, c: false, d: false })

      // Should have original change plus flip change
      expect(result.length).toBe(2)
      expect(result[0]).toEqual(['a', true, {}])
      expect(result[1][0]).toBe('b')
      expect(result[1][1]).toBe(false) // Flipped from true
      expect(result[1][2].isFlipPathChange).toBe(true)
    })

    test('should flip boolean false to true', () => {
      const registry = createFlipPathsRegistry<BooleanState>()
      registry.register('ab', 'a', 'b')

      const synchronizer = createFlipPathsSynchronizer<
        BooleanState,
        GenericMeta
      >(registry)

      const changes: ArrayOfChanges<BooleanState, GenericMeta> = [
        ['a', false, {}],
      ]

      const result = synchronizer(changes, { a: true, b: false, c: false, d: false })

      expect(result[1][1]).toBe(true) // Flipped from false
    })

    test('should work bidirectionally', () => {
      const registry = createFlipPathsRegistry<BooleanState>()
      registry.register('ab', 'a', 'b')

      const synchronizer = createFlipPathsSynchronizer<
        BooleanState,
        GenericMeta
      >(registry)

      // Change 'b' instead of 'a'
      const changes: ArrayOfChanges<BooleanState, GenericMeta> = [
        ['b', true, {}],
      ]

      const result = synchronizer(changes, { a: true, b: false, c: false, d: false })

      // Should flip 'a' to false
      expect(result.length).toBe(2)
      const flipChange = result.find((c) => c[0] === 'a')
      expect(flipChange?.[1]).toBe(false)
      expect(flipChange?.[2].isFlipPathChange).toBe(true)
    })
  })

  describe('Enum Flipping', () => {
    test('should swap enum values', () => {
      const registry = createFlipPathsRegistry<EnumState>()
      registry.register('mode-theme', 'mode', 'theme')

      const synchronizer = createFlipPathsSynchronizer<EnumState, GenericMeta>(
        registry
      )

      // Initial: mode='light', theme='dark'
      // Change mode to 'dark' → theme should become 'light' (swap)
      const changes: ArrayOfChanges<EnumState, GenericMeta> = [
        ['mode', 'dark', {}],
      ]

      const result = synchronizer(changes, {
        mode: 'light',
        theme: 'dark',
        background: 'light',
      })

      expect(result.length).toBe(2)
      const flipChange = result.find((c) => c[0] === 'theme')
      expect(flipChange?.[1]).toBe('light') // Gets old value of 'mode'
    })

    test('should swap enum values bidirectionally', () => {
      const registry = createFlipPathsRegistry<EnumState>()
      registry.register('mode-theme', 'mode', 'theme')

      const synchronizer = createFlipPathsSynchronizer<EnumState, GenericMeta>(
        registry
      )

      // Change theme instead
      const changes: ArrayOfChanges<EnumState, GenericMeta> = [
        ['theme', 'light', {}],
      ]

      const result = synchronizer(changes, {
        mode: 'light',
        theme: 'dark',
        background: 'light',
      })

      const flipChange = result.find((c) => c[0] === 'mode')
      expect(flipChange?.[1]).toBe('dark') // Gets old value of 'theme'
    })
  })

  describe('Multiple Flip Pairs', () => {
    test('should handle multiple flip pairs', () => {
      const registry = createFlipPathsRegistry<BooleanState>()
      registry.register('ab', 'a', 'b')
      registry.register('cd', 'c', 'd')

      const synchronizer = createFlipPathsSynchronizer<
        BooleanState,
        GenericMeta
      >(registry)

      const changes: ArrayOfChanges<BooleanState, GenericMeta> = [
        ['a', true, {}],
        ['c', false, {}],
      ]

      const result = synchronizer(changes, { a: false, b: true, c: true, d: false })

      // Should have 2 original + 2 flip changes
      expect(result.length).toBe(4)

      const flipB = result.find((c) => c[0] === 'b' && c[2].isFlipPathChange)
      const flipD = result.find((c) => c[0] === 'd' && c[2].isFlipPathChange)

      expect(flipB?.[1]).toBe(false)
      expect(flipD?.[1]).toBe(true)
    })
  })

  describe('Metadata Handling', () => {
    test('should set isFlipPathChange flag', () => {
      const registry = createFlipPathsRegistry<BooleanState>()
      registry.register('ab', 'a', 'b')

      const synchronizer = createFlipPathsSynchronizer<
        BooleanState,
        GenericMeta
      >(registry)

      const changes: ArrayOfChanges<BooleanState, GenericMeta> = [
        ['a', true, { sender: 'test' }],
      ]

      const result = synchronizer(changes, { a: false, b: true, c: false, d: false })

      const flipChange = result.find((c) => c[0] === 'b')
      expect(flipChange?.[2].isFlipPathChange).toBe(true)
      expect(flipChange?.[2].sender).toBe('test') // Preserves other metadata
    })

    test('should preserve all existing metadata', () => {
      const registry = createFlipPathsRegistry<BooleanState>()
      registry.register('ab', 'a', 'b')

      const synchronizer = createFlipPathsSynchronizer<
        BooleanState,
        GenericMeta
      >(registry)

      const changes: ArrayOfChanges<BooleanState, GenericMeta> = [
        ['a', true, { sender: 'user-123', isProgramaticChange: false }],
      ]

      const result = synchronizer(changes, { a: false, b: true, c: false, d: false })

      const flipChange = result.find((c) => c[0] === 'b')
      expect(flipChange?.[2].sender).toBe('user-123')
      expect(flipChange?.[2].isProgramaticChange).toBe(false)
      expect(flipChange?.[2].isFlipPathChange).toBe(true)
    })
  })

  describe('Infinite Loop Prevention', () => {
    test('should skip processing flip changes', () => {
      const registry = createFlipPathsRegistry<BooleanState>()
      registry.register('ab', 'a', 'b')

      const synchronizer = createFlipPathsSynchronizer<
        BooleanState,
        GenericMeta
      >(registry)

      const changes: ArrayOfChanges<BooleanState, GenericMeta> = [
        ['a', true, { isFlipPathChange: true }],
      ]

      const result = synchronizer(changes, { a: false, b: true, c: false, d: false })

      // Should not propagate (already a flip change)
      expect(result.length).toBe(1)
      expect(result[0]).toEqual(['a', true, { isFlipPathChange: true }])
    })

    test('should handle mixed changes', () => {
      const registry = createFlipPathsRegistry<BooleanState>()
      registry.register('ab', 'a', 'b')
      registry.register('cd', 'c', 'd')

      const synchronizer = createFlipPathsSynchronizer<
        BooleanState,
        GenericMeta
      >(registry)

      const changes: ArrayOfChanges<BooleanState, GenericMeta> = [
        ['a', true, {}],
        ['c', false, { isFlipPathChange: true }],
      ]

      const result = synchronizer(changes, { a: false, b: true, c: true, d: false })

      // Should only propagate change from 'a', not from 'c'
      expect(result.length).toBe(3) // a, c, and flip change for b
      const flipChanges = result.filter((c) => c[2].isFlipPathChange)
      expect(flipChanges.length).toBe(2) // 'c' original + 'b' flipped from 'a'
    })

    test('should prevent cascading flips', () => {
      const registry = createFlipPathsRegistry<BooleanState>()
      registry.register('ab', 'a', 'b')

      const synchronizer = createFlipPathsSynchronizer<
        BooleanState,
        GenericMeta
      >(registry)

      // First pass: original change
      const changes1: ArrayOfChanges<BooleanState, GenericMeta> = [
        ['a', true, {}],
      ]

      const result1 = synchronizer(changes1, { a: false, b: true, c: false, d: false })
      expect(result1.length).toBe(2) // Original + flip

      // Second pass: pass only the flip changes (typical pipeline scenario)
      const flipChanges = result1.filter((c) => c[2].isFlipPathChange)
      const result2 = synchronizer(flipChanges, { a: true, b: false, c: false, d: false })

      // Should NOT create additional flips (flip changes are skipped)
      expect(result2.length).toBe(flipChanges.length)
      expect(result2[0][2].isFlipPathChange).toBe(true)
    })
  })

  describe('Edge Cases', () => {
    test('should handle empty changes', () => {
      const registry = createFlipPathsRegistry<BooleanState>()
      const synchronizer = createFlipPathsSynchronizer<
        BooleanState,
        GenericMeta
      >(registry)

      const changes: ArrayOfChanges<BooleanState, GenericMeta> = []
      const result = synchronizer(changes, { a: false, b: true, c: false, d: false })

      expect(result).toEqual([])
    })

    test('should handle changes to paths with no flip', () => {
      const registry = createFlipPathsRegistry<BooleanState>()
      registry.register('ab', 'a', 'b')

      const synchronizer = createFlipPathsSynchronizer<
        BooleanState,
        GenericMeta
      >(registry)

      const changes: ArrayOfChanges<BooleanState, GenericMeta> = [
        ['c', true, {}],
      ]

      const result = synchronizer(changes, { a: false, b: true, c: false, d: false })

      // Should return only the original change
      expect(result.length).toBe(1)
      expect(result[0]).toEqual(['c', true, {}])
    })

    test('should handle multiple changes to same path', () => {
      const registry = createFlipPathsRegistry<BooleanState>()
      registry.register('ab', 'a', 'b')

      const synchronizer = createFlipPathsSynchronizer<
        BooleanState,
        GenericMeta
      >(registry)

      const changes: ArrayOfChanges<BooleanState, GenericMeta> = [
        ['a', true, {}],
        ['a', false, {}],
      ]

      const result = synchronizer(changes, { a: false, b: true, c: false, d: false })

      // Should create flip changes for both
      expect(result.length).toBe(4)
      const flipChanges = result.filter(
        (c) => c[0] === 'b' && c[2].isFlipPathChange
      )
      expect(flipChanges.length).toBe(2)
    })

    test('should handle undefined current values', () => {
      interface OptionalState {
        a: boolean | undefined
        b: boolean | undefined
      }

      const registry = createFlipPathsRegistry<OptionalState>()
      registry.register('ab', 'a', 'b')

      const synchronizer = createFlipPathsSynchronizer<
        OptionalState,
        GenericMeta
      >(registry)

      const changes: ArrayOfChanges<OptionalState, GenericMeta> = [
        ['a', true, {}],
      ]

      const result = synchronizer(changes, { a: undefined, b: undefined })

      // Should flip to false (true → false)
      const flipChange = result.find((c) => c[0] === 'b')
      expect(flipChange?.[1]).toBe(false)
    })
  })

  describe('Performance', () => {
    test('should handle many flip pairs efficiently', () => {
      const registry = createFlipPathsRegistry<any>()

      // Create 20 independent flip pairs
      for (let i = 0; i < 20; i++) {
        registry.register(`flip${i}`, `path${i}`, `flipped${i}`)
      }

      const synchronizer = createFlipPathsSynchronizer(registry)

      const changes: ArrayOfChanges<any, GenericMeta> = []
      for (let i = 0; i < 20; i++) {
        changes.push([`path${i}`, true, {}])
      }

      const state: any = {}
      for (let i = 0; i < 20; i++) {
        state[`path${i}`] = false
        state[`flipped${i}`] = true
      }

      const start = performance.now()
      const result = synchronizer(changes, state)
      const duration = performance.now() - start

      // Should complete in reasonable time (< 10ms)
      expect(duration).toBeLessThan(10)

      // Should have original changes plus flip changes
      expect(result.length).toBe(40)
    })
  })
})
