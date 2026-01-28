/**
 * Tests for flipPaths registry
 *
 * Validates registration, unregistration, and bidirectional lookup.
 */

import { describe, test, expect } from 'vitest'
import { createFlipPathsRegistry } from '../../../src/sideEffects/flipPaths/registry'

interface TestState {
  a: boolean
  b: boolean
  c: string
  d: string
}

describe('createFlipPathsRegistry', () => {
  describe('Registration', () => {
    test('should register flip path pair', () => {
      const registry = createFlipPathsRegistry<TestState>()

      registry.register('ab', 'a', 'b')

      expect(registry.hasFlip('a')).toBe(true)
      expect(registry.hasFlip('b')).toBe(true)
    })

    test('should create bidirectional mapping', () => {
      const registry = createFlipPathsRegistry<TestState>()

      registry.register('ab', 'a', 'b')

      expect(registry.getFlippedPath('a')).toBe('b')
      expect(registry.getFlippedPath('b')).toBe('a')
    })

    test('should handle multiple flip pairs', () => {
      const registry = createFlipPathsRegistry<TestState>()

      registry.register('ab', 'a', 'b')
      registry.register('cd', 'c', 'd')

      expect(registry.getFlippedPath('a')).toBe('b')
      expect(registry.getFlippedPath('c')).toBe('d')
      expect(registry.getFlippedPath('d')).toBe('c')
    })

    test('should overwrite existing registration with same ID', () => {
      const registry = createFlipPathsRegistry<TestState>()

      registry.register('ab', 'a', 'b')
      registry.register('ab', 'c', 'd')

      // Old registration should be replaced
      expect(registry.getFlippedPath('a')).toBeUndefined()
      expect(registry.getFlippedPath('b')).toBeUndefined()

      // New registration should work
      expect(registry.getFlippedPath('c')).toBe('d')
      expect(registry.getFlippedPath('d')).toBe('c')
    })
  })

  describe('Unregistration', () => {
    test('should unregister flip path pair', () => {
      const registry = createFlipPathsRegistry<TestState>()

      registry.register('ab', 'a', 'b')
      registry.unregister('ab')

      expect(registry.hasFlip('a')).toBe(false)
      expect(registry.hasFlip('b')).toBe(false)
      expect(registry.getFlippedPath('a')).toBeUndefined()
      expect(registry.getFlippedPath('b')).toBeUndefined()
    })

    test('should handle unregistering non-existent ID', () => {
      const registry = createFlipPathsRegistry<TestState>()

      // Should not throw
      expect(() => registry.unregister('nonexistent')).not.toThrow()
    })

    test('should unregister only specified pair', () => {
      const registry = createFlipPathsRegistry<TestState>()

      registry.register('ab', 'a', 'b')
      registry.register('cd', 'c', 'd')

      registry.unregister('ab')

      // 'ab' pair should be removed
      expect(registry.hasFlip('a')).toBe(false)
      expect(registry.hasFlip('b')).toBe(false)

      // 'cd' pair should remain
      expect(registry.hasFlip('c')).toBe(true)
      expect(registry.hasFlip('d')).toBe(true)
      expect(registry.getFlippedPath('c')).toBe('d')
    })
  })

  describe('Lookups', () => {
    test('should return undefined for unregistered paths', () => {
      const registry = createFlipPathsRegistry<TestState>()

      expect(registry.getFlippedPath('a')).toBeUndefined()
      expect(registry.hasFlip('a')).toBe(false)
    })

    test('should return correct flipped path', () => {
      const registry = createFlipPathsRegistry<TestState>()

      registry.register('ab', 'a', 'b')

      expect(registry.getFlippedPath('a')).toBe('b')
      expect(registry.getFlippedPath('b')).toBe('a')
    })

    test('should work with nested paths', () => {
      interface NestedState {
        user: {
          settings: {
            darkMode: boolean
            lightMode: boolean
          }
        }
      }

      const registry = createFlipPathsRegistry<NestedState>()

      registry.register(
        'modes',
        'user.settings.darkMode',
        'user.settings.lightMode'
      )

      expect(registry.getFlippedPath('user.settings.darkMode')).toBe(
        'user.settings.lightMode'
      )
      expect(registry.getFlippedPath('user.settings.lightMode')).toBe(
        'user.settings.darkMode'
      )
    })
  })

  describe('Edge Cases', () => {
    test('should handle empty registry', () => {
      const registry = createFlipPathsRegistry<TestState>()

      expect(registry.hasFlip('a')).toBe(false)
      expect(registry.getFlippedPath('a')).toBeUndefined()
    })

    test('should handle same path in multiple pairs (last registration wins)', () => {
      const registry = createFlipPathsRegistry<TestState>()

      registry.register('ab', 'a', 'b')
      registry.register('ac', 'a', 'c') // 'a' now flips with 'c', not 'b'

      // Last registration wins for 'a'
      expect(registry.getFlippedPath('a')).toBe('c')

      // But 'b' still remembers 'a' from first registration
      expect(registry.getFlippedPath('b')).toBe('a')
    })

    test('should support re-registration after unregistration', () => {
      const registry = createFlipPathsRegistry<TestState>()

      registry.register('ab', 'a', 'b')
      registry.unregister('ab')
      registry.register('ab', 'a', 'b')

      expect(registry.getFlippedPath('a')).toBe('b')
      expect(registry.getFlippedPath('b')).toBe('a')
    })
  })
})
