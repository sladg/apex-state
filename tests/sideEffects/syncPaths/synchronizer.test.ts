/**
 * Tests for syncPaths synchronizer
 *
 * Validates change propagation, metadata flags, and infinite loop prevention.
 */

import { describe, test, expect } from 'vitest'
import { createSyncPathsSynchronizer } from '../../../src/pipeline/synchronizers/syncPaths'
import { createSyncPathsRegistry } from '../../../src/sideEffects/syncPaths/registry'
import type { ArrayOfChanges, GenericMeta } from '../../../src/types'

interface TestState {
  a: number
  b: number
  c: number
  d: number
}

describe('createSyncPathsSynchronizer', () => {
  describe('Basic Synchronization', () => {
    test('should propagate change to synced path', () => {
      const registry = createSyncPathsRegistry<TestState>()
      registry.register('ab', 'a', 'b')

      const synchronizer = createSyncPathsSynchronizer<TestState, GenericMeta>(
        registry
      )

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['a', 10, {}],
      ]

      const result = synchronizer(changes, { a: 0, b: 0, c: 0, d: 0 })

      // Should have original change plus sync change
      expect(result.length).toBe(2)
      expect(result[0]).toEqual(['a', 10, {}])
      expect(result[1][0]).toBe('b')
      expect(result[1][1]).toBe(10)
      expect(result[1][2].isSyncPathChange).toBe(true)
    })

    test('should propagate to multiple synced paths', () => {
      const registry = createSyncPathsRegistry<TestState>()
      registry.register('ab', 'a', 'b')
      registry.register('ac', 'a', 'c')

      const synchronizer = createSyncPathsSynchronizer<TestState, GenericMeta>(
        registry
      )

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['a', 10, {}],
      ]

      const result = synchronizer(changes, { a: 0, b: 0, c: 0, d: 0 })

      // Should have original change plus two sync changes
      expect(result.length).toBe(3)
      expect(result.filter((c) => c[2].isSyncPathChange).length).toBe(2)
    })

    test('should handle transitive sync paths', () => {
      const registry = createSyncPathsRegistry<TestState>()
      registry.register('ab', 'a', 'b')
      registry.register('bc', 'b', 'c')

      const synchronizer = createSyncPathsSynchronizer<TestState, GenericMeta>(
        registry
      )

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['a', 10, {}],
      ]

      const result = synchronizer(changes, { a: 0, b: 0, c: 0, d: 0 })

      // Should sync to both b and c
      const syncedPaths = result
        .filter((c) => c[2].isSyncPathChange)
        .map((c) => c[0])

      expect(syncedPaths).toContain('b')
      expect(syncedPaths).toContain('c')
    })
  })

  describe('Metadata Handling', () => {
    test('should set isSyncPathChange flag', () => {
      const registry = createSyncPathsRegistry<TestState>()
      registry.register('ab', 'a', 'b')

      const synchronizer = createSyncPathsSynchronizer<TestState, GenericMeta>(
        registry
      )

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['a', 10, { sender: 'test' }],
      ]

      const result = synchronizer(changes, { a: 0, b: 0, c: 0, d: 0 })

      const syncChange = result.find((c) => c[0] === 'b')
      expect(syncChange?.[2].isSyncPathChange).toBe(true)
      expect(syncChange?.[2].sender).toBe('test') // Preserves other metadata
    })

    test('should preserve existing metadata', () => {
      const registry = createSyncPathsRegistry<TestState>()
      registry.register('ab', 'a', 'b')

      const synchronizer = createSyncPathsSynchronizer<TestState, GenericMeta>(
        registry
      )

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['a', 10, { sender: 'user-123', isProgramaticChange: false }],
      ]

      const result = synchronizer(changes, { a: 0, b: 0, c: 0, d: 0 })

      const syncChange = result.find((c) => c[0] === 'b')
      expect(syncChange?.[2].sender).toBe('user-123')
      expect(syncChange?.[2].isProgramaticChange).toBe(false)
      expect(syncChange?.[2].isSyncPathChange).toBe(true)
    })
  })

  describe('Infinite Loop Prevention', () => {
    test('should skip processing sync changes', () => {
      const registry = createSyncPathsRegistry<TestState>()
      registry.register('ab', 'a', 'b')

      const synchronizer = createSyncPathsSynchronizer<TestState, GenericMeta>(
        registry
      )

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['a', 10, { isSyncPathChange: true }],
      ]

      const result = synchronizer(changes, { a: 0, b: 0, c: 0, d: 0 })

      // Should not propagate (already a sync change)
      expect(result.length).toBe(1)
      expect(result[0]).toEqual(['a', 10, { isSyncPathChange: true }])
    })

    test('should handle mixed changes', () => {
      const registry = createSyncPathsRegistry<TestState>()
      registry.register('ab', 'a', 'b')
      registry.register('cd', 'c', 'd')

      const synchronizer = createSyncPathsSynchronizer<TestState, GenericMeta>(
        registry
      )

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['a', 10, {}],
        ['c', 20, { isSyncPathChange: true }],
      ]

      const result = synchronizer(changes, { a: 0, b: 0, c: 0, d: 0 })

      // Should only propagate change from 'a', not from 'c'
      expect(result.length).toBe(3) // a, c, and sync change for b
      const syncChanges = result.filter((c) => c[2].isSyncPathChange)
      expect(syncChanges.length).toBe(2) // 'c' original + 'b' synced from 'a'
    })
  })

  describe('Edge Cases', () => {
    test('should handle empty changes', () => {
      const registry = createSyncPathsRegistry<TestState>()
      const synchronizer = createSyncPathsSynchronizer<TestState, GenericMeta>(
        registry
      )

      const changes: ArrayOfChanges<TestState, GenericMeta> = []
      const result = synchronizer(changes, { a: 0, b: 0, c: 0, d: 0 })

      expect(result).toEqual([])
    })

    test('should handle changes to paths with no sync', () => {
      const registry = createSyncPathsRegistry<TestState>()
      registry.register('ab', 'a', 'b')

      const synchronizer = createSyncPathsSynchronizer<TestState, GenericMeta>(
        registry
      )

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['c', 30, {}],
      ]

      const result = synchronizer(changes, { a: 0, b: 0, c: 0, d: 0 })

      // Should return only the original change
      expect(result.length).toBe(1)
      expect(result[0]).toEqual(['c', 30, {}])
    })

    test('should handle multiple changes to same path', () => {
      const registry = createSyncPathsRegistry<TestState>()
      registry.register('ab', 'a', 'b')

      const synchronizer = createSyncPathsSynchronizer<TestState, GenericMeta>(
        registry
      )

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['a', 10, {}],
        ['a', 20, {}],
      ]

      const result = synchronizer(changes, { a: 0, b: 0, c: 0, d: 0 })

      // Should create sync changes for both
      expect(result.length).toBe(4)
      const syncChanges = result.filter(
        (c) => c[0] === 'b' && c[2].isSyncPathChange
      )
      expect(syncChanges.length).toBe(2)
    })
  })

  describe('Performance', () => {
    test('should handle many synced paths efficiently', () => {
      const registry = createSyncPathsRegistry<any>()

      // Create a chain of 20 synced paths
      for (let i = 0; i < 20; i++) {
        registry.register(`sync${i}`, `path${i}`, `path${i + 1}`)
      }

      const synchronizer = createSyncPathsSynchronizer(registry)

      const changes: ArrayOfChanges<any, GenericMeta> = [
        ['path0', 'value', {}],
      ]

      const start = performance.now()
      const result = synchronizer(changes, {})
      const duration = performance.now() - start

      // Should complete in reasonable time (< 10ms)
      expect(duration).toBeLessThan(10)

      // Should propagate to all 20 connected paths
      expect(result.length).toBeGreaterThan(1)
    })

    test('should handle multiple independent changes', () => {
      const registry = createSyncPathsRegistry<any>()

      // Create multiple independent sync pairs
      for (let i = 0; i < 10; i++) {
        registry.register(`sync${i}`, `path${i}`, `pathSync${i}`)
      }

      const synchronizer = createSyncPathsSynchronizer(registry)

      const changes: ArrayOfChanges<any, GenericMeta> = []
      for (let i = 0; i < 10; i++) {
        changes.push([`path${i}`, i, {}])
      }

      const start = performance.now()
      const result = synchronizer(changes, {})
      const duration = performance.now() - start

      // Should process all efficiently (< 10ms)
      expect(duration).toBeLessThan(10)

      // Should have original changes plus sync changes
      expect(result.length).toBe(20)
    })
  })
})
