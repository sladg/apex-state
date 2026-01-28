/**
 * Tests for Aggregations Synchronizer
 *
 * Validates bidirectional aggregation logic and performance optimizations.
 */

import { describe, test, expect } from 'vitest'
import { createAggregationsSynchronizer } from '../../../src/pipeline/synchronizers/aggregations'
import { createAggregationsRegistry } from '../../../src/sideEffects/aggregations/registry'
import type { ArrayOfChanges, GenericMeta } from '../../../src/types'

interface TestState {
  a: number
  b: number
  c: number
  target: number | undefined
  target2: number | undefined
}

describe('Aggregations Synchronizer', () => {
  describe('Source to Target Aggregation', () => {
    test('should set target when all sources are equal', () => {
      const registry = createAggregationsRegistry<TestState>()
      registry.register('agg1', 'target', ['a', 'b'])

      const synchronizer = createAggregationsSynchronizer(registry)
      const state: TestState = { a: 10, b: 10, c: 0, target: undefined, target2: undefined }

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['a', 10, { isProgramaticChange: false }],
      ]

      const result = synchronizer(changes, state)

      // Should add change for target
      const targetChange = result.find(([path]) => path === 'target')
      expect(targetChange).toBeDefined()
      expect(targetChange![1]).toBe(10)
      expect((targetChange![2] as any).isAggregationChange).toBe(true)
    })

    test('should set target to undefined when sources differ', () => {
      const registry = createAggregationsRegistry<TestState>()
      registry.register('agg1', 'target', ['a', 'b'])

      const synchronizer = createAggregationsSynchronizer(registry)
      const state: TestState = { a: 10, b: 20, c: 0, target: 10, target2: undefined }

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['a', 10, { isProgramaticChange: false }],
      ]

      const result = synchronizer(changes, state)

      const targetChange = result.find(([path]) => path === 'target')
      expect(targetChange).toBeDefined()
      expect(targetChange![1]).toBeUndefined()
    })

    test('should handle multiple sources correctly', () => {
      const registry = createAggregationsRegistry<TestState>()
      registry.register('agg1', 'target', ['a', 'b', 'c'])

      const synchronizer = createAggregationsSynchronizer(registry)
      const state: TestState = { a: 5, b: 5, c: 5, target: undefined, target2: undefined }

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['a', 5, { isProgramaticChange: false }],
      ]

      const result = synchronizer(changes, state)

      const targetChange = result.find(([path]) => path === 'target')
      expect(targetChange).toBeDefined()
      expect(targetChange![1]).toBe(5)
    })

    test('should not process aggregation changes recursively', () => {
      const registry = createAggregationsRegistry<TestState>()
      registry.register('agg1', 'target', ['a'])

      const synchronizer = createAggregationsSynchronizer(registry)
      const state: TestState = { a: 10, b: 0, c: 0, target: 10, target2: undefined }

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['a', 10, { isProgramaticChange: true, isAggregationChange: true }],
      ]

      const result = synchronizer(changes, state)

      // Should not add any new changes (already aggregation change)
      expect(result.length).toBe(1)
    })
  })

  describe('Target to Sources Distribution', () => {
    test('should distribute target value to all sources', () => {
      const registry = createAggregationsRegistry<TestState>()
      registry.register('agg1', 'target', ['a', 'b'])

      const synchronizer = createAggregationsSynchronizer(registry)
      const state: TestState = { a: 5, b: 5, c: 0, target: 100, target2: undefined }

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['target', 100, { isProgramaticChange: false }],
      ]

      const result = synchronizer(changes, state)

      const aChange = result.find(([path]) => path === 'a')
      const bChange = result.find(([path]) => path === 'b')

      expect(aChange).toBeDefined()
      expect(aChange![1]).toBe(100)
      expect(bChange).toBeDefined()
      expect(bChange![1]).toBe(100)
      expect((aChange![2] as any).isAggregationChange).toBe(true)
      expect((bChange![2] as any).isAggregationChange).toBe(true)
    })

    test('should distribute undefined to sources', () => {
      const registry = createAggregationsRegistry<TestState>()
      registry.register('agg1', 'target', ['a', 'b'])

      const synchronizer = createAggregationsSynchronizer(registry)
      const state: TestState = { a: 10, b: 10, c: 0, target: undefined, target2: undefined }

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['target', undefined, { isProgramaticChange: false }],
      ]

      const result = synchronizer(changes, state)

      const aChange = result.find(([path]) => path === 'a')
      const bChange = result.find(([path]) => path === 'b')

      expect(aChange![1]).toBeUndefined()
      expect(bChange![1]).toBeUndefined()
    })
  })

  describe('Multiple Aggregations for Same Target', () => {
    test('should handle multiple aggregations correctly', () => {
      const registry = createAggregationsRegistry<TestState>()
      registry.register('agg1', 'target', ['a', 'b'])
      registry.register('agg2', 'target', ['b', 'c'])

      const synchronizer = createAggregationsSynchronizer(registry)
      const state: TestState = { a: 10, b: 10, c: 10, target: undefined, target2: undefined }

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['a', 10, { isProgramaticChange: false }],
      ]

      const result = synchronizer(changes, state)

      // Should create changes for target from both aggregations
      const targetChanges = result.filter(([path]) => path === 'target')
      expect(targetChanges.length).toBeGreaterThanOrEqual(1)
    })

    test('should distribute to all sources in all aggregations', () => {
      const registry = createAggregationsRegistry<TestState>()
      registry.register('agg1', 'target', ['a'])
      registry.register('agg2', 'target', ['b'])

      const synchronizer = createAggregationsSynchronizer(registry)
      const state: TestState = { a: 0, b: 0, c: 0, target: 50, target2: undefined }

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['target', 50, { isProgramaticChange: false }],
      ]

      const result = synchronizer(changes, state)

      const aChange = result.find(([path]) => path === 'a')
      const bChange = result.find(([path]) => path === 'b')

      expect(aChange![1]).toBe(50)
      expect(bChange![1]).toBe(50)
    })
  })

  describe('Performance Optimizations', () => {
    test('should early exit when no changes', () => {
      const registry = createAggregationsRegistry<TestState>()
      registry.register('agg1', 'target', ['a', 'b'])

      const synchronizer = createAggregationsSynchronizer(registry)
      const state: TestState = { a: 10, b: 10, c: 0, target: 10, target2: undefined }

      const changes: ArrayOfChanges<TestState, GenericMeta> = []

      const start = performance.now()
      const result = synchronizer(changes, state)
      const duration = performance.now() - start

      expect(result.length).toBe(0)
      expect(duration).toBeLessThan(1)
    })

    test('should early exit when path has no aggregations', () => {
      const registry = createAggregationsRegistry<TestState>()
      registry.register('agg1', 'target', ['a'])

      const synchronizer = createAggregationsSynchronizer(registry)
      const state: TestState = { a: 10, b: 10, c: 0, target: 10, target2: undefined }

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['c', 99, { isProgramaticChange: false }],
      ]

      const start = performance.now()
      const result = synchronizer(changes, state)
      const duration = performance.now() - start

      // Should only contain original change
      expect(result.length).toBe(1)
      expect(duration).toBeLessThan(1)
    })

    test('should not process same target multiple times', () => {
      const registry = createAggregationsRegistry<TestState>()
      registry.register('agg1', 'target', ['a', 'b'])

      const synchronizer = createAggregationsSynchronizer(registry)
      const state: TestState = { a: 10, b: 10, c: 0, target: 10, target2: undefined }

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['a', 10, { isProgramaticChange: false }],
        ['b', 10, { isProgramaticChange: false }],
      ]

      const result = synchronizer(changes, state)

      // Should deduplicate target processing
      const targetChanges = result.filter(([path]) => path === 'target')
      expect(targetChanges.length).toBeGreaterThanOrEqual(1)
      expect(targetChanges.length).toBeLessThanOrEqual(2)
    })

    test('should handle complex graph efficiently', () => {
      const registry = createAggregationsRegistry<any>()

      // Create 50 aggregations
      for (let i = 0; i < 50; i++) {
        registry.register(`agg${i}`, `target${i}`, [`source${i}`])
      }

      const synchronizer = createAggregationsSynchronizer(registry)
      const state: any = {}
      for (let i = 0; i < 50; i++) {
        state[`source${i}`] = i
        state[`target${i}`] = i
      }

      const changes: ArrayOfChanges<any, GenericMeta> = [
        ['source0', 100, { isProgramaticChange: false }],
      ]

      const start = performance.now()
      synchronizer(changes, state)
      const duration = performance.now() - start

      // Should complete in < 5ms
      expect(duration).toBeLessThan(5)
    })
  })

  describe('Edge Cases', () => {
    test('should handle empty state', () => {
      const registry = createAggregationsRegistry<TestState>()
      registry.register('agg1', 'target', ['a', 'b'])

      const synchronizer = createAggregationsSynchronizer(registry)
      const state: TestState = { a: undefined as any, b: undefined as any, c: 0, target: undefined, target2: undefined }

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['a', undefined as any, { isProgramaticChange: false }],
      ]

      expect(() => {
        synchronizer(changes, state)
      }).not.toThrow()
    })

    test('should handle unregistered path changes', () => {
      const registry = createAggregationsRegistry<TestState>()
      const synchronizer = createAggregationsSynchronizer(registry)
      const state: TestState = { a: 10, b: 10, c: 0, target: undefined, target2: undefined }

      const changes: ArrayOfChanges<TestState, GenericMeta> = [
        ['c', 100, { isProgramaticChange: false }],
      ]

      const result = synchronizer(changes, state)
      expect(result.length).toBe(1) // Only original change
    })
  })
})
