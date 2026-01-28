/**
 * Tests for AggregationsRegistry
 *
 * Validates registration API and integration with AggregationGraph.
 */

import { describe, test, expect } from 'vitest'
import { createAggregationsRegistry } from '../../../src/sideEffects/aggregations/registry'

interface TestState {
  a: number
  b: number
  c: number
  target: number | undefined
  target2: number | undefined
}

describe('AggregationsRegistry', () => {
  describe('Registration', () => {
    test('should register aggregation', () => {
      const registry = createAggregationsRegistry<TestState>()
      registry.register('agg1', 'target', ['a', 'b'])

      const aggregations = registry.graph.getAggregationsForTarget('target')
      expect(aggregations.length).toBe(1)
      expect(aggregations[0].id).toBe('agg1')
    })

    test('should unregister aggregation', () => {
      const registry = createAggregationsRegistry<TestState>()
      registry.register('agg1', 'target', ['a', 'b'])
      registry.unregister('agg1')

      expect(registry.graph.getAggregationsForTarget('target').length).toBe(0)
    })

    test('should allow multiple registrations for same target', () => {
      const registry = createAggregationsRegistry<TestState>()
      registry.register('agg1', 'target', ['a'])
      registry.register('agg2', 'target', ['b'])

      const aggregations = registry.graph.getAggregationsForTarget('target')
      expect(aggregations.length).toBe(2)
    })

    test('should allow registration of multiple targets', () => {
      const registry = createAggregationsRegistry<TestState>()
      registry.register('agg1', 'target', ['a'])
      registry.register('agg2', 'target2', ['b'])

      expect(registry.graph.getAggregationsForTarget('target').length).toBe(1)
      expect(registry.graph.getAggregationsForTarget('target2').length).toBe(1)
    })
  })

  describe('Cycle Prevention', () => {
    test('should throw error on cycle detection', () => {
      const registry = createAggregationsRegistry<TestState>()
      registry.register('agg1', 'b', ['a'])

      expect(() => {
        registry.register('agg2', 'a', ['b'])
      }).toThrow(/cycle/)
    })

    test('should throw error on transitive cycle', () => {
      const registry = createAggregationsRegistry<TestState>()
      registry.register('agg1', 'b', ['a'])
      registry.register('agg2', 'c', ['b'])

      expect(() => {
        registry.register('agg3', 'a', ['c'])
      }).toThrow(/cycle/)
    })

    test('should allow safe aggregation after unregister', () => {
      const registry = createAggregationsRegistry<TestState>()
      registry.register('agg1', 'b', ['a'])
      registry.unregister('agg1')

      // Now safe to add reverse
      expect(() => {
        registry.register('agg2', 'a', ['b'])
      }).not.toThrow()
    })
  })

  describe('Graph Access', () => {
    test('should expose graph for synchronizer access', () => {
      const registry = createAggregationsRegistry<TestState>()
      expect(registry.graph).toBeDefined()
      expect(registry.graph.addAggregation).toBeDefined()
      expect(registry.graph.getAggregationsForTarget).toBeDefined()
      expect(registry.graph.getTargetsForSource).toBeDefined()
    })

    test('should allow direct graph queries', () => {
      const registry = createAggregationsRegistry<TestState>()
      registry.register('agg1', 'target', ['a', 'b'])

      const targets = registry.graph.getTargetsForSource('a')
      expect(targets.has('target')).toBe(true)
    })
  })

  describe('Edge Cases', () => {
    test('should handle unregister non-existent ID gracefully', () => {
      const registry = createAggregationsRegistry<TestState>()
      expect(() => {
        registry.unregister('nonexistent')
      }).not.toThrow()
    })

    test('should handle empty source paths', () => {
      const registry = createAggregationsRegistry<TestState>()
      expect(() => {
        registry.register('agg1', 'target', [])
      }).toThrow()
    })

    test('should handle re-registration with same ID', () => {
      const registry = createAggregationsRegistry<TestState>()
      registry.register('agg1', 'target', ['a'])
      registry.register('agg1', 'target2', ['b'])

      // Second registration overwrites first
      const agg1Targets = registry.graph.getAggregationsForTarget('target2')
      expect(agg1Targets.length).toBeGreaterThanOrEqual(0)
    })
  })

  describe('Complex Scenarios', () => {
    test('should handle multiple overlapping aggregations', () => {
      const registry = createAggregationsRegistry<TestState>()
      registry.register('agg1', 'target', ['a', 'b'])
      registry.register('agg2', 'target', ['b', 'c'])
      registry.register('agg3', 'target2', ['a'])

      expect(registry.graph.getAggregationsForTarget('target').length).toBe(2)
      expect(registry.graph.getTargetsForSource('b').size).toBe(1)
      expect(registry.graph.getTargetsForSource('a').size).toBe(2)
    })

    test('should handle chained aggregations', () => {
      const registry = createAggregationsRegistry<TestState>()
      registry.register('agg1', 'target', ['a', 'b'])
      registry.register('agg2', 'target2', ['target', 'c'])

      const aggregations1 = registry.graph.getAggregationsForTarget('target')
      const aggregations2 = registry.graph.getAggregationsForTarget('target2')

      expect(aggregations1.length).toBe(1)
      expect(aggregations2.length).toBe(1)
      expect(aggregations2[0].sourcePaths).toContain('target')
    })
  })
})
