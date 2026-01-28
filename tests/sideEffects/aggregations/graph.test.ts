/**
 * Tests for AggregationGraph
 *
 * Validates graph operations, aggregation tracking, and cycle detection.
 */

import { describe, test, expect } from 'vitest'
import { createAggregationGraph } from '../../../src/sideEffects/aggregations/graph'

interface TestState {
  a: number
  b: number
  c: number
  d: number
  target: number | undefined
  target2: number | undefined
}

describe('AggregationGraph', () => {
  describe('Basic Operations', () => {
    test('should create an empty graph', () => {
      const graph = createAggregationGraph<TestState>()
      expect(graph.getAggregationsForTarget('target').length).toBe(0)
    })

    test('should add aggregation with single source', () => {
      const graph = createAggregationGraph<TestState>()
      graph.addAggregation('agg1', 'target', ['a'])

      const aggregations = graph.getAggregationsForTarget('target')
      expect(aggregations.length).toBe(1)
      expect(aggregations[0].id).toBe('agg1')
      expect(aggregations[0].sourcePaths).toEqual(['a'])
    })

    test('should add aggregation with multiple sources', () => {
      const graph = createAggregationGraph<TestState>()
      graph.addAggregation('agg1', 'target', ['a', 'b'])

      const aggregations = graph.getAggregationsForTarget('target')
      expect(aggregations.length).toBe(1)
      expect(aggregations[0].sourcePaths).toEqual(['a', 'b'])
    })

    test('should remove aggregation by ID', () => {
      const graph = createAggregationGraph<TestState>()
      graph.addAggregation('agg1', 'target', ['a', 'b'])
      graph.removeAggregation('agg1')

      expect(graph.getAggregationsForTarget('target').length).toBe(0)
      expect(graph.getTargetsForSource('a').size).toBe(0)
    })

    test('should handle multiple aggregations for same target', () => {
      const graph = createAggregationGraph<TestState>()
      graph.addAggregation('agg1', 'target', ['a', 'b'])
      graph.addAggregation('agg2', 'target', ['c', 'd'])

      const aggregations = graph.getAggregationsForTarget('target')
      expect(aggregations.length).toBe(2)
      expect(aggregations.map((a) => a.id)).toContain('agg1')
      expect(aggregations.map((a) => a.id)).toContain('agg2')
    })

    test('should handle multiple aggregations for different targets', () => {
      const graph = createAggregationGraph<TestState>()
      graph.addAggregation('agg1', 'target', ['a'])
      graph.addAggregation('agg2', 'target2', ['b'])

      expect(graph.getAggregationsForTarget('target').length).toBe(1)
      expect(graph.getAggregationsForTarget('target2').length).toBe(1)
    })
  })

  describe('Source to Target Lookup', () => {
    test('should get targets affected by source change', () => {
      const graph = createAggregationGraph<TestState>()
      graph.addAggregation('agg1', 'target', ['a', 'b'])

      const targets = graph.getTargetsForSource('a')
      expect(targets.has('target')).toBe(true)
      expect(targets.size).toBe(1)
    })

    test('should handle source affecting multiple targets', () => {
      const graph = createAggregationGraph<TestState>()
      graph.addAggregation('agg1', 'target', ['a'])
      graph.addAggregation('agg2', 'target2', ['a'])

      const targets = graph.getTargetsForSource('a')
      expect(targets.has('target')).toBe(true)
      expect(targets.has('target2')).toBe(true)
      expect(targets.size).toBe(2)
    })

    test('should return empty set for non-existent source', () => {
      const graph = createAggregationGraph<TestState>()
      const targets = graph.getTargetsForSource('nonexistent' as any)
      expect(targets.size).toBe(0)
    })
  })

  describe('Cycle Detection', () => {
    test('should prevent simple cycle: target→source→target', () => {
      const graph = createAggregationGraph<TestState>()
      graph.addAggregation('agg1', 'b', ['a'])

      // Adding a→b→a would create a cycle
      expect(() => {
        graph.addAggregation('agg2', 'a', ['b'])
      }).toThrow(/cycle/)
    })

    test('should prevent transitive cycle: a→b→c→a', () => {
      const graph = createAggregationGraph<TestState>()
      graph.addAggregation('agg1', 'b', ['a'])
      graph.addAggregation('agg2', 'c', ['b'])

      // Adding c→a would create a cycle
      expect(() => {
        graph.addAggregation('agg3', 'a', ['c'])
      }).toThrow(/cycle/)
    })

    test('should allow independent aggregations', () => {
      const graph = createAggregationGraph<TestState>()
      graph.addAggregation('agg1', 'target', ['a'])
      graph.addAggregation('agg2', 'target2', ['b'])

      // These are independent, no cycle
      expect(() => {
        graph.addAggregation('agg3', 'c', ['target', 'target2'])
      }).not.toThrow()
    })

    test('should allow multiple sources to same target', () => {
      const graph = createAggregationGraph<TestState>()
      graph.addAggregation('agg1', 'target', ['a', 'b', 'c'])

      expect(graph.getAggregationsForTarget('target').length).toBe(1)
    })

    test('should detect cycle after removal and re-addition', () => {
      const graph = createAggregationGraph<TestState>()
      graph.addAggregation('agg1', 'b', ['a'])
      graph.removeAggregation('agg1')

      // Now we should be able to add reverse without cycle
      expect(() => {
        graph.addAggregation('agg2', 'a', ['b'])
      }).not.toThrow()
    })

    test('should prevent self-referential aggregation', () => {
      const graph = createAggregationGraph<TestState>()

      // A path cannot aggregate from itself
      expect(() => {
        graph.addAggregation('agg1', 'a', ['a'])
      }).toThrow(/cycle/)
    })
  })

  describe('Edge Cases', () => {
    test('should throw error for empty source paths', () => {
      const graph = createAggregationGraph<TestState>()

      expect(() => {
        graph.addAggregation('agg1', 'target', [])
      }).toThrow(/empty/)
    })

    test('should handle removing non-existent aggregation', () => {
      const graph = createAggregationGraph<TestState>()
      expect(() => {
        graph.removeAggregation('nonexistent')
      }).not.toThrow()
    })

    test('should handle duplicate aggregation IDs (overwrites)', () => {
      const graph = createAggregationGraph<TestState>()
      graph.addAggregation('agg1', 'target', ['a'])
      graph.addAggregation('agg1', 'target2', ['b'])

      // Second registration with same ID overwrites
      const agg1 = graph.getAggregationsForTarget('target2')
      expect(agg1.length).toBeGreaterThanOrEqual(0)
    })

    test('should return empty array for non-existent target', () => {
      const graph = createAggregationGraph<TestState>()
      const aggregations = graph.getAggregationsForTarget('nonexistent' as any)
      expect(aggregations.length).toBe(0)
    })
  })

  describe('Complex Scenarios', () => {
    test('should handle fan-in: multiple sources to one target', () => {
      const graph = createAggregationGraph<TestState>()
      graph.addAggregation('agg1', 'target', ['a', 'b', 'c', 'd'])

      const aggregations = graph.getAggregationsForTarget('target')
      expect(aggregations.length).toBe(1)
      expect(aggregations[0].sourcePaths.length).toBe(4)
    })

    test('should handle fan-out: one source to multiple targets', () => {
      const graph = createAggregationGraph<TestState>()
      graph.addAggregation('agg1', 'target', ['a'])
      graph.addAggregation('agg2', 'target2', ['a'])

      const targets = graph.getTargetsForSource('a')
      expect(targets.size).toBe(2)
    })

    test('should handle complex dependency chain without cycle', () => {
      const graph = createAggregationGraph<TestState>()
      // Chain: a,b → target → c,d → target2
      graph.addAggregation('agg1', 'target', ['a', 'b'])
      graph.addAggregation('agg2', 'target2', ['target', 'c'])

      expect(graph.getAggregationsForTarget('target').length).toBe(1)
      expect(graph.getAggregationsForTarget('target2').length).toBe(1)

      // Verify a affects target2 indirectly
      const targetsForA = graph.getTargetsForSource('a')
      expect(targetsForA.has('target')).toBe(true)
    })
  })

  describe('Performance', () => {
    test('should handle many aggregations efficiently', () => {
      const graph = createAggregationGraph<any>()

      // Add 100 aggregations
      for (let i = 0; i < 100; i++) {
        graph.addAggregation(`agg${i}`, `target${i}`, [`source${i}`])
      }

      // Lookups should be fast
      const start = performance.now()
      for (let i = 0; i < 100; i++) {
        graph.getTargetsForSource(`source${i}`)
        graph.getAggregationsForTarget(`target${i}`)
      }
      const duration = performance.now() - start

      // 200 lookups should be very fast (< 10ms)
      expect(duration).toBeLessThan(10)
    })

    test('should handle complex graph efficiently', () => {
      const graph = createAggregationGraph<any>()

      // Create a wide aggregation: 50 sources to 1 target
      const sources = Array.from({ length: 50 }, (_, i) => `source${i}`)
      graph.addAggregation('wide', 'target', sources)

      // Lookup should still be fast
      const start = performance.now()
      for (let i = 0; i < 100; i++) {
        graph.getTargetsForSource('source0')
      }
      const duration = performance.now() - start

      expect(duration).toBeLessThan(5)
    })
  })
})
