/**
 * Integration tests for aggregations side-effect
 *
 * Tests complete aggregations functionality in a React component context.
 */

import React from 'react'
import { describe, test, expect } from 'vitest'
import { render, waitFor } from '@testing-library/react'
import { createGenericStore } from '../../../src/store/createStore'
import { createAggregationsRegistry } from '../../../src/sideEffects/aggregations/registry'
import { createAggregationsSynchronizer } from '../../../src/pipeline/synchronizers/aggregations'
import type { GenericMeta } from '../../../src/types'

interface TestState {
  a: number
  b: number
  c: number
  target: number | undefined
  target2: number | undefined
}

describe('Aggregations Integration', () => {
  describe('Manual synchronizer integration', () => {
    test('should aggregate equal sources to target', () => {
      const registry = createAggregationsRegistry<TestState>()
      registry.register('agg1', 'target', ['a', 'b'])

      const synchronizer = createAggregationsSynchronizer<TestState, GenericMeta>(
        registry
      )

      const initialState: TestState = { a: 10, b: 10, c: 0, target: undefined, target2: undefined }
      const changes = synchronizer([['a', 10, {} as GenericMeta]], initialState)

      // Should have original change plus aggregation change for target
      expect(changes.length).toBe(2)
      expect(changes[0]).toEqual(['a', 10, {}])
      expect(changes[1][0]).toBe('target')
      expect(changes[1][1]).toBe(10)
      expect((changes[1][2] as any).isAggregationChange).toBe(true)
    })

    test('should set target to undefined when sources differ', () => {
      const registry = createAggregationsRegistry<TestState>()
      registry.register('agg1', 'target', ['a', 'b'])

      const synchronizer = createAggregationsSynchronizer<TestState, GenericMeta>(
        registry
      )

      const initialState: TestState = { a: 10, b: 20, c: 0, target: 10, target2: undefined }
      const changes = synchronizer([['a', 10, {} as GenericMeta]], initialState)

      const targetChange = changes.find((c) => c[0] === 'target')
      expect(targetChange).toBeDefined()
      expect(targetChange![1]).toBeUndefined()
    })

    test('should distribute target value to sources', () => {
      const registry = createAggregationsRegistry<TestState>()
      registry.register('agg1', 'target', ['a', 'b'])

      const synchronizer = createAggregationsSynchronizer<TestState, GenericMeta>(
        registry
      )

      const initialState: TestState = { a: 5, b: 5, c: 0, target: 100, target2: undefined }
      const changes = synchronizer([['target', 100, {} as GenericMeta]], initialState)

      // Should distribute to both sources
      const pathsChanged = changes.map((c) => c[0])
      expect(pathsChanged).toContain('target')
      expect(pathsChanged).toContain('a')
      expect(pathsChanged).toContain('b')

      const aChange = changes.find((c) => c[0] === 'a')
      const bChange = changes.find((c) => c[0] === 'b')
      expect(aChange![1]).toBe(100)
      expect(bChange![1]).toBe(100)
    })

    test('should prevent infinite loops', () => {
      const registry = createAggregationsRegistry<TestState>()
      registry.register('agg1', 'target', ['a', 'b'])

      const synchronizer = createAggregationsSynchronizer<TestState, GenericMeta>(
        registry
      )

      const initialState: TestState = { a: 10, b: 10, c: 0, target: 10, target2: undefined }

      // First pass
      const firstPass = synchronizer([['a', 10, {} as GenericMeta]], initialState)
      expect(firstPass.length).toBe(2) // Original + 1 aggregation

      // Second pass with only aggregation changes
      const aggChangesOnly = firstPass.filter((c) => (c[2] as any).isAggregationChange)
      const secondPass = synchronizer(aggChangesOnly, initialState)

      // Should not create more aggregation changes (already marked)
      expect(secondPass.length).toBe(aggChangesOnly.length)
    })
  })

  describe('Store integration', () => {
    test('basic store operations work', async () => {
      const store = createGenericStore<TestState, GenericMeta>()

      function TestComponent() {
        const [a, setA] = store.useStore('a')
        const [target] = store.useStore('target')

        return (
          <div>
            <span data-testid="a">{a}</span>
            <span data-testid="target">{String(target)}</span>
            <button onClick={() => setA(50)}>Set A</button>
          </div>
        )
      }

      const { getByText, getByTestId } = render(
        <store.Provider initialState={{ a: 0, b: 0, c: 0, target: undefined, target2: undefined }}>
          <TestComponent />
        </store.Provider>
      )

      expect(getByTestId('a').textContent).toBe('0')

      getByText('Set A').click()

      await waitFor(() => {
        expect(getByTestId('a').textContent).toBe('50')
      })
    })

    test('useSideEffects hook registration works', () => {
      const store = createGenericStore<TestState, GenericMeta>()

      function TestComponent() {
        // Register aggregations side-effect
        store.useSideEffects('test-aggregations', {
          aggregations: {
            rules: [{ id: 'agg1', targetPath: 'target', sourcePaths: ['a', 'b'] }],
          },
        })

        return <div>Component with aggregations</div>
      }

      const { container } = render(
        <store.Provider initialState={{ a: 0, b: 0, c: 0, target: undefined, target2: undefined }}>
          <TestComponent />
        </store.Provider>
      )

      // Should render without errors
      expect(container).toBeTruthy()
    })
  })

  describe('Registry lifecycle', () => {
    test('should handle dynamic registration', () => {
      const registry = createAggregationsRegistry<TestState>()

      // Register first aggregation
      registry.register('agg1', 'target', ['a', 'b'])
      expect(registry.graph.getAggregationsForTarget('target').length).toBe(1)

      // Register second aggregation for same target
      registry.register('agg2', 'target', ['c'])
      expect(registry.graph.getAggregationsForTarget('target').length).toBe(2)

      // Unregister first aggregation
      registry.unregister('agg1')
      expect(registry.graph.getAggregationsForTarget('target').length).toBe(1)
    })

    test('should handle mount/unmount scenarios', () => {
      const registry = createAggregationsRegistry<TestState>()

      // Simulate component mount
      registry.register('component-1', 'target', ['a'])

      // Simulate another component mount
      registry.register('component-2', 'target2', ['b'])

      // Both should be active
      expect(registry.graph.getAggregationsForTarget('target').length).toBe(1)
      expect(registry.graph.getAggregationsForTarget('target2').length).toBe(1)

      // Simulate first component unmount
      registry.unregister('component-1')
      expect(registry.graph.getAggregationsForTarget('target').length).toBe(0)

      // Second should still be active
      expect(registry.graph.getAggregationsForTarget('target2').length).toBe(1)

      // Simulate second component unmount
      registry.unregister('component-2')
      expect(registry.graph.getAggregationsForTarget('target2').length).toBe(0)
    })
  })

  describe('Error handling', () => {
    test('should throw on cycle creation', () => {
      const registry = createAggregationsRegistry<TestState>()

      registry.register('agg1', 'b', ['a'])

      // Adding reverse aggregation would create a cycle
      expect(() => {
        registry.register('agg2', 'a', ['b'])
      }).toThrow(/cycle/)
    })

    test('should handle invalid paths gracefully', () => {
      const registry = createAggregationsRegistry<TestState>()
      const synchronizer = createAggregationsSynchronizer<TestState, GenericMeta>(
        registry
      )

      // Register valid aggregation
      registry.register('agg1', 'target', ['a', 'b'])

      // Changes to non-existent path should pass through
      const initialState: TestState = { a: 0, b: 0, c: 0, target: undefined, target2: undefined }
      const changes = synchronizer(
        [['nonexistent' as any, 99, {} as GenericMeta]],
        initialState
      )

      expect(changes.length).toBe(1)
      expect(changes[0][0]).toBe('nonexistent')
    })

    test('should handle empty sources', () => {
      const registry = createAggregationsRegistry<TestState>()

      expect(() => {
        registry.register('agg1', 'target', [])
      }).toThrow()
    })
  })

  describe('Performance scenarios', () => {
    test('should handle rapid state changes', () => {
      const registry = createAggregationsRegistry<TestState>()
      registry.register('agg1', 'target', ['a', 'b'])

      const synchronizer = createAggregationsSynchronizer<TestState, GenericMeta>(
        registry
      )

      const initialState: TestState = { a: 0, b: 0, c: 0, target: undefined, target2: undefined }

      // Simulate 100 rapid changes
      const start = performance.now()
      for (let i = 0; i < 100; i++) {
        synchronizer([['a', i, {} as GenericMeta]], { ...initialState, a: i })
      }
      const duration = performance.now() - start

      // Should complete quickly (< 20ms for 100 iterations)
      expect(duration).toBeLessThan(20)
    })

    test('should handle multiple aggregations efficiently', () => {
      const registry = createAggregationsRegistry<any>()

      // Create 10 aggregations
      for (let i = 0; i < 10; i++) {
        registry.register(`agg${i}`, `target${i}`, [`source${i}a`, `source${i}b`])
      }

      const synchronizer = createAggregationsSynchronizer(registry)

      const state: any = {}
      for (let i = 0; i < 10; i++) {
        state[`source${i}a`] = 100
        state[`source${i}b`] = 100
        state[`target${i}`] = undefined
      }

      const start = performance.now()
      const changes = synchronizer([['source0a', 100, {} as GenericMeta]], state)
      const duration = performance.now() - start

      // Should complete quickly
      expect(duration).toBeLessThan(5)

      // Should create aggregation change
      expect(changes.length).toBeGreaterThan(1)
    })
  })

  describe('Complex scenarios', () => {
    test('should handle multiple aggregations for same target', () => {
      const registry = createAggregationsRegistry<TestState>()
      registry.register('agg1', 'target', ['a', 'b'])
      registry.register('agg2', 'target', ['b', 'c'])

      const synchronizer = createAggregationsSynchronizer<TestState, GenericMeta>(
        registry
      )

      const state: TestState = { a: 10, b: 10, c: 10, target: undefined, target2: undefined }
      const changes = synchronizer([['a', 10, {} as GenericMeta]], state)

      // Should process both aggregations
      const targetChanges = changes.filter((c) => c[0] === 'target')
      expect(targetChanges.length).toBeGreaterThanOrEqual(1)
    })

    test('should handle chained aggregations', () => {
      const registry = createAggregationsRegistry<TestState>()
      registry.register('agg1', 'target', ['a', 'b'])
      registry.register('agg2', 'target2', ['target', 'c'])

      const synchronizer = createAggregationsSynchronizer<TestState, GenericMeta>(
        registry
      )

      const state: TestState = { a: 5, b: 5, c: 5, target: 5, target2: undefined }
      const changes = synchronizer([['a', 5, {} as GenericMeta]], state)

      // Should create change for target
      const targetChange = changes.find((c) => c[0] === 'target')
      expect(targetChange).toBeDefined()
      expect(targetChange![1]).toBe(5)

      // In a real pipeline, this would trigger another synchronizer pass
      // which would then update target2
    })

    test('should handle target to sources distribution with multiple aggregations', () => {
      const registry = createAggregationsRegistry<TestState>()
      registry.register('agg1', 'target', ['a'])
      registry.register('agg2', 'target', ['b'])

      const synchronizer = createAggregationsSynchronizer<TestState, GenericMeta>(
        registry
      )

      const state: TestState = { a: 0, b: 0, c: 0, target: 50, target2: undefined }
      const changes = synchronizer([['target', 50, {} as GenericMeta]], state)

      // Should distribute to both a and b
      const aChange = changes.find((c) => c[0] === 'a')
      const bChange = changes.find((c) => c[0] === 'b')

      expect(aChange![1]).toBe(50)
      expect(bChange![1]).toBe(50)
    })
  })
})
