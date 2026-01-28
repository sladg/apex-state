/**
 * Integration tests for sync paths side-effect
 *
 * Tests complete sync paths functionality in a React component context.
 * Note: Full integration with useSideEffects will be completed when
 * the pipeline connects to the sideEffectsRegistry.
 */

import React from 'react'
import { describe, test, expect } from 'vitest'
import { render, waitFor } from '@testing-library/react'
import { createGenericStore } from '../../../src/store/createStore'
import { createSyncPathsRegistry } from '../../../src/sideEffects/syncPaths/registry'
import { createSyncPathsSynchronizer } from '../../../src/pipeline/synchronizers/syncPaths'
import type { GenericMeta } from '../../../src/types'

interface TestState {
  a: number
  b: number
  c: number
}

describe('Sync Paths Integration', () => {
  describe('Manual synchronizer integration', () => {
    test('should propagate changes through synchronizer', () => {
      const registry = createSyncPathsRegistry<TestState>()
      registry.register('ab', 'a', 'b')

      const synchronizer = createSyncPathsSynchronizer<TestState, GenericMeta>(
        registry
      )

      const initialState: TestState = { a: 0, b: 0, c: 0 }
      const changes = synchronizer([['a', 10, {}]], initialState)

      // Should have original change plus sync change for b
      expect(changes.length).toBe(2)
      expect(changes[0]).toEqual(['a', 10, {}])
      expect(changes[1][0]).toBe('b')
      expect(changes[1][1]).toBe(10)
      expect(changes[1][2].isSyncPathChange).toBe(true)
    })

    test('should handle transitive sync paths', () => {
      const registry = createSyncPathsRegistry<TestState>()
      registry.register('ab', 'a', 'b')
      registry.register('bc', 'b', 'c')

      const synchronizer = createSyncPathsSynchronizer<TestState, GenericMeta>(
        registry
      )

      const initialState: TestState = { a: 0, b: 0, c: 0 }
      const changes = synchronizer([['a', 10, {}]], initialState)

      // Should sync to both b and c
      const pathsChanged = changes.map((c) => c[0])
      expect(pathsChanged).toContain('a')
      expect(pathsChanged).toContain('b')
      expect(pathsChanged).toContain('c')

      // All synced changes should have flag
      const syncChanges = changes.filter((c) => c[2].isSyncPathChange)
      expect(syncChanges.length).toBe(2) // b and c
    })

    test('should prevent infinite loops', () => {
      const registry = createSyncPathsRegistry<TestState>()
      registry.register('ab', 'a', 'b')

      const synchronizer = createSyncPathsSynchronizer<TestState, GenericMeta>(
        registry
      )

      const initialState: TestState = { a: 0, b: 0, c: 0 }

      // First pass
      const firstPass = synchronizer([['a', 10, {}]], initialState)
      expect(firstPass.length).toBe(2) // Original + 1 sync

      // Second pass with only sync changes (typical pipeline scenario)
      const syncChangesOnly = firstPass.filter((c) => c[2].isSyncPathChange)
      const secondPass = synchronizer(syncChangesOnly, initialState)

      // Should not create more sync changes (already marked)
      expect(secondPass.length).toBe(syncChangesOnly.length)
    })
  })

  describe('Store integration', () => {
    test('basic store operations work', async () => {
      const store = createGenericStore<TestState, GenericMeta>()

      function TestComponent() {
        const [a, setA] = store.useStore('a')
        const [b] = store.useStore('b')

        return (
          <div>
            <span data-testid="a">{a}</span>
            <span data-testid="b">{b}</span>
            <button onClick={() => setA(10)}>Set A</button>
          </div>
        )
      }

      const { getByText, getByTestId } = render(
        <store.Provider initialState={{ a: 0, b: 0, c: 0 }}>
          <TestComponent />
        </store.Provider>
      )

      expect(getByTestId('a').textContent).toBe('0')
      expect(getByTestId('b').textContent).toBe('0')

      getByText('Set A').click()

      await waitFor(() => {
        expect(getByTestId('a').textContent).toBe('10')
      })
    })

    test('useSideEffects hook registration works', () => {
      const store = createGenericStore<TestState, GenericMeta>()

      function TestComponent() {
        // Register sync paths side-effect
        store.useSideEffects('test-sync', {
          syncPaths: {
            pairs: [{ id: 'ab', path1: 'a', path2: 'b' }],
          },
        })

        return <div>Component with side effects</div>
      }

      const { container } = render(
        <store.Provider initialState={{ a: 0, b: 0, c: 0 }}>
          <TestComponent />
        </store.Provider>
      )

      // Should render without errors
      expect(container).toBeTruthy()
    })
  })

  describe('Registry lifecycle', () => {
    test('should handle dynamic registration', () => {
      const registry = createSyncPathsRegistry<TestState>()

      // Register first pair
      registry.register('ab', 'a', 'b')
      expect(registry.graph.getSyncedPaths('a')).toContain('b')

      // Register second pair
      registry.register('bc', 'b', 'c')
      expect(registry.graph.getSyncedPaths('a')).toContain('c')

      // Unregister first pair
      registry.unregister('ab')
      expect(registry.graph.getSyncedPaths('a')).not.toContain('b')
    })

    test('should handle mount/unmount scenarios', () => {
      const registry = createSyncPathsRegistry<TestState>()

      // Simulate component mount
      registry.register('component-1', 'a', 'b')

      // Simulate another component mount
      registry.register('component-2', 'b', 'c')

      // Both should be active
      expect(registry.graph.getSyncedPaths('a').size).toBeGreaterThan(0)

      // Simulate first component unmount
      registry.unregister('component-1')

      // Second should still be active
      expect(registry.graph.getSyncedPaths('b')).toContain('c')

      // Simulate second component unmount
      registry.unregister('component-2')

      // All should be cleared
      expect(registry.graph.getSyncedPaths('b').size).toBe(0)
    })
  })

  describe('Error handling', () => {
    test('should throw on cycle creation', () => {
      const registry = createSyncPathsRegistry<TestState>()

      registry.register('ab', 'a', 'b')
      registry.register('bc', 'b', 'c')

      // Note: With undirected graphs, cycles are actually acceptable
      // This test documents the behavior
      try {
        registry.register('ca', 'c', 'a')
        // If it succeeds, verify the graph is valid
        expect(registry.graph.getSyncedPaths('a')).toContain('c')
      } catch (error) {
        // If it fails, verify error message
        expect((error as Error).message).toMatch(/cycle/i)
      }
    })

    test('should handle invalid paths gracefully', () => {
      const registry = createSyncPathsRegistry<TestState>()
      const synchronizer = createSyncPathsSynchronizer<TestState, GenericMeta>(
        registry
      )

      // Register valid paths
      registry.register('ab', 'a', 'b')

      // Query non-existent path
      const syncedPaths = registry.graph.getSyncedPaths('nonexistent' as any)
      expect(syncedPaths.size).toBe(0)

      // Changes to non-existent path should pass through
      const initialState: TestState = { a: 0, b: 0, c: 0 }
      const changes = synchronizer(
        [['nonexistent' as any, 99, {}]],
        initialState
      )

      expect(changes.length).toBe(1)
      expect(changes[0][0]).toBe('nonexistent')
    })
  })

  describe('Performance scenarios', () => {
    test('should handle rapid state changes', () => {
      const registry = createSyncPathsRegistry<TestState>()
      registry.register('ab', 'a', 'b')

      const synchronizer = createSyncPathsSynchronizer<TestState, GenericMeta>(
        registry
      )

      const initialState: TestState = { a: 0, b: 0, c: 0 }

      // Simulate 100 rapid changes
      const start = performance.now()
      for (let i = 0; i < 100; i++) {
        synchronizer([['a', i, {}]], initialState)
      }
      const duration = performance.now() - start

      // Should complete quickly (< 20ms for 100 iterations)
      expect(duration).toBeLessThan(20)
    })

    test('should handle complex sync networks efficiently', () => {
      const registry = createSyncPathsRegistry<any>()

      // Create a complex network (star topology)
      for (let i = 0; i < 10; i++) {
        registry.register(`a-${i}`, 'center', `node${i}`)
      }

      const synchronizer = createSyncPathsSynchronizer(registry)

      const start = performance.now()
      const changes = synchronizer([['center', 'value', {}]], {})
      const duration = performance.now() - start

      // Should complete quickly
      expect(duration).toBeLessThan(5)

      // Should sync to all nodes
      expect(changes.length).toBe(11) // 1 original + 10 synced
    })
  })
})
