/**
 * Valtio Integration Tests for Shadow State
 *
 * Tests the integration between shadow state trees and valtio proxy state.
 * Verifies automatic synchronization, subscription handling, and affected path tracking.
 */

import { proxy } from 'valtio/vanilla'
import { describe, expect, it, vi } from 'vitest'

import {
  createShadowState,
  getValue,
  hasPath,
  syncShadowTree,
  syncWithValtio,
} from '../../src/shadow-state'

describe('valtio integration', () => {
  describe('createShadowState', () => {
    it('should create shadow tree from valtio proxy', () => {
      const state = proxy({ count: 0, name: 'Alice' })
      const { tree, cleanup } = createShadowState(state)

      expect(tree).toBeDefined()
      expect(tree.root).toBeDefined()
      expect(tree.root.value).toEqual({ count: 0, name: 'Alice' })

      cleanup()
    })

    it('should sync shadow state when proxy changes via manual sync', () => {
      const state = proxy({ count: 0 })
      const { tree: originalTree, cleanup } = createShadowState(state)

      // Change proxy
      state.count = 42

      // Manual sync to get updated tree
      const tree = syncShadowTree(originalTree, state)

      // Shadow state should be updated
      const countValue = getValue(tree, 'count')
      expect(countValue).toBe(42)

      cleanup()
    })

    it('should sync nested object changes via manual sync', () => {
      interface TestState {
        user: {
          name: string
          profile: {
            age: number
          }
        }
      }

      const state = proxy<TestState>({
        user: {
          name: 'Alice',
          profile: {
            age: 30,
          },
        },
      })

      const { tree: originalTree, cleanup } = createShadowState(state)

      // Change nested value
      state.user.profile.age = 31

      // Manual sync to get updated tree
      const tree = syncShadowTree(originalTree, state)

      const ageValue = getValue(tree, 'user.profile.age')
      expect(ageValue).toBe(31)

      cleanup()
    })

    it('should sync array changes via manual sync', () => {
      interface TestState {
        todos: string[]
      }

      const state = proxy<TestState>({
        todos: ['Buy milk', 'Walk dog'],
      })

      const { tree: originalTree, cleanup } = createShadowState(state)

      // Change array element
      state.todos[0] = 'Buy bread'

      // Manual sync to get updated tree
      const tree = syncShadowTree(originalTree, state)

      const todoValue = getValue(tree, 'todos.0')
      expect(todoValue).toBe('Buy bread')

      cleanup()
    })

    it('should call onUpdate callback with affected paths', async () => {
      const state = proxy({ count: 0, name: 'Alice' })
      const onUpdate = vi.fn()

      const { cleanup } = createShadowState(state, { onUpdate })

      // Change proxy
      state.count = 42

      await new Promise((resolve) => setTimeout(resolve, 0))

      // Callback should have been called
      expect(onUpdate).toHaveBeenCalled()

      // Should include affected paths
      const [affectedPaths] = onUpdate.mock.calls[0] ?? []
      expect(affectedPaths).toBeDefined()
      expect(Array.isArray(affectedPaths)).toBe(true)

      cleanup()
    })

    it('should handle multiple changes via manual sync', () => {
      const state = proxy({ a: 1, b: 2, c: 3 })

      const { tree: originalTree, cleanup } = createShadowState(state)

      // Multiple changes
      state.a = 10
      state.b = 20
      state.c = 30

      // Manual sync to get updated tree
      const tree = syncShadowTree(originalTree, state)

      // All values should be updated
      expect(getValue(tree, 'a')).toBe(10)
      expect(getValue(tree, 'b')).toBe(20)
      expect(getValue(tree, 'c')).toBe(30)

      cleanup()
    })

    it('should support multiple independent shadow states', () => {
      const state1 = proxy({ id: 'store1', value: 1 })
      const state2 = proxy({ id: 'store2', value: 2 })

      const { tree: tree1orig, cleanup: cleanup1 } = createShadowState(state1)
      const { tree: tree2orig, cleanup: cleanup2 } = createShadowState(state2)

      // Change both
      state1.value = 10
      state2.value = 20

      // Manual sync both
      const tree1 = syncShadowTree(tree1orig, state1)
      const tree2 = syncShadowTree(tree2orig, state2)

      // Each shadow state should be independent
      expect(getValue(tree1, 'value')).toBe(10)
      expect(getValue(tree2, 'value')).toBe(20)

      cleanup1()
      cleanup2()
    })

    it('should cleanup subscription when cleanup is called', async () => {
      const state = proxy({ count: 0 })
      const onUpdate = vi.fn()

      const { cleanup } = createShadowState(state, { onUpdate })

      // Change before cleanup
      state.count = 1
      await new Promise((resolve) => setTimeout(resolve, 0))

      const callCountBefore = onUpdate.mock.calls.length

      // Cleanup
      cleanup()

      // Change after cleanup
      state.count = 2
      await new Promise((resolve) => setTimeout(resolve, 0))

      // Callback should not have been called again
      expect(onUpdate.mock.calls.length).toBe(callCountBefore)
    })

    it('should handle null and undefined values', () => {
      interface TestState {
        nullable: string | null
        optional?: number
      }

      const state = proxy<TestState>({
        nullable: null,
      })

      const { tree: originalTree, cleanup } = createShadowState(state)

      // Initial values
      expect(getValue(originalTree, 'nullable')).toBe(null)
      expect(hasPath(originalTree, 'optional')).toBe(true)

      // Update to non-null
      state.nullable = 'value'

      // Manual sync
      const tree = syncShadowTree(originalTree, state)

      expect(getValue(tree, 'nullable')).toBe('value')

      cleanup()
    })

    it('should handle empty objects', () => {
      const state = proxy({})
      const { tree, cleanup } = createShadowState(state)

      expect(tree.root.value).toEqual({})
      expect(tree.nodeCount).toBe(1) // Just root

      cleanup()
    })

    it('should handle deeply nested structures', () => {
      interface TestState {
        level1: {
          level2: {
            level3: {
              level4: {
                value: number
              }
            }
          }
        }
      }

      const state = proxy<TestState>({
        level1: {
          level2: {
            level3: {
              level4: {
                value: 42,
              },
            },
          },
        },
      })

      const { tree: originalTree, cleanup } = createShadowState(state)

      // Change deep value
      state.level1.level2.level3.level4.value = 100

      // Manual sync
      const tree = syncShadowTree(originalTree, state)

      const deepValue = getValue(tree, 'level1.level2.level3.level4.value')
      expect(deepValue).toBe(100)

      cleanup()
    })

    it('should respect maxDepth option', () => {
      const state = proxy({
        level1: {
          level2: {
            level3: {
              value: 'too deep',
            },
          },
        },
      })

      const { tree, cleanup } = createShadowState(state, { maxDepth: 2 })

      // Should have created nodes up to maxDepth
      expect(hasPath(tree, 'level1')).toBe(true)
      expect(hasPath(tree, 'level1.level2')).toBe(true)

      // Beyond maxDepth should not have individual child nodes
      // level3 should be part of level2's value but not a separate node
      const level2Value = getValue(tree, 'level1.level2')
      expect(level2Value).toHaveProperty('level3')

      cleanup()
    })

    it('should handle array mutations with push', async () => {
      const state = proxy({ items: [1, 2, 3] })
      const { tree, cleanup } = createShadowState(state)

      // Push new item
      state.items.push(4)

      await new Promise((resolve) => setTimeout(resolve, 0))

      const itemsValue = getValue(tree, 'items')
      expect(itemsValue).toEqual([1, 2, 3, 4])

      cleanup()
    })

    it('should handle array mutations with pop', async () => {
      const state = proxy({ items: [1, 2, 3] })
      const { tree, cleanup } = createShadowState(state)

      // Pop item
      state.items.pop()

      await new Promise((resolve) => setTimeout(resolve, 0))

      const itemsValue = getValue(tree, 'items')
      expect(itemsValue).toEqual([1, 2])

      cleanup()
    })

    it('should handle array mutations with splice', async () => {
      const state = proxy({ items: [1, 2, 3, 4, 5] })
      const { tree, cleanup } = createShadowState(state)

      // Splice: remove 2 items at index 1, add 99
      state.items.splice(1, 2, 99)

      await new Promise((resolve) => setTimeout(resolve, 0))

      const itemsValue = getValue(tree, 'items')
      expect(itemsValue).toEqual([1, 99, 4, 5])

      cleanup()
    })

    it('should pass tree to onUpdate callback', async () => {
      const state = proxy({ count: 0 })
      const onUpdate = vi.fn()

      const { cleanup } = createShadowState(state, { onUpdate })

      state.count = 42

      await new Promise((resolve) => setTimeout(resolve, 0))

      expect(onUpdate).toHaveBeenCalled()

      const [_affectedPaths, tree] = onUpdate.mock.calls[0] ?? []
      expect(tree).toBeDefined()
      expect(tree.root).toBeDefined()

      cleanup()
    })
  })

  describe('syncShadowTree', () => {
    it('should manually sync shadow tree with proxy state', () => {
      const state = proxy({ count: 0, name: 'Alice' })
      const { tree: oldTree, cleanup } = createShadowState(state)

      // Change proxy without waiting for subscription
      state.count = 100
      state.name = 'Bob'

      // Manual sync
      const newTree = syncShadowTree(oldTree, state)

      // New tree should have updated values
      expect(getValue(newTree, 'count')).toBe(100)
      expect(getValue(newTree, 'name')).toBe('Bob')

      cleanup()
    })

    it('should create fresh tree with full state', () => {
      const state = proxy({
        user: { name: 'Alice', age: 30 },
        todos: ['Buy milk'],
      })

      const { tree: tree1, cleanup } = createShadowState(state)

      // Make changes
      state.user.age = 31
      state.todos.push('Walk dog')

      // Manual sync creates a new tree
      const tree2 = syncShadowTree(tree1, state)

      // Trees should be different instances
      expect(tree2).not.toBe(tree1)

      // New tree should have all current state
      expect(getValue(tree2, 'user.age')).toBe(31)
      expect(getValue(tree2, 'todos')).toEqual(['Buy milk', 'Walk dog'])

      cleanup()
    })

    it('should respect options on manual sync', () => {
      const state = proxy({
        deep: {
          nested: {
            value: 123,
          },
        },
      })

      const { tree: tree1, cleanup } = createShadowState(state)

      // Sync with maxDepth limit
      const tree2 = syncShadowTree(tree1, state, { maxDepth: 1 })

      expect(hasPath(tree2, 'deep')).toBe(true)

      // level2+ should not have separate nodes
      const deepValue = getValue(tree2, 'deep')
      expect(deepValue).toHaveProperty('nested')

      cleanup()
    })
  })

  describe('syncWithValtio', () => {
    it('should be an alias for createShadowState', () => {
      const state = proxy({ value: 42 })
      const { tree, cleanup } = syncWithValtio(state)

      expect(tree).toBeDefined()
      expect(tree.root.value).toEqual({ value: 42 })

      cleanup()
    })

    it('should support same options as createShadowState', async () => {
      const state = proxy({ count: 0 })
      const onUpdate = vi.fn()

      const { cleanup } = syncWithValtio(state, { onUpdate })

      state.count = 1

      await new Promise((resolve) => setTimeout(resolve, 0))

      expect(onUpdate).toHaveBeenCalled()

      cleanup()
    })
  })

  describe('performance', () => {
    it('should sync small changes in reasonable time', () => {
      const state = proxy({ count: 0 })
      const { tree: originalTree, cleanup } = createShadowState(state)

      // Single change
      state.count = 100

      const start = performance.now()
      const tree = syncShadowTree(originalTree, state)
      const duration = performance.now() - start

      // Should complete quickly (within 10ms for manual sync)
      expect(duration).toBeLessThan(10)
      expect(getValue(tree, 'count')).toBe(100)

      cleanup()
    })

    it('should handle large trees efficiently', async () => {
      // Create a larger state object
      const largeState: Record<string, any> = {}
      for (let i = 0; i < 100; i++) {
        largeState[`field${i}`] = {
          value: i,
          nested: {
            data: `value-${i}`,
          },
        }
      }

      const state = proxy(largeState)

      const start = performance.now()
      const { tree, cleanup } = createShadowState(state)
      const duration = performance.now() - start

      // Tree creation should complete in reasonable time
      expect(duration).toBeLessThan(100)

      // Should have created the full tree
      expect(tree.nodeCount).toBeGreaterThan(100)

      cleanup()
    })

    it('should handle multiple rapid changes efficiently', () => {
      const state = proxy({ counter: 0 })
      const { tree: originalTree, cleanup } = createShadowState(state)

      // Rapid changes
      for (let i = 0; i < 10; i++) {
        state.counter = i
      }

      const start = performance.now()
      const tree = syncShadowTree(originalTree, state)
      const duration = performance.now() - start

      // Should handle rapid updates
      expect(duration).toBeLessThan(10)
      expect(getValue(tree, 'counter')).toBe(9)

      cleanup()
    })
  })

  describe('edge cases', () => {
    it('should handle adding new properties to proxy', async () => {
      const state = proxy<Record<string, any>>({ existing: 'value' })
      const { tree, cleanup } = createShadowState(state)

      // Add new property
      state['newProp'] = 'new value'

      await new Promise((resolve) => setTimeout(resolve, 0))

      // Shadow tree should reflect the new property
      const treeValue = getValue(tree, '')
      expect(treeValue).toHaveProperty('newProp')

      cleanup()
    })

    it('should handle deleting properties from proxy', async () => {
      const state = proxy<Record<string, any>>({
        keep: 'this',
        remove: 'that',
      })
      const { tree, cleanup } = createShadowState(state)

      // Delete property
      delete state['remove']

      await new Promise((resolve) => setTimeout(resolve, 0))

      // Shadow tree should reflect the deletion
      const treeValue = getValue(tree, '')
      expect(treeValue).not.toHaveProperty('remove')
      expect(treeValue).toHaveProperty('keep')

      cleanup()
    })

    it('should handle replacing entire subtrees', () => {
      interface TestState {
        user: {
          name: string
          age: number
        }
      }

      const state = proxy<TestState>({
        user: {
          name: 'Alice',
          age: 30,
        },
      })

      const { tree: originalTree, cleanup } = createShadowState(state)

      // Replace entire user object
      state.user = {
        name: 'Bob',
        age: 25,
      }

      // Manual sync
      const tree = syncShadowTree(originalTree, state)

      expect(getValue(tree, 'user.name')).toBe('Bob')
      expect(getValue(tree, 'user.age')).toBe(25)

      cleanup()
    })

    it('should handle circular reference detection', () => {
      const state: any = proxy({ name: 'root' })
      state.self = state // Circular reference

      const { tree, cleanup } = createShadowState(state, {
        detectCircular: true,
      })

      // Should create tree without infinite loop
      expect(tree).toBeDefined()
      expect(tree.root.value).toHaveProperty('name')

      cleanup()
    })

    it('should handle empty arrays', async () => {
      const state = proxy({ items: [] as number[] })
      const { tree, cleanup } = createShadowState(state)

      expect(getValue(tree, 'items')).toEqual([])

      // Add item
      state.items.push(1)

      await new Promise((resolve) => setTimeout(resolve, 0))

      expect(getValue(tree, 'items')).toEqual([1])

      cleanup()
    })

    it('should handle nested arrays', async () => {
      const state = proxy({
        matrix: [
          [1, 2],
          [3, 4],
        ],
      })

      const { tree, cleanup } = createShadowState(state)

      // Change nested array element
      state.matrix[0]![1] = 99

      await new Promise((resolve) => setTimeout(resolve, 0))

      const matrixValue = getValue(tree, 'matrix')
      expect(matrixValue).toEqual([
        [1, 99],
        [3, 4],
      ])

      cleanup()
    })

    it('should handle boolean and number values', () => {
      const state = proxy({
        isActive: true,
        count: 0,
        price: 9.99,
      })

      const { tree: originalTree, cleanup } = createShadowState(state)

      state.isActive = false
      state.count = 42
      state.price = 19.99

      // Manual sync
      const tree = syncShadowTree(originalTree, state)

      expect(getValue(tree, 'isActive')).toBe(false)
      expect(getValue(tree, 'count')).toBe(42)
      expect(getValue(tree, 'price')).toBe(19.99)

      cleanup()
    })

    it('should handle special characters in keys', async () => {
      const state = proxy({
        'key-with-dash': 'value1',
        'key.with.dot': 'value2',
        'key with space': 'value3',
      })

      const { tree, cleanup } = createShadowState(state)

      // Access with bracket notation
      const value1 = getValue(tree, 'key-with-dash')
      expect(value1).toBe('value1')

      cleanup()
    })
  })
})
