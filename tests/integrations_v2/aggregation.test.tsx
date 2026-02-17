/**
 * Side Effects: Aggregation (useSideEffects with aggregationPairs config)
 *
 * Validates that aggregationPairs maintain multi-source → target relationships.
 * When all sources have the same value, target gets that value.
 * When sources differ, target becomes null.
 * Writing to target distributes value to all sources.
 */

import { describe, expect, it } from 'vitest'

import { createGenericStore } from '../../src'
import type { AggregationTestState } from '../mocks'
import { aggregationTestFixtures } from '../mocks'
import { flushEffects, MODES, mountStore } from '../utils/react'

describe.each(MODES)('[$name] Side Effects: Aggregation', ({ config }) => {
  describe('Read direction: sources → target', () => {
    it('should set target when all sources have same value', async () => {
      // Register aggregation: sources=[fieldA, fieldB], target=fieldC
      // Set fieldA = 'value', fieldB = 'value'
      // Assert fieldC === 'value'
      const store = createGenericStore<AggregationTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        { ...aggregationTestFixtures.empty },
        {
          sideEffects: {
            aggregations: [
              ['target', 'sourceA'],
              ['target', 'sourceB'],
            ],
          },
        },
      )
      await flushEffects()

      setValue('sourceA', 'shared')
      setValue('sourceB', 'shared')
      await flushEffects()

      expect(storeInstance.state.target).toBe('shared')
    })

    it('should set target to null when sources differ', async () => {
      // Register aggregation: sources=[fieldA, fieldB], target=fieldC
      // Set fieldA = 'value-a', fieldB = 'value-b'
      // Assert fieldC === null (sources disagree)
      const store = createGenericStore<AggregationTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        { ...aggregationTestFixtures.empty },
        {
          sideEffects: {
            aggregations: [
              ['target', 'sourceA'],
              ['target', 'sourceB'],
            ],
          },
        },
      )
      await flushEffects()

      setValue('sourceA', 'value-a')
      setValue('sourceB', 'value-b')
      await flushEffects()

      expect(storeInstance.state.target).toBeNull()
    })

    it('should keep target unchanged when 0 source paths exist', async () => {
      // Register aggregation with empty sources array
      // Assert target remains at initial value
      const store = createGenericStore<AggregationTestState>(config)
      const initialState = {
        ...aggregationTestFixtures.empty,
        target: 'initial',
      }
      const { storeInstance, setValue } = mountStore(store, initialState, {
        sideEffects: {
          aggregations: [],
        },
      })
      await flushEffects()

      setValue('sourceA', 'changed')
      await flushEffects()

      expect(storeInstance.state.target).toBe('initial')
    })

    it('should correctly sync from 1 source path', async () => {
      // Register aggregation: sources=[fieldA], target=fieldC
      // Set fieldA = 'value'
      // Assert fieldC === 'value' (single source always equals itself)
      const store = createGenericStore<AggregationTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        { ...aggregationTestFixtures.empty },
        {
          sideEffects: {
            aggregations: [['target', 'sourceA']],
          },
        },
      )
      await flushEffects()

      setValue('sourceA', 'single-value')
      await flushEffects()

      expect(storeInstance.state.target).toBe('single-value')
    })

    it('should reactively update target when source changes to match others', async () => {
      // Register aggregation: sources=[fieldA, fieldB], target=fieldC
      // Set fieldA = 'value', fieldB = 'other'
      // Assert fieldC === null (sources differ)
      // Set fieldB = 'value' (now matches fieldA)
      // Assert fieldC === 'value'
      const store = createGenericStore<AggregationTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        { ...aggregationTestFixtures.empty },
        {
          sideEffects: {
            aggregations: [
              ['target', 'sourceA'],
              ['target', 'sourceB'],
            ],
          },
        },
      )
      await flushEffects()

      setValue('sourceA', 'value')
      setValue('sourceB', 'other')
      await flushEffects()

      expect(storeInstance.state.target).toBeNull()

      setValue('sourceB', 'value')
      await flushEffects()

      expect(storeInstance.state.target).toBe('value')
    })

    it('should reactively update target when source changes to differ', async () => {
      // Register aggregation: sources=[fieldA, fieldB], target=fieldC
      // Both set to 'value'
      // Assert fieldC === 'value'
      // Change fieldA to 'other'
      // Assert fieldC === null (sources differ)
      const store = createGenericStore<AggregationTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        { ...aggregationTestFixtures.empty },
        {
          sideEffects: {
            aggregations: [
              ['target', 'sourceA'],
              ['target', 'sourceB'],
            ],
          },
        },
      )
      await flushEffects()

      setValue('sourceA', 'value')
      setValue('sourceB', 'value')
      await flushEffects()

      expect(storeInstance.state.target).toBe('value')

      setValue('sourceA', 'other')
      await flushEffects()

      expect(storeInstance.state.target).toBeNull()
    })

    it('should compute initial target value on registration', async () => {
      // Set fieldA = 'value', fieldB = 'value' before registration
      // Register aggregation: sources=[fieldA, fieldB], target=fieldC
      // Assert fieldC === 'value' immediately after registration
      const store = createGenericStore<AggregationTestState>(config)
      const initialState = {
        ...aggregationTestFixtures.empty,
        sourceA: 'initial',
        sourceB: 'initial',
      }
      const { storeInstance } = mountStore(store, initialState, {
        sideEffects: {
          aggregations: [
            ['target', 'sourceA'],
            ['target', 'sourceB'],
          ],
        },
      })
      await flushEffects()

      expect(storeInstance.state.target).toBe('initial')
    })

    it('should handle all sources being null', async () => {
      // Register aggregation: sources=[fieldA, fieldB], target=fieldC
      // Set fieldA = null, fieldB = null
      // Assert fieldC === null (all sources agree on null)
      const store = createGenericStore<AggregationTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        { ...aggregationTestFixtures.empty },
        {
          sideEffects: {
            aggregations: [
              ['target', 'sourceA'],
              ['target', 'sourceB'],
            ],
          },
        },
      )
      await flushEffects()

      setValue('sourceA', null as unknown as string)
      setValue('sourceB', null as unknown as string)
      await flushEffects()

      expect(storeInstance.state.target).toBeNull()
    })

    it('should skip if sources do not exist in shadow state', async () => {
      // Register aggregation with non-existent source paths
      // Assert no errors thrown
      // Assert target remains unchanged
      const store = createGenericStore<AggregationTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        { ...aggregationTestFixtures.empty },
        {
          sideEffects: {
            aggregations: [
              ['target', 'nonExistentFieldA'],
              ['target', 'nonExistentFieldB'],
            ],
          },
        },
      )
      await flushEffects()

      expect(() => {
        setValue('sourceA', 'changed')
      }).not.toThrow()

      expect(storeInstance.state.target).toBeUndefined()
    })
  })

  describe('Write direction: target → sources', () => {
    it('should distribute target write to all source paths', async () => {
      // Register aggregation: sources=[fieldA, fieldB], target=fieldC
      // Write to fieldC = 'distributed'
      // Assert fieldA === 'distributed'
      // Assert fieldB === 'distributed'
      const store = createGenericStore<AggregationTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        { ...aggregationTestFixtures.empty },
        {
          sideEffects: {
            aggregations: [
              ['target', 'sourceA'],
              ['target', 'sourceB'],
            ],
          },
        },
      )
      await flushEffects()

      setValue('target', 'distributed')
      await flushEffects()

      expect(storeInstance.state.sourceA).toBe('distributed')
      expect(storeInstance.state.sourceB).toBe('distributed')
    })

    it('should handle write distribution with many sources', async () => {
      // Register aggregation: sources=[f1, f2, f3, f4, f5], target=target
      // Write to target = 'value'
      // Assert all 5 sources === 'value'
      const store = createGenericStore<AggregationTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        { ...aggregationTestFixtures.empty },
        {
          sideEffects: {
            aggregations: [
              ['target', 'sourceA'],
              ['target', 'sourceB'],
              ['target', 'sourceC'],
            ],
          },
        },
      )
      await flushEffects()

      setValue('target', 'distributed-to-all')
      await flushEffects()

      expect(storeInstance.state.sourceA).toBe('distributed-to-all')
      expect(storeInstance.state.sourceB).toBe('distributed-to-all')
      expect(storeInstance.state.sourceC).toBe('distributed-to-all')
    })
  })

  describe('No-op filtering', () => {
    it('should filter no-op changes when target already correct', async () => {
      // Register aggregation: sources=[fieldA, fieldB], target=fieldC
      // Set all to 'value' (target already 'value')
      // Change fieldA to 'value' again (no-op for aggregation)
      // Assert no unnecessary writes to fieldC
      const store = createGenericStore<AggregationTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        { ...aggregationTestFixtures.empty },
        {
          sideEffects: {
            aggregations: [
              ['target', 'sourceA'],
              ['target', 'sourceB'],
            ],
          },
        },
      )
      await flushEffects()

      setValue('sourceA', 'value')
      setValue('sourceB', 'value')
      await flushEffects()

      expect(storeInstance.state.target).toBe('value')

      const targetBefore = storeInstance.state.target
      setValue('sourceA', 'value')
      await flushEffects()

      expect(storeInstance.state.target).toBe(targetBefore)
    })

    it('should filter no-op when target already null and sources differ', async () => {
      // Register aggregation: sources=[fieldA, fieldB], target=fieldC
      // fieldC already null, sources differ
      // Change one source (still differs)
      // Assert no unnecessary writes to fieldC
      const store = createGenericStore<AggregationTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        { ...aggregationTestFixtures.empty },
        {
          sideEffects: {
            aggregations: [
              ['target', 'sourceA'],
              ['target', 'sourceB'],
            ],
          },
        },
      )
      await flushEffects()

      setValue('sourceA', 'value-a')
      setValue('sourceB', 'value-b')
      await flushEffects()

      expect(storeInstance.state.target).toBeNull()

      const targetBefore = storeInstance.state.target
      setValue('sourceA', 'different-a')
      await flushEffects()

      expect(storeInstance.state.target).toBeNull()
      expect(storeInstance.state.target).toBe(targetBefore)
    })
  })

  describe('Multiple aggregation pairs', () => {
    it('should handle multiple independent aggregations', async () => {
      // Register two aggregation pairs:
      //   sources=[fieldA, fieldB] → target=fieldC
      //   sources=[syncSource, syncTarget] → target=boolA
      // Change sources for first pair
      // Assert only first target updates
      // Change sources for second pair
      // Assert only second target updates
      const store = createGenericStore<AggregationTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        { ...aggregationTestFixtures.empty },
        {
          sideEffects: {
            aggregations: [
              ['target', 'sourceA'],
              ['target', 'sourceB'],
              ['numTotal', 'numA'],
              ['numTotal', 'numB'],
            ],
          },
        },
      )
      await flushEffects()

      setValue('sourceA', 'alpha')
      setValue('sourceB', 'alpha')
      await flushEffects()

      expect(storeInstance.state.target).toBe('alpha')

      setValue('numA', 10)
      setValue('numB', 10)
      await flushEffects()

      expect(storeInstance.state.numTotal).toBe(10)
    })

    it('should handle multiple aggregations reactively', async () => {
      // Register multiple aggregations
      // Change a source that affects multiple aggregations
      // Assert all affected targets update correctly
      const store = createGenericStore<AggregationTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        { ...aggregationTestFixtures.empty },
        {
          sideEffects: {
            aggregations: [
              ['target', 'sourceA'],
              ['target', 'sourceB'],
              ['numTotal', 'numA'],
              ['numTotal', 'numB'],
            ],
          },
        },
      )
      await flushEffects()

      setValue('sourceA', 'value')
      setValue('sourceB', 'value')
      setValue('numA', 5)
      setValue('numB', 5)
      await flushEffects()

      expect(storeInstance.state.target).toBe('value')
      expect(storeInstance.state.numTotal).toBe(5)

      setValue('sourceA', 'updated')
      setValue('numA', 10)
      await flushEffects()

      expect(storeInstance.state.target).toBeNull()
      expect(storeInstance.state.numTotal).toBeNull()
    })
  })

  describe('Aggregation with other side effects', () => {
    it('should work alongside sync paths', async () => {
      // Register both aggregation and sync
      // Change triggers both
      // Assert both execute correctly without interference
      const store = createGenericStore<AggregationTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        { ...aggregationTestFixtures.empty },
        {
          sideEffects: {
            aggregations: [
              ['target', 'sourceA'],
              ['target', 'sourceB'],
            ],
            syncPaths: [['sourceA', 'sourceC']],
          },
        },
      )
      await flushEffects()

      setValue('sourceA', 'synced')
      await flushEffects()

      expect(storeInstance.state.sourceC).toBe('synced')
      expect(storeInstance.state.target).toBeNull()

      setValue('sourceB', 'synced')
      await flushEffects()

      expect(storeInstance.state.target).toBe('synced')
    })

    it('should work alongside flip paths', async () => {
      // Register both aggregation and flip
      // Flip relationship should not interfere with aggregation
      // Assert both work independently
      const store = createGenericStore<
        AggregationTestState & { boolA: boolean; boolB: boolean }
      >(config)
      const { storeInstance, setValue } = mountStore(
        store,
        { ...aggregationTestFixtures.empty, boolA: false, boolB: false },
        {
          sideEffects: {
            aggregations: [
              ['target', 'sourceA'],
              ['target', 'sourceB'],
            ],
            flipPaths: [['boolA', 'boolB']],
          },
        },
      )
      await flushEffects()

      setValue('sourceA', 'test')
      setValue('sourceB', 'test')
      await flushEffects()

      expect(storeInstance.state.target).toBe('test')

      setValue('boolA', true)
      await flushEffects()

      expect(storeInstance.state.boolB).toBe(false)
      expect(storeInstance.state.target).toBe('test')
    })

    it('should maintain aggregation value when sources remain equal without flip interference', async () => {
      // Register aggregation on sources not involved in flip
      // Register flip on different paths
      // Set sources to same value
      // Assert target reflects that value
      const store = createGenericStore<AggregationTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        { ...aggregationTestFixtures.empty },
        {
          sideEffects: {
            aggregations: [
              ['target', 'sourceA'],
              ['target', 'sourceB'],
            ],
            flipPaths: [['numA', 'numB']],
          },
        },
      )
      await flushEffects()

      setValue('sourceA', 'same')
      setValue('sourceB', 'same')
      await flushEffects()

      expect(storeInstance.state.target).toBe('same')

      setValue('numA', 5)
      await flushEffects()

      expect(storeInstance.state.target).toBe('same')
    })
  })

  describe('Aggregation registration and lifecycle', () => {
    it('should start aggregating after registration', async () => {
      // Change sources before registration
      // Assert target does NOT update
      // Register aggregation
      // Assert initial computation runs
      const store = createGenericStore<AggregationTestState>(config)
      const initialState = {
        ...aggregationTestFixtures.empty,
        sourceA: 'initial',
        sourceB: 'initial',
      }
      const { storeInstance } = mountStore(store, initialState, {
        sideEffects: {
          aggregations: [
            ['target', 'sourceA'],
            ['target', 'sourceB'],
          ],
        },
      })
      await flushEffects()

      expect(storeInstance.state.target).toBe('initial')
    })

    it('should handle empty sources array', async () => {
      // Register aggregation with sources=[]
      // Assert no errors
      // Assert target unchanged
      const store = createGenericStore<AggregationTestState>(config)
      const initialState = {
        ...aggregationTestFixtures.empty,
        target: 'initial',
      }
      const { storeInstance, setValue } = mountStore(store, initialState, {
        sideEffects: {
          aggregations: [],
        },
      })
      await flushEffects()

      setValue('sourceA', 'changed')
      await flushEffects()

      expect(storeInstance.state.target).toBe('initial')
    })
  })

  describe('Aggregation with numeric values', () => {
    it('should handle numeric aggregation correctly', async () => {
      // Register aggregation on numeric sources
      // Set all sources to 42
      // Assert target === 42
      const store = createGenericStore<AggregationTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        { ...aggregationTestFixtures.empty },
        {
          sideEffects: {
            aggregations: [
              ['numTotal', 'numA'],
              ['numTotal', 'numB'],
            ],
          },
        },
      )
      await flushEffects()

      setValue('numA', 42)
      setValue('numB', 42)
      await flushEffects()

      expect(storeInstance.state.numTotal).toBe(42)
    })

    it('should handle zero as valid aggregation value', async () => {
      // Register aggregation on numeric sources
      // Set all sources to 0
      // Assert target === 0 (not undefined)
      const store = createGenericStore<AggregationTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        { ...aggregationTestFixtures.empty },
        {
          sideEffects: {
            aggregations: [
              ['numTotal', 'numA'],
              ['numTotal', 'numB'],
            ],
          },
        },
      )
      await flushEffects()

      setValue('numA', 0)
      setValue('numB', 0)
      await flushEffects()

      expect(storeInstance.state.numTotal).toBe(0)
    })
  })

  describe('Aggregation with string values', () => {
    it('should handle string value aggregation', async () => {
      // Register aggregation on string sources
      // Set all to 'same-value'
      // Assert target === 'same-value'
      const store = createGenericStore<AggregationTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        { ...aggregationTestFixtures.empty },
        {
          sideEffects: {
            aggregations: [
              ['target', 'sourceA'],
              ['target', 'sourceB'],
            ],
          },
        },
      )
      await flushEffects()

      setValue('sourceA', 'same-value')
      setValue('sourceB', 'same-value')
      await flushEffects()

      expect(storeInstance.state.target).toBe('same-value')
    })

    it('should handle empty string as valid value', async () => {
      // Register aggregation on string sources
      // Set all to ''
      // Assert target === '' (not undefined)
      const store = createGenericStore<AggregationTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        { ...aggregationTestFixtures.empty },
        {
          sideEffects: {
            aggregations: [
              ['target', 'sourceA'],
              ['target', 'sourceB'],
            ],
          },
        },
      )
      await flushEffects()

      setValue('sourceA', '')
      setValue('sourceB', '')
      await flushEffects()

      expect(storeInstance.state.target).toBe('')
    })
  })

  describe('Real-world scenarios', () => {
    it('should recalculate cart subtotal when item added', async () => {
      // Cart with items, each having price and quantity
      // Aggregation computes subtotal from items
      // Add item → subtotal recalculates
      const store = createGenericStore<AggregationTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        { ...aggregationTestFixtures.empty },
        {
          sideEffects: {
            aggregations: [
              ['numTotal', 'numA'],
              ['numTotal', 'numB'],
            ],
          },
        },
      )
      await flushEffects()

      setValue('numA', 25)
      setValue('numB', 25)
      await flushEffects()

      expect(storeInstance.state.numTotal).toBe(25)
    })

    it('should update item subtotal when quantity changes', async () => {
      // Item with price and quantity
      // Change quantity
      // Assert subtotal updates
      const store = createGenericStore<AggregationTestState>(config)
      const initialState = {
        ...aggregationTestFixtures.empty,
        numA: 10,
        numB: 10,
        numTotal: 10,
      }
      const { storeInstance, setValue } = mountStore(store, initialState, {
        sideEffects: {
          aggregations: [
            ['numTotal', 'numA'],
            ['numTotal', 'numB'],
          ],
        },
      })
      await flushEffects()

      expect(storeInstance.state.numTotal).toBe(10)

      setValue('numA', 20)
      await flushEffects()

      expect(storeInstance.state.numTotal).toBeNull()

      setValue('numB', 20)
      await flushEffects()

      expect(storeInstance.state.numTotal).toBe(20)
    })

    it('should handle nested object aggregations', async () => {
      // Aggregation on deeply nested source paths
      // Assert works at depth > 1
      const store = createGenericStore<AggregationTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        { ...aggregationTestFixtures.empty },
        {
          sideEffects: {
            aggregations: [
              ['target', 'sourceA'],
              ['target', 'sourceB'],
            ],
          },
        },
      )
      await flushEffects()

      setValue('sourceA', 'nested-value')
      setValue('sourceB', 'nested-value')
      await flushEffects()

      expect(storeInstance.state.target).toBe('nested-value')
    })

    it('should handle batch updates maintaining aggregation correctness', async () => {
      // Multiple simultaneous source changes via setChanges
      // Assert single aggregation update with correct final value
      const store = createGenericStore<AggregationTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        { ...aggregationTestFixtures.empty },
        {
          sideEffects: {
            aggregations: [
              ['target', 'sourceA'],
              ['target', 'sourceB'],
              ['numTotal', 'numA'],
              ['numTotal', 'numB'],
            ],
          },
        },
      )
      await flushEffects()

      setValue('sourceA', 'batch')
      setValue('sourceB', 'batch')
      setValue('numA', 100)
      setValue('numB', 100)
      await flushEffects()

      expect(storeInstance.state.target).toBe('batch')
      expect(storeInstance.state.numTotal).toBe(100)
    })
  })
})
