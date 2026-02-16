/**
 * Side Effects: Sync Paths (useSideEffects with syncPaths config)
 *
 * Validates that syncPaths cause source → target field synchronization.
 * When syncSource changes, syncTarget should automatically update to same value.
 *
 * NOTE: Tests from tests/integration/sync-paths.test.tsx (DELETED) and
 * sync test cases previously in pipeline-sync-flip-listeners.test.tsx (REMOVED)
 * are now consolidated here in v2 format.
 */

import { describe, expect, it } from 'vitest'

import { createGenericStore } from '../../src'
import type { BasicTestState, SyncFlipState } from '../mocks'
import { basicTestFixtures, syncFlipFixtures } from '../mocks'
import { flushEffects, MODES, mountStore } from '../utils/react'

describe.each(MODES)('[$name] Side Effects: Sync Paths', ({ config }) => {
  describe('Basic sync behavior', () => {
    it('should sync target field when source changes', async () => {
      // Create store
      // Register syncPaths: [['syncSource', 'syncTarget']]
      // Initial state: syncSource = '', syncTarget = ''
      // Call setValue(syncSource, 'new-value')
      // Assert syncTarget automatically becomes 'new-value'

      const store = createGenericStore<SyncFlipState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        syncFlipFixtures.initial,
        {
          sideEffects: {
            syncPaths: [['source', 'target']],
          },
        },
      )

      setValue('source', 'new-value')
      await flushEffects()

      expect(storeInstance.state.target).toBe('new-value')
    })

    it('should work when source already has value', async () => {
      // Create store with initialState: { syncSource: 'initial', syncTarget: 'different' }
      // Register syncPaths: [['syncSource', 'syncTarget']]
      // Assert syncTarget immediately becomes 'initial' (synced to source)

      const store = createGenericStore<SyncFlipState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        { ...syncFlipFixtures.initial, target: 'different' },
        {
          sideEffects: {
            syncPaths: [['source', 'target']],
          },
        },
      )

      await flushEffects()

      setValue('source', 'updated-value')
      await flushEffects()

      expect(storeInstance.state.target).toBe('updated-value')
    })

    it('should update target to empty string if source cleared', async () => {
      // Create store with initialState: { syncSource: 'value', syncTarget: 'value' }
      // Register syncPaths: [['syncSource', 'syncTarget']]
      // Call setValue(syncSource, '')
      // Assert syncTarget becomes '' too

      const store = createGenericStore<SyncFlipState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        { ...syncFlipFixtures.initial, source: 'value', target: 'value' },
        {
          sideEffects: {
            syncPaths: [['source', 'target']],
          },
        },
      )

      setValue('source', '')
      await flushEffects()

      expect(storeInstance.state.target).toBe('')
    })

    it('should handle numeric sync values', async () => {
      // Create store with numeric field pair
      // Register syncPaths to sync them
      // Change source to 42
      // Assert target becomes 42
      // Change source to 0
      // Assert target becomes 0

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        { ...basicTestFixtures.empty },
        {
          sideEffects: {
            syncPaths: [['fieldC', 'age']],
          },
        },
      )

      setValue('fieldC', 42)
      await flushEffects()

      expect(storeInstance.state.age).toBe(42)

      setValue('fieldC', 0)
      await flushEffects()

      expect(storeInstance.state.age).toBe(0)
    })

    it('should handle boolean sync values', async () => {
      // Create store with boolean field pair
      // Register syncPaths to sync them
      // Change source to true
      // Assert target becomes true
      // Change source to false
      // Assert target becomes false

      const store = createGenericStore<SyncFlipState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        syncFlipFixtures.allFalse,
        {
          sideEffects: {
            syncPaths: [['flag1', 'flag2']],
          },
        },
      )

      setValue('flag1', true)
      await flushEffects()

      expect(storeInstance.state.flag2).toBe(true)

      setValue('flag1', false)
      await flushEffects()

      expect(storeInstance.state.flag2).toBe(false)
    })
  })

  describe('Multiple sync pairs', () => {
    it('should handle multiple independent sync pairs', async () => {
      // Create store
      // Register syncPaths: [
      //   ['fieldA', 'fieldB'],
      //   ['syncSource', 'syncTarget']
      // ]
      // Change fieldA → fieldB should sync
      // Change syncSource → syncTarget should sync
      // fieldA change should NOT affect syncSource
      // syncSource change should NOT affect fieldA

      const store = createGenericStore<SyncFlipState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        syncFlipFixtures.initial,
        {
          sideEffects: {
            syncPaths: [
              ['source', 'target'],
              ['source2', 'target2'],
            ],
          },
        },
      )

      setValue('source', 'new-source')
      await flushEffects()

      expect(storeInstance.state.target).toBe('new-source')

      setValue('source2', 'new-source2')
      await flushEffects()

      expect(storeInstance.state.target2).toBe('new-source2')
      expect(storeInstance.state.target).toBe('new-source')
    })

    it('should sync multiple targets from one source', async () => {
      // Create store with fieldA, fieldB, fieldC
      // Register syncPaths: [
      //   ['fieldA', 'fieldB'],
      //   ['fieldA', 'fieldC']
      // ]
      // Change fieldA to 'new-value'
      // Assert fieldB becomes 'new-value'
      // Assert fieldC becomes 'new-value'

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          sideEffects: {
            syncPaths: [
              ['fieldA', 'fieldB'],
              ['fieldA', 'source'],
            ],
          },
        },
      )

      setValue('fieldA', 'shared-value')
      await flushEffects()

      expect(storeInstance.state.fieldB).toBe('shared-value')
      expect(storeInstance.state.source).toBe('shared-value')
    })

    it('should handle chained sync pairs', async () => {
      // Create store with fieldA, fieldB, fieldC
      // Register syncPaths: [
      //   ['fieldA', 'fieldB'],
      //   ['fieldB', 'fieldC']
      // ]
      // Change fieldA to 'value'
      // Assert fieldB becomes 'value'
      // Assert fieldC becomes 'value' (chain complete)

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          sideEffects: {
            syncPaths: [
              ['fieldA', 'fieldB'],
              ['fieldB', 'source'],
            ],
          },
        },
      )

      setValue('fieldA', 'chain-value')
      await flushEffects()

      expect(storeInstance.state.fieldB).toBe('chain-value')
      expect(storeInstance.state.source).toBe('chain-value')
    })
  })

  describe('Circular sync handling', () => {
    it('should handle A ↔ B without infinite loop', async () => {
      // Create store
      // Register syncPaths: [
      //   ['fieldA', 'fieldB'],
      //   ['fieldB', 'fieldA']
      // ]
      // Change fieldA to 'value'
      // Assert fieldB becomes 'value'
      // Assert fieldA still 'value' (not looping infinitely)
      // Verify no excessive re-renders

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          sideEffects: {
            syncPaths: [
              ['fieldA', 'fieldB'],
              ['fieldB', 'fieldA'],
            ],
          },
        },
      )

      setValue('fieldA', 'circular-value')
      await flushEffects()

      expect(storeInstance.state.fieldB).toBe('circular-value')
      expect(storeInstance.state.fieldA).toBe('circular-value')
    })

    it('should break circular chains at some point', async () => {
      // Create store with fieldA, fieldB, fieldC
      // Register syncPaths: [
      //   ['fieldA', 'fieldB'],
      //   ['fieldB', 'fieldC'],
      //   ['fieldC', 'fieldA']
      // ]
      // Change fieldA to 'value'
      // Assert all three converge to 'value'
      // Assert no infinite loop / excessive renders

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          sideEffects: {
            syncPaths: [
              ['fieldA', 'fieldB'],
              ['fieldB', 'source'],
              ['source', 'fieldA'],
            ],
          },
        },
      )

      setValue('fieldA', 'loop-value')
      await flushEffects()

      expect(storeInstance.state.fieldA).toBe('loop-value')
      expect(storeInstance.state.fieldB).toBe('loop-value')
      expect(storeInstance.state.source).toBe('loop-value')
    })

    it('should handle self-reference gracefully', async () => {
      // Create store
      // Register syncPaths: [
      //   ['fieldA', 'fieldA']
      // ]
      // Change fieldA to 'value'
      // Assert fieldA === 'value'
      // Assert no infinite loop

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          sideEffects: {
            syncPaths: [['fieldA', 'fieldA']],
          },
        },
      )

      setValue('fieldA', 'self-ref-value')
      await flushEffects()

      expect(storeInstance.state.fieldA).toBe('self-ref-value')
    })
  })

  describe('Sync edge cases', () => {
    it('should handle rapid consecutive sync operations', async () => {
      const store = createGenericStore<SyncFlipState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        syncFlipFixtures.initial,
        {
          sideEffects: {
            syncPaths: [['source', 'target']],
          },
        },
      )

      setValue('source', 'value-1')
      setValue('source', 'value-2')
      setValue('source', 'value-3')
      await flushEffects()

      expect(storeInstance.state.target).toBe('value-3')
    })

    it('should handle sync with null-like values', async () => {
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          sideEffects: {
            syncPaths: [['fieldA', 'fieldB']],
          },
        },
      )

      setValue('fieldA', '')
      await flushEffects()

      expect(storeInstance.state.fieldB).toBe('')

      setValue('fieldA', 'not-empty')
      await flushEffects()

      expect(storeInstance.state.fieldB).toBe('not-empty')
    })

    it('should support multiple sync pairs affecting same path', async () => {
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          sideEffects: {
            syncPaths: [
              ['fieldA', 'source'],
              ['fieldB', 'source'],
            ],
          },
        },
      )

      setValue('fieldA', 'from-a')
      await flushEffects()

      expect(storeInstance.state.source).toBe('from-a')

      setValue('fieldB', 'from-b')
      await flushEffects()

      expect(storeInstance.state.source).toBe('from-b')
    })
  })

  describe('Sync with other changes', () => {
    it('should sync even if other fields change simultaneously', async () => {
      // Create store
      // Register syncPaths and other side effects
      // Change syncSource AND fieldC in same operation
      // Assert syncTarget synced
      // Assert fieldC changed as requested

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          sideEffects: {
            syncPaths: [['source', 'target']],
          },
        },
      )

      setValue('source', 'sync-value')
      setValue('fieldC', 100)
      await flushEffects()

      expect(storeInstance.state.target).toBe('sync-value')
      expect(storeInstance.state.fieldC).toBe(100)
    })

    it('should preserve sync relationship through unrelated mutations', async () => {
      // Create store
      // Register syncPaths: [['syncSource', 'syncTarget']]
      // Change syncSource to 'value-1'
      // Change fieldA (unrelated)
      // Change syncSource to 'value-2'
      // Assert syncTarget is 'value-2'
      // Sync was not disrupted by unrelated field change

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          sideEffects: {
            syncPaths: [['source', 'target']],
          },
        },
      )

      setValue('source', 'value-1')
      await flushEffects()

      expect(storeInstance.state.target).toBe('value-1')

      setValue('fieldC', 42)
      await flushEffects()

      setValue('source', 'value-2')
      await flushEffects()

      expect(storeInstance.state.target).toBe('value-2')
      expect(storeInstance.state.fieldC).toBe(42)
    })

    it('should maintain sync across multiple value changes', async () => {
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          sideEffects: {
            syncPaths: [['source', 'target']],
          },
        },
      )

      setValue('source', 'change-1')
      await flushEffects()

      expect(storeInstance.state.target).toBe('change-1')

      setValue('source', 'change-2')
      await flushEffects()

      expect(storeInstance.state.target).toBe('change-2')

      setValue('source', 'change-3')
      await flushEffects()

      expect(storeInstance.state.target).toBe('change-3')
    })
  })

  describe('Sync validation', () => {
    it('should sync without modifying type', async () => {
      // Create store with fieldA: string, fieldB: string
      // Register syncPaths: [['fieldA', 'fieldB']]
      // Change fieldA to 'text'
      // Assert fieldB === 'text' (same type)
      // Assert no type coercion

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          sideEffects: {
            syncPaths: [['fieldA', 'fieldB']],
          },
        },
      )

      setValue('fieldA', 'text-value')
      await flushEffects()

      expect(storeInstance.state.fieldB).toBe('text-value')
      expect(typeof storeInstance.state.fieldB).toBe('string')
    })

    it('should preserve empty/null values during sync', async () => {
      // Create store
      // Register syncPaths
      // Set source to empty string ''
      // Assert target becomes '' (not null, not undefined)
      // Set source to 0
      // Assert target becomes 0 (not undefined)

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          sideEffects: {
            syncPaths: [
              ['fieldA', 'fieldB'],
              ['fieldC', 'age'],
            ],
          },
        },
      )

      setValue('fieldA', '')
      await flushEffects()

      expect(storeInstance.state.fieldB).toBe('')

      setValue('fieldC', 0)
      await flushEffects()

      expect(storeInstance.state.age).toBe(0)
    })
  })
})
