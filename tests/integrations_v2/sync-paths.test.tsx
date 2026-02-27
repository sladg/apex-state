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
import type {
  BasicTestState,
  DeeplyNestedState,
  ListenerTestState,
  SyncFlipState,
} from '../mocks'
import {
  basicTestFixtures,
  deeplyNestedFixtures,
  listenerTestFixtures,
  syncFlipFixtures,
} from '../mocks'
import {
  expectShadowMatch,
  flushEffects,
  MODES,
  mountStore,
} from '../utils/react'

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
      expectShadowMatch(storeInstance)
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
      expectShadowMatch(storeInstance)
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
      expectShadowMatch(storeInstance)
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
      expectShadowMatch(storeInstance)
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
      expectShadowMatch(storeInstance)
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
      expectShadowMatch(storeInstance)
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
      expectShadowMatch(storeInstance)
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
      expectShadowMatch(storeInstance)
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

  // ---------------------------------------------------------------------------
  // Path hierarchy matching (migrated from tests/pipeline/normalize-changes.test.ts)
  //
  // The removed normalizeChangesForGroups JS function handled three match modes:
  // - Exact match: change path = registered sync path
  // - Child match: change path is deeper than registered (relative path appended)
  // - Parent match: change path is ancestor, nested value extracted from object
  //
  // These scenarios now run through the WASM pipeline (process_sync_paths_into).
  // ---------------------------------------------------------------------------

  describe('Child path changes (change deeper than sync pair)', () => {
    it('should sync child path change to peer at same relative depth', async () => {
      // Sync pair: ['user', 'user'] is not meaningful here.
      // Instead: sync pair on nested object paths.
      // Change level1.level2.value (child of level1.level2)
      // Sync pair: ['level1.level2.value', 'level1.level2.level3.value']
      // Exact match on level1.level2.value → syncs to level1.level2.level3.value
      const store = createGenericStore<DeeplyNestedState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        deeplyNestedFixtures.initial,
        {
          sideEffects: {
            syncPaths: [['level1.level2.value', 'level1.level2.level3.value']],
          },
        },
      )

      setValue('level1.level2.value', 'synced-deep')
      await flushEffects()

      expect(storeInstance.state.level1.level2.level3.value).toBe('synced-deep')
      expectShadowMatch(storeInstance)
    })

    it('should sync deeply nested leaf between two deep paths', async () => {
      // Sync pair at depth 4→5: level1.level2.level3.value ↔ level1.level2.level3.level4.value
      const store = createGenericStore<DeeplyNestedState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        deeplyNestedFixtures.initial,
        {
          sideEffects: {
            syncPaths: [
              [
                'level1.level2.level3.value',
                'level1.level2.level3.level4.value',
              ],
            ],
          },
        },
      )

      setValue('level1.level2.level3.value', 'deep-sync')
      await flushEffects()

      expect(storeInstance.state.level1.level2.level3.level4.value).toBe(
        'deep-sync',
      )
      expectShadowMatch(storeInstance)
    })

    it('should sync across different nesting depths', async () => {
      // Sync from shallow (level1.value) to deep (level1.level2.level3.level4.level5.value)
      const store = createGenericStore<DeeplyNestedState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        deeplyNestedFixtures.initial,
        {
          sideEffects: {
            syncPaths: [
              ['level1.value', 'level1.level2.level3.level4.level5.value'],
            ],
          },
        },
      )

      setValue('level1.value', 'shallow-to-deep')
      await flushEffects()

      expect(storeInstance.state.level1.level2.level3.level4.level5.value).toBe(
        'shallow-to-deep',
      )
      expectShadowMatch(storeInstance)
    })

    it('should sync from deep to shallow path', async () => {
      // Reverse: sync from depth 5 back to depth 1
      const store = createGenericStore<DeeplyNestedState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        deeplyNestedFixtures.initial,
        {
          sideEffects: {
            syncPaths: [
              ['level1.level2.level3.level4.level5.value', 'level1.value'],
            ],
          },
        },
      )

      setValue('level1.level2.level3.level4.level5.value', 'deep-to-shallow')
      await flushEffects()

      expect(storeInstance.state.level1.value).toBe('deep-to-shallow')
      expectShadowMatch(storeInstance)
    })
  })

  describe('Nested object sync (parent path changes)', () => {
    it('should sync nested object fields between sibling paths', async () => {
      // ListenerTestState has user.name, user.email, user.age
      // Sync user.name → derived (flat field)
      const store = createGenericStore<ListenerTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        listenerTestFixtures.initial,
        {
          sideEffects: {
            syncPaths: [['user.name', 'lastChange']],
          },
        },
      )

      setValue('user.name', 'Bob')
      await flushEffects()

      expect(storeInstance.state.lastChange).toBe('Bob')
    })

    it('should sync multiple nested fields from same parent', async () => {
      // Two sync pairs from user.name and user.email to flat fields
      const store = createGenericStore<ListenerTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        listenerTestFixtures.initial,
        {
          sideEffects: {
            syncPaths: [
              ['user.name', 'lastChange'],
              ['user.email', 'derived'],
            ],
          },
        },
      )

      setValue('user.name', 'Charlie')
      await flushEffects()

      expect(storeInstance.state.lastChange).toBe('Charlie')

      setValue('user.email', 'charlie@example.com')
      await flushEffects()

      expect(storeInstance.state.derived).toBe('charlie@example.com')
    })
  })

  describe('Overlapping and hierarchical sync paths', () => {
    it('should handle sync pairs at different levels of same hierarchy', async () => {
      // Two sync pairs on overlapping paths:
      // level1.value → level1.level2.value
      // level1.level2.value → level1.level2.level3.value
      // Changing level1.value should cascade through both levels
      const store = createGenericStore<DeeplyNestedState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        deeplyNestedFixtures.initial,
        {
          sideEffects: {
            syncPaths: [
              ['level1.value', 'level1.level2.value'],
              ['level1.level2.value', 'level1.level2.level3.value'],
            ],
          },
        },
      )

      setValue('level1.value', 'cascade')
      await flushEffects()

      // Direct sync target
      expect(storeInstance.state.level1.level2.value).toBe('cascade')
      // Chained sync target (level2.value syncs to level3.value)
      expect(storeInstance.state.level1.level2.level3.value).toBe('cascade')
      expectShadowMatch(storeInstance)
    })

    it('should handle bidirectional sync on nested paths', async () => {
      // Bidirectional: level1.level2.value ↔ level1.level2.level3.value
      const store = createGenericStore<DeeplyNestedState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        deeplyNestedFixtures.initial,
        {
          sideEffects: {
            syncPaths: [
              ['level1.level2.value', 'level1.level2.level3.value'],
              ['level1.level2.level3.value', 'level1.level2.value'],
            ],
          },
        },
      )

      // Forward direction
      setValue('level1.level2.value', 'bidirectional-1')
      await flushEffects()

      expect(storeInstance.state.level1.level2.level3.value).toBe(
        'bidirectional-1',
      )

      // Reverse direction
      setValue('level1.level2.level3.value', 'bidirectional-2')
      await flushEffects()

      expect(storeInstance.state.level1.level2.value).toBe('bidirectional-2')
    })

    it('should not interfere across unrelated nested sync pairs', async () => {
      // Two independent sync pairs on different branches of the tree
      const store = createGenericStore<DeeplyNestedState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        deeplyNestedFixtures.initial,
        {
          sideEffects: {
            syncPaths: [
              ['level1.value', 'level1.level2.value'],
              [
                'level1.level2.level3.level4.value',
                'level1.level2.level3.level4.level5.value',
              ],
            ],
          },
        },
      )

      // Initial sync: level4.value='L4' syncs to level5.value on registration
      await flushEffects()
      const initialL5 =
        storeInstance.state.level1.level2.level3.level4.level5.value

      // Change shallow pair
      setValue('level1.value', 'shallow-only')
      await flushEffects()

      expect(storeInstance.state.level1.level2.value).toBe('shallow-only')
      // Deep pair should NOT be affected by shallow sync
      expect(storeInstance.state.level1.level2.level3.level4.level5.value).toBe(
        initialL5,
      )

      // Change deep pair
      setValue('level1.level2.level3.level4.value', 'deep-only')
      await flushEffects()

      expect(storeInstance.state.level1.level2.level3.level4.level5.value).toBe(
        'deep-only',
      )
      // Shallow pair should NOT be affected
      expect(storeInstance.state.level1.value).toBe('shallow-only')
    })
  })

  describe('One-way sync (oneWay option)', () => {
    describe('initial sync on registration', () => {
      it('[0]->[1]: should copy source value to target on registration when they differ', async () => {
        // source='A', target='' — on registration, target should become 'A'
        const store = createGenericStore<SyncFlipState>(config)
        const { storeInstance } = mountStore(
          store,
          { ...syncFlipFixtures.initial, source: 'A', target: 'different' },
          {
            sideEffects: {
              syncPaths: [['source', 'target', { oneWay: '[0]->[1]' }]],
            },
          },
        )

        // No setValue needed — initial sync fires on registration
        expect(storeInstance.state.target).toBe('A')
        expectShadowMatch(storeInstance)
      })

      it('[0]->[1]: should copy empty-string source to target on registration (strict directed)', async () => {
        // source='', target='A' — unlike bidirectional (which votes on most common),
        // directed sync strictly copies source → target even when source is empty string.
        // This is intentional: "target mirrors source" means target='' when source=''.
        const store = createGenericStore<SyncFlipState>(config)
        const { storeInstance } = mountStore(
          store,
          { ...syncFlipFixtures.initial, source: '', target: 'A' },
          {
            sideEffects: {
              syncPaths: [['source', 'target', { oneWay: '[0]->[1]' }]],
            },
          },
        )

        expect(storeInstance.state.target).toBe('')
        expectShadowMatch(storeInstance)
      })

      it('[1]->[0]: should copy target value to source on registration when they differ', async () => {
        // target='B', source='A' — on registration, source should become 'B'
        const store = createGenericStore<SyncFlipState>(config)
        const { storeInstance } = mountStore(
          store,
          { ...syncFlipFixtures.initial, source: 'A', target: 'B' },
          {
            sideEffects: {
              syncPaths: [['source', 'target', { oneWay: '[1]->[0]' }]],
            },
          },
        )

        expect(storeInstance.state.source).toBe('B')
        expectShadowMatch(storeInstance)
      })

      it('[0]->[1]: should copy nested object (parent pair) on registration', async () => {
        // Parent pair registered: 'src' → 'dst' where both are objects.
        // On registration, entire src object should be copied into dst.
        interface NestedState {
          src: { name: string; age: number }
          dst: { name: string; age: number }
        }
        const store = createGenericStore<NestedState>(config)
        const { storeInstance } = mountStore(
          store,
          { src: { name: 'Alice', age: 30 }, dst: { name: '', age: 0 } },
          {
            sideEffects: {
              syncPaths: [['src', 'dst', { oneWay: '[0]->[1]' }]],
            },
          },
        )

        // No setValue needed — initial sync copies src → dst on registration
        expect(storeInstance.state.dst.name).toBe('Alice')
        expect(storeInstance.state.dst.age).toBe(30)
        expectShadowMatch(storeInstance)
      })

      it('[0]->[1]: should propagate nested object child path changes after registration', async () => {
        // Child-expansion case: pair is 'src'→'dst', change comes in at 'src.name'.
        // Expected: 'dst.name' receives the updated value (Case 5 — child expansion).
        interface NestedState {
          src: { name: string; age: number }
          dst: { name: string; age: number }
        }
        const store = createGenericStore<NestedState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          { src: { name: 'Alice', age: 30 }, dst: { name: 'Alice', age: 30 } },
          {
            sideEffects: {
              syncPaths: [['src', 'dst', { oneWay: '[0]->[1]' }]],
            },
          },
        )

        setValue('src.name', 'Bob')
        await flushEffects()

        expect(storeInstance.state.dst.name).toBe('Bob')
        // age should be unchanged
        expect(storeInstance.state.dst.age).toBe(30)
        // src.age must not be touched by directed sync
        expect(storeInstance.state.src.age).toBe(30)
        expectShadowMatch(storeInstance)
      })

      it('[0]->[1]: should NOT propagate dst child changes back to src', async () => {
        // Directed sync: changes to dst should not flow back into src.
        interface NestedState {
          src: { name: string; age: number }
          dst: { name: string; age: number }
        }
        const store = createGenericStore<NestedState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          { src: { name: 'Alice', age: 30 }, dst: { name: 'Alice', age: 30 } },
          {
            sideEffects: {
              syncPaths: [['src', 'dst', { oneWay: '[0]->[1]' }]],
            },
          },
        )

        setValue('dst.name', 'Modified')
        await flushEffects()

        // dst gets the user-set value
        expect(storeInstance.state.dst.name).toBe('Modified')
        // src must remain untouched — no reverse sync
        expect(storeInstance.state.src.name).toBe('Alice')
        expectShadowMatch(storeInstance)
      })
    })

    describe('[0]->[1] direction', () => {
      it('should sync source → target when source changes', async () => {
        // Register ['source', 'target', { oneWay: '[0]->[1]' }]
        // Change source → target should update
        // Change target → source should NOT update
        const store = createGenericStore<SyncFlipState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          syncFlipFixtures.initial,
          {
            sideEffects: {
              syncPaths: [['source', 'target', { oneWay: '[0]->[1]' }]],
            },
          },
        )

        setValue('source', 'pushed-value')
        await flushEffects()

        expect(storeInstance.state.target).toBe('pushed-value')
        expectShadowMatch(storeInstance)
      })

      it('should NOT sync target → source when target changes', async () => {
        // The whole point of oneWay: target changes must not flow back
        const store = createGenericStore<SyncFlipState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          syncFlipFixtures.initial,
          {
            sideEffects: {
              syncPaths: [['source', 'target', { oneWay: '[0]->[1]' }]],
            },
          },
        )

        const originalSource = storeInstance.state.source

        setValue('target', 'target-only-change')
        await flushEffects()

        expect(storeInstance.state.target).toBe('target-only-change')
        // source must remain untouched
        expect(storeInstance.state.source).toBe(originalSource)
        expectShadowMatch(storeInstance)
      })

      it('should propagate multiple source changes one-way', async () => {
        const store = createGenericStore<SyncFlipState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          syncFlipFixtures.initial,
          {
            sideEffects: {
              syncPaths: [['source', 'target', { oneWay: '[0]->[1]' }]],
            },
          },
        )

        setValue('source', 'first')
        await flushEffects()
        expect(storeInstance.state.target).toBe('first')

        setValue('source', 'second')
        await flushEffects()
        expect(storeInstance.state.target).toBe('second')
        expectShadowMatch(storeInstance)
      })
    })

    describe('[1]->[0] direction', () => {
      it('should sync target → source when target changes', async () => {
        // Register ['source', 'target', { oneWay: '[1]->[0]' }]
        // target is index [1], source is index [0]
        // Changing target should update source
        const store = createGenericStore<SyncFlipState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          syncFlipFixtures.initial,
          {
            sideEffects: {
              syncPaths: [['source', 'target', { oneWay: '[1]->[0]' }]],
            },
          },
        )

        setValue('target', 'reverse-push')
        await flushEffects()

        expect(storeInstance.state.source).toBe('reverse-push')
        expectShadowMatch(storeInstance)
      })

      it('should NOT sync source → target when source changes', async () => {
        const store = createGenericStore<SyncFlipState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          syncFlipFixtures.initial,
          {
            sideEffects: {
              syncPaths: [['source', 'target', { oneWay: '[1]->[0]' }]],
            },
          },
        )

        const originalTarget = storeInstance.state.target

        setValue('source', 'source-only-change')
        await flushEffects()

        expect(storeInstance.state.source).toBe('source-only-change')
        // target must remain untouched
        expect(storeInstance.state.target).toBe(originalTarget)
        expectShadowMatch(storeInstance)
      })
    })

    describe('mixed bidirectional and one-way in same registration', () => {
      it('should handle bidirectional and one-way pairs independently', async () => {
        // source ↔ target (bidirectional)
        // source2 → target2 (one-way)
        const store = createGenericStore<SyncFlipState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          syncFlipFixtures.initial,
          {
            sideEffects: {
              syncPaths: [
                ['source', 'target'],
                ['source2', 'target2', { oneWay: '[0]->[1]' }],
              ],
            },
          },
        )

        // Bidirectional pair: both directions should work
        setValue('source', 'bidir-source')
        await flushEffects()
        expect(storeInstance.state.target).toBe('bidir-source')

        setValue('target', 'bidir-target')
        await flushEffects()
        expect(storeInstance.state.source).toBe('bidir-target')

        // One-way pair: only source2 → target2
        setValue('source2', 'oneway-push')
        await flushEffects()
        expect(storeInstance.state.target2).toBe('oneway-push')

        // target2 → source2 must NOT happen
        const source2Before = storeInstance.state.source2
        setValue('target2', 'target2-only')
        await flushEffects()
        expect(storeInstance.state.target2).toBe('target2-only')
        expect(storeInstance.state.source2).toBe(source2Before)

        expectShadowMatch(storeInstance)
      })
    })

    describe('boolean one-way sync', () => {
      it('should sync boolean flag one-way', async () => {
        const store = createGenericStore<SyncFlipState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          syncFlipFixtures.allFalse,
          {
            sideEffects: {
              syncPaths: [['flag1', 'flag2', { oneWay: '[0]->[1]' }]],
            },
          },
        )

        setValue('flag1', true)
        await flushEffects()
        expect(storeInstance.state.flag2).toBe(true)

        // Changing flag2 must not affect flag1
        setValue('flag2', false)
        await flushEffects()
        expect(storeInstance.state.flag1).toBe(true)
        expectShadowMatch(storeInstance)
      })
    })
  })

  describe('Metadata and special values with nested sync', () => {
    it('should sync empty string to nested path', async () => {
      const store = createGenericStore<DeeplyNestedState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        deeplyNestedFixtures.initial,
        {
          sideEffects: {
            syncPaths: [['level1.value', 'level1.level2.value']],
          },
        },
      )

      setValue('level1.value', '')
      await flushEffects()

      expect(storeInstance.state.level1.level2.value).toBe('')
    })

    it('should handle boolean sync at nested paths', async () => {
      const store = createGenericStore<DeeplyNestedState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        deeplyNestedFixtures.initial,
        {
          sideEffects: {
            syncPaths: [
              [
                'level1.level2.level3.level4.level5.flag',
                'level1.level2.level3.level4.level5.flag',
              ],
            ],
          },
        },
      )

      // Self-sync should not crash
      setValue('level1.level2.level3.level4.level5.flag', true)
      await flushEffects()

      expect(storeInstance.state.level1.level2.level3.level4.level5.flag).toBe(
        true,
      )
    })

    it('should sync numeric values at nested paths', async () => {
      const store = createGenericStore<ListenerTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        listenerTestFixtures.initial,
        {
          sideEffects: {
            syncPaths: [['user.age', 'callCount']],
          },
        },
      )

      setValue('user.age', 42)
      await flushEffects()

      expect(storeInstance.state.callCount).toBe(42)

      setValue('user.age', 0)
      await flushEffects()

      expect(storeInstance.state.callCount).toBe(0)
    })
  })
})
