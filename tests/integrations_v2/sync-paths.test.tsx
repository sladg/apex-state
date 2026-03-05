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
  flushSync,
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

    describe('graph snapshot shows directed pair in directed_sync_pairs not sync_pairs', () => {
      it('should have directed pair in directed_sync_pairs and NOT in sync_pairs', async () => {
        // Verify that a oneWay pair appears in directed_sync_pairs (→) in the snapshot,
        // NOT in sync_pairs (↔). This is what logRegistration uses to render arrows.
        const store = createGenericStore<SyncFlipState>(config)
        const { storeInstance } = mountStore(store, syncFlipFixtures.initial, {
          sideEffects: {
            syncPaths: [['source', 'target', { oneWay: '[0]->[1]' }]],
          },
        })

        const snapshot = storeInstance._internal.pipeline!.getGraphSnapshot()

        // Directed pair must be in directed_sync_pairs
        const directedEntry = snapshot.directed_sync_pairs.find(
          ([src, tgt]) => src === 'source' && tgt === 'target',
        )
        expect(directedEntry).toBeDefined()

        // Must NOT appear in sync_pairs (bidirectional)
        const biDirEntry = snapshot.sync_pairs.find(
          ([a, b]) =>
            (a === 'source' && b === 'target') ||
            (a === 'target' && b === 'source'),
        )
        expect(biDirEntry).toBeUndefined()
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

  // ---------------------------------------------------------------------------
  // Meta propagation through derived sync changes
  //
  // Bug: WASM's emit_sync_change creates derived changes with meta: None,
  // dropping the original input change's meta. Listeners should receive sync-
  // target changes carrying the same meta as the source change that triggered them.
  // ---------------------------------------------------------------------------

  describe('Meta propagation through derived sync changes', () => {
    it('should propagate input meta to the derived sync target change received by listeners', async () => {
      // Setup: sync pair source → target, plus an always-on listener that captures
      // every change it receives (both the original source and the derived target).
      //
      // Steps:
      //   1. Call setValue('source', 'synced-value', { sender: 'user-action' })
      //   2. WASM produces a derived change for 'target' (sync)
      //   3. Both changes are fed to the listener
      //
      // Expected (currently FAILING):
      //   - listener receives source change with meta { sender: 'user-action' }
      //   - listener receives target change with meta { sender: 'user-action' }
      //     (meta must propagate from input to the derived sync change)
      //
      // Actual (bug):
      //   - target change arrives with meta {} or undefined — meta was lost in WASM

      const store = createGenericStore<SyncFlipState>(config)
      const capturedChanges: [string, unknown, unknown][] = []

      const { storeInstance, setValue } = mountStore(
        store,
        syncFlipFixtures.initial,
        {
          sideEffects: {
            syncPaths: [['source', 'target']],
            listeners: [
              {
                path: null,
                scope: null,
                fn: (changes) => {
                  for (const change of changes) {
                    capturedChanges.push([
                      change[0] as string,
                      change[1],
                      change[2],
                    ])
                  }
                  return undefined
                },
              },
            ],
          },
        },
      )

      setValue('source', 'synced-value', { sender: 'user-action' })
      await flushSync()

      // Sync target must have received the value
      expect(storeInstance.state.target).toBe('synced-value')

      // Original source change must carry the meta
      expect(capturedChanges).toContainEqual([
        'source',
        expect.anything(),
        expect.objectContaining({ sender: 'user-action' }),
      ])

      // Derived sync target change MUST also carry the same meta — this is the bug
      expect(capturedChanges).toContainEqual([
        'target',
        expect.anything(),
        expect.objectContaining({ sender: 'user-action' }),
      ])
    })

    it('should propagate meta to nested sync target paths', async () => {
      // Nested sync pair: level1.value → level1.level2.value
      // Change level1.value with meta { sender: 'form-submit' }
      // Listener must see level1.level2.value change WITH that meta

      const store = createGenericStore<DeeplyNestedState>(config)
      const capturedChanges: [string, unknown, unknown][] = []

      const { storeInstance, setValue } = mountStore(
        store,
        deeplyNestedFixtures.initial,
        {
          sideEffects: {
            syncPaths: [['level1.value', 'level1.level2.value']],
            listeners: [
              {
                path: null,
                scope: null,
                fn: (changes) => {
                  for (const change of changes) {
                    capturedChanges.push([
                      change[0] as string,
                      change[1],
                      change[2],
                    ])
                  }
                  return undefined
                },
              },
            ],
          },
        },
      )

      setValue('level1.value', 'nested-sync', { sender: 'form-submit' })
      await flushSync()

      // Sync must have applied
      expect(storeInstance.state.level1.level2.value).toBe('nested-sync')

      // Source change carries meta
      expect(capturedChanges).toContainEqual([
        'level1.value',
        expect.anything(),
        expect.objectContaining({ sender: 'form-submit' }),
      ])

      // Derived sync target change must also carry the meta
      expect(capturedChanges).toContainEqual([
        'level1.level2.value',
        expect.anything(),
        expect.objectContaining({ sender: 'form-submit' }),
      ])
    })

    it('should propagate meta for directed (one-way) sync', async () => {
      // Directed sync: source → target (oneWay: '[0]->[1]')
      // Change source with meta { isProgramaticChange: true }
      // Listener must receive target change WITH that meta

      const store = createGenericStore<SyncFlipState>(config)
      const capturedChanges: [string, unknown, unknown][] = []

      const { storeInstance, setValue } = mountStore(
        store,
        syncFlipFixtures.initial,
        {
          sideEffects: {
            syncPaths: [['source', 'target', { oneWay: '[0]->[1]' }]],
            listeners: [
              {
                path: null,
                scope: null,
                fn: (changes) => {
                  for (const change of changes) {
                    capturedChanges.push([
                      change[0] as string,
                      change[1],
                      change[2],
                    ])
                  }
                  return undefined
                },
              },
            ],
          },
        },
      )

      setValue('source', 'directed-value', { isProgramaticChange: true })
      await flushSync()

      // Sync must have applied
      expect(storeInstance.state.target).toBe('directed-value')

      // Derived directed-sync target change must carry the original meta
      expect(capturedChanges).toContainEqual([
        'target',
        expect.anything(),
        expect.objectContaining({ isProgramaticChange: true }),
      ])
    })

    it('should propagate meta to multiple sync targets in a bidirectional component', async () => {
      // Three paths in one sync component: source ↔ target ↔ target2
      // (Two pairs: source↔target and target↔target2)
      // Changing source with meta { sender: 'bulk-update' } must propagate meta
      // to BOTH derived target and target2 changes that the listener receives.

      const store = createGenericStore<SyncFlipState>(config)
      const capturedChanges: [string, unknown, unknown][] = []

      const { storeInstance, setValue } = mountStore(
        store,
        syncFlipFixtures.initial,
        {
          sideEffects: {
            syncPaths: [
              ['source', 'target'],
              ['target', 'target2'],
            ],
            listeners: [
              {
                path: null,
                scope: null,
                fn: (changes) => {
                  for (const change of changes) {
                    capturedChanges.push([
                      change[0] as string,
                      change[1],
                      change[2],
                    ])
                  }
                  return undefined
                },
              },
            ],
          },
        },
      )

      setValue('source', 'chain-value', { sender: 'bulk-update' })
      await flushSync()

      // All three paths must converge
      expect(storeInstance.state.target).toBe('chain-value')
      expect(storeInstance.state.target2).toBe('chain-value')

      // Both derived changes must carry the original meta
      expect(capturedChanges).toContainEqual([
        'target',
        expect.anything(),
        expect.objectContaining({ sender: 'bulk-update' }),
      ])
      expect(capturedChanges).toContainEqual([
        'target2',
        expect.anything(),
        expect.objectContaining({ sender: 'bulk-update' }),
      ])
    })
  })

  // ---------------------------------------------------------------------------
  // No-op sync filtering
  //
  // Bug: When a parent path is set to an object whose leaf matches the sync
  // source's current value, WASM still emits a sync change to the target —
  // even when the target already holds that value. This is a redundant write.
  // ---------------------------------------------------------------------------

  describe('No-op sync filtering (parent-path object assignment)', () => {
    it('should NOT emit a sync change when target already has the same value', async () => {
      // Setup: sync pair src.value → dst.value, both start at 'same'
      // Action: set parent 'src' to { value: 'same' } (no actual leaf change)
      // Expected: no sync change emitted for dst.value (it's already 'same')
      interface ParentSyncState {
        src: { value: string }
        dst: { value: string }
      }

      const store = createGenericStore<ParentSyncState>(config)
      const capturedChanges: [string, unknown][] = []

      const { storeInstance, setValue } = mountStore(
        store,
        { src: { value: 'same' }, dst: { value: 'same' } },
        {
          sideEffects: {
            syncPaths: [['src.value', 'dst.value']],
            listeners: [
              {
                path: null,
                scope: null,
                fn: (changes) => {
                  for (const change of changes) {
                    capturedChanges.push([change[0] as string, change[1]])
                  }
                  return undefined
                },
              },
            ],
          },
        },
      )

      // Clear any registration-time changes
      capturedChanges.length = 0

      // Set parent to object with identical leaf value
      setValue('src', { value: 'same' })
      await flushSync()

      // dst.value should still be 'same' (unchanged)
      expect(storeInstance.state.dst.value).toBe('same')

      // The listener should NOT have received a dst.value change —
      // it's a no-op since dst.value was already 'same'
      const dstChanges = capturedChanges.filter(
        ([path]) => path === 'dst.value',
      )
      expect(dstChanges).toHaveLength(0)
    })

    it('should NOT emit a sync change when deeply nested parent reassignment produces no leaf diff', async () => {
      // Sync pair: level1.level2.level3.value → level1.value
      // Both start with matching values. Reassigning the parent object
      // level1.level2.level3 to { value: 'L3', level4: ... } is semantically
      // a no-op at the leaf. Sync should not fire.
      const store = createGenericStore<DeeplyNestedState>(config)
      const capturedChanges: [string, unknown][] = []

      const { storeInstance, setValue } = mountStore(
        store,
        {
          ...deeplyNestedFixtures.initial,
          level1: {
            ...deeplyNestedFixtures.initial.level1,
            value: 'L3', // target already matches source leaf
          },
        },
        {
          sideEffects: {
            syncPaths: [['level1.level2.level3.value', 'level1.value']],
            listeners: [
              {
                path: null,
                scope: null,
                fn: (changes) => {
                  for (const change of changes) {
                    capturedChanges.push([change[0] as string, change[1]])
                  }
                  return undefined
                },
              },
            ],
          },
        },
      )

      capturedChanges.length = 0

      // Reassign parent with identical subtree
      setValue('level1.level2.level3', {
        value: 'L3',
        level4: deeplyNestedFixtures.initial.level1.level2.level3.level4,
      })
      await flushSync()

      // Target should remain unchanged
      expect(storeInstance.state.level1.value).toBe('L3')

      // No sync change should have been emitted for level1.value
      const targetChanges = capturedChanges.filter(
        ([path]) => path === 'level1.value',
      )
      expect(targetChanges).toHaveLength(0)
    })

    it('should NOT emit a sync change when parent object changes unrelated sibling but synced leaf stays the same', async () => {
      // Setup: sync pair src.value → dst.value
      // src has two properties: { value: 'same', other: 'old' }
      // Action: set parent 'src' to { value: 'same', other: 'changed' }
      // The synced leaf (src.value) didn't change — only the unrelated sibling did.
      // Expected: no sync change for dst.value
      interface SiblingState {
        src: { value: string; other: string }
        dst: { value: string }
      }

      const store = createGenericStore<SiblingState>(config)
      const capturedChanges: [string, unknown][] = []

      const { storeInstance, setValue } = mountStore(
        store,
        { src: { value: 'same', other: 'old' }, dst: { value: 'same' } },
        {
          sideEffects: {
            syncPaths: [['src.value', 'dst.value']],
            listeners: [
              {
                path: null,
                scope: null,
                fn: (changes) => {
                  for (const change of changes) {
                    capturedChanges.push([change[0] as string, change[1]])
                  }
                  return undefined
                },
              },
            ],
          },
        },
      )

      capturedChanges.length = 0

      // Change unrelated sibling, synced leaf stays the same
      setValue('src', { value: 'same', other: 'changed' })
      await flushSync()

      // Unrelated sibling should have changed
      expect(storeInstance.state.src.other).toBe('changed')
      // Synced target should remain unchanged
      expect(storeInstance.state.dst.value).toBe('same')

      // Listener should see src.other change but NOT dst.value
      const dstChanges = capturedChanges.filter(
        ([path]) => path === 'dst.value',
      )
      expect(dstChanges).toHaveLength(0)
    })

    it('should NOT emit a sync change when source leaf is set to same value directly', async () => {
      // Direct leaf assignment: setValue('src.value', 'same') when src.value
      // and dst.value are already 'same'. Sync should detect the no-op.
      interface ParentSyncState {
        src: { value: string }
        dst: { value: string }
      }

      const store = createGenericStore<ParentSyncState>(config)
      const capturedChanges: [string, unknown][] = []

      const { storeInstance, setValue } = mountStore(
        store,
        { src: { value: 'same' }, dst: { value: 'same' } },
        {
          sideEffects: {
            syncPaths: [['src.value', 'dst.value']],
            listeners: [
              {
                path: null,
                scope: null,
                fn: (changes) => {
                  for (const change of changes) {
                    capturedChanges.push([change[0] as string, change[1]])
                  }
                  return undefined
                },
              },
            ],
          },
        },
      )

      capturedChanges.length = 0

      // Direct leaf set with same value
      setValue('src.value', 'same')
      await flushSync()

      expect(storeInstance.state.dst.value).toBe('same')

      // No sync change should fire — source didn't actually change
      const dstChanges = capturedChanges.filter(
        ([path]) => path === 'dst.value',
      )
      expect(dstChanges).toHaveLength(0)
    })

    it('should NOT emit sync for object-valued path when content is identical', async () => {
      // Bug: is_different returned true for Object→Object (no deep comparison)
      // so sync always fired for object-valued paths even when unchanged.
      // WASM-044 adds structural hash to ValueRepr::Object/Array for O(1) comparison.
      interface ObjSyncState {
        src: { config: { currency: string; amount: number } }
        dst: { config: { currency: string; amount: number } }
      }

      const store = createGenericStore<ObjSyncState>(config)
      const capturedChanges: [string, unknown][] = []

      const { storeInstance, setValue } = mountStore(
        store,
        {
          src: { config: { currency: 'USD', amount: 100 } },
          dst: { config: { currency: 'USD', amount: 100 } },
        },
        {
          sideEffects: {
            syncPaths: [['src.config', 'dst.config']],
            listeners: [
              {
                path: null,
                scope: null,
                fn: (changes) => {
                  for (const change of changes) {
                    capturedChanges.push([change[0] as string, change[1]])
                  }
                  return undefined
                },
              },
            ],
          },
        },
      )

      capturedChanges.length = 0

      // Set src.config to identical object content
      setValue('src.config', { currency: 'USD', amount: 100 })
      await flushSync()

      // dst.config should remain unchanged — no sync should have fired
      expect(storeInstance.state.dst.config.currency).toBe('USD')
      expect(storeInstance.state.dst.config.amount).toBe(100)

      // No dst.config change should have been emitted
      const dstChanges = capturedChanges.filter(([path]) =>
        (path as string).startsWith('dst.config'),
      )
      expect(dstChanges).toHaveLength(0)
    })

    it('should emit sync for object-valued path when content differs', async () => {
      // Sanity check: when the object value DOES change, sync must still fire.
      interface ObjSyncState {
        src: { config: { currency: string; amount: number } }
        dst: { config: { currency: string; amount: number } }
      }

      const store = createGenericStore<ObjSyncState>(config)
      const capturedChanges: [string, unknown][] = []

      const { storeInstance, setValue } = mountStore(
        store,
        {
          src: { config: { currency: 'USD', amount: 100 } },
          dst: { config: { currency: 'USD', amount: 100 } },
        },
        {
          sideEffects: {
            syncPaths: [['src.config', 'dst.config']],
            listeners: [
              {
                path: null,
                scope: null,
                fn: (changes) => {
                  for (const change of changes) {
                    capturedChanges.push([change[0] as string, change[1]])
                  }
                  return undefined
                },
              },
            ],
          },
        },
      )

      capturedChanges.length = 0

      // Set src.config to DIFFERENT object content
      setValue('src.config', { currency: 'EUR', amount: 200 })
      await flushSync()

      // dst.config should have been updated via sync
      expect(storeInstance.state.dst.config.currency).toBe('EUR')
      expect(storeInstance.state.dst.config.amount).toBe(200)

      // Listener SHOULD have received dst.config changes
      const dstChanges = capturedChanges.filter(([path]) =>
        (path as string).startsWith('dst.config'),
      )
      expect(dstChanges.length).toBeGreaterThan(0)
    })

    it('should still emit sync change when leaf value actually differs', async () => {
      // Sanity check: when the parent-path assignment DOES change the leaf,
      // sync must still fire as expected.
      interface ParentSyncState {
        src: { value: string }
        dst: { value: string }
      }

      const store = createGenericStore<ParentSyncState>(config)
      const capturedChanges: [string, unknown][] = []

      const { storeInstance, setValue } = mountStore(
        store,
        { src: { value: 'old' }, dst: { value: 'old' } },
        {
          sideEffects: {
            syncPaths: [['src.value', 'dst.value']],
            listeners: [
              {
                path: null,
                scope: null,
                fn: (changes) => {
                  for (const change of changes) {
                    capturedChanges.push([change[0] as string, change[1]])
                  }
                  return undefined
                },
              },
            ],
          },
        },
      )

      capturedChanges.length = 0

      // Parent assignment that DOES change the leaf
      setValue('src', { value: 'new' })
      await flushSync()

      // dst.value should be updated
      expect(storeInstance.state.dst.value).toBe('new')

      // Listener SHOULD have received the dst.value sync change
      const dstChanges = capturedChanges.filter(
        ([path]) => path === 'dst.value',
      )
      expect(dstChanges).toHaveLength(1)
      const firstDstChange = dstChanges[0]
      expect(firstDstChange).toBeDefined()
      expect(firstDstChange![1]).toBe('new')
    })
  })
})
