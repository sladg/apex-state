/**
 * E2E: Combined Feature Coverage (v2)
 *
 * End-to-end integration test combining multiple features:
 * - Validation + deep state mutations
 * - BoolLogic concerns (disabledWhen, visibleWhen, readonlyWhen) with AND/OR/NOT
 * - Dynamic text interpolation (dynamicTooltip, dynamicLabel, dynamicPlaceholder)
 * - Sync + Flip + Listeners together
 * - Batch updates across multiple paths
 * - Combined: validation + sync + concerns on same fields
 * - Concern re-evaluation when dependencies change
 * - Direct _concerns proxy access + state mutation tracking
 *
 * Uses v2 fixtures (BasicTestState, DeeplyNestedState, SyncFlipState) instead
 * of the legacy 15-level EcommerceCatalog fixture.
 *
 * ┌─────────────────────────────────────────────────────────────────────┐
 * │ REPLACES:                                                          │
 * ├─────────────────────────────────────────────────────────────────────┤
 * │ tests/integration/ecommerce-catalog.test.tsx        (ENTIRE FILE)  │
 * └─────────────────────────────────────────────────────────────────────┘
 */

import { describe, expect, it } from 'vitest'
import { z } from 'zod'

import { createGenericStore, defaultConcerns } from '../../src'
import type { ConcernType } from '../../src/concerns/types'
import { dot } from '../../src/utils/dot'
import type { BasicTestState, DeeplyNestedState, SyncFlipState } from '../mocks'
import {
  basicTestFixtures,
  deeplyNestedFixtures,
  syncFlipFixtures,
} from '../mocks'
import { flushEffects, MODES, mountStore } from '../utils/react'

// ---------------------------------------------------------------------------
// Custom Concerns (same domain logic as old test, adapted to v2 fixtures)
// ---------------------------------------------------------------------------

/**
 * Custom concern: threshold check
 * Returns { exceeded: boolean, current: number, limit: number }
 */
const thresholdCheck: ConcernType<
  'thresholdCheck',
  { currentPath: string; limit: number },
  { exceeded: boolean; current: number; limit: number }
> = {
  name: 'thresholdCheck',
  description: 'Checks if a numeric value exceeds a threshold',
  evaluate: (props) => {
    const current = Number(dot.get__unsafe(props.state, props.currentPath)) || 0
    return {
      exceeded: current > props.limit,
      current,
      limit: props.limit,
    }
  },
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

describe.each(MODES)('[$name] E2E: Combined Feature Coverage', ({ config }) => {
  // =========================================================================
  // 1. VALIDATION on nested + flat paths
  // =========================================================================

  describe('Validation on nested and flat paths', () => {
    it('validates email with Zod schema', async () => {
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            email: {
              validationState: {
                schema: z
                  .string()
                  .email('Invalid email format')
                  .min(1, 'Email required'),
              },
            },
          },
        },
      )

      await flushEffects()

      // Empty email — validation error
      const emailConcerns = storeInstance._concerns['email']
      const validation = emailConcerns?.['validationState'] as
        | { isError: boolean; errors: { message: string }[] }
        | undefined
      expect(validation?.isError).toBe(true)

      // Set valid email
      setValue('email', 'alice@example.com')
      await flushEffects()

      const updated = storeInstance._concerns['email']?.['validationState'] as
        | { isError: boolean }
        | undefined
      expect(updated?.isError).toBe(false)

      // Set invalid email
      setValue('email', 'not-an-email')
      await flushEffects()

      const invalid = storeInstance._concerns['email']?.['validationState'] as
        | { isError: boolean; errors: { message: string }[] }
        | undefined
      expect(invalid?.isError).toBe(true)
      expect(invalid?.errors[0]?.message).toBe('Invalid email format')
    })

    it('validates numeric field with range constraints', async () => {
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            age: {
              validationState: {
                schema: z
                  .number()
                  .positive('Age must be positive')
                  .max(150, 'Age too high'),
              },
            },
          },
        },
      )

      await flushEffects()

      // age=0 — not positive
      const initial = storeInstance._concerns['age']?.['validationState'] as
        | { isError: boolean }
        | undefined
      expect(initial?.isError).toBe(true)

      // Set valid age
      setValue('age', 25)
      await flushEffects()

      const valid = storeInstance._concerns['age']?.['validationState'] as
        | { isError: boolean }
        | undefined
      expect(valid?.isError).toBe(false)

      // Set too high
      setValue('age', 200)
      await flushEffects()

      const tooHigh = storeInstance._concerns['age']?.['validationState'] as
        | { isError: boolean; errors: { message: string }[] }
        | undefined
      expect(tooHigh?.isError).toBe(true)
      expect(tooHigh?.errors[0]?.message).toBe('Age too high')
    })

    it('validates deeply nested path (5 levels)', async () => {
      const store = createGenericStore<DeeplyNestedState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        deeplyNestedFixtures.initial,
        {
          concerns: {
            'level1.level2.level3.level4.level5.value': {
              validationState: {
                schema: z.string().min(1, 'Deep value required'),
              },
            },
          },
        },
      )

      await flushEffects()

      // Initial 'L5' is valid
      const initial = storeInstance._concerns[
        'level1.level2.level3.level4.level5.value'
      ]?.['validationState'] as { isError: boolean } | undefined
      expect(initial?.isError).toBe(false)

      // Set to empty — invalid
      setValue('level1.level2.level3.level4.level5.value', '')
      await flushEffects()

      const empty = storeInstance._concerns[
        'level1.level2.level3.level4.level5.value'
      ]?.['validationState'] as
        | { isError: boolean; errors: { message: string }[] }
        | undefined
      expect(empty?.isError).toBe(true)
      expect(empty?.errors[0]?.message).toBe('Deep value required')
    })
  })

  // =========================================================================
  // 2. CONDITIONAL UI (disabledWhen, visibleWhen, readonlyWhen) with BoolLogic
  // =========================================================================

  describe('Conditional UI with BoolLogic', () => {
    it('disables field when condition matches (IS_EQUAL)', async () => {
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            fieldA: {
              disabledWhen: {
                boolLogic: {
                  IS_EQUAL: ['fieldB', 'locked'],
                },
              },
            },
          },
        },
      )

      await flushEffects()

      // fieldB is '' → not disabled
      expect(storeInstance._concerns['fieldA']?.['disabledWhen']).toBe(false)

      // Set fieldB to 'locked' → disabled
      setValue('fieldB', 'locked')
      await flushEffects()

      expect(storeInstance._concerns['fieldA']?.['disabledWhen']).toBe(true)

      // Set back → enabled
      setValue('fieldB', 'unlocked')
      await flushEffects()

      expect(storeInstance._concerns['fieldA']?.['disabledWhen']).toBe(false)
    })

    it('uses visibleWhen with NOT BoolLogic', async () => {
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            fieldA: {
              visibleWhen: {
                boolLogic: {
                  NOT: { IS_EQUAL: ['source', 'hidden'] },
                },
              },
            },
          },
        },
      )

      await flushEffects()

      // source is '' → NOT IS_EQUAL('hidden') → visible
      expect(storeInstance._concerns['fieldA']?.['visibleWhen']).toBe(true)

      // Set source to 'hidden' → not visible
      setValue('source', 'hidden')
      await flushEffects()

      expect(storeInstance._concerns['fieldA']?.['visibleWhen']).toBe(false)

      // Set to something else → visible again
      setValue('source', 'shown')
      await flushEffects()

      expect(storeInstance._concerns['fieldA']?.['visibleWhen']).toBe(true)
    })

    it('uses readonlyWhen with IS_EQUAL on boolean', async () => {
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            fieldA: {
              readonlyWhen: {
                boolLogic: {
                  IS_EQUAL: ['boolA', true],
                },
              },
            },
          },
        },
      )

      await flushEffects()

      // boolA is false → not readonly
      expect(storeInstance._concerns['fieldA']?.['readonlyWhen']).toBe(false)

      // Set boolA true → readonly
      setValue('boolA', true)
      await flushEffects()

      expect(storeInstance._concerns['fieldA']?.['readonlyWhen']).toBe(true)
    })

    it('uses complex AND/OR/NOT BoolLogic for multi-condition disable', async () => {
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            fieldA: {
              disabledWhen: {
                boolLogic: {
                  // Disabled when: (source != '') AND (boolA = true OR boolB = false)
                  AND: [
                    { NOT: { IS_EQUAL: ['source', ''] } },
                    {
                      OR: [
                        { IS_EQUAL: ['boolA', true] },
                        { IS_EQUAL: ['boolB', false] },
                      ],
                    },
                  ],
                },
              },
            },
          },
        },
      )

      await flushEffects()

      // source='' → AND fails immediately → enabled
      expect(storeInstance._concerns['fieldA']?.['disabledWhen']).toBe(false)

      // source='x', boolA=false, boolB=true → OR both false → enabled
      setValue('source', 'x')
      await flushEffects()
      // boolB starts as true in empty fixture, boolA is false
      // OR: (false==true → false) OR (true==false → false) → false → AND fails
      expect(storeInstance._concerns['fieldA']?.['disabledWhen']).toBe(false)

      // Set boolA=true → OR succeeds, AND succeeds → disabled
      setValue('boolA', true)
      await flushEffects()

      expect(storeInstance._concerns['fieldA']?.['disabledWhen']).toBe(true)

      // Set source back to '' → AND fails → enabled
      setValue('source', '')
      await flushEffects()

      expect(storeInstance._concerns['fieldA']?.['disabledWhen']).toBe(false)
    })
  })

  // =========================================================================
  // 3. DYNAMIC TEXT (dynamicLabel, dynamicTooltip, dynamicPlaceholder)
  // =========================================================================

  describe('Dynamic text interpolation', () => {
    it('interpolates dynamicTooltip from state paths', async () => {
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.populated,
        {
          concerns: {
            fieldA: {
              dynamicTooltip: {
                template: 'Source: {{source}} | Target: {{target}}',
              },
            },
          },
        },
      )

      await flushEffects()

      const tooltip = storeInstance._concerns['fieldA']?.['dynamicTooltip']
      expect(tooltip).toBe('Source: source-value | Target: target-value')

      // Update source → tooltip re-evaluates
      setValue('source', 'new-source')
      await flushEffects()

      const updated = storeInstance._concerns['fieldA']?.['dynamicTooltip']
      expect(updated).toBe('Source: new-source | Target: target-value')
    })

    it('interpolates dynamicLabel with multiple references', async () => {
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance } = mountStore(store, basicTestFixtures.populated, {
        concerns: {
          fieldC: {
            dynamicLabel: {
              template: 'Count ({{fieldA}} / {{fieldB}})',
            },
          },
        },
      })

      await flushEffects()

      const label = storeInstance._concerns['fieldC']?.['dynamicLabel']
      expect(label).toBe('Count (value-a / value-b)')
    })

    it('interpolates dynamicPlaceholder from deeply nested state', async () => {
      const store = createGenericStore<DeeplyNestedState>(config)
      const { storeInstance } = mountStore(
        store,
        deeplyNestedFixtures.initial,
        {
          concerns: {
            'level1.value': {
              dynamicPlaceholder: {
                template: 'Deep: {{level1.level2.level3.level4.level5.value}}',
              },
            },
          },
        },
      )

      await flushEffects()

      const placeholder =
        storeInstance._concerns['level1.value']?.['dynamicPlaceholder']
      expect(placeholder).toBe('Deep: L5')
    })
  })

  // =========================================================================
  // 4. CUSTOM CONCERNS
  // =========================================================================

  describe('Custom concerns', () => {
    it('thresholdCheck evaluates numeric field against limit', async () => {
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          customRender: () => {
            store.useConcerns(
              'threshold',
              {
                age: {
                  thresholdCheck: {
                    currentPath: 'age',
                    limit: 100,
                  },
                },
              },
              [...defaultConcerns, thresholdCheck],
            )
            return <span>test</span>
          },
        },
      )

      await flushEffects()

      // age=0 → not exceeded
      const check = storeInstance._concerns['age']?.['thresholdCheck'] as
        | { exceeded: boolean; current: number; limit: number }
        | undefined
      expect(check?.exceeded).toBe(false)

      // Set age=150 → exceeded
      setValue('age', 150)
      await flushEffects()

      const exceeded = storeInstance._concerns['age']?.['thresholdCheck'] as
        | { exceeded: boolean; current: number; limit: number }
        | undefined
      expect(exceeded?.exceeded).toBe(true)
      expect(exceeded?.current).toBe(150)
      expect(exceeded?.limit).toBe(100)
    })

    it('custom concern combined with built-in concerns on same path', async () => {
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          customRender: () => {
            store.useConcerns(
              'combined',
              {
                age: {
                  validationState: {
                    schema: z.number().positive('Age must be positive'),
                  },
                  disabledWhen: {
                    boolLogic: {
                      IS_EQUAL: ['boolA', true],
                    },
                  },
                  thresholdCheck: {
                    currentPath: 'age',
                    limit: 100,
                  },
                },
              },
              [...defaultConcerns, thresholdCheck],
            )
            return <span>test</span>
          },
        },
      )

      await flushEffects()

      // All concerns should be evaluated
      const validation = storeInstance._concerns['age']?.['validationState'] as
        | { isError: boolean }
        | undefined
      expect(validation?.isError).toBe(true) // age=0 not positive
      expect(storeInstance._concerns['age']?.['disabledWhen']).toBe(false) // boolA=false
      const threshold = storeInstance._concerns['age']?.['thresholdCheck'] as
        | { exceeded: boolean }
        | undefined
      expect(threshold?.exceeded).toBe(false) // age=0 < 100

      // Set valid age that exceeds threshold
      setValue('age', 150)
      await flushEffects()

      const updatedVal = storeInstance._concerns['age']?.['validationState'] as
        | { isError: boolean }
        | undefined
      expect(updatedVal?.isError).toBe(false) // 150 is positive
      const updatedThreshold = storeInstance._concerns['age']?.[
        'thresholdCheck'
      ] as { exceeded: boolean } | undefined
      expect(updatedThreshold?.exceeded).toBe(true) // 150 > 100
    })
  })

  // =========================================================================
  // 5. SIDE EFFECTS (syncPaths, flipPaths, listeners)
  // =========================================================================

  describe('Side effects: sync, flip, listeners together', () => {
    it('syncs string paths (source → target)', async () => {
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

      await flushEffects()

      // Change source → target syncs
      setValue('source', 'synced-value')
      await flushEffects()

      expect(storeInstance.state.target).toBe('synced-value')
    })

    it('flips boolean paths (flag1 ↔ flag2)', async () => {
      const store = createGenericStore<SyncFlipState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        syncFlipFixtures.initial,
        {
          sideEffects: {
            flipPaths: [['flag1', 'flag2']],
          },
        },
      )

      await flushEffects()

      // Initial: flag1=false, flag2=true
      expect(storeInstance.state.flag1).toBe(false)
      expect(storeInstance.state.flag2).toBe(true)

      // Set flag1=true → flag2 should flip to false
      setValue('flag1', true)
      await flushEffects()

      expect(storeInstance.state.flag1).toBe(true)
      expect(storeInstance.state.flag2).toBe(false)
    })

    it('listener produces changes when watched field changes', async () => {
      const store = createGenericStore<BasicTestState>(config)
      let listenerCallCount = 0
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          sideEffects: {
            listeners: [
              {
                path: 'source',
                scope: null,
                fn: (changes) => {
                  listenerCallCount++
                  // Produce a change: copy source value to target
                  const newValue = changes[0]?.[1]
                  return [['target', newValue, {}]] as any
                },
              },
            ],
          },
        },
      )

      setValue('source', 'listener-output')
      await flushEffects()

      expect(listenerCallCount).toBe(1)
      expect(storeInstance.state.target).toBe('listener-output')
    })

    it('sync + flip + listener work together in same store', async () => {
      const store = createGenericStore<BasicTestState>(config)
      let listenerCallCount = 0
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          sideEffects: {
            syncPaths: [['source', 'target']],
            flipPaths: [['boolA', 'boolB']],
            listeners: [
              {
                path: 'fieldA',
                scope: null,
                fn: () => {
                  listenerCallCount++
                  return undefined
                },
              },
            ],
          },
        },
      )

      await flushEffects()

      // Test sync
      setValue('source', 'sync-test')
      await flushEffects()
      expect(storeInstance.state.target).toBe('sync-test')

      // Test flip
      setValue('boolA', true)
      await flushEffects()
      expect(storeInstance.state.boolB).toBe(false)

      // Test listener
      setValue('fieldA', 'triggers-listener')
      await flushEffects()
      expect(listenerCallCount).toBe(1)
    })
  })

  // =========================================================================
  // 6. BATCH UPDATES across multiple paths
  // =========================================================================

  describe('Batch updates', () => {
    it('applies batch changes across multiple paths', async () => {
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setChanges } = mountStore(
        store,
        basicTestFixtures.empty,
      )

      setChanges([
        ['fieldA', 'batch-a', {} as any],
        ['fieldB', 'batch-b', {} as any],
        ['fieldC', 42, {} as any],
        ['email', 'batch@test.com', {} as any],
        ['age', 30, {} as any],
      ])
      await flushEffects()

      expect(storeInstance.state.fieldA).toBe('batch-a')
      expect(storeInstance.state.fieldB).toBe('batch-b')
      expect(storeInstance.state.fieldC).toBe(42)
      expect(storeInstance.state.email).toBe('batch@test.com')
      expect(storeInstance.state.age).toBe(30)
    })

    it('batch updates trigger concerns re-evaluation', async () => {
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setChanges } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            email: {
              validationState: {
                schema: z.string().email('Invalid email'),
              },
            },
            fieldA: {
              disabledWhen: {
                boolLogic: {
                  IS_EQUAL: ['boolA', true],
                },
              },
            },
          },
        },
      )

      await flushEffects()

      // Batch: set valid email + set boolA=true
      setChanges([
        ['email', 'valid@example.com', {} as any],
        ['boolA', true, {} as any],
      ])
      await flushEffects()

      const emailValid = storeInstance._concerns['email']?.[
        'validationState'
      ] as { isError: boolean } | undefined
      expect(emailValid?.isError).toBe(false)
      expect(storeInstance._concerns['fieldA']?.['disabledWhen']).toBe(true)
    })
  })

  // =========================================================================
  // 7. COMBINED: validation + sync + concerns on same fields
  // =========================================================================

  describe('Combined concerns and side effects', () => {
    it('validation + sync + conditional UI work together', async () => {
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            source: {
              validationState: {
                schema: z
                  .string()
                  .min(1, 'Source required')
                  .max(50, 'Source too long'),
              },
              disabledWhen: {
                boolLogic: {
                  IS_EQUAL: ['boolA', true],
                },
              },
              dynamicTooltip: {
                template: 'Current target: {{target}}',
              },
            },
          },
          sideEffects: {
            syncPaths: [['source', 'target']],
          },
        },
      )

      await flushEffects()

      // Initial: source='' → validation fails, disabled=false, tooltip shows empty target
      const initialVal = storeInstance._concerns['source']?.[
        'validationState'
      ] as { isError: boolean } | undefined
      expect(initialVal?.isError).toBe(true)
      expect(storeInstance._concerns['source']?.['disabledWhen']).toBe(false)

      // Set source → validates, syncs to target, tooltip updates
      setValue('source', 'hello')
      await flushEffects()

      const updatedVal = storeInstance._concerns['source']?.[
        'validationState'
      ] as { isError: boolean } | undefined
      expect(updatedVal?.isError).toBe(false)
      expect(storeInstance.state.target).toBe('hello')
      expect(storeInstance._concerns['source']?.['dynamicTooltip']).toBe(
        'Current target: hello',
      )

      // Disable via boolA
      setValue('boolA', true)
      await flushEffects()

      expect(storeInstance._concerns['source']?.['disabledWhen']).toBe(true)
    })

    it('re-evaluates all concerns when dependencies change', async () => {
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.populated,
        {
          concerns: {
            fieldA: {
              dynamicTooltip: {
                template: 'Source is: {{source}}',
              },
              disabledWhen: {
                boolLogic: {
                  IS_EQUAL: ['source', 'disable-trigger'],
                },
              },
            },
          },
        },
      )

      await flushEffects()

      expect(storeInstance._concerns['fieldA']?.['dynamicTooltip']).toBe(
        'Source is: source-value',
      )
      expect(storeInstance._concerns['fieldA']?.['disabledWhen']).toBe(false)

      // Change source → both concerns re-evaluate
      setValue('source', 'disable-trigger')
      await flushEffects()

      expect(storeInstance._concerns['fieldA']?.['dynamicTooltip']).toBe(
        'Source is: disable-trigger',
      )
      expect(storeInstance._concerns['fieldA']?.['disabledWhen']).toBe(true)
    })
  })

  // =========================================================================
  // 8. DIRECT STORE INSTANCE ASSERTIONS
  // =========================================================================

  describe('Direct store instance assertions', () => {
    it('concern results accessible via _concerns proxy', async () => {
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.populated,
        {
          concerns: {
            email: {
              validationState: {
                schema: z.string().email('Invalid email'),
              },
              disabledWhen: {
                boolLogic: {
                  IS_EQUAL: ['boolA', true],
                },
              },
            },
          },
        },
      )

      await flushEffects()

      // Direct access to _concerns
      const emailConcerns = storeInstance._concerns['email']
      expect(emailConcerns).toBeDefined()
      expect((emailConcerns?.['validationState'] as any)?.isError).toBe(false) // test@example.com is valid
      expect(emailConcerns?.['disabledWhen']).toBe(true) // boolA=true in populated

      // Mutate via setValue → concerns update
      setValue('boolA', false)
      await flushEffects()

      const updatedConcerns = storeInstance._concerns['email']
      expect(updatedConcerns?.['disabledWhen']).toBe(false)
    })

    it('state mutations at nested levels are tracked', async () => {
      const store = createGenericStore<DeeplyNestedState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        deeplyNestedFixtures.initial,
      )

      expect(storeInstance.state.level1.level2.level3.level4.level5.value).toBe(
        'L5',
      )

      setValue('level1.level2.level3.level4.level5.value', 'UPDATED')
      await flushEffects()

      expect(storeInstance.state.level1.level2.level3.level4.level5.value).toBe(
        'UPDATED',
      )
    })

    it('state mutations at level 5 boolean are tracked', async () => {
      const store = createGenericStore<DeeplyNestedState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        deeplyNestedFixtures.initial,
      )

      expect(storeInstance.state.level1.level2.level3.level4.level5.flag).toBe(
        false,
      )

      setValue('level1.level2.level3.level4.level5.flag', true)
      await flushEffects()

      expect(storeInstance.state.level1.level2.level3.level4.level5.flag).toBe(
        true,
      )
    })
  })
})
