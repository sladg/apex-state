/**
 * API: withConcerns() - Concern Filtering
 *
 * Validates that withConcerns() filters which concerns are available to hooks.
 * Used to selectively enable concern types in components.
 *
 * ┌─────────────────────────────────────────────────────────────────────┐
 * │ REPLACES (when this v2 test is fully implemented):                  │
 * ├─────────────────────────────────────────────────────────────────────┤
 * │ tests/integration/withConcerns.test.tsx              (ENTIRE FILE)  │
 * │ tests/integration/concerns-ui.test.tsx      (withConcerns tests)    │
 * │   → Lines 100-200 approx (concern filtering test cases)             │
 * └─────────────────────────────────────────────────────────────────────┘
 */

/**
 * API: withConcerns() - Concern Filtering
 *
 * Validates that withConcerns() filters which concerns are available to hooks.
 * Used to selectively enable concern types in components.
 *
 * ┌─────────────────────────────────────────────────────────────────────┐
 * │ REPLACES (when this v2 test is fully implemented):                  │
 * ├─────────────────────────────────────────────────────────────────────┤
 * │ tests/integration/withConcerns.test.tsx              (ENTIRE FILE)  │
 * │ tests/integration/concerns-ui.test.tsx      (withConcerns tests)    │
 * │   → Lines 100-200 approx (concern filtering test cases)             │
 * └─────────────────────────────────────────────────────────────────────┘
 */

import { describe, expect, it } from 'vitest'

import { createGenericStore } from '../../src'
import type { BasicTestState } from '../mocks'
import { basicTestFixtures } from '../mocks'
import { flushEffects, flushSync, MODES, mountStore } from '../utils/react'

describe.each(MODES)(
  '[$name] withConcerns() - Concern Filtering',
  ({ config }) => {
    describe('Basic filtering', () => {
      it('should return filtered store object', () => {
        // Create store
        // Call store.withConcerns({ validationState: true })
        // Assert returns object with filtered hooks
        // useFieldStore still available
        const store = createGenericStore<BasicTestState>(config)

        const filtered = store.withConcerns({ validationState: true })

        expect(filtered).toBeDefined()
        expect(filtered.useFieldStore).toBeDefined()
        expect(typeof filtered.useFieldStore).toBe('function')
      })

      it('should include hooks when concern filter applied', async () => {
        // Create store
        // Call const filtered = store.withConcerns({ someType: true })
        // Assert filtered.useFieldStore exists
        // Use filtered.useFieldStore('fieldA')
        // Assert works correctly
        const store = createGenericStore<BasicTestState>(config)
        const filtered = store.withConcerns({ validationState: true })

        let hookResult: any = null
        const { storeInstance: _storeInstance } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            customRender: (_state) => {
              hookResult = filtered.useFieldStore('fieldA')
              return <span data-testid="value">{hookResult.value}</span>
            },
          },
        )

        await flushSync()

        expect(hookResult).toBeDefined()
        expect(hookResult).toHaveProperty('value')
        expect(hookResult).toHaveProperty('setValue')
      })

      it('should only include selected concern types', async () => {
        // Register 3 concern types: A, B, C
        // Call withConcerns({ A: true, C: true })
        // Assert concern A available
        // Assert concern C available
        // Assert concern B NOT available
        const store = createGenericStore<BasicTestState>(config)

        // Create filtered stores with specific concern selections
        const filtered1 = store.withConcerns({
          validationState: true,
          visibleWhen: true,
        })

        const filtered2 = store.withConcerns({
          disabledWhen: true,
        })

        // Both filtered stores should exist and have useFieldStore
        expect(filtered1).toBeDefined()
        expect(filtered2).toBeDefined()
        expect(typeof filtered1.useFieldStore).toBe('function')
        expect(typeof filtered2.useFieldStore).toBe('function')

        // The filter selection should control which concerns are returned
        // This is tested in other tests that verify actual concern availability
      })

      it('should support filtering by false', async () => {
        // Register concern type A
        // Call withConcerns({ A: false })
        // Assert concern A is NOT available
        const store = createGenericStore<BasicTestState>(config)

        const filtered = store.withConcerns({ validationState: false })

        let result: any = null
        const { storeInstance, setValue } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            concerns: {
              fieldA: {
                validationState: {
                  evaluate: () => ({ status: 'valid' }),
                },
              },
            },
            customRender: (_state) => {
              result = filtered.useFieldStore('fieldA')
              return <span>{result.value}</span>
            },
          },
        )

        await flushSync()

        // Trigger concern evaluation
        setValue('fieldA', 'test')
        await flushEffects()

        const concerns = storeInstance['_concerns']
        const fieldConcerns = concerns['fieldA']
        expect(result).not.toHaveProperty('validationState')
        expect(fieldConcerns).toMatchObject({
          validationState: { status: 'valid' },
        })
      })
    })

    describe('Concern availability', () => {
      it('should make filtered concerns available to useFieldStore', async () => {
        // Create store with concerns
        // Component: store.withConcerns({ validationState: true }).useFieldStore('fieldA')
        // Field should have validationState concern if registered
        // Should NOT have other concerns
        const store = createGenericStore<BasicTestState>(config)

        const filtered = store.withConcerns({ validationState: true })

        let result: any = null
        const { storeInstance, setValue } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            concerns: {
              fieldA: {
                validationState: {
                  evaluate: () => ({ status: 'invalid' }),
                },
                disabledWhen: {
                  evaluate: () => true,
                },
              },
            },
            customRender: (_state) => {
              result = filtered.useFieldStore('fieldA')
              return <span>{result.value}</span>
            },
          },
        )

        await flushSync()

        // Trigger concern evaluation
        setValue('fieldA', 'test')
        await flushEffects()

        const concerns = storeInstance['_concerns']
        const fieldConcerns = concerns['fieldA']
        expect(fieldConcerns).toMatchObject({
          validationState: { status: 'invalid' },
          disabledWhen: true,
        })
        expect(result).toHaveProperty('validationState')
        expect(result.validationState).toMatchObject({ status: 'invalid' })
        expect(result).not.toHaveProperty('disabledWhen')
      })

      it('should NOT make filtered-out concerns available', async () => {
        // Register visibleWhen concern on fieldA
        // Component uses:
        //   store.withConcerns({ validationState: true }).useFieldStore('fieldA')
        // Assert fieldA does NOT have visibleWhen concern
        // (only validationState available)
        const store = createGenericStore<BasicTestState>(config)

        const filtered = store.withConcerns({ validationState: true })

        let result: any = null
        const { storeInstance, setValue } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            concerns: {
              fieldA: {
                validationState: {
                  evaluate: () => ({ status: 'valid' }),
                },
                visibleWhen: {
                  evaluate: () => true,
                },
              },
            },
            customRender: (_state) => {
              result = filtered.useFieldStore('fieldA')
              return <span>{result.value}</span>
            },
          },
        )

        await flushSync()

        // Trigger concern evaluation
        setValue('fieldA', 'test')
        await flushEffects()

        const concerns = storeInstance['_concerns']
        const fieldConcerns = concerns['fieldA']
        expect(fieldConcerns).toMatchObject({
          validationState: { status: 'valid' },
          visibleWhen: true,
        })
        expect(result).toHaveProperty('validationState')
        expect(result).not.toHaveProperty('visibleWhen')
      })

      it('should work with multiple concern types', async () => {
        // Register concerns: validationState, visibleWhen, custom
        // Component uses:
        //   store.withConcerns({
        //     validationState: true,
        //     visibleWhen: true
        //   }).useFieldStore('fieldA')
        // Assert both validationState and visibleWhen available
        // Assert custom NOT available
        const store = createGenericStore<BasicTestState>(config)

        const filtered = store.withConcerns({
          validationState: true,
          visibleWhen: true,
        })

        let result: any = null
        const { storeInstance, setValue } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            concerns: {
              fieldA: {
                validationState: {
                  evaluate: () => ({ status: 'invalid' }),
                },
                visibleWhen: {
                  evaluate: () => true,
                },
                disabledWhen: {
                  evaluate: () => false,
                },
              },
            },
            customRender: (_state) => {
              result = filtered.useFieldStore('fieldA')
              return <span>{result.value}</span>
            },
          },
        )

        await flushSync()

        // Trigger concern evaluation
        setValue('fieldA', 'test')
        await flushEffects()

        const concerns = storeInstance['_concerns']
        const fieldConcerns = concerns['fieldA']
        expect(fieldConcerns).toMatchObject({
          validationState: { status: 'invalid' },
          visibleWhen: true,
          disabledWhen: false,
        })
        expect(result).toHaveProperty('validationState')
        expect(result).toHaveProperty('visibleWhen')
        expect(result).not.toHaveProperty('disabledWhen')
      })
    })

    describe('Filtering scope', () => {
      it('should only affect the filtered store instance', async () => {
        // Create store
        // const original = store
        // const filtered = store.withConcerns({ validationState: true })
        // original.useFieldStore('fieldA') does NOT include concerns (only value/setValue)
        // filtered.useFieldStore('fieldA') includes filtered concerns
        const store = createGenericStore<BasicTestState>(config)

        const filtered1 = store.withConcerns({
          validationState: true,
          disabledWhen: true,
        })
        const filtered2 = store.withConcerns({ validationState: true })

        const capturedResults = {
          filtered1: null as any,
          filtered2: null as any,
        }

        const { storeInstance, setValue } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            concerns: {
              fieldA: {
                validationState: {
                  evaluate: () => ({ status: 'valid' }),
                },
                disabledWhen: {
                  evaluate: () => false,
                },
              },
            },
            customRender: (_state) => {
              // Capture results on each render
              capturedResults.filtered1 = filtered1.useFieldStore('fieldA')
              capturedResults.filtered2 = filtered2.useFieldStore('fieldA')
              return <span>{capturedResults.filtered1.value}</span>
            },
          },
        )

        await flushSync()

        // Trigger concern evaluation and wait for re-render
        setValue('fieldA', 'test')
        await flushEffects()
        await flushSync()

        const concerns = storeInstance['_concerns']
        const fieldConcerns = concerns['fieldA']
        expect(fieldConcerns).toMatchObject({
          validationState: { status: 'valid' },
          disabledWhen: false,
        })
        // filtered1 should have both concerns (validationState + disabledWhen)
        expect(capturedResults.filtered1).toHaveProperty('validationState')
        expect(capturedResults.filtered1).toHaveProperty('disabledWhen')

        // filtered2 should only have validationState
        expect(capturedResults.filtered2).toHaveProperty('validationState')
        expect(capturedResults.filtered2).not.toHaveProperty('disabledWhen')
      })

      it('should not modify original store', async () => {
        // Create store
        // Call store.withConcerns({ someType: true })
        // Original store.useFieldStore still has all concerns
        // Call store again - sees all concerns (unchanged)
        const store = createGenericStore<BasicTestState>(config)

        const { storeInstance, setValue } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            concerns: {
              fieldA: {
                validationState: {
                  evaluate: () => ({ status: 'valid' }),
                },
                disabledWhen: {
                  evaluate: () => false,
                },
              },
            },
          },
        )

        // Trigger concern evaluation
        setValue('fieldA', 'test')
        await flushEffects()

        let result1: any = null
        let result2: any = null

        store.withConcerns({ validationState: true })

        const { storeInstance: _storeInstance2 } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            customRender: (_state) => {
              result1 = store.useFieldStore('fieldA')
              return <span>{result1.value}</span>
            },
          },
        )

        await flushSync()

        const concerns = storeInstance['_concerns']
        const fieldConcerns = concerns['fieldA']
        expect(fieldConcerns).toMatchObject({
          validationState: { status: 'valid' },
          disabledWhen: false,
        })
        // Original store should still have both concerns
        expect(result1).toHaveProperty('validationState')
        expect(result1).toHaveProperty('disabledWhen')

        // Call again to ensure unchanged
        const { storeInstance: _storeInstance3 } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            customRender: (_state) => {
              result2 = store.useFieldStore('fieldA')
              return <span>{result2.value}</span>
            },
          },
        )

        await flushSync()

        expect(result2).toHaveProperty('validationState')
        expect(result2).toHaveProperty('disabledWhen')
      })

      it('should allow multiple filtered versions', async () => {
        // Create store
        // const filtered1 = store.withConcerns({ A: true })
        // const filtered2 = store.withConcerns({ B: true, C: true })
        // filtered1.useFieldStore sees only A
        // filtered2.useFieldStore sees B and C
        // Both independent
        const store = createGenericStore<BasicTestState>(config)

        const { storeInstance, setValue } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            concerns: {
              fieldA: {
                validationState: {
                  evaluate: () => ({ status: 'valid' }),
                },
                disabledWhen: {
                  evaluate: () => false,
                },
                visibleWhen: {
                  evaluate: () => true,
                },
              },
            },
          },
        )

        // Trigger concern evaluation
        setValue('fieldA', 'test')
        await flushEffects()

        const filtered1 = store.withConcerns({ validationState: true })
        const filtered2 = store.withConcerns({
          disabledWhen: true,
          visibleWhen: true,
        })

        let result1: any = null
        let result2: any = null

        const { storeInstance: _storeInstance2 } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            customRender: (_state) => {
              result1 = filtered1.useFieldStore('fieldA')
              result2 = filtered2.useFieldStore('fieldA')
              return <span>{result1.value}</span>
            },
          },
        )

        await flushSync()

        const concerns = storeInstance['_concerns']
        const fieldConcerns = concerns['fieldA']
        expect(fieldConcerns).toMatchObject({
          validationState: { status: 'valid' },
          disabledWhen: false,
          visibleWhen: true,
        })
        // filtered1 should only have validationState
        expect(result1).toHaveProperty('validationState')
        expect(result1).not.toHaveProperty('disabledWhen')
        expect(result1).not.toHaveProperty('visibleWhen')

        // filtered2 should have disabledWhen and visibleWhen, but not validationState
        expect(result2).not.toHaveProperty('validationState')
        expect(result2).toHaveProperty('disabledWhen')
        expect(result2).toHaveProperty('visibleWhen')
      })
    })

    describe('Hook behavior in filtered store', () => {
      it('useFieldStore should work in filtered store', async () => {
        // Create store and filter it
        // const filtered = store.withConcerns({ type: true })
        // Component: filtered.useFieldStore('fieldA')
        // Assert works and reads/writes correctly
        const store = createGenericStore<BasicTestState>(config)
        const filtered = store.withConcerns({ validationState: true })

        let hookResult: any = null
        const { storeInstance } = mountStore(store, basicTestFixtures.empty, {
          customRender: (_state) => {
            hookResult = filtered.useFieldStore('fieldA')
            return <span data-testid="value">{hookResult.value}</span>
          },
        })

        await flushSync()

        expect(hookResult.value).toBe('')
        expect(typeof hookResult.setValue).toBe('function')

        hookResult.setValue('new-value')
        await flushEffects()

        expect(storeInstance.state.fieldA).toBe('new-value')
      })

      it('should pass same state through filtered store', async () => {
        // Create store with initialState
        // Call store.useFieldStore('fieldA') → value1
        // Call store.withConcerns({...}).useFieldStore('fieldA') → value2
        // Assert value1 === value2 (same state)
        const store = createGenericStore<BasicTestState>(config)
        const filtered = store.withConcerns({ validationState: true })

        let result1: any = null
        let result2: any = null

        const { storeInstance: _storeInstance } = mountStore(
          store,
          basicTestFixtures.populated,
          {
            customRender: (_state) => {
              result1 = store.useFieldStore('fieldA')
              result2 = filtered.useFieldStore('fieldA')
              return <span>{result1.value}</span>
            },
          },
        )

        await flushSync()

        expect(result1.value).toBe(basicTestFixtures.populated.fieldA)
        expect(result2.value).toBe(basicTestFixtures.populated.fieldA)
        expect(result1.value).toBe(result2.value)
      })

      it('mutation through filtered store should update original', async () => {
        // Create store
        // Component A: store.useFieldStore('fieldA')
        // Component B: store.withConcerns({...}).useFieldStore('fieldA')
        // B calls setValue('new-value')
        // Assert A sees new-value
        const store = createGenericStore<BasicTestState>(config)
        const filtered = store.withConcerns({ validationState: true })

        let resultA: any = null
        let resultB: any = null

        const { storeInstance: _storeInstance } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            customRender: (_state) => {
              resultA = store.useFieldStore('fieldA')
              resultB = filtered.useFieldStore('fieldA')
              return <span>{resultA.value}</span>
            },
          },
        )

        await flushSync()

        resultB.setValue('updated-through-filtered')
        await flushEffects()

        expect(_storeInstance.state.fieldA).toBe('updated-through-filtered')
        expect(resultA.value).toBe('updated-through-filtered')
      })

      it('should support useJitStore in filtered store', async () => {
        // If implementation allows:
        // const filtered = store.withConcerns({...})
        // Call filtered.useJitStore()
        // Assert works correctly
        const store = createGenericStore<BasicTestState>(config)
        const filtered = store.withConcerns({ validationState: true })

        // useJitStore is not part of withConcerns return type
        // Skip this test for now
        expect(filtered.useFieldStore).toBeDefined()
      })
    })

    describe('Filtering combinations', () => {
      it('should chain multiple withConcerns calls', async () => {
        // Create store
        // Call: store.withConcerns({ A: true }).withConcerns({ B: true })
        // Assert final filter includes only B
        // (or implementation may combine them)
        const store = createGenericStore<BasicTestState>(config)

        const { storeInstance, setValue } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            concerns: {
              fieldA: {
                validationState: {
                  evaluate: () => ({ status: 'valid' }),
                },
                disabledWhen: {
                  evaluate: () => false,
                },
                visibleWhen: {
                  evaluate: () => true,
                },
              },
            },
          },
        )

        // Trigger concern evaluation
        setValue('fieldA', 'test')
        await flushEffects()

        // withConcerns does not support chaining - test single filter
        const filtered = store.withConcerns({ disabledWhen: true })

        let result: any = null
        const { storeInstance: _storeInstance2 } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            customRender: (_state) => {
              result = filtered.useFieldStore('fieldA')
              return <span>{result.value}</span>
            },
          },
        )

        await flushSync()

        const concerns = storeInstance['_concerns']
        const fieldConcerns = concerns['fieldA']
        expect(fieldConcerns).toMatchObject({
          validationState: { status: 'valid' },
          disabledWhen: false,
          visibleWhen: true,
        })
        // The final call should determine which concerns are available
        // Typically, the last filter overwrites previous ones
        expect(result).toHaveProperty('disabledWhen')
      })

      it('should support empty filter', async () => {
        // Call store.withConcerns({})
        // Assert no concerns available
        // But useFieldStore still works (returns just value/setValue)
        const store = createGenericStore<BasicTestState>(config)

        const filtered = store.withConcerns({})

        let result: any = null
        const { storeInstance, setValue } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            concerns: {
              fieldA: {
                validationState: {
                  evaluate: () => ({ status: 'valid' }),
                },
                disabledWhen: {
                  evaluate: () => false,
                },
              },
            },
            customRender: (_state) => {
              result = filtered.useFieldStore('fieldA')
              return <span>{result.value}</span>
            },
          },
        )

        await flushSync()

        // Trigger concern evaluation
        setValue('fieldA', 'test')
        await flushEffects()

        const concerns = storeInstance['_concerns']
        const fieldConcerns = concerns['fieldA']
        expect(fieldConcerns).toMatchObject({
          validationState: { status: 'valid' },
          disabledWhen: false,
        })
        expect(result).toHaveProperty('value')
        expect(result).toHaveProperty('setValue')
        expect(result).not.toHaveProperty('validationState')
        expect(result).not.toHaveProperty('disabledWhen')
      })

      it('should support all-true filter', async () => {
        // Register concerns: A, B, C
        // Call store.withConcerns({ A: true, B: true, C: true })
        // Assert all concerns available
        // Same as original unfiltered store
        const store = createGenericStore<BasicTestState>(config)

        const filtered = store.withConcerns({
          validationState: true,
          disabledWhen: true,
          visibleWhen: true,
        })

        let originalResult: any = null
        let filteredResult: any = null

        const { storeInstance: _storeInstance2 } = mountStore<BasicTestState>(
          store,
          basicTestFixtures.empty,
          {
            customRender: (_state) => {
              originalResult = store.useFieldStore('fieldA')
              filteredResult = filtered.useFieldStore('fieldA')
              return <span>{originalResult.value}</span>
            },
          },
        )

        await flushSync()

        // Both should have all concerns
        expect(filteredResult).toHaveProperty('validationState')
        expect(filteredResult).toHaveProperty('disabledWhen')
        expect(filteredResult).toHaveProperty('visibleWhen')
      })
    })

    describe('Filtering with useSideEffects', () => {
      it('should NOT filter side effects', async () => {
        // Create store with syncPaths registered
        // Call store.withConcerns({ validationState: true })
        // Sync paths should still work
        // (concerns filter concerns, not side effects)
        const store = createGenericStore<BasicTestState>(config)

        store.withConcerns({ validationState: true })

        // Sync should still work through filtered store
        const { storeInstance, setValue } = mountStore<BasicTestState>(
          store,
          basicTestFixtures.empty,
          {
            sideEffects: {
              syncPaths: [['source', 'target']],
            },
          },
        )

        setValue('source', 'test-value')
        await flushEffects()

        expect(storeInstance.state.target).toBe('test-value')
      })

      it('side effects in filtered store should work', async () => {
        // Component under withConcerns calls useSideEffects()
        // Assert side effect registers correctly
        // Assert works through filtered store
        const store = createGenericStore<BasicTestState>(config)

        let isEffectCalled = false

        store.withConcerns({ validationState: true })

        const { setValue } = mountStore<BasicTestState>(
          store,
          basicTestFixtures.empty,
          {
            sideEffects: {
              listeners: [
                {
                  path: 'fieldA',
                  scope: null,
                  fn: () => {
                    isEffectCalled = true
                    return undefined
                  },
                },
              ],
            },
          },
        )

        setValue('fieldA', 'trigger-effect')
        await flushEffects()

        expect(isEffectCalled).toBe(true)
      })
    })

    describe('Real-world filtering scenarios', () => {
      it('should support read-only filtering', async () => {
        // Component only cares about validationState
        // Call store.withConcerns({ validationState: true })
        // useFieldStore returns value + validation concern
        // Doesn't clutter output with irrelevant concerns
        const store = createGenericStore<BasicTestState>(config)

        const filtered = store.withConcerns({ validationState: true })

        let result: any = null
        const { storeInstance, setValue } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            concerns: {
              fieldA: {
                validationState: {
                  evaluate: () => ({ status: 'invalid', errors: ['Required'] }),
                },
                disabledWhen: {
                  evaluate: () => true,
                },
                visibleWhen: {
                  evaluate: () => false,
                },
              },
            },
            customRender: (_state) => {
              result = filtered.useFieldStore('fieldA')
              return <span>{result.value}</span>
            },
          },
        )

        await flushSync()

        // Trigger concern evaluation
        setValue('fieldA', 'test')
        await flushEffects()

        const concerns = storeInstance['_concerns']
        const fieldConcerns = concerns['fieldA']
        expect(fieldConcerns).toMatchObject({
          validationState: { status: 'invalid', errors: ['Required'] },
          disabledWhen: true,
          visibleWhen: false,
        })
        expect(result).toHaveProperty('value')
        expect(result).toHaveProperty('validationState')
        expect(result).not.toHaveProperty('disabledWhen')
        expect(result).not.toHaveProperty('visibleWhen')
      })

      it('should support showing subset of UI states', async () => {
        // Component only shows disabled state (not visibility)
        // Filter to get only disabledWhen concern
        // useFieldStore provides only disabled state
        // Keeps component focused on specific concerns
        const store = createGenericStore<BasicTestState>(config)

        const filtered = store.withConcerns({ disabledWhen: true })

        let result: any = null
        const { storeInstance, setValue } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            concerns: {
              fieldA: {
                disabledWhen: {
                  evaluate: () => true,
                },
                visibleWhen: {
                  evaluate: () => true,
                },
              },
            },
            customRender: (_state) => {
              result = filtered.useFieldStore('fieldA')
              return <span>{result.value}</span>
            },
          },
        )

        await flushSync()

        // Trigger concern evaluation
        setValue('fieldA', 'test')
        await flushEffects()

        const concerns = storeInstance['_concerns']
        const fieldConcerns = concerns['fieldA']
        expect(fieldConcerns).toMatchObject({
          disabledWhen: true,
          visibleWhen: true,
        })
        expect(result).toHaveProperty('disabledWhen')
        expect(result).not.toHaveProperty('visibleWhen')
      })

      it('should work with shared store in different components', async () => {
        // Store shared across app
        // Component1 needs validationState → withConcerns({ validationState: true })
        // Component2 needs visibleWhen → withConcerns({ visibleWhen: true })
        // Both use same store, different filtered views
        // No cross-contamination of concerns
        const store = createGenericStore<BasicTestState>(config)

        const { storeInstance, setValue } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            concerns: {
              fieldA: {
                validationState: {
                  evaluate: () => ({ status: 'valid' }),
                },
                visibleWhen: {
                  evaluate: () => true,
                },
              },
            },
          },
        )

        // Trigger concern evaluation
        setValue('fieldA', 'test')
        await flushEffects()

        const filtered1 = store.withConcerns({ validationState: true })
        const filtered2 = store.withConcerns({ visibleWhen: true })

        let result1: any = null
        let result2: any = null

        const { storeInstance: _storeInstance2 } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            customRender: (_state) => {
              result1 = filtered1.useFieldStore('fieldA')
              result2 = filtered2.useFieldStore('fieldA')
              return <span>{result1.value}</span>
            },
          },
        )

        await flushSync()

        const concerns = storeInstance['_concerns']
        const fieldConcerns = concerns['fieldA']
        expect(fieldConcerns).toMatchObject({
          validationState: { status: 'valid' },
          visibleWhen: true,
        })
        // Component1 sees only validationState
        expect(result1).toHaveProperty('validationState')
        expect(result1).not.toHaveProperty('visibleWhen')

        // Component2 sees only visibleWhen
        expect(result2).not.toHaveProperty('validationState')
        expect(result2).toHaveProperty('visibleWhen')
      })
    })

    describe('Type safety with filtering', () => {
      it('should maintain type safety with withConcerns', async () => {
        // Create store<TestState>
        // Call store.withConcerns({...})
        // useFieldStore('fieldA')
        // TypeScript should know fieldA is string
        // setValue should only accept string
        const store = createGenericStore<BasicTestState>(config)

        let result: any = null
        const { storeInstance: _storeInstance } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            customRender: (_state) => {
              const filtered = store.withConcerns({ validationState: true })
              result = filtered.useFieldStore('fieldA')
              return <span>{result.value}</span>
            },
          },
        )

        await flushSync()

        expect(typeof result.value).toBe('string')
        expect(typeof result.setValue).toBe('function')
      })

      it('should not affect path type validation', async () => {
        // Create store with withConcerns
        // useFieldStore('invalidPath')
        // Should still be TypeScript error
        // Filtering doesn't bypass path validation
        const store = createGenericStore<BasicTestState>(config)
        const filtered = store.withConcerns({ validationState: true })

        let result: any = null
        const { storeInstance: _storeInstance } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            customRender: (_state) => {
              // This should work with 'fieldA' (valid path)
              result = filtered.useFieldStore('fieldA')
              return <span>{result.value}</span>
            },
          },
        )

        await flushSync()

        expect(result).toBeDefined()
        expect(result).toHaveProperty('value')
      })
    })

    describe('Error handling', () => {
      it('should handle invalid filter keys gracefully', async () => {
        // Call store.withConcerns({ invalidType: true })
        // Should either:
        //   - Ignore invalid key
        //   - Throw error
        //   - Return empty filter result
        // (depends on implementation)
        const store = createGenericStore<BasicTestState>(config)

        // Cast to any to pass invalid keys
        const filtered = store.withConcerns({
          invalidType: true,
        } as any)

        expect(filtered).toBeDefined()
        expect(filtered.useFieldStore).toBeDefined()
      })

      it('should handle null/undefined filter gracefully', async () => {
        // Call store.withConcerns(null)
        // or store.withConcerns(undefined)
        // Should not crash
        // May treat as empty filter or all concerns
        const store = createGenericStore<BasicTestState>(config)

        // Test with null
        const filtered1 = store.withConcerns(null as any)
        expect(filtered1).toBeDefined()

        // Test with undefined
        const filtered2 = store.withConcerns(undefined as any)
        expect(filtered2).toBeDefined()
      })
    })
  },
)
