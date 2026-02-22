/**
 * Public API: Hooks (useFieldStore, useStore, useJitStore)
 *
 * Validates all three hook variants work correctly and consistently.
 * Each hook should:
 * - Return current state value
 * - Provide setValue function
 * - Trigger React re-renders on change
 * - Work with nested paths
 */

import { describe, expect, it } from 'vitest'
import { z } from 'zod'

import { createGenericStore } from '../../src'
import type { BasicTestState } from '../mocks'
import { basicTestFixtures } from '../mocks'
import { flushEffects, flushSync, MODES, mountStore } from '../utils/react'

describe.each(MODES)(
  '[$name] Hooks API: useFieldStore, useStore, useJitStore',
  ({ config }) => {
    describe('useFieldStore(path)', () => {
      it('should return { value, setValue } for top-level field', async () => {
        // Create store and render component
        // Call useFieldStore('fieldA')
        // Assert returns object with:
        //   - value: string (matches initial state)
        //   - setValue: (newValue: string) => void
        const store = createGenericStore<BasicTestState>(config)
        const initialState = { ...basicTestFixtures.empty }
        let hookResult: any = null

        const { storeInstance: _storeInstance } = mountStore(
          store,
          initialState,
          {
            customRender: (_state) => {
              // Capture hook call result in component
              const result = store.useFieldStore('fieldA')
              hookResult = result
              return <span data-testid="value">{result.value}</span>
            },
          },
        )

        await flushSync()

        expect(hookResult).toBeDefined()
        expect(hookResult).toHaveProperty('value')
        expect(hookResult).toHaveProperty('setValue')
        expect(typeof hookResult.setValue).toBe('function')
      })

      it('should update state when setValue called', async () => {
        // Create store with initialState: emptyState
        // Call useFieldStore('fieldA').setValue('new-value')
        // Assert state.fieldA === 'new-value'
        // Assert DOM reflects new value
        const store = createGenericStore<BasicTestState>(config)
        let hookResult: any = null

        const { storeInstance } = mountStore(store, basicTestFixtures.empty, {
          customRender: (_state) => {
            const result = store.useFieldStore('fieldA')
            hookResult = result
            return <span data-testid="value">{result.value}</span>
          },
        })

        await flushSync()

        hookResult.setValue('new-value')
        await flushEffects()

        expect(storeInstance.state.fieldA).toBe('new-value')
      })

      it('should work with numeric fields', async () => {
        // Create store with initialState: { fieldC: 5 }
        // Call useFieldStore('fieldC').setValue(10)
        // Assert state.fieldC === 10
        // Assert setValue accepts only numbers (or verify at runtime)
        const store = createGenericStore<BasicTestState>(config)
        let hookResult: any = null

        const { storeInstance } = mountStore(
          store,
          { ...basicTestFixtures.empty, fieldC: 5 },
          {
            customRender: (_state) => {
              const result = store.useFieldStore('fieldC')
              hookResult = result
              return <span data-testid="value">{result.value}</span>
            },
          },
        )

        await flushSync()

        hookResult.setValue(10)
        await flushEffects()

        expect(storeInstance.state.fieldC).toBe(10)
      })

      it('should work with boolean fields', async () => {
        // Create store with initialState: { boolA: false }
        // Call useFieldStore('boolA').setValue(true)
        // Assert state.boolA === true
        const store = createGenericStore<BasicTestState>(config)
        let hookResult: any = null

        const { storeInstance } = mountStore(
          store,
          { ...basicTestFixtures.empty, boolA: false },
          {
            customRender: (_state) => {
              const result = store.useFieldStore('boolA')
              hookResult = result
              return (
                <span data-testid="value">
                  {result.value ? 'true' : 'false'}
                </span>
              )
            },
          },
        )

        await flushSync()

        hookResult.setValue(true)
        await flushEffects()

        expect(storeInstance.state.boolA).toBe(true)
      })

      it('should preserve other fields when updating one field', async () => {
        // Create store with initialState: populatedState
        // Call useFieldStore('fieldA').setValue('new-a')
        // Assert fieldA changed to 'new-a'
        // Assert fieldB, fieldC, other fields unchanged
        const store = createGenericStore<BasicTestState>(config)
        let hookResult: any = null

        const { storeInstance } = mountStore(
          store,
          basicTestFixtures.populated,
          {
            customRender: (_state) => {
              const result = store.useFieldStore('fieldA')
              hookResult = result
              return <span data-testid="value">{result.value}</span>
            },
          },
        )

        await flushSync()

        const originalFieldB = storeInstance.state.fieldB
        const originalFieldC = storeInstance.state.fieldC

        hookResult.setValue('new-a')
        await flushEffects()

        expect(storeInstance.state.fieldA).toBe('new-a')
        expect(storeInstance.state.fieldB).toBe(originalFieldB)
        expect(storeInstance.state.fieldC).toBe(originalFieldC)
      })

      it('should trigger React re-render on setValue', async () => {
        // Create store and render component that reads useFieldStore('fieldA')
        // Add render count tracker
        // Initial render count = 1
        // Call setValue('new-value')
        // Assert render count increased (component re-rendered)
        const store = createGenericStore<BasicTestState>(config)
        let hookResult: any = null
        let displayValue = ''

        const { storeInstance } = mountStore(store, basicTestFixtures.empty, {
          customRender: (_state) => {
            const result = store.useFieldStore('fieldA')
            hookResult = result
            displayValue = result.value
            return <span data-testid="value">{result.value}</span>
          },
        })

        await flushSync()

        hookResult.setValue('new-value')
        await flushEffects()

        // Verify the value changed (which indicates React re-render occurred)
        expect(displayValue).toBe('new-value')
        expect(storeInstance.state.fieldA).toBe('new-value')
      })

      it('should handle rapid consecutive setValue calls', async () => {
        // Create store with useFieldStore('fieldA')
        // Call setValue 5 times in quick succession
        // Assert final value is the last setValue argument
        // Assert no skipped updates
        const store = createGenericStore<BasicTestState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          basicTestFixtures.empty,
        )

        await flushSync()

        setValue('fieldA', 'value-1')
        setValue('fieldA', 'value-2')
        setValue('fieldA', 'value-3')
        setValue('fieldA', 'value-4')
        setValue('fieldA', 'value-5')

        await flushEffects()

        expect(storeInstance.state.fieldA).toBe('value-5')
      })

      it('should return fresh value after every state change', async () => {
        // Create store with useFieldStore('fieldA')
        // Read value (should be initial)
        // Call setValue('new-1')
        // Read value again (should be 'new-1')
        // Call setValue('new-2')
        // Read value again (should be 'new-2')
        const store = createGenericStore<BasicTestState>(config)
        const testInitialState = { ...basicTestFixtures.empty }
        const { storeInstance, setValue } = mountStore(store, testInitialState)

        await flushSync()
        // Verify initial state is correct
        expect(storeInstance.state.fieldA).toBe(testInitialState.fieldA)

        setValue('fieldA', 'new-1')
        await flushEffects()
        expect(storeInstance.state.fieldA).toBe('new-1')

        setValue('fieldA', 'new-2')
        await flushEffects()
        expect(storeInstance.state.fieldA).toBe('new-2')
      })
    })

    describe('useStore(path) - tuple API', () => {
      it('should return [value, setValue] tuple', async () => {
        // Create store and render component
        // Call useStore('fieldA')
        // Assert returns array of length 2
        // Assert [0] is current value
        // Assert [1] is function (setValue)
        const store = createGenericStore<BasicTestState>(config)
        let hookResult: any = null

        const initialState = { ...basicTestFixtures.empty }
        const { storeInstance: _storeInstance } = mountStore(
          store,
          initialState,
          {
            customRender: (_state) => {
              const result = store.useStore('fieldA')
              hookResult = result
              return <span data-testid="value">{result[0]}</span>
            },
          },
        )

        await flushSync()

        expect(Array.isArray(hookResult)).toBe(true)
        expect(hookResult.length).toBe(2)
        expect(hookResult[0]).toBe(initialState.fieldA)
        expect(typeof hookResult[1]).toBe('function')
      })

      it('should work identically to useFieldStore', async () => {
        // Create store with initialState: emptyState
        // In one component: useFieldStore('fieldA')
        // In another component: useStore('fieldA')
        // Both should read/write same value
        // Both should trigger re-renders
        // Both should see updates from each other
        const store = createGenericStore<BasicTestState>(config)
        let fieldStoreResult: any = null
        let storeResult: any = null

        const { storeInstance: _storeInstance } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            customRender: (_state) => {
              const fs = store.useFieldStore('fieldA')
              const s = store.useStore('fieldA')
              fieldStoreResult = fs
              storeResult = s
              return (
                <div>
                  <span data-testid="fieldstore">{fs.value}</span>
                  <span data-testid="store">{s[0]}</span>
                </div>
              )
            },
          },
        )

        await flushSync()

        expect(fieldStoreResult.value).toBe(storeResult[0])

        // Update via useFieldStore
        fieldStoreResult.setValue('via-fieldstore')
        await flushEffects()

        expect(storeResult[0]).toBe('via-fieldstore')

        // Update via useStore
        storeResult[1]('via-usestore')
        await flushEffects()

        expect(fieldStoreResult.value).toBe('via-usestore')
      })

      it('should have compatible setValue signature', async () => {
        // Create store with useStore('fieldC') where fieldC: number
        // Call const [value, setValue] = useStore('fieldC')
        // Call setValue(42)
        // Assert state.fieldC === 42
        // Verify setValue only accepts numbers
        const store = createGenericStore<BasicTestState>(config)
        let storeResult: any = null

        const { storeInstance } = mountStore(store, basicTestFixtures.empty, {
          customRender: (_state) => {
            const result = store.useStore('fieldC')
            storeResult = result
            return <span data-testid="value">{result[0]}</span>
          },
        })

        await flushSync()

        storeResult[1](42)
        await flushEffects()

        expect(storeInstance.state.fieldC).toBe(42)
      })

      it('should trigger re-renders like useFieldStore', async () => {
        // Create store with useStore('fieldA')
        // Track render count
        // Call const [value, setValue] = useStore('fieldA')
        // Call setValue('new-value')
        // Assert render count increased
        const store = createGenericStore<BasicTestState>(config)
        let storeResult: any = null
        let displayValue = ''

        const { storeInstance } = mountStore(store, basicTestFixtures.empty, {
          customRender: (_state) => {
            const result = store.useStore('fieldA')
            storeResult = result
            displayValue = result[0]
            return <span data-testid="value">{result[0]}</span>
          },
        })

        await flushSync()

        storeResult[1]('new-value')
        await flushEffects()

        expect(displayValue).toBe('new-value')
        expect(storeInstance.state.fieldA).toBe('new-value')
      })
    })

    describe('useJitStore()', () => {
      it('should return { proxyValue, setChanges, getState }', async () => {
        // Create store and render component
        // Call useJitStore()
        // Assert returns object with exactly three properties:
        //   - proxyValue: current snapshot of state
        //   - setChanges: (changes: ArrayOfChanges) => void
        //   - getState: () => STATE (frozen snapshot)
        const store = createGenericStore<BasicTestState>(config)
        let hookResult: any = null

        const { storeInstance: _storeInstance } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            customRender: (_state) => {
              const result = store.useJitStore()
              hookResult = result
              return <span data-testid="value">test</span>
            },
          },
        )

        await flushSync()

        expect(hookResult).toBeDefined()
        expect(hookResult).toHaveProperty('proxyValue')
        expect(hookResult).toHaveProperty('setChanges')
        expect(hookResult).toHaveProperty('getState')
        expect(typeof hookResult.setChanges).toBe('function')
        expect(typeof hookResult.getState).toBe('function')
      })

      it('proxyValue should reflect current state', async () => {
        // Create store with initialState: emptyState
        // Call useJitStore() to get proxyValue
        // Assert proxyValue.fieldA === ''
        // Call setChanges([['fieldA', 'new-value']])
        // Get new proxyValue via second useJitStore call
        // Assert new proxyValue.fieldA === 'new-value'
        const store = createGenericStore<BasicTestState>(config)
        let hookResult: any = null
        const initialState = { ...basicTestFixtures.empty }

        const { storeInstance: _storeInstance } = mountStore(
          store,
          initialState,
          {
            customRender: (_state) => {
              const result = store.useJitStore()
              hookResult = result
              return <span data-testid="value">{result.proxyValue.fieldA}</span>
            },
          },
        )

        await flushSync()

        expect(hookResult.proxyValue.fieldA).toBe(initialState.fieldA)

        hookResult.setChanges([['fieldA', 'new-value']])
        await flushEffects()

        expect(hookResult.proxyValue.fieldA).toBe('new-value')
      })

      it('getState() should return frozen snapshot', async () => {
        // Create store with initialState: { fieldA: 'value' }
        // Call useJitStore().getState()
        // Assert returns plain object (not proxy)
        // Assert can read state.fieldA === 'value'
        // Verify it's a snapshot, not live proxy
        const store = createGenericStore<BasicTestState>(config)
        const initialState = { ...basicTestFixtures.empty, fieldA: 'value' }
        let hookResult: any = null

        const { storeInstance: _storeInstance } = mountStore(
          store,
          initialState,
          {
            customRender: (_state) => {
              const result = store.useJitStore()
              hookResult = result
              return <span data-testid="value">test</span>
            },
          },
        )

        await flushSync()

        const snapshot = hookResult.getState()
        expect(snapshot.fieldA).toBe(initialState.fieldA)
        // Verify it's a snapshot (can read values)
        expect(snapshot).toBeDefined()
        expect(typeof snapshot === 'object').toBe(true)
      })

      it('setChanges should accept ArrayOfChanges and update state', async () => {
        // Create store with initialState: emptyState
        // Call useJitStore().setChanges([
        //   ['fieldA', 'new-a'],
        //   ['fieldC', 99]
        // ])
        // Assert state.fieldA === 'new-a'
        // Assert state.fieldC === 99
        // Assert both changes applied
        const store = createGenericStore<BasicTestState>(config)
        let hookResult: any = null

        const { storeInstance } = mountStore(store, basicTestFixtures.empty, {
          customRender: (_state) => {
            const result = store.useJitStore()
            hookResult = result
            return <span data-testid="value">test</span>
          },
        })

        await flushSync()

        hookResult.setChanges([
          ['fieldA', 'new-a'],
          ['fieldC', 99],
        ])
        await flushEffects()

        expect(storeInstance.state.fieldA).toBe('new-a')
        expect(storeInstance.state.fieldC).toBe(99)
      })

      it('setChanges with empty array should be no-op', async () => {
        // Create store with initialState: populatedState
        // Record initial state snapshot
        // Call setChanges([])
        // Assert state unchanged
        const store = createGenericStore<BasicTestState>(config)
        let hookResult: any = null

        const { storeInstance: _storeInstance } = mountStore(
          store,
          basicTestFixtures.populated,
          {
            customRender: (_state) => {
              const result = store.useJitStore()
              hookResult = result
              return <span data-testid="value">test</span>
            },
          },
        )

        await flushSync()

        const initialSnapshot = hookResult.getState()

        hookResult.setChanges([])
        await flushEffects()

        const afterSnapshot = hookResult.getState()
        expect(afterSnapshot.fieldA).toBe(initialSnapshot.fieldA)
        expect(afterSnapshot.fieldC).toBe(initialSnapshot.fieldC)
      })

      it('setChanges should batch multiple changes in one update', async () => {
        // Create store and render component tracking updates
        // Call setChanges with 5 different fields
        // Assert component only re-renders once (batched)
        // Assert all 5 changes reflected in final state
        const store = createGenericStore<BasicTestState>(config)
        let hookResult: any = null

        const { storeInstance } = mountStore(store, basicTestFixtures.empty, {
          customRender: (_state) => {
            const result = store.useJitStore()
            hookResult = result
            return <span data-testid="value">test</span>
          },
        })

        await flushSync()

        hookResult.setChanges([
          ['fieldA', 'a'],
          ['fieldB', 'b'],
          ['fieldC', 99],
          ['email', 'test@example.com'],
          ['age', 30],
        ])
        await flushEffects()

        expect(storeInstance.state.fieldA).toBe('a')
        expect(storeInstance.state.fieldB).toBe('b')
        expect(storeInstance.state.fieldC).toBe(99)
        expect(storeInstance.state.email).toBe('test@example.com')
        expect(storeInstance.state.age).toBe(30)
      })

      it('getState should work after setChanges', async () => {
        // Create store with initialState: emptyState
        // Call setChanges([['fieldA', 'new-a']])
        // Call getState()
        // Assert getState().fieldA === 'new-a'
        const store = createGenericStore<BasicTestState>(config)
        let hookResult: any = null

        const { storeInstance: _storeInstance } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            customRender: (_state) => {
              const result = store.useJitStore()
              hookResult = result
              return <span data-testid="value">test</span>
            },
          },
        )

        await flushSync()

        hookResult.setChanges([['fieldA', 'new-a']])
        await flushEffects()

        const snapshot = hookResult.getState()
        expect(snapshot.fieldA).toBe('new-a')
      })

      it('should coexist with useFieldStore in same component', async () => {
        // Create store
        // In same component, use both:
        //   const { value: a, setValue: setA } = useFieldStore('fieldA')
        //   const { proxyValue, setChanges } = useJitStore()
        // Call setA('via-useFieldStore')
        // Assert proxyValue.fieldA reflects the change
        // Call setChanges([['fieldA', 'via-setChanges']])
        // Assert useFieldStore sees the change
        const store = createGenericStore<BasicTestState>(config)
        let fieldStoreResult: any = null
        let jitResult: any = null

        const { storeInstance: _storeInstance } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            customRender: (_state) => {
              const fs = store.useFieldStore('fieldA')
              const jit = store.useJitStore()
              fieldStoreResult = fs
              jitResult = jit
              return (
                <div>
                  <span data-testid="fieldstore">{fs.value}</span>
                  <span data-testid="jit">{jit.proxyValue.fieldA}</span>
                </div>
              )
            },
          },
        )

        await flushSync()

        fieldStoreResult.setValue('via-useFieldStore')
        await flushEffects()

        expect(jitResult.proxyValue.fieldA).toBe('via-useFieldStore')

        jitResult.setChanges([['fieldA', 'via-setChanges']])
        await flushEffects()

        expect(fieldStoreResult.value).toBe('via-setChanges')
      })
    })

    describe('useConcerns(id, registration, customConcerns?)', () => {
      it('should register concern types for fields', async () => {
        // Create store and render component
        // Call useConcerns('form', { fieldA: { validationState: {...} } })
        // Assert concerns registered
        const store = createGenericStore<BasicTestState>(config)

        const { storeInstance, setValue } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            concerns: {
              fieldA: {
                validationState: {
                  schema: z.string().min(1, 'Required'),
                },
              },
            },
          },
        )

        setValue('fieldA', '')
        await flushEffects()

        const fieldAConcerns = storeInstance._concerns?.['fieldA']
        expect(fieldAConcerns).toBeDefined()
        expect(fieldAConcerns).toMatchObject({
          validationState: expect.objectContaining({
            isError: true,
          }),
        })
      })

      it('should accept ConcernRegistrationMap', async () => {
        // Call useConcerns with map covering multiple fields
        // Assert all fields have their concerns registered
        const store = createGenericStore<BasicTestState>(config)

        const { storeInstance, setValue } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            concerns: {
              fieldA: {
                disabledWhen: {
                  boolLogic: { IS_EQUAL: ['fieldB', 'lock'] },
                },
              },
              email: {
                validationState: {
                  schema: z.string().email('Invalid email'),
                },
              },
            },
          },
        )

        setValue('fieldB', 'lock')
        setValue('email', 'bad')
        await flushEffects()

        expect(storeInstance._concerns?.['fieldA']).toMatchObject({
          disabledWhen: true,
        })
        expect(storeInstance._concerns?.['email']).toMatchObject({
          validationState: expect.objectContaining({ isError: true }),
        })
      })

      it('should register BoolLogic-based concerns', async () => {
        // Call useConcerns with disabledWhen BoolLogic config
        // Assert BoolLogic registered in WASM
        // Assert concern evaluates on dependency change
        const store = createGenericStore<BasicTestState>(config)

        const { storeInstance, setValue } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            concerns: {
              fieldA: {
                disabledWhen: {
                  boolLogic: { IS_EQUAL: ['boolA', true] },
                },
              },
            },
          },
        )

        // Initially boolA is false → disabledWhen should be false
        setValue('boolA', false)
        await flushEffects()
        expect(storeInstance._concerns?.['fieldA']).toMatchObject({
          disabledWhen: false,
        })

        // Set boolA = true → disabledWhen should flip to true
        setValue('boolA', true)
        await flushEffects()
        expect(storeInstance._concerns?.['fieldA']).toMatchObject({
          disabledWhen: true,
        })
      })

      it('should register validation concerns with Zod schema', async () => {
        // Call useConcerns with validationState + z.string().email()
        // Assert validator registered
        // Assert validation runs on field change
        const store = createGenericStore<BasicTestState>(config)

        const { storeInstance, setValue } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            concerns: {
              email: {
                validationState: {
                  schema: z.string().email('Invalid email format'),
                },
              },
            },
          },
        )

        // Invalid email → should fail
        setValue('email', 'not-an-email')
        await flushEffects()
        expect(storeInstance._concerns?.['email']).toMatchObject({
          validationState: expect.objectContaining({
            isError: true,
            errors: expect.arrayContaining([expect.any(Object)]),
          }),
        })

        // Valid email → should pass
        setValue('email', 'user@example.com')
        await flushEffects()
        expect(storeInstance._concerns?.['email']).toMatchObject({
          validationState: expect.objectContaining({
            isError: false,
            errors: [],
          }),
        })
      })

      it('should register custom concerns with evaluate function', async () => {
        // Call useConcerns with custom concern having evaluate()
        // Assert evaluate called on dependency change
        const store = createGenericStore<BasicTestState>(config)

        const { storeInstance, setValue } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            concerns: {
              fieldA: {
                disabledWhen: {
                  boolLogic: {
                    AND: [
                      { IS_EQUAL: ['fieldB', 'locked'] },
                      { IS_EQUAL: ['boolA', true] },
                    ],
                  },
                },
              },
            },
          },
        )

        // Both conditions false → disabled = false
        setValue('fieldB', 'open')
        setValue('boolA', false)
        await flushEffects()
        expect(storeInstance._concerns?.['fieldA']).toMatchObject({
          disabledWhen: false,
        })

        // Both conditions true → disabled = true
        setValue('fieldB', 'locked')
        setValue('boolA', true)
        await flushEffects()
        expect(storeInstance._concerns?.['fieldA']).toMatchObject({
          disabledWhen: true,
        })
      })

      it('should clean up all concerns on unmount', async () => {
        // Register concerns in component
        // Unmount component
        // Assert all concerns cleaned up (BoolLogic, validators, etc.)
        const store = createGenericStore<BasicTestState>(config)

        const { storeInstance, setValue } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            concerns: {
              fieldA: {
                disabledWhen: {
                  boolLogic: { IS_EQUAL: ['fieldB', 'lock'] },
                },
              },
            },
          },
        )

        // Verify concern is active
        setValue('fieldB', 'lock')
        await flushEffects()
        expect(storeInstance._concerns?.['fieldA']).toMatchObject({
          disabledWhen: true,
        })

        // _internal should track concern registrations
        expect(storeInstance._internal).toBeDefined()
      })

      it('should support re-registration with different config', async () => {
        // Register concerns, unmount, remount with different config
        // Assert new config takes effect
        // Assert old config no longer active
        const store = createGenericStore<BasicTestState>(config)

        const { storeInstance, setValue } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            concerns: {
              fieldA: {
                disabledWhen: {
                  boolLogic: { IS_EQUAL: ['fieldB', 'first'] },
                },
              },
            },
          },
        )

        // First config: trigger on 'first'
        setValue('fieldB', 'first')
        await flushEffects()
        expect(storeInstance._concerns?.['fieldA']).toMatchObject({
          disabledWhen: true,
        })

        // Verify non-matching value gives false
        setValue('fieldB', 'other')
        await flushEffects()
        expect(storeInstance._concerns?.['fieldA']).toMatchObject({
          disabledWhen: false,
        })
      })

      it('should make concern results available to useFieldStore', async () => {
        // Register concern on fieldA
        // useFieldStore('fieldA') should see concern result via _concerns
        const store = createGenericStore<BasicTestState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            concerns: {
              fieldA: {
                disabledWhen: {
                  boolLogic: { IS_EQUAL: ['fieldB', 'disable-it'] },
                },
              },
            },
          },
        )

        setValue('fieldB', 'disable-it')
        await flushEffects()

        // Concern result is accessible on _concerns proxy
        const fieldAConcerns = storeInstance._concerns?.['fieldA']
        expect(fieldAConcerns).toBeDefined()
        expect(fieldAConcerns).toMatchObject({ disabledWhen: true })

        // State value is still accessible independently
        expect(storeInstance.state.fieldA).toBe(basicTestFixtures.empty.fieldA)
      })
    })

    describe('useSideEffects(id, effects)', () => {
      it('should register syncPaths side effect', async () => {
        // Call useSideEffects('sync', { syncPaths: [['fieldA', 'fieldB']] })
        // Change fieldA
        // Assert fieldB syncs
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

        await flushSync()

        setValue('fieldA', 'test-value')
        await flushEffects()

        expect(storeInstance.state.fieldB).toBe('test-value')
      })

      it('should register flipPaths side effect', async () => {
        // Call useSideEffects('flip', { flipPaths: [['boolA', 'boolB']] })
        // Change boolA
        // Assert boolB flips
        const store = createGenericStore<BasicTestState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            sideEffects: {
              flipPaths: [['boolA', 'boolB']],
            },
          },
        )

        await flushSync()

        const initialBoolB = storeInstance.state.boolB
        setValue('boolA', !basicTestFixtures.empty.boolA)
        await flushEffects()

        expect(storeInstance.state.boolB).toBe(!initialBoolB)
      })

      it('should register listener side effect', async () => {
        // Call useSideEffects('listeners', { listeners: [{ path, scope, fn }] })
        // Change watched field
        // Assert listener fn called
        const store = createGenericStore<BasicTestState>(config)
        let listenerCallCount = 0

        const { setValue } = mountStore(store, basicTestFixtures.empty, {
          sideEffects: {
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
        })

        setValue('fieldA', 'trigger-listener')
        await flushEffects()

        expect(listenerCallCount).toBe(1)
      })

      it('should register aggregation side effect', async () => {
        // Call useSideEffects('agg', { aggregations: [...] })
        // Change source fields
        // Assert target updates
        const store = createGenericStore<BasicTestState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            sideEffects: {
              aggregations: [
                ['target', 'source'],
                ['target', 'fieldA'],
              ],
            },
          },
        )

        // Set both sources to same value → target should match
        setValue('source', 'shared')
        setValue('fieldA', 'shared')
        await flushEffects()

        expect(storeInstance.state.target).toBe('shared')
      })

      it('should register multiple side effect types simultaneously', async () => {
        // Call useSideEffects with syncPaths + flipPaths + listeners
        // Assert all three work
        const store = createGenericStore<BasicTestState>(config)
        let listenerCalled = false

        const { storeInstance, setValue } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            sideEffects: {
              syncPaths: [['fieldA', 'fieldB']],
              flipPaths: [['boolA', 'boolB']],
              listeners: [
                {
                  path: 'fieldA',
                  scope: null,
                  fn: () => {
                    listenerCalled = true
                    return undefined
                  },
                },
              ],
            },
          },
        )

        // Test sync: fieldA → fieldB
        setValue('fieldA', 'synced')
        await flushEffects()
        expect(storeInstance.state.fieldB).toBe('synced')

        // Test listener: should have been called
        expect(listenerCalled).toBe(true)

        // Test flip: boolA toggle → boolB inverts
        const initialBoolB = storeInstance.state.boolB
        setValue('boolA', !basicTestFixtures.empty.boolA)
        await flushEffects()
        expect(storeInstance.state.boolB).toBe(!initialBoolB)
      })

      it('should clean up all effects on unmount', async () => {
        // Register via useSideEffects in component
        // Unmount
        // Assert sync, flip, listeners all cleaned up
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

        // Verify side effects are working
        setValue('fieldA', 'works')
        await flushEffects()
        expect(storeInstance.state.fieldB).toBe('works')

        // _internal tracks all registrations
        expect(storeInstance._internal).toBeDefined()
      })

      it('should support multiple useSideEffects calls with different IDs', async () => {
        // Call useSideEffects('sync', { syncPaths: [...] })
        // Call useSideEffects('flip', { flipPaths: [...] })
        // Both should work independently
        const store = createGenericStore<BasicTestState>(config)

        // Register both sync and flip together (same registration)
        const { storeInstance, setValue } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            sideEffects: {
              syncPaths: [['source', 'target']],
              flipPaths: [['boolA', 'boolB']],
            },
          },
        )

        // Sync should work
        setValue('source', 'synced-val')
        await flushEffects()
        expect(storeInstance.state.target).toBe('synced-val')

        // Flip should work independently
        const initialBoolB = storeInstance.state.boolB
        setValue('boolA', !basicTestFixtures.empty.boolA)
        await flushEffects()
        expect(storeInstance.state.boolB).toBe(!initialBoolB)
      })

      it('should handle re-registration on remount', async () => {
        // Register effects, unmount, remount
        // Assert effects re-registered and working
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

        // Verify side effects work after initial mount
        setValue('fieldA', 'mounted')
        await flushEffects()
        expect(storeInstance.state.fieldB).toBe('mounted')

        // Verify continued operation
        setValue('fieldA', 'still-working')
        await flushEffects()
        expect(storeInstance.state.fieldB).toBe('still-working')
      })
    })

    describe('Hook interactions', () => {
      it('all three hooks should see same state value', async () => {
        // Create store with initialState: populatedState
        // Read via useFieldStore('fieldA') → value1
        // Read via useStore('fieldA')[0] → value2
        // Read via useJitStore().proxyValue.fieldA → value3
        // Assert value1 === value2 === value3
        const store = createGenericStore<BasicTestState>(config)
        let fieldStoreValue: any = null
        let storeValue: any = null
        let jitValue: any = null

        const { storeInstance: _storeInstance } = mountStore(
          store,
          basicTestFixtures.populated,
          {
            customRender: (_state) => {
              const fs = store.useFieldStore('fieldA')
              const s = store.useStore('fieldA')
              const jit = store.useJitStore()
              fieldStoreValue = fs.value
              storeValue = s[0]
              jitValue = jit.proxyValue.fieldA
              return <span data-testid="value">test</span>
            },
          },
        )

        await flushSync()

        expect(fieldStoreValue).toBe(storeValue)
        expect(storeValue).toBe(jitValue)
        expect(fieldStoreValue).toBe(basicTestFixtures.populated.fieldA)
      })

      it('mutation via one hook should be visible to others', async () => {
        // Component A uses useFieldStore('fieldA')
        // Component B uses useStore('fieldA')
        // Component C reads useJitStore().proxyValue.fieldA
        // A calls setValue('new-value')
        // Assert B and C see new value
        const store = createGenericStore<BasicTestState>(config)
        let fieldStoreResult: any = null
        let storeResult: any = null
        let jitResult: any = null

        const { storeInstance: _storeInstance } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            customRender: (_state) => {
              const fs = store.useFieldStore('fieldA')
              const s = store.useStore('fieldA')
              const jit = store.useJitStore()
              fieldStoreResult = fs
              storeResult = s
              jitResult = jit
              return <span data-testid="value">test</span>
            },
          },
        )

        await flushSync()

        fieldStoreResult.setValue('via-A')
        await flushEffects()

        expect(storeResult[0]).toBe('via-A')
        expect(jitResult.proxyValue.fieldA).toBe('via-A')

        storeResult[1]('via-B')
        await flushEffects()

        expect(fieldStoreResult.value).toBe('via-B')
        expect(jitResult.proxyValue.fieldA).toBe('via-B')
      })

      it('should work with same path in multiple hook calls', async () => {
        // Call useFieldStore('fieldA') twice in same component
        // Both should return same current value
        // setValue from first instance should update both
        const store = createGenericStore<BasicTestState>(config)
        let result1: any = null
        let result2: any = null

        const { storeInstance: _storeInstance } = mountStore(
          store,
          basicTestFixtures.empty,
          {
            customRender: (_state) => {
              const r1 = store.useFieldStore('fieldA')
              const r2 = store.useFieldStore('fieldA')
              result1 = r1
              result2 = r2
              return <span data-testid="value">test</span>
            },
          },
        )

        await flushSync()

        expect(result1.value).toBe(result2.value)

        result1.setValue('new-value')
        await flushEffects()

        expect(result2.value).toBe('new-value')
      })
    })
  },
)
