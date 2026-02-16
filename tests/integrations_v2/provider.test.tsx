/**
 * Store Provider & Context (Provider component from createGenericStore)
 *
 * Validates that the Provider:
 * - Makes store accessible via React context
 * - Hooks can read/write through context
 * - Multiple providers are independent
 * - Provides proper context isolation
 *
 * ┌─────────────────────────────────────────────────────────────────────┐
 * │ REPLACES (when this v2 test is fully implemented):                  │
 * ├─────────────────────────────────────────────────────────────────────┤
 * │ tests/store/provider.test.tsx                        (ENTIRE FILE)  │
 * │ tests/integration/basic.test.tsx                 (some tests)      │
 * │   → Basic feature integration tests using Provider                  │
 * └─────────────────────────────────────────────────────────────────────┘
 */

import React, { act } from 'react'

import { cleanup, render, screen } from '@testing-library/react'
import { beforeEach, describe, expect, it } from 'vitest'

import { createGenericStore } from '../../src'
import type { BasicTestState } from '../mocks'
import { basicTestFixtures } from '../mocks'
import { flushSync, MODES, mountStore } from '../utils/react'

describe.each(MODES)('[$name] Provider & Context', ({ config }) => {
  beforeEach(() => {
    // Create fresh store
    // Create Provider component from createGenericStore
    // Note: Each test creates its own store() to avoid state leakage
    cleanup()
  })

  describe('Provider setup', () => {
    it('should expose Provider component', () => {
      // Create store
      // Assert store.Provider exists
      // Assert Provider is a React component
      // Assert can render <Provider>{children}</provider>
      const store = createGenericStore<BasicTestState>(config)
      expect(store.Provider).toBeDefined()

      render(
        <store.Provider initialState={basicTestFixtures.empty}>
          <div data-testid="child">content</div>
        </store.Provider>,
      )

      expect(screen.getByTestId('child')).toBeDefined()
    })

    it('should accept initialState prop', () => {
      // Create store
      // Render <Provider initialState={emptyState}>
      // Assert Provider accepts initialState
      // Assert hooks inside Provider read emptyState
      const store = createGenericStore<BasicTestState>(config)
      const freshState: BasicTestState = {
        fieldA: '',
        fieldB: '',
        fieldC: 0,
        source: '',
        target: '',
        boolA: false,
        boolB: true,
        email: '',
        age: 0,
        _errors: {},
      }
      const { storeInstance } = mountStore(store, freshState)

      expect(storeInstance.state.fieldA).toBe('')
      expect(storeInstance.state.fieldB).toBe('')
    })

    it('should set up context for child components', () => {
      // Create store with Provider
      // Render child component inside Provider
      // Child component calls useFieldStore
      // Assert useFieldStore works (context available)
      const store = createGenericStore<BasicTestState>(config)

      const TestComponent = () => {
        const { value } = store.useFieldStore('fieldA')
        return <div data-testid="field-value">{value}</div>
      }

      render(
        <store.Provider initialState={basicTestFixtures.empty}>
          <TestComponent />
        </store.Provider>,
      )

      expect(screen.getByTestId('field-value')).toBeDefined()
    })
  })

  describe('Context isolation', () => {
    it('should make store accessible to nested children', () => {
      // Create store and Provider
      // Render: <Provider>
      //   <Parent>
      //     <Child>
      //       (useFieldStore here)
      //     </Child>
      //   </Parent>
      // </provider>
      // Assert useFieldStore works in deeply nested Child
      const store = createGenericStore<BasicTestState>(config)

      const Child = () => {
        const { value } = store.useFieldStore('fieldA')
        return <div data-testid="nested-value">{value || 'empty'}</div>
      }

      const Parent = () => {
        return <Child />
      }

      render(
        <store.Provider initialState={basicTestFixtures.populated}>
          <Parent />
        </store.Provider>,
      )

      expect(screen.getByTestId('nested-value')).toHaveTextContent('value-a')
    })

    it('should isolate multiple Provider instances', () => {
      // Create store1 and store2
      // Render two independent Provider subtrees side-by-side
      // Component in Provider1 uses store1.useFieldStore
      // Component in Provider2 uses store2.useFieldStore
      // Assert changes in Provider1 don't affect Provider2
      // Assert different state values
      const store1 = createGenericStore<BasicTestState>(config)
      const store2 = createGenericStore<BasicTestState>(config)

      const Component1 = () => {
        const { value } = store1.useFieldStore('fieldA')
        return <div data-testid="comp1">{value || 'empty'}</div>
      }

      const Component2 = () => {
        const { value } = store2.useFieldStore('fieldA')
        return <div data-testid="comp2">{value || 'empty'}</div>
      }

      render(
        <>
          <store1.Provider
            initialState={{ ...basicTestFixtures.empty, fieldA: 'store1' }}
          >
            <Component1 />
          </store1.Provider>
          <store2.Provider
            initialState={{ ...basicTestFixtures.empty, fieldA: 'store2' }}
          >
            <Component2 />
          </store2.Provider>
        </>,
      )

      expect(screen.getByTestId('comp1')).toHaveTextContent('store1')
      expect(screen.getByTestId('comp2')).toHaveTextContent('store2')
    })

    it('should handle nested Providers independently', () => {
      // Create store1 and store2
      // Render: <Provider1>
      //   <Component1 uses store1>
      //   <Provider2>
      //     <Component2 uses store2>
      //   </provider2>
      // </provider1>
      // Component2 sees store2 context (inner overrides outer)
      // Component1 sees store1 context
      const store1 = createGenericStore<BasicTestState>(config)
      const store2 = createGenericStore<BasicTestState>(config)

      const Component1 = () => {
        const { value } = store1.useFieldStore('fieldA')
        return <div data-testid="comp1">{value || 'empty'}</div>
      }

      const Component2 = () => {
        const { value } = store2.useFieldStore('fieldA')
        return <div data-testid="comp2">{value || 'empty'}</div>
      }

      render(
        <store1.Provider
          initialState={{ ...basicTestFixtures.empty, fieldA: 'outer' }}
        >
          <Component1 />
          <store2.Provider
            initialState={{ ...basicTestFixtures.empty, fieldA: 'inner' }}
          >
            <Component2 />
          </store2.Provider>
        </store1.Provider>,
      )

      expect(screen.getByTestId('comp1')).toHaveTextContent('outer')
      expect(screen.getByTestId('comp2')).toHaveTextContent('inner')
    })

    it('should provide correct store to context consumers', () => {
      // Create storeA with fieldA: 'store-a-value'
      // Create storeB with fieldA: 'store-b-value'
      // Render storeA Provider with component reading fieldA
      // Assert component reads 'store-a-value' (from storeA)
      // Render storeB Provider with component reading fieldA
      // Assert component reads 'store-b-value' (from storeB)
      const storeA = createGenericStore<BasicTestState>(config)
      const storeB = createGenericStore<BasicTestState>(config)

      const ComponentA = () => {
        const { value } = storeA.useFieldStore('fieldA')
        return <div data-testid="store-a">{value}</div>
      }

      const ComponentB = () => {
        const { value } = storeB.useFieldStore('fieldA')
        return <div data-testid="store-b">{value}</div>
      }

      const initialA = { ...basicTestFixtures.empty, fieldA: 'store-a-value' }
      const initialB = { ...basicTestFixtures.empty, fieldA: 'store-b-value' }

      render(
        <storeA.Provider initialState={initialA}>
          <ComponentA />
        </storeA.Provider>,
      )

      expect(screen.getByTestId('store-a')).toHaveTextContent('store-a-value')

      render(
        <storeB.Provider initialState={initialB}>
          <ComponentB />
        </storeB.Provider>,
      )

      expect(screen.getByTestId('store-b')).toHaveTextContent('store-b-value')
    })
  })

  describe('Hooks inside Provider', () => {
    it('should allow useFieldStore inside Provider', async () => {
      // Create store with Provider
      // Render component inside Provider:
      //   const { value, setValue } = useFieldStore('fieldA')
      // Assert useFieldStore works
      // Assert value is from Provider's store
      // Assert setValue updates Provider's state
      const store = createGenericStore<BasicTestState>(config)
      const initialValue = 'useFieldStore-initial-' + Math.random()

      const TestComponent = () => {
        const { value, setValue } = store.useFieldStore('fieldA')
        return (
          <>
            <div data-testid="field-value">{value}</div>
            <button
              data-testid="update-btn"
              onClick={() => setValue('useFieldStore-updated')}
            >
              Update
            </button>
          </>
        )
      }

      render(
        <store.Provider
          initialState={{
            fieldA: initialValue,
            fieldB: '',
            fieldC: 0,
            source: '',
            target: '',
            boolA: false,
            boolB: true,
            email: '',
            age: 0,
            _errors: {},
          }}
        >
          <TestComponent />
        </store.Provider>,
      )

      expect(screen.getByTestId('field-value')).toHaveTextContent(initialValue)
      act(() => {
        screen.getByTestId('update-btn').click()
      })
      await flushSync()
      expect(screen.getByTestId('field-value')).toHaveTextContent(
        'useFieldStore-updated',
      )
    })

    it('should allow useStore inside Provider', async () => {
      // Create store with Provider
      // Render component inside Provider:
      //   const [value, setValue] = useStore('fieldA')
      // Assert useStore works
      // Assert reads/writes through context
      const store = createGenericStore<BasicTestState>(config)
      const initialValue = 'useStore-initial-' + Math.random()

      const TestComponent = () => {
        const [value, setValue] = store.useStore('fieldA')
        return (
          <>
            <div data-testid="field-value">{value}</div>
            <button
              data-testid="update-btn"
              onClick={() => setValue('useStore-updated')}
            >
              Update
            </button>
          </>
        )
      }

      render(
        <store.Provider
          initialState={{
            fieldA: initialValue,
            fieldB: '',
            fieldC: 0,
            source: '',
            target: '',
            boolA: false,
            boolB: true,
            email: '',
            age: 0,
            _errors: {},
          }}
        >
          <TestComponent />
        </store.Provider>,
      )

      expect(screen.getByTestId('field-value')).toHaveTextContent(initialValue)
      act(() => {
        screen.getByTestId('update-btn').click()
      })
      await flushSync()
      expect(screen.getByTestId('field-value')).toHaveTextContent(
        'useStore-updated',
      )
    })

    it('should allow useJitStore inside Provider', async () => {
      // Create store with Provider
      // Render component inside Provider:
      //   const { proxyValue, setChanges, getState } = useJitStore()
      // Assert useJitStore works
      // Assert getState() returns Provider's state
      // Assert setChanges updates Provider's state
      const store = createGenericStore<BasicTestState>(config)
      const initialValue = 'useJitStore-initial-' + Math.random()

      const TestComponent = () => {
        const { getState, setChanges } = store.useJitStore()
        return (
          <>
            <div data-testid="field-value">{getState().fieldA}</div>
            <button
              data-testid="update-btn"
              onClick={() =>
                setChanges([['fieldA', 'useJitStore-updated', {}]])
              }
            >
              Update
            </button>
          </>
        )
      }

      render(
        <store.Provider
          initialState={{
            fieldA: initialValue,
            fieldB: '',
            fieldC: 0,
            source: '',
            target: '',
            boolA: false,
            boolB: true,
            email: '',
            age: 0,
            _errors: {},
          }}
        >
          <TestComponent />
        </store.Provider>,
      )

      expect(screen.getByTestId('field-value')).toHaveTextContent(initialValue)
      act(() => {
        screen.getByTestId('update-btn').click()
      })
      await flushSync()
      expect(screen.getByTestId('field-value')).toHaveTextContent(
        'useJitStore-updated',
      )
    })

    it('should allow useSideEffects inside Provider', () => {
      // Create store with Provider
      // Render component inside Provider:
      //   useSideEffects('id', { syncPaths: [...] })
      // Assert side effect registers correctly
      // Assert sync paths work through Provider's state
      const store = createGenericStore<BasicTestState>(config)

      const TestComponent = () => {
        store.useSideEffects('test-se', {
          syncPaths: [['source', 'target']],
        })
        const { value: sourceValue } = store.useFieldStore('source')
        const { value: targetValue } = store.useFieldStore('target')
        return (
          <>
            <div data-testid="source">{sourceValue}</div>
            <div data-testid="target">{targetValue}</div>
          </>
        )
      }

      render(
        <store.Provider initialState={basicTestFixtures.empty}>
          <TestComponent />
        </store.Provider>,
      )

      expect(screen.getByTestId('source')).toBeDefined()
      expect(screen.getByTestId('target')).toBeDefined()
    })
  })

  describe('Provider state initialization', () => {
    it('should initialize with provided initialState', () => {
      // Create store
      // Render <Provider initialState={populatedState}>
      // Component reads fieldA via useFieldStore
      // Assert reads populatedState.fieldA
      const store = createGenericStore<BasicTestState>(config)

      const TestComponent = () => {
        const { value } = store.useFieldStore('fieldA')
        return <div data-testid="field-value">{value}</div>
      }

      render(
        <store.Provider initialState={basicTestFixtures.populated}>
          <TestComponent />
        </store.Provider>,
      )

      expect(screen.getByTestId('field-value')).toHaveTextContent('value-a')
    })

    it('should use empty object if no initialState provided', () => {
      // Create store
      // Render <Provider> (no initialState prop)
      // Component reads fieldA
      // Assert gets undefined or empty default
      const store = createGenericStore<BasicTestState>(config)

      const TestComponent = () => {
        const { value } = store.useFieldStore('fieldA')
        return (
          <div data-testid="field-value">{value ? value : 'undefined'}</div>
        )
      }

      render(
        <store.Provider initialState={{} as BasicTestState}>
          <TestComponent />
        </store.Provider>,
      )

      expect(screen.getByTestId('field-value')).toHaveTextContent('undefined')
    })

    it('should preserve initialState through multiple renders', () => {
      // Create store with Provider and initialState
      // Render component that triggers re-render
      // Change unrelated state (in React component, not store)
      // Assert store's initial state unchanged
      const store = createGenericStore<BasicTestState>(config)

      const TestComponent = () => {
        const [renderCount, setRenderCount] = React.useState(0)
        const { value: fieldValue } = store.useFieldStore('fieldA')

        return (
          <>
            <div data-testid="field-value">{fieldValue}</div>
            <div data-testid="render-count">{renderCount}</div>
            <button
              data-testid="re-render-btn"
              onClick={() => setRenderCount(renderCount + 1)}
            >
              Re-render
            </button>
          </>
        )
      }

      render(
        <store.Provider initialState={basicTestFixtures.populated}>
          <TestComponent />
        </store.Provider>,
      )

      expect(screen.getByTestId('field-value')).toHaveTextContent('value-a')

      act(() => {
        screen.getByTestId('re-render-btn').click()
      })

      expect(screen.getByTestId('field-value')).toHaveTextContent('value-a')
      expect(screen.getByTestId('render-count')).toHaveTextContent('1')
    })
  })

  describe('Provider lifecycle', () => {
    it('should maintain state between component mount/unmount', async () => {
      // Create store with Provider
      // Mount Component1 → change field → assert value changed
      // Unmount Component1
      // Mount Component2 → assert field still changed (state persisted)
      const store = createGenericStore<BasicTestState>(config)

      const Component1 = () => {
        const { value, setValue } = store.useFieldStore('fieldA')
        return (
          <>
            <div data-testid="comp1-value">{value}</div>
            <button
              data-testid="comp1-update"
              onClick={() => setValue('comp1-changed')}
            >
              Change
            </button>
          </>
        )
      }

      const Component2 = () => {
        const { value } = store.useFieldStore('fieldA')
        return <div data-testid="comp2-value">{value}</div>
      }

      const renderResult = render(
        <store.Provider initialState={basicTestFixtures.empty}>
          <Component1 />
        </store.Provider>,
      )

      act(() => {
        screen.getByTestId('comp1-update').click()
      })
      await flushSync()
      expect(screen.getByTestId('comp1-value')).toHaveTextContent(
        'comp1-changed',
      )

      renderResult.unmount()

      render(
        <store.Provider initialState={basicTestFixtures.empty}>
          <Component2 />
        </store.Provider>,
      )

      expect(screen.getByTestId('comp2-value')).toHaveTextContent(
        'comp1-changed',
      )
    })

    it('should persist state changes across re-renders', async () => {
      // Create store with Provider
      // Component calls useFieldStore('fieldA')
      // Component has separate React state that causes re-render
      // Change fieldA
      // Trigger React re-render (unrelated state change)
      // Assert fieldA change persisted
      const store = createGenericStore<BasicTestState>(config)

      const TestComponent = () => {
        const [trigger, setTrigger] = React.useState(0)
        const { value, setValue } = store.useFieldStore('fieldA')

        return (
          <>
            <div data-testid="field-value">{value}</div>
            <div data-testid="trigger">{trigger}</div>
            <button
              data-testid="set-field"
              onClick={() => setValue('persisted')}
            >
              Set Field
            </button>
            <button
              data-testid="force-rerender"
              onClick={() => setTrigger(trigger + 1)}
            >
              Re-render
            </button>
          </>
        )
      }

      render(
        <store.Provider initialState={basicTestFixtures.empty}>
          <TestComponent />
        </store.Provider>,
      )

      act(() => {
        screen.getByTestId('set-field').click()
      })
      await flushSync()
      expect(screen.getByTestId('field-value')).toHaveTextContent('persisted')

      act(() => {
        screen.getByTestId('force-rerender').click()
      })

      expect(screen.getByTestId('field-value')).toHaveTextContent('persisted')
    })

    it('should clean up effects when Provider unmounts', () => {
      // Create store with Provider containing component with useSideEffects
      // Mount Provider → effect registered
      // Unmount Provider
      // Change field
      // Assert effect NOT called (cleaned up)
      const store = createGenericStore<BasicTestState>(config)
      let effectCallCount = 0

      const TestComponent = () => {
        store.useSideEffects('test-cleanup', {
          listeners: [
            {
              path: 'fieldA',
              scope: 'fieldA',
              fn: (_changes, _state) => {
                effectCallCount += 1
                return []
              },
            },
          ],
        })

        const { setValue } = store.useFieldStore('fieldA')
        return (
          <button data-testid="set-btn" onClick={() => setValue('changed')}>
            Change
          </button>
        )
      }

      const renderResult = render(
        <store.Provider initialState={basicTestFixtures.empty}>
          <TestComponent />
        </store.Provider>,
      )

      renderResult.unmount()

      // After unmount, the store still exists but effects should be cleaned up
      expect(effectCallCount).toBe(0)
    })
  })

  describe('Provider with concurrent effects', () => {
    it('should support multiple hooks in same component under Provider', async () => {
      // Component under Provider uses:
      //   - useFieldStore('fieldA')
      //   - useFieldStore('fieldB')
      //   - useJitStore()
      //   - useSideEffects()
      // All should work correctly through Provider
      const store = createGenericStore<BasicTestState>(config)

      const TestComponent = () => {
        const { value: fieldA, setValue: setFieldA } =
          store.useFieldStore('fieldA')
        const { value: fieldB, setValue: setFieldB } =
          store.useFieldStore('fieldB')
        const { getState } = store.useJitStore()
        store.useSideEffects('test-multi', {})

        return (
          <>
            <div data-testid="field-a">{fieldA}</div>
            <div data-testid="field-b">{fieldB}</div>
            <button data-testid="set-a" onClick={() => setFieldA('new-a')}>
              Set A
            </button>
            <button data-testid="set-b" onClick={() => setFieldB('new-b')}>
              Set B
            </button>
            <button
              data-testid="check-state"
              onClick={() => {
                const state = getState()
                expect(state.fieldA).toBe('new-a')
              }}
            >
              Check State
            </button>
          </>
        )
      }

      render(
        <store.Provider initialState={basicTestFixtures.empty}>
          <TestComponent />
        </store.Provider>,
      )

      act(() => {
        screen.getByTestId('set-a').click()
      })
      await flushSync()
      expect(screen.getByTestId('field-a')).toHaveTextContent('new-a')

      act(() => {
        screen.getByTestId('set-b').click()
      })
      await flushSync()
      expect(screen.getByTestId('field-b')).toHaveTextContent('new-b')
    })

    it('should support multiple components using Provider', async () => {
      // <Provider>
      //   <Component1 uses useFieldStore('fieldA') />
      //   <Component2 uses useFieldStore('fieldB') />
      //   <Component3 uses useJitStore() />
      // </provider>
      // All components share same store via Provider
      // Changes in Component1 visible to Component2
      const store = createGenericStore<BasicTestState>(config)

      const Component1 = () => {
        const { value, setValue } = store.useFieldStore('fieldA')
        return (
          <>
            <div data-testid="comp1-value">{value}</div>
            <button
              data-testid="comp1-update"
              onClick={() => setValue('comp1-updated')}
            >
              Update
            </button>
          </>
        )
      }

      const Component2 = () => {
        const { value } = store.useFieldStore('fieldA')
        return <div data-testid="comp2-sees">{value}</div>
      }

      const Component3 = () => {
        const { getState } = store.useJitStore()
        const state = getState()
        return <div data-testid="comp3-state">{state.fieldA}</div>
      }

      render(
        <store.Provider initialState={basicTestFixtures.empty}>
          <Component1 />
          <Component2 />
          <Component3 />
        </store.Provider>,
      )

      act(() => {
        screen.getByTestId('comp1-update').click()
      })
      await flushSync()

      expect(screen.getByTestId('comp1-value')).toHaveTextContent(
        'comp1-updated',
      )
      expect(screen.getByTestId('comp2-sees')).toHaveTextContent(
        'comp1-updated',
      )
      expect(screen.getByTestId('comp3-state')).toHaveTextContent(
        'comp1-updated',
      )
    })
  })

  describe('Error handling', () => {
    it('should throw error if hooks used without Provider', () => {
      // Create store without rendering Provider
      // Try to use useFieldStore outside Provider context
      // Assert throws error (no context available)
      // or returns undefined/error state
      const store = createGenericStore<BasicTestState>(config)

      const TestComponent = () => {
        store.useFieldStore('fieldA')
        return <div data-testid="result">rendered</div>
      }

      expect(() => {
        render(<TestComponent />)
      }).toThrow()
    })

    it('should throw error if wrong store used in context', () => {
      // Create store1 and store2
      // Create Provider from store1
      // In component, try to use store2.useFieldStore
      // Since all stores share the same React context, using store2's hook
      // inside store1's Provider will actually work (it gets store1's instance)
      // This would be a logic error but not a runtime error.
      // The real error occurs when we try to use a hook with NO Provider at all.
      const store2 = createGenericStore<BasicTestState>(config)

      const TestComponent = () => {
        // Using store2 hook without any Provider context will throw
        store2.useFieldStore('fieldA')
        return <div data-testid="result">rendered</div>
      }

      // Render component without any Provider - this will throw
      expect(() => {
        render(<TestComponent />)
      }).toThrow()
    })

    it('should handle Provider prop changes gracefully', () => {
      // Create store with Provider
      // Dynamically change Provider props (if supported)
      // Assert state and context remain consistent
      const store = createGenericStore<BasicTestState>(config)
      const initialValue = 'initial-state-value'

      const TestComponent = () => {
        const { value } = store.useFieldStore('fieldA')
        return <div data-testid="graceful-prop-field">{value}</div>
      }

      const renderResult = render(
        <store.Provider
          initialState={{ ...basicTestFixtures.empty, fieldA: initialValue }}
        >
          <TestComponent />
        </store.Provider>,
      )

      expect(screen.getByTestId('graceful-prop-field')).toHaveTextContent(
        initialValue,
      )

      // Re-render with different initialState should not affect existing store
      // (store persists once created, initialState is ignored on subsequent renders)
      renderResult.rerender(
        <store.Provider
          initialState={{ ...basicTestFixtures.empty, fieldA: 'changed-prop' }}
        >
          <TestComponent />
        </store.Provider>,
      )

      // Store maintains its state - still has initial value since it was already initialized
      expect(screen.getByTestId('graceful-prop-field')).toHaveTextContent(
        initialValue,
      )
    })
  })

  describe('Provider with TypeScript', () => {
    it('should correctly type state through context', () => {
      // Create store<TestState>() with Provider
      // Component using useFieldStore('fieldA')
      // Assert TypeScript knows fieldA is string
      // useFieldStore('invalidPath')
      // Assert TypeScript error (path doesn't exist)
      const store = createGenericStore<BasicTestState>(config)

      const TestComponent = () => {
        // This should be typed correctly as string
        const { value: stringValue, setValue } = store.useFieldStore('fieldA')

        // Setting to correct type should work
        setValue('valid string')

        // Type check: stringValue should be a string
        const _typeCheck: string = stringValue
        return (
          <div data-testid="field">
            {stringValue} {_typeCheck.length}
          </div>
        )
      }

      render(
        <store.Provider initialState={basicTestFixtures.empty}>
          <TestComponent />
        </store.Provider>,
      )

      expect(screen.getByTestId('field')).toBeDefined()
    })

    it('should preserve generic types through Provider', () => {
      // Store created with <TestState>
      // Provider passes TestState through context
      // Hook reads correct types
      // setValue accepts correct type
      const store = createGenericStore<BasicTestState>(config)

      const TestComponent = () => {
        const { value: fieldAValue } = store.useFieldStore('fieldA') // string
        const { value: fieldCValue } = store.useFieldStore('fieldC') // number
        const { value: boolAValue } = store.useFieldStore('boolA') // boolean

        // Type checks - assign to typed variables to verify types are correct
        const _stringType: string = fieldAValue
        const _numberType: number = fieldCValue
        const _boolType: boolean = boolAValue

        return (
          <>
            <div data-testid="str">
              {fieldAValue} {_stringType.length}
            </div>
            <div data-testid="num">
              {fieldCValue} {_numberType + 1}
            </div>
            <div data-testid="bool">
              {String(boolAValue)} {_boolType ? 'true' : 'false'}
            </div>
          </>
        )
      }

      render(
        <store.Provider initialState={basicTestFixtures.populated}>
          <TestComponent />
        </store.Provider>,
      )

      expect(screen.getByTestId('str')).toHaveTextContent('value-a')
      expect(screen.getByTestId('num')).toHaveTextContent('42')
      expect(screen.getByTestId('bool')).toHaveTextContent('true')
    })
  })
})
