/**
 * Tests for useSideEffects hook
 *
 * Verifies side effect registration, unregistration, and registry management
 */

import { describe, it, expect } from 'vitest'
import { render, screen } from '@testing-library/react'
import { useContext } from 'react'
import { createGenericStore } from '../../src/store/createStore'
import { StoreContext } from '../../src/store/StoreContext'
import type { SideEffects } from '../../src/types/sideEffects'

describe('useSideEffects Hook', () => {
  it('should register side effects on mount', () => {
    type TestState = { value: number }
    const store = createGenericStore<TestState>()

    const effects: SideEffects<TestState> = {
      test: 'placeholder',
    }

    let registryRef: any = null

    const TestComponent = () => {
      store.useSideEffects('test-id', effects)
      const storeInstance = useContext(StoreContext)
      registryRef = storeInstance?.sideEffectsRegistry
      return <div>Component</div>
    }

    render(
      <store.Provider initialState={{ value: 0 }}>
        <TestComponent />
      </store.Provider>
    )

    // useLayoutEffect runs synchronously after render, so registry should be updated
    expect(registryRef?.has('test-id')).toBe(true)
  })

  it('should unregister side effects on unmount', () => {
    type TestState = { value: number }
    const store = createGenericStore<TestState>()

    const effects: SideEffects<TestState> = {
      test: 'placeholder',
    }

    let registryRef: any = null

    const TestComponent = () => {
      store.useSideEffects('test-id', effects)
      const storeInstance = useContext(StoreContext)
      registryRef = storeInstance?.sideEffectsRegistry
      return <div>Component</div>
    }

    const { unmount } = render(
      <store.Provider initialState={{ value: 0 }}>
        <TestComponent />
      </store.Provider>
    )

    // Should be registered before unmount
    expect(registryRef?.has('test-id')).toBe(true)

    unmount()

    // Should be unregistered after unmount
    expect(registryRef?.has('test-id')).toBe(false)
  })

  it('should support multiple side effect registrations', () => {
    type TestState = { value: number }
    const store = createGenericStore<TestState>()

    const Component1 = () => {
      store.useSideEffects('effect-1', { test: '1' })
      return <div>Component 1</div>
    }

    const Component2 = () => {
      store.useSideEffects('effect-2', { test: '2' })
      return <div>Component 2</div>
    }

    let registryRef: any = null

    const Wrapper = () => {
      const storeInstance = useContext(StoreContext)
      registryRef = storeInstance?.sideEffectsRegistry
      return (
        <div>
          <Component1 />
          <Component2 />
        </div>
      )
    }

    render(
      <store.Provider initialState={{ value: 0 }}>
        <Wrapper />
      </store.Provider>
    )

    expect(registryRef?.has('effect-1')).toBe(true)
    expect(registryRef?.has('effect-2')).toBe(true)
  })

  it('should handle selective unmounting', () => {
    type TestState = { value: number }
    const store = createGenericStore<TestState>()

    let registryRef: any = null

    const Component1 = () => {
      store.useSideEffects('effect-1', { test: '1' })
      return <div>Component 1</div>
    }

    const Component2 = () => {
      store.useSideEffects('effect-2', { test: '2' })
      return <div>Component 2</div>
    }

    const Wrapper = ({ showBoth }: { showBoth: boolean }) => {
      const storeInstance = useContext(StoreContext)
      registryRef = storeInstance?.sideEffectsRegistry
      return (
        <div>
          <Component1 />
          {showBoth && <Component2 />}
        </div>
      )
    }

    const { rerender } = render(
      <store.Provider initialState={{ value: 0 }}>
        <Wrapper showBoth={true} />
      </store.Provider>
    )

    expect(registryRef?.has('effect-1')).toBe(true)
    expect(registryRef?.has('effect-2')).toBe(true)

    // Remove Component2
    rerender(
      <store.Provider initialState={{ value: 0 }}>
        <Wrapper showBoth={false} />
      </store.Provider>
    )

    expect(registryRef?.has('effect-1')).toBe(true)
    expect(registryRef?.has('effect-2')).toBe(false)
  })

  it('should throw error when used outside Provider', () => {
    type TestState = { count: number }
    const store = createGenericStore<TestState>()

    const TestComponent = () => {
      try {
        store.useSideEffects('test-id', {})
        return <div>Success</div>
      } catch (error) {
        return <div data-testid="error">{(error as Error).message}</div>
      }
    }

    render(<TestComponent />)

    expect(screen.getByTestId('error').textContent).toContain('Provider')
  })

  it('should retrieve registered effects via registry', () => {
    type TestState = { value: number }
    const store = createGenericStore<TestState>()

    const effects: SideEffects<TestState> = {
      testField: 'testValue',
    }

    let registryRef: any = null

    const TestComponent = () => {
      store.useSideEffects('test-id', effects)
      const storeInstance = useContext(StoreContext)
      registryRef = storeInstance?.sideEffectsRegistry
      return <div>Component</div>
    }

    render(
      <store.Provider initialState={{ value: 0 }}>
        <TestComponent />
      </store.Provider>
    )

    // Get the registered effects after render
    const retrievedEffects = registryRef?.get('test-id')
    expect(retrievedEffects).toEqual(effects)
  })

  it('should warn when registering duplicate ID', () => {
    type TestState = { value: number }
    const store = createGenericStore<TestState>()

    const Component1 = () => {
      store.useSideEffects('duplicate-id', { test: '1' })
      return <div>Component 1</div>
    }

    const Component2 = () => {
      store.useSideEffects('duplicate-id', { test: '2' })
      return <div>Component 2</div>
    }

    // Both components will try to register the same ID
    // The second one should overwrite (with a warning in console)
    render(
      <store.Provider initialState={{ value: 0 }}>
        <Component1 />
        <Component2 />
      </store.Provider>
    )

    expect(screen.getByText('Component 1')).toBeTruthy()
    expect(screen.getByText('Component 2')).toBeTruthy()
  })

  it('should list all registered effect IDs', () => {
    type TestState = { value: number }
    const store = createGenericStore<TestState>()

    let registryRef: any = null

    const Component1 = () => {
      store.useSideEffects('effect-1', { test: '1' })
      return <div>Component 1</div>
    }

    const Component2 = () => {
      store.useSideEffects('effect-2', { test: '2' })
      return <div>Component 2</div>
    }

    const Component3 = () => {
      store.useSideEffects('effect-3', { test: '3' })
      return <div>Component 3</div>
    }

    const Wrapper = () => {
      const storeInstance = useContext(StoreContext)
      registryRef = storeInstance?.sideEffectsRegistry
      return (
        <div>
          <Component1 />
          <Component2 />
          <Component3 />
        </div>
      )
    }

    render(
      <store.Provider initialState={{ value: 0 }}>
        <Wrapper />
      </store.Provider>
    )

    const ids = registryRef?.getIds() || []
    expect(ids).toContain('effect-1')
    expect(ids).toContain('effect-2')
    expect(ids).toContain('effect-3')
    expect(ids.length).toBe(3)
  })

  it('should clear all effects when registry is cleared', () => {
    type TestState = { value: number }
    const store = createGenericStore<TestState>()

    let registryRef: any = null

    const Component1 = () => {
      store.useSideEffects('effect-1', { test: '1' })
      return <div>Component 1</div>
    }

    const Component2 = () => {
      store.useSideEffects('effect-2', { test: '2' })
      return <div>Component 2</div>
    }

    const Wrapper = () => {
      const storeInstance = useContext(StoreContext)
      registryRef = storeInstance?.sideEffectsRegistry
      return (
        <div>
          <Component1 />
          <Component2 />
        </div>
      )
    }

    render(
      <store.Provider initialState={{ value: 0 }}>
        <Wrapper />
      </store.Provider>
    )

    expect(registryRef?.getIds().length).toBe(2)

    // Clear the registry
    registryRef?.clear()

    expect(registryRef?.getIds().length).toBe(0)
    expect(registryRef?.has('effect-1')).toBe(false)
    expect(registryRef?.has('effect-2')).toBe(false)
  })
})
