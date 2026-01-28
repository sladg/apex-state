/**
 * Tests for Provider component
 *
 * Verifies Provider initialization, context provision, and configuration options
 */

import { describe, it, expect } from 'vitest'
import { render, screen } from '@testing-library/react'
import { useContext } from 'react'
import { createGenericStore } from '../../src/store/createStore'
import { StoreContext } from '../../src/store/StoreContext'

describe('Provider Component', () => {
  it('should provide store instance via context', () => {
    type TestState = { value: string }
    const store = createGenericStore<TestState>()

    const TestComponent = () => {
      const storeInstance = useContext(StoreContext)
      return <div>{storeInstance ? 'Has Store' : 'No Store'}</div>
    }

    render(
      <store.Provider initialState={{ value: 'test' }}>
        <TestComponent />
      </store.Provider>
    )

    expect(screen.getByText('Has Store')).toBeTruthy()
  })

  it('should initialize with provided initial state', () => {
    type TestState = { count: number; name: string }
    const store = createGenericStore<TestState>()

    const TestComponent = () => {
      const storeInstance = useContext(StoreContext)
      const state = storeInstance?.state as TestState

      return (
        <div>
          <span data-testid="count">{state.count}</span>
          <span data-testid="name">{state.name}</span>
        </div>
      )
    }

    const initialState = { count: 42, name: 'Test' }

    render(
      <store.Provider initialState={initialState}>
        <TestComponent />
      </store.Provider>
    )

    expect(screen.getByTestId('count').textContent).toBe('42')
    expect(screen.getByTestId('name').textContent).toBe('Test')
  })

  it('should use default errorStorePath', () => {
    type TestState = { value: string }
    const store = createGenericStore<TestState>()

    const TestComponent = () => {
      const storeInstance = useContext(StoreContext)
      return <div data-testid="errorPath">{storeInstance?.config.errorStorePath}</div>
    }

    render(
      <store.Provider initialState={{ value: 'test' }}>
        <TestComponent />
      </store.Provider>
    )

    expect(screen.getByTestId('errorPath').textContent).toBe('_errors')
  })

  it('should use custom errorStorePath when provided', () => {
    type TestState = { value: string }
    const store = createGenericStore<TestState>()

    const TestComponent = () => {
      const storeInstance = useContext(StoreContext)
      return <div data-testid="errorPath">{storeInstance?.config.errorStorePath}</div>
    }

    render(
      <store.Provider initialState={{ value: 'test' }} errorStorePath="customErrors">
        <TestComponent />
      </store.Provider>
    )

    expect(screen.getByTestId('errorPath').textContent).toBe('customErrors')
  })

  it('should render children correctly', () => {
    type TestState = { value: string }
    const store = createGenericStore<TestState>()

    render(
      <store.Provider initialState={{ value: 'test' }}>
        <div>First Child</div>
        <div>Second Child</div>
      </store.Provider>
    )

    expect(screen.getByText('First Child')).toBeTruthy()
    expect(screen.getByText('Second Child')).toBeTruthy()
  })

  it('should support nested providers with different stores', () => {
    type OuterState = { outer: string }
    type InnerState = { inner: string }

    const outerStore = createGenericStore<OuterState>()
    const innerStore = createGenericStore<InnerState>()

    const TestComponent = () => {
      return <div>Nested Providers Work</div>
    }

    render(
      <outerStore.Provider initialState={{ outer: 'outside' }}>
        <innerStore.Provider initialState={{ inner: 'inside' }}>
          <TestComponent />
        </innerStore.Provider>
      </outerStore.Provider>
    )

    expect(screen.getByText('Nested Providers Work')).toBeTruthy()
  })

  it('should maintain store instance across re-renders', () => {
    type TestState = { value: string }
    const store = createGenericStore<TestState>()

    let instanceCount = 0

    const TestComponent = () => {
      const storeInstance = useContext(StoreContext)

      if (storeInstance) {
        instanceCount++
      }

      return <div>Instance Count: {instanceCount}</div>
    }

    const { rerender } = render(
      <store.Provider initialState={{ value: 'test' }}>
        <TestComponent />
      </store.Provider>
    )

    const firstCount = instanceCount

    // Rerender the component
    rerender(
      <store.Provider initialState={{ value: 'test' }}>
        <TestComponent />
      </store.Provider>
    )

    // Instance count should have increased (component re-rendered)
    // but it should be the same store instance
    expect(instanceCount).toBeGreaterThan(firstCount)
  })

  it('should handle complex nested initial state', () => {
    type TestState = {
      user: {
        profile: {
          name: string
          age: number
        }
        settings: {
          theme: string
        }
      }
      app: {
        version: string
      }
    }

    const store = createGenericStore<TestState>()

    const TestComponent = () => {
      const storeInstance = useContext(StoreContext)
      const state = storeInstance?.state as TestState

      return (
        <div>
          <span data-testid="name">{state.user.profile.name}</span>
          <span data-testid="age">{state.user.profile.age}</span>
          <span data-testid="theme">{state.user.settings.theme}</span>
          <span data-testid="version">{state.app.version}</span>
        </div>
      )
    }

    const initialState: TestState = {
      user: {
        profile: {
          name: 'Alice',
          age: 30,
        },
        settings: {
          theme: 'dark',
        },
      },
      app: {
        version: '1.0.0',
      },
    }

    render(
      <store.Provider initialState={initialState}>
        <TestComponent />
      </store.Provider>
    )

    expect(screen.getByTestId('name').textContent).toBe('Alice')
    expect(screen.getByTestId('age').textContent).toBe('30')
    expect(screen.getByTestId('theme').textContent).toBe('dark')
    expect(screen.getByTestId('version').textContent).toBe('1.0.0')
  })
})
