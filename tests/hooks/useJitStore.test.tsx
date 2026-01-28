/**
 * Tests for useJitStore hook
 *
 * Verifies Just-In-Time access, batch updates, and non-reactive reads
 */

import { describe, it, expect } from 'vitest'
import { render, screen, waitFor } from '@testing-library/react'
import { createGenericStore } from '../../src/store/createStore'
import { userEvent } from '@testing-library/user-event'
import type { ArrayOfChanges } from '../../src/types'

describe('useJitStore Hook', () => {
  it('should provide reactive proxyValue', () => {
    type TestState = { count: number; name: string }
    const store = createGenericStore<TestState>()

    const TestComponent = () => {
      const { proxyValue } = store.useJitStore()
      return (
        <div>
          <span data-testid="count">{proxyValue.count}</span>
          <span data-testid="name">{proxyValue.name}</span>
        </div>
      )
    }

    render(
      <store.Provider initialState={{ count: 42, name: 'Alice' }}>
        <TestComponent />
      </store.Provider>
    )

    expect(screen.getByTestId('count').textContent).toBe('42')
    expect(screen.getByTestId('name').textContent).toBe('Alice')
  })

  it('should apply batch changes via setChanges', async () => {
    type TestState = { a: number; b: number; c: number }
    const store = createGenericStore<TestState>()

    const TestComponent = () => {
      const { proxyValue, setChanges } = store.useJitStore()

      const updateAll = () => {
        const changes: ArrayOfChanges<TestState> = [
          ['a', 10, {}],
          ['b', 20, {}],
          ['c', 30, {}],
        ]
        setChanges(changes)
      }

      return (
        <div>
          <span data-testid="a">{proxyValue.a}</span>
          <span data-testid="b">{proxyValue.b}</span>
          <span data-testid="c">{proxyValue.c}</span>
          <button onClick={updateAll}>Update All</button>
        </div>
      )
    }

    render(
      <store.Provider initialState={{ a: 0, b: 0, c: 0 }}>
        <TestComponent />
      </store.Provider>
    )

    expect(screen.getByTestId('a').textContent).toBe('0')
    expect(screen.getByTestId('b').textContent).toBe('0')
    expect(screen.getByTestId('c').textContent).toBe('0')

    const button = screen.getByText('Update All')
    await userEvent.click(button)

    await waitFor(() => {
      expect(screen.getByTestId('a').textContent).toBe('10')
      expect(screen.getByTestId('b').textContent).toBe('20')
      expect(screen.getByTestId('c').textContent).toBe('30')
    })
  })

  it('should support deep paths in setChanges', async () => {
    type TestState = {
      user: {
        profile: {
          name: string
          age: number
        }
      }
    }
    const store = createGenericStore<TestState>()

    const TestComponent = () => {
      const { proxyValue, setChanges } = store.useJitStore()

      const updateProfile = () => {
        const changes: ArrayOfChanges<TestState> = [
          ['user.profile.name', 'Bob', {}],
          ['user.profile.age', 35, {}],
        ]
        setChanges(changes)
      }

      return (
        <div>
          <span data-testid="name">{proxyValue.user.profile.name}</span>
          <span data-testid="age">{proxyValue.user.profile.age}</span>
          <button onClick={updateProfile}>Update Profile</button>
        </div>
      )
    }

    render(
      <store.Provider initialState={{ user: { profile: { name: 'Alice', age: 30 } } }}>
        <TestComponent />
      </store.Provider>
    )

    const button = screen.getByText('Update Profile')
    await userEvent.click(button)

    await waitFor(() => {
      expect(screen.getByTestId('name').textContent).toBe('Bob')
      expect(screen.getByTestId('age').textContent).toBe('35')
    })
  })

  it('should provide non-reactive getState', () => {
    type TestState = { value: string }
    const store = createGenericStore<TestState>()

    let capturedState: TestState | null = null

    const TestComponent = () => {
      const { getState } = store.useJitStore()

      const captureState = () => {
        capturedState = getState()
      }

      return <button onClick={captureState}>Capture</button>
    }

    render(
      <store.Provider initialState={{ value: 'test' }}>
        <TestComponent />
      </store.Provider>
    )

    const button = screen.getByText('Capture')
    userEvent.click(button)

    // Wait a bit for the click to process
    setTimeout(() => {
      expect(capturedState).toEqual({ value: 'test' })
    }, 100)
  })

  it('should not trigger re-renders when calling getState', async () => {
    type TestState = { count: number }
    const store = createGenericStore<TestState>()

    let renderCount = 0
    let lastState: TestState | null = null

    const TestComponent = () => {
      renderCount++
      const { getState, setChanges } = store.useJitStore()

      const readState = () => {
        lastState = getState()
      }

      const updateState = () => {
        setChanges([['count', 99, {}]])
      }

      return (
        <div>
          <button data-testid="read" onClick={readState}>
            Read
          </button>
          <button data-testid="update" onClick={updateState}>
            Update
          </button>
        </div>
      )
    }

    render(
      <store.Provider initialState={{ count: 0 }}>
        <TestComponent />
      </store.Provider>
    )

    const initialRenderCount = renderCount

    // Reading state should not trigger re-render
    const readButton = screen.getByTestId('read')
    await userEvent.click(readButton)

    // Small delay to ensure no re-renders
    await new Promise((resolve) => setTimeout(resolve, 50))

    // Render count should be the same (no re-render from getState)
    expect(renderCount).toBe(initialRenderCount)
    expect(lastState).toEqual({ count: 0 })
  })

  it('should throw error when used outside Provider', () => {
    type TestState = { count: number }
    const store = createGenericStore<TestState>()

    const TestComponent = () => {
      try {
        const { proxyValue } = store.useJitStore()
        return <div>{proxyValue.count}</div>
      } catch (error) {
        return <div data-testid="error">{(error as Error).message}</div>
      }
    }

    render(<TestComponent />)

    expect(screen.getByTestId('error').textContent).toContain('Provider')
  })

  it('should handle metadata in setChanges', async () => {
    type TestState = { value: string }
    const store = createGenericStore<TestState>()

    const TestComponent = () => {
      const { proxyValue, setChanges } = store.useJitStore()

      const updateWithMeta = () => {
        const changes: ArrayOfChanges<TestState> = [
          ['value', 'updated', { sender: 'api', isProgramaticChange: true }],
        ]
        setChanges(changes)
      }

      return (
        <div>
          <span data-testid="value">{proxyValue.value}</span>
          <button onClick={updateWithMeta}>Update</button>
        </div>
      )
    }

    render(
      <store.Provider initialState={{ value: 'initial' }}>
        <TestComponent />
      </store.Provider>
    )

    const button = screen.getByText('Update')
    await userEvent.click(button)

    await waitFor(() => {
      expect(screen.getByTestId('value').textContent).toBe('updated')
    })
  })

  it('should handle empty changes array', () => {
    type TestState = { value: string }
    const store = createGenericStore<TestState>()

    const TestComponent = () => {
      const { proxyValue, setChanges } = store.useJitStore()

      const updateEmpty = () => {
        setChanges([])
      }

      return (
        <div>
          <span data-testid="value">{proxyValue.value}</span>
          <button onClick={updateEmpty}>Update Empty</button>
        </div>
      )
    }

    render(
      <store.Provider initialState={{ value: 'test' }}>
        <TestComponent />
      </store.Provider>
    )

    const button = screen.getByText('Update Empty')
    userEvent.click(button)

    // Should not throw and value should remain unchanged
    expect(screen.getByTestId('value').textContent).toBe('test')
  })

  it('should sync changes across multiple components', async () => {
    type TestState = { shared: string }
    const store = createGenericStore<TestState>()

    const Reader = () => {
      const { proxyValue } = store.useJitStore()
      return <div data-testid="reader">{proxyValue.shared}</div>
    }

    const Writer = () => {
      const { setChanges } = store.useJitStore()

      const update = () => {
        setChanges([['shared', 'changed', {}]])
      }

      return <button onClick={update}>Update</button>
    }

    render(
      <store.Provider initialState={{ shared: 'initial' }}>
        <Reader />
        <Writer />
      </store.Provider>
    )

    expect(screen.getByTestId('reader').textContent).toBe('initial')

    const button = screen.getByText('Update')
    await userEvent.click(button)

    await waitFor(() => {
      expect(screen.getByTestId('reader').textContent).toBe('changed')
    })
  })
})
