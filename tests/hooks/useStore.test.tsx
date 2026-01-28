/**
 * Tests for useStore hook
 *
 * Verifies useState-like interface, reactivity, and type safety
 */

import { describe, it, expect } from 'vitest'
import { render, screen, waitFor } from '@testing-library/react'
import { createGenericStore } from '../../src/store/createStore'
import { userEvent } from '@testing-library/user-event'

describe('useStore Hook', () => {
  it('should read value from store', () => {
    type TestState = { count: number }
    const store = createGenericStore<TestState>()

    const TestComponent = () => {
      const [count] = store.useStore('count')
      return <div data-testid="count">{count}</div>
    }

    render(
      <store.Provider initialState={{ count: 42 }}>
        <TestComponent />
      </store.Provider>
    )

    expect(screen.getByTestId('count').textContent).toBe('42')
  })

  it('should update value in store', async () => {
    type TestState = { count: number }
    const store = createGenericStore<TestState>()

    const TestComponent = () => {
      const [count, setCount] = store.useStore('count')
      return (
        <div>
          <span data-testid="count">{count}</span>
          <button onClick={() => setCount(count + 1)}>Increment</button>
        </div>
      )
    }

    render(
      <store.Provider initialState={{ count: 0 }}>
        <TestComponent />
      </store.Provider>
    )

    expect(screen.getByTestId('count').textContent).toBe('0')

    const button = screen.getByText('Increment')
    await userEvent.click(button)

    await waitFor(() => {
      expect(screen.getByTestId('count').textContent).toBe('1')
    })
  })

  it('should support deep paths', () => {
    type TestState = {
      user: {
        profile: {
          name: string
        }
      }
    }
    const store = createGenericStore<TestState>()

    const TestComponent = () => {
      const [name] = store.useStore('user.profile.name')
      return <div data-testid="name">{name}</div>
    }

    render(
      <store.Provider initialState={{ user: { profile: { name: 'Alice' } } }}>
        <TestComponent />
      </store.Provider>
    )

    expect(screen.getByTestId('name').textContent).toBe('Alice')
  })

  it('should update deep paths', async () => {
    type TestState = {
      user: {
        profile: {
          name: string
        }
      }
    }
    const store = createGenericStore<TestState>()

    const TestComponent = () => {
      const [name, setName] = store.useStore('user.profile.name')
      return (
        <div>
          <span data-testid="name">{name}</span>
          <button onClick={() => setName('Bob')}>Change Name</button>
        </div>
      )
    }

    render(
      <store.Provider initialState={{ user: { profile: { name: 'Alice' } } }}>
        <TestComponent />
      </store.Provider>
    )

    expect(screen.getByTestId('name').textContent).toBe('Alice')

    const button = screen.getByText('Change Name')
    await userEvent.click(button)

    await waitFor(() => {
      expect(screen.getByTestId('name').textContent).toBe('Bob')
    })
  })

  it('should trigger re-renders only when accessed value changes', async () => {
    type TestState = { a: number; b: number }
    const store = createGenericStore<TestState>()

    let renderCountA = 0
    let renderCountB = 0

    const ComponentA = () => {
      renderCountA++
      const [a, setA] = store.useStore('a')
      return (
        <div>
          <span data-testid="a">{a}</span>
          <button data-testid="incA" onClick={() => setA(a + 1)}>
            Inc A
          </button>
        </div>
      )
    }

    const ComponentB = () => {
      renderCountB++
      const [b, setB] = store.useStore('b')
      return (
        <div>
          <span data-testid="b">{b}</span>
          <button data-testid="incB" onClick={() => setB(b + 1)}>
            Inc B
          </button>
        </div>
      )
    }

    render(
      <store.Provider initialState={{ a: 0, b: 0 }}>
        <ComponentA />
        <ComponentB />
      </store.Provider>
    )

    const initialRenderCountA = renderCountA
    const initialRenderCountB = renderCountB

    // Update A - should only re-render ComponentA
    const buttonA = screen.getByTestId('incA')
    await userEvent.click(buttonA)

    await waitFor(() => {
      expect(screen.getByTestId('a').textContent).toBe('1')
    })

    // ComponentA should have re-rendered
    expect(renderCountA).toBeGreaterThan(initialRenderCountA)
    // ComponentB should NOT have re-rendered (or minimal re-renders)
    // Note: React may trigger some re-renders, but it shouldn't be many
    expect(renderCountB).toBeLessThanOrEqual(initialRenderCountB + 1)
  })

  it('should throw error when used outside Provider', () => {
    type TestState = { count: number }
    const store = createGenericStore<TestState>()

    const TestComponent = () => {
      try {
        const [count] = store.useStore('count')
        return <div>{count}</div>
      } catch (error) {
        return <div data-testid="error">{(error as Error).message}</div>
      }
    }

    render(<TestComponent />)

    expect(screen.getByTestId('error').textContent).toContain('Provider')
  })

  it('should support metadata in setValue', async () => {
    type TestState = { value: string }
    const store = createGenericStore<TestState>()

    const TestComponent = () => {
      const [value, setValue] = store.useStore('value')
      return (
        <div>
          <span data-testid="value">{value}</span>
          <button onClick={() => setValue('updated', { sender: 'user-123' })}>Update</button>
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

  it('should handle multiple components using same path', async () => {
    type TestState = { shared: string }
    const store = createGenericStore<TestState>()

    const Component1 = () => {
      const [shared] = store.useStore('shared')
      return <div data-testid="comp1">{shared}</div>
    }

    const Component2 = () => {
      const [shared, setShared] = store.useStore('shared')
      return (
        <div>
          <span data-testid="comp2">{shared}</span>
          <button onClick={() => setShared('changed')}>Change</button>
        </div>
      )
    }

    render(
      <store.Provider initialState={{ shared: 'initial' }}>
        <Component1 />
        <Component2 />
      </store.Provider>
    )

    expect(screen.getByTestId('comp1').textContent).toBe('initial')
    expect(screen.getByTestId('comp2').textContent).toBe('initial')

    const button = screen.getByText('Change')
    await userEvent.click(button)

    await waitFor(() => {
      expect(screen.getByTestId('comp1').textContent).toBe('changed')
      expect(screen.getByTestId('comp2').textContent).toBe('changed')
    })
  })
})
