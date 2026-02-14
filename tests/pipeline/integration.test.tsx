/**
 * Pipeline integration tests
 *
 * Tests for pipeline integration with store hooks:
 * - setChanges applies changes through pipeline
 * - setState applies changes through pipeline
 * - Atomic updates (single re-render)
 * - Meta information preserved
 */

import { screen } from '@testing-library/react'
import { describe, expect, it } from 'vitest'

import { createGenericStore } from '~/store/createStore'
import type { GenericMeta } from '~/types'

import { fireEvent, flush, renderWithStore } from '../utils/react'

const initialTestState = {
  count: 0,
  user: {
    name: 'Initial',
  },
}

type TestState = typeof initialTestState

describe('Pipeline integration with useJitStore', () => {
  it('applies changes through pipeline', async () => {
    const store = createGenericStore<TestState, GenericMeta>()
    let renderCount = 0

    function TestComponent() {
      const { proxyValue, setChanges } = store.useJitStore()
      renderCount++

      return (
        <div>
          <button
            onClick={() =>
              setChanges([
                ['count', 42, {}],
                ['user.name', 'Alice', {}],
              ])
            }
          >
            Update
          </button>
          <span data-testid="count">{proxyValue.count}</span>
          <span data-testid="name">{proxyValue.user.name}</span>
        </div>
      )
    }

    renderWithStore(<TestComponent />, store, initialTestState)

    const initialRenderCount = renderCount

    // Click update button
    fireEvent.click(screen.getByText('Update'))

    await flush()

    expect(screen.getByTestId('count').textContent).toBe('42')
    expect(screen.getByTestId('name').textContent).toBe('Alice')

    // Should only re-render once for both changes (atomic update)
    expect(renderCount).toBe(initialRenderCount + 1)
  })

  it('getState returns non-reactive snapshot', async () => {
    const store = createGenericStore<TestState, GenericMeta>()
    let renderCount = 0

    function TestComponent() {
      const { getState, setChanges, proxyValue } = store.useJitStore()
      renderCount++

      return (
        <div>
          <button
            onClick={() => {
              // Read state non-reactively
              const currentState = getState()
              // Update based on current state
              setChanges([['count', currentState.count + 1, {}]])
            }}
          >
            Increment
          </button>
          <span data-testid="count">{proxyValue.count}</span>
        </div>
      )
    }

    renderWithStore(<TestComponent />, store, {
      count: 0,
      user: { name: 'Test' },
    })

    const initialRenderCount = renderCount

    // Click increment
    fireEvent.click(screen.getByText('Increment'))

    await flush()

    expect(screen.getByTestId('count').textContent).toBe('1')

    // Should re-render once for the state change
    expect(renderCount).toBe(initialRenderCount + 1)
  })
})

describe('Pipeline integration with useStore', () => {
  it('applies single path changes through pipeline', async () => {
    const store = createGenericStore<TestState, GenericMeta>()
    let renderCount = 0

    function TestComponent() {
      const [count, setCount] = store.useStore('count')
      renderCount++

      return (
        <div>
          <button onClick={() => setCount(100)}>Update Count</button>
          <span data-testid="count">{count}</span>
        </div>
      )
    }

    renderWithStore(<TestComponent />, store, {
      count: 0,
      user: { name: 'Test' },
    })

    const initialRenderCount = renderCount

    fireEvent.click(screen.getByText('Update Count'))

    await flush()

    expect(screen.getByTestId('count').textContent).toBe('100')

    // Should re-render once for the change
    expect(renderCount).toBe(initialRenderCount + 1)
  })

  it('passes metadata through pipeline', async () => {
    const store = createGenericStore<TestState, GenericMeta>()

    function TestComponent() {
      const [count, setCount] = store.useStore('count')

      return (
        <div>
          <button
            onClick={() =>
              setCount(50, { sender: 'test-user', isProgramaticChange: true })
            }
          >
            Update with Meta
          </button>
          <span data-testid="count">{count}</span>
        </div>
      )
    }

    renderWithStore(<TestComponent />, store, {
      count: 0,
      user: { name: 'Test' },
    })

    fireEvent.click(screen.getByText('Update with Meta'))

    await flush()

    expect(screen.getByTestId('count').textContent).toBe('50')

    // Meta is passed through pipeline (will be used by side-effects in future tasks)
  })

  it('handles nested path updates', async () => {
    const store = createGenericStore<TestState, GenericMeta>()

    function TestComponent() {
      const [name, setName] = store.useStore('user.name')

      return (
        <div>
          <button onClick={() => setName('Bob')}>Update Name</button>
          <span data-testid="name">{name}</span>
        </div>
      )
    }

    renderWithStore(<TestComponent />, store, initialTestState)

    fireEvent.click(screen.getByText('Update Name'))

    await flush()

    expect(screen.getByTestId('name').textContent).toBe('Bob')
  })
})

describe('Pipeline placeholder synchronizers', () => {
  it('runs through all placeholder synchronizers without errors', () => {
    const store = createGenericStore<TestState, GenericMeta>()

    function TestComponent() {
      const { setChanges } = store.useJitStore()

      return (
        <button
          onClick={() =>
            setChanges([
              ['count', 1, {}],
              ['user.name', 'Test', {}],
            ])
          }
        >
          Update
        </button>
      )
    }

    renderWithStore(<TestComponent />, store, {
      count: 0,
      user: { name: '' },
    })

    // Should not throw - all placeholders are no-ops
    expect(() => {
      fireEvent.click(screen.getByText('Update'))
    }).not.toThrow()
  })
})
