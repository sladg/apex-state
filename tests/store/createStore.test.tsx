/**
 * Tests for createGenericStore
 *
 * Verifies basic store creation, Provider component, and valtio proxy setup
 */

import { describe, it, expect } from 'vitest'
import { render, screen } from '@testing-library/react'
import { createGenericStore } from '../../src/store/createStore'
import type { GenericMeta } from '../../src/types'

describe('createGenericStore', () => {
  it('should create a store with Provider', () => {
    type TestState = { count: number }
    const store = createGenericStore<TestState>()

    expect(store).toBeDefined()
    expect(store.Provider).toBeDefined()
    expect(typeof store.Provider).toBe('function')
  })

  it('should render Provider without errors', () => {
    type TestState = { value: string }
    const store = createGenericStore<TestState>()

    const { container } = render(
      <store.Provider initialState={{ value: 'test' }}>
        <div>Child Component</div>
      </store.Provider>
    )

    expect(container).toBeTruthy()
    expect(screen.getByText('Child Component')).toBeTruthy()
  })

  it('should accept errorStorePath prop', () => {
    type TestState = { data: string }
    const store = createGenericStore<TestState>()

    const { container } = render(
      <store.Provider initialState={{ data: 'test' }} errorStorePath="customErrors">
        <div>Test</div>
      </store.Provider>
    )

    expect(container).toBeTruthy()
  })

  it('should support multiple independent Provider instances', () => {
    type TestState = { id: string }
    const store1 = createGenericStore<TestState>()
    const store2 = createGenericStore<TestState>()

    render(
      <>
        <store1.Provider initialState={{ id: 'store1' }}>
          <div>Store 1</div>
        </store1.Provider>
        <store2.Provider initialState={{ id: 'store2' }}>
          <div>Store 2</div>
        </store2.Provider>
      </>
    )

    expect(screen.getByText('Store 1')).toBeTruthy()
    expect(screen.getByText('Store 2')).toBeTruthy()
  })

  it('should work with nested state objects', () => {
    type TestState = {
      user: {
        name: string
        address: {
          city: string
        }
      }
    }
    const store = createGenericStore<TestState>()

    const initialState: TestState = {
      user: {
        name: 'Alice',
        address: {
          city: 'NYC',
        },
      },
    }

    const { container } = render(
      <store.Provider initialState={initialState}>
        <div>Nested State</div>
      </store.Provider>
    )

    expect(container).toBeTruthy()
  })

  it('should handle empty initial state', () => {
    type TestState = Record<string, never>
    const store = createGenericStore<TestState>()

    const { container } = render(
      <store.Provider initialState={{}}>
        <div>Empty</div>
      </store.Provider>
    )

    expect(container).toBeTruthy()
  })

  it('should support custom meta type', () => {
    type TestState = { value: number }
    interface CustomMeta extends GenericMeta {
      timestamp: number
      userId: string
    }

    const store = createGenericStore<TestState, CustomMeta>()

    const { container } = render(
      <store.Provider initialState={{ value: 42 }}>
        <div>Custom Meta</div>
      </store.Provider>
    )

    expect(container).toBeTruthy()
  })
})
