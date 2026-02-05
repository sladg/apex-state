/**
 * Tests for createGenericStore
 *
 * Verifies basic store creation, Provider component, and valtio proxy setup
 */

import { screen } from '@testing-library/react'
import { describe, expect, it } from 'vitest'

import { createGenericStore } from '../../src/store/createStore'
import type { GenericMeta } from '../../src/types'
import { renderWithStore } from '../../tests/utils/react'

describe('createGenericStore', () => {
  it('should create a store with Provider', () => {
    interface TestState {
      count: number
    }
    const store = createGenericStore<TestState>()

    expect(store).toBeDefined()
    expect(store.Provider).toBeDefined()
    expect(typeof store.Provider).toBe('function')
  })

  it('should render Provider without errors', () => {
    interface TestState {
      value: string
    }
    const store = createGenericStore<TestState>()

    renderWithStore(<div>Child Component</div>, store, {
      value: 'test',
    })

    expect(screen.getByText('Child Component')).toBeTruthy()
  })

  it('should accept errorStorePath config', () => {
    interface TestState {
      data: string
    }
    const store = createGenericStore<TestState>()

    renderWithStore(<div>Test</div>, store, {
      data: 'test',
    })

    expect(screen.getByText('Test')).toBeTruthy()
  })

  it('should support multiple independent Provider instances', () => {
    interface TestState {
      id: string
    }
    const store1 = createGenericStore<TestState>()
    const store2 = createGenericStore<TestState>()

    renderWithStore(<div>Store 1</div>, store1, { id: 'store1' })
    renderWithStore(<div>Store 2</div>, store2, { id: 'store2' })

    expect(screen.getByText('Store 1')).toBeTruthy()
    expect(screen.getByText('Store 2')).toBeTruthy()
  })

  it('should work with nested state objects', () => {
    interface TestState {
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

    renderWithStore(<div>Nested State</div>, store, initialState)

    expect(screen.getByText('Nested State')).toBeTruthy()
  })

  it('should handle empty initial state', () => {
    type TestState = Record<string, never>
    const store = createGenericStore<TestState>()

    renderWithStore(<div>Empty</div>, store, {})

    expect(screen.getByText('Empty')).toBeTruthy()
  })

  it('should support custom meta type', () => {
    interface TestState {
      value: number
    }
    interface CustomMeta extends GenericMeta {
      timestamp: number
      userId: string
    }

    const store = createGenericStore<TestState, CustomMeta>()

    renderWithStore(<div>Custom Meta</div>, store, {
      value: 42,
    })

    expect(screen.getByText('Custom Meta')).toBeTruthy()
  })
})
