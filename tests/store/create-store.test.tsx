/**
 * Tests for createGenericStore
 *
 * Verifies basic store creation, Provider component, and valtio proxy setup.
 * Type-level tests live in create-store.test-d.tsx.
 */

import { screen } from '@testing-library/react'
import { describe, expect, it } from 'vitest'

import { createGenericStore } from '~/store/create-store'
import type { GenericMeta } from '~/types'

import {
  deepGetterFixtures,
  deeplyNestedFixtures,
  listenerTestFixtures,
} from '../mocks/fixtures'
import type { DeeplyNestedState, ListenerTestState } from '../mocks/types'
import { mountStore } from '../utils/react'

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

    mountStore(<div>Child Component</div>, store, {
      value: 'test',
    })

    expect(screen.getByText('Child Component')).toBeTruthy()
  })

  it('should accept errorStorePath config', () => {
    interface TestState {
      data: string
    }
    const store = createGenericStore<TestState>()

    mountStore(<div>Test</div>, store, {
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

    mountStore(<div>Store 1</div>, store1, { id: 'store1' })
    mountStore(<div>Store 2</div>, store2, { id: 'store2' })

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

    mountStore(<div>Nested State</div>, store, initialState)

    expect(screen.getByText('Nested State')).toBeTruthy()
  })

  it('should handle empty initial state', () => {
    type TestState = Record<string, never>
    const store = createGenericStore<TestState>()

    mountStore(<div>Empty</div>, store, {})

    expect(screen.getByText('Empty')).toBeTruthy()
  })

  it('should not mutate the original initialState object', () => {
    const store = createGenericStore<DeeplyNestedState>()
    const originalState = deeplyNestedFixtures.initial

    const snapshotBefore = JSON.stringify(originalState)

    mountStore(store, originalState)

    // Original object must be untouched after Provider consumed it
    expect(JSON.stringify(originalState)).toBe(snapshotBefore)
    expect(originalState.level1.value).toBe('L1')
    expect(originalState.level1.level2.level3.level4.level5.value).toBe('L5')
  })

  it('should not share references between initialState and store proxy', () => {
    const store = createGenericStore<ListenerTestState>()
    const originalState = listenerTestFixtures.initial

    const { storeInstance } = mountStore(store, originalState)

    // Mutate store proxy — original must not be affected
    storeInstance.state.user.name = 'Bob'
    storeInstance.state.callCount = 99

    expect(originalState.user.name).toBe('Alice')
    expect(originalState.callCount).toBe(0)
  })

  it('should not mutate initialState with getters', () => {
    const store = createGenericStore<typeof deepGetterFixtures.standard>()
    const originalState = deepGetterFixtures.standard

    mountStore(store, originalState)

    // Original getters must still work correctly
    expect(originalState.a).toBe(1)
    expect(originalState.summary).toBe('root:1')
    expect(originalState.l1.b).toBe(2)
    expect(originalState.l1.doubled).toBe(4)
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

    mountStore(<div>Custom Meta</div>, store, {
      value: 42,
    })

    expect(screen.getByText('Custom Meta')).toBeTruthy()
  })
})
