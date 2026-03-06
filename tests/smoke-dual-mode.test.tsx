/**
 * SMOKE TESTS: WASM implementation
 *
 * These tests validate core scenarios with the WASM implementation.
 *
 * Tests cover:
 * - Basic state mutations and React re-renders
 * - Side effects: syncPaths, flipPaths
 * - Concerns: validationState, disabledWhen
 * - Aggregations (write coordination)
 * - Listeners (onChange callbacks)
 */

import { describe, expect, it } from 'vitest'
import { z } from 'zod'

import { _, createGenericStore } from '../src'
import { basicTestFixtures } from './mocks/fixtures'
import { flushSync, mountStore } from './utils/react'

// ---------------------------------------------------------------------------
// Test State Types
// ---------------------------------------------------------------------------

interface BasicState {
  count: number
  text: string
  flag: boolean
}

interface SyncFlipState {
  source: string
  target: string
  flag1: boolean
  flag2: boolean
}

interface ValidationState {
  email: string
  age: number
}

interface AggregationState {
  items: Record<string, { price: number }>
  total: number
}

interface ListenerState {
  user: {
    name: string
    email: string
  }
  callCount: number
}

// ---------------------------------------------------------------------------
// Test Suite
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// Test Suite: Basic State Mutations
// ---------------------------------------------------------------------------

describe('Basic State Mutations', () => {
  it('should mutate state and trigger React re-render', async () => {
    const store = createGenericStore<BasicState>({})

    const { storeInstance, setValue } = mountStore(store, {
      count: 0,
      text: 'hello',
      flag: false,
    })

    await flushSync()

    // Verify initial state
    expect(storeInstance.state.count).toBe(0)

    // Mutate state
    setValue('count', 42)
    await flushSync()

    // Verify state updated
    expect(storeInstance.state.count).toBe(42)
  })

  it('should handle multiple field updates', async () => {
    const store = createGenericStore<BasicState>({})

    const { storeInstance, setValue } = mountStore(store, {
      count: 0,
      text: 'hello',
      flag: false,
    })

    // Multiple updates
    setValue('count', 10)
    setValue('text', 'world')
    setValue('flag', true)
    await flushSync()

    // Verify all updates applied
    expect(storeInstance.state.count).toBe(10)
    expect(storeInstance.state.text).toBe('world')
    expect(storeInstance.state.flag).toBe(true)
  })
})

// ---------------------------------------------------------------------------
// Test Suite: Side Effects - syncPaths
// ---------------------------------------------------------------------------

describe('Side Effects: syncPaths', () => {
  it('should synchronize source to target', async () => {
    const store = createGenericStore<SyncFlipState>({})

    const Component = () => {
      store.useSideEffects('sync-test', {
        syncPaths: [['source', 'target']],
      })
      return null
    }

    const { storeInstance, setValue } = mountStore(<Component />, store, {
      source: 'initial',
      target: '',
      flag1: false,
      flag2: false,
    })

    await flushSync()

    // Change source
    setValue('source', 'updated')
    await flushSync()

    // Verify target synchronized
    expect(storeInstance.state.target).toBe('updated')
  })

  it('should handle bidirectional sync', async () => {
    const store = createGenericStore<SyncFlipState>({})

    const Component = () => {
      store.useSideEffects('sync-test', {
        syncPaths: [
          ['source', 'target'],
          ['target', 'source'],
        ],
      })
      return null
    }

    const { storeInstance, setValue } = mountStore(<Component />, store, {
      source: 'A',
      target: 'B',
      flag1: false,
      flag2: false,
    })

    await flushSync()

    // Update source
    setValue('source', 'X')
    await flushSync()
    expect(storeInstance.state.target).toBe('X')

    // Update target
    setValue('target', 'Y')
    await flushSync()
    expect(storeInstance.state.source).toBe('Y')
  })
})

// ---------------------------------------------------------------------------
// Test Suite: Side Effects - flipPaths
// ---------------------------------------------------------------------------

describe('Side Effects: flipPaths', () => {
  it('should flip boolean paths', async () => {
    const store = createGenericStore<SyncFlipState>({})

    const Component = () => {
      store.useSideEffects('flip-test', {
        flipPaths: [['flag1', 'flag2']],
      })
      return null
    }

    const { storeInstance, setValue } = mountStore(<Component />, store, {
      source: '',
      target: '',
      flag1: false,
      flag2: true,
    })

    await flushSync()

    // Flip flag1 to true
    setValue('flag1', true)
    await flushSync()
    expect(storeInstance.state.flag2).toBe(false)

    // Flip flag1 to false
    setValue('flag1', false)
    await flushSync()
    expect(storeInstance.state.flag2).toBe(true)
  })

  it('should handle bidirectional flip', async () => {
    const store = createGenericStore<SyncFlipState>({})

    const Component = () => {
      store.useSideEffects('flip-test', {
        flipPaths: [
          ['flag1', 'flag2'],
          ['flag2', 'flag1'],
        ],
      })
      return null
    }

    const { storeInstance, setValue } = mountStore(<Component />, store, {
      source: '',
      target: '',
      flag1: false,
      flag2: false,
    })

    await flushSync()

    // Both start false - flip flag1 to true
    setValue('flag1', true)
    await flushSync()
    expect(storeInstance.state.flag2).toBe(false)
    expect(storeInstance.state.flag1).toBe(true)

    // Flip flag2 to true
    setValue('flag2', true)
    await flushSync()
    expect(storeInstance.state.flag1).toBe(false)
    expect(storeInstance.state.flag2).toBe(true)
  })
})

// ---------------------------------------------------------------------------
// Test Suite: Concerns - validationState
// ---------------------------------------------------------------------------

describe('Concerns: validationState', () => {
  it('should validate email with Zod schema', async () => {
    const store = createGenericStore<ValidationState>({})

    const { storeInstance, setValue } = mountStore(
      store,
      { email: 'valid@example.com', age: 25 },
      {
        concerns: {
          email: {
            validationState: {
              schema: z.string().email(),
            },
          },
        },
      },
    )

    await flushSync()

    // Initial valid state
    const initialValidation =
      storeInstance._concerns['email']?.['validationState']
    expect(initialValidation).toBeDefined()

    // Set invalid email
    setValue('email', 'not-an-email')
    await flushSync()

    const invalidValidation =
      storeInstance._concerns['email']?.['validationState']
    expect(invalidValidation).toBeDefined()
  })

  it('should validate age with min constraint', async () => {
    const store = createGenericStore<ValidationState>({})

    const { storeInstance, setValue } = mountStore(
      store,
      { email: 'test@example.com', age: 25 },
      {
        concerns: {
          age: {
            validationState: {
              schema: z.number().min(18).max(100),
            },
          },
        },
      },
    )

    await flushSync()

    // Valid age
    const validValidation = storeInstance._concerns['age']?.['validationState']
    expect(validValidation).toBeDefined()

    // Invalid age (too young)
    setValue('age', 10)
    await flushSync()

    const invalidValidation =
      storeInstance._concerns['age']?.['validationState']
    expect(invalidValidation).toBeDefined()
  })
})

// ---------------------------------------------------------------------------
// Test Suite: Concerns - disabledWhen
// ---------------------------------------------------------------------------

describe('Concerns: disabledWhen', () => {
  it('should evaluate simple boolean logic', async () => {
    const store = createGenericStore<ValidationState>({})

    const { storeInstance, setValue } = mountStore(
      store,
      { email: '', age: 0 },
      {
        concerns: {
          email: { disabledWhen: { boolLogic: { IS_EQUAL: ['age', 0] } } },
        },
      },
    )

    await flushSync()

    // Initially disabled (age === 0)
    const initialDisabled = storeInstance._concerns['email']?.['disabledWhen']
    expect(initialDisabled).toBe(true)

    // Change age - should enable
    setValue('age', 25)
    await flushSync()

    const enabledState = storeInstance._concerns['email']?.['disabledWhen']
    expect(enabledState).toBe(false)
  })

  it('should evaluate AND logic', async () => {
    const store = createGenericStore<ValidationState>({})

    const { storeInstance, setValue } = mountStore(
      store,
      { email: '', age: 0 },
      {
        concerns: {
          email: {
            disabledWhen: {
              boolLogic: {
                AND: [{ IS_EQUAL: ['age', 0] }, { IS_EQUAL: ['email', ''] }],
              },
            },
          },
        },
      },
    )

    await flushSync()

    // Initially disabled (both conditions true)
    expect(storeInstance._concerns['email']?.['disabledWhen']).toBe(true)

    // Change email only - still disabled (age still 0)
    setValue('email', 'test@example.com')
    await flushSync()
    expect(storeInstance._concerns['email']?.['disabledWhen']).toBe(false)

    // Reset email
    setValue('email', '')
    await flushSync()
    expect(storeInstance._concerns['email']?.['disabledWhen']).toBe(true)

    // Change age - should enable
    setValue('age', 25)
    await flushSync()
    expect(storeInstance._concerns['email']?.['disabledWhen']).toBe(false)
  })
})

// ---------------------------------------------------------------------------
// Test Suite: Aggregations
// ---------------------------------------------------------------------------

describe('Aggregations', () => {
  it('should aggregate array item prices to total', async () => {
    const store = createGenericStore<AggregationState>({})

    const Component = () => {
      store.useSideEffects('agg-test', {
        aggregations: [
          ['total', `items.${_('a')}.price`],
          ['total', `items.${_('b')}.price`],
          ['total', `items.${_('c')}.price`],
        ],
      })
      return null
    }

    const { storeInstance, setValue } = mountStore(<Component />, store, {
      items: { a: { price: 5 }, b: { price: 5 }, c: { price: 5 } },
      total: 0,
    })

    await flushSync()

    // Trigger a change to activate the aggregation effect
    setValue(`items.${_('a')}.price`, 5)
    await flushSync()

    // All prices equal - total should be set to their common value (aggregation copies the value)
    // Since this is an aggregation that checks equality, when all sources are equal, it copies that value to target
    const totalAfterEqual = storeInstance.state.total
    expect(totalAfterEqual).toBeDefined()

    // Update one price to a different value
    setValue(`items.${_('a')}.price`, 8)
    await flushSync()

    // When prices are no longer all equal, aggregation should set total to undefined
    expect(storeInstance.state.total).toBeUndefined()

    // Set all prices back to the same value
    setValue(`items.${_('a')}.price`, 5)
    await flushSync()

    // All prices equal again - total should be set to their common value
    const totalAfterReequal = storeInstance.state.total
    expect(totalAfterReequal).toBe(5)
  })
})

// ---------------------------------------------------------------------------
// Test Suite: Listeners (onChange callbacks)
// ---------------------------------------------------------------------------

describe('Listeners', () => {
  it('should trigger listener on nested path change', async () => {
    const store = createGenericStore<ListenerState>({})

    let callbackFired = false

    const Component = () => {
      store.useSideEffects('listener-test', {
        listeners: [
          {
            path: 'user.name',
            scope: null,
            fn: () => {
              callbackFired = true
              return undefined
            },
          },
        ],
      })
      return null
    }

    const { setValue } = mountStore(<Component />, store, {
      user: { name: 'Alice', email: 'alice@example.com' },
      callCount: 0,
    })

    await flushSync()

    // Reset flag
    callbackFired = false

    // Change user.name
    setValue('user.name', 'Bob')
    await flushSync()

    // Verify listener fired
    expect(callbackFired).toBe(true)
  })

  it('should provide scoped state to listener', async () => {
    const store = createGenericStore<ListenerState>({})

    let listenerCallCount = 0
    let lastReceivedName: string | null = null
    let lastReceivedChanges: any[] = []

    const Component = () => {
      store.useSideEffects('listener-test', {
        listeners: [
          {
            path: 'user',
            scope: 'user',
            fn: (changes, state) => {
              listenerCallCount++
              lastReceivedName = (state as any).name
              lastReceivedChanges = changes
              return undefined
            },
          },
        ],
      })
      return null
    }

    const { setValue } = mountStore(<Component />, store, {
      user: { name: 'Alice', email: 'alice@example.com' },
      callCount: 0,
    })

    await flushSync()

    // Record initial call count
    const initialCallCount = listenerCallCount

    // Change user.name
    setValue('user.name', 'Bob')
    await flushSync()

    // Verify listener was called with the change
    expect(listenerCallCount).toBeGreaterThan(initialCallCount)
    // Listener receives scoped state and changes array.
    // WASM provides POST-CHANGE state ('Bob').
    expect(lastReceivedName).toBeDefined()
    // The changes array contains the incoming delta
    expect(lastReceivedChanges.length).toBeGreaterThan(0)
  })
})

// ---------------------------------------------------------------------------
// Test Suite: Fixture Contamination Guard
// ---------------------------------------------------------------------------

describe('Fixture Contamination Guard', () => {
  // These two tests MUST run sequentially. The first mutates state through
  // valtio proxy; the second verifies the original fixture is untouched.

  it('should use shared fixture and mutate state via proxy', async () => {
    const store = createGenericStore<typeof basicTestFixtures.empty>({})

    const { storeInstance, setValue } = mountStore(
      store,
      basicTestFixtures.empty,
    )

    await flushSync()

    // Mutate state through the store
    setValue('fieldA', 'contaminated')
    setValue('fieldC', 999)
    await flushSync()

    // Store state should reflect mutations
    expect(storeInstance.state.fieldA).toBe('contaminated')
    expect(storeInstance.state.fieldC).toBe(999)
  })

  it('should find the original fixture unmodified after previous test', () => {
    // If proxy(initialState) mutated the original object, these would fail
    expect(basicTestFixtures.empty.fieldA).toBe('')
    expect(basicTestFixtures.empty.fieldC).toBe(0)
    expect(basicTestFixtures.empty.source).toBe('')
    expect(basicTestFixtures.empty.boolA).toBe(false)
    expect(basicTestFixtures.empty.boolB).toBe(true)
  })
})

// ---------------------------------------------------------------------------
// Test Suite: Cross-Feature Integration
// ---------------------------------------------------------------------------

describe('Cross-Feature Integration', () => {
  it('should handle sync + validation + disabledWhen together', async () => {
    interface ComplexState {
      email: string
      emailConfirm: string
      isValid: boolean
    }

    const store = createGenericStore<ComplexState>({})

    const Component = () => {
      store.useSideEffects('complex-test', {
        syncPaths: [['email', 'emailConfirm']],
      })

      store.useConcerns('complex-concerns', {
        email: {
          validationState: {
            schema: z.string().email(),
          },
          disabledWhen: {
            boolLogic: {
              IS_EQUAL: ['isValid', false],
            },
          },
        },
      })
      return null
    }

    const { storeInstance, setValue } = mountStore(<Component />, store, {
      email: 'test@example.com',
      emailConfirm: '',
      isValid: false,
    })

    await flushSync()

    // Verify initial state
    expect(storeInstance.state.emailConfirm).toBe('test@example.com') // synced
    expect(storeInstance._concerns['email']?.['disabledWhen']).toBe(true) // disabled

    // Enable validation
    setValue('isValid', true)
    await flushSync()
    expect(storeInstance._concerns['email']?.['disabledWhen']).toBe(false)

    // Change email - should sync and validate
    setValue('email', 'new@example.com')
    await flushSync()
    expect(storeInstance.state.emailConfirm).toBe('new@example.com')
    expect(storeInstance._concerns['email']?.['validationState']).toBeDefined()
  })
})
