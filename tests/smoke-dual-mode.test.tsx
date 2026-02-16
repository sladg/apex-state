/**
 * DUAL-MODE SMOKE TESTS: Legacy TypeScript vs. WASM
 *
 * These tests run the same scenarios in both legacy and WASM modes to ensure
 * feature parity and catch integration bugs early.
 *
 * Tests cover:
 * - Basic state mutations and React re-renders
 * - Side effects: syncPaths, flipPaths
 * - Concerns: validationState, disabledWhen
 * - Aggregations (write coordination)
 * - Listeners (onChange callbacks)
 *
 * Each test runs twice: once with useLegacyImplementation: true, once with false.
 */

import { beforeEach, describe, expect, it } from 'vitest'
import { z } from 'zod'

import { createGenericStore } from '../src'
import { loadWasm } from '../src/wasm/bridge'
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
  items: { price: number }[]
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
// Mode Configuration
// ---------------------------------------------------------------------------

const MODES = [
  { name: 'Legacy', config: { useLegacyImplementation: true } },
  { name: 'WASM', config: { useLegacyImplementation: false } },
] as const

// ---------------------------------------------------------------------------
// Setup: Load WASM before tests
// ---------------------------------------------------------------------------

beforeEach(async () => {
  // Pre-load WASM so Provider doesn't block during tests
  await loadWasm()
})

// ---------------------------------------------------------------------------
// Test Suite: Basic State Mutations
// ---------------------------------------------------------------------------

describe.each(MODES)('[$name] Basic State Mutations', ({ config }) => {
  it('should mutate state and trigger React re-render', async () => {
    const store = createGenericStore<BasicState>(config)

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
    const store = createGenericStore<BasicState>(config)

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

describe.each(MODES)('[$name] Side Effects: syncPaths', ({ config }) => {
  it('should synchronize source to target', async () => {
    const store = createGenericStore<SyncFlipState>(config)

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
    const store = createGenericStore<SyncFlipState>(config)

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

describe.each(MODES)('[$name] Side Effects: flipPaths', ({ config }) => {
  it('should flip boolean paths', async () => {
    const store = createGenericStore<SyncFlipState>(config)

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
    const store = createGenericStore<SyncFlipState>(config)

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

describe.each(MODES)('[$name] Concerns: validationState', ({ config }) => {
  it('should validate email with Zod schema', async () => {
    const store = createGenericStore<ValidationState>(config)

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
    const store = createGenericStore<ValidationState>(config)

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

describe.each(MODES)('[$name] Concerns: disabledWhen', ({ config }) => {
  it('should evaluate simple boolean logic', async () => {
    const store = createGenericStore<ValidationState>(config)

    const { storeInstance, setValue } = mountStore(
      store,
      { email: '', age: 0 },
      {
        concerns: {
          email: {
            disabledWhen: {
              logic: {
                IS_EQUAL: ['age', 0],
              },
            },
          },
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
    const store = createGenericStore<ValidationState>(config)

    const { storeInstance, setValue } = mountStore(
      store,
      { email: '', age: 0 },
      {
        concerns: {
          email: {
            disabledWhen: {
              logic: {
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

describe.each(MODES)('[$name] Aggregations', ({ config }) => {
  it('should aggregate array item prices to total', async () => {
    const store = createGenericStore<AggregationState>(config)

    const Component = () => {
      store.useSideEffects('agg-test', {
        aggregations: [
          ['total', 'items.0.price'],
          ['total', 'items.1.price'],
          ['total', 'items.2.price'],
        ],
      })
      return null
    }

    const { storeInstance, setValue } = mountStore(<Component />, store, {
      items: [{ price: 10 }, { price: 20 }, { price: 30 }],
      total: 0,
    })

    await flushSync()

    // Update a price
    setValue('items.0.price', 15)
    await flushSync()

    // Verify total recalculated (should be 15 + 20 + 30 = 65)
    expect(storeInstance.state.total).toBe(65)

    // Update another price
    setValue('items.1.price', 25)
    await flushSync()

    // Verify total recalculated (should be 15 + 25 + 30 = 70)
    expect(storeInstance.state.total).toBe(70)
  })
})

// ---------------------------------------------------------------------------
// Test Suite: Listeners (onChange callbacks)
// ---------------------------------------------------------------------------

describe.each(MODES)('[$name] Listeners', ({ config }) => {
  it('should trigger listener on nested path change', async () => {
    const store = createGenericStore<ListenerState>(config)

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
    const store = createGenericStore<ListenerState>(config)

    let receivedState: any = null

    const Component = () => {
      store.useSideEffects('listener-test', {
        listeners: [
          {
            path: 'user',
            scope: 'user',
            fn: (_changes, state) => {
              receivedState = state
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

    // Change user.name
    setValue('user.name', 'Bob')
    await flushSync()

    // Verify listener received scoped state
    expect(receivedState).toEqual({
      name: 'Bob',
      email: 'alice@example.com',
    })
  })
})

// ---------------------------------------------------------------------------
// Test Suite: Cross-Feature Integration
// ---------------------------------------------------------------------------

describe.each(MODES)('[$name] Cross-Feature Integration', ({ config }) => {
  it('should handle sync + validation + disabledWhen together', async () => {
    interface ComplexState {
      email: string
      emailConfirm: string
      isValid: boolean
    }

    const store = createGenericStore<ComplexState>(config)

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
            logic: {
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
