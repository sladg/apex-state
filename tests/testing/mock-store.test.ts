/**
 * Tests for @sladg/apex-state/testing mock module.
 *
 * Verifies the mock createGenericStore and __mocked control API
 * work correctly. Hooks use useSnapshot internally so they are
 * proper React hooks — tested via renderHook / render.
 */

import React from 'react'

import { render, renderHook, screen } from '@testing-library/react'
import { beforeEach, describe, expect, it } from 'vitest'

import { __mocked, createGenericStore } from '~/testing/index'

interface TestState {
  email: string
  name: string
  color: string
  val: number
  count: number
  field: string
  a: number
  b: number
  x: number
  y: number
}

// ---------------------------------------------------------------------------
// Reset between tests
// ---------------------------------------------------------------------------
beforeEach(() => {
  __mocked.reset()
})

// ---------------------------------------------------------------------------
// __mocked control API
// ---------------------------------------------------------------------------
describe('__mocked', () => {
  it('starts with empty state', () => {
    expect(__mocked.state.value).toEqual({})
    expect(__mocked.state.calls).toEqual([])
    expect(__mocked.state.effects).toEqual([])
  })

  it('set<T>() returns typed chainable', () => {
    __mocked.set<TestState>().set('email', 'a@b.com').set('name', 'Alice')

    expect(__mocked.state.value['email']).toBe('a@b.com')
    expect(__mocked.state.value['name']).toBe('Alice')
  })

  it('set<T>(data) seeds state and returns typed chainable', () => {
    __mocked.set<TestState>({ email: 'seed@test.com' }).set('name', 'Bob')

    expect(__mocked.state.value['email']).toBe('seed@test.com')
    expect(__mocked.state.value['name']).toBe('Bob')
  })

  it('set() updates nested paths via chained set', () => {
    __mocked
      .set<{ deep: { nested: { value: number } } }>()
      .set('deep.nested.value', 42)
    expect(__mocked.state.value).toEqual({ deep: { nested: { value: 42 } } })
  })

  it('getState() returns immutable snapshot', () => {
    __mocked.set<TestState>().set('count', 1)
    const snap = __mocked.getState()
    expect(snap).toEqual({ count: 1 })

    // Mutating proxy doesn't affect snapshot
    __mocked.set<TestState>().set('count', 2)
    expect(snap).toEqual({ count: 1 })
  })

  it('reset() clears value, calls, and effects', () => {
    __mocked.set<TestState>().set('x', 1)
    __mocked.state.calls.push({ path: 'x', value: 1 })
    __mocked.state.effects.push({ id: 'e', type: 'concerns', registration: {} })

    __mocked.reset()

    expect(__mocked.state.value).toEqual({})
    expect(__mocked.state.calls).toEqual([])
    expect(__mocked.state.effects).toEqual([])
  })

  it('flush() processes pending React updates', async () => {
    __mocked.set<TestState>().set('x', 1)
    await __mocked.flush()
    expect(__mocked.state.value['x']).toBe(1)
  })

  it('typed chainable also exposes state, getState, flush, reset', async () => {
    const typed = __mocked.set<TestState>({ count: 42 })

    expect(typed.state.value['count']).toBe(42)
    expect(typed.getState()).toEqual({ count: 42 })
    expect(typeof typed.flush).toBe('function')
    expect(typeof typed.reset).toBe('function')
  })
})

// ---------------------------------------------------------------------------
// __mocked — deep clone (never mutate original data)
// ---------------------------------------------------------------------------
describe('__mocked deep clone', () => {
  it('set(data) does not mutate the original seed object', () => {
    const seed = { email: 'orig@test.com', name: 'Alice' }
    __mocked.set<TestState>(seed).set('email', 'changed@test.com')

    expect(__mocked.state.value['email']).toBe('changed@test.com')
    expect(seed.email).toBe('orig@test.com')
  })

  it('set(data) does not share nested object references', () => {
    const nested = { user: { profile: { bio: 'hello' } } }
    __mocked.set(nested)

    // Mutate mock state — original must be untouched
    const stateUser = __mocked.state.value['user'] as Record<string, unknown>
    const stateProfile = stateUser['profile'] as Record<string, unknown>
    stateProfile['bio'] = 'mutated'

    expect(nested.user.profile.bio).toBe('hello')
  })

  it('chainable .set(path, value) does not share object references', () => {
    const address = { street: '123 Main', city: 'Springfield' }
    __mocked.set<{ address: typeof address }>().set('address', address)

    // Mutate mock state — original must be untouched
    const stateAddr = __mocked.state.value['address'] as Record<string, unknown>
    stateAddr['city'] = 'Shelbyville'

    expect(address.city).toBe('Springfield')
  })

  it('Provider initialState does not mutate the original object', () => {
    const initial = { email: 'provider@test.com', name: 'Bob' }
    const store = createGenericStore<TestState>()

    render(
      React.createElement(store.Provider, {
        initialState: initial as unknown as TestState,
        children: null,
      }),
    )

    // Mutate mock state — original must be untouched
    __mocked.set<TestState>().set('email', 'changed@test.com')

    expect(initial.email).toBe('provider@test.com')
  })
})

// ---------------------------------------------------------------------------
// createGenericStore — shape and hooks
// ---------------------------------------------------------------------------
describe('createGenericStore', () => {
  it('returns all expected methods', () => {
    const store = createGenericStore<TestState>()
    expect(store).toHaveProperty('Provider')
    expect(store).toHaveProperty('useStore')
    expect(store).toHaveProperty('useFieldStore')
    expect(store).toHaveProperty('useJitStore')
    expect(store).toHaveProperty('useSideEffects')
    expect(store).toHaveProperty('useConcerns')
    expect(store).toHaveProperty('withConcerns')
  })
})

// ---------------------------------------------------------------------------
// useStore — React hook via renderHook
// ---------------------------------------------------------------------------
describe('useStore', () => {
  it('reads current value from mock state', () => {
    __mocked.set<TestState>().set('email', 'alice@test.com')
    const store = createGenericStore<TestState>()
    const hook = renderHook(() => store.useStore('email'))
    expect(hook.result.current[0]).toBe('alice@test.com')
  })

  it('reads undefined for unset paths', () => {
    const store = createGenericStore<TestState>()
    const hook = renderHook(() => store.useStore('name'))
    expect(hook.result.current[0]).toBeUndefined()
  })

  it('setValue updates state and tracks call', () => {
    const store = createGenericStore<TestState>()
    const hook = renderHook(() => store.useStore('name'))

    hook.result.current[1]('Bob')

    expect(__mocked.state.value['name']).toBe('Bob')
    expect(__mocked.state.calls).toEqual([
      { path: 'name', value: 'Bob', meta: undefined },
    ])
  })

  it('setValue tracks meta when provided', () => {
    const store = createGenericStore<TestState>()
    const hook = renderHook(() => store.useStore('field'))

    hook.result.current[1]('val')

    expect(__mocked.state.calls).toEqual([
      { path: 'field', value: 'val', meta: undefined },
    ])
  })

  it('multiple setValue calls accumulate in calls log', () => {
    const store = createGenericStore<TestState>()
    const hookA = renderHook(() => store.useStore('a'))
    const hookB = renderHook(() => store.useStore('b'))

    hookA.result.current[1](1)
    hookB.result.current[1](2)
    hookA.result.current[1](3)

    expect(__mocked.state.calls).toHaveLength(3)
    expect(__mocked.state.calls[0]).toEqual({
      path: 'a',
      value: 1,
      meta: undefined,
    })
    expect(__mocked.state.calls[1]).toEqual({
      path: 'b',
      value: 2,
      meta: undefined,
    })
    expect(__mocked.state.calls[2]).toEqual({
      path: 'a',
      value: 3,
      meta: undefined,
    })
  })
})

// ---------------------------------------------------------------------------
// useFieldStore
// ---------------------------------------------------------------------------
describe('useFieldStore', () => {
  it('returns { value, setValue } matching useStore', () => {
    __mocked.set<TestState>().set('color', 'red')
    const store = createGenericStore<TestState>()
    const hook = renderHook(() => store.useFieldStore('color'))

    expect(hook.result.current.value).toBe('red')
    expect(typeof hook.result.current.setValue).toBe('function')
  })

  it('setValue updates state and tracks call', () => {
    const store = createGenericStore<TestState>()
    const hook = renderHook(() => store.useFieldStore('color'))

    hook.result.current.setValue('blue')

    expect(__mocked.state.value['color']).toBe('blue')
    expect(__mocked.state.calls).toHaveLength(1)
  })
})

// ---------------------------------------------------------------------------
// useJitStore
// ---------------------------------------------------------------------------
describe('useJitStore', () => {
  it('returns proxyValue, setChanges, getState', () => {
    const store = createGenericStore<TestState>()
    const hook = renderHook(() => store.useJitStore())

    expect(hook.result.current).toHaveProperty('proxyValue')
    expect(typeof hook.result.current.setChanges).toBe('function')
    expect(typeof hook.result.current.getState).toBe('function')
  })

  it('setChanges applies batch and tracks calls', () => {
    const store = createGenericStore<TestState>()
    const hook = renderHook(() => store.useJitStore())

    hook.result.current.setChanges([
      ['x', 1, {}],
      ['y', 2, {}],
    ])

    expect(__mocked.state.value).toEqual({ x: 1, y: 2 })
    expect(__mocked.state.calls).toHaveLength(2)
  })

  it('getState returns snapshot', () => {
    __mocked.set<TestState>().set('count', 42)
    const store = createGenericStore<TestState>()
    const hook = renderHook(() => store.useJitStore())
    const snap = hook.result.current.getState()

    expect(snap).toEqual({ count: 42 })
  })
})

// ---------------------------------------------------------------------------
// useConcerns / useSideEffects — effect tracking (plain functions, no hooks)
// ---------------------------------------------------------------------------
describe('effect tracking', () => {
  it('useConcerns logs to effects', () => {
    const store = createGenericStore<TestState>()
    const registration = { email: { disabledWhen: { boolLogic: { AND: [] } } } }

    store.useConcerns('my-id', registration)

    expect(__mocked.state.effects).toEqual([
      { id: 'my-id', type: 'concerns', registration },
    ])
  })

  it('useSideEffects logs to effects', () => {
    const store = createGenericStore<TestState>()
    const effects = { listeners: [] }

    store.useSideEffects('se-id', effects)

    expect(__mocked.state.effects).toEqual([
      { id: 'se-id', type: 'sideEffects', registration: effects },
    ])
  })
})

// ---------------------------------------------------------------------------
// withConcerns
// ---------------------------------------------------------------------------
describe('withConcerns', () => {
  it('returns useFieldStore', () => {
    const store = createGenericStore<TestState>()
    const scoped = store.withConcerns({})

    expect(typeof scoped.useFieldStore).toBe('function')
  })

  it('useFieldStore works identically to base', () => {
    __mocked.set<TestState>().set('val', 100)
    const store = createGenericStore<TestState>()
    const scoped = store.withConcerns({})
    const hook = renderHook(() => scoped.useFieldStore('val'))

    expect(hook.result.current.value).toBe(100)
  })
})

// ---------------------------------------------------------------------------
// React component rendering — valtio handles re-renders automatically
// ---------------------------------------------------------------------------
describe('React component integration', () => {
  const store = createGenericStore<TestState>()

  const EmailDisplay = () => {
    const [email] = store.useStore('email')
    return React.createElement(
      'span',
      { 'data-testid': 'email' },
      String(email ?? ''),
    )
  }

  it('renders initial state from Provider', async () => {
    render(
      React.createElement(
        store.Provider,
        {
          initialState: { email: 'init@test.com' } as unknown as TestState,
          children: null,
        },
        React.createElement(EmailDisplay),
      ),
    )

    await __mocked.flush()
    expect(screen.getByTestId('email').textContent).toBe('init@test.com')
  })

  it('re-renders after set() + flush()', async () => {
    render(
      React.createElement(
        store.Provider,
        { initialState: { email: '' } as unknown as TestState, children: null },
        React.createElement(EmailDisplay),
      ),
    )

    await __mocked.flush()
    expect(screen.getByTestId('email').textContent).toBe('')

    __mocked.set<TestState>().set('email', 'updated@test.com')
    await __mocked.flush()

    expect(screen.getByTestId('email').textContent).toBe('updated@test.com')
  })

  it('re-renders after hook setValue + flush()', async () => {
    const EmailForm = () => {
      const [email, setEmail] = store.useStore('email')
      return React.createElement(
        'div',
        null,
        React.createElement(
          'span',
          { 'data-testid': 'email-val' },
          String(email ?? ''),
        ),
        React.createElement(
          'button',
          {
            'data-testid': 'update-btn',
            'onClick': () => setEmail('clicked@test.com' as never),
          },
          'Update',
        ),
      )
    }

    render(
      React.createElement(
        store.Provider,
        {
          initialState: { email: 'start' } as unknown as TestState,
          children: null,
        },
        React.createElement(EmailForm),
      ),
    )

    await __mocked.flush()
    expect(screen.getByTestId('email-val').textContent).toBe('start')

    screen.getByTestId('update-btn').click()
    await __mocked.flush()

    expect(screen.getByTestId('email-val').textContent).toBe('clicked@test.com')
    expect(__mocked.state.calls).toContainEqual({
      path: 'email',
      value: 'clicked@test.com',
      meta: undefined,
    })
  })
})
