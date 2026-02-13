/**
 * Listener Spillage Tests
 *
 * Verifies listener change propagation:
 * - Deeper listeners run first (sorted by depth, deepest first)
 * - Changes produced by deeper listeners spill to shallower listeners
 * - Changes from earlier same-path listeners accumulate for later ones
 */

import { screen } from '@testing-library/react'
import { beforeEach, describe, expect, it, vi } from 'vitest'

import type { ListenerRegistration } from '../../src/core/types'
import { registerListener } from '../../src/sideEffects/prebuilts/listeners'
import type { TestState } from '../mocks'
import { testStateFixtures } from '../mocks'
import { createStore, fireEvent, flush, renderWithStore } from '../utils/react'

describe('Pipeline: Listener Spillage', () => {
  let store: ReturnType<typeof createStore<TestState>>

  beforeEach(() => {
    store = createStore<TestState>(testStateFixtures.profileEmpty)
  })

  it('listeners on the same path fire in registration order', async () => {
    const callOrder: string[] = []

    function TestComponent() {
      const { setChanges } = store.useJitStore()
      const field = store.useFieldStore('personalInfo')

      return (
        <div>
          <span data-testid="first">{field.value.firstName}</span>
          <button
            data-testid="set"
            onClick={() =>
              setChanges([['personalInfo.firstName', 'Alice', {}]])
            }
          >
            Set
          </button>
        </div>
      )
    }

    const rendered = renderWithStore(<TestComponent />, store)

    const orderListenerA = () => {
      callOrder.push('A')
      return undefined
    }
    const orderListenerB = () => {
      callOrder.push('B')
      return undefined
    }
    const orderListenerC = () => {
      callOrder.push('C')
      return undefined
    }

    const listenerA: ListenerRegistration<TestState> = {
      path: 'personalInfo',
      scope: 'personalInfo',
      fn: orderListenerA,
    }
    const listenerB: ListenerRegistration<TestState> = {
      path: 'personalInfo',
      scope: 'personalInfo',
      fn: orderListenerB,
    }
    const listenerC: ListenerRegistration<TestState> = {
      path: 'personalInfo',
      scope: 'personalInfo',
      fn: orderListenerC,
    }

    registerListener(rendered.storeInstance, listenerA)
    registerListener(rendered.storeInstance, listenerB)
    registerListener(rendered.storeInstance, listenerC)

    fireEvent.click(screen.getByTestId('set'))
    await flush()

    expect(callOrder).toEqual(['A', 'B', 'C'])
  })

  it('changes from deeper listeners spill to shallower listeners', async () => {
    const shallowSpy = vi.fn()

    function TestComponent() {
      const { setChanges } = store.useJitStore()
      const personalField = store.useFieldStore('personalInfo')
      const bioField = store.useFieldStore('bio')

      return (
        <div>
          <span data-testid="first">{personalField.value.firstName}</span>
          <span data-testid="bio">{bioField.value}</span>
          <button
            data-testid="set"
            onClick={() =>
              setChanges([['personalInfo.firstName', 'Alice', {}]])
            }
          >
            Set
          </button>
        </div>
      )
    }

    const rendered = renderWithStore(<TestComponent />, store)

    // Deep listener on personalInfo — produces a top-level 'bio' change
    const deepListenerFn = () => {
      return [['bio', 'updated-by-deep-listener', {}]]
    }

    const rootListenerFn = (changes: readonly [string, unknown, object][]) => {
      shallowSpy(changes.map((c) => [c[0], c[1]]))
      return undefined
    }

    const deepListener: ListenerRegistration<TestState> = {
      path: 'personalInfo',
      scope: 'personalInfo',
      fn: deepListenerFn,
    }

    // Root listener (null) — should see the 'bio' change produced by the deeper listener
    const rootListener: ListenerRegistration<TestState> = {
      path: null,
      scope: null,
      fn: rootListenerFn,
    }

    registerListener(rendered.storeInstance, deepListener)
    registerListener(rendered.storeInstance, rootListener)

    fireEvent.click(screen.getByTestId('set'))
    await flush()

    // Root listener should have received the 'bio' change produced by the deeper listener
    expect(shallowSpy).toHaveBeenCalled()
    const receivedChanges = shallowSpy.mock.calls[0][0] as [string, unknown][]
    const paths = receivedChanges.map((c) => c[0])
    expect(paths).toContain('bio')

    // And the change should be applied
    expect(screen.getByTestId('bio')).toHaveTextContent(
      'updated-by-deep-listener',
    )
  })

  it('same-path listeners accumulate changes from previous siblings', async () => {
    const listenerBSpy = vi.fn()

    function TestComponent() {
      const { setChanges } = store.useJitStore()
      const personalField = store.useFieldStore('personalInfo')
      const bioField = store.useFieldStore('bio')

      return (
        <div>
          <span data-testid="first">{personalField.value.firstName}</span>
          <span data-testid="bio">{bioField.value}</span>
          <button
            data-testid="set"
            onClick={() =>
              setChanges([['personalInfo.firstName', 'Alice', {}]])
            }
          >
            Set
          </button>
        </div>
      )
    }

    const rendered = renderWithStore(<TestComponent />, store)

    // Listener A on personalInfo — produces a change to personalInfo.lastName
    const accumulateListenerA = () => {
      return [['personalInfo.lastName', 'Smith', {}]]
    }

    // Listener B on personalInfo — should see accumulated changes including
    // the personalInfo.lastName change from listener A
    const accumulateListenerB = (
      changes: readonly [string, unknown, object][],
    ) => {
      listenerBSpy(changes.map((c) => c[0]))
      return undefined
    }

    const listenerA: ListenerRegistration<TestState> = {
      path: 'personalInfo',
      scope: 'personalInfo',
      fn: accumulateListenerA,
    }

    const listenerB: ListenerRegistration<TestState> = {
      path: 'personalInfo',
      scope: 'personalInfo',
      fn: accumulateListenerB,
    }

    registerListener(rendered.storeInstance, listenerA)
    registerListener(rendered.storeInstance, listenerB)

    fireEvent.click(screen.getByTestId('set'))
    await flush()

    // Listener B should see both the original firstName change AND the lastName
    // change produced by listener A (accumulated)
    expect(listenerBSpy).toHaveBeenCalled()
    const receivedPaths = listenerBSpy.mock.calls[0][0] as string[]
    expect(receivedPaths).toContain('firstName')
    expect(receivedPaths).toContain('lastName')
  })

  it('rejects anonymous listener functions', () => {
    const rendered = renderWithStore(<div />, store)

    // Use Object.defineProperty to create a function with no name
    // eslint-disable-next-line @typescript-eslint/no-empty-function
    const anonymousFn = (() => {}) as unknown as () => undefined
    Object.defineProperty(anonymousFn, 'name', { value: '', writable: false })

    const anonymousListener: ListenerRegistration<TestState> = {
      path: 'personalInfo',
      scope: 'personalInfo',
      fn: anonymousFn,
    }

    expect(() => {
      registerListener(rendered.storeInstance, anonymousListener)
    }).toThrow(/anonymous/i)
  })

  it('rejects duplicate listener IDs in dev mode', () => {
    const rendered = renderWithStore(<div />, store)

    const duplicateFn = () => {
      return undefined
    }

    const listenerA: ListenerRegistration<TestState> = {
      path: 'personalInfo',
      scope: 'personalInfo',
      fn: duplicateFn,
    }

    const listenerB: ListenerRegistration<TestState> = {
      path: 'personalInfo',
      scope: 'personalInfo',
      fn: duplicateFn, // Same path + same fn.name = duplicate ID
    }

    registerListener(rendered.storeInstance, listenerA)

    expect(() => {
      registerListener(rendered.storeInstance, listenerB)
    }).toThrow(/duplicate listener id/i)
  })

  it('listener changes on a different path do not trigger unrelated listeners', async () => {
    const addressListenerSpy = vi.fn()

    function TestComponent() {
      const { setChanges } = store.useJitStore()
      const personalField = store.useFieldStore('personalInfo')
      const emailField = store.useFieldStore('email')

      return (
        <div>
          <span data-testid="first">{personalField.value.firstName}</span>
          <span data-testid="email">{emailField.value}</span>
          <button
            data-testid="set"
            onClick={() =>
              setChanges([['personalInfo.firstName', 'Alice', {}]])
            }
          >
            Set
          </button>
        </div>
      )
    }

    const rendered = renderWithStore(<TestComponent />, store)

    // Listener A watches personalInfo and returns a change to 'email' (top-level)
    const emailAutoGenerateListener = () => {
      return [['email', 'auto@generated.com', {}]]
    }

    // Listener B watches addressInfo — should NOT be triggered since
    // no addressInfo child changes occurred (email is not under addressInfo)
    const addressInfoListener = () => {
      addressListenerSpy()
      return undefined
    }

    const listenerA: ListenerRegistration<TestState> = {
      path: 'personalInfo',
      scope: null,
      fn: emailAutoGenerateListener,
    }

    const listenerB: ListenerRegistration<TestState> = {
      path: 'addressInfo',
      scope: 'addressInfo',
      fn: addressInfoListener,
    }

    registerListener(rendered.storeInstance, listenerA)
    registerListener(rendered.storeInstance, listenerB)

    fireEvent.click(screen.getByTestId('set'))
    await flush()

    // Listener B should NOT have been called — no addressInfo child changes
    expect(addressListenerSpy).not.toHaveBeenCalled()

    // But the email change from listener A should still be applied
    expect(screen.getByTestId('email')).toHaveTextContent('auto@generated.com')
  })
})
