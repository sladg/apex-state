/**
 * Null-Path (Root) Listener Tests
 *
 * Verifies that listeners registered with `path: null` receive
 * all top-level changes regardless of which path was modified.
 * Each subsequent root listener sees accumulated changes from previous ones.
 */

import { screen } from '@testing-library/react'
import { beforeEach, describe, expect, it, vi } from 'vitest'

import type { ListenerRegistration } from '~/core/types'
import { registerListener } from '~/sideEffects/prebuilts/listeners'

import type { TestState } from '../mocks'
import { testStateFixtures } from '../mocks'
import { createStore, fireEvent, flush, renderWithStore } from '../utils/react'

describe('Pipeline: Null-Path (Root) Listener', () => {
  let store: ReturnType<typeof createStore<TestState>>

  beforeEach(() => {
    store = createStore<TestState>(testStateFixtures.profileEmpty)
  })

  it('root listener receives top-level changes for any path', async () => {
    const receivedChanges = vi.fn()

    function TestComponent() {
      const usernameField = store.useFieldStore('username')
      const emailField = store.useFieldStore('email')

      return (
        <div>
          <input
            data-testid="username"
            value={usernameField.value}
            onChange={(e) => usernameField.setValue(e.target.value)}
          />
          <input
            data-testid="email"
            value={emailField.value}
            onChange={(e) => emailField.setValue(e.target.value)}
          />
        </div>
      )
    }

    const rendered = renderWithStore(<TestComponent />, store)

    const rootListenerFn = (changes: readonly [string, unknown, object][]) => {
      receivedChanges(changes.map((c) => [c[0], c[1]]))
      return undefined
    }

    const rootListener: ListenerRegistration<TestState> = {
      path: null,
      scope: null,
      fn: rootListenerFn,
    }

    registerListener(rendered.storeInstance, rootListener)

    // Change username — root listener should see it
    fireEvent.change(screen.getByTestId('username'), {
      target: { value: 'alice' },
    })
    await flush()

    expect(receivedChanges).toHaveBeenCalledWith(
      expect.arrayContaining([['username', 'alice']]),
    )

    receivedChanges.mockClear()

    // Change email — root listener should also see it
    fireEvent.change(screen.getByTestId('email'), {
      target: { value: 'alice@example.com' },
    })
    await flush()

    expect(receivedChanges).toHaveBeenCalledWith(
      expect.arrayContaining([['email', 'alice@example.com']]),
    )
  })

  it('root listener only receives top-level paths (not nested)', async () => {
    const receivedPaths = vi.fn()

    function TestComponent() {
      const { setChanges } = store.useJitStore()

      return (
        <div>
          <button
            data-testid="set-nested"
            onClick={() =>
              setChanges([
                ['personalInfo.firstName', 'Alice', {}],
                ['username', 'alice', {}],
              ])
            }
          >
            Set
          </button>
        </div>
      )
    }

    const rendered = renderWithStore(<TestComponent />, store)

    const nestedPathFilterListener = (
      changes: readonly [string, unknown, object][],
    ) => {
      receivedPaths(changes.map((c) => c[0]))
      return undefined
    }

    const rootListener: ListenerRegistration<TestState> = {
      path: null,
      scope: null,
      fn: nestedPathFilterListener,
    }

    registerListener(rendered.storeInstance, rootListener)

    fireEvent.click(screen.getByTestId('set-nested'))
    await flush()

    // Root listener filters to top-level only (no dots in path)
    // So 'personalInfo.firstName' should be excluded, 'username' included
    expect(receivedPaths).toHaveBeenCalled()
    const paths = receivedPaths.mock.calls[0][0] as string[]
    expect(paths).toContain('username')
    expect(paths).not.toContain('personalInfo.firstName')
  })

  it('subsequent root listeners see accumulated changes from previous ones', async () => {
    const spyA = vi.fn()
    const spyB = vi.fn()

    function TestComponent() {
      const usernameField = store.useFieldStore('username')
      const bioField = store.useFieldStore('bio')

      return (
        <div>
          <input
            data-testid="username"
            value={usernameField.value}
            onChange={(e) => usernameField.setValue(e.target.value)}
          />
          <span data-testid="bio">{bioField.value}</span>
        </div>
      )
    }

    const rendered = renderWithStore(<TestComponent />, store)

    // Root listener A produces a 'bio' change
    const accumulatedListenerA = (
      changes: readonly [string, unknown, object][],
    ) => {
      spyA(changes.map((c) => c[0]))
      const usernameChange = changes.find((c) => c[0] === 'username')
      if (usernameChange) {
        return [['bio', 'set-by-root-A', {}]]
      }
      return undefined
    }

    // Root listener B should see the original 'username' change PLUS
    // the 'bio' change produced by root listener A
    const accumulatedListenerB = (
      changes: readonly [string, unknown, object][],
    ) => {
      spyB(changes.map((c) => c[0]))
      return undefined
    }

    const rootListenerA: ListenerRegistration<TestState> = {
      path: null,
      scope: null,
      fn: accumulatedListenerA,
    }

    const rootListenerB: ListenerRegistration<TestState> = {
      path: null,
      scope: null,
      fn: accumulatedListenerB,
    }

    registerListener(rendered.storeInstance, rootListenerA)
    registerListener(rendered.storeInstance, rootListenerB)

    fireEvent.change(screen.getByTestId('username'), {
      target: { value: 'bob' },
    })
    await flush()

    // Listener A sees only the original change
    expect(spyA).toHaveBeenCalled()
    const pathsA = spyA.mock.calls[0][0] as string[]
    expect(pathsA).toContain('username')
    expect(pathsA).not.toContain('bio')

    // Listener B sees accumulated: original + listener A's output
    expect(spyB).toHaveBeenCalled()
    const pathsB = spyB.mock.calls[0][0] as string[]
    expect(pathsB).toContain('username')
    expect(pathsB).toContain('bio')

    // Bio change should be applied
    expect(screen.getByTestId('bio')).toHaveTextContent('set-by-root-A')
  })

  it('root listener can produce changes that get applied', async () => {
    function TestComponent() {
      const usernameField = store.useFieldStore('username')
      const displayNameField = store.useFieldStore('displayName')

      return (
        <div>
          <input
            data-testid="username"
            value={usernameField.value}
            onChange={(e) => usernameField.setValue(e.target.value)}
          />
          <span data-testid="display">{displayNameField.value}</span>
        </div>
      )
    }

    const rendered = renderWithStore(<TestComponent />, store)

    const displayNameAutoSetListener = (
      changes: readonly [string, unknown, object][],
    ) => {
      // When username changes, auto-set displayName
      const usernameChange = changes.find((c) => c[0] === 'username')
      if (usernameChange) {
        return [['displayName', `@${usernameChange[1] as string}`, {}]]
      }
      return undefined
    }

    const rootListener: ListenerRegistration<TestState> = {
      path: null,
      scope: null,
      fn: displayNameAutoSetListener,
    }

    registerListener(rendered.storeInstance, rootListener)

    fireEvent.change(screen.getByTestId('username'), {
      target: { value: 'charlie' },
    })
    await flush()

    expect(screen.getByTestId('display')).toHaveTextContent('@charlie')
  })
})
