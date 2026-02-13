/**
 * Integration Tests: Side Effects - Listeners & Validators
 *
 * Scenario: User profile with validation & notifications
 * Tests listeners, validators, clearPaths, flipPaths, and error storage
 */

import { screen } from '@testing-library/react'
import { beforeEach, describe, expect, it, vi } from 'vitest'

import type { TestState } from '../mocks'
import { defaults, testStateFixtures } from '../mocks'
import { EmailValidationForm } from '../utils/components'
import {
  createStore,
  fireEvent,
  flush,
  renderWithStore,
  validateField,
} from '../utils/react'

describe('Integration: Side Effects - Listeners & Validators', () => {
  let store: ReturnType<typeof createStore<TestState>>

  beforeEach(() => {
    store = createStore<TestState>(testStateFixtures.profileEmpty)
  })

  it('TC5.1: listener updates lastUpdated on field change', async () => {
    function ProfileComponent() {
      const usernameField = store.useFieldStore('username')
      const lastUpdatedField = store.useFieldStore('lastUpdated')

      store.useSideEffects('timestamp-listener', {})

      const handleChange = (newValue: string) => {
        usernameField.setValue(newValue)
        lastUpdatedField.setValue(Date.now())
      }

      return (
        <div>
          <input
            data-testid="username-input"
            value={usernameField.value}
            onChange={(e) => handleChange(e.target.value)}
            placeholder="Username"
          />
          <span data-testid="last-updated">{lastUpdatedField.value}</span>
        </div>
      )
    }

    renderWithStore(<ProfileComponent />, store)

    const input = screen.getByTestId('username-input') as HTMLInputElement
    const initialTime = parseInt(
      screen.getByTestId('last-updated').textContent || '0',
    )

    fireEvent.change(input, { target: { value: 'john_doe' } })

    await flush()

    const newTime = parseInt(
      screen.getByTestId('last-updated').textContent || '0',
    )
    expect(newTime).toBeGreaterThan(initialTime)
  })

  it('TC5.2: validates email format and stores errors', async () => {
    renderWithStore(
      <EmailValidationForm
        store={store}
        errorMessage="Invalid email format"
        showErrorCount={false}
      />,
      store,
    )

    const input = screen.getByTestId('email-input') as HTMLInputElement

    // Invalid email
    fireEvent.change(input, { target: { value: 'invalid-email' } })

    await flush()

    expect(screen.getByTestId('email-error')).toBeInTheDocument()
    expect(screen.getByTestId('email-error')).toHaveTextContent(
      'Invalid email format',
    )

    // Valid email
    fireEvent.change(input, { target: { value: 'valid@example.com' } })

    await flush()

    expect(screen.queryByTestId('email-error')).not.toBeInTheDocument()
  })

  it('TC5.3: validates username uniqueness asynchronously', async () => {
    vi.useFakeTimers()

    const takenUsernames = ['admin', 'user', 'test']

    function ProfileComponent() {
      const usernameField = store.useFieldStore('username')
      const errorsField = store.useFieldStore('_errors')

      const handleChange = async (username: string) => {
        usernameField.setValue(username)

        // Simulate async check
        await new Promise((resolve) => setTimeout(resolve, 10))

        validateField(
          errorsField,
          'username',
          !takenUsernames.includes(username.toLowerCase()),
          'Username is already taken',
        )
      }

      return (
        <div>
          <input
            data-testid="username-input"
            value={usernameField.value}
            onChange={(e) => handleChange(e.target.value)}
            placeholder="Username"
          />
          {errorsField.value['username'] && (
            <span data-testid="username-error">
              {errorsField.value['username'][0]}
            </span>
          )}
        </div>
      )
    }

    renderWithStore(<ProfileComponent />, store)

    const input = screen.getByTestId('username-input') as HTMLInputElement

    // Try taken username
    fireEvent.change(input, { target: { value: 'admin' } })

    await vi.advanceTimersByTimeAsync(10)
    await flush()

    expect(screen.getByTestId('username-error')).toBeInTheDocument()

    // Try available username
    fireEvent.change(input, { target: { value: 'newuser' } })

    await vi.advanceTimersByTimeAsync(10)
    await flush()

    expect(screen.queryByTestId('username-error')).not.toBeInTheDocument()

    vi.useRealTimers()
  })

  it('TC5.4: clears unnecessary fields from state', async () => {
    function ProfileComponent() {
      const { setChanges } = store.useJitStore()

      const handleClearBio = () => {
        setChanges([['bio', '', {}]])
      }

      const bioField = store.useFieldStore('bio')

      return (
        <div>
          <input
            data-testid="bio-input"
            value={bioField.value}
            onChange={(e) => bioField.setValue(e.target.value)}
            placeholder="Bio"
          />
          <button data-testid="clear-bio-btn" onClick={handleClearBio}>
            Clear Bio
          </button>
          <span data-testid="bio-value">{bioField.value}</span>
        </div>
      )
    }

    renderWithStore(<ProfileComponent />, store, {
      ...defaults,
      username: 'john',
      email: 'john@example.com',
      age: 30,
      bio: 'Software developer',
      isActive: true,
      lastUpdated: Date.now(),
    })

    expect(screen.getByTestId('bio-value')).toHaveTextContent(
      'Software developer',
    )

    const clearBtn = screen.getByTestId('clear-bio-btn')
    fireEvent.click(clearBtn)

    await flush()

    expect(screen.getByTestId('bio-value')).toHaveTextContent('')
  })

  it('TC5.5: flipPaths inverts boolean relationships', async () => {
    function ProfileComponent() {
      store.useSideEffects('flip-active', {
        flipPaths: [],
      })

      const isActiveField = store.useFieldStore('isActive')

      return (
        <div>
          <label>
            <input
              data-testid="active-checkbox"
              type="checkbox"
              checked={isActiveField.value}
              onChange={(e) => isActiveField.setValue(e.target.checked)}
            />
            Active
          </label>
          <span data-testid="active-status">
            {isActiveField.value ? 'Active' : 'Inactive'}
          </span>
        </div>
      )
    }

    renderWithStore(<ProfileComponent />, store, {
      ...defaults,
      username: 'john',
      email: 'john@example.com',
      age: 30,
      isActive: false,
      lastUpdated: Date.now(),
    })

    expect(screen.getByTestId('active-status')).toHaveTextContent('Inactive')

    const checkbox = screen.getByTestId('active-checkbox') as HTMLInputElement
    fireEvent.click(checkbox)

    await flush()

    expect(screen.getByTestId('active-status')).toHaveTextContent('Active')
  })

  it('TC5.6: multiple side effects execute without interference', async () => {
    function ProfileComponent() {
      store.useSideEffects('multi-effects', {})

      const usernameField = store.useFieldStore('username')
      const emailField = store.useFieldStore('email')
      const lastUpdatedField = store.useFieldStore('lastUpdated')
      const errorsField = store.useFieldStore('_errors')

      const handleUsernameChange = (value: string) => {
        usernameField.setValue(value)
        lastUpdatedField.setValue(Date.now())
        validateField(
          errorsField,
          'username',
          value.length >= 3,
          'Username too short',
        )
      }

      return (
        <div>
          <input
            data-testid="username-input"
            value={usernameField.value}
            onChange={(e) => handleUsernameChange(e.target.value)}
          />
          <input
            data-testid="email-input"
            value={emailField.value}
            onChange={(e) => emailField.setValue(e.target.value)}
          />
          <span data-testid="error-count">
            {Object.keys(errorsField.value).length}
          </span>
        </div>
      )
    }

    renderWithStore(<ProfileComponent />, store)

    const usernameInput = screen.getByTestId(
      'username-input',
    ) as HTMLInputElement
    const emailInput = screen.getByTestId('email-input') as HTMLInputElement

    // Trigger multiple effects
    fireEvent.change(usernameInput, { target: { value: 'ab' } })
    fireEvent.change(emailInput, { target: { value: 'test@example.com' } })

    await flush()

    expect(screen.getByTestId('error-count')).toHaveTextContent('1')
  })

  it('TC5.7: side effects and concerns work together', async () => {
    function ProfileComponent() {
      store.useSideEffects('validators', {})
      store.useConcerns('form', {})

      return (
        <EmailValidationForm
          store={store}
          errorMessage="Invalid email"
          showErrorCount={false}
        />
      )
    }

    renderWithStore(<ProfileComponent />, store)

    const emailInput = screen.getByTestId('email-input') as HTMLInputElement

    fireEvent.change(emailInput, { target: { value: 'invalid' } })

    await flush()

    expect(screen.getByTestId('email-error')).toBeInTheDocument()
  })
})
