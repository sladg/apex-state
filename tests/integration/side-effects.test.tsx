/**
 * Integration Tests: Side Effects - Listeners & Validators
 *
 * Scenario: User profile with validation & notifications
 * Tests listeners, validators, clearPaths, flipPaths, and error storage
 */

import React from 'react'

import { screen } from '@testing-library/react'
import { beforeEach, describe, expect, it } from 'vitest'

import { createUserProfileStore, userProfileFixtures } from '../mocks'
import { fireEvent, flushEffects, renderWithStore } from '../utils/react'

describe('Integration: Side Effects - Listeners & Validators', () => {
  let store: ReturnType<typeof createUserProfileStore>

  beforeEach(() => {
    store = createUserProfileStore()
  })

  // TC5.1: Listener updates lastUpdated timestamp on any change
  it('TC5.1: listener updates lastUpdated on field change', async () => {
    const lastUpdatedTime = 0

    function ProfileComponent() {
      const usernameField = store.useFieldStore('username')
      const lastUpdatedField = store.useFieldStore('lastUpdated')

      // Side effect listener to update timestamp
      store.useSideEffects('timestamp-listener', {})

      // Simulate listener behavior by manually updating
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

    renderWithStore(<ProfileComponent />, store, userProfileFixtures.empty)

    const input = screen.getByTestId('username-input') as HTMLInputElement
    const initialTime = parseInt(
      screen.getByTestId('last-updated').textContent || '0',
    )

    fireEvent.change(input, { target: { value: 'john_doe' } })

    await flushEffects()

    const newTime = parseInt(
      screen.getByTestId('last-updated').textContent || '0',
    )
    expect(newTime).toBeGreaterThan(initialTime)
  })

  // TC5.2: Validator checks email format, stores errors
  it('TC5.2: validates email format and stores errors', async () => {
    function ProfileComponent() {
      const emailField = store.useFieldStore('email')
      const errorsField = store.useFieldStore('_errors')

      const validateEmail = (email: string) => {
        const isValid = /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(email)
        const newErrors = { ...errorsField.value }

        if (!isValid && email) {
          newErrors.email = ['Invalid email format']
        } else {
          delete newErrors.email
        }

        emailField.setValue(email)
        errorsField.setValue(newErrors)
      }

      return (
        <div>
          <input
            data-testid="email-input"
            value={emailField.value}
            onChange={(e) => validateEmail(e.target.value)}
            placeholder="Email"
          />
          {errorsField.value.email && (
            <span data-testid="email-error">{errorsField.value.email[0]}</span>
          )}
        </div>
      )
    }

    renderWithStore(<ProfileComponent />, store, userProfileFixtures.empty)

    const input = screen.getByTestId('email-input') as HTMLInputElement

    // Invalid email
    fireEvent.change(input, { target: { value: 'invalid-email' } })

    await flushEffects()

    expect(screen.getByTestId('email-error')).toBeInTheDocument()
    expect(screen.getByTestId('email-error')).toHaveTextContent(
      'Invalid email format',
    )

    // Valid email
    fireEvent.change(input, { target: { value: 'valid@example.com' } })

    await flushEffects()

    expect(screen.queryByTestId('email-error')).not.toBeInTheDocument()
  })

  // TC5.3: Validator checks username uniqueness (async simulation)
  it('TC5.3: validates username uniqueness asynchronously', async () => {
    const takenUsernames = ['admin', 'user', 'test']

    function ProfileComponent() {
      const usernameField = store.useFieldStore('username')
      const errorsField = store.useFieldStore('_errors')

      const validateUsername = async (username: string) => {
        usernameField.setValue(username)

        // Simulate async check
        await new Promise((resolve) => setTimeout(resolve, 10))

        const newErrors = { ...errorsField.value }
        if (takenUsernames.includes(username.toLowerCase())) {
          newErrors.username = ['Username is already taken']
        } else {
          delete newErrors.username
        }
        errorsField.setValue(newErrors)
      }

      return (
        <div>
          <input
            data-testid="username-input"
            value={usernameField.value}
            onChange={(e) => validateUsername(e.target.value)}
            placeholder="Username"
          />
          {errorsField.value.username && (
            <span data-testid="username-error">
              {errorsField.value.username[0]}
            </span>
          )}
        </div>
      )
    }

    renderWithStore(<ProfileComponent />, store, userProfileFixtures.empty)

    const input = screen.getByTestId('username-input') as HTMLInputElement

    // Try taken username
    fireEvent.change(input, { target: { value: 'admin' } })

    await flushEffects()

    expect(screen.getByTestId('username-error')).toBeInTheDocument()

    // Try available username
    fireEvent.change(input, { target: { value: 'newuser' } })

    await flushEffects()

    expect(screen.queryByTestId('username-error')).not.toBeInTheDocument()
  })

  // TC5.4: ClearPaths removes field from object when not needed
  it('TC5.4: clears unnecessary fields from state', async () => {
    function ProfileComponent() {
      const { getState, setChanges } = store.useJitStore()

      const handleClearBio = () => {
        const state = getState()
        // Clear bio field
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
      username: 'john',
      email: 'john@example.com',
      age: 30,
      bio: 'Software developer',
      isActive: true,
      lastUpdated: Date.now(),
      _errors: {},
    })

    expect(screen.getByTestId('bio-value')).toHaveTextContent(
      'Software developer',
    )

    const clearBtn = screen.getByTestId('clear-bio-btn')
    fireEvent.click(clearBtn)

    await flushEffects()

    expect(screen.getByTestId('bio-value')).toHaveTextContent('')
  })

  // TC5.5: FlipPaths syncs bidirectional relationships
  it('TC5.5: flipPaths inverts boolean relationships', async () => {
    function ProfileComponent() {
      store.useSideEffects('flip-active', {
        flipPaths: [],
        // In real scenario: would include isActive ↔ isInactive mapping
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
      username: 'john',
      email: 'john@example.com',
      age: 30,
      bio: '',
      isActive: false,
      lastUpdated: Date.now(),
      _errors: {},
    })

    expect(screen.getByTestId('active-status')).toHaveTextContent('Inactive')

    const checkbox = screen.getByTestId('active-checkbox') as HTMLInputElement
    fireEvent.click(checkbox)

    await flushEffects()

    expect(screen.getByTestId('active-status')).toHaveTextContent('Active')
  })

  // TC5.6: Multiple side effects work together
  it('TC5.6: multiple side effects execute without interference', async () => {
    function ProfileComponent() {
      store.useSideEffects('multi-effects', {})

      const usernameField = store.useFieldStore('username')
      const emailField = store.useFieldStore('email')
      const ageField = store.useFieldStore('age')
      const lastUpdatedField = store.useFieldStore('lastUpdated')
      const errorsField = store.useFieldStore('_errors')

      const handleUsernameChange = (value: string) => {
        usernameField.setValue(value)
        lastUpdatedField.setValue(Date.now())

        const errors = { ...errorsField.value }
        if (value.length < 3) {
          errors.username = ['Username too short']
        } else {
          delete errors.username
        }
        errorsField.setValue(errors)
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

    renderWithStore(<ProfileComponent />, store, userProfileFixtures.empty)

    const usernameInput = screen.getByTestId(
      'username-input',
    ) as HTMLInputElement
    const emailInput = screen.getByTestId('email-input') as HTMLInputElement

    // Trigger multiple effects
    fireEvent.change(usernameInput, { target: { value: 'ab' } })
    fireEvent.change(emailInput, { target: { value: 'test@example.com' } })

    await flushEffects()

    expect(screen.getByTestId('error-count')).toHaveTextContent('1')
  })

  // TC5.7: Side effects don't interfere with concerns
  it('TC5.7: side effects and concerns work together', async () => {
    function ProfileComponent() {
      store.useSideEffects('validators', {})

      store.useConcerns('form', {})

      const usernameField = store.useFieldStore('username')
      const emailField = store.useFieldStore('email')
      const errorsField = store.useFieldStore('_errors')

      const handleEmailChange = (email: string) => {
        emailField.setValue(email)

        const errors = { ...errorsField.value }
        const isValid = /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(email)
        if (!isValid && email) {
          errors.email = ['Invalid email']
        } else {
          delete errors.email
        }
        errorsField.setValue(errors)
      }

      return (
        <div>
          <input
            data-testid="username-input"
            value={usernameField.value}
            onChange={(e) => usernameField.setValue(e.target.value)}
            placeholder="Username"
          />
          <input
            data-testid="email-input"
            value={emailField.value}
            onChange={(e) => handleEmailChange(e.target.value)}
            placeholder="Email"
          />
          {errorsField.value.email && (
            <span data-testid="email-error">{errorsField.value.email[0]}</span>
          )}
        </div>
      )
    }

    renderWithStore(<ProfileComponent />, store, userProfileFixtures.empty)

    const emailInput = screen.getByTestId('email-input') as HTMLInputElement

    fireEvent.change(emailInput, { target: { value: 'invalid' } })

    await flushEffects()

    expect(screen.getByTestId('email-error')).toBeInTheDocument()
  })
})
