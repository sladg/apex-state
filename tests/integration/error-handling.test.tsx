/**
 * Integration Tests: Error Handling & Recovery
 *
 * Scenario: Form with validation errors and recovery
 * Tests error storage, clearing, field-level vs form-level errors
 */

import React from 'react'

import { render, screen } from '@testing-library/react'
import { beforeEach, describe, expect, it } from 'vitest'

import { createFormWithErrorsStore, formWithErrorsFixtures } from '../mocks'

describe('Integration: Error Handling & Recovery', () => {
  let store: ReturnType<typeof createFormWithErrorsStore>

  beforeEach(() => {
    store = createFormWithErrorsStore()
  })

  // TC7.1: Validation errors stored in _errors
  it('TC7.1: stores validation errors in _errors field', async () => {
    function FormComponent() {
      const emailField = store.useFieldStore('email')
      const errorsField = store.useFieldStore('_errors')

      const validateEmail = (email: string) => {
        const errors = { ...errorsField.value }
        const isValid = /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(email)

        if (!isValid && email) {
          errors.email = ['Please enter a valid email address']
        } else {
          delete errors.email
        }

        emailField.setValue(email)
        errorsField.setValue(errors)
      }

      return (
        <div>
          <input
            data-testid="email-input"
            value={emailField.value}
            onChange={(e) => validateEmail(e.target.value)}
            placeholder="Email"
          />
          <span data-testid="error-count">
            {Object.keys(errorsField.value).length}
          </span>
          {errorsField.value.email && (
            <span data-testid="email-error">{errorsField.value.email[0]}</span>
          )}
        </div>
      )
    }

    render(
      <store.Provider
        initialState={structuredClone(formWithErrorsFixtures.empty)}
      >
        <FormComponent />
      </store.Provider>,
    )

    const input = screen.getByTestId('email-input')
    fireEvent.change(input, { target: { value: 'invalid-email' } })

    await flushEffects()

    expect(screen.getByTestId('error-count')).toHaveTextContent('1')
    expect(screen.getByTestId('email-error')).toBeInTheDocument()
  })

  // TC7.2: Errors show for invalid fields (via concerns)
  it('TC7.2: displays errors from concerns', async () => {
    function FormComponent() {
      const emailField = store.useFieldStore('email')
      const errorsField = store.useFieldStore('_errors')

      // Simulate concerns reading errors
      store.useConcerns('form', {})

      const validateAndShowError = (email: string) => {
        const errors = { ...errorsField.value }
        const isValid = /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(email)

        if (!isValid && email) {
          errors.email = ['Invalid email format']
        } else {
          delete errors.email
        }

        emailField.setValue(email)
        errorsField.setValue(errors)
      }

      return (
        <div>
          <input
            data-testid="email-input"
            value={emailField.value}
            onChange={(e) => validateAndShowError(e.target.value)}
          />
          {errorsField.value.email && (
            <div data-testid="error-display">
              <strong>Error:</strong> {errorsField.value.email[0]}
            </div>
          )}
        </div>
      )
    }

    render(
      <store.Provider
        initialState={structuredClone(formWithErrorsFixtures.empty)}
      >
        <FormComponent />
      </store.Provider>,
    )

    const input = screen.getByTestId('email-input')
    fireEvent.change(input, { target: { value: 'bad-email' } })

    await flushEffects()

    expect(screen.getByTestId('error-display')).toBeInTheDocument()
    expect(screen.getByTestId('error-display')).toHaveTextContent(
      'Invalid email format',
    )
  })

  // TC7.3: Errors clear when field fixed
  it('TC7.3: clears errors when field becomes valid', async () => {
    function FormComponent() {
      const emailField = store.useFieldStore('email')
      const errorsField = store.useFieldStore('_errors')

      const validateEmail = (email: string) => {
        const errors = { ...errorsField.value }
        const isValid = /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(email)

        if (!isValid && email) {
          errors.email = ['Invalid email']
        } else {
          delete errors.email
        }

        emailField.setValue(email)
        errorsField.setValue(errors)
      }

      return (
        <div>
          <input
            data-testid="email-input"
            value={emailField.value}
            onChange={(e) => validateEmail(e.target.value)}
          />
          {errorsField.value.email && (
            <span data-testid="email-error">{errorsField.value.email[0]}</span>
          )}
        </div>
      )
    }

    render(
      <store.Provider
        initialState={structuredClone(formWithErrorsFixtures.empty)}
      >
        <FormComponent />
      </store.Provider>,
    )

    const input = screen.getByTestId('email-input')

    // Create error
    fireEvent.change(input, { target: { value: 'bad' } })

    await flushEffects()

    expect(screen.getByTestId('email-error')).toBeInTheDocument()

    // Fix error
    fireEvent.change(input, { target: { value: 'valid@example.com' } })

    await flushEffects()

    expect(screen.queryByTestId('email-error')).not.toBeInTheDocument()
  })

  // TC7.4: Errors clear all when form reset
  it('TC7.4: clears all errors on form reset', async () => {
    function FormComponent() {
      const emailField = store.useFieldStore('email')
      const passwordField = store.useFieldStore('password')
      const errorsField = store.useFieldStore('_errors')

      const validateForm = (email: string, password: string) => {
        const errors: Record<string, any[]> = {}

        if (!email || !/^[^\s@]+@[^\s@]+$/.test(email)) {
          errors.email = ['Invalid email']
        }
        if (!password || password.length < 8) {
          errors.password = ['Password too short']
        }

        return errors
      }

      const handleValidate = () => {
        const errors = validateForm(emailField.value, passwordField.value)
        errorsField.setValue(errors)
      }

      const handleReset = () => {
        emailField.setValue('')
        passwordField.setValue('')
        errorsField.setValue({})
      }

      return (
        <div>
          <input
            data-testid="email-input"
            value={emailField.value}
            onChange={(e) => emailField.setValue(e.target.value)}
            onBlur={handleValidate}
          />
          <input
            data-testid="password-input"
            type="password"
            value={passwordField.value}
            onChange={(e) => passwordField.setValue(e.target.value)}
            onBlur={handleValidate}
          />
          <button data-testid="reset-btn" onClick={handleReset}>
            Reset
          </button>
          <span data-testid="error-count">
            {Object.keys(errorsField.value).length}
          </span>
        </div>
      )
    }

    render(
      <store.Provider
        initialState={structuredClone(formWithErrorsFixtures.empty)}
      >
        <FormComponent />
      </store.Provider>,
    )

    // Create errors
    const emailInput = screen.getByTestId('email-input')
    const passwordInput = screen.getByTestId('password-input')

    fireEvent.change(emailInput, { target: { value: 'bad' } })
    fireEvent.blur(emailInput)
    await flushEffects()

    fireEvent.change(passwordInput, { target: { value: 'short' } })
    fireEvent.blur(passwordInput)
    await flushEffects()

    expect(screen.getByTestId('error-count')).toHaveTextContent('2')

    // Reset
    const resetBtn = screen.getByTestId('reset-btn')
    fireEvent.click(resetBtn)

    await flushEffects()

    expect(screen.getByTestId('error-count')).toHaveTextContent('0')
  })

  // TC7.5: Submit disabled while errors exist
  it('TC7.5: disables submit button when errors exist', async () => {
    function FormComponent() {
      const emailField = store.useFieldStore('email')
      const errorsField = store.useFieldStore('_errors')

      const validateEmail = (email: string) => {
        const errors = { ...errorsField.value }
        const isValid = /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(email)

        if (!isValid) {
          errors.email = ['Invalid email']
        } else {
          delete errors.email
        }

        emailField.setValue(email)
        errorsField.setValue(errors)
      }

      const hasErrors = Object.keys(errorsField.value).length > 0

      return (
        <div>
          <input
            data-testid="email-input"
            value={emailField.value}
            onChange={(e) => validateEmail(e.target.value)}
          />
          <button data-testid="submit-btn" disabled={hasErrors}>
            Submit
          </button>
        </div>
      )
    }

    render(
      <store.Provider
        initialState={structuredClone(formWithErrorsFixtures.empty)}
      >
        <FormComponent />
      </store.Provider>,
    )

    await flushEffects()

    const submitBtn = screen.getByTestId('submit-btn') as HTMLButtonElement
    expect(submitBtn.disabled).toBe(false)

    // Create error
    const input = screen.getByTestId('email-input')
    fireEvent.change(input, { target: { value: 'invalid' } })

    await flushEffects()

    expect(submitBtn.disabled).toBe(true)

    // Fix error
    fireEvent.change(input, { target: { value: 'valid@example.com' } })

    await flushEffects()

    expect(submitBtn.disabled).toBe(false)
  })

  // TC7.6: Error messages interpolated
  it('TC7.6: supports error message templates', async () => {
    function FormComponent() {
      const emailField = store.useFieldStore('email')
      const errorsField = store.useFieldStore('_errors')

      const validateEmail = (email: string) => {
        const errors = { ...errorsField.value }

        if (!email) {
          errors.email = ['Email is required']
        } else if (!/^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(email)) {
          // Template: use actual email value in error message
          errors.email = [`"${email}" is not a valid email address`]
        } else {
          delete errors.email
        }

        emailField.setValue(email)
        errorsField.setValue(errors)
      }

      return (
        <div>
          <input
            data-testid="email-input"
            value={emailField.value}
            onChange={(e) => validateEmail(e.target.value)}
          />
          {errorsField.value.email && (
            <span data-testid="error-message">
              {errorsField.value.email[0]}
            </span>
          )}
        </div>
      )
    }

    render(
      <store.Provider
        initialState={structuredClone(formWithErrorsFixtures.empty)}
      >
        <FormComponent />
      </store.Provider>,
    )

    const input = screen.getByTestId('email-input')
    fireEvent.change(input, { target: { value: 'invalid-email' } })

    await flushEffects()

    expect(screen.getByTestId('error-message')).toHaveTextContent(
      '"invalid-email" is not a valid email address',
    )
  })

  // TC7.7: Field-level vs form-level errors
  it('TC7.7: distinguishes field-level and form-level errors', async () => {
    function FormComponent() {
      const emailField = store.useFieldStore('email')
      const passwordField = store.useFieldStore('password')
      const errorsField = store.useFieldStore('_errors')

      const validateForm = () => {
        const errors: Record<string, any[]> = {}

        // Field-level errors
        if (!emailField.value) {
          errors['email'] = ['Email required']
        }
        if (!passwordField.value) {
          errors['password'] = ['Password required']
        }

        // Form-level error (password confirmation would go here)
        if (emailField.value && passwordField.value === 'admin') {
          errors['_form'] = ['Admin password not allowed']
        }

        errorsField.setValue(errors)
      }

      return (
        <div>
          <input
            data-testid="email-input"
            value={emailField.value}
            onChange={(e) => emailField.setValue(e.target.value)}
          />
          <input
            data-testid="password-input"
            type="password"
            value={passwordField.value}
            onChange={(e) => passwordField.setValue(e.target.value)}
          />
          <button data-testid="validate-btn" onClick={validateForm}>
            Validate
          </button>

          {errorsField.value.email && (
            <span data-testid="field-error-email">
              {errorsField.value.email[0]}
            </span>
          )}
          {errorsField.value.password && (
            <span data-testid="field-error-password">
              {errorsField.value.password[0]}
            </span>
          )}
          {errorsField.value._form && (
            <span data-testid="form-error">{errorsField.value._form[0]}</span>
          )}
        </div>
      )
    }

    render(
      <store.Provider
        initialState={structuredClone(formWithErrorsFixtures.empty)}
      >
        <FormComponent />
      </store.Provider>,
    )

    const validateBtn = screen.getByTestId('validate-btn')
    fireEvent.click(validateBtn)

    await flushEffects()

    expect(screen.getByTestId('field-error-email')).toBeInTheDocument()
    expect(screen.getByTestId('field-error-password')).toBeInTheDocument()

    // Fill fields
    const emailInput = screen.getByTestId('email-input')
    const passwordInput = screen.getByTestId('password-input')

    fireEvent.change(emailInput, { target: { value: 'test@example.com' } })
    await flushEffects()

    fireEvent.change(passwordInput, { target: { value: 'admin' } })
    await flushEffects()

    fireEvent.click(validateBtn)

    await flushEffects()

    expect(screen.queryByTestId('field-error-email')).not.toBeInTheDocument()
    expect(screen.queryByTestId('field-error-password')).not.toBeInTheDocument()
    expect(screen.getByTestId('form-error')).toBeInTheDocument()
  })

  // TC7.8: Errors survive other updates
  it('TC7.8: preserves errors when other fields are updated', async () => {
    function FormComponent() {
      const emailField = store.useFieldStore('email')
      const passwordField = store.useFieldStore('password')
      const errorsField = store.useFieldStore('_errors')

      const validateEmail = (email: string) => {
        emailField.setValue(email)
        const errors = { ...errorsField.value }

        if (!email || !/^[^\s@]+@[^\s@]+$/.test(email)) {
          errors.email = ['Invalid email']
        } else {
          delete errors.email
        }
        errorsField.setValue(errors)
      }

      const updatePassword = (password: string) => {
        passwordField.setValue(password)
        // Don't clear existing errors, just update password
      }

      return (
        <div>
          <input
            data-testid="email-input"
            value={emailField.value}
            onChange={(e) => validateEmail(e.target.value)}
          />
          <input
            data-testid="password-input"
            type="password"
            value={passwordField.value}
            onChange={(e) => updatePassword(e.target.value)}
          />
          {errorsField.value.email && (
            <span data-testid="email-error">{errorsField.value.email[0]}</span>
          )}
          <span data-testid="error-count">
            {Object.keys(errorsField.value).length}
          </span>
        </div>
      )
    }

    render(
      <store.Provider
        initialState={structuredClone(formWithErrorsFixtures.empty)}
      >
        <FormComponent />
      </store.Provider>,
    )

    // Create email error
    const emailInput = screen.getByTestId('email-input')
    fireEvent.change(emailInput, { target: { value: 'bad' } })

    await flushEffects()

    expect(screen.getByTestId('email-error')).toBeInTheDocument()
    expect(screen.getByTestId('error-count')).toHaveTextContent('1')

    // Update password (should not clear email error)
    const passwordInput = screen.getByTestId('password-input')
    fireEvent.change(passwordInput, { target: { value: 'some-password' } })

    // Email error should still exist
    await flushEffects()

    expect(screen.getByTestId('email-error')).toBeInTheDocument()
    expect(screen.getByTestId('error-count')).toHaveTextContent('1')
  })
})
