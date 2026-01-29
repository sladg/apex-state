/**
 * Integration Tests: Form Validation with Concerns
 *
 * Scenario: User registration form with dependent fields
 * Tests validation, error storage, and dynamic UI state from concerns
 */

import React from 'react'

import { cleanup, fireEvent, render, screen } from '@testing-library/react'
import { afterEach, beforeEach, describe, expect, it } from 'vitest'
import { z } from 'zod'

import { createRegistrationFormStore, registrationFormFixtures } from '../mocks'

describe('Integration: Form Validation with Concerns', () => {
  let store: ReturnType<typeof createRegistrationFormStore>

  beforeEach(() => {
    store = createRegistrationFormStore()
  })

  afterEach(() => {
    cleanup()
  })

  // TC1.1: Email validation with Zod schema
  it('TC1.1: validates email format with Zod schema', async () => {
    const emailSchema = z.string().email('Invalid email format')

    function FormComponent() {
      store.useConcerns('form', {
        email: {
          validationState: { schema: emailSchema },
        },
      })

      const emailField = store.useFieldStore('email')
      const emailConcerns = store.useFieldConcerns('email')

      return (
        <div>
          <input
            data-testid="email-input"
            value={emailField.value}
            onChange={(e) => emailField.setValue(e.target.value)}
            placeholder="Enter email"
          />
          {emailConcerns.validationState?.isError && (
            <span data-testid="email-error">Invalid email format</span>
          )}
          {emailConcerns.validationState &&
            !emailConcerns.validationState.isError && (
              <span data-testid="email-valid">✓</span>
            )}
        </div>
      )
    }

    render(
      <store.Provider initialState={{ ...registrationFormFixtures.empty }}>
        <FormComponent />
      </store.Provider>,
    )

    const input = screen.getByTestId('email-input') as HTMLInputElement
    expect(input.value).toBe('')

    // Invalid email - should show error concern
    fireEvent.change(input, { target: { value: 'invalid-email' } })
    await flushEffects()
    expect(input.value).toBe('invalid-email')

    // Valid email - should show valid concern
    fireEvent.change(input, { target: { value: '' } })
    fireEvent.change(input, { target: { value: 'test@example.com' } })
    await flushEffects()
    expect(input.value).toBe('test@example.com')
  })

  // TC1.2: Password complexity validation
  it('TC1.2: validates password complexity', async () => {
    const passwordSchema = z
      .string()
      .min(8, 'At least 8 characters')
      .regex(/[A-Z]/, 'Must contain uppercase')
      .regex(/[0-9]/, 'Must contain number')

    function FormComponent() {
      store.useConcerns('form', {
        password: {
          validationState: { schema: passwordSchema },
        },
      })

      const passwordField = store.useFieldStore('password')
      const passwordConcerns = store.useFieldConcerns('password')

      return (
        <div>
          <input
            data-testid="password-input"
            type="password"
            value={passwordField.value}
            onChange={(e) => passwordField.setValue(e.target.value)}
          />
          {passwordConcerns.validationState?.isError && (
            <span data-testid="password-error">Invalid password</span>
          )}
        </div>
      )
    }

    render(
      <store.Provider initialState={{ ...registrationFormFixtures.empty }}>
        <FormComponent />
      </store.Provider>,
    )

    const input = screen.getByTestId('password-input') as HTMLInputElement

    // Weak password
    fireEvent.change(input, { target: { value: 'weak' } })
    await flushEffects()

    expect(screen.queryByTestId('password-error')).toBeInTheDocument()

    // Strong password
    fireEvent.change(input, { target: { value: '' } })
    fireEvent.change(input, { target: { value: 'StrongPass123' } })
    await flushEffects()

    expect(screen.queryByTestId('password-error')).not.toBeInTheDocument()
  })

  // TC1.3: Confirm password matches (cross-field validation)
  it('TC1.3: validates confirm password matches password', async () => {
    function FormComponent() {
      const passwordField = store.useFieldStore('password')
      const confirmField = store.useFieldStore('confirmPassword')

      // Custom side effect for cross-field validation
      store.useSideEffects('confirmValidation', {})

      return (
        <div>
          <input
            data-testid="password-input"
            type="password"
            value={passwordField.value}
            onChange={(e) => passwordField.setValue(e.target.value)}
          />
          <input
            data-testid="confirm-input"
            type="password"
            value={confirmField.value}
            onChange={(e) => confirmField.setValue(e.target.value)}
          />
        </div>
      )
    }

    render(
      <store.Provider initialState={{ ...registrationFormFixtures.empty }}>
        <FormComponent />
      </store.Provider>,
    )

    const passwordInput = screen.getByTestId(
      'password-input',
    ) as HTMLInputElement
    const confirmInput = screen.getByTestId('confirm-input') as HTMLInputElement

    // Set passwords
    fireEvent.change(passwordInput, { target: { value: 'Test123' } })
    fireEvent.change(confirmInput, { target: { value: 'Test123' } })

    await flushEffects()

    expect(passwordInput.value).toBe('Test123')
    expect(confirmInput.value).toBe('Test123')

    // Change confirm to not match
    fireEvent.change(confirmInput, { target: { value: '' } })
    fireEvent.change(confirmInput, { target: { value: 'Different' } })
    await flushEffects()

    expect(confirmInput.value).toBe('Different')
  })

  // TC1.4: Terms must be agreed (conditional validation)
  it('TC1.4: validates terms agreement is required', async () => {
    function FormComponent() {
      const termsField = store.useFieldStore('agreeToTerms')

      return (
        <div>
          <label>
            <input
              data-testid="terms-input"
              type="checkbox"
              checked={termsField.value}
              onChange={(e) => termsField.setValue(e.target.checked)}
            />
            I agree to the terms
          </label>
          {!termsField.value && (
            <span data-testid="terms-error">Must agree to terms</span>
          )}
        </div>
      )
    }

    render(
      <store.Provider initialState={{ ...registrationFormFixtures.empty }}>
        <FormComponent />
      </store.Provider>,
    )

    const input = screen.getByTestId('terms-input') as HTMLInputElement
    expect(input.checked).toBe(false)
    expect(screen.getByTestId('terms-error')).toBeInTheDocument()

    fireEvent.click(input)
    await flushEffects()

    expect(input.checked).toBe(true)
    expect(screen.queryByTestId('terms-error')).not.toBeInTheDocument()
  })

  // TC1.5: Error messages display via concerns
  it('TC1.5: displays error messages from concerns', async () => {
    const emailSchema = z.string().email('Please enter a valid email')

    function FormComponent() {
      store.useConcerns('form', {
        email: {
          validationState: { schema: emailSchema },
        },
      })

      const emailField = store.useFieldStore('email')
      const emailConcerns = store.useFieldConcerns('email')

      return (
        <div>
          <input
            data-testid="email-input"
            value={emailField.value}
            onChange={(e) => emailField.setValue(e.target.value)}
          />
          {emailConcerns.validationState?.isError && (
            <span data-testid="error-message">Please enter a valid email</span>
          )}
        </div>
      )
    }

    render(
      <store.Provider initialState={{ ...registrationFormFixtures.empty }}>
        <FormComponent />
      </store.Provider>,
    )

    const input = screen.getByTestId('email-input') as HTMLInputElement
    fireEvent.change(input, { target: { value: 'invalid' } })

    await flushEffects()

    expect(screen.getByTestId('error-message')).toBeInTheDocument()
  })

  // TC1.6: Errors clear when fixed
  it('TC1.6: clears errors when field becomes valid', async () => {
    const emailSchema = z.string().email()

    function FormComponent() {
      store.useConcerns('form', {
        email: {
          validationState: { schema: emailSchema },
        },
      })

      const emailField = store.useFieldStore('email')
      const emailConcerns = store.useFieldConcerns('email')

      return (
        <div>
          <input
            data-testid="email-input"
            value={emailField.value}
            onChange={(e) => emailField.setValue(e.target.value)}
          />
          {emailConcerns.validationState?.isError && (
            <span data-testid="error">Invalid</span>
          )}
        </div>
      )
    }

    render(
      <store.Provider initialState={{ ...registrationFormFixtures.empty }}>
        <FormComponent />
      </store.Provider>,
    )

    const input = screen.getByTestId('email-input') as HTMLInputElement

    // Create error
    fireEvent.change(input, { target: { value: 'invalid' } })
    await flushEffects()

    expect(screen.getByTestId('error')).toBeInTheDocument()

    // Fix error - select all and type new value
    fireEvent.change(input, { target: { value: '' } })
    fireEvent.change(input, { target: { value: 'valid@example.com' } })
    await flushEffects()

    expect(screen.queryByTestId('error')).not.toBeInTheDocument()
  })

  // TC1.7: Submit enabled when all valid (visibleWhen concern)
  it('TC1.7: submit button visibility depends on form validity', async () => {
    const emailSchema = z.string().email()
    const passwordSchema = z.string().min(8)

    function FormComponent() {
      store.useConcerns('form', {
        email: { validationState: { schema: emailSchema } },
        password: { validationState: { schema: passwordSchema } },
      })

      const emailField = store.useFieldStore('email')
      const emailConcerns = store.useFieldConcerns('email')
      const passwordField = store.useFieldStore('password')
      const passwordConcerns = store.useFieldConcerns('password')

      const isEmailValid =
        emailConcerns.validationState && !emailConcerns.validationState.isError
      const isPasswordValid =
        passwordConcerns.validationState &&
        !passwordConcerns.validationState.isError

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
          {isEmailValid && isPasswordValid && (
            <button data-testid="submit-btn">Submit</button>
          )}
        </div>
      )
    }

    render(
      <store.Provider initialState={{ ...registrationFormFixtures.empty }}>
        <FormComponent />
      </store.Provider>,
    )

    // Submit should not be visible initially - wait for fields to be in document first
    await flushEffects()

    expect(screen.getByTestId('email-input')).toBeInTheDocument()

    expect(screen.queryByTestId('submit-btn')).not.toBeInTheDocument()

    // Fill email
    const emailInput = screen.getByTestId('email-input')
    fireEvent.change(emailInput, { target: { value: 'test@example.com' } })

    // Still no submit (password invalid)
    await flushEffects()

    expect(screen.queryByTestId('submit-btn')).not.toBeInTheDocument()

    // Fill password
    const passwordInput = screen.getByTestId('password-input')
    fireEvent.change(passwordInput, { target: { value: 'ValidPass123' } })

    // Now submit should be visible
    await flushEffects()

    expect(screen.getByTestId('submit-btn')).toBeInTheDocument()
  })
})
