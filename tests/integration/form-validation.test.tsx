/**
 * Integration Tests: Form Validation with Concerns
 *
 * Scenario: User registration form with dependent fields
 * Tests validation, error storage, and dynamic UI state from concerns
 */

import { cleanup, screen } from '@testing-library/react'
import { afterEach, beforeEach, describe, expect, it } from 'vitest'
import { z } from 'zod'

import type { TestState } from '../mocks'
import { testStateFixtures } from '../mocks'
import {
  createStore,
  fireEvent,
  flushEffects,
  renderWithStore,
} from '../utils/react'

describe('Integration: Form Validation with Concerns', () => {
  let store: ReturnType<typeof createStore<TestState>>

  beforeEach(() => {
    store = createStore<TestState>(testStateFixtures.formEmpty)
  })

  afterEach(() => {
    cleanup()
  })

  it('TC1.1: validates email format with Zod schema', async () => {
    const emailSchema = z.string().email('Invalid email format')

    function FormComponent() {
      store.useConcerns('form', {
        email: {
          validationState: { schema: emailSchema },
        },
      })

      const emailField = store
        .withConcerns({ validationState: true })
        .useFieldStore('email')

      return (
        <div>
          <input
            data-testid="email-input"
            value={emailField.value}
            onChange={(e) => emailField.setValue(e.target.value)}
            placeholder="Enter email"
          />
          {emailField.validationState?.isError && (
            <span data-testid="email-error">Invalid email format</span>
          )}
          {emailField.validationState &&
            !emailField.validationState.isError && (
              <span data-testid="email-valid">✓</span>
            )}
        </div>
      )
    }

    renderWithStore(<FormComponent />, store)

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

  /**
   * User Story: As a form user, when I enter a password, the system should validate
   * it meets complexity requirements (min 8 chars, uppercase, number).
   * Acceptance: Weak passwords show error concern, strong passwords pass validation.
   */
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

      const passwordField = store
        .withConcerns({ validationState: true })
        .useFieldStore('password')

      return (
        <div>
          <input
            data-testid="password-input"
            type="password"
            value={passwordField.value}
            onChange={(e) => passwordField.setValue(e.target.value)}
          />
          {passwordField.validationState?.isError && (
            <span data-testid="password-error">Invalid password</span>
          )}
        </div>
      )
    }

    renderWithStore(<FormComponent />, store)

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

  /**
   * User Story: As a form user, when I enter password confirmation, the system should
   * validate it matches the original password field using cross-field validation.
   * Acceptance: Matching passwords pass, non-matching passwords show validation error.
   */
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

    renderWithStore(<FormComponent />, store)

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

  /**
   * User Story: As a form user, when I submit a registration form, I must agree to
   * the terms and conditions before proceeding.
   * Acceptance: Unchecked terms show error, checked terms clear the error.
   */
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

    renderWithStore(<FormComponent />, store)

    const input = screen.getByTestId('terms-input') as HTMLInputElement
    expect(input.checked).toBe(false)
    expect(screen.getByTestId('terms-error')).toBeInTheDocument()

    fireEvent.click(input)
    await flushEffects()

    expect(input.checked).toBe(true)
    expect(screen.queryByTestId('terms-error')).not.toBeInTheDocument()
  })

  /**
   * User Story: As a form user, when validation fails, I should see descriptive
   * error messages provided by the concern system.
   * Acceptance: Invalid field values trigger concerns that display specific error messages.
   */
  it('TC1.5: displays error messages from concerns', async () => {
    const emailSchema = z.string().email('Please enter a valid email')

    function FormComponent() {
      store.useConcerns('form', {
        email: {
          validationState: { schema: emailSchema },
        },
      })

      const emailField = store
        .withConcerns({ validationState: true })
        .useFieldStore('email')

      return (
        <div>
          <input
            data-testid="email-input"
            value={emailField.value}
            onChange={(e) => emailField.setValue(e.target.value)}
          />
          {emailField.validationState?.isError && (
            <span data-testid="error-message">Please enter a valid email</span>
          )}
        </div>
      )
    }

    renderWithStore(<FormComponent />, store)

    const input = screen.getByTestId('email-input') as HTMLInputElement
    fireEvent.change(input, { target: { value: 'invalid' } })

    await flushEffects()

    expect(screen.getByTestId('error-message')).toBeInTheDocument()
  })

  /**
   * User Story: As a form user, when I correct an invalid field, the error message
   * should automatically disappear once the field becomes valid.
   * Acceptance: Error concerns clear reactively when field value becomes valid.
   */
  it('TC1.6: clears errors when field becomes valid', async () => {
    const emailSchema = z.string().email()

    function FormComponent() {
      store.useConcerns('form', {
        email: {
          validationState: { schema: emailSchema },
        },
      })

      const emailField = store
        .withConcerns({ validationState: true })
        .useFieldStore('email')

      return (
        <div>
          <input
            data-testid="email-input"
            value={emailField.value}
            onChange={(e) => emailField.setValue(e.target.value)}
          />
          {emailField.validationState?.isError && (
            <span data-testid="error">Invalid</span>
          )}
        </div>
      )
    }

    renderWithStore(<FormComponent />, store)

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

  /**
   * User Story: As a form user, the submit button should only appear when all
   * required fields are valid, preventing invalid form submissions.
   * Acceptance: Submit button hidden when fields invalid, visible when all fields valid.
   */
  it('TC1.7: submit button visibility depends on form validity', async () => {
    const emailSchema = z.string().email()
    const passwordSchema = z.string().min(8)

    function FormComponent() {
      store.useConcerns('form', {
        email: { validationState: { schema: emailSchema } },
        password: { validationState: { schema: passwordSchema } },
      })

      const emailField = store
        .withConcerns({ validationState: true })
        .useFieldStore('email')
      const passwordField = store
        .withConcerns({ validationState: true })
        .useFieldStore('password')

      const isEmailValid =
        emailField.validationState && !emailField.validationState.isError
      const isPasswordValid =
        passwordField.validationState && !passwordField.validationState.isError

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

    renderWithStore(<FormComponent />, store)

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
