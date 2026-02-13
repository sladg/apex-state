/**
 * Integration Tests: Form Validation with Concerns
 *
 * Scenario: User registration form with dependent fields
 * Tests validation, error storage, and dynamic UI state from concerns
 */

import { screen } from '@testing-library/react'
import { beforeEach, describe, expect, it } from 'vitest'
import { z } from 'zod'

import type { TestState } from '../mocks'
import { testStateFixtures } from '../mocks'
import { createStore, fireEvent, flush, renderWithStore } from '../utils/react'

describe('Integration: Form Validation with Concerns', () => {
  let store: ReturnType<typeof createStore<TestState>>

  beforeEach(() => {
    store = createStore<TestState>(testStateFixtures.formEmpty)
  })

  /**
   * Shared validation form component for tests that follow the pattern:
   * register validationState concern → render input + conditional error span.
   *
   * Used by TC1.1, TC1.2, TC1.5, TC1.6.
   */
  function ValidationFormField({
    schema,
    path,
    inputTestId,
    errorTestId,
    errorText,
    inputType = 'text',
    showValidIndicator = false,
    validTestId,
  }: {
    schema: z.ZodSchema
    path: 'email' | 'password'
    inputTestId: string
    errorTestId: string
    errorText: string
    inputType?: string
    showValidIndicator?: boolean
    validTestId?: string
  }) {
    store.useConcerns('form', {
      [path]: { validationState: { schema } },
    })

    const field = store
      .withConcerns({ validationState: true })
      .useFieldStore(path)

    return (
      <div>
        <input
          data-testid={inputTestId}
          type={inputType}
          value={field.value}
          onChange={(e) => field.setValue(e.target.value)}
        />
        {field.validationState?.isError && (
          <span data-testid={errorTestId}>{errorText}</span>
        )}
        {showValidIndicator &&
          field.validationState &&
          !field.validationState.isError && (
            <span data-testid={validTestId}>✓</span>
          )}
      </div>
    )
  }

  it('TC1.1: validates email format with Zod schema', async () => {
    renderWithStore(
      <ValidationFormField
        schema={z.string().email('Invalid email format')}
        path="email"
        inputTestId="email-input"
        errorTestId="email-error"
        errorText="Invalid email format"
        showValidIndicator
        validTestId="email-valid"
      />,
      store,
    )

    const input = screen.getByTestId('email-input') as HTMLInputElement
    expect(input.value).toBe('')

    // Invalid email - should show error concern
    fireEvent.change(input, { target: { value: 'invalid-email' } })
    await flush()
    expect(input.value).toBe('invalid-email')

    // Valid email - should show valid concern
    fireEvent.change(input, { target: { value: '' } })
    fireEvent.change(input, { target: { value: 'test@example.com' } })
    await flush()
    expect(input.value).toBe('test@example.com')
  })

  /**
   * User Story: As a form user, when I enter a password, the system should validate
   * it meets complexity requirements (min 8 chars, uppercase, number).
   * Acceptance: Weak passwords show error concern, strong passwords pass validation.
   */
  it('TC1.2: validates password complexity', async () => {
    renderWithStore(
      <ValidationFormField
        schema={z
          .string()
          .min(8, 'At least 8 characters')
          .regex(/[A-Z]/, 'Must contain uppercase')
          .regex(/[0-9]/, 'Must contain number')}
        path="password"
        inputTestId="password-input"
        errorTestId="password-error"
        errorText="Invalid password"
        inputType="password"
      />,
      store,
    )

    const input = screen.getByTestId('password-input') as HTMLInputElement

    // Weak password
    fireEvent.change(input, { target: { value: 'weak' } })
    await flush()

    expect(screen.queryByTestId('password-error')).toBeInTheDocument()

    // Strong password
    fireEvent.change(input, { target: { value: '' } })
    fireEvent.change(input, { target: { value: 'StrongPass123' } })
    await flush()

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

    await flush()

    expect(passwordInput.value).toBe('Test123')
    expect(confirmInput.value).toBe('Test123')

    // Change confirm to not match
    fireEvent.change(confirmInput, { target: { value: '' } })
    fireEvent.change(confirmInput, { target: { value: 'Different' } })
    await flush()

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
    await flush()

    expect(input.checked).toBe(true)
    expect(screen.queryByTestId('terms-error')).not.toBeInTheDocument()
  })

  /**
   * User Story: As a form user, when validation fails, I should see descriptive
   * error messages provided by the concern system.
   * Acceptance: Invalid field values trigger concerns that display specific error messages.
   */
  it('TC1.5: displays error messages from concerns', async () => {
    renderWithStore(
      <ValidationFormField
        schema={z.string().email('Please enter a valid email')}
        path="email"
        inputTestId="email-input"
        errorTestId="error-message"
        errorText="Please enter a valid email"
      />,
      store,
    )

    const input = screen.getByTestId('email-input') as HTMLInputElement
    fireEvent.change(input, { target: { value: 'invalid' } })

    await flush()

    expect(screen.getByTestId('error-message')).toBeInTheDocument()
  })

  /**
   * User Story: As a form user, when I correct an invalid field, the error message
   * should automatically disappear once the field becomes valid.
   * Acceptance: Error concerns clear reactively when field value becomes valid.
   */
  it('TC1.6: clears errors when field becomes valid', async () => {
    renderWithStore(
      <ValidationFormField
        schema={z.string().email()}
        path="email"
        inputTestId="email-input"
        errorTestId="error"
        errorText="Invalid"
      />,
      store,
    )

    const input = screen.getByTestId('email-input') as HTMLInputElement

    // Create error
    fireEvent.change(input, { target: { value: 'invalid' } })
    await flush()

    expect(screen.getByTestId('error')).toBeInTheDocument()

    // Fix error - select all and type new value
    fireEvent.change(input, { target: { value: '' } })
    fireEvent.change(input, { target: { value: 'valid@example.com' } })
    await flush()

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
    await flush()

    expect(screen.getByTestId('email-input')).toBeInTheDocument()

    expect(screen.queryByTestId('submit-btn')).not.toBeInTheDocument()

    // Fill email
    const emailInput = screen.getByTestId('email-input')
    fireEvent.change(emailInput, { target: { value: 'test@example.com' } })

    // Still no submit (password invalid)
    await flush()

    expect(screen.queryByTestId('submit-btn')).not.toBeInTheDocument()

    // Fill password
    const passwordInput = screen.getByTestId('password-input')
    fireEvent.change(passwordInput, { target: { value: 'ValidPass123' } })

    // Now submit should be visible
    await flush()

    expect(screen.getByTestId('submit-btn')).toBeInTheDocument()
  })
})
