/**
 * Integration Tests: Error Handling & Recovery
 *
 * Scenario: Form with validation errors and recovery
 * Tests error storage, clearing, field-level vs form-level errors
 */

import { screen } from '@testing-library/react'
import { beforeEach, describe, expect, it } from 'vitest'

import type { TestState } from '../mocks'
import { testStateFixtures } from '../mocks'
import {
  EmailTemplateForm,
  EmailValidationForm,
  MultiFieldForm,
} from '../utils/components'
import { createStore, fireEvent, flush, renderWithStore } from '../utils/react'

describe('Integration: Error Handling & Recovery', () => {
  let store: ReturnType<typeof createStore<TestState>>

  beforeEach(() => {
    store = createStore<TestState>(testStateFixtures.formEmpty)
  })

  it('TC7.1: stores validation errors in _errors field', async () => {
    renderWithStore(
      <EmailValidationForm
        store={store}
        errorMessage="Please enter a valid email address"
      />,
      store,
    )

    const input = screen.getByTestId('email-input')
    fireEvent.change(input, { target: { value: 'invalid-email' } })

    await flush()

    expect(screen.getByTestId('error-count')).toHaveTextContent('1')
    expect(screen.getByTestId('email-error')).toBeInTheDocument()
  })

  it('TC7.2: displays errors from concerns', async () => {
    function FormComponent() {
      store.useConcerns('form', {})

      return (
        <EmailValidationForm
          store={store}
          errorMessage="Invalid email format"
          errorTestId="error-display"
        />
      )
    }

    renderWithStore(<FormComponent />, store)

    const input = screen.getByTestId('email-input')
    fireEvent.change(input, { target: { value: 'bad-email' } })

    await flush()

    expect(screen.getByTestId('error-display')).toBeInTheDocument()
    expect(screen.getByTestId('error-display')).toHaveTextContent(
      'Invalid email format',
    )
  })

  it('TC7.3: clears errors when field becomes valid', async () => {
    renderWithStore(<EmailValidationForm store={store} />, store)

    const input = screen.getByTestId('email-input')

    // Create error
    fireEvent.change(input, { target: { value: 'bad' } })

    await flush()

    expect(screen.getByTestId('email-error')).toBeInTheDocument()

    // Fix error
    fireEvent.change(input, { target: { value: 'valid@example.com' } })

    await flush()

    expect(screen.queryByTestId('email-error')).not.toBeInTheDocument()
  })

  it('TC7.4: clears all errors on form reset', async () => {
    renderWithStore(
      <MultiFieldForm store={store} showResetButton validateOnBlur />,
      store,
    )

    // Create errors
    const emailInput = screen.getByTestId('email-input')
    const passwordInput = screen.getByTestId('password-input')

    fireEvent.change(emailInput, { target: { value: 'bad' } })
    fireEvent.blur(emailInput)
    await flush()

    fireEvent.change(passwordInput, { target: { value: 'short' } })
    fireEvent.blur(passwordInput)
    await flush()

    expect(screen.getByTestId('error-count')).toHaveTextContent('2')

    // Reset
    const resetBtn = screen.getByTestId('reset-btn')
    fireEvent.click(resetBtn)

    await flush()

    expect(screen.getByTestId('error-count')).toHaveTextContent('0')
  })

  it('TC7.5: disables submit button when errors exist', async () => {
    renderWithStore(
      <EmailValidationForm store={store} showSubmitButton />,
      store,
    )

    await flush()

    const submitBtn = screen.getByTestId('submit-btn') as HTMLButtonElement
    expect(submitBtn.disabled).toBe(false)

    // Create error
    const input = screen.getByTestId('email-input')
    fireEvent.change(input, { target: { value: 'invalid' } })

    await flush()

    expect(submitBtn.disabled).toBe(true)

    // Fix error
    fireEvent.change(input, { target: { value: 'valid@example.com' } })

    await flush()

    expect(submitBtn.disabled).toBe(false)
  })

  it('TC7.6: supports error message templates', async () => {
    renderWithStore(<EmailTemplateForm store={store} />, store)

    const input = screen.getByTestId('email-input')
    fireEvent.change(input, { target: { value: 'invalid-email' } })

    await flush()

    expect(screen.getByTestId('error-message')).toHaveTextContent(
      '"invalid-email" is not a valid email address',
    )
  })

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

          {errorsField.value['email'] && (
            <span data-testid="field-error-email">
              {errorsField.value['email'][0]}
            </span>
          )}
          {errorsField.value['password'] && (
            <span data-testid="field-error-password">
              {errorsField.value['password'][0]}
            </span>
          )}
          {errorsField.value['_form'] && (
            <span data-testid="form-error">
              {errorsField.value['_form'][0]}
            </span>
          )}
        </div>
      )
    }

    renderWithStore(<FormComponent />, store)

    const validateBtn = screen.getByTestId('validate-btn')
    fireEvent.click(validateBtn)

    await flush()

    expect(screen.getByTestId('field-error-email')).toBeInTheDocument()
    expect(screen.getByTestId('field-error-password')).toBeInTheDocument()

    // Fill fields
    const emailInput = screen.getByTestId('email-input')
    const passwordInput = screen.getByTestId('password-input')

    fireEvent.change(emailInput, { target: { value: 'test@example.com' } })
    await flush()

    fireEvent.change(passwordInput, { target: { value: 'admin' } })
    await flush()

    fireEvent.click(validateBtn)

    await flush()

    expect(screen.queryByTestId('field-error-email')).not.toBeInTheDocument()
    expect(screen.queryByTestId('field-error-password')).not.toBeInTheDocument()
    expect(screen.getByTestId('form-error')).toBeInTheDocument()
  })

  it('TC7.8: preserves errors when other fields are updated', async () => {
    renderWithStore(
      <MultiFieldForm store={store} validateEmailOnChange />,
      store,
    )

    // Create email error
    const emailInput = screen.getByTestId('email-input')
    fireEvent.change(emailInput, { target: { value: 'bad' } })

    await flush()

    expect(screen.getByTestId('email-error')).toBeInTheDocument()
    expect(screen.getByTestId('error-count')).toHaveTextContent('1')

    // Update password (should not clear email error)
    const passwordInput = screen.getByTestId('password-input')
    fireEvent.change(passwordInput, { target: { value: 'some-password' } })

    // Email error should still exist
    await flush()

    expect(screen.getByTestId('email-error')).toBeInTheDocument()
    expect(screen.getByTestId('error-count')).toHaveTextContent('1')
  })
})
