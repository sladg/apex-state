/**
 * Tests for React test utilities
 *
 * Verifies that the extracted utilities work correctly.
 */

import { screen } from '@testing-library/react'
import { describe, expect, it } from 'vitest'
import { z } from 'zod'

import { createRegistrationFormStore } from '../mocks'
import { registrationFormFixtures } from '../mocks/fixtures'
import {
  assertions,
  createTestStore,
  domHelpers,
  fireEvent,
  flushEffects,
  renderWithStore,
} from './react'

describe('React Test Utilities', () => {
  describe('createTestStore', () => {
    it('creates a minimal test store with proxy', () => {
      const store = createTestStore({ count: 0 })

      expect(store.proxy).toBeDefined()
      expect(store.proxy.count).toBe(0)
    })

    it('provides useConcerns function', () => {
      const store = createTestStore({ count: 0 })

      expect(store.useConcerns).toBeDefined()
      expect(typeof store.useConcerns).toBe('function')
    })

    it('provides getFieldConcerns function', () => {
      const store = createTestStore({ count: 0 })

      expect(store.getFieldConcerns).toBeDefined()
      expect(typeof store.getFieldConcerns).toBe('function')
    })
  })

  describe('renderWithStore', () => {
    it('renders component with store Provider', async () => {
      const store = createRegistrationFormStore()

      const TestComponent = () => {
        const emailField = store.useFieldStore('email')
        return (
          <input
            data-testid="email-input"
            value={emailField.value}
            onChange={(e) => emailField.setValue(e.target.value)}
          />
        )
      }

      renderWithStore(<TestComponent />, store, registrationFormFixtures.empty)

      const input = screen.getByTestId('email-input') as HTMLInputElement
      expect(input).toBeInTheDocument()
      expect(input.value).toBe('')
    })
  })

  describe('fireEvent', () => {
    it('wraps change events in act()', async () => {
      const store = createRegistrationFormStore()

      const TestComponent = () => {
        const emailField = store.useFieldStore('email')
        return (
          <input
            data-testid="email-input"
            value={emailField.value}
            onChange={(e) => emailField.setValue(e.target.value)}
          />
        )
      }

      renderWithStore(<TestComponent />, store, registrationFormFixtures.empty)

      const input = screen.getByTestId('email-input') as HTMLInputElement

      fireEvent.change(input, { target: { value: 'test@example.com' } })
      await flushEffects()

      expect(input.value).toBe('test@example.com')
    })

    it('wraps click events in act()', async () => {
      const store = createRegistrationFormStore()

      const TestComponent = () => {
        const termsField = store.useFieldStore('agreeToTerms')
        return (
          <input
            data-testid="terms-checkbox"
            type="checkbox"
            checked={termsField.value}
            onChange={(e) => termsField.setValue(e.target.checked)}
          />
        )
      }

      renderWithStore(<TestComponent />, store, registrationFormFixtures.empty)

      const checkbox = screen.getByTestId('terms-checkbox') as HTMLInputElement
      expect(checkbox.checked).toBe(false)

      fireEvent.click(checkbox)
      await flushEffects()

      expect(checkbox.checked).toBe(true)
    })
  })

  describe('flushEffects', () => {
    it('flushes async valtio updates', async () => {
      const store = createRegistrationFormStore()

      const TestComponent = () => {
        store.useConcerns('test', {
          email: {
            validationState: { schema: z.string().email() },
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

      renderWithStore(<TestComponent />, store, registrationFormFixtures.empty)

      const input = screen.getByTestId('email-input') as HTMLInputElement
      fireEvent.change(input, { target: { value: 'invalid' } })

      await flushEffects()

      expect(screen.getByTestId('error')).toBeInTheDocument()
    })
  })

  describe('assertions', () => {
    it('fieldValue checks input value', () => {
      const input = document.createElement('input')
      input.value = 'test'

      expect(assertions.fieldValue(input, 'test')).toBe(true)
      expect(assertions.fieldValue(input, 'other')).toBe(false)
    })

    it('checkboxState checks checkbox', () => {
      const checkbox = document.createElement('input')
      checkbox.type = 'checkbox'
      checkbox.checked = true

      expect(assertions.checkboxState(checkbox, true)).toBe(true)
      expect(assertions.checkboxState(checkbox, false)).toBe(false)
    })

    it('isVisible checks element exists', () => {
      const element = document.createElement('div')

      expect(assertions.isVisible(element)).toBe(true)
      expect(assertions.isVisible(null)).toBe(false)
    })

    it('isHidden checks element is null', () => {
      const element = document.createElement('div')

      expect(assertions.isHidden(null)).toBe(true)
      expect(assertions.isHidden(element)).toBe(false)
    })

    it('isDisabled checks disabled state', () => {
      const button = document.createElement('button')

      button.disabled = false
      expect(assertions.isDisabled(button)).toBe(false)

      button.disabled = true
      expect(assertions.isDisabled(button)).toBe(true)
    })

    it('isEnabled checks enabled state', () => {
      const button = document.createElement('button')

      button.disabled = false
      expect(assertions.isEnabled(button)).toBe(true)

      button.disabled = true
      expect(assertions.isEnabled(button)).toBe(false)
    })

    it('isReadOnly checks readonly state', () => {
      const input = document.createElement('input')

      input.readOnly = false
      expect(assertions.isReadOnly(input)).toBe(false)

      input.readOnly = true
      expect(assertions.isReadOnly(input)).toBe(true)
    })
  })

  describe('domHelpers', () => {
    it('getAllErrors finds error elements', () => {
      const container = document.createElement('div')
      const error1 = document.createElement('span')
      error1.setAttribute('data-testid', 'email-error')
      error1.textContent = 'Invalid email'

      const error2 = document.createElement('span')
      error2.setAttribute('data-testid', 'password-error')
      error2.textContent = 'Invalid password'

      container.appendChild(error1)
      container.appendChild(error2)

      const errors = domHelpers.getAllErrors(container)
      expect(errors).toHaveLength(2)
      expect(errors).toContain('Invalid email')
      expect(errors).toContain('Invalid password')
    })

    it('hasErrors checks if errors exist', () => {
      const container = document.createElement('div')
      expect(domHelpers.hasErrors(container)).toBe(false)

      const error = document.createElement('span')
      error.setAttribute('data-testid', 'email-error')
      container.appendChild(error)

      expect(domHelpers.hasErrors(container)).toBe(true)
    })

    it('getErrorCount returns error count', () => {
      const container = document.createElement('div')
      expect(domHelpers.getErrorCount(container)).toBe(0)

      const error1 = document.createElement('span')
      error1.setAttribute('data-testid', 'email-error')
      container.appendChild(error1)

      expect(domHelpers.getErrorCount(container)).toBe(1)

      const error2 = document.createElement('span')
      error2.setAttribute('data-testid', 'password-error')
      container.appendChild(error2)

      expect(domHelpers.getErrorCount(container)).toBe(2)
    })

    it('getField retrieves input by testid', () => {
      const container = document.createElement('div')
      const input = document.createElement('input')
      input.setAttribute('data-testid', 'email-input')
      container.appendChild(input)

      document.body.appendChild(container)

      const found = domHelpers.getField('email-input')
      expect(found).toBe(input)

      document.body.removeChild(container)
    })

    it('getButton retrieves button by testid', () => {
      const container = document.createElement('div')
      const button = document.createElement('button')
      button.setAttribute('data-testid', 'submit-button')
      container.appendChild(button)

      document.body.appendChild(container)

      const found = domHelpers.getButton('submit-button')
      expect(found).toBe(button)

      document.body.removeChild(container)
    })
  })
})
