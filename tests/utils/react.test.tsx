/**
 * Tests for React test utilities
 *
 * Verifies that the extracted utilities work correctly.
 */

import { screen } from '@testing-library/react'
import { describe, expect, it } from 'vitest'
import { z } from 'zod'

import { createGenericStore } from '../../src'
import type { RegistrationForm } from '../mocks'
import { registrationFormFixtures } from '../mocks'
import { fireEvent, flushEffects, MODES, mountStore } from './react'

const createRegistrationFormStore = () => createGenericStore<RegistrationForm>()

describe('React Test Utilities', () => {
  describe('MODES', () => {
    it('exports Legacy and WASM configurations', () => {
      expect(MODES).toHaveLength(2)
      expect(MODES[0]!.name).toBe('Legacy')
      expect(MODES[0]!.config.useLegacyImplementation).toBe(true)
      expect(MODES[1]!.name).toBe('WASM')
      expect(MODES[1]!.config.useLegacyImplementation).toBe(false)
    })
  })

  describe('mountStore', () => {
    it('renders component with store Provider (old API)', async () => {
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

      mountStore(<TestComponent />, store, registrationFormFixtures.empty)

      const input = screen.getByTestId('email-input') as HTMLInputElement
      expect(input).toBeInTheDocument()
      expect(input.value).toBe('')
    })

    it('renders store without component (new API)', async () => {
      const store = createRegistrationFormStore()

      const { storeInstance } = mountStore(
        store,
        registrationFormFixtures.empty,
      )

      expect(storeInstance).toBeDefined()
      expect(storeInstance.state.email).toBe('')
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

      mountStore(<TestComponent />, store, registrationFormFixtures.empty)

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

      mountStore(<TestComponent />, store, registrationFormFixtures.empty)

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

      mountStore(<TestComponent />, store, registrationFormFixtures.empty)

      const input = screen.getByTestId('email-input') as HTMLInputElement
      fireEvent.change(input, { target: { value: 'invalid' } })

      await flushEffects()

      expect(screen.getByTestId('error')).toBeInTheDocument()
    })
  })
})
