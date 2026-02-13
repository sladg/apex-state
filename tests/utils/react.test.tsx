/**
 * Tests for React test utilities
 *
 * Verifies that the extracted utilities work correctly.
 */

import { screen } from '@testing-library/react'
import { describe, expect, it } from 'vitest'
import { z } from 'zod'

import type { TestState } from '../mocks'
import { testStateFixtures } from '../mocks'
import {
  createStore,
  createTestStore,
  fireEvent,
  flush,
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
      const store = createStore<TestState>(testStateFixtures.formEmpty)

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

      renderWithStore(<TestComponent />, store)

      const input = screen.getByTestId('email-input') as HTMLInputElement
      expect(input).toBeInTheDocument()
      expect(input.value).toBe('')
    })
  })

  describe('fireEvent', () => {
    it('wraps change events in act()', async () => {
      const store = createStore<TestState>(testStateFixtures.formEmpty)

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

      renderWithStore(<TestComponent />, store)

      const input = screen.getByTestId('email-input') as HTMLInputElement

      fireEvent.change(input, { target: { value: 'test@example.com' } })
      await flush()

      expect(input.value).toBe('test@example.com')
    })

    it('wraps click events in act()', async () => {
      const store = createStore<TestState>(testStateFixtures.formEmpty)

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

      renderWithStore(<TestComponent />, store)

      const checkbox = screen.getByTestId('terms-checkbox') as HTMLInputElement
      expect(checkbox.checked).toBe(false)

      fireEvent.click(checkbox)
      await flush()

      expect(checkbox.checked).toBe(true)
    })
  })

  describe('flush', () => {
    it('flushes async valtio updates', async () => {
      const store = createStore<TestState>(testStateFixtures.formEmpty)

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

      renderWithStore(<TestComponent />, store)

      const input = screen.getByTestId('email-input') as HTMLInputElement
      fireEvent.change(input, { target: { value: 'invalid' } })

      await flush()

      expect(screen.getByTestId('error')).toBeInTheDocument()
    })
  })
})
