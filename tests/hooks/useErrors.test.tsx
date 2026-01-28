/**
 * Tests for useErrors hook
 *
 * Validates reactive error retrieval from state.
 */

import React from 'react'
import { describe, test, expect } from 'vitest'
import { render, waitFor, fireEvent } from '@testing-library/react'
import { z } from 'zod'
import { createGenericStore } from '../../src/store/createStore'
import { useErrors } from '../../src/hooks/useErrors'
import type { GenericMeta } from '../../src/types'
import type { StoredError } from '../../src/sideEffects/validators/types'

interface TestState {
  email: string
  password: string
  _errors?: {
    email?: StoredError[]
    password?: StoredError[]
  }
}

describe('useErrors', () => {
  describe('Basic functionality', () => {
    test('should return empty array when no errors', () => {
      const store = createGenericStore<TestState, GenericMeta>()

      function TestComponent() {
        const errors = useErrors<TestState>('email')

        return <div data-testid="error-count">{errors.length}</div>
      }

      const { getByTestId } = render(
        <store.Provider initialState={{ email: '', password: '' }}>
          <TestComponent />
        </store.Provider>
      )

      expect(getByTestId('error-count').textContent).toBe('0')
    })

    test('should return error messages when errors exist', () => {
      const store = createGenericStore<TestState, GenericMeta>()

      function TestComponent() {
        const errors = useErrors<TestState>('email')

        return (
          <div>
            <div data-testid="error-count">{errors.length}</div>
            {errors.map((err, i) => (
              <div key={i} data-testid={`error-${i}`}>
                {err}
              </div>
            ))}
          </div>
        )
      }

      const { getByTestId } = render(
        <store.Provider
          initialState={{
            email: '',
            password: '',
            _errors: {
              email: [
                { id: 'validator-1', message: 'Invalid email format' }
              ]
            }
          }}
        >
          <TestComponent />
        </store.Provider>
      )

      expect(getByTestId('error-count').textContent).toBe('1')
      expect(getByTestId('error-0').textContent).toBe('Invalid email format')
    })

    test('should return multiple error messages', () => {
      const store = createGenericStore<TestState, GenericMeta>()

      function TestComponent() {
        const errors = useErrors<TestState>('password')

        return (
          <div>
            <div data-testid="error-count">{errors.length}</div>
            {errors.map((err, i) => (
              <div key={i} data-testid={`error-${i}`}>
                {err}
              </div>
            ))}
          </div>
        )
      }

      const { getByTestId } = render(
        <store.Provider
          initialState={{
            email: '',
            password: '',
            _errors: {
              password: [
                { id: 'validator-1', message: 'Too short' },
                { id: 'validator-2', message: 'Missing uppercase' }
              ]
            }
          }}
        >
          <TestComponent />
        </store.Provider>
      )

      expect(getByTestId('error-count').textContent).toBe('2')
      expect(getByTestId('error-0').textContent).toBe('Too short')
      expect(getByTestId('error-1').textContent).toBe('Missing uppercase')
    })
  })

  describe('Reactivity', () => {
    test('should re-render when errors change', async () => {
      const store = createGenericStore<TestState, GenericMeta>()

      function TestComponent() {
        const [email, setEmail] = store.useStore('email')
        const [errors, setErrors] = store.useStore('_errors.email')
        const errorMessages = useErrors<TestState>('email')

        return (
          <div>
            <div data-testid="error-count">{errorMessages.length}</div>
            <button
              onClick={() =>
                setErrors([
                  { id: 'test', message: 'New error' }
                ] as StoredError[])
              }
            >
              Add Error
            </button>
            <button onClick={() => setErrors(undefined)}>Clear Errors</button>
          </div>
        )
      }

      const { getByTestId, getByText } = render(
        <store.Provider initialState={{ email: '', password: '' }}>
          <TestComponent />
        </store.Provider>
      )

      // Initially no errors
      expect(getByTestId('error-count').textContent).toBe('0')

      // Add error
      fireEvent.click(getByText('Add Error'))

      await waitFor(() => {
        expect(getByTestId('error-count').textContent).toBe('1')
      })

      // Clear errors
      fireEvent.click(getByText('Clear Errors'))

      await waitFor(() => {
        expect(getByTestId('error-count').textContent).toBe('0')
      })
    })

    test('should update when individual errors are added/removed', async () => {
      const store = createGenericStore<TestState, GenericMeta>()

      function TestComponent() {
        const [errors, setErrors] = store.useStore('_errors.password')
        const errorMessages = useErrors<TestState>('password')

        const addError = () => {
          const current = (errors as StoredError[]) || []
          setErrors([
            ...current,
            { id: `error-${current.length}`, message: `Error ${current.length + 1}` }
          ] as StoredError[])
        }

        const removeError = () => {
          const current = (errors as StoredError[]) || []
          setErrors(current.slice(0, -1) as StoredError[])
        }

        return (
          <div>
            <div data-testid="error-count">{errorMessages.length}</div>
            <button onClick={addError}>Add Error</button>
            <button onClick={removeError}>Remove Error</button>
          </div>
        )
      }

      const { getByTestId, getByText } = render(
        <store.Provider initialState={{ email: '', password: '' }}>
          <TestComponent />
        </store.Provider>
      )

      expect(getByTestId('error-count').textContent).toBe('0')

      // Add first error
      fireEvent.click(getByText('Add Error'))
      await waitFor(() => {
        expect(getByTestId('error-count').textContent).toBe('1')
      })

      // Add second error
      fireEvent.click(getByText('Add Error'))
      await waitFor(() => {
        expect(getByTestId('error-count').textContent).toBe('2')
      })

      // Remove one error
      fireEvent.click(getByText('Remove Error'))
      await waitFor(() => {
        expect(getByTestId('error-count').textContent).toBe('1')
      })

      // Remove last error
      fireEvent.click(getByText('Remove Error'))
      await waitFor(() => {
        expect(getByTestId('error-count').textContent).toBe('0')
      })
    })
  })

  describe('Custom error store path', () => {
    test('should support custom error store path', () => {
      interface CustomState {
        email: string
        validationErrors?: {
          email?: StoredError[]
        }
      }

      const store = createGenericStore<CustomState, GenericMeta>()

      function TestComponent() {
        const errors = useErrors<CustomState>('email', 'validationErrors')

        return <div data-testid="error-count">{errors.length}</div>
      }

      const { getByTestId } = render(
        <store.Provider
          initialState={{
            email: '',
            validationErrors: {
              email: [{ id: 'test', message: 'Custom error' }]
            }
          }}
        >
          <TestComponent />
        </store.Provider>
      )

      expect(getByTestId('error-count').textContent).toBe('1')
    })
  })

  describe('Edge cases', () => {
    test('should handle undefined errors', () => {
      const store = createGenericStore<TestState, GenericMeta>()

      function TestComponent() {
        const errors = useErrors<TestState>('email')

        return <div data-testid="error-count">{errors.length}</div>
      }

      const { getByTestId } = render(
        <store.Provider
          initialState={{
            email: '',
            password: '',
            _errors: undefined
          }}
        >
          <TestComponent />
        </store.Provider>
      )

      expect(getByTestId('error-count').textContent).toBe('0')
    })

    test('should handle non-array errors gracefully', () => {
      const store = createGenericStore<TestState, GenericMeta>()

      function TestComponent() {
        const errors = useErrors<TestState>('email')

        return <div data-testid="error-count">{errors.length}</div>
      }

      const { getByTestId } = render(
        <store.Provider
          initialState={{
            email: '',
            password: '',
            _errors: {
              email: 'not an array' as any
            }
          }}
        >
          <TestComponent />
        </store.Provider>
      )

      // Should return empty array, not crash
      expect(getByTestId('error-count').textContent).toBe('0')
    })

    test('should handle empty errors array', () => {
      const store = createGenericStore<TestState, GenericMeta>()

      function TestComponent() {
        const errors = useErrors<TestState>('email')

        return <div data-testid="error-count">{errors.length}</div>
      }

      const { getByTestId } = render(
        <store.Provider
          initialState={{
            email: '',
            password: '',
            _errors: {
              email: []
            }
          }}
        >
          <TestComponent />
        </store.Provider>
      )

      expect(getByTestId('error-count').textContent).toBe('0')
    })
  })

  describe('Multiple components', () => {
    test('should support multiple components using same errors', () => {
      const store = createGenericStore<TestState, GenericMeta>()

      function ErrorDisplay({ path }: { path: string }) {
        const errors = useErrors<TestState>(path as any)

        return <div data-testid={`errors-${path}`}>{errors.length}</div>
      }

      const { getByTestId } = render(
        <store.Provider
          initialState={{
            email: '',
            password: '',
            _errors: {
              email: [{ id: 'test', message: 'Email error' }],
              password: [
                { id: 'test1', message: 'Password error 1' },
                { id: 'test2', message: 'Password error 2' }
              ]
            }
          }}
        >
          <ErrorDisplay path="email" />
          <ErrorDisplay path="password" />
        </store.Provider>
      )

      expect(getByTestId('errors-email').textContent).toBe('1')
      expect(getByTestId('errors-password').textContent).toBe('2')
    })

    test('should support independent error updates', async () => {
      const store = createGenericStore<TestState, GenericMeta>()

      function EmailErrors() {
        const [, setErrors] = store.useStore('_errors.email')
        const errors = useErrors<TestState>('email')

        return (
          <div>
            <div data-testid="email-errors">{errors.length}</div>
            <button
              onClick={() =>
                setErrors([
                  { id: 'test', message: 'Email error' }
                ] as StoredError[])
              }
            >
              Add Email Error
            </button>
          </div>
        )
      }

      function PasswordErrors() {
        const [, setErrors] = store.useStore('_errors.password')
        const errors = useErrors<TestState>('password')

        return (
          <div>
            <div data-testid="password-errors">{errors.length}</div>
            <button
              onClick={() =>
                setErrors([
                  { id: 'test', message: 'Password error' }
                ] as StoredError[])
              }
            >
              Add Password Error
            </button>
          </div>
        )
      }

      const { getByTestId, getByText } = render(
        <store.Provider initialState={{ email: '', password: '' }}>
          <EmailErrors />
          <PasswordErrors />
        </store.Provider>
      )

      expect(getByTestId('email-errors').textContent).toBe('0')
      expect(getByTestId('password-errors').textContent).toBe('0')

      // Add email error
      fireEvent.click(getByText('Add Email Error'))
      await waitFor(() => {
        expect(getByTestId('email-errors').textContent).toBe('1')
        expect(getByTestId('password-errors').textContent).toBe('0')
      })

      // Add password error
      fireEvent.click(getByText('Add Password Error'))
      await waitFor(() => {
        expect(getByTestId('email-errors').textContent).toBe('1')
        expect(getByTestId('password-errors').textContent).toBe('1')
      })
    })
  })
})
