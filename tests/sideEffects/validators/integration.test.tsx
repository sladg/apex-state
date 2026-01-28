/**
 * Integration tests for validators side-effect
 *
 * Tests complete validation functionality in a React component context.
 */

import React from 'react'
import { describe, test, expect } from 'vitest'
import { render, waitFor, fireEvent } from '@testing-library/react'
import { z } from 'zod'
import { createGenericStore } from '../../../src/store/createStore'
import { createValidatorsRegistry } from '../../../src/sideEffects/validators/registry'
import { createValidatorsSynchronizer } from '../../../src/pipeline/synchronizers/validators'
import type { GenericMeta } from '../../../src/types'
import type { StoredError } from '../../../src/sideEffects/validators/types'

interface TestState {
  email: string
  password: string
  user: {
    name: string
    age: number
  }
  _errors?: {
    email?: StoredError[]
    password?: StoredError[]
    'user.name'?: StoredError[]
    user?: StoredError[]
  }
}

describe('Validators Integration', () => {
  describe('Manual synchronizer integration', () => {
    test('should validate and add errors', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()
      registry.register({
        id: 'email-validator',
        scope: 'email',
        schema: z.string().email(),
        errorPath: 'email'
      })

      const synchronizer = createValidatorsSynchronizer(registry, '_errors')

      const initialState: TestState = {
        email: 'invalid-email',
        password: '',
        user: { name: '', age: 0 }
      }

      const changes = synchronizer([['email', 'invalid-email', {}]], initialState)

      // Should have original change plus error change
      expect(changes.length).toBe(2)
      expect(changes[1][0]).toBe('_errors.email')

      const errors = changes[1][1] as StoredError[]
      expect(errors).toHaveLength(1)
      expect(errors[0].id).toBe('email-validator')
    })

    test('should handle multiple validators for same path', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()

      registry.register({
        id: 'password-min',
        scope: 'password',
        schema: z.string().min(8),
        errorPath: 'password'
      })

      registry.register({
        id: 'password-pattern',
        scope: 'password',
        schema: z.string().regex(/[A-Z]/),
        errorPath: 'password'
      })

      const synchronizer = createValidatorsSynchronizer(registry, '_errors')

      const initialState: TestState = {
        email: '',
        password: 'short',
        user: { name: '', age: 0 }
      }

      const changes = synchronizer([['password', 'short', {}]], initialState)

      const errorChange = changes.find(c => c[0] === '_errors.password')
      const errors = errorChange?.[1] as StoredError[]

      // Both validators should fail
      expect(errors).toHaveLength(2)
    })

    test('should remove errors when validation passes', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()

      registry.register({
        id: 'email-validator',
        scope: 'email',
        schema: z.string().email(),
        errorPath: 'email'
      })

      const synchronizer = createValidatorsSynchronizer(registry, '_errors')

      const initialState: TestState = {
        email: 'valid@email.com',
        password: '',
        user: { name: '', age: 0 },
        _errors: {
          email: [{ id: 'email-validator', message: 'Invalid email' }]
        }
      }

      const changes = synchronizer([['email', 'valid@email.com', {}]], initialState)

      const errorChange = changes.find(c => c[0] === '_errors.email')
      expect(errorChange?.[1]).toBeUndefined() // Errors cleared
    })
  })

  describe('Store integration', () => {
    test('basic store operations work', async () => {
      const store = createGenericStore<TestState, GenericMeta>()

      function TestComponent() {
        const [email, setEmail] = store.useStore('email')
        const [password] = store.useStore('password')

        return (
          <div>
            <input
              data-testid="email"
              value={email}
              onChange={e => setEmail(e.target.value)}
            />
            <span data-testid="password">{password}</span>
          </div>
        )
      }

      const { getByTestId } = render(
        <store.Provider
          initialState={{
            email: 'test@example.com',
            password: '',
            user: { name: '', age: 0 }
          }}
        >
          <TestComponent />
        </store.Provider>
      )

      expect((getByTestId('email') as HTMLInputElement).value).toBe(
        'test@example.com'
      )

      fireEvent.change(getByTestId('email'), {
        target: { value: 'new@example.com' }
      })

      await waitFor(() => {
        expect((getByTestId('email') as HTMLInputElement).value).toBe(
          'new@example.com'
        )
      })
    })

    test('useSideEffects hook registration works', () => {
      const store = createGenericStore<TestState, GenericMeta>()

      function TestComponent() {
        // Register validators side-effect
        store.useSideEffects('test-validators', {
          validators: {
            validators: [
              {
                id: 'email-validator',
                scope: 'email',
                schema: z.string().email(),
                errorPath: 'email'
              }
            ]
          }
        })

        return <div>Component with validators</div>
      }

      const { container } = render(
        <store.Provider
          initialState={{
            email: '',
            password: '',
            user: { name: '', age: 0 }
          }}
        >
          <TestComponent />
        </store.Provider>
      )

      // Should render without errors
      expect(container).toBeTruthy()
    })
  })

  describe('Registry lifecycle', () => {
    test('should handle dynamic registration', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()

      // Register first validator
      registry.register({
        id: 'email-validator',
        scope: 'email',
        schema: z.string().email(),
        errorPath: 'email'
      })

      expect(registry.getValidator('email-validator')).toBeDefined()

      // Register second validator
      registry.register({
        id: 'password-validator',
        scope: 'password',
        schema: z.string().min(8),
        errorPath: 'password'
      })

      expect(registry.getAllValidators()).toHaveLength(2)

      // Unregister first validator
      registry.unregister('email-validator')

      expect(registry.getValidator('email-validator')).toBeUndefined()
      expect(registry.getAllValidators()).toHaveLength(1)
    })

    test('should handle mount/unmount scenarios', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()

      // Simulate component mount
      registry.register({
        id: 'component-1',
        scope: 'email',
        schema: z.string().email(),
        errorPath: 'email'
      })

      // Simulate another component mount
      registry.register({
        id: 'component-2',
        scope: 'password',
        schema: z.string().min(8),
        errorPath: 'password'
      })

      // Both should be active
      expect(registry.getAllValidators()).toHaveLength(2)

      // Simulate first component unmount
      registry.unregister('component-1')

      // Second should still be active
      expect(registry.getValidator('component-2')).toBeDefined()

      // Simulate second component unmount
      registry.unregister('component-2')

      // All should be cleared
      expect(registry.getAllValidators()).toHaveLength(0)
    })
  })

  describe('Error handling', () => {
    test('should handle invalid data gracefully', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()

      registry.register({
        id: 'user-validator',
        scope: 'user',
        schema: z.object({
          name: z.string(),
          age: z.number()
        }),
        errorPath: 'user'
      })

      const synchronizer = createValidatorsSynchronizer(registry, '_errors')

      const initialState: TestState = {
        email: '',
        password: '',
        user: { name: 'Alice', age: -1 }
      }

      // Should not throw, just add errors
      expect(() => {
        synchronizer([['user.age', -1, {}]], initialState)
      }).not.toThrow()
    })

    test('should handle missing scope data', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()

      registry.register({
        id: 'missing-validator',
        scope: 'nonexistent.path' as any,
        schema: z.string(),
        errorPath: 'nonexistent' as any
      })

      const synchronizer = createValidatorsSynchronizer(registry, '_errors')

      const initialState: TestState = {
        email: '',
        password: '',
        user: { name: '', age: 0 }
      }

      // Should handle gracefully
      expect(() => {
        synchronizer([['email', 'test', {}]], initialState)
      }).not.toThrow()
    })
  })

  describe('Performance scenarios', () => {
    test('should handle rapid validations', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()

      registry.register({
        id: 'email-validator',
        scope: 'email',
        schema: z.string().email(),
        errorPath: 'email'
      })

      const synchronizer = createValidatorsSynchronizer(registry, '_errors')

      const initialState: TestState = {
        email: '',
        password: '',
        user: { name: '', age: 0 }
      }

      // Simulate 100 rapid validations
      const start = performance.now()
      for (let i = 0; i < 100; i++) {
        synchronizer([['email', `test${i}@example.com`, {}]], {
          ...initialState,
          email: `test${i}@example.com`
        })
      }
      const duration = performance.now() - start

      // Should complete quickly (< 50ms for 100 iterations)
      expect(duration).toBeLessThan(50)
    })

    test('should handle multiple validators efficiently', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()

      // Register 10 validators
      for (let i = 0; i < 10; i++) {
        registry.register({
          id: `validator-${i}`,
          scope: 'email',
          schema: z.string().min(i),
          errorPath: 'email'
        })
      }

      const synchronizer = createValidatorsSynchronizer(registry, '_errors')

      const initialState: TestState = {
        email: 'test',
        password: '',
        user: { name: '', age: 0 }
      }

      const start = performance.now()
      synchronizer([['email', 'test', {}]], initialState)
      const duration = performance.now() - start

      // Should complete quickly
      expect(duration).toBeLessThan(10)
    })
  })

  describe('Complex validation scenarios', () => {
    test('should handle nested object validation', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()

      registry.register({
        id: 'user-validator',
        scope: 'user',
        schema: z.object({
          name: z.string().min(2),
          age: z.number().min(0).max(150)
        }),
        errorPath: 'user'
      })

      const synchronizer = createValidatorsSynchronizer(registry, '_errors')

      const initialState: TestState = {
        email: '',
        password: '',
        user: { name: 'A', age: -5 }
      }

      const changes = synchronizer([['user.name', 'A', {}]], initialState)

      const errorChange = changes.find(c => c[0] === '_errors.user')
      expect(errorChange).toBeDefined()

      const errors = errorChange?.[1] as StoredError[]
      expect(errors.length).toBeGreaterThan(0)
    })

    test('should support conditional validation', () => {
      const registry = createValidatorsRegistry<TestState, GenericMeta>()

      registry.register({
        id: 'password-required',
        scope: 'password',
        schema: z.string().min(1, 'Password is required'),
        errorPath: 'password'
      })

      const synchronizer = createValidatorsSynchronizer(registry, '_errors')

      // Empty password should fail
      const failState: TestState = {
        email: '',
        password: '',
        user: { name: '', age: 0 }
      }

      const failChanges = synchronizer([['password', '', {}]], failState)
      const failError = failChanges.find(c => c[0] === '_errors.password')
      expect(failError?.[1]).toBeDefined()

      // Non-empty password should pass
      const passState: TestState = {
        email: '',
        password: 'secret123',
        user: { name: '', age: 0 },
        _errors: {
          password: [
            { id: 'password-required', message: 'Password is required' }
          ]
        }
      }

      const passChanges = synchronizer([['password', 'secret123', {}]], passState)
      const passError = passChanges.find(c => c[0] === '_errors.password')
      expect(passError?.[1]).toBeUndefined()
    })
  })
})
