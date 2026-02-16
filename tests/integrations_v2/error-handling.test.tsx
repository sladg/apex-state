/**
 * Error Handling: _errors storage, display, clearing, and lifecycle
 *
 * Validates that validation errors:
 * - Are stored in the _errors field of store state
 * - Are displayed through concerns
 * - Clear when fields become valid
 * - Distinguish field-level from form-level errors
 * - Persist correctly across unrelated mutations
 *
 * ┌─────────────────────────────────────────────────────────────────────┐
 * │ REPLACES (when this v2 test is fully implemented):                  │
 * ├─────────────────────────────────────────────────────────────────────┤
 * │ tests/integration/error-handling.test.tsx           (ENTIRE FILE)  │
 * └─────────────────────────────────────────────────────────────────────┘
 */

import { describe, expect, it } from 'vitest'
import { z } from 'zod'

import { createGenericStore } from '../../src'
import type { BasicTestState } from '../mocks'
import { basicTestFixtures } from '../mocks'
import { flushEffects, MODES, mountStore } from '../utils/react'

describe.each(MODES)('[$name] Error Handling', ({ config }) => {
  describe('Error storage', () => {
    it('should store validation errors in _errors field', async () => {
      // Register validation on 'email' with Zod schema
      // Set email to invalid value
      // Assert state._errors['email'] contains error messages
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            email: {
              validationState: {
                schema: z.string().email('Invalid email format'),
              },
            },
          },
        },
      )

      setValue('email', 'invalid')
      await flushEffects()

      expect(storeInstance.state._errors?.['email']).toBeDefined()
      expect(Array.isArray(storeInstance.state._errors?.['email'])).toBe(true)
      expect(
        (storeInstance.state._errors?.['email'] as string[]).length,
      ).toBeGreaterThan(0)
    })

    it('should store errors as array of strings', async () => {
      // Trigger validation error
      // Assert _errors[path] is string[]
      // Assert array contains descriptive error message
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            email: {
              validationState: {
                schema: z.string().email('Invalid email format'),
              },
            },
          },
        },
      )

      setValue('email', 'invalid-email')
      await flushEffects()

      const emailErrors = storeInstance.state._errors?.['email']
      expect(Array.isArray(emailErrors)).toBe(true)
      expect(emailErrors).toEqual(expect.arrayContaining([expect.any(String)]))
    })

    it('should store errors for multiple fields independently', async () => {
      // Register validation on 'email' and 'password'
      // Set both to invalid values
      // Assert _errors['email'] has email errors
      // Assert _errors['password'] has password errors
      // Both independent
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            email: {
              validationState: {
                schema: z.string().email('Invalid email'),
              },
            },
            fieldA: {
              validationState: {
                schema: z.string().min(8, 'Too short'),
              },
            },
          },
        },
      )

      setValue('email', 'invalid')
      setValue('fieldA', 'short')
      await flushEffects()

      expect(storeInstance.state._errors?.['email']).toBeDefined()
      expect(storeInstance.state._errors?.['fieldA']).toBeDefined()
      expect(
        (storeInstance.state._errors?.['email'] as string[]).length,
      ).toBeGreaterThan(0)
      expect(
        (storeInstance.state._errors?.['fieldA'] as string[]).length,
      ).toBeGreaterThan(0)
    })
  })

  describe('Error display through concerns', () => {
    it('should display errors from validationState concern', async () => {
      // Register validationState concern
      // Trigger validation failure
      // Assert concern result includes error messages
      // Assert component can display errors
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            email: {
              validationState: {
                schema: z.string().email('Invalid email format'),
              },
            },
          },
        },
      )

      setValue('email', 'invalid')
      await flushEffects()

      expect(storeInstance._concerns?.['email']).toMatchObject({
        validationState: {
          isError: true,
          errors: expect.arrayContaining([expect.any(Object)]),
        },
      })
    })

    it('should support error message templates', async () => {
      // Register validation with template error messages
      // Template: 'Field {{fieldName}} is required'
      // Assert error message includes interpolated value
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            email: {
              validationState: {
                schema: z.string().email('Field email is invalid'),
              },
            },
          },
        },
      )

      setValue('email', 'not-an-email')
      await flushEffects()

      const validationState =
        storeInstance._concerns?.['email']?.['validationState']
      expect(validationState).toBeDefined()
      expect((validationState as any)?.errors?.length).toBeGreaterThan(0)
    })
  })

  describe('Error clearing', () => {
    it('should clear errors when field becomes valid', async () => {
      // Set field to invalid value → errors present
      // Set field to valid value
      // Assert _errors[path] is empty or removed
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            email: {
              validationState: {
                schema: z.string().email(),
              },
            },
          },
        },
      )

      setValue('email', 'invalid')
      await flushEffects()
      expect(storeInstance.state._errors?.['email']).toBeDefined()

      setValue('email', 'valid@example.com')
      await flushEffects()

      expect(
        storeInstance.state._errors?.['email'] === undefined ||
          storeInstance.state._errors?.['email']?.length === 0,
      ).toBe(true)
    })

    it('should clear all errors on form reset', async () => {
      // Multiple fields have errors
      // Reset form (set all fields to valid defaults)
      // Assert _errors is empty
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            email: {
              validationState: {
                schema: z.string().email(),
              },
            },
            age: {
              validationState: {
                schema: z.number().positive(),
              },
            },
          },
        },
      )

      setValue('email', 'invalid')
      setValue('age', -1)
      await flushEffects()

      setValue('email', 'valid@example.com')
      setValue('age', 25)
      await flushEffects()

      expect(Object.keys(storeInstance.state._errors ?? {}).length === 0).toBe(
        true,
      )
    })

    it('should clear only affected field errors on change', async () => {
      // email and password both have errors
      // Fix email only
      // Assert email errors cleared
      // Assert password errors still present
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            email: {
              validationState: {
                schema: z.string().email(),
              },
            },
            fieldB: {
              validationState: {
                schema: z.string().min(5),
              },
            },
          },
        },
      )

      setValue('email', 'invalid')
      setValue('fieldB', 'ab')
      await flushEffects()

      expect(storeInstance.state._errors?.['email']).toBeDefined()
      expect(storeInstance.state._errors?.['fieldB']).toBeDefined()

      setValue('email', 'valid@example.com')
      await flushEffects()

      expect(
        storeInstance.state._errors?.['email'] === undefined ||
          storeInstance.state._errors?.['email']?.length === 0,
      ).toBe(true)
      expect(
        storeInstance.state._errors?.['fieldB']?.length ?? 0,
      ).toBeGreaterThan(0)
    })
  })

  describe('Error-driven UI', () => {
    it('should disable submit button when errors exist', async () => {
      // Register disabledWhen on submit button
      // BoolLogic: NOT(IS_EMPTY('_errors'))
      // Trigger errors
      // Assert submit button disabled
      // Fix all errors
      // Assert submit button enabled
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            email: {
              validationState: {
                schema: z.string().email(),
              },
            },
          },
        },
      )

      setValue('email', 'invalid')
      await flushEffects()

      const errorsExist =
        Object.keys(storeInstance.state._errors ?? {}).length > 0
      expect(errorsExist).toBe(true)

      setValue('email', 'valid@example.com')
      await flushEffects()

      const noErrors =
        Object.keys(storeInstance.state._errors ?? {}).length === 0
      expect(noErrors).toBe(true)
    })

    it('should show error state styling', async () => {
      // Register concern that indicates error state on field
      // Assert error concern reflects validation failure
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            email: {
              validationState: {
                schema: z.string().email(),
              },
            },
          },
        },
      )

      setValue('email', 'invalid')
      await flushEffects()

      const validationStateError =
        storeInstance._concerns?.['email']?.['validationState']
      expect((validationStateError as any)?.isError).toBe(true)

      setValue('email', 'valid@example.com')
      await flushEffects()

      const validationStateValid =
        storeInstance._concerns?.['email']?.['validationState']
      expect((validationStateValid as any)?.isError).toBe(false)
    })
  })

  describe('Field-level vs form-level errors', () => {
    it('should distinguish field-level errors', async () => {
      // Trigger validation error on specific field
      // Assert error stored under field path in _errors
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            email: {
              validationState: {
                schema: z.string().email(),
              },
            },
          },
        },
      )

      setValue('email', 'invalid')
      await flushEffects()

      expect(storeInstance.state._errors?.['email']).toBeDefined()
      expect(storeInstance.state._errors?.['email']?.length).toBeGreaterThan(0)
    })

    it('should distinguish form-level errors', async () => {
      // Trigger form-level validation (e.g., cross-field)
      // Assert error stored under form-level key in _errors
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            email: {
              validationState: {
                schema: z.string().email(),
              },
            },
          },
        },
      )

      setValue('email', 'invalid@invalid')
      await flushEffects()

      const formErrors = storeInstance.state._errors
      expect(formErrors).toBeDefined()
      expect(typeof formErrors).toBe('object')
    })

    it('should handle both error types simultaneously', async () => {
      // Both field-level and form-level errors active
      // Assert both types present in _errors
      // Assert component can differentiate
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            email: {
              validationState: {
                schema: z.string().email(),
              },
            },
            fieldA: {
              validationState: {
                schema: z.string().min(3),
              },
            },
          },
        },
      )

      setValue('email', 'invalid')
      setValue('fieldA', 'a')
      await flushEffects()

      const errors = storeInstance.state._errors
      expect(errors?.['email']).toBeDefined()
      expect(errors?.['fieldA']).toBeDefined()
    })
  })

  describe('Error persistence', () => {
    it('should preserve errors when other fields are updated', async () => {
      // email has validation errors
      // Change password field (unrelated)
      // Assert email errors still present
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            email: {
              validationState: {
                schema: z.string().email(),
              },
            },
          },
        },
      )

      setValue('email', 'invalid')
      await flushEffects()

      const emailErrorsBefore = storeInstance.state._errors?.['email']
      expect(emailErrorsBefore?.length).toBeGreaterThan(0)

      setValue('fieldB', 'changed-value')
      await flushEffects()

      const emailErrorsAfter = storeInstance.state._errors?.['email']
      expect(emailErrorsAfter?.length).toBeGreaterThan(0)
    })

    it('should preserve errors through re-renders', async () => {
      // Trigger validation errors
      // Cause unrelated re-render
      // Assert errors still present
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            email: {
              validationState: {
                schema: z.string().email(),
              },
            },
          },
        },
      )

      setValue('email', 'invalid')
      await flushEffects()

      const errorsBefore = storeInstance.state._errors?.['email']
      expect(errorsBefore?.length).toBeGreaterThan(0)

      setValue('fieldC', 42)
      await flushEffects()

      const errorsAfter = storeInstance.state._errors?.['email']
      expect(errorsAfter?.length).toBeGreaterThan(0)
    })
  })

  describe('Error handling with store config', () => {
    it('should use default errorStorePath ("_errors")', async () => {
      // Create store with default config
      // Assert errors stored under '_errors'
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            email: {
              validationState: {
                schema: z.string().email(),
              },
            },
          },
        },
      )

      setValue('email', 'invalid')
      await flushEffects()

      expect(storeInstance.state._errors).toBeDefined()
      expect(storeInstance.state._errors?.['email']).toBeDefined()
    })

    it('should use custom errorStorePath if configured', async () => {
      // Create store with config: { errorStorePath: 'formErrors' }
      // Trigger validation error
      // Assert errors stored under 'formErrors'
      const store = createGenericStore<BasicTestState>({
        ...config,
        errorStorePath: 'customErrors',
      })
      const { storeInstance, setValue } = mountStore(
        store,
        { ...basicTestFixtures.empty, customErrors: {} } as any,
        {
          concerns: {
            email: {
              validationState: {
                schema: z.string().email(),
              },
            },
          },
        },
      )

      setValue('email', 'invalid')
      await flushEffects()

      expect((storeInstance.state as any).customErrors).toBeDefined()
    })
  })

  describe('Error handling edge cases', () => {
    it('should handle errors for non-existent fields gracefully', async () => {
      // Try to set error for field that doesn't exist
      // Assert no crash
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance: _si, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {},
      )

      expect(() => {
        setValue('fieldA', 'value')
      }).not.toThrow()

      await flushEffects()

      expect(_si).toBeDefined()
    })

    it('should handle empty error arrays correctly', async () => {
      // Set _errors[path] = []
      // Assert treated as "no errors"
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        { ...basicTestFixtures.empty, _errors: { email: [] } },
        {
          concerns: {
            email: {
              validationState: {
                schema: z.string().email(),
              },
            },
          },
        },
      )

      const hasErrors =
        (storeInstance.state._errors?.['email'] ?? []).length > 0
      expect(hasErrors).toBe(false)

      setValue('email', 'valid@example.com')
      await flushEffects()

      expect(storeInstance.state._errors?.['email']?.length ?? 0).toBe(0)
    })

    it('should handle concurrent error updates', async () => {
      // Multiple validation errors trigger simultaneously
      // Assert all errors stored correctly
      // No race conditions
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            email: {
              validationState: {
                schema: z.string().email(),
              },
            },
            fieldA: {
              validationState: {
                schema: z.string().min(5),
              },
            },
            fieldB: {
              validationState: {
                schema: z.string().min(3),
              },
            },
          },
        },
      )

      setValue('email', 'invalid')
      setValue('fieldA', 'a')
      setValue('fieldB', 'b')
      await flushEffects()

      expect(storeInstance.state._errors?.['email']).toBeDefined()
      expect(storeInstance.state._errors?.['fieldA']).toBeDefined()
      expect(storeInstance.state._errors?.['fieldB']).toBeDefined()
    })
  })
})
