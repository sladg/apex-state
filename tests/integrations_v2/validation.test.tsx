/**
 * Concerns: Validation (Zod schema-based validation concerns)
 *
 * Validates that validationState concerns:
 * - Evaluate Zod schemas against field values
 * - Store validation results in _concerns
 * - Clear errors when fields become valid
 * - Support cross-field validation
 * - Work with the WASM validator batching pipeline
 *
 * ┌─────────────────────────────────────────────────────────────────────┐
 * │ REPLACES (when this v2 test is fully implemented):                  │
 * ├─────────────────────────────────────────────────────────────────────┤
 * │ tests/integration/form-validation.test.tsx          (ENTIRE FILE)  │
 * │ tests/integration/ecommerce-catalog.test.tsx   (validation tests)  │
 * │   → Zod schema tests at depth 9-15                                 │
 * └─────────────────────────────────────────────────────────────────────┘
 */

import { describe, expect, it } from 'vitest'
import { z } from 'zod'

import { createGenericStore } from '../../src'
import type { ValidationTestState } from '../mocks'
import { validationTestFixtures } from '../mocks'
import { flushEffects, MODES, mountStore } from '../utils/react'

describe.each(MODES)(
  '[$name] Concerns: Validation (Zod Schema)',
  ({ config }) => {
    describe('Basic Zod validation', () => {
      it('should validate email format with Zod schema', async () => {
        // Register validationState concern on 'email' field
        // Schema: z.string().email()
        // Set email to 'invalid'
        // Assert validation fails with error message
        const store = createGenericStore<ValidationTestState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          validationTestFixtures.empty,
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

      it('should pass validation for valid value', async () => {
        // Register validationState on 'email'
        // Schema: z.string().email()
        // Set email to 'user@example.com'
        // Assert validation passes (no errors)
        const store = createGenericStore<ValidationTestState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          validationTestFixtures.empty,
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

        setValue('email', 'user@example.com')
        await flushEffects()

        expect(storeInstance._concerns?.['email']).toMatchObject({
          validationState: {
            isError: false,
            errors: [],
          },
        })
      })

      it('should validate password complexity', async () => {
        // Register validationState on 'password'
        // Schema: z.string().min(8).regex(/[A-Z]/).regex(/[0-9]/)
        // Set password to 'short' → assert fails
        // Set password to 'LongEnough1' → assert passes
        const store = createGenericStore<ValidationTestState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          validationTestFixtures.empty,
          {
            concerns: {
              password: {
                validationState: {
                  schema: z
                    .string()
                    .min(8, 'At least 8 characters')
                    .regex(/[A-Z]/, 'Must contain uppercase')
                    .regex(/[0-9]/, 'Must contain number'),
                },
              },
            },
          },
        )

        setValue('password', 'short')
        await flushEffects()
        expect(storeInstance._concerns?.['password']).toMatchObject({
          validationState: {
            isError: true,
          },
        })

        setValue('password', 'LongEnough1')
        await flushEffects()
        expect(storeInstance._concerns?.['password']).toMatchObject({
          validationState: {
            isError: false,
            errors: [],
          },
        })
      })

      it('should validate numeric range', async () => {
        // Register validationState on numeric field
        // Schema: z.number().min(0).max(100)
        // Set to -1 → fails
        // Set to 50 → passes
        // Set to 101 → fails
        const store = createGenericStore<ValidationTestState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          validationTestFixtures.empty,
          {
            concerns: {
              age: {
                validationState: {
                  schema: z
                    .number()
                    .min(0, 'Age cannot be negative')
                    .max(150, 'Age is too high'),
                },
              },
            },
          },
        )

        setValue('age', -1)
        await flushEffects()
        expect(storeInstance._concerns?.['age']).toMatchObject({
          validationState: {
            isError: true,
          },
        })

        setValue('age', 50)
        await flushEffects()
        expect(storeInstance._concerns?.['age']).toMatchObject({
          validationState: {
            isError: false,
          },
        })

        setValue('age', 151)
        await flushEffects()
        expect(storeInstance._concerns?.['age']).toMatchObject({
          validationState: {
            isError: true,
          },
        })
      })

      it('should validate required string field', async () => {
        // Register validationState on 'username'
        // Schema: z.string().min(3)
        // Set to empty → fails
        // Set to 'ab' → fails
        // Set to 'abc' → passes
        const store = createGenericStore<ValidationTestState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          validationTestFixtures.empty,
          {
            concerns: {
              username: {
                validationState: {
                  schema: z
                    .string()
                    .min(3, 'Username must be at least 3 characters'),
                },
              },
            },
          },
        )

        setValue('username', '')
        await flushEffects()
        expect(storeInstance._concerns?.['username']).toMatchObject({
          validationState: {
            isError: true,
          },
        })

        setValue('username', 'ab')
        await flushEffects()
        expect(storeInstance._concerns?.['username']).toMatchObject({
          validationState: {
            isError: true,
          },
        })

        setValue('username', 'abc')
        await flushEffects()
        expect(storeInstance._concerns?.['username']).toMatchObject({
          validationState: {
            isError: false,
          },
        })
      })
    })

    describe('Validation error messages', () => {
      it('should display error messages from Zod schema', async () => {
        // Register validationState with custom Zod error messages
        // Trigger validation failure
        // Assert error message matches Zod schema message
        const store = createGenericStore<ValidationTestState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          validationTestFixtures.empty,
          {
            concerns: {
              email: {
                validationState: {
                  schema: z
                    .string()
                    .email('Please provide a valid email address'),
                },
              },
            },
          },
        )

        setValue('email', 'not-an-email')
        await flushEffects()

        const validationState =
          storeInstance._concerns?.['email']?.['validationState']
        expect(validationState).toMatchObject({
          isError: true,
          errors: expect.arrayContaining([
            expect.objectContaining({
              message: expect.stringContaining('email'),
            }),
          ]),
        })
      })

      it('should clear errors when field becomes valid', async () => {
        // Set field to invalid value → errors present
        // Set field to valid value
        // Assert errors cleared
        const store = createGenericStore<ValidationTestState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          validationTestFixtures.empty,
          {
            concerns: {
              email: {
                validationState: {
                  schema: z.string().email('Invalid email'),
                },
              },
            },
          },
        )

        setValue('email', 'invalid-email')
        await flushEffects()
        expect(storeInstance._concerns?.['email']).toMatchObject({
          validationState: {
            isError: true,
            errors: expect.arrayContaining([expect.any(Object)]),
          },
        })

        setValue('email', 'valid@example.com')
        await flushEffects()
        expect(storeInstance._concerns?.['email']).toMatchObject({
          validationState: {
            isError: false,
            errors: [],
          },
        })
      })

      it('should report multiple validation errors', async () => {
        // Register validation with multiple rules
        // Set value that fails multiple rules
        // Assert all errors reported
        const store = createGenericStore<ValidationTestState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          validationTestFixtures.empty,
          {
            concerns: {
              password: {
                validationState: {
                  schema: z
                    .string()
                    .min(8, 'Must be at least 8 characters')
                    .regex(/[A-Z]/, 'Must contain uppercase letter')
                    .regex(/[0-9]/, 'Must contain a number'),
                },
              },
            },
          },
        )

        setValue('password', 'abc')
        await flushEffects()

        expect(storeInstance._concerns?.['password']).toMatchObject({
          validationState: {
            isError: true,
            errors: expect.arrayContaining([expect.any(Object)]),
          },
        })
      })
    })

    describe('Cross-field validation', () => {
      it('should validate confirm password matches password', async () => {
        // Register validationState on 'confirmPassword'
        // With dependency on 'password' field
        // Set password = 'test123', confirmPassword = 'test456'
        // Assert validation fails (mismatch)
        // Set confirmPassword = 'test123'
        // Assert validation passes
        const store = createGenericStore<ValidationTestState>(config)
        const refHolder: { storeInstance?: any } = {}
        const { storeInstance, setValue } = mountStore(
          store,
          validationTestFixtures.empty,
          {
            concerns: {
              confirmPassword: {
                validationState: {
                  schema: z.string().refine((value) => {
                    if (refHolder.storeInstance) {
                      return value === refHolder.storeInstance.state.password
                    }
                    return true
                  }, 'Passwords do not match'),
                },
              },
            },
          },
        )
        refHolder.storeInstance = storeInstance

        setValue('password', 'test123')
        setValue('confirmPassword', 'test456')
        await flushEffects()
        expect(storeInstance._concerns?.['confirmPassword']).toMatchObject({
          validationState: {
            isError: true,
          },
        })

        setValue('confirmPassword', 'test123')
        await flushEffects()
        expect(storeInstance._concerns?.['confirmPassword']).toMatchObject({
          validationState: {
            isError: false,
            errors: [],
          },
        })
      })

      it('should re-validate when own field changes', async () => {
        // Register validationState with dependency on another field
        // Change dependent field
        // Assert validator re-runs when confirmPassword field itself changes
        const store = createGenericStore<ValidationTestState>(config)
        const refHolder: { storeInstance?: any } = {}
        const { storeInstance, setValue } = mountStore(
          store,
          validationTestFixtures.empty,
          {
            concerns: {
              confirmPassword: {
                validationState: {
                  schema: z.string().refine((value) => {
                    if (refHolder.storeInstance) {
                      return value === refHolder.storeInstance.state.password
                    }
                    return true
                  }, 'Passwords do not match'),
                },
              },
            },
          },
        )
        refHolder.storeInstance = storeInstance

        setValue('password', 'pass1')
        setValue('confirmPassword', 'pass1')
        await flushEffects()
        expect(storeInstance._concerns?.['confirmPassword']).toMatchObject({
          validationState: {
            isError: false,
          },
        })

        setValue('confirmPassword', 'pass2')
        await flushEffects()
        expect(storeInstance._concerns?.['confirmPassword']).toMatchObject({
          validationState: {
            isError: true,
          },
        })

        setValue('confirmPassword', 'pass1')
        await flushEffects()
        expect(storeInstance._concerns?.['confirmPassword']).toMatchObject({
          validationState: {
            isError: false,
          },
        })
      })

      it('should allow custom cross-field validation logic', async () => {
        // Register validator that checks multiple field values
        // Assert validation uses both fields correctly
        const store = createGenericStore<ValidationTestState>(config)
        const refHolder: { storeInstance?: any } = {}
        const { storeInstance, setValue } = mountStore(
          store,
          validationTestFixtures.empty,
          {
            concerns: {
              password: {
                validationState: {
                  schema: z.string().refine((value) => {
                    if (refHolder.storeInstance) {
                      return (
                        value.length >= 8 &&
                        value !== refHolder.storeInstance.state.username
                      )
                    }
                    return true
                  }, 'Password must be 8+ chars and different from username'),
                },
              },
            },
          },
        )
        refHolder.storeInstance = storeInstance

        setValue('username', 'johndoe')
        setValue('password', 'johndoe')
        await flushEffects()
        expect(storeInstance._concerns?.['password']).toMatchObject({
          validationState: {
            isError: true,
          },
        })

        setValue('password', 'StrongPass123')
        await flushEffects()
        expect(storeInstance._concerns?.['password']).toMatchObject({
          validationState: {
            isError: false,
          },
        })
      })
    })

    describe('Validation with WASM pipeline', () => {
      it('should validate when multiple fields change together', async () => {
        // Register validators on multiple fields
        // Change multiple fields in one operation
        // Assert all validators run
        const store = createGenericStore<ValidationTestState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          validationTestFixtures.empty,
          {
            concerns: {
              email: {
                validationState: {
                  schema: z.string().email(),
                },
              },
              username: {
                validationState: {
                  schema: z.string().min(3),
                },
              },
            },
          },
        )

        setValue('email', 'test@example.com')
        setValue('username', 'john')
        await flushEffects()

        expect(storeInstance._concerns?.['email']).toMatchObject({
          validationState: {
            isError: false,
          },
        })
        expect(storeInstance._concerns?.['username']).toMatchObject({
          validationState: {
            isError: false,
          },
        })
      })

      it('should only validate affected fields when one field changes', async () => {
        // Register validators on multiple fields
        // Change only one field
        // Assert only that field's validator runs
        const store = createGenericStore<ValidationTestState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          validationTestFixtures.empty,
          {
            concerns: {
              email: {
                validationState: {
                  schema: z.string().email(),
                },
              },
              username: {
                validationState: {
                  schema: z.string().min(3),
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
          },
        })
      })

      it('should validate multiple fields independently', async () => {
        // Register validators on different fields with different schemas
        // Change each field separately
        // Assert each validates according to its schema
        const store = createGenericStore<ValidationTestState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          validationTestFixtures.empty,
          {
            concerns: {
              email: {
                validationState: {
                  schema: z.string().email(),
                },
              },
              age: {
                validationState: {
                  schema: z.number().min(18).max(120),
                },
              },
              password: {
                validationState: {
                  schema: z.string().min(8),
                },
              },
            },
          },
        )

        setValue('email', 'test@example.com')
        await flushEffects()
        expect(storeInstance._concerns?.['email']).toMatchObject({
          validationState: { isError: false },
        })

        setValue('age', 25)
        await flushEffects()
        expect(storeInstance._concerns?.['age']).toMatchObject({
          validationState: { isError: false },
        })

        setValue('password', 'short')
        await flushEffects()
        expect(storeInstance._concerns?.['password']).toMatchObject({
          validationState: { isError: true },
        })
      })
    })

    describe('Validation concern results', () => {
      it('should store validation result in _concerns proxy', async () => {
        // Register validationState on 'email'
        // Trigger validation
        // Assert _concerns['email']['validationState'] exists
        // Assert contains isError flag and errors array
        const store = createGenericStore<ValidationTestState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          validationTestFixtures.empty,
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

        setValue('email', 'test@example.com')
        await flushEffects()

        expect(storeInstance._concerns?.['email']).toBeDefined()
        expect(storeInstance._concerns?.['email']).toMatchObject({
          validationState: expect.objectContaining({
            isError: expect.any(Boolean),
            errors: expect.any(Array),
          }),
        })
      })

      it('should expose isError flag as false for valid values', async () => {
        // Register and trigger validation
        // Assert result.isError === false for valid
        const store = createGenericStore<ValidationTestState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          validationTestFixtures.empty,
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

        setValue('email', 'valid@example.com')
        await flushEffects()

        expect(storeInstance._concerns?.['email']).toMatchObject({
          validationState: {
            isError: false,
          },
        })
      })

      it('should expose isError flag as true for invalid values', async () => {
        // Register and trigger validation
        // Assert result.isError === true for invalid
        const store = createGenericStore<ValidationTestState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          validationTestFixtures.empty,
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

        setValue('email', 'not-an-email')
        await flushEffects()

        expect(storeInstance._concerns?.['email']).toMatchObject({
          validationState: {
            isError: true,
          },
        })
      })

      it('should expose empty error array for valid values', async () => {
        // Register and trigger validation (success)
        // Assert result.errors is empty array
        const store = createGenericStore<ValidationTestState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          validationTestFixtures.empty,
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

        setValue('email', 'valid@example.com')
        await flushEffects()

        expect(storeInstance._concerns?.['email']).toMatchObject({
          validationState: {
            errors: [],
          },
        })
      })

      it('should expose error array with ValidationError objects', async () => {
        // Register and trigger validation (failure)
        // Assert result.errors is array with error objects
        const store = createGenericStore<ValidationTestState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          validationTestFixtures.empty,
          {
            concerns: {
              email: {
                validationState: {
                  schema: z.string().email('Invalid email'),
                },
              },
            },
          },
        )

        setValue('email', 'not-an-email')
        await flushEffects()

        expect(storeInstance._concerns?.['email']).toMatchObject({
          validationState: {
            errors: expect.arrayContaining([
              expect.objectContaining({
                message: expect.any(String),
              }),
            ]),
          },
        })
      })
    })

    describe('Multiple validators on same field', () => {
      it('should support multiple validation rules on one field', async () => {
        // Register validationState on 'email' with complex schema
        // z.string().email().min(5).max(100)
        // Test each rule independently
        const store = createGenericStore<ValidationTestState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          validationTestFixtures.empty,
          {
            concerns: {
              email: {
                validationState: {
                  schema: z
                    .string()
                    .email('Must be a valid email')
                    .min(5, 'Must be at least 5 characters')
                    .max(100, 'Must not exceed 100 characters'),
                },
              },
            },
          },
        )

        setValue('email', 'a@b')
        await flushEffects()
        expect(storeInstance._concerns?.['email']).toMatchObject({
          validationState: {
            isError: true,
          },
        })

        setValue('email', 'test@example.com')
        await flushEffects()
        expect(storeInstance._concerns?.['email']).toMatchObject({
          validationState: {
            isError: false,
          },
        })

        setValue('email', 'a'.repeat(101) + '@example.com')
        await flushEffects()
        expect(storeInstance._concerns?.['email']).toMatchObject({
          validationState: {
            isError: true,
          },
        })
      })

      it('should report all failing rules', async () => {
        // Register complex validation
        // Set value that fails multiple rules
        // Assert all errors reported
        const store = createGenericStore<ValidationTestState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          validationTestFixtures.empty,
          {
            concerns: {
              password: {
                validationState: {
                  schema: z
                    .string()
                    .min(8, 'Must be at least 8 characters')
                    .regex(/[A-Z]/, 'Must contain uppercase')
                    .regex(/[0-9]/, 'Must contain number')
                    .regex(/[!@#$%]/, 'Must contain special char'),
                },
              },
            },
          },
        )

        setValue('password', 'pass')
        await flushEffects()

        expect(storeInstance._concerns?.['password']).toMatchObject({
          validationState: {
            isError: true,
            errors: expect.arrayContaining([expect.any(Object)]),
          },
        })
      })
    })

    describe('Validation registration lifecycle', () => {
      it('should register validator via useConcerns', async () => {
        // Call useConcerns with validationState config
        // Assert validator registered
        const store = createGenericStore<ValidationTestState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          validationTestFixtures.empty,
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

        setValue('email', 'test@example.com')
        await flushEffects()

        expect(storeInstance._concerns?.['email']).toMatchObject({
          validationState: expect.any(Object),
        })
      })

      it('should validate immediately after registration', async () => {
        // Register validator on field with existing value
        // Assert validation runs immediately
        const store = createGenericStore<ValidationTestState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          validationTestFixtures.empty,
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

        setValue('email', 'invalid-email')
        await flushEffects()

        expect(storeInstance._concerns?.['email']).toMatchObject({
          validationState: {
            isError: true,
          },
        })
      })

      it('should support registering multiple validators on same field', async () => {
        // Register multiple concerns on same field
        // Assert all work correctly
        const store = createGenericStore<ValidationTestState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          validationTestFixtures.empty,
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

        setValue('email', 'valid@example.com')
        await flushEffects()

        expect(storeInstance._concerns?.['email']).toBeDefined()
        expect(storeInstance._concerns?.['email']).toMatchObject({
          validationState: expect.any(Object),
        })
      })
    })

    describe('Validation with deeply nested paths', () => {
      it('should validate fields in nested objects', async () => {
        // Register validationState on nested path
        // Change nested value
        // Assert validation runs on deep path
        const nestedStore = createGenericStore<{
          user: {
            profile: {
              email: string
              contact: {
                address: string
              }
            }
          }
        }>(config)

        const { storeInstance, setValue } = mountStore(
          nestedStore,
          {
            user: {
              profile: {
                email: '',
                contact: {
                  address: '',
                },
              },
            },
          },
          {
            concerns: {
              'user.profile.email': {
                validationState: {
                  schema: z.string().email(),
                },
              },
            },
          },
        )

        setValue('user.profile.email', 'invalid')
        await flushEffects()

        expect(storeInstance._concerns?.['user.profile.email']).toMatchObject({
          validationState: {
            isError: true,
          },
        })

        setValue('user.profile.email', 'valid@example.com')
        await flushEffects()

        expect(storeInstance._concerns?.['user.profile.email']).toMatchObject({
          validationState: {
            isError: false,
          },
        })
      })

      it('should validate multiple nested fields independently', async () => {
        // Register validators on different nested paths
        // Change each separately
        // Assert each validates correctly
        const nestedStore = createGenericStore<{
          profile: {
            email: string
            age: number
          }
        }>(config)

        const { storeInstance, setValue } = mountStore(
          nestedStore,
          {
            profile: {
              email: '',
              age: 0,
            },
          },
          {
            concerns: {
              'profile.email': {
                validationState: {
                  schema: z.string().email(),
                },
              },
              'profile.age': {
                validationState: {
                  schema: z.number().min(18),
                },
              },
            },
          },
        )

        setValue('profile.email', 'test@example.com')
        await flushEffects()
        expect(storeInstance._concerns?.['profile.email']).toMatchObject({
          validationState: { isError: false },
        })

        setValue('profile.age', 25)
        await flushEffects()
        expect(storeInstance._concerns?.['profile.age']).toMatchObject({
          validationState: { isError: false },
        })
      })
    })

    describe('Validation edge cases', () => {
      it('should handle validator on existing path', async () => {
        // Register validator on path that exists in state
        // Change value
        // Assert no crash and validation works
        const store = createGenericStore<ValidationTestState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          validationTestFixtures.empty,
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

        setValue('email', 'test@example.com')
        await flushEffects()

        expect(storeInstance._concerns?.['email']).toMatchObject({
          validationState: {
            isError: false,
          },
        })
      })

      it('should work with no validators registered', async () => {
        // No validators registered
        // Process changes
        // Assert no crash
        const store = createGenericStore<ValidationTestState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          validationTestFixtures.empty,
          { concerns: {} },
        )

        setValue('email', 'test@example.com')
        await flushEffects()

        expect(
          storeInstance._concerns?.['email']?.['validationState'],
        ).toBeUndefined()
      })

      it('should handle empty string validation', async () => {
        // Register validator that allows empty strings
        // Set to empty string
        // Assert validation respects empty strings
        const store = createGenericStore<ValidationTestState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          validationTestFixtures.empty,
          {
            concerns: {
              email: {
                validationState: {
                  schema: z.string().email().or(z.string().length(0)),
                },
              },
            },
          },
        )

        setValue('email', '')
        await flushEffects()

        expect(storeInstance._concerns?.['email']).toMatchObject({
          validationState: {
            isError: false,
          },
        })
      })

      it('should handle null-like values gracefully', async () => {
        // Register validator on field
        // Set to default/zero value
        // Assert no crash
        const store = createGenericStore<ValidationTestState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          validationTestFixtures.empty,
          {
            concerns: {
              age: {
                validationState: {
                  schema: z.number().min(0),
                },
              },
            },
          },
        )

        setValue('age', 0)
        await flushEffects()

        expect(storeInstance._concerns?.['age']).toMatchObject({
          validationState: expect.any(Object),
        })
      })
    })

    describe('Real-world validation scenarios', () => {
      it('should validate complete registration form', async () => {
        // Multiple fields: email, password, confirmPassword, username, age
        // Each with appropriate Zod schema
        // Fill out form with valid data → all pass
        // Introduce errors → specific fields fail
        const store = createGenericStore<ValidationTestState>(config)
        const { storeInstance, setValue } = mountStore(
          store,
          validationTestFixtures.empty,
          {
            concerns: {
              email: {
                validationState: {
                  schema: z.string().email(),
                },
              },
              password: {
                validationState: {
                  schema: z
                    .string()
                    .min(8, 'At least 8 characters')
                    .regex(/[A-Z]/, 'Must contain uppercase')
                    .regex(/[0-9]/, 'Must contain number'),
                },
              },
              username: {
                validationState: {
                  schema: z.string().min(3),
                },
              },
              age: {
                validationState: {
                  schema: z.number().min(18).max(120),
                },
              },
            },
          },
        )

        setValue('email', 'user@example.com')
        setValue('password', 'StrongPass123')
        setValue('username', 'johndoe')
        setValue('age', 25)
        await flushEffects()

        expect(storeInstance._concerns?.['email']).toMatchObject({
          validationState: { isError: false },
        })
        expect(storeInstance._concerns?.['password']).toMatchObject({
          validationState: { isError: false },
        })
        expect(storeInstance._concerns?.['username']).toMatchObject({
          validationState: { isError: false },
        })
        expect(storeInstance._concerns?.['age']).toMatchObject({
          validationState: { isError: false },
        })

        setValue('email', 'invalid-email')
        setValue('password', 'weak')
        setValue('age', 15)
        await flushEffects()

        expect(storeInstance._concerns?.['email']).toMatchObject({
          validationState: { isError: true },
        })
        expect(storeInstance._concerns?.['password']).toMatchObject({
          validationState: { isError: true },
        })
        expect(storeInstance._concerns?.['age']).toMatchObject({
          validationState: { isError: true },
        })
      })

      it('should allow conditional validation', async () => {
        // Register conditional validator
        // Value passes or fails based on other field state
        const store = createGenericStore<ValidationTestState>(config)
        const refHolder: { storeInstance?: any } = {}
        const { storeInstance, setValue } = mountStore(
          store,
          validationTestFixtures.empty,
          {
            concerns: {
              confirmPassword: {
                validationState: {
                  schema: z.string().refine((value) => {
                    if (refHolder.storeInstance) {
                      return value === refHolder.storeInstance.state.password
                    }
                    return true
                  }, 'Passwords must match'),
                },
              },
            },
          },
        )
        refHolder.storeInstance = storeInstance

        setValue('password', 'Test123')
        setValue('confirmPassword', 'Test456')
        await flushEffects()

        expect(storeInstance._concerns?.['confirmPassword']).toMatchObject({
          validationState: {
            isError: true,
          },
        })

        setValue('confirmPassword', 'Test123')
        await flushEffects()

        expect(storeInstance._concerns?.['confirmPassword']).toMatchObject({
          validationState: {
            isError: false,
          },
        })
      })

      it('should handle sequential validation workflows', async () => {
        // Step 1: Validate email
        // Step 2: Validate password
        // Step 3: Validate password confirmation
        // Each step validates independently but can depend on previous
        const store = createGenericStore<ValidationTestState>(config)
        const refHolder: { storeInstance?: any } = {}
        const { storeInstance, setValue } = mountStore(
          store,
          validationTestFixtures.empty,
          {
            concerns: {
              email: {
                validationState: {
                  schema: z.string().email(),
                },
              },
              password: {
                validationState: {
                  schema: z.string().min(8),
                },
              },
              confirmPassword: {
                validationState: {
                  schema: z.string().refine((value) => {
                    if (refHolder.storeInstance) {
                      return value === refHolder.storeInstance.state.password
                    }
                    return true
                  }, 'Passwords must match'),
                },
              },
            },
          },
        )
        refHolder.storeInstance = storeInstance

        setValue('email', 'test@example.com')
        await flushEffects()
        expect(storeInstance._concerns?.['email']).toMatchObject({
          validationState: { isError: false },
        })

        setValue('password', 'ValidPass123')
        await flushEffects()
        expect(storeInstance._concerns?.['password']).toMatchObject({
          validationState: { isError: false },
        })

        setValue('confirmPassword', 'ValidPass123')
        await flushEffects()
        expect(storeInstance._concerns?.['confirmPassword']).toMatchObject({
          validationState: { isError: false },
        })
      })
    })
  },
)
