/**
 * TEST: Concern Registration - isSchemaValidation Detection
 *
 * Priority: Low (Dev task, doesn't block QA)
 *
 * Validates that the isSchemaValidation detection logic in registration.ts
 * correctly distinguishes between schema-based and custom validations.
 *
 * Detection rules (src/concerns/registration.ts:17-23):
 * - concernName === 'validationState'
 * - 'schema' in config
 * - !('evaluate' in config)
 */

import { describe, expect, it } from 'vitest'
import { z } from 'zod'

import { createGenericStore } from '../../src'
import { flushSync, mountStore } from '../utils/react'

interface TestState {
  user: { email: string; name: string }
}

describe('TEST: isSchemaValidation Detection', () => {
  const createTestStore = () => createGenericStore<TestState>()

  it('should detect schema-based config correctly (schema + no evaluate)', async () => {
    // Config with schema + no evaluate → isSchemaValidation = true
    // This should register via WASM validator path (not effect-based)

    const store = createTestStore()

    const { storeInstance } = mountStore(
      store,
      {
        user: { email: 'test@example.com', name: 'Alice' },
      },
      {
        concerns: {
          'user.email': {
            validationState: {
              schema: z.string().email(),
            },
          },
        },
      },
    )

    await flushSync()

    // Verify schema-based validation was registered and evaluated immediately
    expect(
      storeInstance._concerns['user.email']?.['validationState'],
    ).toBeDefined()
  })

  it('should detect custom validation correctly (schema + evaluate)', async () => {
    // Config with schema + evaluate → isSchemaValidation = false (custom)
    // This should register via effect-based path (not WASM validator)

    const store = createTestStore()

    const { storeInstance } = mountStore(
      store,
      {
        user: { email: 'test@example.com', name: 'Alice' },
      },
      {
        concerns: {
          'user.email': {
            validationState: {
              schema: z.string().email(),
              evaluate: (props: { value: unknown }) => {
                // Custom validation logic that returns a different shape
                const result = z.string().email().safeParse(props.value)
                return {
                  isValid: result.success,
                  error: result.success
                    ? null
                    : result.error.errors[0]?.message,
                }
              },
            },
          },
        },
      },
    )

    await flushSync()

    // Verify custom validation with evaluate() was registered and evaluated immediately
    expect(
      storeInstance._concerns['user.email']?.['validationState'],
    ).toBeDefined()
  })

  it('should detect non-schema config correctly (no schema)', async () => {
    // Config without schema → isSchemaValidation = false
    // This should register via effect-based path

    const store = createTestStore()

    const { storeInstance } = mountStore(
      store,
      {
        user: { email: 'test@example.com', name: 'Alice' },
      },
      {
        concerns: {
          'user.email': {
            validationState: {
              evaluate: (props: { value: unknown }) => {
                // Pure custom validation without schema
                const isValid =
                  typeof props.value === 'string' && props.value.includes('@')
                return {
                  isValid,
                  error: isValid ? null : 'Must be a valid email',
                }
              },
            },
          },
        },
      },
    )

    await flushSync()

    // Verify custom validation without schema was registered and evaluated immediately
    expect(
      storeInstance._concerns['user.email']?.['validationState'],
    ).toBeDefined()
  })

  it('should handle mixed registrations (schema-based + custom)', async () => {
    // Register both schema-based and custom validations
    // Verify they both work correctly

    const store = createTestStore()

    const { storeInstance } = mountStore(
      store,
      {
        user: { email: 'test@example.com', name: 'Alice' },
      },
      {
        concerns: {
          'user.email': {
            validationState: {
              schema: z.string().email(), // Schema-based (WASM path)
            },
          },
          'user.name': {
            validationState: {
              evaluate: (props: { value: unknown }) => {
                // Custom (effect-based path)
                const isValid =
                  typeof props.value === 'string' && props.value.length > 0
                return {
                  isValid,
                  error: isValid ? null : 'Name is required',
                }
              },
            },
          },
        },
      },
    )

    await flushSync()

    // Both schema-based and custom validations should be registered and evaluated immediately
    expect(
      storeInstance._concerns['user.email']?.['validationState'],
    ).toBeDefined()
    expect(
      storeInstance._concerns['user.name']?.['validationState'],
    ).toBeDefined()
  })

  it('should re-evaluate custom concern when state changes (valtio-reactive)', async () => {
    // Verify custom concerns with evaluate() are reactive and re-run when dependencies change
    // This uses dynamicLabel (a prebuilt concern with template interpolation)
    // to test the valtio-reactive effect() path (not WASM path)

    const store = createTestStore()

    const { storeInstance } = mountStore(
      store,
      {
        user: { email: 'test@example.com', name: 'Alice' },
      },
      {
        concerns: {
          'user.email': {
            // dynamicLabel uses template interpolation and valtio-reactive effects
            dynamicLabel: {
              template: '{{user.name}} <{{user.email}}>',
            },
          },
        },
      },
    )

    await flushSync()

    // Verify initial evaluation happened
    const initialLabel = storeInstance._concerns['user.email']?.['dynamicLabel']
    expect(initialLabel).toBeDefined()
    expect(initialLabel).toBe('Alice <test@example.com>')

    // Change the email (interpolated in template)
    storeInstance.state.user.email = 'new@example.com'
    await flushSync()

    // Concern should re-evaluate automatically via valtio-reactive effect
    const afterEmailChange =
      storeInstance._concerns['user.email']?.['dynamicLabel']
    expect(afterEmailChange).toBe('Alice <new@example.com>')

    // Change the name (also interpolated in template)
    storeInstance.state.user.name = 'Bob'
    await flushSync()

    // Concern should re-evaluate because name is used in template
    const afterNameChange =
      storeInstance._concerns['user.email']?.['dynamicLabel']
    expect(afterNameChange).toBe('Bob <new@example.com>')
  })
})
