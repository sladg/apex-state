/**
 * Prebuilt Concerns Return Type Tests
 *
 * Verifies that all prebuilt concerns return correctly typed values
 * when accessed via store.withConcerns().useFieldStore()
 *
 * Expected return types:
 * - validationState → ValidationStateResult { isError: boolean, errors: ValidationError[] }
 * - disabledWhen → boolean
 * - visibleWhen → boolean
 * - readonlyWhen → boolean
 * - dynamicTooltip → string
 * - dynamicLabel → string
 * - dynamicPlaceholder → string
 */

import { describe, expect, expectTypeOf, it } from 'vitest'
import { z } from 'zod'

import { createGenericStore } from '../../src'
import type { ValidationStateResult } from '../../src/concerns/prebuilts'
import { flushEffects, mountStore } from '../utils/react'

interface TestState {
  name: string
  age: number
  status: 'active' | 'inactive'
  settings: {
    theme: 'light' | 'dark'
    notifications: boolean
  }
}

describe('Prebuilt Concerns Return Types', () => {
  const createTestStore = () => createGenericStore<TestState>()

  const initialState: TestState = {
    name: 'John',
    age: 30,
    status: 'active',
    settings: {
      theme: 'light',
      notifications: true,
    },
  }

  describe('validationState concern', () => {
    it('returns ValidationStateResult with isError and errors', async () => {
      const store = createTestStore()

      function TestComponent() {
        store.useConcerns('test', {
          name: {
            validationState: {
              schema: z.string().min(1, 'Name is required'),
            },
          },
        })

        const field = store
          .withConcerns({ validationState: true })
          .useFieldStore('name')

        // Type assertion - validationState should be ValidationStateResult | undefined
        const validation = field.validationState

        return (
          <div>
            <span data-testid="is-error">
              {String(validation?.isError ?? 'undefined')}
            </span>
            <span data-testid="errors-length">
              {String(validation?.errors?.length ?? 'undefined')}
            </span>
            <span data-testid="first-error">
              {validation?.errors?.[0]?.message ?? 'no-error'}
            </span>
          </div>
        )
      }

      mountStore(<TestComponent />, store, initialState)
      await flushEffects()

      // Runtime assertions
      expect(
        document.querySelector('[data-testid="is-error"]'),
      ).toHaveTextContent('false')
      expect(
        document.querySelector('[data-testid="errors-length"]'),
      ).toHaveTextContent('0')
    })

    it('has correctly typed ValidationStateResult structure', async () => {
      const store = createTestStore()

      function TypeCheckComponent() {
        store.useConcerns('test', {
          name: {
            validationState: {
              schema: z.string().min(5, 'Too short'),
            },
          },
        })

        const field = store
          .withConcerns({ validationState: true })
          .useFieldStore('name')

        // Type check: validationState should match ValidationStateResult
        if (field.validationState) {
          // These should be valid accesses based on ValidationStateResult type
          const isError = field.validationState.isError
          const errors = field.validationState.errors

          // Type check that errors have correct shape
          expectTypeOf(isError).toBeBoolean()
          expectTypeOf(errors).toBeArray()

          if (errors.length > 0) {
            expectTypeOf(errors[0]!.message).toBeString()
          }
        }

        return <div data-testid="type-check">checked</div>
      }

      mountStore(<TypeCheckComponent />, store, initialState)
      await flushEffects()

      expect(
        document.querySelector('[data-testid="type-check"]'),
      ).toHaveTextContent('checked')
    })

    it('returns validation errors when schema fails', async () => {
      const store = createTestStore()

      function ValidationErrorComponent() {
        store.useConcerns('test', {
          name: {
            validationState: {
              schema: z.string().min(10, 'Name must be at least 10 characters'),
            },
          },
        })

        const field = store
          .withConcerns({ validationState: true })
          .useFieldStore('name')

        return (
          <div>
            <span data-testid="is-error">
              {String(field.validationState?.isError)}
            </span>
            <span data-testid="error-message">
              {field.validationState?.errors[0]?.message ?? 'none'}
            </span>
          </div>
        )
      }

      // Name is 'John' (4 chars), should fail min(10) validation
      mountStore(<ValidationErrorComponent />, store, initialState)
      await flushEffects()

      expect(
        document.querySelector('[data-testid="is-error"]'),
      ).toHaveTextContent('true')
      expect(
        document.querySelector('[data-testid="error-message"]'),
      ).toHaveTextContent('Name must be at least 10 characters')
    })
  })

  describe('disabledWhen concern', () => {
    it('returns boolean type', async () => {
      const store = createTestStore()

      function TestComponent() {
        store.useConcerns('test', {
          name: {
            disabledWhen: {
              condition: { IS_EQUAL: ['status', 'inactive'] },
            },
          },
        })

        const field = store
          .withConcerns({ disabledWhen: true })
          .useFieldStore('name')

        // Type assertion - disabledWhen should be boolean | undefined
        const isDisabled = field.disabledWhen

        return (
          <div>
            <span data-testid="disabled">{String(isDisabled)}</span>
            <span data-testid="type">{typeof isDisabled}</span>
          </div>
        )
      }

      mountStore(<TestComponent />, store, initialState)
      await flushEffects()

      // Status is 'active', so should not be disabled
      expect(
        document.querySelector('[data-testid="disabled"]'),
      ).toHaveTextContent('false')

      expect(document.querySelector('[data-testid="type"]')).toHaveTextContent(
        'boolean',
      )
    })

    it('returns true when condition matches', async () => {
      const store = createTestStore()

      function TestComponent() {
        store.useConcerns('test', {
          name: {
            disabledWhen: {
              condition: { IS_EQUAL: ['status', 'active'] },
            },
          },
        })

        const field = store
          .withConcerns({ disabledWhen: true })
          .useFieldStore('name')

        return <span data-testid="disabled">{String(field.disabledWhen)}</span>
      }

      mountStore(<TestComponent />, store, initialState)
      await flushEffects()

      expect(
        document.querySelector('[data-testid="disabled"]'),
      ).toHaveTextContent('true')
    })
  })

  describe('visibleWhen concern', () => {
    it('returns boolean type', async () => {
      const store = createTestStore()

      function TestComponent() {
        store.useConcerns('test', {
          'settings.notifications': {
            visibleWhen: {
              condition: { IS_EQUAL: ['settings.theme', 'dark'] },
            },
          },
        })

        const field = store
          .withConcerns({ visibleWhen: true })
          .useFieldStore('settings.notifications')

        return (
          <div>
            <span data-testid="visible">{String(field.visibleWhen)}</span>
            <span data-testid="type">{typeof field.visibleWhen}</span>
          </div>
        )
      }

      mountStore(<TestComponent />, store, initialState)
      await flushEffects()

      // Theme is 'light', condition checks for 'dark', so should be false
      expect(
        document.querySelector('[data-testid="visible"]'),
      ).toHaveTextContent('false')
      expect(document.querySelector('[data-testid="type"]')).toHaveTextContent(
        'boolean',
      )
    })

    it('returns true when condition matches', async () => {
      const store = createTestStore()

      function TestComponent() {
        store.useConcerns('test', {
          'settings.notifications': {
            visibleWhen: {
              condition: { IS_EQUAL: ['settings.theme', 'light'] },
            },
          },
        })

        const field = store
          .withConcerns({ visibleWhen: true })
          .useFieldStore('settings.notifications')

        return <span data-testid="visible">{String(field.visibleWhen)}</span>
      }

      mountStore(<TestComponent />, store, initialState)
      await flushEffects()

      expect(
        document.querySelector('[data-testid="visible"]'),
      ).toHaveTextContent('true')
    })
  })

  describe('readonlyWhen concern', () => {
    it('returns boolean type', async () => {
      const store = createTestStore()

      function TestComponent() {
        store.useConcerns('test', {
          age: {
            readonlyWhen: {
              condition: { IS_EQUAL: ['status', 'inactive'] },
            },
          },
        })

        const field = store
          .withConcerns({ readonlyWhen: true })
          .useFieldStore('age')

        return (
          <div>
            <span data-testid="readonly">{String(field.readonlyWhen)}</span>
            <span data-testid="type">{typeof field.readonlyWhen}</span>
          </div>
        )
      }

      mountStore(<TestComponent />, store, initialState)
      await flushEffects()

      expect(
        document.querySelector('[data-testid="readonly"]'),
      ).toHaveTextContent('false')
      expect(document.querySelector('[data-testid="type"]')).toHaveTextContent(
        'boolean',
      )
    })

    it('returns true when condition matches', async () => {
      const store = createTestStore()

      function TestComponent() {
        store.useConcerns('test', {
          age: {
            readonlyWhen: {
              condition: { IS_EQUAL: ['status', 'active'] },
            },
          },
        })

        const field = store
          .withConcerns({ readonlyWhen: true })
          .useFieldStore('age')

        return <span data-testid="readonly">{String(field.readonlyWhen)}</span>
      }

      mountStore(<TestComponent />, store, initialState)
      await flushEffects()

      expect(
        document.querySelector('[data-testid="readonly"]'),
      ).toHaveTextContent('true')
    })
  })

  describe('dynamicTooltip concern', () => {
    it('returns string type', async () => {
      const store = createTestStore()

      function TestComponent() {
        store.useConcerns('test', {
          name: {
            dynamicTooltip: {
              template: 'User: {{name}}, Age: {{age}}',
            },
          },
        })

        const field = store
          .withConcerns({ dynamicTooltip: true })
          .useFieldStore('name')

        return (
          <div>
            <span data-testid="tooltip">{field.dynamicTooltip}</span>
            <span data-testid="type">{typeof field.dynamicTooltip}</span>
          </div>
        )
      }

      mountStore(<TestComponent />, store, initialState)
      await flushEffects()

      expect(
        document.querySelector('[data-testid="tooltip"]'),
      ).toHaveTextContent('User: John, Age: 30')
      expect(document.querySelector('[data-testid="type"]')).toHaveTextContent(
        'string',
      )
    })

    it('interpolates nested state values', async () => {
      const store = createTestStore()

      function TestComponent() {
        store.useConcerns('test', {
          name: {
            dynamicTooltip: {
              template: 'Theme: {{settings.theme}}',
            },
          },
        })

        const field = store
          .withConcerns({ dynamicTooltip: true })
          .useFieldStore('name')

        return <span data-testid="tooltip">{field.dynamicTooltip}</span>
      }

      mountStore(<TestComponent />, store, initialState)
      await flushEffects()

      expect(
        document.querySelector('[data-testid="tooltip"]'),
      ).toHaveTextContent('Theme: light')
    })
  })

  describe('dynamicLabel concern', () => {
    it('returns string type', async () => {
      const store = createTestStore()

      function TestComponent() {
        store.useConcerns('test', {
          name: {
            dynamicLabel: {
              template: 'Name ({{status}})',
            },
          },
        })

        const field = store
          .withConcerns({ dynamicLabel: true })
          .useFieldStore('name')

        return (
          <div>
            <span data-testid="label">{field.dynamicLabel}</span>
            <span data-testid="type">{typeof field.dynamicLabel}</span>
          </div>
        )
      }

      mountStore(<TestComponent />, store, initialState)
      await flushEffects()

      expect(document.querySelector('[data-testid="label"]')).toHaveTextContent(
        'Name (active)',
      )
      expect(document.querySelector('[data-testid="type"]')).toHaveTextContent(
        'string',
      )
    })
  })

  describe('dynamicPlaceholder concern', () => {
    it('returns string type', async () => {
      const store = createTestStore()

      function TestComponent() {
        store.useConcerns('test', {
          name: {
            dynamicPlaceholder: {
              template: 'Enter name for {{status}} user',
            },
          },
        })

        const field = store
          .withConcerns({ dynamicPlaceholder: true })
          .useFieldStore('name')

        return (
          <div>
            <span data-testid="placeholder">{field.dynamicPlaceholder}</span>
            <span data-testid="type">{typeof field.dynamicPlaceholder}</span>
          </div>
        )
      }

      mountStore(<TestComponent />, store, initialState)
      await flushEffects()

      expect(
        document.querySelector('[data-testid="placeholder"]'),
      ).toHaveTextContent('Enter name for active user')
      expect(document.querySelector('[data-testid="type"]')).toHaveTextContent(
        'string',
      )
    })
  })

  describe('Multiple concerns on same field', () => {
    it('returns all selected concerns with correct types', async () => {
      const store = createTestStore()

      function TestComponent() {
        store.useConcerns('test', {
          name: {
            validationState: {
              schema: z.string().min(1),
            },
            disabledWhen: {
              condition: { IS_EQUAL: ['status', 'inactive'] },
            },
            dynamicTooltip: {
              template: 'Current: {{name}}',
            },
          },
        })

        const field = store
          .withConcerns({
            validationState: true,
            disabledWhen: true,
            dynamicTooltip: true,
          })
          .useFieldStore('name')

        return (
          <div>
            <span data-testid="is-error">
              {String(field.validationState?.isError)}
            </span>
            <span data-testid="disabled">{String(field.disabledWhen)}</span>
            <span data-testid="tooltip">{field.dynamicTooltip}</span>
            <span data-testid="value">{field.value}</span>
          </div>
        )
      }

      mountStore(<TestComponent />, store, initialState)
      await flushEffects()

      expect(
        document.querySelector('[data-testid="is-error"]'),
      ).toHaveTextContent('false')
      expect(
        document.querySelector('[data-testid="disabled"]'),
      ).toHaveTextContent('false')
      expect(
        document.querySelector('[data-testid="tooltip"]'),
      ).toHaveTextContent('Current: John')
      expect(document.querySelector('[data-testid="value"]')).toHaveTextContent(
        'John',
      )
    })
  })

  describe('Static type checking', () => {
    it('withConcerns selection correctly filters available concerns', async () => {
      const store = createTestStore()

      // This is a compile-time type check
      // The withConcerns API should only expose selected concerns

      function TypeCheckComponent() {
        // Only selecting validationState
        const fieldOnlyValidation = store
          .withConcerns({ validationState: true })
          .useFieldStore('name')

        // Should have validationState
        expectTypeOf(fieldOnlyValidation.validationState).toEqualTypeOf<
          ValidationStateResult | undefined
        >()

        // Should have value and setValue from base field store
        expectTypeOf(fieldOnlyValidation.value).toBeString()

        return <div data-testid="type-check">pass</div>
      }

      // Use mountStore options pattern for concerns registration
      mountStore(<TypeCheckComponent />, store, initialState, {
        concerns: {
          name: {
            validationState: { schema: z.string() },
            disabledWhen: { condition: { IS_EQUAL: ['status', 'active'] } },
          },
        },
      })

      await flushEffects()
    })
  })
})
