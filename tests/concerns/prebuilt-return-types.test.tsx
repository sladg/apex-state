/**
 * Prebuilt Concerns Return Type Tests
 *
 * Verifies that all prebuilt concerns return correctly typed values.
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

import { beforeEach, describe, expect, it } from 'vitest'
import { z } from 'zod'

import { createGenericStore } from '../../src'
import type { ValidationStateResult } from '../../src/concerns/prebuilts'
import type { StoreInstance } from '../../src/core/types'
import { flush, renderWithStore } from '../utils/react'

interface TestState {
  name: string
  age: number
  status: 'active' | 'inactive'
  settings: {
    theme: 'light' | 'dark'
    notifications: boolean
  }
}

const initialState: TestState = {
  name: 'John',
  age: 30,
  status: 'active',
  settings: {
    theme: 'light',
    notifications: true,
  },
}

describe('Prebuilt Concerns Return Types', () => {
  let storeInstance: StoreInstance<TestState>

  const setup = async (concerns: Record<string, Record<string, unknown>>) => {
    const store = createGenericStore<TestState>()
    const result = renderWithStore(store, structuredClone(initialState), {
      concerns,
    })
    storeInstance = result.storeInstance
    await flush()
  }

  describe('validationState concern', () => {
    beforeEach(() =>
      setup({
        name: {
          validationState: {
            schema: z.string().min(1, 'Name is required'),
          },
        },
      }),
    )

    it('returns ValidationStateResult with isError and errors', () => {
      const validation = storeInstance._concerns['name']?.[
        'validationState'
      ] as ValidationStateResult | undefined

      expect(validation?.isError).toBe(false)
      expect(validation?.errors).toHaveLength(0)
    })

    it('returns validation errors when schema fails', async () => {
      // Re-setup with a schema that the initial value ('John', 4 chars) will fail
      await setup({
        name: {
          validationState: {
            schema: z.string().min(10, 'Name must be at least 10 characters'),
          },
        },
      })

      const validation = storeInstance._concerns['name']?.[
        'validationState'
      ] as ValidationStateResult | undefined

      expect(validation?.isError).toBe(true)
      expect(validation?.errors[0]?.message).toBe(
        'Name must be at least 10 characters',
      )
    })
  })

  describe('disabledWhen concern', () => {
    it('returns false when condition does not match', async () => {
      await setup({
        name: {
          disabledWhen: {
            condition: { IS_EQUAL: ['status', 'inactive'] },
          },
        },
      })

      const disabled = storeInstance._concerns['name']?.['disabledWhen']
      expect(disabled).toBe(false)
      expect(typeof disabled).toBe('boolean')
    })

    it('returns true when condition matches', async () => {
      await setup({
        name: {
          disabledWhen: {
            condition: { IS_EQUAL: ['status', 'active'] },
          },
        },
      })

      const disabled = storeInstance._concerns['name']?.['disabledWhen']
      expect(disabled).toBe(true)
    })
  })

  describe('visibleWhen concern', () => {
    it('returns false when condition does not match', async () => {
      await setup({
        'settings.notifications': {
          visibleWhen: {
            condition: { IS_EQUAL: ['settings.theme', 'dark'] },
          },
        },
      })

      const visible =
        storeInstance._concerns['settings.notifications']?.['visibleWhen']
      expect(visible).toBe(false)
      expect(typeof visible).toBe('boolean')
    })

    it('returns true when condition matches', async () => {
      await setup({
        'settings.notifications': {
          visibleWhen: {
            condition: { IS_EQUAL: ['settings.theme', 'light'] },
          },
        },
      })

      const visible =
        storeInstance._concerns['settings.notifications']?.['visibleWhen']
      expect(visible).toBe(true)
    })
  })

  describe('readonlyWhen concern', () => {
    it('returns false when condition does not match', async () => {
      await setup({
        age: {
          readonlyWhen: {
            condition: { IS_EQUAL: ['status', 'inactive'] },
          },
        },
      })

      const readonly = storeInstance._concerns['age']?.['readonlyWhen']
      expect(readonly).toBe(false)
      expect(typeof readonly).toBe('boolean')
    })

    it('returns true when condition matches', async () => {
      await setup({
        age: {
          readonlyWhen: {
            condition: { IS_EQUAL: ['status', 'active'] },
          },
        },
      })

      const readonly = storeInstance._concerns['age']?.['readonlyWhen']
      expect(readonly).toBe(true)
    })
  })

  describe('dynamicTooltip concern', () => {
    it('returns interpolated string', async () => {
      await setup({
        name: {
          dynamicTooltip: {
            template: 'User: {{name}}, Age: {{age}}',
          },
        },
      })

      const tooltip = storeInstance._concerns['name']?.['dynamicTooltip']
      expect(tooltip).toBe('User: John, Age: 30')
      expect(typeof tooltip).toBe('string')
    })

    it('interpolates nested state values', async () => {
      await setup({
        name: {
          dynamicTooltip: {
            template: 'Theme: {{settings.theme}}',
          },
        },
      })

      const tooltip = storeInstance._concerns['name']?.['dynamicTooltip']
      expect(tooltip).toBe('Theme: light')
    })
  })

  describe('dynamicLabel concern', () => {
    it('returns interpolated string', async () => {
      await setup({
        name: {
          dynamicLabel: {
            template: 'Name ({{status}})',
          },
        },
      })

      const label = storeInstance._concerns['name']?.['dynamicLabel']
      expect(label).toBe('Name (active)')
      expect(typeof label).toBe('string')
    })
  })

  describe('dynamicPlaceholder concern', () => {
    it('returns interpolated string', async () => {
      await setup({
        name: {
          dynamicPlaceholder: {
            template: 'Enter name for {{status}} user',
          },
        },
      })

      const placeholder =
        storeInstance._concerns['name']?.['dynamicPlaceholder']
      expect(placeholder).toBe('Enter name for active user')
      expect(typeof placeholder).toBe('string')
    })
  })

  describe('Multiple concerns on same field', () => {
    it('returns all concerns with correct values', async () => {
      await setup({
        name: {
          validationState: { schema: z.string().min(1) },
          disabledWhen: {
            condition: { IS_EQUAL: ['status', 'inactive'] },
          },
          dynamicTooltip: { template: 'Current: {{name}}' },
        },
      })

      const nameConcerns = storeInstance._concerns['name']
      const validation = nameConcerns?.['validationState'] as
        | ValidationStateResult
        | undefined

      expect(validation?.isError).toBe(false)
      expect(nameConcerns?.['disabledWhen']).toBe(false)
      expect(nameConcerns?.['dynamicTooltip']).toBe('Current: John')
    })
  })
})
