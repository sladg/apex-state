/**
 * Tests for concern test utilities
 *
 * Verifies that createTestStore utility works correctly for unit testing concerns
 */

import { describe, expect, it } from 'vitest'
import { z } from 'zod'

import { waitForEffects } from '../concerns/test-utils'
import type { ConcernType } from './concerns'
import { createTestStore } from './concerns'

describe('createTestStore', () => {
  it('creates a working test store with concern registration', async () => {
    const validationState: ConcernType = {
      name: 'validationState',
      evaluate: (props: any) => {
        const result = props.schema.safeParse(props.value)
        return {
          isError: !result.success,
          errors: result.success ? [] : result.error.errors,
        }
      },
    }

    const store = createTestStore({ user: { age: 25 } }, { validationState })

    const dispose = store.useConcerns('test', {
      'user.age': {
        validationState: { schema: z.number().min(18) },
      },
    })

    await waitForEffects()

    const concerns = store.getFieldConcerns('user.age')
    expect(concerns['validationState']).toBeDefined()

    store.proxy.user.age = 10

    await waitForEffects()

    const concernsAfter = store.getFieldConcerns('user.age')
    expect(concernsAfter['validationState']).toBeDefined()

    dispose()
  })

  it('handles multiple concerns on the same path', async () => {
    const validationState: ConcernType = {
      name: 'validationState',
      evaluate: (props: any) => {
        const result = props.schema.safeParse(props.value)
        return {
          isError: !result.success,
          errors: result.success ? [] : result.error.errors,
        }
      },
    }

    const tooltip: ConcernType = {
      name: 'tooltip',
      evaluate: (props: any) => {
        return props.template.replace('{{value}}', String(props.value))
      },
    }

    const store = createTestStore(
      { user: { age: 25 } },
      { validationState, tooltip },
    )

    store.useConcerns('test', {
      'user.age': {
        validationState: { schema: z.number().min(18) },
        tooltip: { template: 'Age: {{value}}' },
      },
    })

    await waitForEffects()

    const concerns = store.getFieldConcerns('user.age')
    expect(concerns['validationState']).toBeDefined()
    expect(concerns['tooltip']).toBe('Age: 25')
  })

  it('properly cleans up on dispose', async () => {
    const validationState: ConcernType = {
      name: 'validationState',
      evaluate: (props: any) => {
        const result = props.schema.safeParse(props.value)
        return {
          isError: !result.success,
          errors: result.success ? [] : result.error.errors,
        }
      },
    }

    const store = createTestStore({ user: { age: 25 } }, { validationState })

    const dispose = store.useConcerns('test', {
      'user.age': {
        validationState: { schema: z.number().min(18) },
      },
    })

    await waitForEffects()

    const concerns = store.getFieldConcerns('user.age')
    expect(concerns['validationState']).toBeDefined()

    dispose()

    const concernsAfterDispose = store.getFieldConcerns('user.age')
    expect(Object.keys(concernsAfterDispose)).toHaveLength(0)
  })

  it('handles multiple registrations with different ids', async () => {
    const validationState: ConcernType = {
      name: 'validationState',
      evaluate: (props: any) => {
        const result = props.schema.safeParse(props.value)
        return {
          isError: !result.success,
          errors: result.success ? [] : result.error.errors,
        }
      },
    }

    const store = createTestStore({ user: { age: 25 } }, { validationState })

    store.useConcerns('test-1', {
      'user.age': {
        validationState: { schema: z.number().min(18) },
      },
    })

    store.useConcerns('test-2', {
      'user.age': {
        validationState: { schema: z.number().max(100) },
      },
    })

    await waitForEffects()

    const concerns = store.getFieldConcerns('user.age')
    expect(concerns['validationState']).toBeDefined()
  })
})
