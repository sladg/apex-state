/**
 * Type tests for DeepKeyFiltered utility
 *
 * Tests that DeepKeyFiltered correctly filters paths by their value type.
 */

import { describe, expectTypeOf, test } from 'vitest'

import type { DeepKeyFiltered } from '~/types'

describe('DeepKeyFiltered type utility', () => {
  interface TestData {
    isActive: boolean
    isVerified: boolean
    name: string
    email: string
    count: number
    age: number
    user: {
      isAdmin: boolean
      username: string
      score: number
    }
  }

  test('filters boolean paths correctly', () => {
    type BooleanPaths = DeepKeyFiltered<TestData, boolean>

    expectTypeOf<BooleanPaths>().toEqualTypeOf<
      'isActive' | 'isVerified' | 'user.isAdmin'
    >()
  })

  test('filters string paths correctly', () => {
    type StringPaths = DeepKeyFiltered<TestData, string>

    expectTypeOf<StringPaths>().toEqualTypeOf<
      'name' | 'email' | 'user.username'
    >()
  })

  test('filters number paths correctly', () => {
    type NumberPaths = DeepKeyFiltered<TestData, number>

    expectTypeOf<NumberPaths>().toEqualTypeOf<'count' | 'age' | 'user.score'>()
  })

  test('works with nested object types', () => {
    interface NestedData {
      settings: {
        theme: string
        notifications: {
          enabled: boolean
          frequency: number
        }
      }
    }

    type NestedBooleans = DeepKeyFiltered<NestedData, boolean>

    expectTypeOf<NestedBooleans>().toEqualTypeOf<'settings.notifications.enabled'>()
  })

  test('returns never for non-existent types', () => {
    type DatePaths = DeepKeyFiltered<TestData, Date>

    expectTypeOf<DatePaths>().toEqualTypeOf<never>()
  })
})
