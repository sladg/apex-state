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

describe('DeepKeyFiltered with nullable fields', () => {
  interface NullableData {
    price: number
    discount: number | undefined
    tax: number | null
    bonus?: number
    name: string
    label: string | undefined
    title?: string
    isActive: boolean
    isHidden: boolean | undefined
    isVerified?: boolean
    nested: {
      amount: number
      optional?: number
    }
  }

  test('includes number | undefined paths when filtering for number', () => {
    type NumberPaths = DeepKeyFiltered<NullableData, number>

    expectTypeOf<NumberPaths>().toEqualTypeOf<
      | 'price'
      | 'discount'
      | 'tax'
      | 'bonus'
      | 'nested.amount'
      | 'nested.optional'
    >()
  })

  test('includes string | undefined paths when filtering for string', () => {
    type StringPaths = DeepKeyFiltered<NullableData, string>

    expectTypeOf<StringPaths>().toEqualTypeOf<'name' | 'label' | 'title'>()
  })

  test('includes boolean | undefined paths when filtering for boolean', () => {
    type BooleanPaths = DeepKeyFiltered<NullableData, boolean>

    expectTypeOf<BooleanPaths>().toEqualTypeOf<
      'isActive' | 'isHidden' | 'isVerified'
    >()
  })

  test('does not include object paths when filtering for primitives', () => {
    type NumberPaths = DeepKeyFiltered<NullableData, number>

    // 'nested' is an object, should not appear in number paths
    expectTypeOf<'nested'>().not.toMatchTypeOf<NumberPaths>()
  })
})

describe('DeepKeyFiltered — null/undefined intermediate handling', () => {
  // Three flavours of nullable intermediate:
  //   optional (?:)     → T | undefined
  //   nullable (| null) → T | null
  //   both              → T | null | undefined
  interface Mixed {
    // optional intermediate (undefined)
    optUser?: {
      name: string
      score: number
    }
    // nullable intermediate (null)
    nullUser: {
      name: string
      score: number
    } | null
    // both null and undefined
    bothUser:
      | {
          name: string
          score: number
        }
      | null
      | undefined
    // object intermediate (non-nullable, for comparison)
    nested: {
      label?: string // optional leaf only
      count?: number
    }
    active: boolean
    root: string
  }

  test('filter for string — optional intermediate (|undefined)', () => {
    type StringPaths = DeepKeyFiltered<Mixed, string>

    expectTypeOf<StringPaths>().toEqualTypeOf<
      | 'root'
      | 'nested.label'
      | 'optUser.name'
      | 'nullUser.name'
      | 'bothUser.name'
    >()
  })

  test('filter for string|undefined — optional intermediate', () => {
    type StringPaths = DeepKeyFiltered<Mixed, string | undefined>

    expectTypeOf<StringPaths>().toEqualTypeOf<
      | 'root'
      | 'nested.label'
      | 'optUser.name'
      | 'nullUser.name'
      | 'bothUser.name'
    >()
  })

  test('filter for string|null — nullable intermediate', () => {
    type StringPaths = DeepKeyFiltered<Mixed, string | null>

    expectTypeOf<StringPaths>().toEqualTypeOf<
      | 'root'
      | 'nested.label'
      | 'optUser.name'
      | 'nullUser.name'
      | 'bothUser.name'
    >()
  })

  test('filter for number — all three nullable intermediate flavours', () => {
    type NumberPaths = DeepKeyFiltered<Mixed, number>

    expectTypeOf<NumberPaths>().toEqualTypeOf<
      'nested.count' | 'optUser.score' | 'nullUser.score' | 'bothUser.score'
    >()
  })

  test('filter for object type — paths resolving to an object shape', () => {
    interface UserShape {
      name: string
      score: number
    }
    type ObjectPaths = DeepKeyFiltered<Mixed, UserShape>

    // 'optUser', 'nullUser', 'bothUser' all resolve to UserShape (after stripping null/undefined)
    expectTypeOf<ObjectPaths>().toEqualTypeOf<
      'optUser' | 'nullUser' | 'bothUser'
    >()
  })

  test('does not include wrong-type paths through nullable intermediates', () => {
    type BoolPaths = DeepKeyFiltered<Mixed, boolean>

    // 'active' is boolean, but optUser.name / nullUser.name / bothUser.name are string
    expectTypeOf<'active'>().toMatchTypeOf<BoolPaths>()
    expectTypeOf<'optUser.name'>().not.toMatchTypeOf<BoolPaths>()
    expectTypeOf<'nullUser.name'>().not.toMatchTypeOf<BoolPaths>()
    expectTypeOf<'bothUser.name'>().not.toMatchTypeOf<BoolPaths>()
  })
})
