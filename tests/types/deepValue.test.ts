/**
 * Type tests for DeepValue utility
 *
 * These tests verify that DeepValue correctly extracts the value type
 * for any valid dot-notation path.
 */

import { expectTypeOf, test } from 'vitest'

import type { DeepValue } from '../../src/types/deepValue'

// Test types
interface User {
  name: string
  age: number
  address: {
    street: string
    city: string
    coordinates: {
      lat: number
      lng: number
    }
  }
  tags: string[]
  friends: {
    name: string
    email: string
  }[]
  metadata?: {
    createdAt: Date
    updatedAt?: Date
  }
}

test('DeepValue - top level properties', () => {
  expectTypeOf<DeepValue<User, 'name'>>().toEqualTypeOf<string>()
  expectTypeOf<DeepValue<User, 'age'>>().toEqualTypeOf<number>()
  expectTypeOf<DeepValue<User, 'tags'>>().toEqualTypeOf<string[]>()
})

test('DeepValue - nested object properties', () => {
  expectTypeOf<DeepValue<User, 'address'>>().toEqualTypeOf<{
    street: string
    city: string
    coordinates: {
      lat: number
      lng: number
    }
  }>()

  expectTypeOf<DeepValue<User, 'address.street'>>().toEqualTypeOf<string>()
  expectTypeOf<DeepValue<User, 'address.city'>>().toEqualTypeOf<string>()
})

test('DeepValue - deep nested properties', () => {
  expectTypeOf<DeepValue<User, 'address.coordinates'>>().toEqualTypeOf<{
    lat: number
    lng: number
  }>()

  expectTypeOf<
    DeepValue<User, 'address.coordinates.lat'>
  >().toEqualTypeOf<number>()
  expectTypeOf<
    DeepValue<User, 'address.coordinates.lng'>
  >().toEqualTypeOf<number>()
})

test('DeepValue - array types', () => {
  expectTypeOf<DeepValue<User, 'friends'>>().toEqualTypeOf<
    {
      name: string
      email: string
    }[]
  >()

  expectTypeOf<DeepValue<User, `${number}`>>().toEqualTypeOf<unknown>()
})

test('DeepValue - array element properties', () => {
  // Arrays are treated as a whole, not indexed
  expectTypeOf<DeepValue<User, 'friends'>>().toEqualTypeOf<
    {
      name: string
      email: string
    }[]
  >()
})

test('DeepValue - optional properties', () => {
  type MetadataValue = DeepValue<User, 'metadata'>

  expectTypeOf<MetadataValue>().toMatchTypeOf<
    | {
        createdAt: Date
        updatedAt?: Date
      }
    | undefined
  >()
})

test('DeepValue - complex nested structure', () => {
  interface ComplexType {
    level1: {
      level2: {
        level3: {
          value: string
          nested: {
            deep: number
          }
        }
      }
    }
  }

  expectTypeOf<
    DeepValue<ComplexType, 'level1.level2.level3.value'>
  >().toEqualTypeOf<string>()

  expectTypeOf<
    DeepValue<ComplexType, 'level1.level2.level3.nested.deep'>
  >().toEqualTypeOf<number>()

  expectTypeOf<DeepValue<ComplexType, 'level1.level2.level3'>>().toEqualTypeOf<{
    value: string
    nested: {
      deep: number
    }
  }>()
})

test('DeepValue - should return unknown for invalid paths', () => {
  expectTypeOf<DeepValue<User, 'invalid'>>().toEqualTypeOf<unknown>()
  expectTypeOf<DeepValue<User, 'address.invalid'>>().toEqualTypeOf<unknown>()
})

test('DeepValue - real world example', () => {
  interface AppState {
    user: {
      profile: {
        name: string
        email: string
      }
      settings: {
        theme: 'light' | 'dark'
        notifications: boolean
      }
    }
    items: {
      id: string
      title: string
    }[]
  }

  expectTypeOf<
    DeepValue<AppState, 'user.profile.name'>
  >().toEqualTypeOf<string>()

  expectTypeOf<DeepValue<AppState, 'user.settings.theme'>>().toEqualTypeOf<
    'light' | 'dark'
  >()

  expectTypeOf<
    DeepValue<AppState, 'user.settings.notifications'>
  >().toEqualTypeOf<boolean>()

  expectTypeOf<DeepValue<AppState, 'items'>>().toEqualTypeOf<
    {
      id: string
      title: string
    }[]
  >()
})
