/**
 * Type tests for DeepValue utility
 *
 * These tests verify that DeepValue correctly extracts the value type
 * for any valid dot-notation path.
 */

import { describe, expectTypeOf, it } from 'vitest'

import type { DeepValue } from '../../src/types/deep-value'

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

describe('DeepValue', () => {
  it('handles top level properties', () => {
    expectTypeOf<DeepValue<User, 'name'>>().toEqualTypeOf<string>()
    expectTypeOf<DeepValue<User, 'age'>>().toEqualTypeOf<number>()
    expectTypeOf<DeepValue<User, 'tags'>>().toEqualTypeOf<string[]>()
  })

  it('handles nested object properties', () => {
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

  it('handles deep nested properties', () => {
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

  it('handles array types', () => {
    expectTypeOf<DeepValue<User, 'friends'>>().toEqualTypeOf<
      {
        name: string
        email: string
      }[]
    >()

    expectTypeOf<DeepValue<User, `${number}`>>().toEqualTypeOf<unknown>()
  })

  it('handles array element properties', () => {
    // Arrays are treated as a whole, not indexed
    expectTypeOf<DeepValue<User, 'friends'>>().toEqualTypeOf<
      {
        name: string
        email: string
      }[]
    >()
  })

  it('handles optional properties', () => {
    type MetadataValue = DeepValue<User, 'metadata'>

    expectTypeOf<MetadataValue>().toMatchTypeOf<
      | {
          createdAt: Date
          updatedAt?: Date
        }
      | undefined
    >()
  })

  it('handles complex nested structure', () => {
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

    expectTypeOf<
      DeepValue<ComplexType, 'level1.level2.level3'>
    >().toEqualTypeOf<{
      value: string
      nested: {
        deep: number
      }
    }>()
  })

  it('should return unknown for invalid paths', () => {
    expectTypeOf<DeepValue<User, 'invalid'>>().toEqualTypeOf<unknown>()
    expectTypeOf<DeepValue<User, 'address.invalid'>>().toEqualTypeOf<unknown>()
  })

  it('handles real world example', () => {
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

  describe('Record support with [*] wildcard', () => {
    it('handles Record<string, T> at leaf', () => {
      type Users = Record<string, { name: string; age: number }>

      expectTypeOf<DeepValue<Users, '[*]'>>().toEqualTypeOf<{
        name: string
        age: number
      }>()
    })

    it('handles nested Record with sub-paths', () => {
      interface State {
        users: Record<string, { name: string; age: number }>
        title: string
      }

      expectTypeOf<DeepValue<State, 'users'>>().toEqualTypeOf<
        Record<string, { name: string; age: number }>
      >()

      expectTypeOf<DeepValue<State, 'users.[*]'>>().toEqualTypeOf<{
        name: string
        age: number
      }>()

      expectTypeOf<DeepValue<State, 'users.[*].name'>>().toEqualTypeOf<string>()

      expectTypeOf<DeepValue<State, 'users.[*].age'>>().toEqualTypeOf<number>()

      expectTypeOf<DeepValue<State, 'title'>>().toEqualTypeOf<string>()
    })

    it('handles nested Records (two levels)', () => {
      interface State {
        books: Record<
          string,
          {
            title: string
            products: Record<string, { price: number; sku: string }>
          }
        >
      }

      expectTypeOf<DeepValue<State, 'books.[*]'>>().toEqualTypeOf<{
        title: string
        products: Record<string, { price: number; sku: string }>
      }>()

      expectTypeOf<
        DeepValue<State, 'books.[*].title'>
      >().toEqualTypeOf<string>()

      expectTypeOf<DeepValue<State, 'books.[*].products'>>().toEqualTypeOf<
        Record<string, { price: number; sku: string }>
      >()

      expectTypeOf<DeepValue<State, 'books.[*].products.[*]'>>().toEqualTypeOf<{
        price: number
        sku: string
      }>()

      expectTypeOf<
        DeepValue<State, 'books.[*].products.[*].price'>
      >().toEqualTypeOf<number>()

      expectTypeOf<
        DeepValue<State, 'books.[*].products.[*].sku'>
      >().toEqualTypeOf<string>()
    })

    it('handles Record<string, T> with primitive value', () => {
      type StringMap = Record<string, string>

      expectTypeOf<DeepValue<StringMap, '[*]'>>().toEqualTypeOf<string>()
    })

    it('handles Record<string, number>', () => {
      type NumberMap = Record<string, number>

      expectTypeOf<DeepValue<NumberMap, '[*]'>>().toEqualTypeOf<number>()
    })

    it('handles Record with complex nested structure', () => {
      interface Product {
        id: string
        variants: Record<
          string,
          {
            color: string
            sizes: Record<string, { inStock: boolean; quantity: number }>
          }
        >
      }

      expectTypeOf<DeepValue<Product, 'variants.[*]'>>().toEqualTypeOf<{
        color: string
        sizes: Record<string, { inStock: boolean; quantity: number }>
      }>()

      expectTypeOf<
        DeepValue<Product, 'variants.[*].sizes.[*]'>
      >().toEqualTypeOf<{ inStock: boolean; quantity: number }>()

      expectTypeOf<
        DeepValue<Product, 'variants.[*].sizes.[*].inStock'>
      >().toEqualTypeOf<boolean>()

      expectTypeOf<
        DeepValue<Product, 'variants.[*].sizes.[*].quantity'>
      >().toEqualTypeOf<number>()
    })
  })
})
