/**
 * Type tests for DeepKey utility
 *
 * These tests verify that DeepKey correctly generates all possible
 * dot-notation paths for nested objects and arrays.
 */

import { describe, expectTypeOf, it } from 'vitest'

import type { DeepKey } from '~/types/deep-key'

// Simple object type
interface SimpleUser {
  name: string
  age: number
}

// Nested object type
interface NestedUser {
  name: string
  address: {
    street: string
    city: string
  }
}

// Deep nested object type (5 levels)
interface DeepNested {
  level1: {
    level2: {
      level3: {
        level4: {
          level5: string
        }
      }
    }
  }
}

// Type with arrays
interface WithArray {
  users: {
    name: string
    age: number
  }[]
  count: number
}

// Type with optional properties
interface WithOptional {
  name: string
  email?: string
  profile?: {
    bio: string
    avatar?: string
  }
}

describe('DeepKey', () => {
  it('handles simple object', () => {
    type Keys = DeepKey<SimpleUser>
    type Expected = 'name' | 'age'

    expectTypeOf<Keys>().toMatchTypeOf<Expected>()
    expectTypeOf<Expected>().toMatchTypeOf<Keys>()
  })

  it('handles nested object', () => {
    type Keys = DeepKey<NestedUser>
    type Expected = 'name' | 'address' | 'address.street' | 'address.city'

    expectTypeOf<Keys>().toMatchTypeOf<Expected>()
    expectTypeOf<Expected>().toMatchTypeOf<Keys>()
  })

  it('handles deep nested (5 levels)', () => {
    type Keys = DeepKey<DeepNested>
    type Expected =
      | 'level1'
      | 'level1.level2'
      | 'level1.level2.level3'
      | 'level1.level2.level3.level4'
      | 'level1.level2.level3.level4.level5'

    expectTypeOf<Keys>().toMatchTypeOf<Expected>()
    expectTypeOf<Expected>().toMatchTypeOf<Keys>()
  })

  it('handles with arrays', () => {
    type Keys = DeepKey<WithArray>
    type Expected = 'users' | 'count'

    expectTypeOf<Keys>().toMatchTypeOf<Expected>()
    expectTypeOf<Expected>().toMatchTypeOf<Keys>()
  })

  it('handles with optional properties', () => {
    type Keys = DeepKey<WithOptional>
    type Expected =
      | 'name'
      | 'email'
      | 'profile'
      | 'profile.bio'
      | 'profile.avatar'

    expectTypeOf<Keys>().toMatchTypeOf<Expected>()
    expectTypeOf<Expected>().toMatchTypeOf<Keys>()
  })

  it('handles complex nested structure', () => {
    interface ComplexType {
      id: string
      user: {
        profile: {
          personal: {
            name: string
            age: number
          }
          contact: {
            email: string
            phone?: string
          }
        }
        settings: {
          theme: string
        }
      }
      items: {
        title: string
        value: number
      }[]
    }

    type Keys = DeepKey<ComplexType>

    // Just verify the type exists and has expected keys
    type HasId = 'id' extends Keys ? true : false
    type HasUser = 'user' extends Keys ? true : false
    type HasUserProfile = 'user.profile' extends Keys ? true : false
    type HasUserSettings = 'user.settings' extends Keys ? true : false
    type HasItems = 'items' extends Keys ? true : false

    expectTypeOf<HasId>().toEqualTypeOf<true>()
    expectTypeOf<HasUser>().toEqualTypeOf<true>()
    expectTypeOf<HasUserProfile>().toEqualTypeOf<true>()
    expectTypeOf<HasUserSettings>().toEqualTypeOf<true>()
    expectTypeOf<HasItems>().toEqualTypeOf<true>()
  })

  it('should not accept invalid paths', () => {
    type Keys = DeepKey<NestedUser>

    // Verify invalid paths are not assignable
    type IsValidPath = 'invalid' extends Keys ? true : false
    type IsInvalidNestedPath = 'address.invalid' extends Keys ? true : false
    type IsWrongSeparator = 'address/street' extends Keys ? true : false

    expectTypeOf<IsValidPath>().toEqualTypeOf<false>()
    expectTypeOf<IsInvalidNestedPath>().toEqualTypeOf<false>()
    expectTypeOf<IsWrongSeparator>().toEqualTypeOf<false>()
  })

  describe('Record support with [*] wildcard', () => {
    it('handles simple Record<string, T>', () => {
      type Users = Record<string, { name: string; age: number }>
      type Keys = DeepKey<Users>
      type Expected = '[*]' | '[*].name' | '[*].age'

      expectTypeOf<Keys>().toMatchTypeOf<Expected>()
      expectTypeOf<Expected>().toMatchTypeOf<Keys>()
    })

    it('handles Record with nested object value', () => {
      interface State {
        users: Record<string, { name: string; age: number }>
        title: string
      }
      type Keys = DeepKey<State>

      type HasUsers = 'users' extends Keys ? true : false
      type HasUserWildcard = 'users.[*]' extends Keys ? true : false
      type HasUserName = 'users.[*].name' extends Keys ? true : false
      type HasUserAge = 'users.[*].age' extends Keys ? true : false
      type HasTitle = 'title' extends Keys ? true : false

      expectTypeOf<HasUsers>().toEqualTypeOf<true>()
      expectTypeOf<HasUserWildcard>().toEqualTypeOf<true>()
      expectTypeOf<HasUserName>().toEqualTypeOf<true>()
      expectTypeOf<HasUserAge>().toEqualTypeOf<true>()
      expectTypeOf<HasTitle>().toEqualTypeOf<true>()
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
      type Keys = DeepKey<State>

      type HasBooks = 'books' extends Keys ? true : false
      type HasBooksWildcard = 'books.[*]' extends Keys ? true : false
      type HasBooksTitle = 'books.[*].title' extends Keys ? true : false
      type HasBooksProducts = 'books.[*].products' extends Keys ? true : false
      type HasBooksProductsWildcard = 'books.[*].products.[*]' extends Keys
        ? true
        : false
      type HasBooksProductsPrice = 'books.[*].products.[*].price' extends Keys
        ? true
        : false

      expectTypeOf<HasBooks>().toEqualTypeOf<true>()
      expectTypeOf<HasBooksWildcard>().toEqualTypeOf<true>()
      expectTypeOf<HasBooksTitle>().toEqualTypeOf<true>()
      expectTypeOf<HasBooksProducts>().toEqualTypeOf<true>()
      expectTypeOf<HasBooksProductsWildcard>().toEqualTypeOf<true>()
      expectTypeOf<HasBooksProductsPrice>().toEqualTypeOf<true>()
    })

    it('handles Record alongside concrete keys', () => {
      interface MixedState {
        id: string
        items: Record<string, { value: number; label: string }>
        name: string
      }
      type Keys = DeepKey<MixedState>

      type HasId = 'id' extends Keys ? true : false
      type HasName = 'name' extends Keys ? true : false
      type HasItems = 'items' extends Keys ? true : false
      type HasItemsWildcard = 'items.[*]' extends Keys ? true : false
      type HasItemsValue = 'items.[*].value' extends Keys ? true : false
      type HasItemsLabel = 'items.[*].label' extends Keys ? true : false

      expectTypeOf<HasId>().toEqualTypeOf<true>()
      expectTypeOf<HasName>().toEqualTypeOf<true>()
      expectTypeOf<HasItems>().toEqualTypeOf<true>()
      expectTypeOf<HasItemsWildcard>().toEqualTypeOf<true>()
      expectTypeOf<HasItemsValue>().toEqualTypeOf<true>()
      expectTypeOf<HasItemsLabel>().toEqualTypeOf<true>()
    })

    it('handles Record<string, T> with primitive value', () => {
      type StringMap = Record<string, string>
      type Keys = DeepKey<StringMap>
      type Expected = '[*]'

      expectTypeOf<Keys>().toMatchTypeOf<Expected>()
      expectTypeOf<Expected>().toMatchTypeOf<Keys>()
    })

    it('handles Record<string, number>', () => {
      type NumberMap = Record<string, number>
      type Keys = DeepKey<NumberMap>
      type Expected = '[*]'

      expectTypeOf<Keys>().toMatchTypeOf<Expected>()
      expectTypeOf<Expected>().toMatchTypeOf<Keys>()
    })
  })
})
