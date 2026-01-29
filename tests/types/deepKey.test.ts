/**
 * Type tests for DeepKey utility
 *
 * These tests verify that DeepKey correctly generates all possible
 * dot-notation paths for nested objects and arrays.
 */

import { describe, expectTypeOf, it } from 'vitest'

import type { DeepKey } from '../../src/types/deepKey'

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
})
