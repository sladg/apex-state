/**
 * Type tests for ArrayOfChanges and GenericMeta
 *
 * These tests verify that the changes array and metadata types
 * work correctly together with proper type safety.
 */

import { expectTypeOf, test } from 'vitest'
import type { ArrayOfChanges, GenericMeta } from '../../src/types'

// Test data types
type User = {
  name: string
  age: number
  email: string
}

type NestedData = {
  user: {
    profile: {
      name: string
      bio: string
    }
    settings: {
      theme: string
    }
  }
  count: number
}

test('GenericMeta - has all required properties', () => {
  type Meta = GenericMeta

  expectTypeOf<Meta>().toHaveProperty('isSyncPathChange')
  expectTypeOf<Meta>().toHaveProperty('isFlipPathChange')
  expectTypeOf<Meta>().toHaveProperty('isProgramaticChange')
  expectTypeOf<Meta>().toHaveProperty('sender')
})

test('GenericMeta - all properties are optional', () => {
  const meta1: GenericMeta = {}
  const meta2: GenericMeta = { sender: 'test' }
  const meta3: GenericMeta = {
    isSyncPathChange: true,
    isFlipPathChange: false,
    isProgramaticChange: true,
    sender: 'user-123',
  }

  expectTypeOf(meta1).toMatchTypeOf<GenericMeta>()
  expectTypeOf(meta2).toMatchTypeOf<GenericMeta>()
  expectTypeOf(meta3).toMatchTypeOf<GenericMeta>()
})

test('GenericMeta - can be extended', () => {
  interface CustomMeta extends GenericMeta {
    timestamp: number
    userId: string
  }

  const customMeta: CustomMeta = {
    timestamp: Date.now(),
    userId: 'user-123',
    sender: 'component-x',
  }

  expectTypeOf(customMeta).toMatchTypeOf<GenericMeta>()
  expectTypeOf(customMeta).toHaveProperty('timestamp')
  expectTypeOf(customMeta).toHaveProperty('userId')
})

test('ArrayOfChanges - simple object', () => {
  type Changes = ArrayOfChanges<User, GenericMeta>

  const changes: Changes = [
    ['name', 'John', { sender: 'test' }],
    ['age', 30, { isProgramaticChange: true }],
    ['email', 'john@example.com', {}],
  ]

  expectTypeOf(changes).toMatchTypeOf<Changes>()
})

test('ArrayOfChanges - nested object', () => {
  type Changes = ArrayOfChanges<NestedData, GenericMeta>

  const changes: Changes = [
    ['user', { profile: { name: 'John', bio: 'Bio' }, settings: { theme: 'dark' } }, {}],
    ['user.profile', { name: 'John', bio: 'Bio' }, { sender: 'user' }],
    ['user.profile.name', 'John', { isSyncPathChange: true }],
    ['user.profile.bio', 'Bio text', {}],
    ['user.settings.theme', 'dark', { isProgramaticChange: true }],
    ['count', 42, {}],
  ]

  expectTypeOf(changes).toMatchTypeOf<Changes>()
})

test('ArrayOfChanges - type safety for values', () => {
  type Changes = ArrayOfChanges<User, GenericMeta>

  // Valid changes
  const validChanges: Changes = [
    ['name', 'John', {}],
    ['age', 30, {}],
  ]

  expectTypeOf(validChanges).toMatchTypeOf<Changes>()
})

test('ArrayOfChanges - with custom metadata', () => {
  interface CustomMeta extends GenericMeta {
    timestamp: number
    source: string
  }

  type Changes = ArrayOfChanges<User, CustomMeta>

  const changes: Changes = [
    [
      'name',
      'John',
      {
        timestamp: Date.now(),
        source: 'api',
        sender: 'user-123',
      },
    ],
    [
      'age',
      30,
      {
        timestamp: Date.now(),
        source: 'ui',
        isProgramaticChange: false,
      },
    ],
  ]

  expectTypeOf(changes).toMatchTypeOf<Changes>()
})

test('ArrayOfChanges - real world scenario', () => {
  type FormState = {
    fields: {
      username: string
      email: string
      age: number
    }
    validation: {
      errors: string[]
      isValid: boolean
    }
  }

  interface FormChangeMeta extends GenericMeta {
    fieldName: string
    validationTriggered: boolean
  }

  type FormChanges = ArrayOfChanges<FormState, FormChangeMeta>

  const formChanges: FormChanges = [
    [
      'fields.username',
      'john_doe',
      {
        fieldName: 'username',
        validationTriggered: true,
        sender: 'input-field',
      },
    ],
    [
      'fields.email',
      'john@example.com',
      {
        fieldName: 'email',
        validationTriggered: true,
        sender: 'input-field',
      },
    ],
    [
      'validation.isValid',
      true,
      {
        fieldName: 'form',
        validationTriggered: false,
        isProgramaticChange: true,
      },
    ],
  ]

  expectTypeOf(formChanges).toMatchTypeOf<FormChanges>()
})

test('ArrayOfChanges - empty array is valid', () => {
  type Changes = ArrayOfChanges<User, GenericMeta>

  const emptyChanges: Changes = []

  expectTypeOf(emptyChanges).toMatchTypeOf<Changes>()
})

test('ArrayOfChanges - preserves array methods', () => {
  type Changes = ArrayOfChanges<User, GenericMeta>

  const changes: Changes = [['name', 'John', {}]]

  expectTypeOf(changes).toHaveProperty('push')
  expectTypeOf(changes).toHaveProperty('map')
  expectTypeOf(changes).toHaveProperty('filter')
  expectTypeOf(changes).toHaveProperty('length')
})
