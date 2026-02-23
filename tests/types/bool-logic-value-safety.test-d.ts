/**
 * BoolLogic value type safety tests
 *
 * Verifies that BoolLogic operators infer the expected value type from the path.
 * e.g., if `user.role` is `TestRole`, only `TestRole` values should be accepted —
 * not arbitrary strings, numbers, or other ComparableValue types.
 */

import { describe, expectTypeOf, test } from 'vitest'

import type { BoolLogic } from '../../src/types/bool-logic'

// -- Test fixtures --

enum TestRole {
  Admin = 'admin',
  Editor = 'editor',
  Viewer = 'viewer',
}

enum TestStatus {
  Active = 0,
  Inactive = 1,
  Banned = 2,
}

interface TypeSafeState {
  user: {
    role: TestRole
    status: TestStatus
    name: string
    age: number
    active: boolean
    nullable: string | null
  }
}

type Logic = BoolLogic<TypeSafeState>

// ============================================================================
// IS_EQUAL — value must match the type at the path
// ============================================================================

describe('BoolLogic IS_EQUAL value type safety', () => {
  // --- Valid: matching types ---

  test('accepts string enum value for string enum path', () => {
    expectTypeOf<{
      IS_EQUAL: ['user.role', TestRole.Admin]
    }>().toMatchTypeOf<Logic>()
  })

  test('accepts numeric enum value for numeric enum path', () => {
    expectTypeOf<{
      IS_EQUAL: ['user.status', TestStatus.Active]
    }>().toMatchTypeOf<Logic>()
  })

  test('accepts string for string path', () => {
    expectTypeOf<{ IS_EQUAL: ['user.name', 'alice'] }>().toMatchTypeOf<Logic>()
  })

  test('accepts number for number path', () => {
    expectTypeOf<{ IS_EQUAL: ['user.age', 25] }>().toMatchTypeOf<Logic>()
  })

  test('accepts boolean for boolean path', () => {
    expectTypeOf<{ IS_EQUAL: ['user.active', true] }>().toMatchTypeOf<Logic>()
  })

  test('accepts null for nullable path', () => {
    expectTypeOf<{ IS_EQUAL: ['user.nullable', null] }>().toMatchTypeOf<Logic>()
  })

  test('accepts string for nullable string path', () => {
    expectTypeOf<{
      IS_EQUAL: ['user.nullable', 'hello']
    }>().toMatchTypeOf<Logic>()
  })

  // --- Invalid: wrong value type for path ---

  test('rejects number for string path', () => {
    expectTypeOf<{ IS_EQUAL: ['user.name', 42] }>().not.toMatchTypeOf<Logic>()
  })

  test('rejects string for number path', () => {
    expectTypeOf<{
      IS_EQUAL: ['user.age', 'old']
    }>().not.toMatchTypeOf<Logic>()
  })

  test('rejects plain string for string enum path', () => {
    expectTypeOf<{
      IS_EQUAL: ['user.role', 'admin']
    }>().not.toMatchTypeOf<Logic>()
  })

  test('rejects number for string enum path', () => {
    expectTypeOf<{
      IS_EQUAL: ['user.role', 123]
    }>().not.toMatchTypeOf<Logic>()
  })

  test('rejects boolean for string path', () => {
    expectTypeOf<{
      IS_EQUAL: ['user.name', true]
    }>().not.toMatchTypeOf<Logic>()
  })

  test('rejects string for boolean path', () => {
    expectTypeOf<{
      IS_EQUAL: ['user.active', 'yes']
    }>().not.toMatchTypeOf<Logic>()
  })

  test('rejects null for non-nullable path', () => {
    expectTypeOf<{
      IS_EQUAL: ['user.name', null]
    }>().not.toMatchTypeOf<Logic>()
  })
})

// ============================================================================
// Shorthand [path, value] — same rules as IS_EQUAL
// ============================================================================

describe('BoolLogic shorthand [path, value] value type safety', () => {
  test('accepts matching enum value', () => {
    expectTypeOf<['user.role', TestRole.Admin]>().toMatchTypeOf<Logic>()
  })

  test('accepts matching string value', () => {
    expectTypeOf<['user.name', 'alice']>().toMatchTypeOf<Logic>()
  })

  test('rejects plain string for enum path', () => {
    expectTypeOf<['user.role', 'admin']>().not.toMatchTypeOf<Logic>()
  })

  test('rejects number for string path', () => {
    expectTypeOf<['user.name', 42]>().not.toMatchTypeOf<Logic>()
  })

  test('rejects boolean for number path', () => {
    expectTypeOf<['user.age', true]>().not.toMatchTypeOf<Logic>()
  })
})

// ============================================================================
// IN — array values must match the type at the path
// ============================================================================

describe('BoolLogic IN value type safety', () => {
  test('accepts matching enum array', () => {
    expectTypeOf<{
      IN: ['user.role', [TestRole.Admin, TestRole.Editor]]
    }>().toMatchTypeOf<Logic>()
  })

  test('accepts matching number array', () => {
    expectTypeOf<{ IN: ['user.age', [18, 21, 65]] }>().toMatchTypeOf<Logic>()
  })

  test('rejects string array for enum path', () => {
    expectTypeOf<{
      IN: ['user.role', ['admin', 'editor']]
    }>().not.toMatchTypeOf<Logic>()
  })

  test('rejects number array for string path', () => {
    expectTypeOf<{
      IN: ['user.name', [1, 2, 3]]
    }>().not.toMatchTypeOf<Logic>()
  })

  test('rejects mixed types in value array', () => {
    expectTypeOf<{
      IN: ['user.age', [1, 'two', 3]]
    }>().not.toMatchTypeOf<Logic>()
  })
})

// ============================================================================
// GT/LT/GTE/LTE — only valid on number paths
// ============================================================================

describe('BoolLogic GT/LT/GTE/LTE restricted to number paths', () => {
  test('accepts GT on number path', () => {
    expectTypeOf<{ GT: ['user.age', 18] }>().toMatchTypeOf<Logic>()
  })

  test('accepts LTE on number path', () => {
    expectTypeOf<{ LTE: ['user.age', 100] }>().toMatchTypeOf<Logic>()
  })

  test('rejects GT on string path', () => {
    expectTypeOf<{ GT: ['user.name', 5] }>().not.toMatchTypeOf<Logic>()
  })

  test('rejects LT on string enum path', () => {
    expectTypeOf<{ LT: ['user.role', 5] }>().not.toMatchTypeOf<Logic>()
  })

  test('rejects GTE on boolean path', () => {
    expectTypeOf<{ GTE: ['user.active', 1] }>().not.toMatchTypeOf<Logic>()
  })

  test('rejects LTE on nullable string path', () => {
    expectTypeOf<{ LTE: ['user.nullable', 10] }>().not.toMatchTypeOf<Logic>()
  })
})
