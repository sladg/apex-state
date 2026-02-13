/**
 * Type-Only Tests for Prebuilt Concerns Type Constraints
 *
 * This file is not executed, only type-checked.
 * It verifies that invalid paths and types are caught at compile time.
 *
 * Run with: npm run type-check
 */

import type { BoolLogic } from '../../src/types'

// ============================================================================
// Test State Type
// ============================================================================

interface TestState {
  user: {
    id: number
    name: string
    email: string
    isAdmin: boolean
  }
  product: {
    sku: string
    price: number
    status: 'active' | 'inactive'
    inStock: boolean
  }
  settings: {
    darkMode: boolean
  }
}

// ============================================================================
// Valid BoolLogic Type Tests (should compile)
// ============================================================================

// Valid: IS_EQUAL with correct path and value type
type _ValidIsEqual = BoolLogic<TestState> & {
  IS_EQUAL: ['user.id', number]
}

// Valid: IS_EQUAL with string path
type _ValidIsEqualString = BoolLogic<TestState> & {
  IS_EQUAL: ['product.status', 'active' | 'inactive']
}

// Valid: EXISTS with valid path
type _ValidExists = BoolLogic<TestState> & {
  EXISTS: 'user.email'
}

// Valid: GT with numeric path
type _ValidGT = BoolLogic<TestState> & {
  GT: ['product.price', number]
}

// Valid: AND combinator
type _ValidAnd = BoolLogic<TestState> & {
  AND: [
    { IS_EQUAL: ['user.isAdmin', boolean] },
    { IS_EQUAL: ['product.status', 'active' | 'inactive'] },
  ]
}

// Valid: OR combinator
type _ValidOr = BoolLogic<TestState> & {
  OR: [{ IS_EQUAL: ['user.isAdmin', true] }, { EXISTS: 'product.sku' }]
}

// Valid: NOT negation
type _ValidNot = BoolLogic<TestState> & {
  NOT: { IS_EQUAL: ['product.inStock', false] }
}

// Valid: IN operator with matching value array
type _ValidIn = BoolLogic<TestState> & {
  IN: ['product.status', ('active' | 'inactive')[]]
}

// ============================================================================
// Invalid BoolLogic Type Tests (should NOT compile)
// These are marked with @ts-expect-error to verify they fail
// ============================================================================

// Invalid: IS_EQUAL with non-existent path
// @ts-expect-error - 'invalid.path' doesn't exist in TestState
type _InvalidPathIsEqual = BoolLogic<TestState> & {
  IS_EQUAL: ['invalid.path', string]
}

// Invalid: EXISTS with non-existent path
// @ts-expect-error - 'nonexistent' is not a valid path
type _InvalidPathExists = BoolLogic<TestState> & {
  EXISTS: 'nonexistent'
}

// Invalid: GT on non-numeric path (user.name is string)
// @ts-expect-error - GT requires numeric path, user.name is string
type _InvalidNumericComparison = BoolLogic<TestState> & {
  GT: ['user.name', 100]
}

// Invalid: LT on non-numeric path
// @ts-expect-error - LT requires numeric path, product.status is string literal
type _InvalidLT = BoolLogic<TestState> & {
  LT: ['product.status', 50]
}

// Invalid: IN with path that doesn't exist
// @ts-expect-error - 'missing.field' is not a valid path
type _InvalidInPath = BoolLogic<TestState> & {
  IN: ['missing.field', ['value1', 'value2']]
}

// Invalid: Nested path that doesn't exist in AND
// @ts-expect-error - 'user.invalid.nested' doesn't exist
type _InvalidNestedPath = BoolLogic<TestState> & {
  AND: [
    { IS_EQUAL: ['user.id', 1] },
    { IS_EQUAL: ['user.invalid.nested', 'value'] },
  ]
}

// Invalid: Nested path in OR
// @ts-expect-error - 'product.missing' doesn't exist
type _InvalidNestedOR = BoolLogic<TestState> & {
  OR: [{ EXISTS: 'product.sku' }, { IS_EQUAL: ['product.missing', 'value'] }]
}

// Invalid: Nested path in NOT
// @ts-expect-error - 'settings.invalid.mode' doesn't exist
type _InvalidNestedNOT = BoolLogic<TestState> & {
  NOT: { IS_EQUAL: ['settings.invalid.mode', true] }
}

// Invalid: GTE on non-numeric path
// @ts-expect-error - GTE requires numeric path, email is string
type _InvalidGTE = BoolLogic<TestState> & {
  GTE: ['user.email', 10]
}

// Invalid: LTE on non-numeric path
// @ts-expect-error - LTE requires numeric path, name is string
type _InvalidLTE = BoolLogic<TestState> & {
  LTE: ['user.name', 5]
}

// Invalid: IS_EMPTY on completely invalid path
// @ts-expect-error - path doesn't exist
type _InvalidIsEmpty = BoolLogic<TestState> & {
  IS_EMPTY: 'this.does.not.exist'
}
