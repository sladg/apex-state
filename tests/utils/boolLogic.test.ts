/**
 * Tests for BoolLogic evaluation utility
 *
 * Tests all BoolLogic operators:
 * - IS_EQUAL: Equality comparison
 * - EXISTS: Not null/undefined check
 * - IS_EMPTY: Emptiness check
 * - AND/OR/NOT: Boolean combinators
 * - GT/LT/GTE/LTE: Numeric comparisons
 * - IN: Inclusion check
 */

import { describe, expect, it } from 'vitest'

import type { BoolLogic } from '~/types/boolLogic'
import { evaluateBoolLogic } from '~/utils/boolLogic'

interface TestState {
  user: {
    name: string
    age: number
    role: string
    isAdmin: boolean
    email: string | null
    tags: string[]
  }
  product: {
    price: number
    quantity: number
    category: string
    description: string
  }
  settings: {
    enabled: boolean
    value: number | undefined
  }
}

const createTestState = (overrides?: Partial<TestState>): TestState => ({
  user: {
    name: 'John',
    age: 30,
    role: 'editor',
    isAdmin: false,
    email: 'john@example.com',
    tags: ['developer', 'designer'],
  },
  product: {
    price: 99.99,
    quantity: 5,
    category: 'electronics',
    description: '',
  },
  settings: {
    enabled: true,
    value: undefined,
  },
  ...overrides,
})

describe('evaluateBoolLogic', () => {
  describe('IS_EQUAL operator', () => {
    it('should return true when values match', () => {
      const state = createTestState()
      expect(
        evaluateBoolLogic({ IS_EQUAL: ['user.role', 'editor'] }, state),
      ).toBe(true)
    })

    it('should return false when values do not match', () => {
      const state = createTestState()
      expect(
        evaluateBoolLogic({ IS_EQUAL: ['user.role', 'admin'] }, state),
      ).toBe(false)
    })

    it('should handle boolean comparisons', () => {
      const state = createTestState()
      expect(
        evaluateBoolLogic({ IS_EQUAL: ['user.isAdmin', false] }, state),
      ).toBe(true)
      expect(
        evaluateBoolLogic({ IS_EQUAL: ['user.isAdmin', true] }, state),
      ).toBe(false)
    })

    it('should handle number comparisons', () => {
      const state = createTestState()
      expect(evaluateBoolLogic({ IS_EQUAL: ['user.age', 30] }, state)).toBe(
        true,
      )
      expect(evaluateBoolLogic({ IS_EQUAL: ['user.age', 25] }, state)).toBe(
        false,
      )
    })

    it('should handle null comparisons', () => {
      const state = createTestState({
        user: { ...createTestState().user, email: null },
      })
      expect(evaluateBoolLogic({ IS_EQUAL: ['user.email', null] }, state)).toBe(
        true,
      )
    })

    it('should handle undefined comparisons', () => {
      const state = createTestState()
      expect(
        evaluateBoolLogic({ IS_EQUAL: ['settings.value', undefined] }, state),
      ).toBe(true)
    })
  })

  describe('EXISTS operator', () => {
    it('should return true for non-null, non-undefined values', () => {
      const state = createTestState()
      expect(evaluateBoolLogic({ EXISTS: 'user.name' }, state)).toBe(true)
      expect(evaluateBoolLogic({ EXISTS: 'user.email' }, state)).toBe(true)
      expect(evaluateBoolLogic({ EXISTS: 'product.price' }, state)).toBe(true)
    })

    it('should return false for null values', () => {
      const state = createTestState({
        user: { ...createTestState().user, email: null },
      })
      expect(evaluateBoolLogic({ EXISTS: 'user.email' }, state)).toBe(false)
    })

    it('should return false for undefined values', () => {
      const state = createTestState()
      expect(evaluateBoolLogic({ EXISTS: 'settings.value' }, state)).toBe(false)
    })

    it('should return true for falsy but existing values', () => {
      const state = createTestState({
        user: { ...createTestState().user, age: 0 },
        product: { ...createTestState().product, description: '' },
        settings: { enabled: false, value: 0 },
      })
      expect(evaluateBoolLogic({ EXISTS: 'user.age' }, state)).toBe(true)
      expect(evaluateBoolLogic({ EXISTS: 'product.description' }, state)).toBe(
        true,
      )
      expect(evaluateBoolLogic({ EXISTS: 'settings.enabled' }, state)).toBe(
        true,
      )
      expect(evaluateBoolLogic({ EXISTS: 'settings.value' }, state)).toBe(true)
    })
  })

  describe('IS_EMPTY operator', () => {
    it('should return true for empty strings', () => {
      const state = createTestState()
      expect(
        evaluateBoolLogic({ IS_EMPTY: 'product.description' }, state),
      ).toBe(true)
    })

    it('should return false for non-empty strings', () => {
      const state = createTestState()
      expect(evaluateBoolLogic({ IS_EMPTY: 'user.name' }, state)).toBe(false)
    })

    it('should return true for empty arrays', () => {
      const state = createTestState({
        user: { ...createTestState().user, tags: [] },
      })
      expect(evaluateBoolLogic({ IS_EMPTY: 'user.tags' }, state)).toBe(true)
    })

    it('should return false for non-empty arrays', () => {
      const state = createTestState()
      expect(evaluateBoolLogic({ IS_EMPTY: 'user.tags' }, state)).toBe(false)
    })
  })

  describe('AND operator', () => {
    it('should return true when all conditions are true', () => {
      const state = createTestState()
      expect(
        evaluateBoolLogic(
          {
            AND: [
              { IS_EQUAL: ['user.role', 'editor'] },
              { EXISTS: 'user.email' },
              { GT: ['user.age', 18] },
            ],
          },
          state,
        ),
      ).toBe(true)
    })

    it('should return false when any condition is false', () => {
      const state = createTestState()
      expect(
        evaluateBoolLogic(
          {
            AND: [
              { IS_EQUAL: ['user.role', 'editor'] },
              { IS_EQUAL: ['user.isAdmin', true] }, // false
              { GT: ['user.age', 18] },
            ],
          },
          state,
        ),
      ).toBe(false)
    })

    it('should return true for empty AND array', () => {
      const state = createTestState()
      expect(evaluateBoolLogic({ AND: [] }, state)).toBe(true)
    })

    it('should short-circuit on first false', () => {
      const state = createTestState()
      // First condition is false, so evaluation should stop
      expect(
        evaluateBoolLogic(
          {
            AND: [
              { IS_EQUAL: ['user.role', 'admin'] }, // false
              { IS_EQUAL: ['user.name', 'John'] },
            ],
          },
          state,
        ),
      ).toBe(false)
    })
  })

  describe('OR operator', () => {
    it('should return true when at least one condition is true', () => {
      const state = createTestState()
      expect(
        evaluateBoolLogic(
          {
            OR: [
              { IS_EQUAL: ['user.role', 'admin'] }, // false
              { IS_EQUAL: ['user.role', 'editor'] }, // true
            ],
          },
          state,
        ),
      ).toBe(true)
    })

    it('should return false when all conditions are false', () => {
      const state = createTestState()
      expect(
        evaluateBoolLogic(
          {
            OR: [
              { IS_EQUAL: ['user.role', 'admin'] },
              { IS_EQUAL: ['user.isAdmin', true] },
            ],
          },
          state,
        ),
      ).toBe(false)
    })

    it('should return false for empty OR array', () => {
      const state = createTestState()
      expect(evaluateBoolLogic({ OR: [] }, state)).toBe(false)
    })

    it('should short-circuit on first true', () => {
      const state = createTestState()
      expect(
        evaluateBoolLogic(
          {
            OR: [
              { IS_EQUAL: ['user.role', 'editor'] }, // true
              { IS_EQUAL: ['user.name', 'Jane'] }, // not evaluated
            ],
          },
          state,
        ),
      ).toBe(true)
    })
  })

  describe('NOT operator', () => {
    it('should negate true to false', () => {
      const state = createTestState()
      expect(
        evaluateBoolLogic(
          { NOT: { IS_EQUAL: ['user.role', 'editor'] } },
          state,
        ),
      ).toBe(false)
    })

    it('should negate false to true', () => {
      const state = createTestState()
      expect(
        evaluateBoolLogic({ NOT: { IS_EQUAL: ['user.role', 'admin'] } }, state),
      ).toBe(true)
    })

    it('should work with complex nested logic', () => {
      const state = createTestState()
      expect(
        evaluateBoolLogic(
          {
            NOT: {
              AND: [
                { IS_EQUAL: ['user.role', 'editor'] },
                { IS_EQUAL: ['user.isAdmin', true] },
              ],
            },
          },
          state,
        ),
      ).toBe(true) // AND is false, NOT makes it true
    })
  })

  describe('Numeric comparisons', () => {
    describe('GT (greater than)', () => {
      it('should return true when value is greater', () => {
        const state = createTestState()
        expect(evaluateBoolLogic({ GT: ['user.age', 25] }, state)).toBe(true)
        expect(evaluateBoolLogic({ GT: ['product.price', 50] }, state)).toBe(
          true,
        )
      })

      it('should return false when value is equal', () => {
        const state = createTestState()
        expect(evaluateBoolLogic({ GT: ['user.age', 30] }, state)).toBe(false)
      })

      it('should return false when value is less', () => {
        const state = createTestState()
        expect(evaluateBoolLogic({ GT: ['user.age', 35] }, state)).toBe(false)
      })

      it('should return false for non-numeric values', () => {
        const state = createTestState()
        expect(evaluateBoolLogic({ GT: ['user.name', 10] }, state)).toBe(false)
      })
    })

    describe('LT (less than)', () => {
      it('should return true when value is less', () => {
        const state = createTestState()
        expect(evaluateBoolLogic({ LT: ['user.age', 35] }, state)).toBe(true)
        expect(evaluateBoolLogic({ LT: ['product.quantity', 10] }, state)).toBe(
          true,
        )
      })

      it('should return false when value is equal', () => {
        const state = createTestState()
        expect(evaluateBoolLogic({ LT: ['user.age', 30] }, state)).toBe(false)
      })

      it('should return false when value is greater', () => {
        const state = createTestState()
        expect(evaluateBoolLogic({ LT: ['user.age', 25] }, state)).toBe(false)
      })
    })

    describe('GTE (greater than or equal)', () => {
      it('should return true when value is greater', () => {
        const state = createTestState()
        expect(evaluateBoolLogic({ GTE: ['user.age', 25] }, state)).toBe(true)
      })

      it('should return true when value is equal', () => {
        const state = createTestState()
        expect(evaluateBoolLogic({ GTE: ['user.age', 30] }, state)).toBe(true)
      })

      it('should return false when value is less', () => {
        const state = createTestState()
        expect(evaluateBoolLogic({ GTE: ['user.age', 35] }, state)).toBe(false)
      })
    })

    describe('LTE (less than or equal)', () => {
      it('should return true when value is less', () => {
        const state = createTestState()
        expect(evaluateBoolLogic({ LTE: ['user.age', 35] }, state)).toBe(true)
      })

      it('should return true when value is equal', () => {
        const state = createTestState()
        expect(evaluateBoolLogic({ LTE: ['user.age', 30] }, state)).toBe(true)
      })

      it('should return false when value is greater', () => {
        const state = createTestState()
        expect(evaluateBoolLogic({ LTE: ['user.age', 25] }, state)).toBe(false)
      })
    })
  })

  describe('IN operator', () => {
    it('should return true when value is in allowed list', () => {
      const state = createTestState()
      expect(
        evaluateBoolLogic(
          { IN: ['user.role', ['admin', 'editor', 'viewer']] },
          state,
        ),
      ).toBe(true)
    })

    it('should return false when value is not in allowed list', () => {
      const state = createTestState()
      expect(
        evaluateBoolLogic({ IN: ['user.role', ['admin', 'superuser']] }, state),
      ).toBe(false)
    })

    it('should work with number values', () => {
      const state = createTestState()
      expect(evaluateBoolLogic({ IN: ['user.age', [25, 30, 35]] }, state)).toBe(
        true,
      )
      expect(evaluateBoolLogic({ IN: ['user.age', [25, 35, 40]] }, state)).toBe(
        false,
      )
    })

    it('should work with boolean values', () => {
      const state = createTestState()
      expect(
        evaluateBoolLogic({ IN: ['user.isAdmin', [true, false]] }, state),
      ).toBe(true)
      expect(evaluateBoolLogic({ IN: ['user.isAdmin', [true]] }, state)).toBe(
        false,
      )
    })

    it('should return false for empty allowed list', () => {
      const state = createTestState()
      expect(evaluateBoolLogic({ IN: ['user.role', []] }, state)).toBe(false)
    })
  })

  describe('Complex nested logic', () => {
    it('should handle deeply nested AND/OR combinations', () => {
      const state = createTestState()

      // (role === 'editor' OR isAdmin) AND (age >= 18 AND price < 200)
      const logic: BoolLogic<TestState> = {
        AND: [
          {
            OR: [
              { IS_EQUAL: ['user.role', 'editor'] },
              { IS_EQUAL: ['user.isAdmin', true] },
            ],
          },
          {
            AND: [{ GTE: ['user.age', 18] }, { LT: ['product.price', 200] }],
          },
        ],
      }

      expect(evaluateBoolLogic(logic, state)).toBe(true)
    })

    it('should handle NOT with nested AND/OR', () => {
      const state = createTestState()

      // NOT(role === 'admin' AND isAdmin)
      const logic: BoolLogic<TestState> = {
        NOT: {
          AND: [
            { IS_EQUAL: ['user.role', 'admin'] },
            { IS_EQUAL: ['user.isAdmin', true] },
          ],
        },
      }

      expect(evaluateBoolLogic(logic, state)).toBe(true)
    })

    it('should handle real-world authorization scenario', () => {
      const state = createTestState()

      // Can edit if: (is admin) OR (is editor AND document exists AND not empty description)
      const canEdit: BoolLogic<TestState> = {
        OR: [
          { IS_EQUAL: ['user.isAdmin', true] },
          {
            AND: [
              { IS_EQUAL: ['user.role', 'editor'] },
              { EXISTS: 'product.category' },
              { NOT: { IS_EMPTY: 'product.category' } },
            ],
          },
        ],
      }

      expect(evaluateBoolLogic(canEdit, state)).toBe(true)
    })

    it('should handle price range validation', () => {
      const state = createTestState()

      // Price is in valid range: 50 <= price <= 150
      const validPriceRange: BoolLogic<TestState> = {
        AND: [{ GTE: ['product.price', 50] }, { LTE: ['product.price', 150] }],
      }

      expect(evaluateBoolLogic(validPriceRange, state)).toBe(true)

      // Outside range
      const stateExpensive = createTestState({
        product: { ...createTestState().product, price: 200 },
      })
      expect(evaluateBoolLogic(validPriceRange, stateExpensive)).toBe(false)
    })
  })

  describe('Edge cases', () => {
    it('should return false for unknown operator', () => {
      const state = createTestState()
      // @ts-expect-error Testing invalid operator
      expect(evaluateBoolLogic({ UNKNOWN: 'test' }, state)).toBe(false)
    })

    it('should handle zero values correctly in numeric comparisons', () => {
      const state = createTestState({
        settings: { enabled: true, value: 0 },
      })
      expect(evaluateBoolLogic({ GT: ['settings.value', -1] }, state)).toBe(
        true,
      )
      expect(evaluateBoolLogic({ GTE: ['settings.value', 0] }, state)).toBe(
        true,
      )
      expect(evaluateBoolLogic({ LT: ['settings.value', 1] }, state)).toBe(true)
      expect(evaluateBoolLogic({ LTE: ['settings.value', 0] }, state)).toBe(
        true,
      )
    })

    it('should handle negative numbers', () => {
      const state = createTestState({
        product: { ...createTestState().product, price: -10 },
      })
      expect(evaluateBoolLogic({ LT: ['product.price', 0] }, state)).toBe(true)
      expect(evaluateBoolLogic({ GT: ['product.price', -20] }, state)).toBe(
        true,
      )
    })
  })
})
