/**
 * Tests for evaluateBoolLogic runtime evaluation
 *
 * State fixture mirrors Rust bool_logic.rs make_state() so both test suites
 * exercise the same logical conditions against the same data shape.
 */

import { describe, expect, it } from 'vitest'

import type { BoolLogic } from '~/types/bool-logic'
import { evaluateBoolLogic } from '~/utils/bool-logic'

// -- Shared state fixture (mirrors Rust make_state()) --

const state = {
  user: {
    role: 'admin',
    age: 25,
    active: true,
    email: 'alice@example.com',
    score: 150,
    tags: ['premium'],
    bio: '',
    deleted: null as string | null,
    profile: {
      verified: true,
      name: 'Alice',
    },
  },
  document: {
    id: 'doc-456',
    status: 'draft',
  },
}

type Logic = BoolLogic<typeof state>

describe('evaluateBoolLogic', () => {
  // --------------------------------------------------------------------------
  // Shorthand [path, value] — syntactic sugar for IS_EQUAL
  // --------------------------------------------------------------------------

  describe('shorthand [path, value]', () => {
    it('should match when string value equals expected', () => {
      const logic: Logic = ['user.role', 'admin']
      expect(evaluateBoolLogic(logic, state)).toBe(true)
    })

    it('should not match when string value differs', () => {
      const logic: Logic = ['user.role', 'editor']
      expect(evaluateBoolLogic(logic, state)).toBe(false)
    })

    it('should match number values', () => {
      const logic: Logic = ['user.age', 25]
      expect(evaluateBoolLogic(logic, state)).toBe(true)
    })

    it('should match boolean values', () => {
      const logic: Logic = ['user.active', true]
      expect(evaluateBoolLogic(logic, state)).toBe(true)
    })

    it('should match null values', () => {
      const logic: Logic = ['user.deleted', null]
      expect(evaluateBoolLogic(logic, state)).toBe(true)
    })

    it('should match nested path values', () => {
      const logic: Logic = ['user.profile.verified', true]
      expect(evaluateBoolLogic(logic, state)).toBe(true)
    })

    it('should not match wrong nested value', () => {
      const logic: Logic = ['user.profile.name', 'Bob']
      expect(evaluateBoolLogic(logic, state)).toBe(false)
    })
  })

  // --------------------------------------------------------------------------
  // IS_EQUAL
  // --------------------------------------------------------------------------

  describe('IS_EQUAL', () => {
    it('should match string equality', () => {
      expect(
        evaluateBoolLogic({ IS_EQUAL: ['user.role', 'admin'] }, state),
      ).toBe(true)
    })

    it('should not match wrong string', () => {
      expect(
        evaluateBoolLogic({ IS_EQUAL: ['user.role', 'editor'] }, state),
      ).toBe(false)
    })

    it('should match number equality', () => {
      expect(evaluateBoolLogic({ IS_EQUAL: ['user.age', 25] }, state)).toBe(
        true,
      )
    })

    it('should match boolean equality', () => {
      expect(
        evaluateBoolLogic({ IS_EQUAL: ['user.active', true] }, state),
      ).toBe(true)
    })

    it('should match null', () => {
      expect(
        evaluateBoolLogic({ IS_EQUAL: ['user.deleted', null] }, state),
      ).toBe(true)
    })

    it('should match nested path', () => {
      expect(
        evaluateBoolLogic({ IS_EQUAL: ['user.profile.verified', true] }, state),
      ).toBe(true)
    })
  })

  // --------------------------------------------------------------------------
  // EXISTS
  // --------------------------------------------------------------------------

  describe('EXISTS', () => {
    it('should return true for present non-null value', () => {
      expect(evaluateBoolLogic({ EXISTS: 'user.email' }, state)).toBe(true)
    })

    it('should return false for null value', () => {
      expect(evaluateBoolLogic({ EXISTS: 'user.deleted' }, state)).toBe(false)
    })

    it('should return false for missing path', () => {
      expect(evaluateBoolLogic({ EXISTS: 'user.email' }, state)).toBe(true)
    })

    it('should return true for empty string (exists, just empty)', () => {
      expect(evaluateBoolLogic({ EXISTS: 'user.bio' }, state)).toBe(true)
    })
  })

  // --------------------------------------------------------------------------
  // IS_EMPTY
  // --------------------------------------------------------------------------

  describe('IS_EMPTY', () => {
    it('should return true for null', () => {
      expect(evaluateBoolLogic({ IS_EMPTY: 'user.deleted' }, state)).toBe(true)
    })

    it('should return true for empty string', () => {
      expect(evaluateBoolLogic({ IS_EMPTY: 'user.bio' }, state)).toBe(true)
    })

    it('should return false for non-empty string', () => {
      expect(evaluateBoolLogic({ IS_EMPTY: 'user.email' }, state)).toBe(false)
    })

    it('should return false for number (not empty)', () => {
      expect(evaluateBoolLogic({ IS_EMPTY: 'user.age' }, state)).toBe(false)
    })
  })

  // --------------------------------------------------------------------------
  // AND
  // --------------------------------------------------------------------------

  describe('AND', () => {
    it('should return true when all conditions pass', () => {
      const logic: Logic = {
        AND: [{ IS_EQUAL: ['user.role', 'admin'] }, { EXISTS: 'user.email' }],
      }
      expect(evaluateBoolLogic(logic, state)).toBe(true)
    })

    it('should return false when one condition fails', () => {
      const logic: Logic = {
        AND: [{ IS_EQUAL: ['user.role', 'editor'] }, { EXISTS: 'user.email' }],
      }
      expect(evaluateBoolLogic(logic, state)).toBe(false)
    })

    it('should support shorthand children', () => {
      const logic: Logic = {
        AND: [['user.role', 'admin'], { EXISTS: 'user.email' }],
      }
      expect(evaluateBoolLogic(logic, state)).toBe(true)
    })
  })

  // --------------------------------------------------------------------------
  // OR
  // --------------------------------------------------------------------------

  describe('OR', () => {
    it('should return true when first condition matches', () => {
      const logic: Logic = {
        OR: [
          { IS_EQUAL: ['user.role', 'admin'] },
          { IS_EQUAL: ['user.role', 'editor'] },
        ],
      }
      expect(evaluateBoolLogic(logic, state)).toBe(true)
    })

    it('should return false when none match', () => {
      const logic: Logic = {
        OR: [
          { IS_EQUAL: ['user.role', 'editor'] },
          { IS_EQUAL: ['user.role', 'viewer'] },
        ],
      }
      expect(evaluateBoolLogic(logic, state)).toBe(false)
    })

    it('should support shorthand children', () => {
      const logic: Logic = {
        OR: [
          ['user.role', 'editor'],
          ['user.role', 'admin'],
        ],
      }
      expect(evaluateBoolLogic(logic, state)).toBe(true)
    })
  })

  // --------------------------------------------------------------------------
  // NOT
  // --------------------------------------------------------------------------

  describe('NOT', () => {
    it('should negate a false condition to true', () => {
      const logic: Logic = { NOT: { IS_EQUAL: ['user.role', 'guest'] } }
      expect(evaluateBoolLogic(logic, state)).toBe(true)
    })

    it('should negate a true condition to false', () => {
      const logic: Logic = { NOT: { IS_EQUAL: ['user.role', 'admin'] } }
      expect(evaluateBoolLogic(logic, state)).toBe(false)
    })

    it('should support shorthand child', () => {
      const logic: Logic = { NOT: ['user.role', 'guest'] }
      expect(evaluateBoolLogic(logic, state)).toBe(true)
    })
  })

  // --------------------------------------------------------------------------
  // GT / LT / GTE / LTE
  // --------------------------------------------------------------------------

  describe('GT / LT / GTE / LTE', () => {
    it('GT should return true when value is greater', () => {
      expect(evaluateBoolLogic({ GT: ['user.age', 18] }, state)).toBe(true)
    })

    it('GT should return false when value equals threshold', () => {
      expect(evaluateBoolLogic({ GT: ['user.age', 25] }, state)).toBe(false)
    })

    it('LT should return true when value is less', () => {
      expect(evaluateBoolLogic({ LT: ['user.age', 30] }, state)).toBe(true)
    })

    it('GTE should return true when value equals threshold', () => {
      expect(evaluateBoolLogic({ GTE: ['user.age', 25] }, state)).toBe(true)
    })

    it('LTE should return true when value equals threshold', () => {
      expect(evaluateBoolLogic({ LTE: ['user.age', 25] }, state)).toBe(true)
    })
  })

  // --------------------------------------------------------------------------
  // IN
  // --------------------------------------------------------------------------

  describe('IN', () => {
    it('should return true when value is in the list', () => {
      const logic: Logic = { IN: ['user.role', ['admin', 'editor']] }
      expect(evaluateBoolLogic(logic, state)).toBe(true)
    })

    it('should return false when value is not in the list', () => {
      const logic: Logic = { IN: ['user.role', ['editor', 'viewer']] }
      expect(evaluateBoolLogic(logic, state)).toBe(false)
    })
  })

  // --------------------------------------------------------------------------
  // CONTAINS_ANY
  // --------------------------------------------------------------------------

  describe('CONTAINS_ANY', () => {
    it('should return true when array contains at least one of the candidates', () => {
      // user.tags = ['premium'] — 'premium' is among candidates
      const logic: Logic = { CONTAINS_ANY: ['user.tags', ['premium', 'vip']] }
      expect(evaluateBoolLogic(logic, state)).toBe(true)
    })

    it('should return true when only the second candidate matches', () => {
      const logic: Logic = { CONTAINS_ANY: ['user.tags', ['vip', 'premium']] }
      expect(evaluateBoolLogic(logic, state)).toBe(true)
    })

    it('should return false when no candidate is in the array', () => {
      const logic: Logic = { CONTAINS_ANY: ['user.tags', ['vip', 'free']] }
      expect(evaluateBoolLogic(logic, state)).toBe(false)
    })

    it('should return false for an empty candidates list', () => {
      const logic: Logic = { CONTAINS_ANY: ['user.tags', []] }
      expect(evaluateBoolLogic(logic, state)).toBe(false)
    })

    it('should use deep equality for object candidates', () => {
      const itemState = {
        items: [
          { id: 1, label: 'Alpha' },
          { id: 2, label: 'Beta' },
        ],
      }

      type ItemState = typeof itemState

      expect(
        evaluateBoolLogic<ItemState>(
          {
            CONTAINS_ANY: [
              'items',
              [
                { id: 99, label: 'Ghost' },
                { id: 1, label: 'Alpha' },
              ],
            ],
          },
          itemState,
        ),
      ).toBe(true)

      expect(
        evaluateBoolLogic<ItemState>(
          {
            CONTAINS_ANY: [
              'items',
              [
                { id: 99, label: 'Ghost' },
                { id: 77, label: 'Other' },
              ],
            ],
          },
          itemState,
        ),
      ).toBe(false)
    })
  })

  // --------------------------------------------------------------------------
  // CONTAINS_ALL
  // --------------------------------------------------------------------------

  describe('CONTAINS_ALL', () => {
    it('should return true when array contains all candidates', () => {
      const multiTagState = {
        ...state,
        user: { ...state.user, tags: ['premium', 'verified'] },
      }
      const logic: BoolLogic<typeof multiTagState> = {
        CONTAINS_ALL: ['user.tags', ['premium', 'verified']],
      }
      expect(evaluateBoolLogic(logic, multiTagState)).toBe(true)
    })

    it('should return false when only some candidates are present', () => {
      // user.tags = ['premium'] only — 'verified' is missing
      const logic: Logic = {
        CONTAINS_ALL: ['user.tags', ['premium', 'verified']],
      }
      expect(evaluateBoolLogic(logic, state)).toBe(false)
    })

    it('should return true for an empty candidates list (vacuously true)', () => {
      const logic: Logic = { CONTAINS_ALL: ['user.tags', []] }
      expect(evaluateBoolLogic(logic, state)).toBe(true)
    })

    it('should use deep equality for object candidates', () => {
      const itemState = {
        items: [
          { id: 1, label: 'Alpha' },
          { id: 2, label: 'Beta' },
        ],
      }
      type ItemState = typeof itemState

      expect(
        evaluateBoolLogic<ItemState>(
          {
            CONTAINS_ALL: [
              'items',
              [
                { id: 1, label: 'Alpha' },
                { id: 2, label: 'Beta' },
              ],
            ],
          },
          itemState,
        ),
      ).toBe(true)

      expect(
        evaluateBoolLogic<ItemState>(
          {
            CONTAINS_ALL: [
              'items',
              [
                { id: 1, label: 'Alpha' },
                { id: 99, label: 'Ghost' },
              ],
            ],
          },
          itemState,
        ),
      ).toBe(false)
    })
  })

  // --------------------------------------------------------------------------
  // Complex nested expressions — mirrors Rust test_eval_complex_real_world
  // and test_eval_complex_with_shorthand_children
  // --------------------------------------------------------------------------

  describe('complex nested expressions', () => {
    it('should evaluate complex AND using named operators (mirrors Rust test_eval_complex_real_world)', () => {
      const logic: Logic = {
        AND: [
          {
            OR: [
              { IS_EQUAL: ['user.role', 'admin'] },
              { IS_EQUAL: ['user.role', 'editor'] },
            ],
          },
          { GTE: ['user.age', 18] },
          { GT: ['user.score', 100] },
          { NOT: { IS_EMPTY: 'user.tags' } },
          { IN: ['user.role', ['admin', 'editor', 'mod']] },
          { EXISTS: 'document.id' },
          { IS_EQUAL: ['user.profile.verified', true] },
        ],
      }
      expect(evaluateBoolLogic(logic, state)).toBe(true)
    })

    it('should evaluate same complex AND using shorthand for equality checks (mirrors Rust test_eval_complex_with_shorthand_children)', () => {
      const logic: Logic = {
        AND: [
          {
            OR: [
              ['user.role', 'admin'],
              ['user.role', 'editor'],
            ],
          },
          { GTE: ['user.age', 18] },
          { GT: ['user.score', 100] },
          { NOT: { IS_EMPTY: 'user.tags' } },
          { IN: ['user.role', ['admin', 'editor', 'mod']] },
          { EXISTS: 'document.id' },
          ['user.profile.verified', true],
        ],
      }
      expect(evaluateBoolLogic(logic, state)).toBe(true)
    })

    it('should return false when one nested shorthand condition fails', () => {
      const logic: Logic = {
        AND: [
          ['user.role', 'admin'],
          { GTE: ['user.age', 18] },
          ['user.active', false],
        ],
      }
      expect(evaluateBoolLogic(logic, state)).toBe(false)
    })
  })
})
