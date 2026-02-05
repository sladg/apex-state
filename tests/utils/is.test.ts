/**
 * Tests for is utility - Type checking utilities
 *
 * Tests all type guards:
 * - Primitive checks: nil, undefined, null, string, number, boolean, symbol
 * - Complex checks: object, array, function, date, regexp, primitive
 * - Special checks: empty, equal
 * - Negated versions: is.not.*
 */

import { describe, expect, it } from 'vitest'

import { is } from '../../src/utils/is'

describe('is utility', () => {
  describe('nil', () => {
    it('should return true for null', () => {
      expect(is.nil(null)).toBe(true)
    })

    it('should return true for undefined', () => {
      expect(is.nil(undefined)).toBe(true)
    })

    it('should return false for other values', () => {
      expect(is.nil(0)).toBe(false)
      expect(is.nil('')).toBe(false)
      expect(is.nil(false)).toBe(false)
      expect(is.nil({})).toBe(false)
      expect(is.nil([])).toBe(false)
    })
  })

  describe('undefined', () => {
    it('should return true for undefined', () => {
      expect(is.undefined(undefined)).toBe(true)
    })

    it('should return false for null', () => {
      expect(is.undefined(null)).toBe(false)
    })

    it('should return false for other values', () => {
      expect(is.undefined(0)).toBe(false)
      expect(is.undefined('')).toBe(false)
    })
  })

  describe('null', () => {
    it('should return true for null', () => {
      expect(is.null(null)).toBe(true)
    })

    it('should return false for undefined', () => {
      expect(is.null(undefined)).toBe(false)
    })

    it('should return false for other values', () => {
      expect(is.null(0)).toBe(false)
      expect(is.null('')).toBe(false)
    })
  })

  describe('object', () => {
    it('should return true for plain objects', () => {
      expect(is.object({})).toBe(true)
      expect(is.object({ a: 1 })).toBe(true)
    })

    it('should return false for arrays', () => {
      // Note: isObject explicitly excludes arrays
      expect(is.object([])).toBe(false)
      expect(is.object([1, 2, 3])).toBe(false)
    })

    it('should return false for null', () => {
      expect(is.object(null)).toBe(false)
    })

    it('should return false for primitives', () => {
      expect(is.object(42)).toBe(false)
      expect(is.object('string')).toBe(false)
      expect(is.object(true)).toBe(false)
      expect(is.object(undefined)).toBe(false)
    })
  })

  describe('array', () => {
    it('should return true for arrays', () => {
      expect(is.array([])).toBe(true)
      expect(is.array([1, 2, 3])).toBe(true)
      expect(is.array(new Array(3))).toBe(true)
    })

    it('should return false for objects', () => {
      expect(is.array({})).toBe(false)
      expect(is.array({ length: 3 })).toBe(false)
    })

    it('should return false for primitives', () => {
      expect(is.array(null)).toBe(false)
      expect(is.array(undefined)).toBe(false)
      expect(is.array('string')).toBe(false)
    })
  })

  describe('string', () => {
    it('should return true for strings', () => {
      expect(is.string('')).toBe(true)
      expect(is.string('hello')).toBe(true)
      expect(is.string(String('test'))).toBe(true)
    })

    it('should return false for other types', () => {
      expect(is.string(42)).toBe(false)
      expect(is.string(null)).toBe(false)
      expect(is.string([])).toBe(false)
    })
  })

  describe('number', () => {
    it('should return true for numbers', () => {
      expect(is.number(0)).toBe(true)
      expect(is.number(42)).toBe(true)
      expect(is.number(-1)).toBe(true)
      expect(is.number(3.14)).toBe(true)
      expect(is.number(Infinity)).toBe(true)
      expect(is.number(NaN)).toBe(true)
    })

    it('should return false for other types', () => {
      expect(is.number('42')).toBe(false)
      expect(is.number(null)).toBe(false)
      expect(is.number(undefined)).toBe(false)
    })
  })

  describe('boolean', () => {
    it('should return true for booleans', () => {
      expect(is.boolean(true)).toBe(true)
      expect(is.boolean(false)).toBe(true)
    })

    it('should return false for truthy/falsy values that are not boolean', () => {
      expect(is.boolean(0)).toBe(false)
      expect(is.boolean(1)).toBe(false)
      expect(is.boolean('')).toBe(false)
      expect(is.boolean(null)).toBe(false)
    })
  })

  describe('function', () => {
    it('should return true for functions', () => {
      expect(is.function(() => 0)).toBe(true)
      expect(
        is.function(function () {
          return undefined
        }),
      ).toBe(true)
      expect(is.function(async () => 0)).toBe(true)
      expect(
        is.function(
          class {
            m() {
              return 1
            }
          },
        ),
      ).toBe(true)
    })

    it('should return false for other types', () => {
      expect(is.function({})).toBe(false)
      expect(is.function(null)).toBe(false)
    })
  })

  describe('symbol', () => {
    it('should return true for symbols', () => {
      expect(is.symbol(Symbol())).toBe(true)
      expect(is.symbol(Symbol('test'))).toBe(true)
      expect(is.symbol(Symbol.for('key'))).toBe(true)
    })

    it('should return false for other types', () => {
      expect(is.symbol('symbol')).toBe(false)
      expect(is.symbol(null)).toBe(false)
    })
  })

  describe('date', () => {
    it('should return true for dates', () => {
      expect(is.date(new Date())).toBe(true)
      expect(is.date(new Date('2024-01-01'))).toBe(true)
    })

    it('should return false for other types', () => {
      expect(is.date('2024-01-01')).toBe(false)
      expect(is.date(Date.now())).toBe(false)
      expect(is.date(null)).toBe(false)
    })
  })

  describe('regexp', () => {
    it('should return true for regular expressions', () => {
      expect(is.regexp(/test/)).toBe(true)
      expect(is.regexp(new RegExp('test'))).toBe(true)
    })

    it('should return false for other types', () => {
      expect(is.regexp('/test/')).toBe(false)
      expect(is.regexp(null)).toBe(false)
    })
  })

  describe('primitive', () => {
    it('should return true for primitives', () => {
      expect(is.primitive('string')).toBe(true)
      expect(is.primitive(42)).toBe(true)
      expect(is.primitive(true)).toBe(true)
      expect(is.primitive(Symbol())).toBe(true)
      expect(is.primitive(BigInt(42))).toBe(true)
      expect(is.primitive(null)).toBe(true)
      expect(is.primitive(undefined)).toBe(true)
    })

    it('should return false for non-primitives', () => {
      expect(is.primitive({})).toBe(false)
      expect(is.primitive([])).toBe(false)
      expect(is.primitive(() => 0)).toBe(false)
    })
  })

  describe('empty', () => {
    it('should return true for null and undefined', () => {
      expect(is.empty(null)).toBe(true)
      expect(is.empty(undefined)).toBe(true)
    })

    it('should return true for empty strings', () => {
      expect(is.empty('')).toBe(true)
    })

    it('should return false for non-empty strings', () => {
      expect(is.empty('hello')).toBe(false)
      expect(is.empty(' ')).toBe(false)
    })

    it('should return true for empty arrays', () => {
      expect(is.empty([])).toBe(true)
    })

    it('should return false for non-empty arrays', () => {
      expect(is.empty([1])).toBe(false)
    })

    it('should return true for empty objects', () => {
      expect(is.empty({})).toBe(true)
    })

    it('should return false for non-empty objects', () => {
      expect(is.empty({ a: 1 })).toBe(false)
    })

    it('should return false for numbers (including zero)', () => {
      expect(is.empty(0)).toBe(false)
      expect(is.empty(42)).toBe(false)
    })

    it('should return false for booleans', () => {
      expect(is.empty(false)).toBe(false)
      expect(is.empty(true)).toBe(false)
    })

    it('should return false for functions', () => {
      expect(is.empty(() => 0)).toBe(false)
      expect(
        is.empty(function () {
          return undefined
        }),
      ).toBe(false)
    })

    it('should return false for symbols', () => {
      expect(is.empty(Symbol())).toBe(false)
      expect(is.empty(Symbol('test'))).toBe(false)
    })

    it('should return true for dates (treated as empty objects)', () => {
      // Note: Dates are objects with no enumerable own properties
      expect(is.empty(new Date())).toBe(true)
    })

    it('should return true for regexps (treated as empty objects)', () => {
      // Note: RegExps are objects with no enumerable own properties
      expect(is.empty(/test/)).toBe(true)
    })
  })

  describe('equal (deep equality)', () => {
    it('should return true for same reference', () => {
      const obj = { a: 1 }
      expect(is.equal(obj, obj)).toBe(true)
    })

    it('should return true for primitive equality', () => {
      expect(is.equal(42, 42)).toBe(true)
      expect(is.equal('hello', 'hello')).toBe(true)
      expect(is.equal(true, true)).toBe(true)
      expect(is.equal(null, null)).toBe(true)
      expect(is.equal(undefined, undefined)).toBe(true)
    })

    it('should return false for different primitives', () => {
      expect(is.equal(42, 43)).toBe(false)
      expect(is.equal('hello', 'world')).toBe(false)
      expect(is.equal(true, false)).toBe(false)
      expect(is.equal(null, undefined)).toBe(false)
    })

    it('should return true for deeply equal objects', () => {
      expect(is.equal({ a: 1 }, { a: 1 })).toBe(true)
      expect(is.equal({ a: { b: 2 } }, { a: { b: 2 } })).toBe(true)
    })

    it('should return false for different objects', () => {
      expect(is.equal({ a: 1 }, { a: 2 })).toBe(false)
      expect(is.equal({ a: 1 }, { b: 1 })).toBe(false)
      expect(is.equal({ a: 1 }, { a: 1, b: 2 })).toBe(false)
    })

    it('should return true for equal dates', () => {
      const date1 = new Date('2024-01-01')
      const date2 = new Date('2024-01-01')
      expect(is.equal(date1, date2)).toBe(true)
    })

    it('should return false for different dates', () => {
      const date1 = new Date('2024-01-01')
      const date2 = new Date('2024-01-02')
      expect(is.equal(date1, date2)).toBe(false)
    })

    it('should return true for equal regexps', () => {
      expect(is.equal(/test/gi, /test/gi)).toBe(true)
    })

    it('should return false for different regexps', () => {
      expect(is.equal(/test/g, /test/i)).toBe(false)
      expect(is.equal(/test/, /other/)).toBe(false)
    })

    describe('array equality', () => {
      it('should return true for arrays with same values', () => {
        expect(is.equal([1, 2, 3], [1, 2, 3])).toBe(true)
        expect(is.equal(['a', 'b'], ['a', 'b'])).toBe(true)
        expect(is.equal([], [])).toBe(true)
      })

      it('should return false for arrays with different values', () => {
        expect(is.equal([1, 2, 3], [1, 2, 4])).toBe(false)
        expect(is.equal([1, 2], [1, 2, 3])).toBe(false)
      })

      it('should return false for arrays with different lengths', () => {
        expect(is.equal([1, 2, 3], [1, 2])).toBe(false)
        expect(is.equal([1], [1, 2])).toBe(false)
      })

      it('should return true for same array reference', () => {
        const arr = [1, 2, 3]
        expect(is.equal(arr, arr)).toBe(true)
      })

      it('should handle nested arrays', () => {
        expect(
          is.equal(
            [
              [1, 2],
              [3, 4],
            ],
            [
              [1, 2],
              [3, 4],
            ],
          ),
        ).toBe(true)
        expect(
          is.equal(
            [
              [1, 2],
              [3, 4],
            ],
            [
              [1, 2],
              [3, 5],
            ],
          ),
        ).toBe(false)
      })

      it('should handle arrays with objects', () => {
        expect(is.equal([{ a: 1 }], [{ a: 1 }])).toBe(true)
        expect(is.equal([{ a: 1 }], [{ a: 2 }])).toBe(false)
      })

      it('should return false when comparing array to non-array', () => {
        expect(is.equal([1, 2, 3], { 0: 1, 1: 2, 2: 3 })).toBe(false)
        expect(is.equal({ 0: 1, 1: 2, 2: 3 }, [1, 2, 3])).toBe(false)
      })
    })
  })

  describe('negated type guards (is.not.*)', () => {
    describe('is.not.nil', () => {
      it('should return false for null and undefined', () => {
        expect(is.not.nil(null)).toBe(false)
        expect(is.not.nil(undefined)).toBe(false)
      })

      it('should return true for other values', () => {
        expect(is.not.nil(0)).toBe(true)
        expect(is.not.nil('')).toBe(true)
        expect(is.not.nil(false)).toBe(true)
      })
    })

    describe('is.not.undefined', () => {
      it('should return false for undefined', () => {
        expect(is.not.undefined(undefined)).toBe(false)
      })

      it('should return true for null and other values', () => {
        expect(is.not.undefined(null)).toBe(true)
        expect(is.not.undefined(0)).toBe(true)
      })
    })

    describe('is.not.null', () => {
      it('should return false for null', () => {
        expect(is.not.null(null)).toBe(false)
      })

      it('should return true for undefined and other values', () => {
        expect(is.not.null(undefined)).toBe(true)
        expect(is.not.null(0)).toBe(true)
      })
    })

    describe('is.not.object', () => {
      it('should return true for non-objects', () => {
        expect(is.not.object(null)).toBe(true)
        expect(is.not.object(undefined)).toBe(true)
        expect(is.not.object(42)).toBe(true)
        expect(is.not.object([])).toBe(true)
      })

      it('should return false for objects', () => {
        expect(is.not.object({})).toBe(false)
        expect(is.not.object({ a: 1 })).toBe(false)
      })
    })

    describe('is.not.array', () => {
      it('should return true for non-arrays', () => {
        expect(is.not.array({})).toBe(true)
        expect(is.not.array(null)).toBe(true)
        expect(is.not.array('string')).toBe(true)
      })

      it('should return false for arrays', () => {
        expect(is.not.array([])).toBe(false)
        expect(is.not.array([1, 2, 3])).toBe(false)
      })
    })

    describe('is.not.string', () => {
      it('should return true for non-strings', () => {
        expect(is.not.string(42)).toBe(true)
        expect(is.not.string(null)).toBe(true)
      })

      it('should return false for strings', () => {
        expect(is.not.string('')).toBe(false)
        expect(is.not.string('hello')).toBe(false)
      })
    })

    describe('is.not.number', () => {
      it('should return true for non-numbers', () => {
        expect(is.not.number('42')).toBe(true)
        expect(is.not.number(null)).toBe(true)
      })

      it('should return false for numbers', () => {
        expect(is.not.number(0)).toBe(false)
        expect(is.not.number(42)).toBe(false)
      })
    })

    describe('is.not.boolean', () => {
      it('should return true for non-booleans', () => {
        expect(is.not.boolean(0)).toBe(true)
        expect(is.not.boolean('')).toBe(true)
      })

      it('should return false for booleans', () => {
        expect(is.not.boolean(true)).toBe(false)
        expect(is.not.boolean(false)).toBe(false)
      })
    })

    describe('is.not.function', () => {
      it('should return true for non-functions', () => {
        expect(is.not.function({})).toBe(true)
        expect(is.not.function(null)).toBe(true)
      })

      it('should return false for functions', () => {
        expect(is.not.function(() => 0)).toBe(false)
      })
    })

    describe('is.not.symbol', () => {
      it('should return true for non-symbols', () => {
        expect(is.not.symbol('symbol')).toBe(true)
        expect(is.not.symbol(null)).toBe(true)
      })

      it('should return false for symbols', () => {
        expect(is.not.symbol(Symbol())).toBe(false)
      })
    })

    describe('is.not.date', () => {
      it('should return true for non-dates', () => {
        expect(is.not.date('2024-01-01')).toBe(true)
        expect(is.not.date(Date.now())).toBe(true)
      })

      it('should return false for dates', () => {
        expect(is.not.date(new Date())).toBe(false)
      })
    })

    describe('is.not.regexp', () => {
      it('should return true for non-regexps', () => {
        expect(is.not.regexp('/test/')).toBe(true)
        expect(is.not.regexp(null)).toBe(true)
      })

      it('should return false for regexps', () => {
        expect(is.not.regexp(/test/)).toBe(false)
      })
    })

    describe('is.not.primitive', () => {
      it('should return true for non-primitives', () => {
        expect(is.not.primitive({})).toBe(true)
        expect(is.not.primitive([])).toBe(true)
        expect(is.not.primitive(() => 0)).toBe(true)
      })

      it('should return false for primitives', () => {
        expect(is.not.primitive('string')).toBe(false)
        expect(is.not.primitive(42)).toBe(false)
        expect(is.not.primitive(null)).toBe(false)
      })
    })

    describe('is.not.empty', () => {
      it('should return true for non-empty values', () => {
        expect(is.not.empty('hello')).toBe(true)
        expect(is.not.empty([1])).toBe(true)
        expect(is.not.empty({ a: 1 })).toBe(true)
      })

      it('should return false for empty values', () => {
        expect(is.not.empty(null)).toBe(false)
        expect(is.not.empty('')).toBe(false)
        expect(is.not.empty([])).toBe(false)
        expect(is.not.empty({})).toBe(false)
      })
    })

    describe('is.not.equal', () => {
      it('should return true for unequal values', () => {
        expect(is.not.equal(1, 2)).toBe(true)
        expect(is.not.equal({ a: 1 }, { a: 2 })).toBe(true)
      })

      it('should return false for equal values', () => {
        expect(is.not.equal(1, 1)).toBe(false)
        expect(is.not.equal({ a: 1 }, { a: 1 })).toBe(false)
      })
    })
  })

  describe('edge cases', () => {
    it('should handle NaN correctly', () => {
      expect(is.number(NaN)).toBe(true)
      expect(is.nil(NaN)).toBe(false)
    })

    it('should handle Infinity correctly', () => {
      expect(is.number(Infinity)).toBe(true)
      expect(is.number(-Infinity)).toBe(true)
    })

    it('should handle nested objects with different key orders', () => {
      const obj1 = { a: 1, b: 2 }
      const obj2 = { b: 2, a: 1 }
      expect(is.equal(obj1, obj2)).toBe(true)
    })

    it('should handle objects with undefined values', () => {
      const obj1 = { a: undefined }
      const obj2 = { a: undefined }
      expect(is.equal(obj1, obj2)).toBe(true)
    })

    it('should handle prototype chain properly', () => {
      const proto = { inherited: true }
      const obj = Object.create(proto)
      obj.own = true
      expect(is.object(obj)).toBe(true)
      // empty check only considers own properties
      expect(is.empty({ ...{} })).toBe(true)
    })
  })
})
