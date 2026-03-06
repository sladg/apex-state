/**
 * Tests for createFastJson utility — Fast stringify/parse with placeholder substitution.
 *
 * Tests:
 * - Primitive fast-path: number, boolean, null bypass JSON.stringify/parse
 * - Top-level placeholder: undefined ↔ sentinel round-trip
 * - Nested placeholder: undefined inside objects/arrays preserved via replacer
 * - Deep nesting: multiple levels of undefined values
 * - No placeholders: standard JSON.stringify/parse behavior
 */

import { describe, expect, it } from 'vitest'

import { createFastJson } from '~/utils/json'

const UNDEFINED_SENTINEL = '"__APEX_UNDEFINED__"'

const { stringify, parse } = createFastJson([
  { value: undefined, encoded: UNDEFINED_SENTINEL },
])

describe('createFastJson', () => {
  describe('primitive fast-path (no JSON.stringify)', () => {
    it('stringifies numbers', () => {
      expect(stringify(42)).toBe('42')
      expect(stringify(-3.14)).toBe('-3.14')
      expect(stringify(0)).toBe('0')
    })

    it('parses numbers', () => {
      expect(parse('42')).toBe(42)
      expect(parse('-3.14')).toBe(-3.14)
      expect(parse('0')).toBe(0)
    })

    it('stringifies booleans', () => {
      expect(stringify(true)).toBe('true')
      expect(stringify(false)).toBe('false')
    })

    it('parses booleans', () => {
      expect(parse('true')).toBe(true)
      expect(parse('false')).toBe(false)
    })

    it('stringifies null', () => {
      expect(stringify(null)).toBe('null')
    })

    it('parses null', () => {
      expect(parse('null')).toBeNull()
    })
  })

  describe('top-level placeholder (undefined)', () => {
    it('stringifies undefined to sentinel', () => {
      expect(stringify(undefined)).toBe(UNDEFINED_SENTINEL)
    })

    it('parses sentinel back to undefined', () => {
      expect(parse(UNDEFINED_SENTINEL)).toBeUndefined()
    })

    it('round-trips undefined', () => {
      expect(parse(stringify(undefined))).toBeUndefined()
    })
  })

  describe('nested undefined in objects (replacer)', () => {
    it('preserves undefined values in flat objects', () => {
      const result = stringify({ name: undefined, age: 25 })
      const parsed = JSON.parse(result)
      expect(parsed).toEqual({ name: '__APEX_UNDEFINED__', age: 25 })
    })

    it('preserves undefined in deeply nested objects', () => {
      const result = stringify({
        user: {
          profile: {
            bio: undefined,
            active: true,
          },
          name: 'alice',
        },
      })
      const parsed = JSON.parse(result)
      expect(parsed.user.profile.bio).toBe('__APEX_UNDEFINED__')
      expect(parsed.user.profile.active).toBe(true)
      expect(parsed.user.name).toBe('alice')
    })

    it('preserves undefined in arrays', () => {
      const result = stringify([1, undefined, 'hello'])
      const parsed = JSON.parse(result)
      expect(parsed).toEqual([1, '__APEX_UNDEFINED__', 'hello'])
    })

    it('preserves multiple undefined values across nesting levels', () => {
      const result = stringify({
        a: undefined,
        b: {
          c: undefined,
          d: [undefined, { e: undefined }],
        },
      })
      const parsed = JSON.parse(result)
      expect(parsed.a).toBe('__APEX_UNDEFINED__')
      expect(parsed.b.c).toBe('__APEX_UNDEFINED__')
      expect(parsed.b.d[0]).toBe('__APEX_UNDEFINED__')
      expect(parsed.b.d[1].e).toBe('__APEX_UNDEFINED__')
    })
  })

  describe('nested sentinel → undefined in parse (reviver)', () => {
    it('restores undefined in flat objects', () => {
      const json = '{"name":"__APEX_UNDEFINED__","age":25}'
      const result = parse(json)
      expect(result).toEqual({ name: undefined, age: 25 })
      expect('name' in (result as Record<string, unknown>)).toBe(true)
    })

    it('restores undefined in deeply nested objects', () => {
      const json =
        '{"user":{"profile":{"bio":"__APEX_UNDEFINED__","active":true},"name":"alice"}}'
      const result = parse(json) as Record<string, unknown>
      const user = result['user'] as Record<string, unknown>
      const profile = user['profile'] as Record<string, unknown>
      expect(profile['bio']).toBeUndefined()
      expect(profile['active']).toBe(true)
      expect(user['name']).toBe('alice')
    })

    it('restores undefined in arrays', () => {
      const json = '[1,"__APEX_UNDEFINED__","hello"]'
      const result = parse(json)
      expect(result).toEqual([1, undefined, 'hello'])
    })

    it('restores multiple undefined values across nesting levels', () => {
      const json =
        '{"a":"__APEX_UNDEFINED__","b":{"c":"__APEX_UNDEFINED__","d":["__APEX_UNDEFINED__",{"e":"__APEX_UNDEFINED__"}]}}'
      const result = parse(json) as Record<string, unknown>
      expect(result['a']).toBeUndefined()
      const b = result['b'] as Record<string, unknown>
      expect(b['c']).toBeUndefined()
      const d = b['d'] as unknown[]
      expect(d[0]).toBeUndefined()
      expect((d[1] as Record<string, unknown>)['e']).toBeUndefined()
    })
  })

  describe('full round-trip: stringify → parse restores undefined', () => {
    it('round-trips flat object with undefined', () => {
      const original = { name: undefined, age: 25 }
      const result = parse(stringify(original)) as Record<string, unknown>
      expect(result['age']).toBe(25)
      expect(result['name']).toBeUndefined()
    })

    it('round-trips deeply nested undefined', () => {
      const original = {
        user: {
          profile: { bio: undefined, active: true },
          name: 'alice',
        },
      }
      const result = parse(stringify(original)) as Record<string, unknown>
      const user = result['user'] as Record<string, unknown>
      const profile = user['profile'] as Record<string, unknown>
      expect(profile['bio']).toBeUndefined()
      expect(profile['active']).toBe(true)
      expect(user['name']).toBe('alice')
    })

    it('round-trips array with undefined', () => {
      const original = [1, undefined, 'hello']
      expect(parse(stringify(original))).toEqual(original)
    })
  })

  describe('objects without undefined pass through normally', () => {
    it('stringifies plain objects', () => {
      const obj = { name: 'alice', age: 30 }
      expect(stringify(obj)).toBe(JSON.stringify(obj))
    })

    it('stringifies strings', () => {
      expect(stringify('hello')).toBe('"hello"')
    })

    it('round-trips complex objects', () => {
      const obj = { a: [1, 2], b: { c: true, d: null } }
      expect(parse(stringify(obj))).toEqual(obj)
    })
  })

  describe('no placeholders — standard behavior', () => {
    const plain = createFastJson()

    it('uses standard JSON.stringify for objects', () => {
      const obj = { x: 1 }
      expect(plain.stringify(obj)).toBe(JSON.stringify(obj))
    })

    it('strips undefined without replacer (default JSON behavior)', () => {
      const result = plain.stringify({ a: 1, b: undefined })
      expect(JSON.parse(result)).toEqual({ a: 1 })
    })

    it('still fast-paths primitives', () => {
      expect(plain.stringify(42)).toBe('42')
      expect(plain.parse('42')).toBe(42)
    })
  })
})
