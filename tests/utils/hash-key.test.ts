/**
 * Tests for hashKey utility - Hash key notation
 *
 * Tests:
 * - _: Convert concrete ID to hash key notation
 * - hashKey.rejectDynamic: Reject paths with [*] dynamic hash keys
 */

import { describe, expect, it } from 'vitest'

import { _, hashKey } from '../../src/utils/hash-key'

describe('_ (hash key helper)', () => {
  it('should return the ID as-is', () => {
    expect(_('u1')).toBe('u1')
    expect(_('p1')).toBe('p1')
    expect(_('abc123')).toBe('abc123')
  })

  it('should work in template strings', () => {
    const path = `users.${_('u1')}.posts.${_('p1')}.name`
    expect(path).toBe('users.u1.posts.p1.name')
  })

  it('should handle numeric IDs', () => {
    const path = `items.${_('123')}.value`
    expect(path).toBe('items.123.value')
  })

  it('should handle UUID-like IDs', () => {
    const path = `records.${_('550e8400-e29b-41d4-a716-446655440000')}.data`
    expect(path).toBe('records.550e8400-e29b-41d4-a716-446655440000.data')
  })

  it('should handle empty string', () => {
    expect(_('')).toBe('')
  })
})

describe('hash-key.rejectDynamic', () => {
  it('should not throw for paths without [*]', () => {
    expect(() => hashKey.rejectDynamic('users.u1.posts.p1')).not.toThrow()
    expect(() => hashKey.rejectDynamic('simple.path')).not.toThrow()
    expect(() =>
      hashKey.rejectDynamic('deeply.nested.path.to.value'),
    ).not.toThrow()
  })

  it('should throw for paths with [*]', () => {
    expect(() => hashKey.rejectDynamic('users.[*].posts')).toThrow(
      /contains \[\*\] hash key/,
    )
    expect(() => hashKey.rejectDynamic('[*].name')).toThrow(
      /contains \[\*\] hash key/,
    )
    expect(() => hashKey.rejectDynamic('data.[*]')).toThrow(
      /contains \[\*\] hash key/,
    )
  })

  it('should throw for paths with multiple [*]', () => {
    expect(() => hashKey.rejectDynamic('users.[*].posts.[*].name')).toThrow(
      /contains \[\*\] hash key/,
    )
  })

  it('should not throw for paths containing asterisk in other forms', () => {
    // Only [*] should be rejected, not other forms of asterisk
    expect(() => hashKey.rejectDynamic('users.*.posts')).not.toThrow()
    expect(() => hashKey.rejectDynamic('users.[star].posts')).not.toThrow()
  })

  it('should not throw for empty path', () => {
    expect(() => hashKey.rejectDynamic('')).not.toThrow()
  })

  it('should not throw for single segment path', () => {
    expect(() => hashKey.rejectDynamic('users')).not.toThrow()
  })

  it('should handle paths with concrete IDs that look similar to [*]', () => {
    expect(() => hashKey.rejectDynamic('users.[u1].posts')).not.toThrow()
    expect(() => hashKey.rejectDynamic('users.[123].posts')).not.toThrow()
  })
})

describe('hash-key namespace', () => {
  it('should have rejectDynamic function', () => {
    expect(typeof hashKey.rejectDynamic).toBe('function')
  })

  it('should have _ alias', () => {
    expect(hashKey._).toBe(_)
  })

  it('should have all expected properties', () => {
    // Verify the namespace has all expected exports
    // Note: 'as const' is compile-time only, not runtime Object.freeze
    expect(hashKey).toHaveProperty('rejectDynamic')
    expect(hashKey).toHaveProperty('_')
    expect(Object.keys(hashKey).sort()).toEqual(['_', 'rejectDynamic'])
  })
})
