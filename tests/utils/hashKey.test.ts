/**
 * Tests for hashKey utility - Hash key notation
 *
 * Tests:
 * - _: Convert concrete ID to hash key notation
 */

import { describe, expect, it } from 'vitest'

import { _ } from '~/utils/hashKey'

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
