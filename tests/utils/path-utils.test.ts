/**
 * Tests for pathUtils utility - Path manipulation
 *
 * Tests:
 * - getPathDepth: Calculate depth of dot-notation paths
 */

import { describe, expect, it } from 'vitest'

import { getPathDepth } from '~/utils/path-utils'

describe('getPathDepth', () => {
  it('should return 0 for empty string', () => {
    expect(getPathDepth('')).toBe(0)
  })

  it('should return 1 for single segment path', () => {
    expect(getPathDepth('user')).toBe(1)
    expect(getPathDepth('settings')).toBe(1)
  })

  it('should return 2 for two segment path', () => {
    expect(getPathDepth('user.name')).toBe(2)
    expect(getPathDepth('settings.theme')).toBe(2)
  })

  it('should return 3 for three segment path', () => {
    expect(getPathDepth('user.profile.name')).toBe(3)
    expect(getPathDepth('a.b.c')).toBe(3)
  })

  it('should handle deeply nested paths', () => {
    expect(getPathDepth('a.b.c.d.e.f')).toBe(6)
    expect(getPathDepth('user.profile.address.city.zipcode')).toBe(5)
  })

  it('should handle paths with numbers', () => {
    expect(getPathDepth('items.0.name')).toBe(3)
    expect(getPathDepth('users.123.profile')).toBe(3)
  })

  it('should handle paths with underscores', () => {
    expect(getPathDepth('user_data.first_name')).toBe(2)
  })

  it('should handle paths with mixed case', () => {
    expect(getPathDepth('userData.firstName.value')).toBe(3)
  })

  it('should handle single character segments', () => {
    expect(getPathDepth('a.b.c')).toBe(3)
  })
})
