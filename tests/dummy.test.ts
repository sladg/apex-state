import { describe, expect, it } from 'vitest'

import { VERSION } from '../src/index'

describe('Package Setup', () => {
  it('should export VERSION constant', () => {
    expect(VERSION).toBe('0.1.0')
  })

  it('should have proper test infrastructure', () => {
    expect(true).toBe(true)
  })
})
