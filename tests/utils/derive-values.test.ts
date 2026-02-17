/**
 * Tests for deriveValues utility - Getter detection and extraction
 *
 * Tests:
 * - detectGetters: Detect getter properties in objects
 * - extractGetters: Extract getters and base values separately
 */

import { describe, expect, it } from 'vitest'

import { detectGetters, extractGetters } from '../../src/utils/derive-values'

describe('detectGetters', () => {
  it('should return empty object for object without getters', () => {
    const obj = { a: 1, b: 2, c: 'hello' }
    expect(detectGetters(obj)).toEqual({})
  })

  it('should detect single getter', () => {
    const obj = {
      a: 1,
      b: 2,
      get sum() {
        return this.a + this.b
      },
    }
    const getters = detectGetters(obj)
    expect(Object.keys(getters)).toEqual(['sum'])
    expect(typeof getters['sum']).toBe('function')
  })

  it('should detect multiple getters', () => {
    const obj = {
      firstName: 'John',
      lastName: 'Doe',
      get fullName() {
        return `${this.firstName} ${this.lastName}`
      },
      get initials() {
        return `${this.firstName[0]}${this.lastName[0]}`
      },
    }
    const getters = detectGetters(obj)
    expect(Object.keys(getters).sort()).toEqual(['fullName', 'initials'])
  })

  it('should compute getter value correctly', () => {
    const obj = {
      a: 1,
      b: 2,
      get sum() {
        return this.a + this.b
      },
    }
    const getters = detectGetters(obj)
    // The getter function takes the snapshot as argument
    expect(getters['sum']!({ a: 1, b: 2 })).toBe(3)
    expect(getters['sum']!({ a: 5, b: 10 })).toBe(15)
  })

  it('should detect nested getters', () => {
    const obj = {
      user: {
        firstName: 'John',
        lastName: 'Doe',
        get fullName() {
          return `${this.firstName} ${this.lastName}`
        },
      },
    }
    const getters = detectGetters(obj)
    expect(Object.keys(getters)).toEqual(['user.fullName'])
  })

  it('should compute nested getter value correctly', () => {
    const obj = {
      user: {
        firstName: 'John',
        lastName: 'Doe',
        get fullName() {
          return `${this.firstName} ${this.lastName}`
        },
      },
    }
    const getters = detectGetters(obj)
    const snap = { user: { firstName: 'Jane', lastName: 'Smith' } }
    expect(getters['user.fullName']!(snap)).toBe('Jane Smith')
  })

  it('should detect deeply nested getters', () => {
    const obj = {
      data: {
        metrics: {
          value1: 10,
          value2: 20,
          get total() {
            return this.value1 + this.value2
          },
        },
      },
    }
    const getters = detectGetters(obj)
    expect(Object.keys(getters)).toEqual(['data.metrics.total'])
  })

  it('should skip non-enumerable properties', () => {
    const obj: Record<string, unknown> = { a: 1 }
    Object.defineProperty(obj, 'hidden', {
      get() {
        return 'secret'
      },
      enumerable: false,
    })
    const getters = detectGetters(obj)
    expect(Object.keys(getters)).toEqual([])
  })

  it('should skip arrays', () => {
    const obj = {
      items: [1, 2, 3],
      get count() {
        return this.items.length
      },
    }
    const getters = detectGetters(obj)
    expect(Object.keys(getters)).toEqual(['count'])
  })

  it('should skip Date objects', () => {
    const obj = {
      date: new Date('2024-01-01'),
      get year() {
        return this.date.getFullYear()
      },
    }
    const getters = detectGetters(obj)
    expect(Object.keys(getters)).toEqual(['year'])
  })

  it('should skip RegExp objects', () => {
    const obj = {
      pattern: /test/,
      get source() {
        return this.pattern.source
      },
    }
    const getters = detectGetters(obj)
    expect(Object.keys(getters)).toEqual(['source'])
  })

  it('should handle prefix parameter', () => {
    const obj = {
      value: 10,
      get doubled() {
        return this.value * 2
      },
    }
    const getters = detectGetters(obj, 'nested')
    expect(Object.keys(getters)).toEqual(['nested.doubled'])
  })
})

describe('extractGetters', () => {
  it('should separate base and computed for object with getters', () => {
    const obj = {
      a: 1,
      b: 2,
      get sum() {
        return this.a + this.b
      },
    }
    const { base, computed } = extractGetters(obj)
    expect(base).toEqual({ a: 1, b: 2 })
    expect(Object.keys(computed)).toEqual(['sum'])
  })

  it('should return empty computed for object without getters', () => {
    const obj = { a: 1, b: 2, c: 'hello' }
    const { base, computed } = extractGetters(obj)
    expect(base).toEqual({ a: 1, b: 2, c: 'hello' })
    expect(computed).toEqual({})
  })

  it('should return computed getter that works with context binding', () => {
    const obj = {
      a: 1,
      b: 2,
      get sum() {
        return this.a + this.b
      },
    }
    const { computed } = extractGetters(obj)
    expect(computed['sum']!.get.call({ a: 5, b: 10 })).toBe(15)
    expect(computed['sum']!.parentPath).toBe('')
  })

  it('should handle multiple getters', () => {
    const obj = {
      value: 10,
      get doubled() {
        return this.value * 2
      },
      get tripled() {
        return this.value * 3
      },
    }
    const { base, computed } = extractGetters(obj)
    expect(base).toEqual({ value: 10 })
    expect(Object.keys(computed).sort()).toEqual(['doubled', 'tripled'])
    expect(computed['doubled']!.get.call({ value: 5 })).toBe(10)
    expect(computed['tripled']!.get.call({ value: 5 })).toBe(15)
  })

  it('should preserve nested objects in base', () => {
    const obj = {
      user: {
        name: 'John',
        age: 30,
      },
      get info() {
        return `${this.user.name}, ${this.user.age}`
      },
    }
    const { base, computed } = extractGetters(obj)
    expect(base).toEqual({
      user: { name: 'John', age: 30 },
    })
    expect(Object.keys(computed)).toEqual(['info'])
  })

  it('should preserve arrays in base', () => {
    const obj = {
      items: [1, 2, 3],
      get sum() {
        return this.items.reduce((a: number, b: number) => a + b, 0)
      },
    }
    const { base, computed } = extractGetters(obj)
    expect(base).toEqual({ items: [1, 2, 3] })
    expect(Object.keys(computed)).toEqual(['sum'])
    expect(computed['sum']!.get.call({ items: [1, 2, 3] })).toBe(6)
  })

  it('should skip non-enumerable properties', () => {
    const obj: Record<string, unknown> = { a: 1 }
    Object.defineProperty(obj, 'hidden', {
      value: 'secret',
      enumerable: false,
    })
    const { base, computed } = extractGetters(obj)
    expect(base).toEqual({ a: 1 })
    expect(computed).toEqual({})
  })

  it('should handle boolean values', () => {
    const obj = {
      enabled: true,
      get status() {
        return this.enabled ? 'on' : 'off'
      },
    }
    const { base, computed } = extractGetters(obj)
    expect(base).toEqual({ enabled: true })
    expect(computed['status']!.get.call({ enabled: false })).toBe('off')
  })

  it('should extract nested getters with dot-path keys', () => {
    const obj = {
      user: {
        firstName: 'John',
        lastName: 'Doe',
        get fullName() {
          return `${this.firstName} ${this.lastName}`
        },
      },
    }
    const { base, computed } = extractGetters(obj)
    expect(base).toEqual({ user: { firstName: 'John', lastName: 'Doe' } })
    expect(Object.keys(computed)).toEqual(['user.fullName'])
    expect(computed['user.fullName']!.parentPath).toBe('user')
    expect(
      computed['user.fullName']!.get.call({
        firstName: 'Jane',
        lastName: 'Smith',
      }),
    ).toBe('Jane Smith')
  })

  it('should handle null values', () => {
    const obj = {
      value: null,
      get hasValue() {
        return this.value !== null
      },
    }
    const { base, computed } = extractGetters(obj)
    expect(base).toEqual({ value: null })
    expect(computed['hasValue']!.get.call({ value: null })).toBe(false)
    expect(computed['hasValue']!.get.call({ value: 42 })).toBe(true)
  })
})
