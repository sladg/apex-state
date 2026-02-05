import { describe, expect, it } from 'vitest'

import { deepMerge } from '../../src/utils/deepMerge'

describe('deepMerge', () => {
  it('should return target when source is undefined', () => {
    const target = { a: 1, b: 2 }
    expect(deepMerge(target)).toBe(target)
    expect(deepMerge(target, undefined)).toBe(target)
  })

  it('should shallow merge top-level properties', () => {
    const target = { a: 1, b: 2 }
    const result = deepMerge(target, { a: 10 })

    expect(result).toEqual({ a: 10, b: 2 })
  })

  it('should not mutate target', () => {
    const target = { a: 1, nested: { x: 1 } }
    const result = deepMerge(target, { a: 2, nested: { x: 2 } })

    expect(target.a).toBe(1)
    expect(target.nested.x).toBe(1)
    expect(result.a).toBe(2)
    expect(result.nested.x).toBe(2)
  })

  it('should recursively merge nested plain objects', () => {
    const target = { debug: { timing: false, timingThreshold: 5 } }
    const result = deepMerge(target, { debug: { timing: true } })

    expect(result).toEqual({ debug: { timing: true, timingThreshold: 5 } })
  })

  it('should skip undefined source values (preserve target)', () => {
    const target = { a: 1, b: 2 }
    const result = deepMerge(target, { a: undefined })

    expect(result).toEqual({ a: 1, b: 2 })
  })

  it('should allow null to override target values', () => {
    const target = { a: 'hello' as string | null }
    const result = deepMerge(target, { a: null })

    expect(result).toEqual({ a: null })
  })

  it('should replace arrays wholesale, not merge them', () => {
    const target = { tags: [1, 2, 3] }
    const result = deepMerge(target, { tags: [4, 5] })

    expect(result).toEqual({ tags: [4, 5] })
  })

  it('should replace Date instances atomically, not merge them', () => {
    const d1 = new Date('2024-01-01')
    const d2 = new Date('2025-06-15')
    const target = { created: d1 }
    const result = deepMerge(target, { created: d2 as typeof d1 })

    expect(result.created).toBe(d2)
  })

  it('should replace RegExp instances atomically', () => {
    const r1 = /foo/
    const r2 = /bar/g
    const target = { pattern: r1 }
    const result = deepMerge(target, { pattern: r2 as typeof r1 })

    expect(result.pattern).toBe(r2)
  })

  it('should replace class instances atomically, not merge fields', () => {
    class Config {
      constructor(public value: number) {}
    }
    const c1 = new Config(1)
    const c2 = new Config(2)
    const target = { cfg: c1 }
    const result = deepMerge(target, { cfg: c2 as typeof c1 })

    expect(result.cfg).toBe(c2)
    expect(result.cfg.value).toBe(2)
  })

  it('should ignore inherited properties from source prototype', () => {
    const proto = { inherited: 'from proto' }
    const source = Object.create(proto) as { own: string; inherited?: string }
    source.own = 'value'

    const target = { own: 'original', inherited: 'original' }
    const result = deepMerge(target, source)

    expect(result.own).toBe('value')
    expect(result.inherited).toBe('original')
  })

  it('should handle deeply nested merges (3+ levels)', () => {
    const target = { a: { b: { c: { d: 1, e: 2 } } } }
    const result = deepMerge(target, { a: { b: { c: { d: 99 } } } })

    expect(result).toEqual({ a: { b: { c: { d: 99, e: 2 } } } })
  })

  it('should handle source with Object.create(null)', () => {
    const source = Object.create(null) as { a: number }
    source.a = 42

    const target = { a: 1, b: 2 }
    const result = deepMerge(target, source)

    expect(result).toEqual({ a: 42, b: 2 })
  })

  it('should merge when target has plain object and source replaces with primitive', () => {
    const target = { value: { nested: true } as unknown }
    const result = deepMerge(target, { value: 'string' as unknown })

    expect(result.value).toBe('string')
  })

  it('should merge when target has primitive and source provides object', () => {
    const target = { value: 'string' as unknown }
    const result = deepMerge(target, {
      value: { nested: true } as unknown,
    })

    expect(result.value).toEqual({ nested: true })
  })
})
