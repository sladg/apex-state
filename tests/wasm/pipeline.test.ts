import { afterEach, beforeEach, describe, expect, it } from 'vitest'

import {
  initWasm,
  internCount,
  isWasmLoaded,
  processChanges,
  registerBoolLogic,
  resetWasm,
  shadowDump,
  shadowGet,
  shadowInit,
  unregisterBoolLogic,
} from '../../src/wasm/bridge'

let wasmModule: unknown

describe('WASM Pipeline Integration', () => {
  beforeEach(async () => {
    wasmModule = await import('../../rust/pkg-node/apex_state_wasm.js')
    initWasm(wasmModule)
    shadowInit({ user: { role: 'guest', age: 20, email: 'test@test.com' } })
  })

  afterEach(() => {
    resetWasm()
  })

  describe('WASM loading', () => {
    it('should be loaded after initWasm', () => {
      expect(isWasmLoaded()).toBe(true)
    })

    it('should throw if bridge functions called before loading', () => {
      resetWasm()
      expect(() => shadowInit({ a: 1 })).toThrow('WASM not loaded')
    })
  })

  describe('shadow state', () => {
    it('should dump shadow state', () => {
      expect(shadowDump()).toEqual({
        user: { role: 'guest', age: 20, email: 'test@test.com' },
      })
    })

    it('should get value at path', () => {
      expect(shadowGet('user.role')).toBe('guest')
    })

    it('should return undefined for missing path', () => {
      expect(shadowGet('user.missing')).toBeUndefined()
    })

    it('should handle nested objects', () => {
      shadowInit({ a: { b: { c: { d: 42 } } } })
      expect(shadowGet('a.b.c.d')).toBe(42)
    })

    it('should handle arrays', () => {
      shadowInit({ items: [10, 20, 30] })
      expect(shadowGet('items.0')).toBe(10)
      expect(shadowGet('items.2')).toBe(30)
    })
  })

  describe('processChanges — echo input', () => {
    it('should echo input changes', () => {
      const result = processChanges([{ path: 'user.role', value: 'admin' }])
      expect(result).toHaveLength(1)
      expect(result[0]).toEqual({ path: 'user.role', value: 'admin' })
    })

    it('should update shadow state', () => {
      processChanges([{ path: 'user.role', value: 'admin' }])
      expect(shadowGet('user.role')).toBe('admin')
    })

    it('should handle multiple changes', () => {
      const result = processChanges([
        { path: 'user.role', value: 'admin' },
        { path: 'user.age', value: 30 },
      ])
      expect(result).toHaveLength(2)
      expect(shadowGet('user.role')).toBe('admin')
      expect(shadowGet('user.age')).toBe(30)
    })

    it('should handle empty changes', () => {
      const result = processChanges([])
      expect(result).toHaveLength(0)
    })

    it('should handle nested object replacement', () => {
      processChanges([{ path: 'user', value: { role: 'admin', name: 'Bob' } }])
      expect(shadowGet('user.role')).toBe('admin')
      expect(shadowGet('user.name')).toBe('Bob')
      expect(shadowGet('user.age')).toBeUndefined()
    })
  })

  describe('processChanges — BoolLogic evaluation', () => {
    it('should evaluate affected BoolLogic (true case)', () => {
      registerBoolLogic('_concerns.user.email.disabledWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })

      const result = processChanges([{ path: 'user.role', value: 'admin' }])

      expect(result).toHaveLength(2)
      const bl = result.find((c) => c.path.includes('disabledWhen'))
      expect(bl).toBeDefined()
      expect(bl!.value).toBe(true)
    })

    it('should evaluate affected BoolLogic (false case)', () => {
      registerBoolLogic('_concerns.user.email.disabledWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })

      const result = processChanges([{ path: 'user.role', value: 'editor' }])

      const bl = result.find((c) => c.path.includes('disabledWhen'))
      expect(bl!.value).toBe(false)
    })

    it('should not evaluate unrelated BoolLogic', () => {
      registerBoolLogic('_concerns.user.email.disabledWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })

      const result = processChanges([{ path: 'user.age', value: 25 }])
      expect(result).toHaveLength(1)
    })

    it('should evaluate multiple concerns on same dependency', () => {
      registerBoolLogic('_concerns.user.email.disabledWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })
      registerBoolLogic('_concerns.user.email.readonlyWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })
      registerBoolLogic('_concerns.user.name.visibleWhen', {
        EXISTS: 'user.role',
      })

      const result = processChanges([{ path: 'user.role', value: 'admin' }])

      expect(result).toHaveLength(4)
      const blPaths = result
        .filter((c) => c.path.startsWith('_concerns'))
        .map((c) => c.path)
      expect(blPaths).toContain('_concerns.user.email.disabledWhen')
      expect(blPaths).toContain('_concerns.user.email.readonlyWhen')
      expect(blPaths).toContain('_concerns.user.name.visibleWhen')
    })

    it('should evaluate complex AND logic', () => {
      registerBoolLogic('_concerns.user.panel.visibleWhen', {
        AND: [{ IS_EQUAL: ['user.role', 'admin'] }, { GTE: ['user.age', 18] }],
      })

      const result = processChanges([{ path: 'user.role', value: 'admin' }])
      const bl = result.find((c) => c.path.includes('visibleWhen'))
      expect(bl!.value).toBe(true)
    })

    it('should handle nested object update triggering BoolLogic', () => {
      registerBoolLogic('_concerns.user.email.disabledWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })

      const result = processChanges([
        { path: 'user', value: { role: 'admin', age: 30 } },
      ])

      const bl = result.find((c) => c.path.includes('disabledWhen'))
      expect(bl!.value).toBe(true)
    })

    it('should handle IN operator', () => {
      registerBoolLogic('_concerns.user.panel.visibleWhen', {
        IN: ['user.role', ['admin', 'editor', 'moderator']],
      })

      const result = processChanges([{ path: 'user.role', value: 'editor' }])
      const bl = result.find((c) => c.path.includes('visibleWhen'))
      expect(bl!.value).toBe(true)
    })

    it('should handle NOT operator', () => {
      registerBoolLogic('_concerns.user.email.disabledWhen', {
        NOT: { IS_EQUAL: ['user.role', 'admin'] },
      })

      const result = processChanges([{ path: 'user.role', value: 'guest' }])
      const bl = result.find((c) => c.path.includes('disabledWhen'))
      expect(bl!.value).toBe(true)
    })

    it('should handle IS_EMPTY operator', () => {
      shadowInit({ user: { bio: '' } })
      registerBoolLogic('_concerns.user.bio.visibleWhen', {
        IS_EMPTY: 'user.bio',
      })

      const result = processChanges([{ path: 'user.bio', value: '' }])
      const bl = result.find((c) => c.path.includes('visibleWhen'))
      expect(bl!.value).toBe(true)
    })

    it('should handle EXISTS operator', () => {
      shadowInit({ user: {} })
      registerBoolLogic('_concerns.user.email.visibleWhen', {
        EXISTS: 'user.email',
      })

      const result = processChanges([{ path: 'user.email', value: 'a@b.com' }])
      const bl = result.find((c) => c.path.includes('visibleWhen'))
      expect(bl!.value).toBe(true)
    })
  })

  describe('registration lifecycle', () => {
    it('should unregister BoolLogic expression', () => {
      const id = registerBoolLogic('_concerns.user.email.disabledWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })

      let result = processChanges([{ path: 'user.role', value: 'admin' }])
      expect(result).toHaveLength(2)

      unregisterBoolLogic(id)

      result = processChanges([{ path: 'user.role', value: 'editor' }])
      expect(result).toHaveLength(1)
    })

    it('should handle multiple register/unregister cycles', () => {
      const id1 = registerBoolLogic('out1', {
        IS_EQUAL: ['user.role', 'admin'],
      })
      const id2 = registerBoolLogic('out2', { EXISTS: 'user.role' })

      let result = processChanges([{ path: 'user.role', value: 'admin' }])
      expect(result).toHaveLength(3)

      unregisterBoolLogic(id1)
      result = processChanges([{ path: 'user.role', value: 'editor' }])
      expect(result).toHaveLength(2)

      unregisterBoolLogic(id2)
      result = processChanges([{ path: 'user.role', value: 'guest' }])
      expect(result).toHaveLength(1)
    })
  })

  describe('intern count', () => {
    it('should track interned paths', () => {
      const before = internCount()
      registerBoolLogic('out', { IS_EQUAL: ['user.role', 'admin'] })
      expect(internCount()).toBeGreaterThan(before)
    })
  })

  describe('value types', () => {
    it('should handle string values', () => {
      const result = processChanges([{ path: 'user.role', value: 'admin' }])
      expect(result[0].value).toBe('admin')
    })

    it('should handle number values', () => {
      const result = processChanges([{ path: 'user.age', value: 42 }])
      expect(result[0].value).toBe(42)
    })

    it('should handle boolean values', () => {
      const result = processChanges([{ path: 'user.active', value: true }])
      expect(result[0].value).toBe(true)
    })

    it('should handle null values', () => {
      const result = processChanges([{ path: 'user.deleted', value: null }])
      expect(result[0].value).toBeNull()
    })

    it('should handle object values', () => {
      const result = processChanges([
        { path: 'user.profile', value: { name: 'Alice', verified: true } },
      ])
      expect(result[0].value).toEqual({ name: 'Alice', verified: true })
    })

    it('should handle array values', () => {
      const result = processChanges([
        { path: 'user.tags', value: ['admin', 'premium'] },
      ])
      expect(result[0].value).toEqual(['admin', 'premium'])
    })
  })
})
