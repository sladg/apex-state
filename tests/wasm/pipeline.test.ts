import { afterEach, beforeEach, describe, expect, it } from 'vitest'

import { initWasm, isWasmLoaded, resetWasm, wasm } from '../../src/wasm/bridge'

let wasmModule: unknown

describe('WASM Pipeline Integration', () => {
  beforeEach(async () => {
    wasmModule = await import('../../rust/pkg/apex_state_wasm.js')
    initWasm(wasmModule)
    wasm.shadowInit({
      user: { role: 'guest', age: 20, email: 'test@test.com' },
    })
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
      expect(() => wasm.shadowInit({ a: 1 })).toThrow('WASM not loaded')
    })
  })

  describe('shadow state', () => {
    it('should dump shadow state', () => {
      expect(wasm.shadowDump()).toEqual({
        user: { role: 'guest', age: 20, email: 'test@test.com' },
      })
    })

    it('should get value at path', () => {
      expect(wasm.shadowGet('user.role')).toBe('guest')
    })

    it('should return undefined for missing path', () => {
      expect(wasm.shadowGet('user.missing')).toBeUndefined()
    })

    it('should handle nested objects', () => {
      wasm.shadowInit({ a: { b: { c: { d: 42 } } } })
      expect(wasm.shadowGet('a.b.c.d')).toBe(42)
    })

    it('should handle arrays', () => {
      wasm.shadowInit({ items: [10, 20, 30] })
      expect(wasm.shadowGet('items.0')).toBe(10)
      expect(wasm.shadowGet('items.2')).toBe(30)
    })
  })

  describe('processChanges — echo input', () => {
    it('should echo input changes', () => {
      const result = wasm.processChanges([
        { path: 'user.role', value: 'admin' },
      ])
      expect(result.changes).toHaveLength(1)
      expect(result.changes[0]).toEqual({ path: 'user.role', value: 'admin' })
    })

    it('should update shadow state', () => {
      wasm.processChanges([{ path: 'user.role', value: 'admin' }])
      expect(wasm.shadowGet('user.role')).toBe('admin')
    })

    it('should handle multiple changes', () => {
      const result = wasm.processChanges([
        { path: 'user.role', value: 'admin' },
        { path: 'user.age', value: 30 },
      ])
      expect(result.changes).toHaveLength(2)
      expect(wasm.shadowGet('user.role')).toBe('admin')
      expect(wasm.shadowGet('user.age')).toBe(30)
    })

    it('should handle empty changes', () => {
      const result = wasm.processChanges([])
      expect(result.changes).toHaveLength(0)
    })

    it('should handle nested object replacement', () => {
      wasm.processChanges([
        { path: 'user', value: { role: 'admin', name: 'Bob' } },
      ])
      expect(wasm.shadowGet('user.role')).toBe('admin')
      expect(wasm.shadowGet('user.name')).toBe('Bob')
      expect(wasm.shadowGet('user.age')).toBeUndefined()
    })
  })

  describe('processChanges — BoolLogic evaluation', () => {
    it('should evaluate affected BoolLogic (true case)', () => {
      wasm.registerBoolLogic('_concerns.user.email.disabledWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })

      const result = wasm.processChanges([
        { path: 'user.role', value: 'admin' },
      ])

      expect(result.changes).toHaveLength(1)
      expect(result.concern_changes).toHaveLength(1)
      const bl = result.concern_changes.find((c) =>
        c.path.includes('disabledWhen'),
      )
      expect(bl).toBeDefined()
      expect(bl!.value).toBe(true)
    })

    it('should evaluate affected BoolLogic (false case)', () => {
      wasm.registerBoolLogic('_concerns.user.email.disabledWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })

      const result = wasm.processChanges([
        { path: 'user.role', value: 'editor' },
      ])

      const bl = result.concern_changes.find((c) =>
        c.path.includes('disabledWhen'),
      )
      expect(bl!.value).toBe(false)
    })

    it('should not evaluate unrelated BoolLogic', () => {
      wasm.registerBoolLogic('_concerns.user.email.disabledWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })

      const result = wasm.processChanges([{ path: 'user.age', value: 25 }])
      expect(result.changes).toHaveLength(1)
      expect(result.concern_changes).toHaveLength(0)
    })

    it('should evaluate multiple concerns on same dependency', () => {
      wasm.registerBoolLogic('_concerns.user.email.disabledWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })
      wasm.registerBoolLogic('_concerns.user.email.readonlyWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })
      wasm.registerBoolLogic('_concerns.user.name.visibleWhen', {
        EXISTS: 'user.role',
      })

      const result = wasm.processChanges([
        { path: 'user.role', value: 'admin' },
      ])

      expect(result.changes).toHaveLength(1)
      expect(result.concern_changes).toHaveLength(3)
      const blPaths = result.concern_changes.map((c) => c.path)
      expect(blPaths).toContain('_concerns.user.email.disabledWhen')
      expect(blPaths).toContain('_concerns.user.email.readonlyWhen')
      expect(blPaths).toContain('_concerns.user.name.visibleWhen')
    })

    it('should evaluate complex AND logic', () => {
      wasm.registerBoolLogic('_concerns.user.panel.visibleWhen', {
        AND: [{ IS_EQUAL: ['user.role', 'admin'] }, { GTE: ['user.age', 18] }],
      })

      const result = wasm.processChanges([
        { path: 'user.role', value: 'admin' },
      ])
      const bl = result.concern_changes.find((c) =>
        c.path.includes('visibleWhen'),
      )
      expect(bl!.value).toBe(true)
    })

    it('should handle nested object update triggering BoolLogic', () => {
      wasm.registerBoolLogic('_concerns.user.email.disabledWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })

      const result = wasm.processChanges([
        { path: 'user', value: { role: 'admin', age: 30 } },
      ])

      const bl = result.concern_changes.find((c) =>
        c.path.includes('disabledWhen'),
      )
      expect(bl!.value).toBe(true)
    })

    it('should handle IN operator', () => {
      wasm.registerBoolLogic('_concerns.user.panel.visibleWhen', {
        IN: ['user.role', ['admin', 'editor', 'moderator']],
      })

      const result = wasm.processChanges([
        { path: 'user.role', value: 'editor' },
      ])
      const bl = result.concern_changes.find((c) =>
        c.path.includes('visibleWhen'),
      )
      expect(bl!.value).toBe(true)
    })

    it('should handle NOT operator', () => {
      wasm.registerBoolLogic('_concerns.user.email.disabledWhen', {
        NOT: { IS_EQUAL: ['user.role', 'admin'] },
      })

      const result = wasm.processChanges([
        { path: 'user.role', value: 'editor' },
      ])
      const bl = result.concern_changes.find((c) =>
        c.path.includes('disabledWhen'),
      )
      expect(bl!.value).toBe(true)
    })

    it('should handle IS_EMPTY operator', () => {
      wasm.shadowInit({ user: { bio: 'something' } })
      wasm.registerBoolLogic('_concerns.user.bio.visibleWhen', {
        IS_EMPTY: 'user.bio',
      })

      const result = wasm.processChanges([{ path: 'user.bio', value: '' }])
      const bl = result.concern_changes.find((c) =>
        c.path.includes('visibleWhen'),
      )
      expect(bl!.value).toBe(true)
    })

    it('should handle EXISTS operator', () => {
      wasm.shadowInit({ user: {} })
      wasm.registerBoolLogic('_concerns.user.email.visibleWhen', {
        EXISTS: 'user.email',
      })

      const result = wasm.processChanges([
        { path: 'user.email', value: 'a@b.com' },
      ])
      const bl = result.concern_changes.find((c) =>
        c.path.includes('visibleWhen'),
      )
      expect(bl!.value).toBe(true)
    })
  })

  describe('registration lifecycle', () => {
    it('should unregister BoolLogic expression', () => {
      const id = wasm.registerBoolLogic('_concerns.user.email.disabledWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })

      let result = wasm.processChanges([{ path: 'user.role', value: 'admin' }])
      expect(result.changes).toHaveLength(1)
      expect(result.concern_changes).toHaveLength(1)

      wasm.unregisterBoolLogic(id)

      result = wasm.processChanges([{ path: 'user.role', value: 'editor' }])
      expect(result.changes).toHaveLength(1)
      expect(result.concern_changes).toHaveLength(0)
    })

    it('should handle multiple register/unregister cycles', () => {
      const id1 = wasm.registerBoolLogic('out1', {
        IS_EQUAL: ['user.role', 'admin'],
      })
      const id2 = wasm.registerBoolLogic('out2', { EXISTS: 'user.role' })

      let result = wasm.processChanges([{ path: 'user.role', value: 'admin' }])
      expect(result.changes).toHaveLength(1)
      expect(result.concern_changes).toHaveLength(2)

      wasm.unregisterBoolLogic(id1)
      result = wasm.processChanges([{ path: 'user.role', value: 'editor' }])
      expect(result.changes).toHaveLength(1)
      expect(result.concern_changes).toHaveLength(1)

      wasm.unregisterBoolLogic(id2)
      result = wasm.processChanges([{ path: 'user.role', value: 'guest' }])
      expect(result.changes).toHaveLength(1)
      expect(result.concern_changes).toHaveLength(0)
    })
  })

  describe('intern count', () => {
    it('should track interned paths', () => {
      const before = internCount()
      wasm.registerBoolLogic('out', { IS_EQUAL: ['user.role', 'admin'] })
      expect(internCount()).toBeGreaterThan(before)
    })
  })

  describe('pipelineReset', () => {
    it.todo('should clear all BoolLogic registrations')
    // TODO: Register BoolLogic, verify it fires, call pipelineReset(), re-init shadow, verify it no longer fires
  })

  describe('unregistration lifecycle', () => {
    it.todo('should stop aggregation after unregisterAggregationBatch')
    // TODO: Register aggregation target→sources, unregister, verify write no longer distributes

    it.todo('should stop sync after unregisterSyncBatch')
    // TODO: Register sync pair, unregister, verify change no longer propagates

    it.todo('should stop flip after unregisterFlipBatch')
    // TODO: Register flip pair, unregister, verify boolean no longer inverts

    it.todo('should reject circular aggregation dependencies')
    // TODO: registerAggregationBatch with A→B and B→A, expect throw /Circular aggregation/
  })

  describe('large batch stress', () => {
    it.todo('should process 50+ changes through pipeline with BoolLogic')
    // TODO: Init 60 fields, register BoolLogic on 2, send 55 changes, verify all state + concern changes

    it.todo('should process 50+ changes with sync + flip')
    // TODO: Init 60 fields + sync pair + flip pair, send 52 changes, verify sync/flip propagation at scale
  })

  describe('value types', () => {
    it('should handle string values', () => {
      const result = wasm.processChanges([
        { path: 'user.role', value: 'admin' },
      ])
      expect(result.changes[0].value).toBe('admin')
    })

    it('should handle number values', () => {
      const result = wasm.processChanges([{ path: 'user.age', value: 42 }])
      expect(result.changes[0].value).toBe(42)
    })

    it('should handle boolean values', () => {
      const result = wasm.processChanges([{ path: 'user.active', value: true }])
      expect(result.changes[0].value).toBe(true)
    })

    it('should handle null values', () => {
      const result = wasm.processChanges([
        { path: 'user.deleted', value: null },
      ])
      expect(result.changes[0].value).toBeNull()
    })

    it('should handle object values', () => {
      const result = wasm.processChanges([
        { path: 'user.profile', value: { name: 'Alice', verified: true } },
      ])
      expect(result.changes[0].value).toEqual({
        name: 'Alice',
        verified: true,
      })
    })

    it('should handle array values', () => {
      const result = wasm.processChanges([
        { path: 'user.tags', value: ['admin', 'premium'] },
      ])
      expect(result.changes[0].value).toEqual(['admin', 'premium'])
    })
  })
})
