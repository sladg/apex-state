/**
 * EP2 Pipeline Integration Tests
 *
 * Tests the full WASM pipeline: processChanges + BoolLogic evaluation,
 * plus sync/flip/aggregation when those WASM exports become available.
 *
 * Tests for not-yet-compiled WASM functions are marked with .todo()
 * so they appear in test output as pending work.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest'

import {
  initWasm,
  pipelineReset,
  resetWasm,
  shadowDump,
  shadowGet,
  unregisterAggregationBatch,
  unregisterFlipBatch,
  unregisterSyncBatch,
  wasm,
} from '../../src/wasm/bridge'

beforeEach(async () => {
  const wasmModule = await import('../../rust/pkg/apex_state_wasm.js')
  initWasm(wasmModule)
})

afterEach(() => {
  resetWasm()
})

// ---------------------------------------------------------------------------
// Tests using currently available WASM: processChanges + BoolLogic
// ---------------------------------------------------------------------------

describe('EP2 Pipeline — processChanges + BoolLogic', () => {
  describe('basic processChanges', () => {
    it('should return empty array for empty input', () => {
      wasm.shadowInit({ a: 1 })
      const result = wasm.processChanges([])
      expect(result.changes).toHaveLength(0)
    })

    it('should echo single change and update shadow', () => {
      wasm.shadowInit({ user: { name: 'Alice' } })

      const result = wasm.processChanges([{ path: 'user.name', value: 'Bob' }])

      expect(result.changes).toHaveLength(1)
      expect(result.changes[0]).toEqual({ path: 'user.name', value: 'Bob' })
      expect(shadowGet('user.name')).toBe('Bob')
    })

    it('should process multiple changes in one batch', () => {
      wasm.shadowInit({ x: 0, y: 0, z: 0 })

      const result = wasm.processChanges([
        { path: 'x', value: 10 },
        { path: 'y', value: 20 },
        { path: 'z', value: 30 },
      ])

      expect(result.changes).toHaveLength(3)
      expect(shadowGet('x')).toBe(10)
      expect(shadowGet('y')).toBe(20)
      expect(shadowGet('z')).toBe(30)
    })

    it('should handle nested object replacement', () => {
      wasm.shadowInit({ user: { profile: { name: 'Old', age: 20 } } })

      wasm.processChanges([
        { path: 'user.profile', value: { name: 'New', age: 30 } },
      ])

      expect(shadowGet('user.profile.name')).toBe('New')
      expect(shadowGet('user.profile.age')).toBe(30)
    })

    it('should handle null values', () => {
      wasm.shadowInit({ field: 'value' })

      const result = wasm.processChanges([{ path: 'field', value: null }])

      expect(result.changes[0].value).toBeNull()
      expect(shadowGet('field')).toBeNull()
    })

    it('should handle boolean values', () => {
      wasm.shadowInit({ flag: false })

      wasm.processChanges([{ path: 'flag', value: true }])

      expect(shadowGet('flag')).toBe(true)
    })

    it('should handle array values', () => {
      wasm.shadowInit({ items: [] })

      wasm.processChanges([{ path: 'items', value: [1, 2, 3] }])

      expect(shadowGet('items')).toEqual([1, 2, 3])
    })
  })

  describe('BoolLogic evaluation through pipeline', () => {
    it('should evaluate IS_EQUAL to true when condition matches', () => {
      wasm.shadowInit({ user: { role: 'guest' } })
      wasm.registerBoolLogic('_concerns.user.role.disabledWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })

      const result = wasm.processChanges([
        { path: 'user.role', value: 'admin' },
      ])

      const bl = result.concern_changes.find((c) =>
        c.path.includes('disabledWhen'),
      )
      expect(bl).toBeDefined()
      expect(bl!.value).toBe(true)
    })

    it('should evaluate IS_EQUAL to false when condition does not match', () => {
      wasm.shadowInit({ user: { role: 'guest' } })
      wasm.registerBoolLogic('_concerns.user.role.disabledWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })

      const result = wasm.processChanges([
        { path: 'user.role', value: 'editor' },
      ])

      const bl = result.concern_changes.find((c) =>
        c.path.includes('disabledWhen'),
      )
      expect(bl).toBeDefined()
      expect(bl!.value).toBe(false)
    })

    it('should not evaluate BoolLogic for unrelated path changes', () => {
      wasm.shadowInit({ user: { role: 'guest', age: 20 } })
      wasm.registerBoolLogic('_concerns.user.role.disabledWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })

      const result = wasm.processChanges([{ path: 'user.age', value: 25 }])

      // Only the direct change, no BoolLogic output
      expect(result.changes).toHaveLength(1)
      expect(result.changes[0].path).toBe('user.age')
      expect(result.concern_changes).toHaveLength(0)
    })

    it('should evaluate multiple BoolLogics on same dependency', () => {
      wasm.shadowInit({ user: { role: 'guest' } })
      wasm.registerBoolLogic('_concerns.user.role.disabledWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })
      wasm.registerBoolLogic('_concerns.user.role.readonlyWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })
      wasm.registerBoolLogic('_concerns.user.name.visibleWhen', {
        EXISTS: 'user.role',
      })

      const result = wasm.processChanges([
        { path: 'user.role', value: 'admin' },
      ])

      // 1 state change + 3 concern changes
      expect(result.changes).toHaveLength(1)
      expect(result.concern_changes).toHaveLength(3)

      const blPaths = result.concern_changes.map((c) => c.path)
      expect(blPaths).toContain('_concerns.user.role.disabledWhen')
      expect(blPaths).toContain('_concerns.user.role.readonlyWhen')
      expect(blPaths).toContain('_concerns.user.name.visibleWhen')
    })

    it('should evaluate complex AND logic', () => {
      wasm.shadowInit({ user: { role: 'guest', age: 20 } })
      wasm.registerBoolLogic('_concerns.panel.visibleWhen', {
        AND: [{ IS_EQUAL: ['user.role', 'admin'] }, { GTE: ['user.age', 18] }],
      })

      const result = wasm.processChanges([
        { path: 'user.role', value: 'admin' },
      ])

      const bl = result.concern_changes.find((c) =>
        c.path.includes('visibleWhen'),
      )
      expect(bl).toBeDefined()
      expect(bl!.value).toBe(true)
    })

    it('should evaluate NOT operator', () => {
      wasm.shadowInit({ user: { role: 'guest' } })
      wasm.registerBoolLogic('_concerns.user.role.enabledWhen', {
        NOT: { IS_EQUAL: ['user.role', 'admin'] },
      })

      const result = wasm.processChanges([
        { path: 'user.role', value: 'editor' },
      ])

      const bl = result.concern_changes.find((c) =>
        c.path.includes('enabledWhen'),
      )
      expect(bl!.value).toBe(true)
    })

    it('should evaluate IN operator', () => {
      wasm.shadowInit({ user: { role: 'guest' } })
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

    it('should handle nested object update triggering BoolLogic', () => {
      wasm.shadowInit({ user: { role: 'guest', age: 20 } })
      wasm.registerBoolLogic('_concerns.user.email.disabledWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })

      const result = wasm.processChanges([
        { path: 'user', value: { role: 'admin', age: 30 } },
      ])

      const bl = result.concern_changes.find((c) =>
        c.path.includes('disabledWhen'),
      )
      expect(bl).toBeDefined()
      expect(bl!.value).toBe(true)
    })
  })

  describe('BoolLogic unregistration', () => {
    it('should stop evaluating after unregister', () => {
      wasm.shadowInit({ user: { role: 'guest' } })
      const id = wasm.registerBoolLogic('_concerns.user.role.disabledWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })

      // Verify it works
      let result = wasm.processChanges([{ path: 'user.role', value: 'admin' }])
      expect(result.changes).toHaveLength(1)
      expect(result.concern_changes).toHaveLength(1)

      // Unregister
      wasm.unregisterBoolLogic(id)

      result = wasm.processChanges([{ path: 'user.role', value: 'editor' }])
      expect(result.changes).toHaveLength(1)
      expect(result.concern_changes).toHaveLength(0)
    })

    it('should handle multiple register/unregister cycles', () => {
      wasm.shadowInit({ user: { role: 'guest' } })
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

  describe('shadow state consistency', () => {
    it('should maintain full shadow state dump after pipeline', () => {
      wasm.shadowInit({ x: 1, y: 2, z: 3 })

      wasm.processChanges([{ path: 'x', value: 10 }])

      const dump = shadowDump() as Record<string, unknown>
      expect(dump).toEqual({ x: 10, y: 2, z: 3 })
    })

    it('should update shadow state for all changes in a batch', () => {
      wasm.shadowInit({ a: 1, b: 2, c: 3 })

      wasm.processChanges([
        { path: 'a', value: 100 },
        { path: 'b', value: 200 },
      ])

      expect(shadowGet('a')).toBe(100)
      expect(shadowGet('b')).toBe(200)
      expect(shadowGet('c')).toBe(3)
    })

    it('should handle deep nested path updates in shadow', () => {
      wasm.shadowInit({ a: { b: { c: { d: 0 } } } })

      wasm.processChanges([{ path: 'a.b.c.d', value: 42 }])

      expect(shadowGet('a.b.c.d')).toBe(42)
      expect(shadowGet('a.b.c')).toEqual({ d: 42 })
    })
  })

  describe('large batch', () => {
    it('should process 50+ changes through pipeline', () => {
      const initialState: Record<string, number> = {}
      for (let i = 0; i < 60; i++) {
        initialState[`field${i}`] = 0
      }
      wasm.shadowInit(initialState)

      // Register BoolLogic on a few fields
      wasm.registerBoolLogic('_concerns.field0.active', {
        GTE: ['field0', 50],
      })
      wasm.registerBoolLogic('_concerns.field10.active', {
        GTE: ['field10', 50],
      })

      const changes = Array.from({ length: 55 }, (_, i) => ({
        path: `field${i}`,
        value: i + 100,
      }))

      const result = wasm.processChanges(changes)

      // 55 state changes + 2 concern changes
      expect(result.changes).toHaveLength(55)
      expect(result.concern_changes).toHaveLength(2)

      // Shadow should be consistent
      for (let i = 0; i < 55; i++) {
        expect(shadowGet(`field${i}`)).toBe(i + 100)
      }
      // Untouched fields remain
      expect(shadowGet('field55')).toBe(0)
    })
  })

  describe('pipelineReset', () => {
    it('should clear all BoolLogic registrations', () => {
      wasm.shadowInit({ x: 0 })
      wasm.registerBoolLogic('_concerns.x.active', { GTE: ['x', 5] })

      let result = wasm.processChanges([{ path: 'x', value: 10 }])
      expect(result.changes).toHaveLength(1)
      expect(result.concern_changes).toHaveLength(1)

      pipelineReset()
      wasm.shadowInit({ x: 0 })

      result = wasm.processChanges([{ path: 'x', value: 10 }])
      expect(result.changes).toHaveLength(1)
      expect(result.concern_changes).toHaveLength(0)
    })
  })
})

// ---------------------------------------------------------------------------
// EP2: sync/flip/aggregation pipeline tests
// ---------------------------------------------------------------------------

describe('EP2 Pipeline — sync/flip/aggregation', () => {
  describe('aggregation', () => {
    it('should distribute target write to source paths', () => {
      wasm.shadowInit({ allUsers: null, user1: 'a', user2: 'b', user3: 'c' })
      wasm.registerAggregationBatch([
        { target: 'allUsers', sources: ['user1', 'user2', 'user3'] },
      ])

      const result = wasm.processChanges([{ path: 'allUsers', value: 'alice' }])

      const paths = result.changes.map((c) => c.path)
      expect(paths).toContain('user1')
      expect(paths).toContain('user2')
      expect(paths).toContain('user3')
      expect(paths).not.toContain('allUsers')
      expect(result.changes.every((c) => c.value === 'alice')).toBe(true)
    })

    it('should handle aggregation with child path', () => {
      wasm.shadowInit({ allUsers: {}, user1: {}, user2: {} })
      wasm.registerAggregationBatch([
        { target: 'allUsers', sources: ['user1', 'user2'] },
      ])

      const result = wasm.processChanges([
        { path: 'allUsers.email', value: 'test@example.com' },
      ])

      const paths = result.changes.map((c) => c.path)
      expect(paths).toContain('user1.email')
      expect(paths).toContain('user2.email')
    })

    it('should stop aggregation after unregistration', () => {
      wasm.shadowInit({ allUsers: null, user1: 'a', user2: 'b' })
      wasm.registerAggregationBatch([
        { target: 'allUsers', sources: ['user1', 'user2'] },
      ])

      unregisterAggregationBatch(['allUsers'])

      const result = wasm.processChanges([{ path: 'allUsers', value: 'alice' }])
      expect(result.changes).toHaveLength(1)
      expect(result.changes[0].path).toBe('allUsers')
    })
  })

  describe('sync', () => {
    it('should propagate value to sync peer', () => {
      wasm.shadowInit({ a: 'old', b: 'old' })
      wasm.registerSyncBatch([['a', 'b']])

      const result = wasm.processChanges([{ path: 'a', value: 'new' }])

      expect(result.changes).toHaveLength(2)
      const paths = result.changes.map((c) => c.path)
      expect(paths).toContain('a')
      expect(paths).toContain('b')
      expect(result.changes.every((c) => c.value === 'new')).toBe(true)
      expect(shadowGet('b')).toBe('new')
    })

    it('should sync in the reverse direction', () => {
      wasm.shadowInit({ a: 'old', b: 'old' })
      wasm.registerSyncBatch([['a', 'b']])

      const result = wasm.processChanges([{ path: 'b', value: 'reverse' }])

      const paths = result.changes.map((c) => c.path)
      expect(paths).toContain('a')
      expect(paths).toContain('b')
      expect(shadowGet('a')).toBe('reverse')
    })

    it('should stop sync after unregistration', () => {
      wasm.shadowInit({ a: 'old', b: 'old' })
      wasm.registerSyncBatch([['a', 'b']])
      unregisterSyncBatch([['a', 'b']])

      const result = wasm.processChanges([{ path: 'a', value: 'new' }])
      expect(result.changes).toHaveLength(1)
      expect(result.changes[0].path).toBe('a')
    })

    it('should sync multi-component groups', () => {
      wasm.shadowInit({ a: 'x', b: 'x', c: 'x' })
      wasm.registerSyncBatch([
        ['a', 'b'],
        ['b', 'c'],
      ])

      const result = wasm.processChanges([{ path: 'a', value: 'synced' }])

      const paths = result.changes.map((c) => c.path)
      expect(paths).toContain('a')
      expect(paths).toContain('b')
      expect(paths).toContain('c')
    })
  })

  describe('flip', () => {
    it('should invert boolean value for flip peer', () => {
      // Start with opposite values so the change is NOT a no-op
      wasm.shadowInit({ visible: false, hidden: true })
      wasm.registerFlipBatch([['visible', 'hidden']])

      const result = wasm.processChanges([{ path: 'visible', value: true }])

      expect(result.changes).toHaveLength(2)
      const hidden = result.changes.find((c) => c.path === 'hidden')
      expect(hidden!.value).toBe(false)
    })

    it('should flip in the reverse direction', () => {
      // Start with opposite values so the change is NOT a no-op
      wasm.shadowInit({ visible: false, hidden: true })
      wasm.registerFlipBatch([['visible', 'hidden']])

      const result = wasm.processChanges([{ path: 'hidden', value: false }])

      const visible = result.changes.find((c) => c.path === 'visible')
      expect(visible!.value).toBe(true)
    })

    it('should not flip non-boolean values', () => {
      wasm.shadowInit({ a: 'x', b: 'y' })
      wasm.registerFlipBatch([['a', 'b']])

      const result = wasm.processChanges([{ path: 'a', value: 'hello' }])
      expect(result.changes).toHaveLength(1)
    })

    it('should stop flip after unregistration', () => {
      // Start with opposite value so the change is NOT a no-op
      wasm.shadowInit({ a: false, b: true })
      wasm.registerFlipBatch([['a', 'b']])
      unregisterFlipBatch([['a', 'b']])

      const result = wasm.processChanges([{ path: 'a', value: true }])
      expect(result.changes).toHaveLength(1)
    })
  })

  describe('combined pipeline', () => {
    it('should process sync + flip in one batch', () => {
      // Start with different values so changes are NOT no-ops
      wasm.shadowInit({ a: 1, b: 1, flag1: false, flag2: true })
      wasm.registerSyncBatch([['a', 'b']])
      wasm.registerFlipBatch([['flag1', 'flag2']])

      const result = wasm.processChanges([
        { path: 'a', value: 42 },
        { path: 'flag1', value: true },
      ])

      const paths = result.changes.map((c) => c.path)
      expect(paths).toContain('a')
      expect(paths).toContain('b')
      expect(paths).toContain('flag1')
      expect(paths).toContain('flag2')

      expect(shadowGet('b')).toBe(42)
      expect(shadowGet('flag2')).toBe(false)
    })

    it('should trigger BoolLogic from synced changes', () => {
      wasm.shadowInit({ x: 'old', y: 'old' })
      wasm.registerSyncBatch([['x', 'y']])
      wasm.registerBoolLogic('_concerns.field.disabledWhen', {
        IS_EQUAL: ['y', 'admin'],
      })

      const result = wasm.processChanges([{ path: 'x', value: 'admin' }])

      const bl = result.concern_changes.find((c) =>
        c.path.includes('disabledWhen'),
      )
      expect(bl).toBeDefined()
      expect(bl!.value).toBe(true)
    })

    it('should trigger BoolLogic from flip-produced changes', () => {
      wasm.shadowInit({ flag: true, inverse: false })
      wasm.registerFlipBatch([['flag', 'inverse']])
      wasm.registerBoolLogic('_concerns.panel.visibleWhen', {
        IS_EQUAL: ['inverse', true],
      })

      const result = wasm.processChanges([{ path: 'flag', value: false }])

      const bl = result.concern_changes.find((c) =>
        c.path.includes('visibleWhen'),
      )
      expect(bl).toBeDefined()
      expect(bl!.value).toBe(true)
    })
  })

  describe('shadow state with sync/flip', () => {
    it('should reflect all changes including sync/flip in shadow', () => {
      // Start with different values so changes are NOT no-ops
      wasm.shadowInit({ a: 1, b: 1, x: false, y: true })
      wasm.registerSyncBatch([['a', 'b']])
      wasm.registerFlipBatch([['x', 'y']])

      wasm.processChanges([
        { path: 'a', value: 99 },
        { path: 'x', value: true },
      ])

      expect(shadowGet('a')).toBe(99)
      expect(shadowGet('b')).toBe(99)
      expect(shadowGet('x')).toBe(true)
      expect(shadowGet('y')).toBe(false)
    })
  })

  describe('large batch with sync/flip', () => {
    it('should process 50+ changes through full pipeline', () => {
      const state: Record<string, unknown> = {}
      for (let i = 0; i < 60; i++) state[`f${i}`] = 0
      state['flagA'] = true
      state['flagB'] = false
      state['syncA'] = 'x'
      state['syncB'] = 'x'
      wasm.shadowInit(state)

      wasm.registerSyncBatch([['syncA', 'syncB']])
      wasm.registerFlipBatch([['flagA', 'flagB']])

      const changes = Array.from({ length: 50 }, (_, i) => ({
        path: `f${i}`,
        value: i + 1,
      }))
      changes.push({ path: 'syncA', value: 'synced' })
      changes.push({ path: 'flagA', value: false })

      const result = wasm.processChanges(changes)

      // 52 direct + 1 sync + 1 flip = 54
      expect(result.changes.length).toBeGreaterThanOrEqual(54)
      expect(shadowGet('syncB')).toBe('synced')
      expect(shadowGet('flagB')).toBe(true)
    })
  })
})
