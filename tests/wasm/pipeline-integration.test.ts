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
  processChanges,
  registerAggregationBatch,
  registerBoolLogic,
  registerFlipBatch,
  registerSyncBatch,
  resetWasm,
  shadowDump,
  shadowGet,
  shadowInit,
  unregisterAggregationBatch,
  unregisterBoolLogic,
  unregisterFlipBatch,
  unregisterSyncBatch,
} from '../../src/wasm/bridge'

beforeEach(async () => {
  const wasmModule = await import('../../rust/pkg-node/apex_state_wasm.js')
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
      shadowInit({ a: 1 })
      const result = processChanges([])
      expect(result.changes).toHaveLength(0)
    })

    it('should echo single change and update shadow', () => {
      shadowInit({ user: { name: 'Alice' } })

      const result = processChanges([{ path: 'user.name', value: 'Bob' }])

      expect(result.changes).toHaveLength(1)
      expect(result.changes[0]).toEqual({ path: 'user.name', value: 'Bob' })
      expect(shadowGet('user.name')).toBe('Bob')
    })

    it('should process multiple changes in one batch', () => {
      shadowInit({ x: 0, y: 0, z: 0 })

      const result = processChanges([
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
      shadowInit({ user: { profile: { name: 'Old', age: 20 } } })

      processChanges([
        { path: 'user.profile', value: { name: 'New', age: 30 } },
      ])

      expect(shadowGet('user.profile.name')).toBe('New')
      expect(shadowGet('user.profile.age')).toBe(30)
    })

    it('should handle null values', () => {
      shadowInit({ field: 'value' })

      const result = processChanges([{ path: 'field', value: null }])

      expect(result.changes[0].value).toBeNull()
      expect(shadowGet('field')).toBeNull()
    })

    it('should handle boolean values', () => {
      shadowInit({ flag: false })

      processChanges([{ path: 'flag', value: true }])

      expect(shadowGet('flag')).toBe(true)
    })

    it('should handle array values', () => {
      shadowInit({ items: [] })

      processChanges([{ path: 'items', value: [1, 2, 3] }])

      expect(shadowGet('items')).toEqual([1, 2, 3])
    })
  })

  describe('BoolLogic evaluation through pipeline', () => {
    it('should evaluate IS_EQUAL to true when condition matches', () => {
      shadowInit({ user: { role: 'guest' } })
      registerBoolLogic('_concerns.user.role.disabledWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })

      const result = processChanges([{ path: 'user.role', value: 'admin' }])

      const bl = result.concern_changes.find((c) =>
        c.path.includes('disabledWhen'),
      )
      expect(bl).toBeDefined()
      expect(bl!.value).toBe(true)
    })

    it('should evaluate IS_EQUAL to false when condition does not match', () => {
      shadowInit({ user: { role: 'guest' } })
      registerBoolLogic('_concerns.user.role.disabledWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })

      const result = processChanges([{ path: 'user.role', value: 'editor' }])

      const bl = result.concern_changes.find((c) =>
        c.path.includes('disabledWhen'),
      )
      expect(bl).toBeDefined()
      expect(bl!.value).toBe(false)
    })

    it('should not evaluate BoolLogic for unrelated path changes', () => {
      shadowInit({ user: { role: 'guest', age: 20 } })
      registerBoolLogic('_concerns.user.role.disabledWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })

      const result = processChanges([{ path: 'user.age', value: 25 }])

      // Only the direct change, no BoolLogic output
      expect(result.changes).toHaveLength(1)
      expect(result.changes[0].path).toBe('user.age')
      expect(result.concern_changes).toHaveLength(0)
    })

    it('should evaluate multiple BoolLogics on same dependency', () => {
      shadowInit({ user: { role: 'guest' } })
      registerBoolLogic('_concerns.user.role.disabledWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })
      registerBoolLogic('_concerns.user.role.readonlyWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })
      registerBoolLogic('_concerns.user.name.visibleWhen', {
        EXISTS: 'user.role',
      })

      const result = processChanges([{ path: 'user.role', value: 'admin' }])

      // 1 state change + 3 concern changes
      expect(result.changes).toHaveLength(1)
      expect(result.concern_changes).toHaveLength(3)

      const blPaths = result.concern_changes.map((c) => c.path)
      expect(blPaths).toContain('_concerns.user.role.disabledWhen')
      expect(blPaths).toContain('_concerns.user.role.readonlyWhen')
      expect(blPaths).toContain('_concerns.user.name.visibleWhen')
    })

    it('should evaluate complex AND logic', () => {
      shadowInit({ user: { role: 'guest', age: 20 } })
      registerBoolLogic('_concerns.panel.visibleWhen', {
        AND: [{ IS_EQUAL: ['user.role', 'admin'] }, { GTE: ['user.age', 18] }],
      })

      const result = processChanges([{ path: 'user.role', value: 'admin' }])

      const bl = result.concern_changes.find((c) =>
        c.path.includes('visibleWhen'),
      )
      expect(bl).toBeDefined()
      expect(bl!.value).toBe(true)
    })

    it('should evaluate NOT operator', () => {
      shadowInit({ user: { role: 'guest' } })
      registerBoolLogic('_concerns.user.role.enabledWhen', {
        NOT: { IS_EQUAL: ['user.role', 'admin'] },
      })

      const result = processChanges([{ path: 'user.role', value: 'editor' }])

      const bl = result.concern_changes.find((c) =>
        c.path.includes('enabledWhen'),
      )
      expect(bl!.value).toBe(true)
    })

    it('should evaluate IN operator', () => {
      shadowInit({ user: { role: 'guest' } })
      registerBoolLogic('_concerns.user.panel.visibleWhen', {
        IN: ['user.role', ['admin', 'editor', 'moderator']],
      })

      const result = processChanges([{ path: 'user.role', value: 'editor' }])

      const bl = result.concern_changes.find((c) =>
        c.path.includes('visibleWhen'),
      )
      expect(bl!.value).toBe(true)
    })

    it('should handle nested object update triggering BoolLogic', () => {
      shadowInit({ user: { role: 'guest', age: 20 } })
      registerBoolLogic('_concerns.user.email.disabledWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })

      const result = processChanges([
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
      shadowInit({ user: { role: 'guest' } })
      const id = registerBoolLogic('_concerns.user.role.disabledWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })

      // Verify it works
      let result = processChanges([{ path: 'user.role', value: 'admin' }])
      expect(result.changes).toHaveLength(1)
      expect(result.concern_changes).toHaveLength(1)

      // Unregister
      unregisterBoolLogic(id)

      result = processChanges([{ path: 'user.role', value: 'editor' }])
      expect(result.changes).toHaveLength(1)
      expect(result.concern_changes).toHaveLength(0)
    })

    it('should handle multiple register/unregister cycles', () => {
      shadowInit({ user: { role: 'guest' } })
      const id1 = registerBoolLogic('out1', {
        IS_EQUAL: ['user.role', 'admin'],
      })
      const id2 = registerBoolLogic('out2', { EXISTS: 'user.role' })

      let result = processChanges([{ path: 'user.role', value: 'admin' }])
      expect(result.changes).toHaveLength(1)
      expect(result.concern_changes).toHaveLength(2)

      unregisterBoolLogic(id1)
      result = processChanges([{ path: 'user.role', value: 'editor' }])
      expect(result.changes).toHaveLength(1)
      expect(result.concern_changes).toHaveLength(1)

      unregisterBoolLogic(id2)
      result = processChanges([{ path: 'user.role', value: 'guest' }])
      expect(result.changes).toHaveLength(1)
      expect(result.concern_changes).toHaveLength(0)
    })
  })

  describe('shadow state consistency', () => {
    it('should maintain full shadow state dump after pipeline', () => {
      shadowInit({ x: 1, y: 2, z: 3 })

      processChanges([{ path: 'x', value: 10 }])

      const dump = shadowDump() as Record<string, unknown>
      expect(dump).toEqual({ x: 10, y: 2, z: 3 })
    })

    it('should update shadow state for all changes in a batch', () => {
      shadowInit({ a: 1, b: 2, c: 3 })

      processChanges([
        { path: 'a', value: 100 },
        { path: 'b', value: 200 },
      ])

      expect(shadowGet('a')).toBe(100)
      expect(shadowGet('b')).toBe(200)
      expect(shadowGet('c')).toBe(3)
    })

    it('should handle deep nested path updates in shadow', () => {
      shadowInit({ a: { b: { c: { d: 0 } } } })

      processChanges([{ path: 'a.b.c.d', value: 42 }])

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
      shadowInit(initialState)

      // Register BoolLogic on a few fields
      registerBoolLogic('_concerns.field0.active', {
        GTE: ['field0', 50],
      })
      registerBoolLogic('_concerns.field10.active', {
        GTE: ['field10', 50],
      })

      const changes = Array.from({ length: 55 }, (_, i) => ({
        path: `field${i}`,
        value: i + 100,
      }))

      const result = processChanges(changes)

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
      shadowInit({ x: 0 })
      registerBoolLogic('_concerns.x.active', { GTE: ['x', 5] })

      let result = processChanges([{ path: 'x', value: 10 }])
      expect(result.changes).toHaveLength(1)
      expect(result.concern_changes).toHaveLength(1)

      pipelineReset()
      shadowInit({ x: 0 })

      result = processChanges([{ path: 'x', value: 10 }])
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
      shadowInit({ allUsers: null, user1: 'a', user2: 'b', user3: 'c' })
      registerAggregationBatch([
        { target: 'allUsers', sources: ['user1', 'user2', 'user3'] },
      ])

      const result = processChanges([{ path: 'allUsers', value: 'alice' }])

      const paths = result.changes.map((c) => c.path)
      expect(paths).toContain('user1')
      expect(paths).toContain('user2')
      expect(paths).toContain('user3')
      expect(paths).not.toContain('allUsers')
      expect(result.changes.every((c) => c.value === 'alice')).toBe(true)
    })

    it('should handle aggregation with child path', () => {
      shadowInit({ allUsers: {}, user1: {}, user2: {} })
      registerAggregationBatch([
        { target: 'allUsers', sources: ['user1', 'user2'] },
      ])

      const result = processChanges([
        { path: 'allUsers.email', value: 'test@example.com' },
      ])

      const paths = result.changes.map((c) => c.path)
      expect(paths).toContain('user1.email')
      expect(paths).toContain('user2.email')
    })

    it('should stop aggregation after unregistration', () => {
      shadowInit({ allUsers: null, user1: 'a', user2: 'b' })
      registerAggregationBatch([
        { target: 'allUsers', sources: ['user1', 'user2'] },
      ])

      unregisterAggregationBatch(['allUsers'])

      const result = processChanges([{ path: 'allUsers', value: 'alice' }])
      expect(result.changes).toHaveLength(1)
      expect(result.changes[0].path).toBe('allUsers')
    })
  })

  describe('sync', () => {
    it('should propagate value to sync peer', () => {
      shadowInit({ a: 'old', b: 'old' })
      registerSyncBatch([['a', 'b']])

      const result = processChanges([{ path: 'a', value: 'new' }])

      expect(result.changes).toHaveLength(2)
      const paths = result.changes.map((c) => c.path)
      expect(paths).toContain('a')
      expect(paths).toContain('b')
      expect(result.changes.every((c) => c.value === 'new')).toBe(true)
      expect(shadowGet('b')).toBe('new')
    })

    it('should sync in the reverse direction', () => {
      shadowInit({ a: 'old', b: 'old' })
      registerSyncBatch([['a', 'b']])

      const result = processChanges([{ path: 'b', value: 'reverse' }])

      const paths = result.changes.map((c) => c.path)
      expect(paths).toContain('a')
      expect(paths).toContain('b')
      expect(shadowGet('a')).toBe('reverse')
    })

    it('should stop sync after unregistration', () => {
      shadowInit({ a: 'old', b: 'old' })
      registerSyncBatch([['a', 'b']])
      unregisterSyncBatch([['a', 'b']])

      const result = processChanges([{ path: 'a', value: 'new' }])
      expect(result.changes).toHaveLength(1)
      expect(result.changes[0].path).toBe('a')
    })

    it('should sync multi-component groups', () => {
      shadowInit({ a: 'x', b: 'x', c: 'x' })
      registerSyncBatch([
        ['a', 'b'],
        ['b', 'c'],
      ])

      const result = processChanges([{ path: 'a', value: 'synced' }])

      const paths = result.changes.map((c) => c.path)
      expect(paths).toContain('a')
      expect(paths).toContain('b')
      expect(paths).toContain('c')
    })
  })

  describe('flip', () => {
    it('should invert boolean value for flip peer', () => {
      // Start with opposite values so the change is NOT a no-op
      shadowInit({ visible: false, hidden: true })
      registerFlipBatch([['visible', 'hidden']])

      const result = processChanges([{ path: 'visible', value: true }])

      expect(result.changes).toHaveLength(2)
      const hidden = result.changes.find((c) => c.path === 'hidden')
      expect(hidden!.value).toBe(false)
    })

    it('should flip in the reverse direction', () => {
      // Start with opposite values so the change is NOT a no-op
      shadowInit({ visible: false, hidden: true })
      registerFlipBatch([['visible', 'hidden']])

      const result = processChanges([{ path: 'hidden', value: false }])

      const visible = result.changes.find((c) => c.path === 'visible')
      expect(visible!.value).toBe(true)
    })

    it('should not flip non-boolean values', () => {
      shadowInit({ a: 'x', b: 'y' })
      registerFlipBatch([['a', 'b']])

      const result = processChanges([{ path: 'a', value: 'hello' }])
      expect(result.changes).toHaveLength(1)
    })

    it('should stop flip after unregistration', () => {
      // Start with opposite value so the change is NOT a no-op
      shadowInit({ a: false, b: true })
      registerFlipBatch([['a', 'b']])
      unregisterFlipBatch([['a', 'b']])

      const result = processChanges([{ path: 'a', value: true }])
      expect(result.changes).toHaveLength(1)
    })
  })

  describe('combined pipeline', () => {
    it('should process sync + flip in one batch', () => {
      // Start with different values so changes are NOT no-ops
      shadowInit({ a: 1, b: 1, flag1: false, flag2: true })
      registerSyncBatch([['a', 'b']])
      registerFlipBatch([['flag1', 'flag2']])

      const result = processChanges([
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
      shadowInit({ x: 'old', y: 'old' })
      registerSyncBatch([['x', 'y']])
      registerBoolLogic('_concerns.field.disabledWhen', {
        IS_EQUAL: ['y', 'admin'],
      })

      const result = processChanges([{ path: 'x', value: 'admin' }])

      const bl = result.concern_changes.find((c) =>
        c.path.includes('disabledWhen'),
      )
      expect(bl).toBeDefined()
      expect(bl!.value).toBe(true)
    })

    it('should trigger BoolLogic from flip-produced changes', () => {
      shadowInit({ flag: true, inverse: false })
      registerFlipBatch([['flag', 'inverse']])
      registerBoolLogic('_concerns.panel.visibleWhen', {
        IS_EQUAL: ['inverse', true],
      })

      const result = processChanges([{ path: 'flag', value: false }])

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
      shadowInit({ a: 1, b: 1, x: false, y: true })
      registerSyncBatch([['a', 'b']])
      registerFlipBatch([['x', 'y']])

      processChanges([
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
      shadowInit(state)

      registerSyncBatch([['syncA', 'syncB']])
      registerFlipBatch([['flagA', 'flagB']])

      const changes = Array.from({ length: 50 }, (_, i) => ({
        path: `f${i}`,
        value: i + 1,
      }))
      changes.push({ path: 'syncA', value: 'synced' })
      changes.push({ path: 'flagA', value: false })

      const result = processChanges(changes)

      // 52 direct + 1 sync + 1 flip = 54
      expect(result.changes.length).toBeGreaterThanOrEqual(54)
      expect(shadowGet('syncB')).toBe('synced')
      expect(shadowGet('flagB')).toBe(true)
    })
  })
})
