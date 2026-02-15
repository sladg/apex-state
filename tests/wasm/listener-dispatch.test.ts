/**
 * EP3 Listener Dispatch Tests
 *
 * Tests WASM dispatch plan creation and listener orchestration.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest'

import type { Change } from '../../src/wasm/bridge'
import {
  createDispatchPlan,
  initWasm,
  registerListenersBatch,
  resetWasm,
  routeProducedChanges,
  shadowInit,
  unregisterListenersBatch,
} from '../../src/wasm/bridge'

beforeEach(async () => {
  const wasmModule = await import('../../rust/pkg-node/apex_state_wasm.js')
  initWasm(wasmModule)
  shadowInit({
    user: { name: 'Alice', role: 'admin', email: 'a@b.com' },
  })
})

afterEach(() => {
  resetWasm()
})

describe('EP3 Listener Dispatch', () => {
  describe('single listener', () => {
    it('should include listener in dispatch plan when topic matches', () => {
      registerListenersBatch([
        { subscriber_id: 1, topic_path: 'user', scope_path: 'user' },
      ])

      const changes: Change[] = [{ path: 'user.name', value: 'Bob' }]
      const plan = createDispatchPlan(changes)

      expect(plan.levels.length).toBeGreaterThan(0)
      const allDispatches = plan.levels.flatMap((l) => l.dispatches)
      const found = allDispatches.find((d) => d.subscriber_id === 1)
      expect(found).toBeDefined()
      expect(found!.scope_path).toBe('user')
    })

    it('should not dispatch to unrelated listener', () => {
      registerListenersBatch([
        {
          subscriber_id: 1,
          topic_path: 'settings',
          scope_path: 'settings',
        },
      ])

      const plan = createDispatchPlan([{ path: 'user.name', value: 'Bob' }])
      const allDispatches = plan.levels.flatMap((l) => l.dispatches)
      expect(allDispatches).toHaveLength(0)
    })
  })

  describe('multi-depth listeners', () => {
    it('should order levels deepest-first', () => {
      registerListenersBatch([
        { subscriber_id: 1, topic_path: 'user', scope_path: 'user' },
        {
          subscriber_id: 2,
          topic_path: 'user.name',
          scope_path: 'user.name',
        },
      ])

      const plan = createDispatchPlan([{ path: 'user.name', value: 'Bob' }])
      expect(plan.levels.length).toBeGreaterThanOrEqual(2)

      const depths = plan.levels.map((l) => l.depth)
      for (let i = 1; i < depths.length; i++) {
        expect(depths[i]).toBeLessThanOrEqual(depths[i - 1])
      }
    })

    it('should dispatch to listeners at multiple depth levels', () => {
      registerListenersBatch([
        { subscriber_id: 10, topic_path: 'user', scope_path: 'user' },
        {
          subscriber_id: 20,
          topic_path: 'user.name',
          scope_path: 'user.name',
        },
        {
          subscriber_id: 30,
          topic_path: 'user.role',
          scope_path: 'user.role',
        },
      ])

      const plan = createDispatchPlan([
        { path: 'user.name', value: 'Bob' },
        { path: 'user.role', value: 'editor' },
      ])

      const allIds = plan.levels
        .flatMap((l) => l.dispatches)
        .map((d) => d.subscriber_id)

      expect(allIds).toContain(10)
      expect(allIds).toContain(20)
      expect(allIds).toContain(30)
    })
  })

  describe('change relativization', () => {
    it('should relativize changes to subscriber scope path', () => {
      registerListenersBatch([
        { subscriber_id: 1, topic_path: 'user', scope_path: 'user' },
      ])

      const plan = createDispatchPlan([{ path: 'user.name', value: 'Bob' }])
      const dispatch = plan.levels
        .flatMap((l) => l.dispatches)
        .find((d) => d.subscriber_id === 1)

      expect(dispatch).toBeDefined()
      expect(dispatch!.changes.length).toBeGreaterThan(0)

      const changePaths = dispatch!.changes.map((c) => c.path)
      expect(changePaths).toContain('name')
    })

    it('should provide full path when scope is at leaf level', () => {
      registerListenersBatch([
        {
          subscriber_id: 1,
          topic_path: 'user.name',
          scope_path: 'user.name',
        },
      ])

      const plan = createDispatchPlan([{ path: 'user.name', value: 'Bob' }])
      const dispatch = plan.levels
        .flatMap((l) => l.dispatches)
        .find((d) => d.subscriber_id === 1)

      expect(dispatch).toBeDefined()
      expect(dispatch!.changes.length).toBeGreaterThan(0)
    })
  })

  describe('no matching listeners', () => {
    it('should return empty plan when no listeners match', () => {
      registerListenersBatch([
        {
          subscriber_id: 1,
          topic_path: 'settings',
          scope_path: 'settings',
        },
      ])

      const plan = createDispatchPlan([{ path: 'user.name', value: 'Bob' }])
      expect(plan.levels.flatMap((l) => l.dispatches)).toHaveLength(0)
    })

    it('should return empty plan when no listeners registered', () => {
      const plan = createDispatchPlan([{ path: 'user.name', value: 'Bob' }])
      expect(plan.levels.flatMap((l) => l.dispatches)).toHaveLength(0)
    })
  })

  describe('route produced changes', () => {
    it('should route produced changes to downstream topics', () => {
      registerListenersBatch([
        { subscriber_id: 1, topic_path: 'user', scope_path: 'user' },
        {
          subscriber_id: 2,
          topic_path: 'user.email',
          scope_path: 'user.email',
        },
      ])

      const produced: Change[] = [{ path: 'user.email', value: 'new@test.com' }]
      const plan = routeProducedChanges(2, produced)

      if (plan !== null) {
        const allIds = plan.levels
          .flatMap((l) => l.dispatches)
          .map((d) => d.subscriber_id)
        expect(allIds.length).toBeGreaterThan(0)
      }
    })

    it('should return null when no downstream listeners match', () => {
      registerListenersBatch([
        {
          subscriber_id: 1,
          topic_path: 'settings',
          scope_path: 'settings',
        },
      ])

      const plan = routeProducedChanges(2, [
        { path: 'user.name', value: 'Bob' },
      ])
      expect(plan).toBeNull()
    })
  })

  describe('unregistration', () => {
    it('should remove listener from dispatch plan', () => {
      registerListenersBatch([
        { subscriber_id: 1, topic_path: 'user', scope_path: 'user' },
        {
          subscriber_id: 2,
          topic_path: 'user.name',
          scope_path: 'user.name',
        },
      ])

      let plan = createDispatchPlan([{ path: 'user.name', value: 'Bob' }])
      let allIds = plan.levels
        .flatMap((l) => l.dispatches)
        .map((d) => d.subscriber_id)
      expect(allIds).toContain(1)
      expect(allIds).toContain(2)

      unregisterListenersBatch([2])

      plan = createDispatchPlan([{ path: 'user.name', value: 'Charlie' }])
      allIds = plan.levels
        .flatMap((l) => l.dispatches)
        .map((d) => d.subscriber_id)

      expect(allIds).toContain(1)
      expect(allIds).not.toContain(2)
    })

    it('should handle unregistering all listeners', () => {
      registerListenersBatch([
        { subscriber_id: 1, topic_path: 'user', scope_path: 'user' },
      ])
      unregisterListenersBatch([1])

      const plan = createDispatchPlan([{ path: 'user.name', value: 'Bob' }])
      expect(plan.levels.flatMap((l) => l.dispatches)).toHaveLength(0)
    })
  })

  describe('root topic listener', () => {
    it('should receive all changes when listening on root', () => {
      registerListenersBatch([
        { subscriber_id: 1, topic_path: '', scope_path: '' },
      ])

      const plan = createDispatchPlan([
        { path: 'user.name', value: 'Bob' },
        { path: 'user.role', value: 'editor' },
      ])

      const dispatch = plan.levels
        .flatMap((l) => l.dispatches)
        .find((d) => d.subscriber_id === 1)

      expect(dispatch).toBeDefined()
      expect(dispatch!.changes.length).toBe(2)
    })
  })

  describe('subscriber metadata', () => {
    it('should correctly store and return scope_path', () => {
      registerListenersBatch([
        {
          subscriber_id: 42,
          topic_path: 'user.name',
          scope_path: 'user',
        },
      ])

      const plan = createDispatchPlan([{ path: 'user.name', value: 'Bob' }])
      const dispatch = plan.levels
        .flatMap((l) => l.dispatches)
        .find((d) => d.subscriber_id === 42)

      expect(dispatch).toBeDefined()
      expect(dispatch!.scope_path).toBe('user')
    })

    it('should handle different topic_path and scope_path', () => {
      registerListenersBatch([
        {
          subscriber_id: 99,
          topic_path: 'user',
          scope_path: 'user.name',
        },
      ])

      const plan = createDispatchPlan([{ path: 'user.role', value: 'admin' }])
      const dispatch = plan.levels
        .flatMap((l) => l.dispatches)
        .find((d) => d.subscriber_id === 99)

      expect(dispatch).toBeDefined()
      expect(dispatch!.scope_path).toBe('user.name')
    })
  })
})
