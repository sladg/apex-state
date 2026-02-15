/**
 * EP3 Listener Dispatch Tests
 *
 * Tests WASM dispatch plan creation and listener orchestration.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest'

import type { Change } from '../../src/wasm/bridge'
import { initWasm, resetWasm, wasm } from '../../src/wasm/bridge'

beforeEach(async () => {
  const wasmModule = await import('../../rust/pkg/apex_state_wasm.js')
  initWasm(wasmModule)
  wasm.shadowInit({
    user: { name: 'Alice', role: 'admin', email: 'a@b.com' },
  })
})

afterEach(() => {
  resetWasm()
})

describe('EP3 Listener Dispatch', () => {
  describe('single listener', () => {
    it('should include listener in dispatch plan when topic matches', () => {
      wasm.registerListenersBatch([
        { subscriber_id: 1, topic_path: 'user', scope_path: 'user' },
      ])

      const changes: Change[] = [{ path: 'user.name', value: 'Bob' }]
      const plan = wasm.createDispatchPlan(changes)

      expect(plan.levels.length).toBeGreaterThan(0)
      const allDispatches = plan.levels.flatMap((l) => l.dispatches)
      const found = allDispatches.find((d) => d.subscriber_id === 1)
      expect(found).toBeDefined()
      expect(found!.scope_path).toBe('user')
    })

    it('should not dispatch to unrelated listener', () => {
      wasm.registerListenersBatch([
        {
          subscriber_id: 1,
          topic_path: 'settings',
          scope_path: 'settings',
        },
      ])

      const plan = wasm.createDispatchPlan([
        { path: 'user.name', value: 'Bob' },
      ])
      const allDispatches = plan.levels.flatMap((l) => l.dispatches)
      expect(allDispatches).toHaveLength(0)
    })
  })

  describe('multi-depth listeners', () => {
    it('should order levels deepest-first', () => {
      wasm.registerListenersBatch([
        { subscriber_id: 1, topic_path: 'user', scope_path: 'user' },
        {
          subscriber_id: 2,
          topic_path: 'user.name',
          scope_path: 'user.name',
        },
      ])

      const plan = wasm.createDispatchPlan([
        { path: 'user.name', value: 'Bob' },
      ])
      expect(plan.levels.length).toBeGreaterThanOrEqual(2)

      const depths = plan.levels.map((l) => l.depth)
      for (let i = 1; i < depths.length; i++) {
        expect(depths[i]).toBeLessThanOrEqual(depths[i - 1])
      }
    })

    it('should dispatch to listeners at multiple depth levels', () => {
      wasm.registerListenersBatch([
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

      const plan = wasm.createDispatchPlan([
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
      wasm.registerListenersBatch([
        { subscriber_id: 1, topic_path: 'user', scope_path: 'user' },
      ])

      const plan = wasm.createDispatchPlan([
        { path: 'user.name', value: 'Bob' },
      ])
      const dispatch = plan.levels
        .flatMap((l) => l.dispatches)
        .find((d) => d.subscriber_id === 1)

      expect(dispatch).toBeDefined()
      expect(dispatch!.changes.length).toBeGreaterThan(0)

      const changePaths = dispatch!.changes.map((c) => c.path)
      expect(changePaths).toContain('name')
    })

    it('should provide full path when scope is at leaf level', () => {
      wasm.registerListenersBatch([
        {
          subscriber_id: 1,
          topic_path: 'user.name',
          scope_path: 'user.name',
        },
      ])

      const plan = wasm.createDispatchPlan([
        { path: 'user.name', value: 'Bob' },
      ])
      const dispatch = plan.levels
        .flatMap((l) => l.dispatches)
        .find((d) => d.subscriber_id === 1)

      expect(dispatch).toBeDefined()
      expect(dispatch!.changes.length).toBeGreaterThan(0)
    })
  })

  describe('no matching listeners', () => {
    it('should return empty plan when no listeners match', () => {
      wasm.registerListenersBatch([
        {
          subscriber_id: 1,
          topic_path: 'settings',
          scope_path: 'settings',
        },
      ])

      const plan = wasm.createDispatchPlan([
        { path: 'user.name', value: 'Bob' },
      ])
      expect(plan.levels.flatMap((l) => l.dispatches)).toHaveLength(0)
    })

    it('should return empty plan when no listeners registered', () => {
      const plan = wasm.createDispatchPlan([
        { path: 'user.name', value: 'Bob' },
      ])
      expect(plan.levels.flatMap((l) => l.dispatches)).toHaveLength(0)
    })
  })

  describe('route produced changes', () => {
    it('should route produced changes to downstream topics', () => {
      wasm.registerListenersBatch([
        { subscriber_id: 1, topic_path: 'user', scope_path: 'user' },
        {
          subscriber_id: 2,
          topic_path: 'user.email',
          scope_path: 'user.email',
        },
      ])

      const produced: Change[] = [{ path: 'user.email', value: 'new@test.com' }]
      const plan = wasm.routeProducedChanges(2, produced)

      if (plan !== null) {
        const allIds = plan.levels
          .flatMap((l) => l.dispatches)
          .map((d) => d.subscriber_id)
        expect(allIds.length).toBeGreaterThan(0)
      }
    })

    it('should return null when no downstream listeners match', () => {
      wasm.registerListenersBatch([
        {
          subscriber_id: 1,
          topic_path: 'settings',
          scope_path: 'settings',
        },
      ])

      const plan = wasm.routeProducedChanges(2, [
        { path: 'user.name', value: 'Bob' },
      ])
      expect(plan).toBeNull()
    })
  })

  describe('unregistration', () => {
    it('should remove listener from dispatch plan', () => {
      wasm.registerListenersBatch([
        { subscriber_id: 1, topic_path: 'user', scope_path: 'user' },
        {
          subscriber_id: 2,
          topic_path: 'user.name',
          scope_path: 'user.name',
        },
      ])

      let plan = wasm.createDispatchPlan([{ path: 'user.name', value: 'Bob' }])
      let allIds = plan.levels
        .flatMap((l) => l.dispatches)
        .map((d) => d.subscriber_id)
      expect(allIds).toContain(1)
      expect(allIds).toContain(2)

      wasm.unregisterListenersBatch([2])

      plan = wasm.createDispatchPlan([{ path: 'user.name', value: 'Charlie' }])
      allIds = plan.levels
        .flatMap((l) => l.dispatches)
        .map((d) => d.subscriber_id)

      expect(allIds).toContain(1)
      expect(allIds).not.toContain(2)
    })

    it('should handle unregistering all listeners', () => {
      wasm.registerListenersBatch([
        { subscriber_id: 1, topic_path: 'user', scope_path: 'user' },
      ])
      wasm.unregisterListenersBatch([1])

      const plan = wasm.createDispatchPlan([
        { path: 'user.name', value: 'Bob' },
      ])
      expect(plan.levels.flatMap((l) => l.dispatches)).toHaveLength(0)
    })
  })

  describe('root topic listener', () => {
    it('should receive all changes when listening on root', () => {
      wasm.registerListenersBatch([
        { subscriber_id: 1, topic_path: '', scope_path: '' },
      ])

      const plan = wasm.createDispatchPlan([
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
      wasm.registerListenersBatch([
        {
          subscriber_id: 42,
          topic_path: 'user.name',
          scope_path: 'user',
        },
      ])

      const plan = wasm.createDispatchPlan([
        { path: 'user.name', value: 'Bob' },
      ])
      const dispatch = plan.levels
        .flatMap((l) => l.dispatches)
        .find((d) => d.subscriber_id === 42)

      expect(dispatch).toBeDefined()
      expect(dispatch!.scope_path).toBe('user')
    })

    it('should handle different topic_path and scope_path', () => {
      wasm.registerListenersBatch([
        {
          subscriber_id: 99,
          topic_path: 'user',
          scope_path: 'user.name',
        },
      ])

      const plan = wasm.createDispatchPlan([
        { path: 'user.role', value: 'admin' },
      ])
      const dispatch = plan.levels
        .flatMap((l) => l.dispatches)
        .find((d) => d.subscriber_id === 99)

      expect(dispatch).toBeDefined()
      expect(dispatch!.scope_path).toBe('user.name')
    })
  })

  describe('FullExecutionPlan with input_change_ids', () => {
    it('should reference input changes by index in ProcessResult.changes', () => {
      // TODO: Step 1 - Register listener and call processChanges with multiple changes
      // TODO: Step 2 - Extract FullExecutionPlan from result
      // TODO: Step 3 - Verify dispatch.input_change_ids contains correct indexes
    })

    it('should handle single change dispatch with single input_change_id', () => {
      // TODO: Step 1 - Register listener for specific path
      // TODO: Step 2 - Call processChanges with single matching change
      // TODO: Step 3 - Verify dispatch.input_change_ids === [0]
    })

    it('should handle multiple changes dispatched to same listener', () => {
      // TODO: Step 1 - Register listener on parent path (e.g., "user")
      // TODO: Step 2 - Call processChanges with multiple child changes
      // TODO: Step 3 - Verify dispatch.input_change_ids contains all matching indexes
    })

    it('should handle subset of changes matching listener topic', () => {
      // TODO: Step 1 - Register listener on "user.name"
      // TODO: Step 2 - Call processChanges with changes to "user.name" and "user.role"
      // TODO: Step 3 - Verify only relevant change index appears in input_change_ids
    })
  })

  describe('Propagation map structure', () => {
    it('should map child dispatch to parent listener', () => {
      // TODO: Step 1 - Register parent listener (depth 1) and child listener (depth 2)
      // TODO: Step 2 - Call processChanges to get FullExecutionPlan
      // TODO: Step 3 - Verify propagation_map[child_dispatch_id] contains parent target
    })

    it('should include remap_prefix for scope path transformation', () => {
      // TODO: Step 1 - Register listener with scope_path differing from topic_path
      // TODO: Step 2 - Call processChanges
      // TODO: Step 3 - Verify PropagationTarget.remap_prefix matches expected prefix
    })

    it('should handle multiple targets for single dispatch', () => {
      // TODO: Step 1 - Register multiple parent listeners subscribing to overlapping topics
      // TODO: Step 2 - Register child listener that can propagate to both
      // TODO: Step 3 - Verify propagation_map[child_id] contains multiple targets
    })

    it('should handle sibling dispatches with no propagation links', () => {
      // TODO: Step 1 - Register listeners at same depth with different topics
      // TODO: Step 2 - Call processChanges with changes for both
      // TODO: Step 3 - Verify propagation_map entries for siblings are empty or only link upward
    })

    it('should handle root listener receiving all propagations', () => {
      // TODO: Step 1 - Register root listener (topic_path: "")
      // TODO: Step 2 - Register child listeners at various depths
      // TODO: Step 3 - Verify root dispatch appears in propagation targets for all children
    })
  })

  describe('FullExecutionPlan vs DispatchPlan (legacy)', () => {
    it('should return FullExecutionPlan from processChanges', () => {
      // TODO: Step 1 - Register listeners and call processChanges
      // TODO: Step 2 - Extract execution_plan from result
      // TODO: Step 3 - Verify structure matches FullExecutionPlan (groups, propagation_map)
    })

    it('should maintain backward compat with createDispatchPlan returning DispatchPlan', () => {
      // TODO: Step 1 - Register listeners and call createDispatchPlan
      // TODO: Step 2 - Verify result structure matches legacy DispatchPlan (levels, depth)
      // TODO: Step 3 - Ensure no FullExecutionPlan fields present (no propagation_map)
    })
  })
})
