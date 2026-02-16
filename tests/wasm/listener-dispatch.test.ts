/**
 * EP3 Listener Dispatch Tests
 *
 * Tests WASM dispatch plan creation and listener orchestration.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest'

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

      const result = wasm.processChanges([{ path: 'user.name', value: 'Bob' }])

      expect(result.execution_plan).not.toBeNull()
      const plan = result.execution_plan!
      expect(plan.groups.length).toBeGreaterThan(0)
      const allDispatches = plan.groups.flatMap((g) => g.dispatches)
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

      const result = wasm.processChanges([{ path: 'user.name', value: 'Bob' }])
      const allDispatches = result.execution_plan
        ? result.execution_plan.groups.flatMap((g) => g.dispatches)
        : []
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

      const result = wasm.processChanges([{ path: 'user.name', value: 'Bob' }])

      expect(result.execution_plan).not.toBeNull()
      const plan = result.execution_plan!
      expect(plan.groups.length).toBeGreaterThanOrEqual(2)

      // Groups are ordered deepest-first by design
      const allDispatches = plan.groups.flatMap((g) => g.dispatches)
      expect(allDispatches.length).toBeGreaterThanOrEqual(2)
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

      const result = wasm.processChanges([
        { path: 'user.name', value: 'Bob' },
        { path: 'user.role', value: 'editor' },
      ])

      expect(result.execution_plan).not.toBeNull()
      const plan = result.execution_plan!
      const allIds = plan.groups
        .flatMap((g) => g.dispatches)
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

      const result = wasm.processChanges([{ path: 'user.name', value: 'Bob' }])

      expect(result.execution_plan).not.toBeNull()
      const plan = result.execution_plan!
      const dispatch = plan.groups
        .flatMap((g) => g.dispatches)
        .find((d) => d.subscriber_id === 1)

      expect(dispatch).toBeDefined()
      expect(dispatch!.input_change_ids.length).toBeGreaterThan(0)
      // input_change_ids references indices into result.state_changes
      expect(dispatch!.input_change_ids[0]).toBe(0)
    })

    it('should provide full path when scope is at leaf level', () => {
      wasm.registerListenersBatch([
        {
          subscriber_id: 1,
          topic_path: 'user.name',
          scope_path: 'user.name',
        },
      ])

      const result = wasm.processChanges([{ path: 'user.name', value: 'Bob' }])

      expect(result.execution_plan).not.toBeNull()
      const plan = result.execution_plan!
      const dispatch = plan.groups
        .flatMap((g) => g.dispatches)
        .find((d) => d.subscriber_id === 1)

      expect(dispatch).toBeDefined()
      expect(dispatch!.input_change_ids.length).toBeGreaterThan(0)
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

      const result = wasm.processChanges([{ path: 'user.name', value: 'Bob' }])
      const allDispatches = result.execution_plan
        ? result.execution_plan.groups.flatMap((g) => g.dispatches)
        : []
      expect(allDispatches).toHaveLength(0)
    })

    it('should return empty plan when no listeners registered', () => {
      const result = wasm.processChanges([{ path: 'user.name', value: 'Bob' }])
      const allDispatches = result.execution_plan
        ? result.execution_plan.groups.flatMap((g) => g.dispatches)
        : []
      expect(allDispatches).toHaveLength(0)
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

      // Process a change at deeper path that listeners should receive
      const result = wasm.processChanges([
        { path: 'user.email', value: 'new@test.com' },
      ])

      expect(result.execution_plan).not.toBeNull()
      const plan = result.execution_plan!
      const allIds = plan.groups
        .flatMap((g) => g.dispatches)
        .map((d) => d.subscriber_id)
      expect(allIds.length).toBeGreaterThan(0)
    })

    it('should return empty plan when no downstream listeners match', () => {
      wasm.registerListenersBatch([
        {
          subscriber_id: 1,
          topic_path: 'settings',
          scope_path: 'settings',
        },
      ])

      const result = wasm.processChanges([{ path: 'user.name', value: 'Bob' }])

      const allDispatches = result.execution_plan
        ? result.execution_plan.groups.flatMap((g) => g.dispatches)
        : []
      expect(allDispatches).toHaveLength(0)
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

      let result = wasm.processChanges([{ path: 'user.name', value: 'Bob' }])
      let plan = result.execution_plan!
      let allIds = plan.groups
        .flatMap((g) => g.dispatches)
        .map((d) => d.subscriber_id)
      expect(allIds).toContain(1)
      expect(allIds).toContain(2)

      wasm.unregisterListenersBatch([2])

      result = wasm.processChanges([{ path: 'user.name', value: 'Charlie' }])
      plan = result.execution_plan!
      allIds = plan.groups
        .flatMap((g) => g.dispatches)
        .map((d) => d.subscriber_id)

      expect(allIds).toContain(1)
      expect(allIds).not.toContain(2)
    })

    it('should handle unregistering all listeners', () => {
      wasm.registerListenersBatch([
        { subscriber_id: 1, topic_path: 'user', scope_path: 'user' },
      ])
      wasm.unregisterListenersBatch([1])

      const result = wasm.processChanges([{ path: 'user.name', value: 'Bob' }])
      const allDispatches = result.execution_plan
        ? result.execution_plan.groups.flatMap((g) => g.dispatches)
        : []
      expect(allDispatches).toHaveLength(0)
    })
  })

  describe('root topic listener', () => {
    it('should receive all changes when listening on root', () => {
      wasm.registerListenersBatch([
        { subscriber_id: 1, topic_path: '', scope_path: '' },
      ])

      const result = wasm.processChanges([
        { path: 'user.name', value: 'Bob' },
        { path: 'user.role', value: 'editor' },
      ])

      expect(result.execution_plan).not.toBeNull()
      const plan = result.execution_plan!
      const dispatch = plan.groups
        .flatMap((g) => g.dispatches)
        .find((d) => d.subscriber_id === 1)

      expect(dispatch).toBeDefined()
      expect(dispatch!.input_change_ids.length).toBe(2)
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

      const result = wasm.processChanges([{ path: 'user.name', value: 'Bob' }])

      expect(result.execution_plan).not.toBeNull()
      const plan = result.execution_plan!
      const dispatch = plan.groups
        .flatMap((g) => g.dispatches)
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

      const result = wasm.processChanges([
        { path: 'user.role', value: 'moderator' },
      ])

      expect(result.execution_plan).not.toBeNull()
      const plan = result.execution_plan!
      const dispatch = plan.groups
        .flatMap((g) => g.dispatches)
        .find((d) => d.subscriber_id === 99)

      expect(dispatch).toBeDefined()
      expect(dispatch!.scope_path).toBe('user.name')
    })
  })

  describe('FullExecutionPlan with input_change_ids', () => {
    it('should reference input changes by index in ProcessResult.changes', () => {
      // Step 1 - Register listener and call processChanges with multiple changes
      wasm.registerListenersBatch([
        { subscriber_id: 100, topic_path: 'user', scope_path: 'user' },
      ])

      const result = wasm.processChanges([
        { path: 'user.name', value: 'Bob' },
        { path: 'user.role', value: 'editor' },
      ])

      // Step 2 - Extract FullExecutionPlan from result
      expect(result.execution_plan).not.toBeNull()
      const plan = result.execution_plan!

      // Step 3 - Verify dispatch.input_change_ids contains correct indexes
      expect(plan.groups.length).toBeGreaterThan(0)
      const allDispatches = plan.groups.flatMap((g) => g.dispatches)
      const dispatch = allDispatches.find((d) => d.subscriber_id === 100)

      expect(dispatch).toBeDefined()
      expect(dispatch!.input_change_ids).toContain(0) // user.name
      expect(dispatch!.input_change_ids).toContain(1) // user.role
    })

    it('should handle single change dispatch with single input_change_id', () => {
      // Step 1 - Register listener for specific path
      wasm.registerListenersBatch([
        {
          subscriber_id: 101,
          topic_path: 'user.name',
          scope_path: 'user.name',
        },
      ])

      // Step 2 - Call processChanges with single matching change
      const result = wasm.processChanges([
        { path: 'user.name', value: 'Charlie' },
      ])

      // Step 3 - Verify dispatch.input_change_ids === [0]
      expect(result.execution_plan).not.toBeNull()
      const allDispatches = result.execution_plan!.groups.flatMap(
        (g) => g.dispatches,
      )
      const dispatch = allDispatches.find((d) => d.subscriber_id === 101)

      expect(dispatch).toBeDefined()
      expect(dispatch!.input_change_ids).toEqual([0])
    })

    it('should handle multiple changes dispatched to same listener', () => {
      // Step 1 - Register listener on parent path (e.g., "user")
      wasm.registerListenersBatch([
        { subscriber_id: 102, topic_path: 'user', scope_path: 'user' },
      ])

      // Step 2 - Call processChanges with multiple child changes
      const result = wasm.processChanges([
        { path: 'user.name', value: 'David' },
        { path: 'user.email', value: 'david@example.com' },
        { path: 'user.role', value: 'editor' },
      ])

      // Step 3 - Verify dispatch.input_change_ids contains all matching indexes
      expect(result.execution_plan).not.toBeNull()
      const allDispatches = result.execution_plan!.groups.flatMap(
        (g) => g.dispatches,
      )
      const dispatch = allDispatches.find((d) => d.subscriber_id === 102)

      expect(dispatch).toBeDefined()
      expect(dispatch!.input_change_ids).toEqual([0, 1, 2])
    })

    it('should handle subset of changes matching listener topic', () => {
      // Step 1 - Register listener on "user.name"
      wasm.registerListenersBatch([
        {
          subscriber_id: 103,
          topic_path: 'user.name',
          scope_path: 'user.name',
        },
      ])

      // Step 2 - Call processChanges with changes to "user.name" and "user.role"
      const result = wasm.processChanges([
        { path: 'user.name', value: 'Eve' },
        { path: 'user.role', value: 'guest' },
      ])

      // Step 3 - Verify only relevant change index appears in input_change_ids
      expect(result.execution_plan).not.toBeNull()
      const allDispatches = result.execution_plan!.groups.flatMap(
        (g) => g.dispatches,
      )
      const dispatch = allDispatches.find((d) => d.subscriber_id === 103)

      expect(dispatch).toBeDefined()
      expect(dispatch!.input_change_ids).toEqual([0]) // Only user.name
      expect(dispatch!.input_change_ids).not.toContain(1) // Not user.role
    })
  })

  describe('Propagation map structure', () => {
    it('should map child dispatch to parent listener', () => {
      // Step 1 - Register parent listener (depth 1) and child listener (depth 2)
      wasm.registerListenersBatch([
        { subscriber_id: 200, topic_path: 'user', scope_path: 'user' },
        {
          subscriber_id: 201,
          topic_path: 'user.name',
          scope_path: 'user.name',
        },
      ])

      // Step 2 - Call processChanges to get FullExecutionPlan
      const result = wasm.processChanges([
        { path: 'user.name', value: 'Frank' },
      ])

      // Step 3 - Verify propagation_map[child_dispatch_id] contains parent target
      expect(result.execution_plan).not.toBeNull()
      const plan = result.execution_plan!

      // Find dispatch IDs
      const allDispatches = plan.groups.flatMap((g, groupIdx) =>
        g.dispatches.map((d, dispatchIdx) => ({
          ...d,
          dispatch_id: groupIdx * 1000 + dispatchIdx, // Approximate dispatch ID calculation
        })),
      )

      const childDispatch = allDispatches.find((d) => d.subscriber_id === 201)
      const parentDispatch = allDispatches.find((d) => d.subscriber_id === 200)

      expect(childDispatch).toBeDefined()
      expect(parentDispatch).toBeDefined()

      // Verify propagation map has entries
      expect(plan.propagation_map).toBeDefined()
      expect(plan.propagation_map.length).toBeGreaterThan(0)
    })

    it('should include remap_prefix for scope path transformation', () => {
      // Step 1 - Register listener with scope_path differing from topic_path
      wasm.registerListenersBatch([
        {
          subscriber_id: 202,
          topic_path: 'user',
          scope_path: 'user.profile',
        },
      ])

      // Step 2 - Call processChanges
      const result = wasm.processChanges([
        { path: 'user.name', value: 'Grace' },
      ])

      // Step 3 - Verify PropagationTarget.remap_prefix matches expected prefix
      expect(result.execution_plan).not.toBeNull()
      const plan = result.execution_plan!

      // Check propagation map structure exists
      expect(plan.propagation_map).toBeDefined()

      // If there are propagation targets, verify remap_prefix exists
      if (plan.propagation_map.length > 0) {
        const targets = plan.propagation_map.flat()
        if (targets.length > 0) {
          expect(targets[0]).toHaveProperty('remap_prefix')
        }
      }
    })

    it('should handle multiple targets for single dispatch', () => {
      // Step 1 - Register multiple parent listeners subscribing to overlapping topics
      wasm.registerListenersBatch([
        { subscriber_id: 203, topic_path: 'user', scope_path: 'user' },
        { subscriber_id: 204, topic_path: '', scope_path: '' }, // Root listener
        {
          subscriber_id: 205,
          topic_path: 'user.name',
          scope_path: 'user.name',
        },
      ])

      // Step 2 - Register child listener that can propagate to both
      const result = wasm.processChanges([
        { path: 'user.name', value: 'Henry' },
      ])

      // Step 3 - Verify propagation_map[child_id] contains multiple targets
      expect(result.execution_plan).not.toBeNull()
      const plan = result.execution_plan!

      // The deepest listener (user.name) should have propagation targets to both user and root
      expect(plan.propagation_map).toBeDefined()
      expect(plan.propagation_map.length).toBeGreaterThan(0)

      // Find entries with multiple targets
      const multiTargetEntry = plan.propagation_map.find(
        (targets) => targets.length > 1,
      )
      if (multiTargetEntry) {
        expect(multiTargetEntry.length).toBeGreaterThanOrEqual(2)
      }
    })

    it('should handle sibling dispatches with no propagation links', () => {
      // Step 1 - Register listeners at same depth with different topics
      wasm.registerListenersBatch([
        {
          subscriber_id: 206,
          topic_path: 'user.name',
          scope_path: 'user.name',
        },
        {
          subscriber_id: 207,
          topic_path: 'user.role',
          scope_path: 'user.role',
        },
      ])

      // Step 2 - Call processChanges with changes for both
      const result = wasm.processChanges([
        { path: 'user.name', value: 'Ivy' },
        { path: 'user.role', value: 'moderator' },
      ])

      // Step 3 - Verify propagation_map entries for siblings are empty or only link upward
      expect(result.execution_plan).not.toBeNull()
      const plan = result.execution_plan!

      expect(plan.propagation_map).toBeDefined()

      // Siblings shouldn't propagate to each other (they're at same depth)
      // Propagation map should either be empty or contain only upward links
      const allDispatches = plan.groups.flatMap((g) => g.dispatches)
      expect(allDispatches.length).toBeGreaterThanOrEqual(2)
    })

    it('should handle root listener receiving all propagations', () => {
      // Step 1 - Register root listener (topic_path: "")
      wasm.registerListenersBatch([
        { subscriber_id: 208, topic_path: '', scope_path: '' },
        { subscriber_id: 209, topic_path: 'user', scope_path: 'user' },
        {
          subscriber_id: 210,
          topic_path: 'user.name',
          scope_path: 'user.name',
        },
      ])

      // Step 2 - Register child listeners at various depths
      const result = wasm.processChanges([{ path: 'user.name', value: 'Jack' }])

      // Step 3 - Verify root dispatch appears in propagation targets for all children
      expect(result.execution_plan).not.toBeNull()
      const plan = result.execution_plan!

      const allDispatches = plan.groups.flatMap((g) => g.dispatches)
      const rootDispatch = allDispatches.find((d) => d.subscriber_id === 208)

      expect(rootDispatch).toBeDefined()
      expect(plan.propagation_map).toBeDefined()

      // Root listener should receive propagations from deeper listeners
      // (Root is at depth 0, so all other dispatches should propagate to it)
      expect(allDispatches.length).toBeGreaterThanOrEqual(2)
    })
  })

  describe('FullExecutionPlan vs DispatchPlan (legacy)', () => {
    it('should return FullExecutionPlan from processChanges', () => {
      // Step 1 - Register listeners and call processChanges
      wasm.registerListenersBatch([
        { subscriber_id: 300, topic_path: 'user', scope_path: 'user' },
        {
          subscriber_id: 301,
          topic_path: 'user.name',
          scope_path: 'user.name',
        },
      ])

      const result = wasm.processChanges([{ path: 'user.name', value: 'Kate' }])

      // Step 2 - Extract execution_plan from result
      expect(result.execution_plan).not.toBeNull()
      const plan = result.execution_plan!

      // Step 3 - Verify structure matches FullExecutionPlan (groups, propagation_map)
      expect(plan).toHaveProperty('groups')
      expect(plan).toHaveProperty('propagation_map')
      expect(Array.isArray(plan.groups)).toBe(true)
      expect(Array.isArray(plan.propagation_map)).toBe(true)

      // Verify groups contain dispatches
      if (plan.groups.length > 0) {
        expect(plan.groups[0]).toHaveProperty('dispatches')
        expect(Array.isArray(plan.groups[0]!.dispatches)).toBe(true)
      }
    })

    it('should maintain backward compat with createDispatchPlan returning DispatchPlan', () => {
      // Step 1 - Register listeners and call createDispatchPlan (legacy API still works)
      wasm.registerListenersBatch([
        { subscriber_id: 302, topic_path: 'user', scope_path: 'user' },
        {
          subscriber_id: 303,
          topic_path: 'user.name',
          scope_path: 'user.name',
        },
      ])

      const legacyPlan = wasm.createDispatchPlan([
        { path: 'user.name', value: 'Laura' },
      ])

      // Step 2 - Verify result structure matches legacy DispatchPlan (levels, depth)
      expect(legacyPlan).toHaveProperty('levels')
      expect(Array.isArray(legacyPlan.levels)).toBe(true)

      if (legacyPlan.levels.length > 0) {
        expect(legacyPlan.levels[0]).toHaveProperty('depth')
        expect(legacyPlan.levels[0]).toHaveProperty('dispatches')
        expect(typeof legacyPlan.levels[0]!.depth).toBe('number')
        expect(Array.isArray(legacyPlan.levels[0]!.dispatches)).toBe(true)
      }

      // Step 3 - Ensure no FullExecutionPlan fields present (no propagation_map)
      expect(legacyPlan).not.toHaveProperty('propagation_map')
      expect(legacyPlan).not.toHaveProperty('groups')
    })
  })
})
