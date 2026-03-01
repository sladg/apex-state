/**
 * TEST: AnchorPath Side-Effect (WASM-EP10)
 *
 * Validates the anchorPath feature end-to-end through the WASM pipeline.
 * When an anchorPath is set on a registration, all resources (listeners, BoolLogics,
 * validators) in that registration are silently skipped if the anchor path is
 * structurally absent from shadow state.
 *
 * This provides safe "guard" semantics: a listener for "user.profile.name"
 * with anchor_path="user.profile" will not execute if user.profile doesn't exist,
 * avoiding errors from missing data.
 *
 * Tests cover:
 * - Anchor path present → listeners execute normally
 * - Anchor path absent → listeners are skipped (not in execution plan)
 * - Anchor path removed mid-session → listeners stop executing
 * - Anchor path restored → listeners resume executing
 */

import { afterEach, describe, expect, it } from 'vitest'

import { createWasmPipeline, type WasmPipeline } from '~/wasm/bridge'

/**
 * Helper: extract listener subscriber IDs from execution plan
 * Listeners appear as dispatches in the execution plan groups.
 * Returns subscriber_ids of all listeners that are in the execution plan.
 */
const getListenerIdsFromPlan = (
  plan: {
    groups: {
      dispatches: { dispatch_id: number; subscriber_id: number }[]
    }[]
  } | null,
): number[] => {
  if (!plan) return []
  const listenerIds: number[] = []
  for (const group of plan.groups) {
    for (const dispatch of group.dispatches) {
      // subscriber_id identifies the listener
      listenerIds.push(dispatch.subscriber_id)
    }
  }
  return listenerIds
}

describe('AnchorPath: Resource Guarding', () => {
  let pipeline: WasmPipeline

  afterEach(() => {
    pipeline?.destroy()
  })

  // Setup: user exists with profile
  // Register listener with anchor_path="user.profile"
  // Change: modify user.profile.role
  // Expected: listener in execution plan (should execute)
  it('should execute listeners when anchor path is present', () => {
    pipeline = createWasmPipeline()
    pipeline.shadowInit({
      user: {
        profile: {
          name: 'Alice',
          role: 'guest',
        },
      },
    })

    // Register BoolLogic and listener with anchor path
    pipeline.registerSideEffects({
      registration_id: 'profile-guard',
      anchor_path: 'user.profile',
      listeners: [
        {
          subscriber_id: 1,
          topic_paths: ['user.profile.role'],
          scope_path: 'user.profile',
        },
      ],
    })

    // Change role
    const result = pipeline.processChanges([
      { path: 'user.profile.role', value: 'admin', meta: {} },
    ])

    // Listener should be in execution plan (anchor is present)
    const listenerIds = getListenerIdsFromPlan(result.execution_plan)
    expect(listenerIds).toContain(1)
  })

  // Setup: user exists but profile is missing
  // Register listener with anchor_path="user.profile"
  // Change: modify unrelated field (e.g., user.email)
  // Expected: listener NOT in execution plan (anchor absent, skipped)
  it('should skip listeners when anchor path is absent', () => {
    pipeline = createWasmPipeline()
    pipeline.shadowInit({
      user: {
        email: 'alice@example.com',
        // Note: profile is missing
      },
    })

    // Register listener with anchor path that doesn't exist
    pipeline.registerSideEffects({
      registration_id: 'profile-guard',
      anchor_path: 'user.profile',
      listeners: [
        {
          subscriber_id: 2,
          topic_paths: ['user.profile.role'],
          scope_path: 'user.profile',
        },
      ],
    })

    // Change unrelated field
    const result = pipeline.processChanges([
      { path: 'user.email', value: 'newemail@example.com', meta: {} },
    ])

    // Listener should NOT be in execution plan (anchor absent)
    const listenerIds = getListenerIdsFromPlan(result.execution_plan)
    expect(listenerIds).not.toContain(2)
  })

  // Setup: user.profile exists
  // Register listener with anchor_path="user.profile"
  // Action 1: change role → listener fires
  // Action 2: set user to {} (remove profile)
  // Action 3: change email
  // Expected: listener fires in action 1, but not in action 3 (anchor removed)
  it('should stop executing listeners when anchor path is removed mid-session', () => {
    pipeline = createWasmPipeline()
    pipeline.shadowInit({
      user: {
        profile: {
          name: 'Alice',
          role: 'guest',
        },
        email: 'alice@example.com',
      },
    })

    pipeline.registerSideEffects({
      registration_id: 'profile-guard',
      anchor_path: 'user.profile',
      listeners: [
        {
          subscriber_id: 3,
          topic_paths: ['user.profile.role'],
          scope_path: 'user.profile',
        },
      ],
    })

    // Action 1: Change role (anchor present → listener should be in plan)
    const result1 = pipeline.processChanges([
      { path: 'user.profile.role', value: 'admin', meta: {} },
    ])
    const listenerIds1 = getListenerIdsFromPlan(result1.execution_plan)
    expect(listenerIds1).toContain(3)

    // Action 2: Remove profile by setting user to empty object
    pipeline.processChanges([{ path: 'user', value: {}, meta: {} }])

    // Action 3: Change email (anchor now absent → listener should NOT be in plan)
    const result3 = pipeline.processChanges([
      { path: 'user.email', value: 'newemail@example.com', meta: {} },
    ])
    const listenerIds3 = getListenerIdsFromPlan(result3.execution_plan)
    expect(listenerIds3).not.toContain(3)
  })

  // Setup: user.profile is missing
  // Register listener with anchor_path="user.profile"
  // Action 1: change email (anchor absent → listener skipped)
  // Action 2: set user.profile = {...} (restore anchor)
  // Expected: listener NOT in plan for action 1, but IS in plan for action 2+
  it('should resume executing listeners when anchor path is restored', () => {
    pipeline = createWasmPipeline()
    pipeline.shadowInit({
      user: {
        email: 'alice@example.com',
        // Note: profile is missing initially
      },
    })

    pipeline.registerSideEffects({
      registration_id: 'profile-guard',
      anchor_path: 'user.profile',
      listeners: [
        {
          subscriber_id: 4,
          topic_paths: ['user.profile.role'],
          scope_path: 'user.profile',
        },
      ],
    })

    // Action 1: Change unrelated field (anchor absent)
    const result1 = pipeline.processChanges([
      { path: 'user.email', value: 'newemail@example.com', meta: {} },
    ])
    const listenerIds1 = getListenerIdsFromPlan(result1.execution_plan)
    expect(listenerIds1).not.toContain(4)

    // Action 2: Restore anchor by setting user.profile
    const result2 = pipeline.processChanges([
      {
        path: 'user.profile',
        value: { name: 'Bob', role: 'admin' },
        meta: {},
      },
    ])
    const listenerIds2 = getListenerIdsFromPlan(result2.execution_plan)
    expect(listenerIds2).toContain(4)

    // Action 3: Change role (anchor present, should continue to be in plan)
    const result3 = pipeline.processChanges([
      { path: 'user.profile.role', value: 'guest', meta: {} },
    ])
    const listenerIds3 = getListenerIdsFromPlan(result3.execution_plan)
    expect(listenerIds3).toContain(4)
  })

  // Setup: nested anchor path user.settings.notifications
  // Test that deep anchor paths work correctly
  it('should handle nested anchor paths correctly', () => {
    pipeline = createWasmPipeline()
    pipeline.shadowInit({
      user: {
        settings: {
          notifications: {
            email: true,
            push: false,
          },
          theme: 'dark',
        },
      },
    })

    // Register with nested anchor path
    pipeline.registerSideEffects({
      registration_id: 'notifications-guard',
      anchor_path: 'user.settings.notifications',
      listeners: [
        {
          subscriber_id: 5,
          topic_paths: ['user.settings.notifications.email'],
          scope_path: 'user.settings.notifications',
        },
      ],
    })

    // Action 1: Change notification setting (anchor present → in plan)
    const result1 = pipeline.processChanges([
      { path: 'user.settings.notifications.email', value: false, meta: {} },
    ])
    const listenerIds1 = getListenerIdsFromPlan(result1.execution_plan)
    expect(listenerIds1).toContain(5)

    // Action 2: Remove nested anchor by setting notifications to null
    pipeline.processChanges([
      { path: 'user.settings.notifications', value: null, meta: {} },
    ])

    // Action 3: Change another setting (anchor absent → not in plan)
    const result3 = pipeline.processChanges([
      { path: 'user.settings.theme', value: 'light', meta: {} },
    ])
    const listenerIds3 = getListenerIdsFromPlan(result3.execution_plan)
    expect(listenerIds3).not.toContain(5)
  })

  // Multiple listeners in same registration: all should be guarded by anchor
  it('should guard all listeners in a registration by the same anchor path', () => {
    pipeline = createWasmPipeline()
    pipeline.shadowInit({
      user: {
        profile: {
          name: 'Alice',
          email: 'alice@example.com',
        },
      },
    })

    // Register multiple listeners with shared anchor
    pipeline.registerSideEffects({
      registration_id: 'profile-guard-multi',
      anchor_path: 'user.profile',
      listeners: [
        {
          subscriber_id: 10,
          topic_paths: ['user.profile.name'],
          scope_path: 'user.profile',
        },
        {
          subscriber_id: 11,
          topic_paths: ['user.profile.email'],
          scope_path: 'user.profile',
        },
      ],
    })

    // When anchor is present and name changes, listener 10 fires
    let result = pipeline.processChanges([
      { path: 'user.profile.name', value: 'Bob', meta: {} },
    ])
    let listenerIds = getListenerIdsFromPlan(result.execution_plan)
    expect(listenerIds).toContain(10)

    // When email changes, listener 11 fires
    result = pipeline.processChanges([
      { path: 'user.profile.email', value: 'bob@example.com', meta: {} },
    ])
    listenerIds = getListenerIdsFromPlan(result.execution_plan)
    expect(listenerIds).toContain(11)

    // Remove anchor
    pipeline.processChanges([{ path: 'user.profile', value: null, meta: {} }])

    // When anchor absent, no listeners in plan (even if fields change)
    result = pipeline.processChanges([
      { path: 'user.email', value: 'other@example.com', meta: {} },
    ])
    listenerIds = getListenerIdsFromPlan(result.execution_plan)
    expect(listenerIds).not.toContain(10)
    expect(listenerIds).not.toContain(11)
  })

  // No anchor path set: listeners execute normally regardless of state structure
  it('should execute listeners normally when no anchor path is set', () => {
    pipeline = createWasmPipeline()
    pipeline.shadowInit({
      email: 'alice@example.com',
      // No user.profile structure at all
    })

    // Register listener WITHOUT anchor path
    pipeline.registerSideEffects({
      registration_id: 'no-anchor',
      listeners: [
        {
          subscriber_id: 20,
          topic_paths: ['email'],
          scope_path: '',
        },
      ],
    })

    // Listener should execute regardless of missing data structure
    const result = pipeline.processChanges([
      { path: 'email', value: 'newemail@example.com', meta: {} },
    ])
    const listenerIds = getListenerIdsFromPlan(result.execution_plan)
    expect(listenerIds).toContain(20)
  })
})
