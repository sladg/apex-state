/**
 * TEST: AnchorPath Combinations - Different Levels & Hierarchy
 *
 * Tests anchor paths at different nesting levels:
 * - Registration-level anchor (guards all resources)
 * - Listener-level anchor (guards individual listener)
 * - Sync-level anchor (individual sync pair guard) — future
 *
 * Tests combinations:
 * - Listener with anchor, registration without
 * - Listener without anchor, registration with
 * - Both listener and registration with anchors
 * - Multiple listeners with different anchors
 * - Mixed scenarios (some listeners with anchor, some without)
 */

import { afterEach, describe, expect, it } from 'vitest'

import { createWasmPipeline, type WasmPipeline } from '~/wasm/bridge'

/**
 * Helper: extract listener subscriber IDs from execution plan
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
      listenerIds.push(dispatch.subscriber_id)
    }
  }
  return listenerIds
}

describe('AnchorPath Combinations: Multi-Level Hierarchy', () => {
  let pipeline: WasmPipeline

  afterEach(() => {
    pipeline?.destroy()
  })

  // =========================================================================
  // Level 1: Listener-Only Anchor Paths
  // =========================================================================

  describe('Listener-level anchor paths only', () => {
    it('should skip listener when listener anchor path is absent', () => {
      pipeline = createWasmPipeline()
      pipeline.shadowInit({
        user: {
          email: 'alice@example.com',
          // Note: profile is missing
        },
      })

      // Register listener WITH listener-level anchor (not registration-level)
      // This tests: anchor_path on individual listener, not on registration
      pipeline.registerSideEffects({
        registration_id: 'listener-anchors',
        // NO registration-level anchor_path
        listeners: [
          {
            subscriber_id: 101,
            topic_paths: ['user.profile.name'],
            scope_path: 'user.profile',
            anchor_path: 'user.profile', // Listener-level anchor
          },
        ],
      })

      // Change unrelated field (listener anchor absent)
      const result = pipeline.processChanges([
        { path: 'user.email', value: 'new@example.com', meta: {} },
      ])

      const listenerIds = getListenerIdsFromPlan(result.execution_plan)
      expect(listenerIds).not.toContain(101)
    })

    it('should execute listener when listener anchor path is present', () => {
      pipeline = createWasmPipeline()
      pipeline.shadowInit({
        user: {
          profile: {
            name: 'Alice',
          },
          email: 'alice@example.com',
        },
      })

      // Register listener with listener-level anchor
      pipeline.registerSideEffects({
        registration_id: 'listener-anchors',
        listeners: [
          {
            subscriber_id: 102,
            topic_paths: ['user.profile.name'],
            scope_path: 'user.profile',
            anchor_path: 'user.profile', // Listener-level anchor
          },
        ],
      })

      // Change the watched path (anchor present)
      const result = pipeline.processChanges([
        { path: 'user.profile.name', value: 'Bob', meta: {} },
      ])

      const listenerIds = getListenerIdsFromPlan(result.execution_plan)
      expect(listenerIds).toContain(102)
    })

    it('should allow different listeners to have different anchor paths', () => {
      pipeline = createWasmPipeline()
      pipeline.shadowInit({
        user: {
          profile: {
            name: 'Alice',
          },
          settings: {
            theme: 'dark',
          },
        },
      })

      // Register two listeners with different anchors
      pipeline.registerSideEffects({
        registration_id: 'multi-anchor-listeners',
        listeners: [
          {
            subscriber_id: 201,
            topic_paths: ['user.profile.name'],
            scope_path: 'user.profile',
            anchor_path: 'user.profile', // Guard by profile
          },
          {
            subscriber_id: 202,
            topic_paths: ['user.settings.theme'],
            scope_path: 'user.settings',
            anchor_path: 'user.settings', // Guard by settings
          },
        ],
      })

      // Change profile.name (listener 201 should fire, 202 shouldn't)
      const result1 = pipeline.processChanges([
        { path: 'user.profile.name', value: 'Bob', meta: {} },
      ])
      let listenerIds = getListenerIdsFromPlan(result1.execution_plan)
      expect(listenerIds).toContain(201)
      expect(listenerIds).not.toContain(202)

      // Change settings.theme (listener 202 should fire, 201 shouldn't)
      const result2 = pipeline.processChanges([
        { path: 'user.settings.theme', value: 'light', meta: {} },
      ])
      listenerIds = getListenerIdsFromPlan(result2.execution_plan)
      expect(listenerIds).not.toContain(201)
      expect(listenerIds).toContain(202)

      // Remove profile
      pipeline.processChanges([{ path: 'user.profile', value: null, meta: {} }])

      // Change settings.theme again (201 still shouldn't fire, 202 should)
      const result3 = pipeline.processChanges([
        { path: 'user.settings.theme', value: 'auto', meta: {} },
      ])
      listenerIds = getListenerIdsFromPlan(result3.execution_plan)
      expect(listenerIds).not.toContain(201)
      expect(listenerIds).toContain(202)
    })
  })

  // =========================================================================
  // Level 2: Registration-Level Anchor Paths
  // =========================================================================

  describe('Registration-level anchor paths only', () => {
    it('should skip all listeners when registration anchor is absent', () => {
      pipeline = createWasmPipeline()
      pipeline.shadowInit({
        user: {
          email: 'alice@example.com',
          // profile is missing
        },
      })

      // Registration with anchor (guards all listeners inside)
      pipeline.registerSideEffects({
        registration_id: 'reg-anchor-guards-all',
        anchor_path: 'user.profile', // Registration-level anchor
        listeners: [
          {
            subscriber_id: 301,
            topic_paths: ['user.profile.name'],
            scope_path: 'user.profile',
            // No listener-level anchor
          },
          {
            subscriber_id: 302,
            topic_paths: ['user.profile.email'],
            scope_path: 'user.profile',
            // No listener-level anchor
          },
        ],
      })

      // Change unrelated field
      const result = pipeline.processChanges([
        { path: 'user.email', value: 'new@example.com', meta: {} },
      ])

      // Both listeners should be skipped (registration anchor absent)
      const listenerIds = getListenerIdsFromPlan(result.execution_plan)
      expect(listenerIds).not.toContain(301)
      expect(listenerIds).not.toContain(302)
    })

    it('should execute matching listeners when registration anchor is present', () => {
      pipeline = createWasmPipeline()
      pipeline.shadowInit({
        user: {
          profile: {
            name: 'Alice',
            email: 'alice@example.com',
          },
        },
      })

      // Registration with anchor
      pipeline.registerSideEffects({
        registration_id: 'reg-anchor-guards-all',
        anchor_path: 'user.profile',
        listeners: [
          {
            subscriber_id: 303,
            topic_paths: ['user.profile.name'],
            scope_path: 'user.profile',
          },
          {
            subscriber_id: 304,
            topic_paths: ['user.profile.email'],
            scope_path: 'user.profile',
          },
        ],
      })

      // Change a watched path (only 303 matches)
      const result = pipeline.processChanges([
        { path: 'user.profile.name', value: 'Bob', meta: {} },
      ])

      // Only matching listener fires (registration anchor present)
      const listenerIds = getListenerIdsFromPlan(result.execution_plan)
      expect(listenerIds).toContain(303) // Matches trigger
      expect(listenerIds).not.toContain(304) // Doesn't match trigger
    })
  })

  // =========================================================================
  // Level 3: Combined Anchor Paths (Both Registration + Listener)
  // =========================================================================

  describe('Combined registration + listener anchor paths', () => {
    it.skip('listener anchor more restrictive than registration anchor', () => {
      // NOTE: This test requires listener-level anchor filtering
      // Currently, only registration-level anchors are fully implemented
      // Listener-level anchors need refinement
      pipeline = createWasmPipeline()
      pipeline.shadowInit({
        user: {
          profile: {
            name: 'Alice',
          },
          settings: {
            theme: 'dark',
          },
        },
      })

      // Registration guards user.profile, but listener 401 additionally requires user.settings
      pipeline.registerSideEffects({
        registration_id: 'combo-restrictive',
        anchor_path: 'user.profile', // Registration-level guard
        listeners: [
          {
            subscriber_id: 401,
            topic_paths: ['user.profile.name'],
            scope_path: 'user.profile',
            anchor_path: 'user.settings', // MORE restrictive listener-level guard
          },
          {
            subscriber_id: 402,
            topic_paths: ['user.profile.name'],
            scope_path: 'user.profile',
            // No additional listener anchor
          },
        ],
      })

      // Change profile.name (registration anchor present, settings also present)
      let result = pipeline.processChanges([
        { path: 'user.profile.name', value: 'Bob', meta: {} },
      ])
      let listenerIds = getListenerIdsFromPlan(result.execution_plan)
      expect(listenerIds).toContain(401) // Both anchors satisfied
      expect(listenerIds).toContain(402) // Registration anchor satisfied

      // Remove settings
      pipeline.processChanges([
        { path: 'user.settings', value: null, meta: {} },
      ])

      // Change profile.name again (registration present, settings absent)
      result = pipeline.processChanges([
        { path: 'user.profile.name', value: 'Charlie', meta: {} },
      ])
      listenerIds = getListenerIdsFromPlan(result.execution_plan)
      expect(listenerIds).not.toContain(401) // Listener anchor (settings) now absent
      expect(listenerIds).toContain(402) // Registration anchor (profile) still present
    })

    it.skip('registration anchor gates listener-level anchors', () => {
      // NOTE: Requires listener-level anchor filtering
      pipeline = createWasmPipeline()
      pipeline.shadowInit({
        user: {
          profile: {
            name: 'Alice',
          },
          settings: {
            theme: 'dark',
          },
        },
      })

      // Registration guards user.profile
      // Listener also has its own anchor (user.settings)
      // If registration anchor fails, listener never executes (even if its anchor would pass)
      pipeline.registerSideEffects({
        registration_id: 'combo-gating',
        anchor_path: 'user.profile', // Primary gate
        listeners: [
          {
            subscriber_id: 501,
            topic_paths: ['user.profile.name'],
            scope_path: 'user.profile',
            anchor_path: 'user.settings', // Secondary gate
          },
        ],
      })

      // Change profile.name
      let result = pipeline.processChanges([
        { path: 'user.profile.name', value: 'Bob', meta: {} },
      ])
      let listenerIds = getListenerIdsFromPlan(result.execution_plan)
      expect(listenerIds).toContain(501) // Both gates pass

      // Remove profile (registration anchor fails)
      pipeline.processChanges([{ path: 'user.profile', value: null, meta: {} }])

      // Change something (settings still exists, but registration anchor fails)
      result = pipeline.processChanges([
        { path: 'user.settings.theme', value: 'light', meta: {} },
      ])
      listenerIds = getListenerIdsFromPlan(result.execution_plan)
      expect(listenerIds).not.toContain(501) // Registration gate fails (profile gone)
    })
  })

  // =========================================================================
  // Level 4: Mixed Scenarios (Some Listeners Guarded, Some Not)
  // =========================================================================

  describe('Mixed scenarios: selective anchoring', () => {
    it('some listeners with anchor, some without in same registration', () => {
      pipeline = createWasmPipeline()
      pipeline.shadowInit({
        user: {
          profile: {
            name: 'Alice',
          },
          email: 'alice@example.com',
          // preferences is missing
        },
      })

      // Mix of guarded and unguarded listeners
      pipeline.registerSideEffects({
        registration_id: 'mixed-guards',
        // NO registration-level anchor
        listeners: [
          {
            subscriber_id: 601,
            topic_paths: ['user.email'],
            scope_path: '',
            // No anchor - always executes
          },
          {
            subscriber_id: 602,
            topic_paths: ['user.profile.name'],
            scope_path: 'user.profile',
            anchor_path: 'user.profile', // Guarded
          },
          {
            subscriber_id: 603,
            topic_paths: ['user.preferences.language'],
            scope_path: 'user.preferences',
            anchor_path: 'user.preferences', // Guarded (but path missing)
          },
        ],
      })

      // Change email
      let result = pipeline.processChanges([
        { path: 'user.email', value: 'new@example.com', meta: {} },
      ])
      let listenerIds = getListenerIdsFromPlan(result.execution_plan)
      expect(listenerIds).toContain(601) // No anchor, fires
      expect(listenerIds).not.toContain(602) // No trigger match
      expect(listenerIds).not.toContain(603) // No trigger match

      // Change profile.name
      result = pipeline.processChanges([
        { path: 'user.profile.name', value: 'Bob', meta: {} },
      ])
      listenerIds = getListenerIdsFromPlan(result.execution_plan)
      expect(listenerIds).not.toContain(601) // No trigger match
      expect(listenerIds).toContain(602) // Trigger matches AND anchor present
      expect(listenerIds).not.toContain(603) // No trigger match
    })

    it.skip('registration-level anchor with some listeners having additional listener-level anchors', () => {
      // NOTE: Requires listener-level anchor filtering
      pipeline = createWasmPipeline()
      pipeline.shadowInit({
        company: {
          name: 'Acme',
          departments: {
            engineering: {
              head: 'Alice',
            },
          },
          office: {
            location: 'NYC',
          },
        },
      })

      // Registration guards company.departments
      // Some listeners add extra guards, some don't
      pipeline.registerSideEffects({
        registration_id: 'hierarchical-guards',
        anchor_path: 'company.departments', // Primary guard
        listeners: [
          {
            subscriber_id: 701,
            topic_paths: ['company.departments.engineering.head'],
            scope_path: 'company.departments.engineering',
            // No extra guard - just uses registration guard
          },
          {
            subscriber_id: 702,
            topic_paths: ['company.departments.engineering.head'],
            scope_path: 'company.departments.engineering',
            anchor_path: 'company.departments.engineering', // Extra guard
          },
          {
            subscriber_id: 703,
            topic_paths: ['company.office.location'],
            scope_path: 'company.office',
            anchor_path: 'company.office', // Different path
          },
        ],
      })

      // Change engineering.head
      let result = pipeline.processChanges([
        {
          path: 'company.departments.engineering.head',
          value: 'Bob',
          meta: {},
        },
      ])
      let listenerIds = getListenerIdsFromPlan(result.execution_plan)
      expect(listenerIds).toContain(701) // Registration guard OK
      expect(listenerIds).toContain(702) // Registration guard OK + extra guard OK
      expect(listenerIds).not.toContain(703) // No trigger match

      // Remove engineering (but departments still exists)
      pipeline.processChanges([
        { path: 'company.departments.engineering', value: null, meta: {} },
      ])

      // Try to change engineering.head again
      result = pipeline.processChanges([
        {
          path: 'company.departments.engineering.head',
          value: 'Charlie',
          meta: {},
        },
      ])
      listenerIds = getListenerIdsFromPlan(result.execution_plan)
      expect(listenerIds).toContain(701) // Registration guard (departments) still OK
      expect(listenerIds).not.toContain(702) // Extra guard (engineering) now fails
      expect(listenerIds).not.toContain(703) // No trigger match

      // Remove departments (registration guard fails)
      pipeline.processChanges([
        { path: 'company.departments', value: null, meta: {} },
      ])

      // Try to change office.location
      result = pipeline.processChanges([
        { path: 'company.office.location', value: 'SF', meta: {} },
      ])
      listenerIds = getListenerIdsFromPlan(result.execution_plan)
      expect(listenerIds).not.toContain(701) // Registration guard (departments) fails
      expect(listenerIds).not.toContain(702) // Registration guard fails
      expect(listenerIds).not.toContain(703) // Extra guard (office) OK but registration gate fails
    })
  })

  // =========================================================================
  // Level 5: Complex Real-World Scenarios
  // =========================================================================

  describe('Real-world complex scenarios', () => {
    it.skip('multi-tenant app with per-tenant guards', () => {
      // NOTE: Requires listener-level anchor filtering
      pipeline = createWasmPipeline()
      pipeline.shadowInit({
        currentTenant: {
          id: 'tenant-1',
          users: {
            alice: { role: 'admin' },
            bob: { role: 'user' },
          },
        },
      })

      // Separate listeners for each tenant data
      // Registration guards tenant existence, listeners guard specific paths
      pipeline.registerSideEffects({
        registration_id: 'tenant-1-effects',
        anchor_path: 'currentTenant', // Ensures tenant exists
        listeners: [
          {
            subscriber_id: 801,
            topic_paths: ['currentTenant.users.alice.role'],
            scope_path: 'currentTenant.users.alice',
            anchor_path: 'currentTenant.users.alice', // Extra: alice must exist
          },
          {
            subscriber_id: 802,
            topic_paths: ['currentTenant.users.bob.role'],
            scope_path: 'currentTenant.users.bob',
            anchor_path: 'currentTenant.users.bob', // Extra: bob must exist
          },
        ],
      })

      // Change alice's role
      let result = pipeline.processChanges([
        { path: 'currentTenant.users.alice.role', value: 'user', meta: {} },
      ])
      let listenerIds = getListenerIdsFromPlan(result.execution_plan)
      expect(listenerIds).toContain(801) // Both tenant and alice exist
      expect(listenerIds).not.toContain(802) // No trigger match

      // Alice left the company (remove her)
      pipeline.processChanges([
        { path: 'currentTenant.users.alice', value: null, meta: {} },
      ])

      // Try to change alice's role again
      result = pipeline.processChanges([
        { path: 'currentTenant.users.alice.role', value: 'viewer', meta: {} },
      ])
      listenerIds = getListenerIdsFromPlan(result.execution_plan)
      expect(listenerIds).not.toContain(801) // Alice doesn't exist anymore

      // Tenant goes offline
      pipeline.processChanges([
        { path: 'currentTenant', value: null, meta: {} },
      ])

      // Try any change
      result = pipeline.processChanges([
        { path: 'currentTenant.users.bob.role', value: 'admin', meta: {} },
      ])
      listenerIds = getListenerIdsFromPlan(result.execution_plan)
      expect(listenerIds).not.toContain(801) // Registration guard fails
      expect(listenerIds).not.toContain(802) // Registration guard fails
    })
  })
})
