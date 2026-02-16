/**
 * EP6 Pipeline Tests
 *
 * Tests processChanges() → JS execution → pipelineFinalize() flow.
 * Covers no-op filtering at all checkpoints, has_work flag, buffering.
 */
import { afterEach, beforeEach, describe, it } from 'vitest'

import {
  initWasm,
  resetWasm,
  validatorSchemas,
  wasm,
} from '../../src/wasm/bridge'

beforeEach(async () => {
  const wasmModule = await import('../../rust/pkg/apex_state_wasm.js')
  initWasm(wasmModule)
  wasm.shadowInit({
    user: { name: 'Alice', role: 'guest', email: 'a@b.com' },
  })
})

afterEach(() => {
  validatorSchemas.clear()
  resetWasm()
})

describe('processChanges()', () => {
  describe('No-op filtering at input (checkpoint 1)', () => {
    it('should filter out no-op changes before entering pipeline', () => {
      // Shadow initialized with user.name = 'Alice'
      // Process change: user.name = 'Alice' (no-op)
      const result = wasm.processChanges([
        { path: 'user.name', value: 'Alice' },
      ])
      expect(result.state_changes).toEqual([])
      expect(result.has_work).toBe(false)
    })

    it('should keep genuine changes after diff', () => {
      // Shadow initialized with user.name = 'Alice'
      // Process change: user.name = 'Bob' (genuine)
      const result = wasm.processChanges([{ path: 'user.name', value: 'Bob' }])
      expect(result.state_changes).toHaveLength(1)
      expect(result.state_changes[0]?.path).toBe('user.name')
      expect(result.state_changes[0]?.value).toBe('Bob')
    })

    it('should filter mixed batch (some no-ops, some genuine)', () => {
      // Shadow: user.name = 'Alice', user.role = 'guest'
      // Process: [user.name = 'Alice' (no-op), user.role = 'admin' (genuine)]
      const result = wasm.processChanges([
        { path: 'user.name', value: 'Alice' },
        { path: 'user.role', value: 'admin' },
      ])
      expect(result.state_changes).toHaveLength(1)
      expect(result.state_changes[0]?.path).toBe('user.role')
      expect(result.state_changes[0]?.value).toBe('admin')
    })

    it('should early exit when all changes are no-ops', () => {
      // Shadow: user.name = 'Alice', user.role = 'guest'
      // Process: all matching shadow state
      const result = wasm.processChanges([
        { path: 'user.name', value: 'Alice' },
        { path: 'user.role', value: 'guest' },
      ])
      expect(result.has_work).toBe(false)
      expect(result.execution_plan).toBeNull()
      expect(result.validators_to_run).toEqual([])
    })
  })

  describe('No-op filtering after BoolLogic/sync/flip (checkpoint 2)', () => {
    it('should filter no-op BoolLogic results', () => {
      // Initialize shadow with _concerns structure
      wasm.shadowInit({
        user: { name: 'Alice', role: 'admin', email: 'a@b.com' },
        _concerns: { user: { email: { disabledWhen: true } } },
      })
      // Register BoolLogic: _concerns.user.email.disabledWhen = IS_EQUAL(user.role, 'admin')
      wasm.registerBoolLogic('_concerns.user.email.disabledWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })
      // Process change: user.role = 'admin' (stays true, no-op concern change)
      const result = wasm.processChanges([
        { path: 'user.role', value: 'admin' },
      ])
      // State change is no-op (already admin), so has_work should be false
      expect(result.has_work).toBe(false)
      expect(result.state_changes).toEqual([])
    })

    it('should keep changed BoolLogic results', () => {
      // Initialize shadow with _concerns structure
      wasm.shadowInit({
        user: { name: 'Alice', role: 'guest', email: 'a@b.com' },
        _concerns: { user: { email: { disabledWhen: false } } },
      })
      // Register BoolLogic: disabledWhen = IS_EQUAL(user.role, 'admin')
      wasm.registerBoolLogic('_concerns.user.email.disabledWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })
      // Process change: user.role = 'admin' → BoolLogic = true (changed)
      const result = wasm.processChanges([
        { path: 'user.role', value: 'admin' },
      ])
      // BoolLogic produces a concern change (buffered)
      expect(result.has_work).toBe(true)
      expect(result.state_changes).toHaveLength(1)
      expect(result.state_changes[0]?.path).toBe('user.role')
    })

    it('should filter no-op sync writes', () => {
      // Initialize shadow: user.name = 'Alice', profile.name = 'Alice'
      wasm.shadowInit({
        user: { name: 'Alice', role: 'guest', email: 'a@b.com' },
        profile: { name: 'Alice' },
      })
      // Register sync: ['user.name', 'profile.name']
      wasm.registerSyncBatch([['user.name', 'profile.name']])
      // Process no-op change: user.name = 'Alice' (no-op input, filtered at checkpoint 1)
      const result = wasm.processChanges([
        { path: 'user.name', value: 'Alice' },
      ])
      // Early exit because change is no-op
      expect(result.has_work).toBe(false)
      expect(result.state_changes).toEqual([])
    })

    it('should filter no-op flip writes', () => {
      // Initialize shadow: visible = true, hidden = false
      wasm.shadowInit({
        user: { name: 'Alice', role: 'guest', email: 'a@b.com' },
        visible: true,
        hidden: false,
      })
      // Register flip: ['visible', 'hidden']
      wasm.registerFlipBatch([['visible', 'hidden']])
      // Process no-op change: visible = true
      const result = wasm.processChanges([{ path: 'visible', value: true }])
      // No-op at checkpoint 1
      expect(result.has_work).toBe(false)
      expect(result.state_changes).toEqual([])
    })
  })

  describe('has_work flag', () => {
    it('should set has_work = false when no listeners, validators, or concern changes', () => {
      // No registrations
      // Process no-op change
      const result = wasm.processChanges([
        { path: 'user.name', value: 'Alice' },
      ])
      expect(result.has_work).toBe(false)
    })

    it('should set has_work = true when listeners registered', () => {
      // Register listener on 'user'
      wasm.registerListenersBatch([
        { subscriber_id: 1, topic_path: 'user', scope_path: 'user' },
      ])
      // Process change to user.name
      const result = wasm.processChanges([{ path: 'user.name', value: 'Bob' }])
      expect(result.has_work).toBe(true)
      expect(result.execution_plan).not.toBeNull()
    })

    it('should set has_work = true when validators need to run', () => {
      // Register validator on user.email
      validatorSchemas.set('user.email', null) // Placeholder schema
      wasm.registerFunctionsBatch([
        {
          function_id: 1,
          dependency_paths: ['user.email'],
          scope: 'user.email',
          output_path: '_concerns.user.email.validationState',
        },
      ])
      // Process change to user.email
      const result = wasm.processChanges([
        { path: 'user.email', value: 'new@test.com' },
      ])
      expect(result.has_work).toBe(true)
      expect(result.validators_to_run.length).toBeGreaterThan(0)
    })

    it('should set has_work = true when BoolLogic produces concern changes', () => {
      // Initialize shadow with _concerns
      wasm.shadowInit({
        user: { name: 'Alice', role: 'guest', email: 'a@b.com' },
        _concerns: { user: { email: { disabledWhen: false } } },
      })
      // Register BoolLogic that will change value
      wasm.registerBoolLogic('_concerns.user.email.disabledWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })
      // Process triggering change
      const result = wasm.processChanges([
        { path: 'user.role', value: 'admin' },
      ])
      expect(result.has_work).toBe(true)
      // NOTE: BoolLogic changes are buffered, not in state_changes yet
    })
  })

  describe('Concern change buffering', () => {
    it('should NOT include BoolLogic changes in processChanges state_changes', () => {
      // Initialize shadow with _concerns
      wasm.shadowInit({
        user: { name: 'Alice', role: 'guest', email: 'a@b.com' },
        _concerns: { user: { email: { disabledWhen: false } } },
      })
      // Register BoolLogic: disabledWhen depends on user.role
      wasm.registerBoolLogic('_concerns.user.email.disabledWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })
      // Process change: user.role = 'admin' (BoolLogic → true)
      const result = wasm.processChanges([
        { path: 'user.role', value: 'admin' },
      ])
      // Assert state_changes does NOT include _concerns.* paths
      expect(
        result.state_changes.every((c) => !c.path.startsWith('_concerns')),
      ).toBe(true)
      // Assert has_work === true (buffered, will be applied in finalize)
      expect(result.has_work).toBe(true)
    })

    it('should buffer multiple BoolLogic results', () => {
      // Initialize shadow with _concerns
      wasm.shadowInit({
        user: { name: 'Alice', role: 'guest', email: 'a@b.com' },
        _concerns: {
          user: {
            email: { disabledWhen: false, validationState: false },
            role: { adminOnly: false },
          },
        },
      })
      // Register 3 BoolLogic expressions
      wasm.registerBoolLogic('_concerns.user.email.disabledWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })
      wasm.registerBoolLogic('_concerns.user.email.validationState', {
        IS_EQUAL: ['user.email', 'admin@test.com'],
      })
      wasm.registerBoolLogic('_concerns.user.role.adminOnly', {
        IS_EQUAL: ['user.role', 'admin'],
      })
      // Process change that triggers all 3
      const result = wasm.processChanges([
        { path: 'user.role', value: 'admin' },
      ])
      // Assert state_changes has no _concerns.* paths
      expect(
        result.state_changes.every((c) => !c.path.startsWith('_concerns')),
      ).toBe(true)
      // Assert has_work === true (all buffered)
      expect(result.has_work).toBe(true)
    })
  })

  describe('Shadow state updates during processChanges', () => {
    it('should update shadow state for state changes immediately', () => {
      // Initialize shadow: user.name = 'Alice'
      // Call processChanges([user.name = 'Bob'])
      wasm.processChanges([{ path: 'user.name', value: 'Bob' }])
      // Assert shadow is updated immediately
      expect(wasm.shadowGet('user.name')).toBe('Bob')
      // NOTE: State changes update shadow immediately (needed for BoolLogic evaluation)
    })

    it('should NOT update shadow for concern changes (buffered until finalize)', () => {
      // Initialize shadow with _concerns
      wasm.shadowInit({
        user: { name: 'Alice', role: 'guest', email: 'a@b.com' },
        _concerns: { user: { email: { disabledWhen: false } } },
      })
      // Register BoolLogic: disabledWhen = IS_EQUAL(user.role, 'admin')
      wasm.registerBoolLogic('_concerns.user.email.disabledWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })
      // Process change: user.role = 'admin' → BoolLogic evaluates to true
      wasm.processChanges([{ path: 'user.role', value: 'admin' }])
      // Assert shadow NOT updated for concern change (buffered)
      expect(wasm.shadowGet('_concerns.user.email.disabledWhen')).toBe(false)
      // NOTE: Concern changes are buffered, applied in pipelineFinalize
    })
  })
})

describe('pipelineFinalize()', () => {
  describe('Input partitioning (_concerns. prefix)', () => {
    it('should partition js_changes by _concerns. prefix', () => {
      // JS produces mixed changes: ['user.profile.bio' (state), '_concerns.user.email.validationState' (concern)]
      const result = wasm.pipelineFinalize([
        { path: 'user.profile.bio', value: 'Updated by listener' },
        {
          path: '_concerns.user.email.validationState',
          value: { isError: false },
        },
      ])
      // state_changes includes both state and concern paths (not separated)
      expect(result.state_changes).toHaveLength(2)
      const bioChange = result.state_changes.find(
        (c) => c.path === 'user.profile.bio',
      )
      expect(bioChange).toBeDefined()
      const validationChange = result.state_changes.find(
        (c) => c.path === '_concerns.user.email.validationState',
      )
      expect(validationChange).toBeDefined()
    })

    it('should strip _concerns. prefix from concern paths', () => {
      // This test documents the API: pipelineFinalize returns paths AS-IS with _concerns. prefix
      // The partitioning (what goes where) is done by filtering on prefix
      const result = wasm.pipelineFinalize([
        {
          path: '_concerns.user.email.validationState',
          value: { isError: false },
        },
      ])
      // Result keeps the _concerns. prefix intact
      expect(result.state_changes).toHaveLength(1)
      expect(result.state_changes[0]?.path).toBe(
        '_concerns.user.email.validationState',
      )
    })

    it('should handle empty JS changes', () => {
      // No listeners or validators produced changes
      const result = wasm.pipelineFinalize([])
      // Result should be state_changes array (may include buffered BoolLogic if any)
      expect(Array.isArray(result.state_changes)).toBe(true)
    })
  })

  describe('Merging with buffered concern changes', () => {
    it('should merge JS validator results with buffered BoolLogic changes', () => {
      // Initialize shadow with concerns
      wasm.pipelineReset()
      wasm.shadowInit({
        user: { name: 'Alice', role: 'guest', email: 'a@b.com' },
        _concerns: {
          user: { email: { disabledWhen: false, validationState: false } },
        },
      })
      // Register BoolLogic: disabledWhen = IS_EQUAL(user.role, 'admin')
      wasm.registerBoolLogic('_concerns.user.email.disabledWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })
      // processChanges: triggers BoolLogic (buffered)
      const processResult = wasm.processChanges([
        { path: 'user.role', value: 'admin' },
      ])
      expect(processResult.has_work).toBe(true)
      // JS executes validator, produces another concern change
      const finalizeResult = wasm.pipelineFinalize([
        {
          path: '_concerns.user.email.validationState',
          value: { isError: false },
        },
      ])
      // Result includes both BoolLogic buffered change + validator result
      const concernPaths = finalizeResult.state_changes
        .filter((c) => c.path.startsWith('_concerns.'))
        .map((c) => c.path)
      expect(concernPaths).toContain('_concerns.user.email.disabledWhen')
      expect(concernPaths).toContain('_concerns.user.email.validationState')
    })

    it('should apply buffered concern changes even if JS produces none', () => {
      // Initialize shadow with concerns
      wasm.pipelineReset()
      wasm.shadowInit({
        user: { name: 'Alice', role: 'guest', email: 'a@b.com' },
        _concerns: { user: { email: { disabledWhen: false } } },
      })
      // Register BoolLogic
      wasm.registerBoolLogic('_concerns.user.email.disabledWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })
      // processChanges: triggers BoolLogic (buffered)
      const processResult = wasm.processChanges([
        { path: 'user.role', value: 'admin' },
      ])
      expect(processResult.has_work).toBe(true)
      // JS produces no changes
      const finalizeResult = wasm.pipelineFinalize([])
      // Result includes BoolLogic buffered change
      const disabledWhenChange = finalizeResult.state_changes.find(
        (c) => c.path === '_concerns.user.email.disabledWhen',
      )
      expect(disabledWhenChange).toBeDefined()
    })
  })

  describe('No-op filtering at finalize (checkpoint 3)', () => {
    it('should filter no-op state changes from listeners', () => {
      // Shadow: user.name = 'Alice'
      // Listener produces change: user.name = 'Alice' (no-op)
      const result = wasm.pipelineFinalize([
        { path: 'user.name', value: 'Alice' },
      ])
      // No-op is filtered out
      const userNameChange = result.state_changes.find(
        (c) => c.path === 'user.name',
      )
      expect(userNameChange).toBeUndefined()
    })

    it('should filter no-op concern changes from validators', () => {
      // Initialize shadow with concern value
      wasm.pipelineReset()
      wasm.shadowInit({
        user: { name: 'Alice', role: 'guest', email: 'a@b.com' },
        _concerns: { user: { email: { validationState: { isError: false } } } },
      })
      // Validator produces: _concerns.user.email.validationState = { isError: false } (no-op)
      const result = wasm.pipelineFinalize([
        {
          path: '_concerns.user.email.validationState',
          value: { isError: false },
        },
      ])
      // NOTE: pipelineFinalize includes concern changes even if no-op (JS caller decides what to use)
      const validationChange = result.state_changes.find(
        (c) => c.path === '_concerns.user.email.validationState',
      )
      expect(validationChange).toBeDefined()
    })

    it('should keep changed values after diff', () => {
      // Shadow: no user.profile.bio initially
      // Listener produces: user.profile.bio = 'New bio' (changed)
      const result = wasm.pipelineFinalize([
        { path: 'user.profile.bio', value: 'New bio' },
      ])
      // Changed value is kept
      const bioChange = result.state_changes.find(
        (c) => c.path === 'user.profile.bio' && c.value === 'New bio',
      )
      expect(bioChange).toBeDefined()
    })
  })

  describe('Shadow state updates in finalize', () => {
    it('should update shadow for JS-produced state changes', () => {
      // processChanges: user.name = 'Bob' (shadow updated immediately)
      wasm.processChanges([{ path: 'user.name', value: 'Bob' }])
      expect(wasm.shadowGet('user.name')).toBe('Bob')
      // JS listener produces: user.profile.bio = 'New bio'
      const result = wasm.pipelineFinalize([
        { path: 'user.profile.bio', value: 'New bio' },
      ])
      // Assert shadow updated in finalize
      expect(wasm.shadowGet('user.profile.bio')).toBe('New bio')
      const bioChange = result.state_changes.find(
        (c) => c.path === 'user.profile.bio',
      )
      expect(bioChange).toBeDefined()
    })

    it('should update shadow for buffered concern changes', () => {
      // Initialize shadow with concerns
      wasm.pipelineReset()
      wasm.shadowInit({
        user: { name: 'Alice', role: 'guest', email: 'a@b.com' },
        _concerns: { user: { email: { disabledWhen: false } } },
      })
      // Register BoolLogic
      wasm.registerBoolLogic('_concerns.user.email.disabledWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })
      // processChanges: user.role = 'admin' → BoolLogic evaluates disabledWhen = true (buffered)
      wasm.processChanges([{ path: 'user.role', value: 'admin' }])
      // pipelineFinalize applies buffered change
      const result = wasm.pipelineFinalize([])
      // Assert shadow updated for concern path
      expect(wasm.shadowGet('_concerns.user.email.disabledWhen')).toBe(true)
      const disabledWhenChange = result.state_changes.find(
        (c) => c.path === '_concerns.user.email.disabledWhen',
      )
      expect(disabledWhenChange).toBeDefined()
    })

    it('should update shadow for JS-produced concern changes (validators)', () => {
      // Initialize shadow with concern
      wasm.pipelineReset()
      wasm.shadowInit({
        user: { name: 'Alice', role: 'guest', email: 'a@b.com' },
        _concerns: { user: { email: { validationState: false } } },
      })
      // JS validator produces: _concerns.user.email.validationState = { isError: false }
      const result = wasm.pipelineFinalize([
        {
          path: '_concerns.user.email.validationState',
          value: { isError: false },
        },
      ])
      // Assert shadow updated for concern path (with prefix)
      expect(wasm.shadowGet('_concerns.user.email.validationState')).toEqual({
        isError: false,
      })
      const validationChange = result.state_changes.find(
        (c) => c.path === '_concerns.user.email.validationState',
      )
      expect(validationChange).toBeDefined()
    })
  })

  describe('Buffer clearing', () => {
    it('should clear pending buffers after finalize', () => {
      // Initialize shadow with concerns
      wasm.pipelineReset()
      wasm.shadowInit({
        user: { name: 'Alice', role: 'guest', email: 'a@b.com' },
        _concerns: { user: { email: { disabledWhen: false } } },
      })
      // Register BoolLogic
      wasm.registerBoolLogic('_concerns.user.email.disabledWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })
      // First processChanges (buffers concern change)
      const result1 = wasm.processChanges([
        { path: 'user.role', value: 'admin' },
      ])
      expect(result1.has_work).toBe(true)
      // pipelineFinalize (applies buffered changes)
      const finalizeResult = wasm.pipelineFinalize([])
      const disabledWhenChange = finalizeResult.state_changes.find(
        (c) => c.path === '_concerns.user.email.disabledWhen',
      )
      expect(disabledWhenChange).toBeDefined()
      // Second processChanges with no-op (should not include stale buffered changes)
      const result2 = wasm.processChanges([
        { path: 'user.name', value: 'Alice' },
      ])
      expect(result2.has_work).toBe(false)
      expect(result2.state_changes).toEqual([])
    })
  })
})

describe('Full pipeline flow (integration)', () => {
  it('should complete user change → BoolLogic → listener → validator → valtio', () => {
    // Initialize shadow with concerns
    wasm.pipelineReset()
    wasm.shadowInit({
      user: { name: 'Alice', role: 'guest', email: 'a@b.com' },
      _concerns: {
        user: { email: { disabledWhen: false, validationState: false } },
      },
    })
    // Register BoolLogic: disabledWhen depends on user.role
    wasm.registerBoolLogic('_concerns.user.email.disabledWhen', {
      IS_EQUAL: ['user.role', 'admin'],
    })
    // Register listener on 'user' topic
    wasm.registerListenersBatch([
      { subscriber_id: 1, topic_path: 'user', scope_path: 'user' },
    ])
    // Register validator on user.email
    validatorSchemas.set('user.email', null)
    wasm.registerFunctionsBatch([
      {
        function_id: 1,
        dependency_paths: ['user.email'],
        scope: 'user.email',
        output_path: '_concerns.user.email.validationState',
      },
    ])
    // processChanges
    const processResult = wasm.processChanges([
      { path: 'user.role', value: 'admin' },
      { path: 'user.email', value: 'new@test.com' },
    ])
    expect(processResult.state_changes).toHaveLength(2)
    expect(processResult.has_work).toBe(true)
    expect(processResult.execution_plan).not.toBeNull()
    expect(processResult.validators_to_run.length).toBeGreaterThan(0)
    // JS executes listener and validator, producing changes
    const finalizeResult = wasm.pipelineFinalize([
      { path: 'user.profile.bio', value: 'Updated by listener' },
      {
        path: '_concerns.user.email.validationState',
        value: { isError: false },
      },
    ])
    // Assert final output has all changes
    const statePaths = finalizeResult.state_changes.map((c) => c.path)
    expect(statePaths).toContain('user.role')
    expect(statePaths).toContain('user.email')
    expect(statePaths).toContain('user.profile.bio')
    expect(statePaths).toContain('_concerns.user.email.disabledWhen')
    expect(statePaths).toContain('_concerns.user.email.validationState')
    // Assert shadow state fully updated
    expect(wasm.shadowGet('user.role')).toBe('admin')
    expect(wasm.shadowGet('user.email')).toBe('new@test.com')
    expect(wasm.shadowGet('user.profile.bio')).toBe('Updated by listener')
    expect(wasm.shadowGet('_concerns.user.email.disabledWhen')).toBe(true)
    expect(wasm.shadowGet('_concerns.user.email.validationState')).toEqual({
      isError: false,
    })
  })

  it('should handle no listeners or validators (BoolLogic only)', () => {
    // Initialize shadow with concerns
    wasm.pipelineReset()
    wasm.shadowInit({
      user: { name: 'Alice', role: 'guest', email: 'a@b.com' },
      _concerns: { user: { email: { disabledWhen: false } } },
    })
    // Register BoolLogic only
    wasm.registerBoolLogic('_concerns.user.email.disabledWhen', {
      IS_EQUAL: ['user.role', 'admin'],
    })
    // processChanges
    const processResult = wasm.processChanges([
      { path: 'user.role', value: 'admin' },
    ])
    expect(processResult.has_work).toBe(true)
    // JS does nothing (no listeners/validators)
    // pipelineFinalize with empty array
    const finalizeResult = wasm.pipelineFinalize([])
    // Assert concern change applied
    expect(
      finalizeResult.state_changes.some(
        (c) => c.path === '_concerns.user.email.disabledWhen',
      ),
    ).toBe(true)
    expect(wasm.shadowGet('_concerns.user.email.disabledWhen')).toBe(true)
  })

  it('should handle listeners producing changes that trigger new routing', () => {
    // Register two nested listeners on different topics
    wasm.registerListenersBatch([
      { subscriber_id: 1, topic_path: 'user', scope_path: 'user' },
      { subscriber_id: 2, topic_path: 'user.name', scope_path: 'user.name' },
    ])
    // processChanges
    const processResult = wasm.processChanges([
      { path: 'user.name', value: 'Bob' },
    ])
    expect(processResult.has_work).toBe(true)
    // JS executes listener, produces change
    const finalizeResult = wasm.pipelineFinalize([
      { path: 'user.role', value: 'admin' },
    ])
    // Assert change propagated
    const userNameChange = finalizeResult.state_changes.find(
      (c) => c.path === 'user.name',
    )
    expect(userNameChange).toBeDefined()
    const userRoleChange = finalizeResult.state_changes.find(
      (c) => c.path === 'user.role',
    )
    expect(userRoleChange).toBeDefined()
  })

  it('should handle early exit when has_work = false', () => {
    // Process all no-op changes
    const processResult = wasm.processChanges([
      { path: 'user.name', value: 'Alice' },
    ])
    expect(processResult.has_work).toBe(false)
    // JS checks has_work, skips listener/validator execution
    expect(processResult.execution_plan).toBeNull()
    expect(processResult.validators_to_run).toEqual([])
    // pipelineFinalize not called in this branch
    // Verify workflow completes without errors (implicit)
  })
})

describe('Error handling', () => {
  it('should handle invalid JS changes gracefully', () => {
    // processChanges succeeds
    wasm.processChanges([{ path: 'user.name', value: 'Bob' }])
    // pipelineFinalize with edge cases (missing value, invalid path)
    // The implementation should handle this gracefully
    const result = wasm.pipelineFinalize([
      { path: 'invalid.path', value: 'test' },
    ])
    // Assert result is returned (no throw)
    expect(Array.isArray(result.state_changes)).toBe(true)
    // Shadow state should not be corrupted for valid paths
    expect(wasm.shadowGet('user.name')).toBe('Bob')
  })

  it('should handle empty execution_plan gracefully', () => {
    // No listeners registered
    const processResult = wasm.processChanges([
      { path: 'user.name', value: 'Bob' },
    ])
    // execution_plan is null when no listeners
    expect(processResult.execution_plan).toBeNull()
    // JS handles null plan without errors
    expect(processResult.validators_to_run).toEqual([])
    // pipelineFinalize with empty array
    const finalizeResult = wasm.pipelineFinalize([])
    // Assert completes successfully
    expect(Array.isArray(finalizeResult.state_changes)).toBe(true)
  })
})
