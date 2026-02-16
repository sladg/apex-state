# WASM Pipeline Test Plan (Placeholders Only)

**Purpose**: Comprehensive test coverage for WASM pipeline (EP1-EP6).
**Status**: Placeholder stubs with step comments — NO implementations.
**Date**: 2026-02-15

---

## Test Coverage Summary

| Epic | Component | Existing Coverage | Planned Additions |
|------|-----------|-------------------|-------------------|
| EP1 | BoolLogic | ✅ Comprehensive (991 lines) | None needed |
| EP1 | Interning | ✅ Comprehensive (533 lines) | None needed |
| EP2 | Shadow State | ✅ Comprehensive (1994 lines) | None needed |
| EP2 | Pipeline Basic | ✅ Good (340 lines) | Minor additions |
| EP2 | Sync/Flip/Agg | ✅ Good (630 lines) | Minor additions |
| EP3 | Listener Dispatch (legacy) | ⚠️ Partial (316 lines, has bug) | Fix + extend |
| **EP3** | **FullExecutionPlan** | ❌ **Missing** | **New file needed** |
| **EP4** | **Validation** | ❌ **Stubs only** | **Implement all 18** |
| EP5 | Diff Engine | ✅ Comprehensive (271 lines) | None needed |
| **EP6** | **Pipeline (processChanges + finalize)** | ⚠️ **Partial** | **Major additions** |
| **Integration** | **End-to-End** | ❌ **Missing** | **New file needed** |

---

## Priority Order

1. **Critical** (blocks production use):
   - EP4 Validation — all 18 stubs need implementation
   - EP6 Pipeline — processChanges + pipelineFinalize flow
   - EP3 FullExecutionPlan — propagation map, input_change_ids

2. **High** (completes feature coverage):
   - Integration tests — full pipeline scenarios
   - EP3 listener orchestration — multi-round execution

3. **Medium** (nice to have):
   - Edge cases in existing test files
   - Performance benchmarks

---

## File-by-File Plan

### 1. `tests/wasm/listener-dispatch.test.ts` (EXTEND EXISTING)

**Status**: 316 lines, basic tests exist, **has bug on line 230 (typo)**
**Action**: Fix bug + add FullExecutionPlan tests

#### Bug Fix
```typescript
// Line 230 — REPLACE:
unwasm.registerListenersBatch([2])
// WITH:
wasm.unregisterListenersBatch([2])
```

#### New Tests to Add (in same file)

##### FullExecutionPlan Structure Tests

```typescript
describe('FullExecutionPlan with input_change_ids', () => {
  it('should reference state_changes array by index', () => {
    // Register listener on 'user'
    // Process changes: [{ path: 'user.name', value: 'Bob' }, { path: 'user.age', value: 30 }]
    // Call processChanges (returns PrepareResult with execution_plan)
    // Assert execution_plan.groups[0].dispatches[0].input_change_ids references correct indices
    // Assert indices point to state_changes array elements
  })

  it('should handle single change with single dispatch', () => {
    // Register listener on 'user.email'
    // Process single change to user.email
    // Assert input_change_ids = [0]
  })

  it('should handle multiple changes matching single listener', () => {
    // Register listener on 'user' (watches user.*)
    // Process 3 changes: user.name, user.age, user.email
    // Assert dispatch has input_change_ids = [0, 1, 2]
  })

  it('should handle subset of changes matching listener topic', () => {
    // Register listener on 'user.profile'
    // Process changes: user.name (no match), user.profile.bio (match), settings.theme (no match)
    // Assert dispatch input_change_ids only includes index of user.profile.bio
  })
})

describe('Propagation map structure', () => {
  it('should build propagation_map for parent-child listeners', () => {
    // Register listener 1 on 'user.profile' (depth 2)
    // Register listener 2 on 'user' (depth 1, parent of listener 1)
    // Process change to 'user.profile.bio'
    // Assert execution_plan.propagation_map[dispatch_id_1] contains target for listener 2
    // Assert remap_prefix is 'profile'
  })

  it('should handle multiple propagation targets', () => {
    // Register listener 1 on 'user.profile.settings' (depth 3)
    // Register listener 2 on 'user.profile' (depth 2)
    // Register listener 3 on 'user' (depth 1)
    // Process change to 'user.profile.settings.theme'
    // Assert listener 1 propagates to both listener 2 and listener 3
    // Assert remap_prefix values: 'settings' for depth 2, 'profile.settings' for depth 1
  })

  it('should use empty remap_prefix when target is direct parent', () => {
    // Register listener 1 on 'user.name' (depth 2, leaf)
    // Register listener 2 on 'user' (depth 1, direct parent)
    // Process change to 'user.name'
    // Assert propagation target has remap_prefix = '' (name is already relative to user)
  })

  it('should not create propagation entries for sibling listeners', () => {
    // Register listener 1 on 'user.email'
    // Register listener 2 on 'user.name' (sibling, same depth)
    // Process change to 'user.email'
    // Assert propagation_map does not include listener 2
  })

  it('should handle root listener receiving all changes', () => {
    // Register listener 1 on '' (root, depth 0)
    // Register listener 2 on 'user.profile' (depth 2)
    // Process change to 'user.profile.bio'
    // Assert listener 2 propagates to listener 1 (root)
    // Assert remap_prefix = 'user.profile'
  })
})

describe('FullExecutionPlan vs DispatchPlan (legacy)', () => {
  it('should return FullExecutionPlan from processChanges', () => {
    // Register listener
    // Call wasm.processChanges() → PrepareResult
    // Assert execution_plan is FullExecutionPlan (has groups, propagation_map)
    // Assert NOT DispatchPlan (no levels array)
  })

  it('should still support legacy createDispatchPlan for backward compat', () => {
    // Register listener
    // Call wasm.createDispatchPlan() directly
    // Assert result is DispatchPlan (has levels array)
    // NOTE: This is legacy API, kept for compatibility
  })
})
```

---

### 2. `tests/wasm/validation-batching.test.ts` (IMPLEMENT ALL STUBS)

**Status**: 154 lines, all stubs with step comments
**Action**: Implement all 18 test bodies

**Implementation Note**: Each stub already has step comments. Convert comments to actual code.

#### Tests to Implement (bodies only, names already exist)

```typescript
describe('Single field with Zod schema', () => {
  it('should return validator in processChanges output when dep path changes', () => {
    // IMPLEMENT:
    // 1. Register validator: wasm.registerValidatorsBatch([{ validator_id: 1, output_path: '_concerns.user.email.validationState', dependency_paths: ['user.email'] }])
    // 2. Register Zod schema in validatorSchemas map: validatorSchemas.set(1, z.string().email())
    // 3. Call wasm.processChanges([{ path: 'user.email', value: 'newemail@test.com' }])
    // 4. Assert result.validators_to_run.length === 1
    // 5. Assert validators_to_run[0].validator_id === 1
    // 6. Assert validators_to_run[0].output_path === '_concerns.user.email.validationState'
    // 7. Assert validators_to_run[0].dependency_values['user.email'] === '"newemail@test.com"' (JSON string)
  })

  it('should include correct shadow state value in dependency_values', () => {
    // IMPLEMENT:
    // 1. Register validator on user.name
    // 2. Process change: user.name → 'Bob'
    // 3. Assert validators_to_run[0].dependency_values['user.name'] === '"Bob"'
  })
})

describe('Multi-field validation', () => {
  it('should only trigger affected validators', () => {
    // IMPLEMENT:
    // 1. Register validator_id=1 on user.email, validator_id=2 on user.name
    // 2. Process change to user.email only
    // 3. Assert validators_to_run.length === 1
    // 4. Assert validators_to_run[0].validator_id === 1
    // 5. Assert no validator_id=2 in list
  })

  it('should trigger multiple validators when multiple deps change', () => {
    // IMPLEMENT:
    // 1. Register validator_id=1 on user.email, validator_id=2 on user.name
    // 2. Process changes: [user.email, user.name]
    // 3. Assert validators_to_run.length === 2
    // 4. Assert validator_ids are [1, 2] in some order
  })
})

describe('Multi-path dependency validator', () => {
  it('should trigger validator when any dependency changes', () => {
    // IMPLEMENT:
    // 1. Register validator with dependency_paths: ['order.amount', 'order.currency']
    // 2. Process change to order.amount only
    // 3. Assert validators_to_run.length === 1
    // 4. Assert dependency_values has BOTH 'order.amount' (new) and 'order.currency' (from shadow)
    // 5. Assert 'order.currency' value is from shadow state (e.g., 'USD')
  })

  it('should include all dependency values even when only one changes', () => {
    // IMPLEMENT:
    // 1. Register validator with deps: ['order.amount', 'order.currency', 'order.discount']
    // 2. Process change to order.discount only
    // 3. Assert dependency_values.keys === ['order.amount', 'order.currency', 'order.discount']
    // 4. Assert all values are correct (from shadow state or new change)
  })
})

describe('Mixed concerns: BoolLogic + validation', () => {
  it('should return both concern_changes and validators_to_run', () => {
    // IMPLEMENT:
    // 1. Register BoolLogic: wasm.registerBoolLogic('_concerns.user.email.disabledWhen', { IS_EQUAL: ['user.role', 'admin'] })
    // 2. Register validator on user.email
    // 3. Process changes: [user.role → 'admin', user.email → 'new@test.com']
    // 4. Assert result.state_changes.length > 0 (input changes)
    // 5. Assert result.concern_changes includes BoolLogic result (disabledWhen = true)
    // 6. Assert result.validators_to_run includes validator for user.email
    // 7. Assert they are separate (no interference)
  })

  it('should only trigger BoolLogic when only its dep changes', () => {
    // IMPLEMENT:
    // 1. Register BoolLogic on user.role
    // 2. Register validator on user.email
    // 3. Process change: user.role only
    // 4. Assert concern_changes.length > 0 (BoolLogic triggered)
    // 5. Assert validators_to_run.length === 0 (validator NOT triggered)
  })

  it('should only trigger validator when only its dep changes', () => {
    // IMPLEMENT:
    // 1. Register BoolLogic on user.role
    // 2. Register validator on user.email
    // 3. Process change: user.email only
    // 4. Assert concern_changes.length === 0 (BoolLogic NOT triggered)
    // 5. Assert validators_to_run.length === 1 (validator triggered)
  })
})

describe('Registration lifecycle', () => {
  it('should register and unregister validators', () => {
    // IMPLEMENT:
    // 1. Register validator_id=1 on user.email
    // 2. Process user.email change → assert validators_to_run.length === 1
    // 3. Unregister: wasm.unregisterValidatorsBatch([1])
    // 4. Process user.email change again → assert validators_to_run.length === 0
  })

  it('should handle batch register and unregister', () => {
    // IMPLEMENT:
    // 1. Register 3 validators in one batch: registerValidatorsBatch([...3 entries])
    // 2. Process changes to all 3 fields → assert validators_to_run.length === 3
    // 3. Unregister all 3: unregisterValidatorsBatch([1, 2, 3])
    // 4. Process same changes → assert validators_to_run.length === 0
  })

  it('should clean up reverse index on unregister', () => {
    // IMPLEMENT:
    // 1. Register validator with 2 dependency paths: ['order.amount', 'order.currency']
    // 2. Unregister it: unregisterValidatorsBatch([validator_id])
    // 3. Process change to order.amount
    // 4. Assert validators_to_run.length === 0 (no stale entries)
    // 5. Process change to order.currency
    // 6. Assert validators_to_run.length === 0 (cleanup was thorough)
  })
})

describe('Schema vs custom validation', () => {
  it('should detect schema-based config correctly', () => {
    // IMPLEMENT (NOTE: This may need to be in a different test file if it tests registration.ts logic):
    // 1. Create concern config with schema + no evaluate → isSchemaValidation should be true
    // 2. Create concern config with schema + evaluate → isSchemaValidation should be false
    // 3. Create concern config without schema → isSchemaValidation should be false
    // This tests the detection logic in registration.ts — may need integration with JS layer
    // If testing WASM behavior, verify that validators_to_run only includes schema-based validators
  })
})

describe('Edge cases', () => {
  it('should return empty validators_to_run when no validators registered', () => {
    // IMPLEMENT:
    // 1. Do NOT register any validators
    // 2. Process any change
    // 3. Assert validators_to_run is empty array or undefined
  })

  it('should handle validator on non-existent shadow path gracefully', () => {
    // IMPLEMENT:
    // 1. Register validator with dependency on 'missing.path' (does not exist in shadow state)
    // 2. Process change to some other field that triggers reverse index lookup
    // 3. Assert dependency_values contains 'missing.path' with value 'null' (JSON string)
  })

  it('should deduplicate validators when multiple deps change', () => {
    // IMPLEMENT:
    // 1. Register validator with deps: ['order.amount', 'order.currency']
    // 2. Process changes: [order.amount, order.currency] in same batch
    // 3. Assert validators_to_run.length === 1 (validator appears exactly once)
    // 4. Assert dependency_values includes both paths
  })
})
```

---

### 3. `tests/wasm/pipeline-finalize.test.ts` (NEW FILE)

**Status**: Does not exist
**Action**: Create new file for EP6 pipeline finalize flow tests

```typescript
/**
 * EP6 Two-Phase Pipeline Tests
 *
 * Tests processChanges() → JS execution → pipelineFinalize() flow.
 * Covers no-op filtering at all checkpoints, has_work flag, buffering.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest'

import type { Change } from '../../src/wasm/bridge'
import { initWasm, resetWasm, validatorSchemas, wasm } from '../../src/wasm/bridge'
import { z } from 'zod'

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

describe('Phase 1: processChanges', () => {
  describe('No-op filtering at input (checkpoint 1)', () => {
    it('should filter out no-op changes before entering pipeline', () => {
      // Initialize shadow state: user.name = 'Alice'
      // Process change: user.name = 'Alice' (no-op)
      // Assert result.state_changes is empty
      // Assert result.has_work === false
    })

    it('should keep genuine changes after diff', () => {
      // Initialize shadow state: user.name = 'Alice'
      // Process change: user.name = 'Bob' (genuine change)
      // Assert result.state_changes.length === 1
      // Assert result.state_changes[0].value === 'Bob'
    })

    it('should filter mixed batch (some no-ops, some genuine)', () => {
      // Initialize shadow state: user.name = 'Alice', user.role = 'guest'
      // Process changes: [user.name = 'Alice' (no-op), user.role = 'admin' (genuine)]
      // Assert result.state_changes.length === 1
      // Assert result.state_changes[0].path === 'user.role'
    })

    it('should early exit when all changes are no-ops', () => {
      // Initialize shadow state: user.name = 'Alice', user.role = 'guest'
      // Process changes: all matching shadow state
      // Assert result.has_work === false
      // Assert result.execution_plan is null or empty
      // Assert result.validators_to_run is empty
    })
  })

  describe('No-op filtering after BoolLogic/sync/flip (checkpoint 2)', () => {
    it('should filter no-op BoolLogic results', () => {
      // Register BoolLogic: _concerns.user.email.disabledWhen = IS_EQUAL(user.role, 'admin')
      // Initialize BoolLogic result in shadow: disabledWhen = false
      // Process change: user.role = 'guest' → BoolLogic evaluates to false (no-op)
      // Assert result.concern_changes does NOT include disabledWhen (filtered)
    })

    it('should keep changed BoolLogic results', () => {
      // Register BoolLogic: disabledWhen = IS_EQUAL(user.role, 'admin')
      // Initialize shadow: disabledWhen = false
      // Process change: user.role = 'admin' → BoolLogic = true (changed)
      // Assert result includes concern change for disabledWhen = true
    })

    it('should filter no-op sync writes', () => {
      // Register sync: ['user.name', 'profile.name']
      // Initialize shadow: user.name = 'Alice', profile.name = 'Alice' (already synced)
      // Process change: user.name = 'Alice' (no-op input, filtered at checkpoint 1)
      // Assert no sync writes generated (early exit)
    })

    it('should filter no-op flip writes', () => {
      // Register flip: ['visible', 'hidden']
      // Initialize shadow: visible = true, hidden = false (already flipped)
      // Process change: visible = true (no-op)
      // Assert no flip writes generated
    })
  })

  describe('has_work flag', () => {
    it('should set has_work = false when no listeners, validators, or concern changes', () => {
      // No registrations
      // Process no-op change
      // Assert result.has_work === false
    })

    it('should set has_work = true when listeners registered', () => {
      // Register listener on 'user'
      // Process change to user.name
      // Assert result.has_work === true
      // Assert result.execution_plan is not null
    })

    it('should set has_work = true when validators need to run', () => {
      // Register validator on user.email
      // Process change to user.email
      // Assert result.has_work === true
      // Assert result.validators_to_run.length > 0
    })

    it('should set has_work = true when BoolLogic produces concern changes', () => {
      // Register BoolLogic that will change value
      // Process triggering change
      // Assert result.has_work === true
      // NOTE: BoolLogic changes are buffered, not in state_changes yet
    })
  })

  describe('Concern change buffering', () => {
    it('should NOT include BoolLogic changes in Phase 1 state_changes', () => {
      // Register BoolLogic: disabledWhen depends on user.role
      // Process change: user.role = 'admin' (BoolLogic → true)
      // Assert result.state_changes does NOT include _concerns.* paths
      // Assert result has_work === true (work is buffered, will be applied in finalize)
    })

    it('should buffer multiple BoolLogic results', () => {
      // Register 3 BoolLogic expressions on different paths
      // Process change that triggers all 3
      // Assert state_changes has no _concerns.* paths
      // Assert has_work === true (all buffered)
    })
  })

  describe('Shadow state NOT updated in Phase 1', () => {
    it('should NOT update shadow state during processChanges', () => {
      // Initialize shadow: user.name = 'Alice'
      // Call processChanges([user.name = 'Bob'])
      // Assert wasm.shadowGet('user.name') === 'Alice' (unchanged)
      // NOTE: Shadow update happens in pipelineFinalize
    })

    it('should return readonly state_changes for JS listener execution', () => {
      // Process change to user.name
      // Assert result.state_changes is array of changes (readonly context)
      // Assert shadow state is still old value
    })
  })
})

describe('Phase 2: pipelineFinalize', () => {
  describe('Input partitioning (_concerns. prefix)', () => {
    it('should partition js_changes by _concerns. prefix', () => {
      // JS produces mixed changes: ['user.profile.bio' (state), '_concerns.user.email.validationState' (concern)]
      // Call pipelineFinalize([...mixed changes])
      // Assert result.state_changes includes 'user.profile.bio'
      // Assert result.concern_changes includes 'user.email.validationState' (prefix stripped)
    })

    it('should strip _concerns. prefix from concern paths', () => {
      // JS produces: [{ path: '_concerns.user.email.validationState', value: {...} }]
      // Call pipelineFinalize([...])
      // Assert result.concern_changes[0].path === 'user.email.validationState' (no prefix)
    })

    it('should handle empty JS changes', () => {
      // No listeners or validators produced changes
      // Call pipelineFinalize([])
      // Assert result.state_changes is from buffered BoolLogic only (if any)
      // Assert result.concern_changes is from buffered BoolLogic only
    })
  })

  describe('Merging with buffered concern changes', () => {
    it('should merge JS validator results with buffered BoolLogic changes', () => {
      // Phase 1: BoolLogic produces concern change (buffered)
      // JS executes validator, produces concern change
      // Call pipelineFinalize with validator result
      // Assert result.concern_changes includes BOTH BoolLogic + validator results
    })

    it('should apply buffered concern changes even if JS produces none', () => {
      // Phase 1: BoolLogic produces concern change (buffered)
      // JS produces no changes (no listeners/validators ran)
      // Call pipelineFinalize([])
      // Assert result.concern_changes includes BoolLogic result
    })
  })

  describe('No-op filtering at finalize (checkpoint 3)', () => {
    it('should filter no-op state changes from listeners', () => {
      // Listener produces change: user.name = 'Alice' (matches shadow state)
      // Call pipelineFinalize([{ path: 'user.name', value: 'Alice' }])
      // Assert result.state_changes does NOT include user.name (filtered)
    })

    it('should filter no-op concern changes from validators', () => {
      // Validator produces: _concerns.user.email.validationState = { isError: false }
      // Shadow concern state already has same value
      // Call pipelineFinalize with validator result
      // Assert result.concern_changes does NOT include this path (filtered)
    })

    it('should keep changed values after diff', () => {
      // Listener produces: user.profile.bio = 'New bio' (different from shadow)
      // Call pipelineFinalize([...])
      // Assert result.state_changes includes user.profile.bio
    })
  })

  describe('Shadow state update in finalize', () => {
    it('should update shadow state after finalize', () => {
      // Phase 1: processChanges with user.name = 'Bob'
      // Assert shadow still has 'Alice'
      // Phase 2: pipelineFinalize([])
      // Assert wasm.shadowGet('user.name') === 'Bob' (updated)
    })

    it('should update shadow for both state and concern changes', () => {
      // Phase 1: state change + BoolLogic concern change
      // Phase 2: finalize
      // Assert shadow state updated for state path
      // Assert shadow _concerns.* updated for concern path
    })
  })

  describe('Buffer clearing', () => {
    it('should clear pending buffers after finalize', () => {
      // Phase 1: processChanges (buffers concern changes)
      // Phase 2: pipelineFinalize
      // Call processChanges again (new batch)
      // Assert no stale buffered changes from previous call
    })
  })
})

describe('Full two-phase flow (integration)', () => {
  it('should complete user change → BoolLogic → listener → validator → valtio', () => {
    // Register BoolLogic: disabledWhen depends on user.role
    // Register listener on 'user' that produces change to user.profile.bio
    // Register validator on user.email
    // Phase 1: processChanges([user.role = 'admin', user.email = 'new@test.com'])
    //   → returns: { state_changes: [user.role, user.email], execution_plan, validators_to_run }
    // JS executes listener (produce: user.profile.bio = 'Updated by listener')
    // JS executes validator (produce: _concerns.user.email.validationState = {...})
    // Phase 2: pipelineFinalize([listener output, validator output])
    //   → returns: { state_changes: [user.role, user.email, user.profile.bio], concern_changes: [disabledWhen, validationState] }
    // Assert final output has all changes, correctly partitioned
    // Assert shadow state fully updated
  })

  it('should handle no listeners or validators (BoolLogic only)', () => {
    // Register BoolLogic only
    // Phase 1: processChanges
    //   → returns has_work = true (BoolLogic buffered)
    // JS does nothing (no listeners/validators)
    // Phase 2: pipelineFinalize([])
    //   → returns concern_changes from buffered BoolLogic
    // Assert concern changes applied
  })

  it('should handle listeners producing changes that trigger new routing', () => {
    // Register nested listeners: depth 2, depth 1
    // Phase 1: processChanges (depth 2 listener in plan)
    // JS executes depth 2 listener, produces change
    // Change propagates to depth 1 listener via propagation_map
    // NOTE: This tests propagation, not multi-round (multi-round is separate concern)
    // Assert depth 1 listener receives propagated changes in next execution
  })

  it('should handle early exit when has_work = false', () => {
    // Process all no-op changes
    // Phase 1: has_work = false
    // JS checks has_work, skips listener/validator execution
    // JS does NOT call pipelineFinalize
    // Assert workflow completes without errors
  })
})

describe('Error handling', () => {
  it('should handle invalid JS changes gracefully', () => {
    // Phase 1: processChanges succeeds
    // Phase 2: pipelineFinalize with malformed change (e.g., missing value)
    // Assert error is returned, shadow state not corrupted
  })

  it('should handle empty execution_plan gracefully', () => {
    // No listeners registered
    // Phase 1: execution_plan is null
    // JS handles null plan without errors
    // Phase 2: finalize with empty array
    // Assert completes successfully
  })
})
```

---

### 4. `tests/wasm/end-to-end-integration.test.ts` (NEW FILE)

**Status**: Does not exist
**Action**: Create new file for full pipeline integration tests

```typescript
/**
 * End-to-End Integration Tests
 *
 * Tests complete scenarios combining all pipeline features:
 * BoolLogic, listeners, validators, sync/flip, multi-round execution.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest'

import type { Change } from '../../src/wasm/bridge'
import { initWasm, resetWasm, validatorSchemas, wasm } from '../../src/wasm/bridge'
import { z } from 'zod'

beforeEach(async () => {
  const wasmModule = await import('../../rust/pkg/apex_state_wasm.js')
  initWasm(wasmModule)
})

afterEach(() => {
  validatorSchemas.clear()
  resetWasm()
})

describe('Complete pipeline scenarios', () => {
  describe('User profile update flow', () => {
    it('should handle: user input → validation → BoolLogic → listeners → final state', () => {
      // Initialize state: user = { name: 'Alice', email: 'old@test.com', role: 'guest', profile: { bio: '' } }
      // Register BoolLogic: _concerns.user.email.disabledWhen = IS_EQUAL(user.role, 'admin')
      // Register validator on user.email (Zod email schema)
      // Register listener on 'user' that updates user.profile.bio when user.name changes
      //
      // Phase 1: processChanges([user.name = 'Bob', user.email = 'bob@test.com', user.role = 'admin'])
      //   → BoolLogic evaluates: disabledWhen = true (buffered)
      //   → Validator triggered: user.email validation
      //   → Listener triggered: 'user' listener in execution_plan
      //
      // JS executes listener:
      //   → handler sees user.name = 'Bob'
      //   → produces: [{ path: 'user.profile.bio', value: 'Updated for Bob' }]
      //
      // JS executes validator:
      //   → Zod validates 'bob@test.com' → success
      //   → produces: [{ path: '_concerns.user.email.validationState', value: { isError: false, errors: [] } }]
      //
      // Phase 2: pipelineFinalize([listener output, validator output])
      //   → merges with buffered BoolLogic
      //   → diffs all changes
      //   → returns: { state_changes: [user.name, user.email, user.role, user.profile.bio], concern_changes: [disabledWhen, validationState] }
      //
      // Assert final state_changes has all 4 paths
      // Assert concern_changes has both disabledWhen and validationState
      // Assert shadow state fully updated
    })
  })

  describe('Multi-round listener execution', () => {
    it('should execute listeners in depth order: deepest first', () => {
      // Register listener 1 on 'user.profile.settings' (depth 3)
      // Register listener 2 on 'user.profile' (depth 2)
      // Register listener 3 on 'user' (depth 1)
      //
      // Process change to 'user.profile.settings.theme'
      //
      // Phase 1: execution_plan.groups ordered deepest-first
      //   → group[0]: listener 1 (depth 3)
      //   → group[1]: listener 2 (depth 2)
      //   → group[2]: listener 3 (depth 1)
      //
      // JS executes group 0 (listener 1):
      //   → produces change X
      //   → propagates to group 1 via propagation_map
      //
      // JS executes group 1 (listener 2):
      //   → receives change X + original changes
      //   → produces change Y
      //   → propagates to group 2
      //
      // JS executes group 2 (listener 3):
      //   → receives change Y + propagated changes
      //   → produces final change Z
      //
      // Assert all groups executed in order
      // Assert propagated changes correctly remapped
    })

    it('should handle listener producing change that triggers validator', () => {
      // Register listener on 'user.profile' that updates user.email
      // Register validator on user.email
      //
      // Phase 1: processChanges([user.profile.bio = 'New bio'])
      //   → listener in execution_plan
      //   → validator NOT in validators_to_run (user.email not changed yet)
      //
      // JS executes listener:
      //   → produces: [{ path: 'user.email', value: 'generated@test.com' }]
      //
      // Phase 2: pipelineFinalize([listener output])
      //   → applies user.email change
      //   → NOTE: Validator is NOT re-triggered in same batch (validators run once per processChanges call)
      //
      // Assert user.email updated
      // Assert validation does NOT run in this batch (expected behavior: validators only trigger on direct processChanges input)
    })
  })

  describe('Sync + Flip + BoolLogic + Listeners', () => {
    it('should combine all pipeline features in one batch', () => {
      // Initialize state: { fieldA: 'old', fieldB: 'old', flagX: true, flagY: false, user: { role: 'guest' } }
      // Register sync: ['fieldA', 'fieldB']
      // Register flip: ['flagX', 'flagY']
      // Register BoolLogic: _concerns.panel.visibleWhen = IS_EQUAL(user.role, 'admin')
      // Register listener on 'user' that updates user.lastModified
      //
      // Phase 1: processChanges([fieldA = 'new', flagX = false, user.role = 'admin'])
      //   → aggregation: none
      //   → sync: fieldB = 'new' (synced from fieldA)
      //   → flip: flagY = true (flipped from flagX)
      //   → BoolLogic: visibleWhen = true (buffered)
      //   → listener: 'user' listener triggered
      //
      // JS executes listener:
      //   → produces: [{ path: 'user.lastModified', value: Date.now() }]
      //
      // Phase 2: pipelineFinalize([listener output])
      //   → state_changes: [fieldA, fieldB, flagX, flagY, user.role, user.lastModified]
      //   → concern_changes: [visibleWhen]
      //
      // Assert all changes present
      // Assert sync/flip applied correctly
    })
  })

  describe('Aggregation write distribution', () => {
    it('should distribute aggregation write to multiple sources', () => {
      // Initialize state: { allUsers: null, user1: { name: 'Alice' }, user2: { name: 'Bob' }, user3: { name: 'Charlie' } }
      // Register aggregation: target = 'allUsers', sources = ['user1', 'user2', 'user3']
      //
      // Phase 1: processChanges([allUsers.status = 'active'])
      //   → aggregation intercepts 'allUsers.status' write
      //   → distributes to: user1.status, user2.status, user3.status
      //   → output state_changes: [user1.status, user2.status, user3.status] (allUsers removed)
      //
      // Phase 2: pipelineFinalize([])
      //   → applies distributed changes
      //
      // Assert user1.status, user2.status, user3.status all = 'active'
      // Assert allUsers.status NOT in output (intercepted)
    })

    it('should handle aggregation + listener combination', () => {
      // Register aggregation: allUsers → [user1, user2]
      // Register listener on 'user1' that logs changes
      //
      // Phase 1: processChanges([allUsers.role = 'editor'])
      //   → aggregation: distributes to user1.role, user2.role
      //   → listener: 'user1' listener triggered by user1.role change
      //
      // JS executes listener:
      //   → logs user1.role change
      //   → produces: [{ path: 'logs', value: [...] }]
      //
      // Phase 2: finalize
      //   → applies all changes
      //
      // Assert user1.role and user2.role updated
      // Assert listener executed and produced log entry
    })
  })

  describe('Complex BoolLogic with multiple dependencies', () => {
    it('should evaluate AND/OR logic with nested paths', () => {
      // Initialize state: { user: { role: 'guest', age: 25, verified: true }, product: { price: 100 } }
      // Register BoolLogic: _concerns.checkout.enabledWhen =
      //   AND([
      //     OR([IS_EQUAL(user.role, 'admin'), IS_EQUAL(user.role, 'premium')]),
      //     GTE(user.age, 18),
      //     IS_EQUAL(user.verified, true),
      //     LT(product.price, 1000)
      //   ])
      //
      // Initial evaluation: false (role is 'guest')
      //
      // Phase 1: processChanges([user.role = 'premium'])
      //   → BoolLogic evaluates: true (all conditions met)
      //   → buffered concern change: enabledWhen = true
      //
      // Phase 2: finalize
      //   → applies concern change
      //
      // Assert _concerns.checkout.enabledWhen = true
    })
  })

  describe('Validation with multi-path dependencies', () => {
    it('should validate order total based on amount + currency + discount', () => {
      // Initialize state: { order: { amount: 100, currency: 'USD', discount: 10 } }
      // Register validator on order.total with deps: ['order.amount', 'order.currency', 'order.discount']
      // Zod schema: validates calculated total = amount * (1 - discount/100) in correct currency
      //
      // Phase 1: processChanges([order.discount = 20])
      //   → validator triggered (order.discount is a dependency)
      //   → validators_to_run includes validator with all 3 dependency values
      //
      // JS executes validator:
      //   → calculates total: 100 * (1 - 0.2) = 80 USD
      //   → validates: success
      //   → produces: [{ path: '_concerns.order.total.validationState', value: { isError: false } }]
      //
      // Phase 2: finalize
      //   → applies validation result
      //
      // Assert _concerns.order.total.validationState = { isError: false }
    })
  })

  describe('No-op optimization across full pipeline', () => {
    it('should filter no-ops at all 3 checkpoints', () => {
      // Initialize state: { user: { name: 'Alice', role: 'admin' } }
      // Initialize concern state: { _concerns.user.email.disabledWhen: true }
      // Register BoolLogic: disabledWhen = IS_EQUAL(user.role, 'admin') (already true)
      //
      // Phase 1: processChanges([user.name = 'Alice', user.role = 'admin'])
      //   → checkpoint 1 (input diff): both no-ops → filtered → early exit
      //   → has_work = false
      //
      // JS checks has_work, skips execution
      //
      // Assert result.state_changes is empty
      // Assert result.has_work = false
      // Assert no finalize call needed
    })

    it('should filter no-op BoolLogic at checkpoint 2', () => {
      // Initialize state: user.role = 'admin'
      // Initialize concern: disabledWhen = true
      // Register BoolLogic: disabledWhen = IS_EQUAL(user.role, 'admin')
      //
      // Phase 1: processChanges([user.role = 'admin'])
      //   → checkpoint 1: no-op → filtered → early exit OR
      //   → if change makes it through (edge case), BoolLogic evaluates to true
      //   → checkpoint 2: disabledWhen = true (no-op) → filtered
      //
      // Assert concern_changes does NOT include disabledWhen (filtered at checkpoint 2)
    })

    it('should filter no-op listener output at checkpoint 3', () => {
      // Initialize state: user.profile.bio = 'Existing bio'
      // Register listener on 'user.name' that sets user.profile.bio = 'Existing bio' (same value)
      //
      // Phase 1: processChanges([user.name = 'Bob'])
      //   → listener triggered
      //
      // JS executes listener:
      //   → produces: [{ path: 'user.profile.bio', value: 'Existing bio' }]
      //
      // Phase 2: pipelineFinalize([listener output])
      //   → checkpoint 3: user.profile.bio = 'Existing bio' matches shadow → filtered
      //
      // Assert result.state_changes does NOT include user.profile.bio
    })
  })

  describe('Error recovery', () => {
    it('should handle listener throwing error gracefully', () => {
      // Register listener that throws error
      // Phase 1: processChanges
      // JS executes listener → error thrown
      // JS catches error, continues processing
      // Phase 2: finalize with partial results
      // Assert pipeline completes, shadow state consistent
      // NOTE: Error handling is JS layer concern, WASM should not crash
    })

    it('should handle validator throwing error gracefully', () => {
      // Register validator with Zod schema that throws
      // Phase 1: processChanges
      // JS executes validator → error thrown
      // JS catches error, logs, continues
      // Phase 2: finalize with partial results
      // Assert pipeline completes
    })
  })
})

describe('Performance benchmarks', () => {
  it('should process 1000 changes through full pipeline efficiently', () => {
    // Initialize state with 1000 fields
    // Register BoolLogic on 10 fields
    // Register validators on 10 fields
    // Register 5 listeners at various depths
    //
    // Phase 1: processChanges with 1000 changes
    //   → measure time for WASM processing
    //
    // Phase 2: finalize
    //   → measure time for diff + apply
    //
    // Assert total time < threshold (e.g., 50ms for 1000 changes)
    // Assert no memory leaks
  })

  it('should handle high no-op ratio efficiently (90% no-ops)', () => {
    // Initialize state with 10000 fields
    // Process 10000 changes, 90% are no-ops
    //
    // Phase 1: processChanges
    //   → checkpoint 1 should filter 9000 changes quickly
    //
    // Assert time < threshold
    // Assert only 1000 genuine changes processed
  })
})
```

---

## Test Execution Priority

### Immediate (blocking production)

1. **EP4 Validation** — `validation-batching.test.ts`
   - All 18 stubs → implementations
   - Critical for production validation feature

2. **EP6 Pipeline** — `pipeline-finalize.test.ts` (new file)
   - processChanges + pipelineFinalize flow
   - No-op filtering at all checkpoints
   - has_work flag behavior

### High Priority (feature completion)

3. **EP3 FullExecutionPlan** — extend `listener-dispatch.test.ts`
   - Fix bug on line 230
   - Add FullExecutionPlan structure tests
   - Add propagation_map tests

4. **End-to-End Integration** — `end-to-end-integration.test.ts` (new file)
   - Complete pipeline scenarios
   - Multi-round listener execution
   - Complex feature combinations

### Medium Priority (polish)

5. Performance benchmarks in integration tests
6. Edge cases in existing test files

---

## Summary Statistics

| Category | Test Files | Test Count (est.) | Status |
|----------|-----------|-------------------|--------|
| Existing (EP1-EP2) | 5 files | ~150 tests | ✅ Complete |
| EP3 Extend | 1 file | +15 tests | ⚠️ Fix + extend |
| EP4 Implement | 1 file | 18 tests | ❌ All stubs |
| EP6 New File | 1 file | ~40 tests | ❌ Missing |
| Integration New File | 1 file | ~20 tests | ❌ Missing |
| **Total** | **9 files** | **~243 tests** | **~60% done** |

---

## Notes

- **NO implementations yet** — this document is placeholders only
- Each test has step-comment structure describing what to test and how
- Next step: implement test bodies following these placeholders
- Priority order ensures critical features tested first
- Integration tests verify features work together correctly
- Performance benchmarks ensure WASM optimization is effective

---

**Next Steps**: Review this plan → approve → begin implementations
