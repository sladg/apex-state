# WASM-EP4: Validation Batching

**Type**: Epic
**Priority**: P1
**Depends on**: WASM-EP2
**Goal**: Replace per-field `effect()` for validation concerns with WASM-driven dispatch plans. Zod schemas stay in JS; Rust decides which validators to run.

---

## WASM-021: Validator reverse dependency index

**Type**: Story | **Points**: 3 | **Priority**: P1
**Depends on**: WASM-004

### Description

Extend the reverse dependency index (WASM-004) to support validators. When a path changes, the index returns which validators need re-evaluation.

### Interface

```rust
fn register_validator(validator_id: u32, dep_path_ids: &[u32])
// Internally: for each dep_path_id, add validator_id to reverse index

fn unregister_validator(validator_id: u32)
// Remove from all reverse index entries

fn affected_validators(changed_path_ids: &[u32]) -> Vec<(u32, u32)>
// Returns [(path_id, validator_id)] pairs to evaluate
```

### Key insight

Validators may depend on multiple paths (e.g., "max value depends on currency and notional"). The reverse index must track all dependency paths per validator.

### Acceptance criteria

- [ ] Multi-path dependency tracking per validator
- [ ] Reverse index returns correct validator IDs for single-path changes
- [ ] Reverse index returns correct validator IDs for multi-path changes (deduped)
- [ ] Unregistration cleans up all reverse index entries
- [ ] Performance: lookup in O(1) average case

---

## WASM-022: Validation dispatch plan

**Type**: Story | **Points**: 2 | **Priority**: P1
**Depends on**: WASM-021

### Description

After pipeline processing, WASM produces a validation dispatch plan listing which validators to run. This replaces per-field `effect()` for validation concerns.

### Output format

```rust
struct ValidationPlan {
    validations: Vec<ValidationDispatch>,
}

struct ValidationDispatch {
    path_id: u32,        // field being validated
    validator_id: u32,   // which validator to run
    value_slot: u32,     // current value (slot index) to validate
}
```

### Acceptance criteria

- [ ] Plan includes only validators affected by the current change batch
- [ ] Plan is deduped (each validator appears at most once per pipeline run)
- [ ] Plan includes the current value slot for the field being validated
- [ ] Empty plan when no validators are affected

---

## WASM-023: JS-side batch Zod execution

**Type**: Story | **Points**: 3 | **Priority**: P1
**Depends on**: WASM-022

### Description

Implement the JS-side batch executor that runs Zod schemas based on WASM's validation dispatch plan. Results are written to the `_concerns` proxy.

### Flow

```
WASM returns validationPlan
JS:
  for (pathId, validatorId, valueSlot) of validationPlan:
    value = resolveSlot(valueSlot)
    result = schemaMap[validatorId].safeParse(value)
    store._concerns[resolvePath(pathId)].validationState = result
```

### Key files

- `src/concerns/prebuilts/validationState.ts` — extract schema registration
- `src/_internal/concerns/registration.ts` — skip `effect()` for validation concerns with schema-only config

### Acceptance criteria

- [ ] Validation concerns with Zod schemas no longer create individual `effect()` instances
- [ ] Validation results match current behavior exactly
- [ ] Custom validation concerns (with user `evaluate()`) still use `effect()`
- [ ] Cleanup on unmount removes validator registrations from WASM
- [ ] All existing validation tests pass

---

## WASM-024: Phase 4 integration tests

**Type**: Story | **Points**: 2 | **Priority**: P1
**Depends on**: WASM-023

### Description

End-to-end tests for WASM-driven validation batching.

### Test scenarios

1. Single field with Zod schema — validates on change
2. Multi-field validation — only affected validators run
3. Validator with multi-path dependencies — triggers on any dependency change
4. Mixed: BoolLogic concerns + validation in same pipeline run
5. Registration lifecycle: mount, validate, unmount, re-mount

### Acceptance criteria

- [ ] All scenarios pass
- [ ] Validation timing matches current behavior (synchronous within pipeline)
- [ ] No regression in validation result format (Zod SafeParseReturnType)
