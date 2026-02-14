# WASM-EP4: Validation Batching (REVISED)

**Type**: Epic
**Priority**: P1
**Depends on**: WASM-EP2
**Goal**: Replace per-field `effect()` for validation concerns with WASM-driven dispatch. Zod schemas stay in JS; WASM decides which validators to run based on processChanges() output.

**Key Architectural Decisions**:
- Batch validator registration (single call)
- Validators integrated into processChanges() return value
- Zod schemas execute in JS (can't cross WASM boundary)
- Validation results are just another type of change in flat output
- String paths at boundary (no path IDs)

---

## WASM-024: Validator registry and reverse index

**Type**: Story | **Points**: 3 | **Priority**: P1
**Depends on**: WASM-005

### Description

Extend the reverse dependency index to support validators. Similar to BoolLogic, when paths change, the index returns which validators need re-evaluation. Validators may depend on multiple paths.

### Data model

```rust
struct ValidatorRegistry {
    // validator_id → metadata
    validators: HashMap<u32, ValidatorMetadata>,
}

struct ValidatorMetadata {
    output_path: String,      // "_concerns.user.email.validationState"
    dependency_paths: Vec<String>,  // ["user.email", "user.role"]
}

// Uses shared reverse dependency index from WASM-005
// path_id → [validator_ids]
```

### Interface

```rust
fn register_validators_batch(validators_json: &str) -> Result<(), JsValue>
// Input: [
//   {
//     "validator_id": 1,
//     "output_path": "_concerns.user.email.validationState",
//     "dependency_paths": ["user.email"]
//   },
//   {
//     "validator_id": 2,
//     "output_path": "_concerns.order.amount.validationState",
//     "dependency_paths": ["order.amount", "order.currency"]
//   }
// ]

fn unregister_validators_batch(validator_ids_json: &str) -> Result<(), JsValue>
// Input: [1, 2, 3]
```

### Acceptance criteria

- [ ] Batch registration of validators
- [ ] Multi-path dependency tracking per validator
- [ ] Reverse index returns correct validator IDs for changes
- [ ] Deduplication (each validator appears once even if multiple deps changed)
- [ ] Unregistration cleans up all reverse index entries
- [ ] Performance: lookup in O(1) average case

---

## WASM-025: Validator evaluation in processChanges()

**Type**: Story | **Points**: 4 | **Priority**: P1
**Depends on**: WASM-024

### Description

Extend processChanges() to return affected validators as part of the output. WASM identifies which validators to run; JS executes Zod schemas and returns results; WASM includes validator results in final change list.

### Enhanced output format

```rust
// processChanges() returns:
{
  "changes": [
    { "path": "user.email", "value_json": "\"test@example.com\"" },  // Input
    { "path": "_concerns.user.email.disabledWhen", "value_json": "false" }  // BoolLogic
  ],
  "validators_to_run": [
    {
      "validator_id": 1,
      "output_path": "_concerns.user.email.validationState",
      "dependency_values": {
        "user.email": "\"test@example.com\""  // JSON-encoded values
      }
    }
  ]
}
```

### Algorithm extension

```rust
fn process_changes(changes_json: &str) -> Result<String, JsValue> {
    // 1-10: Same as EP2 (pipeline + BoolLogic)
    let all_changes = run_pipeline_with_boollogic(changes_json)?;

    // 11: Find affected validators
    let affected_paths = extract_changed_paths(&all_changes);
    let affected_validator_ids = find_affected_validators(affected_paths);

    // 12: Prepare validator dispatch info
    let validators_to_run = affected_validator_ids.iter().map(|vid| {
        let meta = validators.get(vid).unwrap();
        let dependency_values = meta.dependency_paths.iter().map(|path| {
            (path.clone(), shadow_get(path).unwrap_or("null".to_string()))
        }).collect();

        ValidatorDispatch {
            validator_id: *vid,
            output_path: meta.output_path.clone(),
            dependency_values,
        }
    }).collect();

    // 13: Return changes + validator dispatch info
    Ok(json!({
        "changes": serialize_changes(all_changes),
        "validators_to_run": validators_to_run
    }).to_string())
}
```

### JS-side validation execution

```typescript
// Modified processChanges to handle validators
export const processChanges = (changes: Change[]): Change[] => {
  const changesJson = JSON.stringify(
    changes.map(c => ({ path: c.path, value_json: JSON.stringify(c.value) }))
  )

  const resultJson = wasm.process_changes(changesJson)
  const result = JSON.parse(resultJson)

  const allChanges = result.changes.map((c: any) => ({
    path: c.path,
    value: JSON.parse(c.value_json)
  }))

  // Execute validators if any are affected
  if (result.validators_to_run && result.validators_to_run.length > 0) {
    for (const validator of result.validators_to_run) {
      const schema = validatorSchemas.get(validator.validator_id)
      if (!schema) continue

      // Parse dependency values
      const values = Object.fromEntries(
        Object.entries(validator.dependency_values).map(([k, v]) => [k, JSON.parse(v as string)])
      )

      // For single-field validators, validate the primary value
      const primaryPath = Object.keys(values)[0]
      const result = schema.safeParse(values[primaryPath])

      // Add validation result to changes
      allChanges.push({
        path: validator.output_path,
        value: result
      })
    }
  }

  return allChanges
}
```

### Acceptance criteria

- [ ] Affected validators identified correctly
- [ ] Validator dispatch includes all dependency values
- [ ] JS executes Zod schemas with correct values
- [ ] Validation results added to final change list
- [ ] Multi-path validators receive all dependency values
- [ ] Performance: validator dispatch overhead < 5µs

---

## WASM-026: Validation concern integration

**Type**: Story | **Points**: 3 | **Priority**: P1
**Depends on**: WASM-025

### Description

Update validation concern registration to use batch WASM registration instead of per-field `effect()`. Schema-based validation concerns skip `effect()` wrapping; custom validators with user-provided `evaluate()` functions still use `effect()`.

### Changes to registration

**`src/_internal/concerns/registration.ts`**:
```typescript
// Detect schema-based validation concerns
const isSchemaValidation = (config: any): boolean => {
  return config.schema && !config.evaluate
}

// Registration logic
export const registerConcern = (
  store: StoreInstance,
  path: string,
  concernName: string,
  config: ConcernConfig
): CleanupFn => {
  const outputPath = `_concerns.${path}.${concernName}`

  if (isSchemaValidation(config)) {
    // Schema-based validation → WASM batch registration
    const validatorId = nextValidatorId++

    validatorSchemas.set(validatorId, config.schema)

    // Will be registered in batch during store initialization
    pendingValidators.push({
      validator_id: validatorId,
      output_path: outputPath,
      dependency_paths: [path]  // Single-field validation
    })

    return () => {
      validatorSchemas.delete(validatorId)
      wasm.unregister_validators_batch(JSON.stringify([validatorId]))
    }
  } else {
    // Custom validation → traditional effect() wrapping
    return registerCustomConcern(store, path, concernName, config)
  }
}
```

**Batch registration on store init**:
```typescript
// Called once during store initialization
export const flushPendingValidators = (): void => {
  if (pendingValidators.length > 0) {
    wasm.register_validators_batch(JSON.stringify(pendingValidators))
    pendingValidators = []
  }
}
```

### Acceptance criteria

- [ ] Schema-based validation uses WASM (no `effect()`)
- [ ] Custom validation still uses `effect()` (no regression)
- [ ] Batch registration on store initialization
- [ ] Cleanup calls unregister for validators
- [ ] All existing validation tests pass
- [ ] No public API changes

---

## WASM-027: Phase 4 integration tests

**Type**: Story | **Points**: 2 | **Priority**: P1
**Depends on**: WASM-026

### Description

End-to-end tests for WASM-driven validation batching.

### Test scenarios

1. **Single field with Zod schema**
   - Field changes → validator runs → result in `_concerns`

2. **Multi-field validation**
   - Multiple fields change → only affected validators run

3. **Validator with multi-path dependencies**
   - Validation depends on two paths (e.g., amount + currency)
   - Change either path → validator runs with both values

4. **Mixed concerns**
   - BoolLogic + validation in same pipeline run
   - Verify both types of concerns evaluate correctly

5. **Schema vs custom validation**
   - Schema-based uses WASM
   - Custom evaluate() uses effect()
   - Both work correctly

6. **Registration lifecycle**
   - Mount component → validators registered
   - Unmount → validators unregistered
   - Re-mount → validators work again

7. **Validation error formatting**
   - Zod errors formatted correctly
   - Error messages accessible in UI

### Performance targets

- 10 validation changes: < 100µs
- 100 fields with 50 validators: < 500µs

### Acceptance criteria

- [ ] All scenarios pass
- [ ] Validation timing synchronous within pipeline
- [ ] No regression in Zod result format (SafeParseReturnType)
- [ ] Performance targets met
- [ ] Tests run in Node (vitest)

---

## Summary

**Phase 4 Stories**:
- WASM-024: Validator registry and reverse index (batch registration)
- WASM-025: Validator evaluation in processChanges()
- WASM-026: Validation concern integration (skip effect() for schemas)
- WASM-027: Integration tests

**Total Points**: 12

**Key Simplifications from Original**:
- Batch validator registration (single call)
- Validators integrated into processChanges() return value
- Validation results are regular changes (uniform output)
- String paths at boundary (no path IDs)
- Schema execution stays in JS (can't cross WASM boundary)

**Dependencies**:
- WASM-024 extends reverse dependency index from WASM-005 (EP1)
- WASM-025 extends processChanges from WASM-015 (EP2)
- Works alongside BoolLogic evaluation (both in processChanges output)

**Integration Pattern**:
```
processChanges(input_changes) {
  1. Run pipeline (aggregation → sync → flip)
  2. Evaluate BoolLogic
  3. Identify affected validators
  4. Return: changes + validators_to_run
}

JS executes Zod schemas
→ Add validation results to changes
→ Apply all changes to store
```
