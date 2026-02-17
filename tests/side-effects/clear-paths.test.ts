/**
 * TEST: ClearPaths Side-Effect
 *
 * Validates the clearPaths side-effect end-to-end through the WASM pipeline.
 * Tests cover: concrete paths, wildcard triggers/targets ([*] correlated, [**] expanded),
 * pipeline ordering (Step 3.5), and interactions with sync/flip/BoolLogic.
 *
 * See docs/CLEAR_PATHS.md for architecture details.
 */

import { afterEach, beforeEach, describe, expect, it } from 'vitest'

import { createWasmPipeline, type WasmPipeline } from '../../src/wasm/bridge'

/** Helper: find change by path */
const findChange = (
  changes: { path: string; value: unknown }[],
  path: string,
) => changes.find((c) => c.path === path)

/** Helper: get all paths from changes */
const getPaths = (changes: { path: string; value: unknown }[]) =>
  changes.map((c) => c.path)

describe('ClearPaths: Concrete triggers and targets', () => {
  let pipeline: WasmPipeline
  beforeEach(() => {
    pipeline = createWasmPipeline()
  })
  afterEach(() => {
    pipeline.destroy()
  })

  // Setup: form.email = "old@test.com", form.errors = "invalid", form.touched = true
  // Clear rule: triggers=["form.email"], targets=["form.errors", "form.touched"]
  //
  // Action: setValue("form.email", "new@test.com")
  //
  // Expected:
  //   - form.email = "new@test.com"
  //   - form.errors = null
  //   - form.touched = null
  it('should clear target paths when trigger path changes', () => {
    pipeline.shadowInit({
      form: { email: 'old@test.com', errors: 'invalid', touched: true },
    })
    pipeline.registerSideEffects({
      registration_id: 'test',
      clear_paths: [
        {
          triggers: ['form.email'],
          targets: ['form.errors', 'form.touched'],
        },
      ],
    })

    const changes = pipeline.processChanges([
      { path: 'form.email', value: 'new@test.com' },
    ]).state_changes
    const paths = getPaths(changes)
    expect(paths).toContain('form.email')
    expect(paths).toContain('form.errors')
    expect(paths).toContain('form.touched')

    const errorsChange = findChange(changes, 'form.errors')
    expect(errorsChange?.value).toBeNull()

    const touchedChange = findChange(changes, 'form.touched')
    expect(touchedChange?.value).toBeNull()
  })

  // Setup: form.email = "same@test.com", form.errors = "invalid"
  // Clear rule: triggers=["form.email"], targets=["form.errors"]
  //
  // Action: setValue("form.email", "same@test.com") — same value, diff filters it
  //
  // Expected: no changes, form.errors remains "invalid"
  it('should not clear when trigger value is unchanged (no-op)', () => {
    pipeline.shadowInit({
      form: { email: 'same@test.com', errors: 'invalid' },
    })
    pipeline.registerSideEffects({
      registration_id: 'test',
      clear_paths: [{ triggers: ['form.email'], targets: ['form.errors'] }],
    })

    const changes = pipeline.processChanges([
      { path: 'form.email', value: 'same@test.com' },
    ]).state_changes
    expect(changes).toHaveLength(0)
  })

  // Setup: form.email = "a@b.com", form.errors = null
  // Clear rule: triggers=["form.email"], targets=["form.errors"]
  //
  // Action: setValue("form.email", "new@b.com")
  //
  // Expected: only form.email change, form.errors NOT in output (already null)
  it('should skip clearing targets that are already null', () => {
    pipeline.shadowInit({
      form: { email: 'a@b.com', errors: null },
    })
    pipeline.registerSideEffects({
      registration_id: 'test',
      clear_paths: [{ triggers: ['form.email'], targets: ['form.errors'] }],
    })

    const changes = pipeline.processChanges([
      { path: 'form.email', value: 'new@b.com' },
    ]).state_changes
    expect(changes).toHaveLength(1)
    const firstChange = changes[0]
    expect(firstChange).toBeDefined()
    expect(firstChange?.path).toBe('form.email')
  })

  // Setup: form.email = "a", form.name = "b", form.errors = "err"
  // Clear rule: triggers=["form.email", "form.name"], targets=["form.errors"]
  //
  // Action: setValue("form.email", "new")
  //
  // Expected: form.email + form.errors = null
  it('should handle multiple triggers on the same rule', () => {
    pipeline.shadowInit({
      form: { email: 'a', name: 'b', errors: 'err' },
    })
    pipeline.registerSideEffects({
      registration_id: 'test',
      clear_paths: [
        {
          triggers: ['form.email', 'form.name'],
          targets: ['form.errors'],
        },
      ],
    })

    const changes = pipeline.processChanges([
      { path: 'form.email', value: 'new' },
    ]).state_changes
    const paths = getPaths(changes)
    expect(paths).toContain('form.email')
    expect(paths).toContain('form.errors')

    const errorsChange = findChange(changes, 'form.errors')
    expect(errorsChange?.value).toBeNull()
  })

  // Setup: a = "x", b = "y", target = "z"
  // Clear rule 1: triggers=["a"], targets=["target"]
  // Clear rule 2: triggers=["b"], targets=["target"]
  //
  // Action: batch change a + b
  //
  // Expected: target cleared once (not duplicated in output)
  it('should deduplicate overlapping targets from multiple rules', () => {
    pipeline.shadowInit({ a: 'x', b: 'y', target: 'z' })
    pipeline.registerSideEffects({
      registration_id: 'test',
      clear_paths: [
        { triggers: ['a'], targets: ['target'] },
        { triggers: ['b'], targets: ['target'] },
      ],
    })

    const changes = pipeline.processChanges([
      { path: 'a', value: 'x2' },
      { path: 'b', value: 'y2' },
    ]).state_changes
    const targetCount = changes.filter((c) => c.path === 'target').length
    expect(targetCount).toBe(1)
  })

  // Setup: form.errors = { email: "e1", name: "e2" }
  // Clear rule: triggers=["form.email"], targets=["form.errors"]
  //
  // Action: setValue("form.email", "new")
  //
  // Expected: form.errors = null (entire object becomes null)
  it('should clear entire subtree when target is an object', () => {
    pipeline.shadowInit({
      form: { email: 'old', errors: { email: 'e1', name: 'e2' } },
    })
    pipeline.registerSideEffects({
      registration_id: 'test',
      clear_paths: [{ triggers: ['form.email'], targets: ['form.errors'] }],
    })

    const changes = pipeline.processChanges([
      { path: 'form.email', value: 'new' },
    ]).state_changes
    const errorsChange = findChange(changes, 'form.errors')
    expect(errorsChange).toBeDefined()
    expect(errorsChange?.value).toBeNull()
  })
})

describe('Wildcard triggers with correlated targets ([*])', () => {
  let pipeline: WasmPipeline
  beforeEach(() => {
    pipeline = createWasmPipeline()
  })
  afterEach(() => {
    pipeline.destroy()
  })

  // Setup: form.fields.email = { value: "old", error: "invalid" }
  //        form.fields.name  = { value: "Alice", error: "too short" }
  // Clear rule: triggers=["form.fields.[*].value"], targets=["form.fields.[*].error"]
  //
  // Action: setValue("form.fields.email.value", "new@test.com")
  //
  // Expected:
  //   - form.fields.email.value = "new@test.com"
  //   - form.fields.email.error = null (correlated: same key "email")
  //   - form.fields.name.error  remains "too short" (different key)
  it('should clear only the matching key when trigger and target share [*]', () => {
    pipeline.shadowInit({
      form: {
        fields: {
          email: { value: 'old', error: 'invalid' },
          name: { value: 'Alice', error: 'too short' },
        },
      },
    })
    pipeline.registerSideEffects({
      registration_id: 'test',
      clear_paths: [
        {
          triggers: ['form.fields.[*].value'],
          targets: ['form.fields.[*].error'],
        },
      ],
    })

    const changes = pipeline.processChanges([
      { path: 'form.fields.email.value', value: 'new@test.com' },
    ]).state_changes
    const paths = getPaths(changes)
    expect(paths).toContain('form.fields.email.value')
    expect(paths).toContain('form.fields.email.error')
    expect(paths).not.toContain('form.fields.name.error')
  })

  // Setup: app.section1.fieldA = { value: "v1", error: "e1" }
  //        app.section1.fieldB = { value: "v2", error: "e2" }
  //        app.section2.fieldC = { value: "v3", error: "e3" }
  // Clear rule: triggers=["app.[*].[*].value"], targets=["app.[*].[*].error"]
  //
  // Action: setValue("app.section1.fieldA.value", "new")
  //
  // Expected: only app.section1.fieldA.error cleared
  it('should handle multi-level wildcard correlation', () => {
    pipeline.shadowInit({
      app: {
        section1: {
          fieldA: { value: 'v1', error: 'e1' },
          fieldB: { value: 'v2', error: 'e2' },
        },
        section2: {
          fieldC: { value: 'v3', error: 'e3' },
        },
      },
    })
    pipeline.registerSideEffects({
      registration_id: 'test',
      clear_paths: [
        {
          triggers: ['app.[*].[*].value'],
          targets: ['app.[*].[*].error'],
        },
      ],
    })

    const changes = pipeline.processChanges([
      { path: 'app.section1.fieldA.value', value: 'new' },
    ]).state_changes
    const paths = getPaths(changes)
    expect(paths).toContain('app.section1.fieldA.error')
    expect(paths).not.toContain('app.section1.fieldB.error')
    expect(paths).not.toContain('app.section2.fieldC.error')
  })

  // Setup: form.fields.email = { value: "v1", error: "e1" }
  //        form.fields.name  = { value: "v2", error: "e2" }
  // Clear rule: triggers=["form.fields.[*].value"], targets=["form.fields.[*].error"]
  //
  // Action: batch setValue for email.value AND name.value
  //
  // Expected: both email.error and name.error cleared
  // Setup: items.row1 = { input: "old" } — NO warning key at all
  // Clear rule: triggers=["items.[*].input"], targets=["items.[*].warning"]
  //
  // Action: setValue("items.row1.input", "new")
  //
  // Expected: only row1.input change — row1.warning doesn't exist (treated as null), skip
  it('should skip [*] target when resolved path does not exist', () => {
    pipeline.shadowInit({
      items: {
        row1: { input: 'old' }, // no "warning" key
      },
    })
    pipeline.registerSideEffects({
      registration_id: 'test',
      clear_paths: [
        {
          triggers: ['items.[*].input'],
          targets: ['items.[*].warning'],
        },
      ],
    })

    const changes = pipeline.processChanges([
      { path: 'items.row1.input', value: 'new' },
    ]).state_changes
    const paths = getPaths(changes)
    expect(paths).toContain('items.row1.input')
    // warning doesn't exist → treated as null → no clear change produced
    expect(paths).not.toContain('items.row1.warning')
  })

  // Setup: items.row1 = { input: "old", warning: null }
  // Clear rule: triggers=["items.[*].input"], targets=["items.[*].warning"]
  //
  // Action: setValue("items.row1.input", "new")
  //
  // Expected: only row1.input change — row1.warning is already null, skip
  it('should skip [*] target when resolved path is already null', () => {
    pipeline.shadowInit({
      items: {
        row1: { input: 'old', warning: null },
      },
    })
    pipeline.registerSideEffects({
      registration_id: 'test',
      clear_paths: [
        {
          triggers: ['items.[*].input'],
          targets: ['items.[*].warning'],
        },
      ],
    })

    const changes = pipeline.processChanges([
      { path: 'items.row1.input', value: 'new' },
    ]).state_changes
    const paths = getPaths(changes)
    expect(paths).toContain('items.row1.input')
    expect(paths).not.toContain('items.row1.warning')
  })

  it('should handle multiple changes in one batch matching different wildcard keys', () => {
    pipeline.shadowInit({
      form: {
        fields: {
          email: { value: 'old1', error: 'e1' },
          name: { value: 'old2', error: 'e2' },
        },
      },
    })
    pipeline.registerSideEffects({
      registration_id: 'test',
      clear_paths: [
        {
          triggers: ['form.fields.[*].value'],
          targets: ['form.fields.[*].error'],
        },
      ],
    })

    const changes = pipeline.processChanges([
      { path: 'form.fields.email.value', value: 'new1' },
      { path: 'form.fields.name.value', value: 'new2' },
    ]).state_changes
    const paths = getPaths(changes)
    expect(paths).toContain('form.fields.email.error')
    expect(paths).toContain('form.fields.name.error')
  })
})

describe('Wildcard triggers with expanded targets ([**] via expandMatch)', () => {
  let pipeline: WasmPipeline
  beforeEach(() => {
    pipeline = createWasmPipeline()
  })
  afterEach(() => {
    pipeline.destroy()
  })

  // Setup: form.fields.email = { value: "old", error: "e1" }
  //        form.fields.name  = { value: "old", error: "e2" }
  //        form.fields.age   = { value: "old", error: "e3" }
  // Clear rule: triggers=["form.fields.[*].value"], targets=["form.fields.[*].error"]
  //   with expandMatch: true → TS rewrites target [*] to [**]
  //
  // Action: setValue("form.fields.email.value", "new")
  //
  // Expected: ALL field errors cleared (email, name, age) — not just email
  it('should clear ALL keys when expandMatch: true', () => {
    pipeline.shadowInit({
      form: {
        fields: {
          email: { value: 'old', error: 'e1' },
          name: { value: 'old', error: 'e2' },
          age: { value: 'old', error: 'e3' },
        },
      },
    })
    // expandMatch: true → TS rewrites [*] to [**] in targets
    pipeline.registerSideEffects({
      registration_id: 'test',
      clear_paths: [
        {
          triggers: ['form.fields.[*].value'],
          targets: ['form.fields.[**].error'],
        },
      ],
    })

    const changes = pipeline.processChanges([
      { path: 'form.fields.email.value', value: 'new' },
    ]).state_changes
    const paths = getPaths(changes)
    expect(paths).toContain('form.fields.email.error')
    expect(paths).toContain('form.fields.name.error')
    expect(paths).toContain('form.fields.age.error')
  })

  // Setup: form.email = "old"
  //        form.fields.email = { error: "e1" }
  //        form.fields.name  = { error: "e2" }
  // Clear rule: triggers=["form.email"], targets=["form.fields.[**].error"]
  //   (concrete trigger, expanded target)
  //
  // Action: setValue("form.email", "new")
  //
  // Expected: both form.fields.email.error and form.fields.name.error cleared
  it('should expand [**] even when trigger is concrete', () => {
    pipeline.shadowInit({
      form: {
        email: 'old',
        fields: {
          email: { error: 'e1' },
          name: { error: 'e2' },
        },
      },
    })
    pipeline.registerSideEffects({
      registration_id: 'test',
      clear_paths: [
        {
          triggers: ['form.email'],
          targets: ['form.fields.[**].error'],
        },
      ],
    })

    const changes = pipeline.processChanges([
      { path: 'form.email', value: 'new' },
    ]).state_changes
    const paths = getPaths(changes)
    expect(paths).toContain('form.fields.email.error')
    expect(paths).toContain('form.fields.name.error')
  })

  // Setup: form.fields.email has value but no error, name has error but it's null
  // Clear rule: triggers=["form.fields.[*].value"], targets=["form.fields.[**].error"]
  //
  // Action: setValue("form.fields.email.value", "new")
  //
  // Expected: only email.value change — expanded [**] finds keys but errors are null/missing
  it('should skip [**] expanded targets when resolved paths are null or missing', () => {
    pipeline.shadowInit({
      form: {
        fields: {
          email: { value: 'old' }, // no "error" key at all
          name: { value: 'x', error: null }, // error exists but is null
        },
      },
    })
    pipeline.registerSideEffects({
      registration_id: 'test',
      clear_paths: [
        {
          triggers: ['form.fields.[*].value'],
          targets: ['form.fields.[**].error'],
        },
      ],
    })

    const changes = pipeline.processChanges([
      { path: 'form.fields.email.value', value: 'new' },
    ]).state_changes
    const paths = getPaths(changes)
    expect(paths).toContain('form.fields.email.value')
    // email.error doesn't exist, name.error is null → neither produces a clear change
    expect(paths).not.toContain('form.fields.email.error')
    expect(paths).not.toContain('form.fields.name.error')
  })

  // Setup: some fields have errors, others don't
  // Clear rule: triggers=["form.submit"], targets=["form.fields.[**].error"]
  //
  // Expected: only fields with non-null errors are cleared
  it('should only clear non-null targets when [**] expands over mixed null/non-null', () => {
    pipeline.shadowInit({
      form: {
        submit: false,
        fields: {
          email: { error: 'required' },
          name: { error: null },
          age: { error: 'too young' },
        },
      },
    })
    pipeline.registerSideEffects({
      registration_id: 'test',
      clear_paths: [
        {
          triggers: ['form.submit'],
          targets: ['form.fields.[**].error'],
        },
      ],
    })

    const changes = pipeline.processChanges([
      { path: 'form.submit', value: true },
    ]).state_changes
    const paths = getPaths(changes)
    expect(paths).toContain('form.submit')
    // email.error and age.error have values → cleared
    expect(paths).toContain('form.fields.email.error')
    expect(paths).toContain('form.fields.age.error')
    // name.error is already null → not in output
    expect(paths).not.toContain('form.fields.name.error')
  })

  // Setup: form.email = "old", form.fields = {}
  // Clear rule: triggers=["form.email"], targets=["form.fields.[**].error"]
  //
  // Action: setValue("form.email", "new")
  //
  // Expected: only form.email change, no clears (empty object has no keys)
  it('should produce no changes when [**] expands over empty object', () => {
    pipeline.shadowInit({
      form: { email: 'old', fields: {} },
    })
    pipeline.registerSideEffects({
      registration_id: 'test',
      clear_paths: [
        {
          triggers: ['form.email'],
          targets: ['form.fields.[**].error'],
        },
      ],
    })

    const changes = pipeline.processChanges([
      { path: 'form.email', value: 'new' },
    ]).state_changes
    expect(changes).toHaveLength(1)
    const firstChange = changes[0]
    expect(firstChange).toBeDefined()
    expect(firstChange?.path).toBe('form.email')
  })
})

describe('Undefined/null parent paths with [**] expansion', () => {
  let pipeline: WasmPipeline
  beforeEach(() => {
    pipeline = createWasmPipeline()
  })
  afterEach(() => {
    pipeline.destroy()
  })

  // Setup: form.fields is undefined/null initially, errors exist elsewhere
  // Clear rule: triggers=["form.submit"], targets=["form.fields.[**].error"]
  //
  // After form.fields gets populated via a separate change, [**] should expand
  // to the newly created keys.
  it('should handle [**] expansion after parent path transitions from null to object', () => {
    // form.fields starts as null — no keys to expand
    pipeline.shadowInit({
      form: { submit: false, fields: null as unknown },
    })
    pipeline.registerSideEffects({
      registration_id: 'test',
      clear_paths: [
        {
          triggers: ['form.submit'],
          targets: ['form.fields.[**].error'],
        },
      ],
    })

    // First: populate form.fields with some children
    pipeline.processChanges([
      { path: 'form.fields.email.value', value: 'test@test.com' },
      { path: 'form.fields.email.error', value: 'invalid' },
      { path: 'form.fields.name.value', value: 'Alice' },
      { path: 'form.fields.name.error', value: 'too short' },
    ])

    // Now trigger the clear — [**] should expand to email and name
    const changes = pipeline.processChanges([
      { path: 'form.submit', value: true },
    ]).state_changes
    const paths = getPaths(changes)
    expect(paths).toContain('form.submit')
    expect(paths).toContain('form.fields.email.error')
    expect(paths).toContain('form.fields.name.error')

    const emailError = findChange(changes, 'form.fields.email.error')
    expect(emailError?.value).toBeNull()
    const nameError = findChange(changes, 'form.fields.name.error')
    expect(nameError?.value).toBeNull()
  })

  // Setup: form.fields doesn't exist at all (not even null)
  // After nested changes create the structure, [**] expansion should work
  it('should handle [**] expansion when parent path did not exist initially', () => {
    pipeline.shadowInit({
      form: { submit: false },
    })
    pipeline.registerSideEffects({
      registration_id: 'test',
      clear_paths: [
        {
          triggers: ['form.submit'],
          targets: ['form.fields.[**].error'],
        },
      ],
    })

    // Populate form.fields via nested path changes
    pipeline.processChanges([
      { path: 'form.fields.email.error', value: 'required' },
    ])

    // Trigger clear — [**] should find "email" key
    const changes = pipeline.processChanges([
      { path: 'form.submit', value: true },
    ]).state_changes
    const paths = getPaths(changes)
    expect(paths).toContain('form.fields.email.error')

    const emailError = findChange(changes, 'form.fields.email.error')
    expect(emailError?.value).toBeNull()
  })

  // [*] correlated targets should also work when parent was initially null.
  // The clear rule is registered AFTER the structure is populated, so the
  // initial population doesn't trigger clears.
  it('should handle correlated [*] when parent was initially null', () => {
    pipeline.shadowInit({
      form: { fields: null as unknown },
    })

    // Populate structure first (no clear rules registered yet)
    pipeline.processChanges([
      { path: 'form.fields.email.value', value: 'old' },
      { path: 'form.fields.email.error', value: 'invalid' },
    ])

    // Now register the clear rule
    pipeline.registerSideEffects({
      registration_id: 'test',
      clear_paths: [
        {
          triggers: ['form.fields.[*].value'],
          targets: ['form.fields.[*].error'],
        },
      ],
    })

    // Trigger: change email.value → should clear email.error
    const changes = pipeline.processChanges([
      { path: 'form.fields.email.value', value: 'new@test.com' },
    ]).state_changes
    const paths = getPaths(changes)
    expect(paths).toContain('form.fields.email.value')
    expect(paths).toContain('form.fields.email.error')

    const emailError = findChange(changes, 'form.fields.email.error')
    expect(emailError?.value).toBeNull()
  })

  // Setup: source.items exists, target.path is undefined
  // Clear rule: triggers=["source.items.[*].value"],
  //             targets=["target.path.[*].nested.[**].value"] (expandMatch: true)
  //
  // When target.path is undefined, [**] expansion should produce nothing (no crash).
  // After target.path is populated, the expansion should work.
  it('should handle [*] + [**] target when target parent path is undefined', () => {
    pipeline.shadowInit({
      source: {
        items: {
          row1: { value: 'old' },
        },
      },
      // target.path is completely missing / undefined
    })
    pipeline.registerSideEffects({
      registration_id: 'test',
      clear_paths: [
        {
          triggers: ['source.items.[*].value'],
          targets: ['target.path.[*].nested.[**].value'],
        },
      ],
    })

    // First: trigger fires but target.path is undefined → no clears, no crash
    const changes1 = pipeline.processChanges([
      { path: 'source.items.row1.value', value: 'new1' },
    ]).state_changes
    const paths1 = getPaths(changes1)
    expect(paths1).toContain('source.items.row1.value')
    // No target paths cleared (target.path doesn't exist)
    expect(paths1).toHaveLength(1)

    // Now populate the target structure
    pipeline.processChanges([
      { path: 'target.path.row1.nested.a.value', value: 'v1' },
      { path: 'target.path.row1.nested.b.value', value: 'v2' },
    ])

    // Trigger again — [*] binds "row1", [**] expands nested keys ["a", "b"]
    const changes2 = pipeline.processChanges([
      { path: 'source.items.row1.value', value: 'new2' },
    ]).state_changes
    const paths2 = getPaths(changes2)
    expect(paths2).toContain('source.items.row1.value')
    expect(paths2).toContain('target.path.row1.nested.a.value')
    expect(paths2).toContain('target.path.row1.nested.b.value')

    const aChange = findChange(changes2, 'target.path.row1.nested.a.value')
    expect(aChange?.value).toBeNull()
    const bChange = findChange(changes2, 'target.path.row1.nested.b.value')
    expect(bChange?.value).toBeNull()
  })
})

describe('Pipeline ordering (Step 3.5) — interaction with other side-effects', () => {
  let pipeline: WasmPipeline
  beforeEach(() => {
    pipeline = createWasmPipeline()
  })
  afterEach(() => {
    pipeline.destroy()
  })

  // Setup: form.errors = "err", mirror.errors = "err"
  // Clear rule: triggers=["form.email"], targets=["form.errors"]
  // Sync pair: ["form.errors", "mirror.errors"]
  //
  // Action: setValue("form.email", "new")
  //
  // Expected pipeline:
  //   Step 3:   form.email = "new"
  //   Step 3.5: form.errors = null
  //   Step 4-5: mirror.errors = null (synced)
  it('should propagate cleared values through sync', () => {
    pipeline.shadowInit({
      form: { email: 'old', errors: 'err' },
      mirror: { errors: 'err' },
    })
    pipeline.registerSideEffects({
      registration_id: 'test',
      sync_pairs: [['form.errors', 'mirror.errors']],
      clear_paths: [{ triggers: ['form.email'], targets: ['form.errors'] }],
    })

    const changes = pipeline.processChanges([
      { path: 'form.email', value: 'new' },
    ]).state_changes
    const paths = getPaths(changes)
    expect(paths).toContain('form.errors')
    expect(paths).toContain('mirror.errors')

    const mirrorChange = findChange(changes, 'mirror.errors')
    expect(mirrorChange?.value).toBeNull()
  })

  // Setup: form.email = "old", form.errors = "invalid"
  // Clear rule: triggers=["form.email"], targets=["form.errors"]
  // BoolLogic: _concerns.form.hasErrors.disabledWhen = EXISTS("form.errors")
  //
  // Action: setValue("form.email", "new")
  //
  // Expected: form.errors cleared → EXISTS evaluates to false → concern updated
  // Note: BoolLogic concern changes are buffered in prepare_changes and only
  // appear after pipelineFinalize (two-phase flow).
  it('should trigger BoolLogic re-evaluation after clear', () => {
    pipeline.shadowInit({
      form: { email: 'old', errors: 'invalid' },
    })
    pipeline.registerSideEffects({
      registration_id: 'test',
      clear_paths: [{ triggers: ['form.email'], targets: ['form.errors'] }],
    })
    pipeline.registerBoolLogic('_concerns.form.hasErrors', {
      EXISTS: 'form.errors',
    })

    // Phase 1: processChanges buffers concern changes
    const phase1 = pipeline.processChanges([
      { path: 'form.email', value: 'new' },
    ])
    expect(phase1.has_work).toBe(true)
    expect(getPaths(phase1.state_changes)).toContain('form.errors')

    // Phase 2: pipelineFinalize returns all changes including concern updates
    const phase2 = pipeline.pipelineFinalize([])
    const concern = findChange(phase2.state_changes, '_concerns.form.hasErrors')
    expect(concern).toBeDefined()
    expect(concern?.value).toBe(false)
  })

  // Setup: a = "x", b = "y", c = "z"
  // Clear rule 1: triggers=["a"], targets=["b"]
  // Clear rule 2: triggers=["b"], targets=["c"]
  //
  // Action: setValue("a", "new")
  //
  // Expected: a="new", b=null. c remains "z" (no cascade)
  it('should NOT cascade into other clear rules', () => {
    pipeline.shadowInit({ a: 'x', b: 'y', c: 'z' })
    pipeline.registerSideEffects({
      registration_id: 'test',
      clear_paths: [
        { triggers: ['a'], targets: ['b'] },
        { triggers: ['b'], targets: ['c'] },
      ],
    })

    const changes = pipeline.processChanges([
      { path: 'a', value: 'new' },
    ]).state_changes
    const paths = getPaths(changes)
    expect(paths).toContain('a')
    expect(paths).toContain('b')
    expect(paths).not.toContain('c')
  })

  // Setup: items.a.price = 10, items.b.price = 10, totals.price = 10
  // Clear rule: triggers=["items.a.price"], targets=["items.b.price"]
  // Aggregation: target="totals.price", sources=["items.a.price", "items.b.price"]
  //
  // Action: setValue("items.a.price", 20)
  //
  // Expected: items.b.price cleared → aggregation reads [20, null]
  // Aggregation all-equal logic excludes null values → only non-null is 20 → totals.price = 20
  it('should affect aggregation read recomputation', () => {
    pipeline.shadowInit({
      items: { a: { price: 10 }, b: { price: 10 } },
      totals: { price: 10 },
    })
    pipeline.registerSideEffects({
      registration_id: 'test',
      aggregation_pairs: [
        ['totals.price', 'items.a.price'],
        ['totals.price', 'items.b.price'],
      ],
      clear_paths: [
        { triggers: ['items.a.price'], targets: ['items.b.price'] },
      ],
    })

    const changes = pipeline.processChanges([
      { path: 'items.a.price', value: 20 },
    ]).state_changes
    const paths = getPaths(changes)
    expect(paths).toContain('items.b.price')
    expect(paths).toContain('totals.price')

    // null is a valid value: [20, null] → sources disagree → null
    const totalsChange = findChange(changes, 'totals.price')
    expect(totalsChange?.value).toBeNull()
  })

  // Setup: enabled = true, disabled = false
  // Clear rule: triggers=["toggle"], targets=["enabled"]
  // Flip pair: ["enabled", "disabled"]
  //
  // Action: setValue("toggle", true)
  //
  // Expected: enabled = null (cleared), flip sees null → not boolean → no flip
  //   disabled remains false
  it('should interact correctly with flip (cleared boolean → null is non-boolean)', () => {
    pipeline.shadowInit({ toggle: false, enabled: true, disabled: false })
    pipeline.registerSideEffects({
      registration_id: 'test',
      flip_pairs: [['enabled', 'disabled']],
      clear_paths: [{ triggers: ['toggle'], targets: ['enabled'] }],
    })

    const changes = pipeline.processChanges([
      { path: 'toggle', value: true },
    ]).state_changes
    const paths = getPaths(changes)
    expect(paths).toContain('toggle')
    expect(paths).toContain('enabled')
    // disabled should NOT be in output (null is not boolean, flip skips)
    expect(paths).not.toContain('disabled')

    const enabledChange = findChange(changes, 'enabled')
    expect(enabledChange?.value).toBeNull()
  })
})

describe('Registration and validation', () => {
  let pipeline: WasmPipeline
  beforeEach(() => {
    pipeline = createWasmPipeline()
  })
  afterEach(() => {
    pipeline.destroy()
  })

  // Register clearPaths alongside sync/flip/aggregation
  // Verify: processing a trigger change produces expected clears
  it('should register via registerSideEffects consolidated API', () => {
    pipeline.shadowInit({ form: { email: 'old', errors: 'err' } })
    pipeline.registerSideEffects({
      registration_id: 'test-effects',
      clear_paths: [{ triggers: ['form.email'], targets: ['form.errors'] }],
    })

    const changes = pipeline.processChanges([
      { path: 'form.email', value: 'new' },
    ]).state_changes
    const paths = getPaths(changes)
    expect(paths).toContain('form.errors')
  })

  // Register, verify clears work, unregister, verify clears stop
  it('should unregister clear rules by registration_id', () => {
    pipeline.shadowInit({ form: { email: 'old', errors: 'err' } })
    pipeline.registerSideEffects({
      registration_id: 'test-effects',
      clear_paths: [{ triggers: ['form.email'], targets: ['form.errors'] }],
    })

    // First: verify clear works
    const changes1 = pipeline.processChanges([
      { path: 'form.email', value: 'new1' },
    ]).state_changes
    expect(getPaths(changes1)).toContain('form.errors')

    // Unregister
    pipeline.unregisterSideEffects('test-effects')

    // Re-init shadow for next change (errors was cleared to null)
    pipeline.pipelineReset()
    pipeline.shadowInit({ form: { email: 'new1', errors: 'err2' } })

    // Second: verify clear stopped
    const changes2 = pipeline.processChanges([
      { path: 'form.email', value: 'new2' },
    ]).state_changes
    expect(getPaths(changes2)).not.toContain('form.errors')
  })

  // Attempt to register triggers=["form.[**].value"]
  // Expected: registration error
  it('should reject [**] in trigger paths at registration time', () => {
    pipeline.shadowInit({ form: { email: 'old' } })
    expect(() => {
      pipeline.registerSideEffects({
        registration_id: 'test-bad',
        clear_paths: [
          {
            triggers: ['form.[**].value'],
            targets: ['form.errors'],
          },
        ],
      })
    }).toThrow()
  })

  // Attempt: triggers=["form.email"], targets=["form.[*].error"]
  // Expected: registration error (target has [*] bind but trigger has no captures)
  it('should reject target [*] when trigger has no wildcards to bind', () => {
    pipeline.shadowInit({ form: { email: 'old' } })
    expect(() => {
      pipeline.registerSideEffects({
        registration_id: 'test-bad',
        clear_paths: [
          {
            triggers: ['form.email'],
            targets: ['form.[*].error'],
          },
        ],
      })
    }).toThrow()
  })
})
