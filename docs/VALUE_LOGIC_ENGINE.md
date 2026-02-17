# ValueLogic Engine — Conditional Value Selection in WASM

**Status:** PROPOSED
**Date:** 2026-02-17
**Author:** Architecture session

---

## Context

BoolLogic evaluates conditions and returns `boolean`. We need a sibling engine that evaluates conditions and returns **arbitrary JSON values** — primarily to drive options lists, labels, or any concern value based on state. This is `ValueLogic`.

**Example use case:** A dropdown's options depend on `user.role`:
```typescript
options: {
  value_logic: {
    IF: { IS_EQUAL: ['user.role', 'admin'] },
    THEN: ['create', 'read', 'update', 'delete'],
    ELSE: ['read'],
  },
}
```

---

## Design Summary

- **Name:** `ValueLogic<STATE, T>` (parallels `BoolLogic<STATE>`)
- **Two variants:** `IF/THEN/ELSE` (nestable for elif chains) and `MATCH` (multi-way switch)
- **Conditions reuse `BoolLogic` trees** — no new condition evaluator
- **THEN/ELSE/CASES values are static JSON** — not derived from state
- **Config discriminator:** `value_logic` key (like `condition` for BoolLogic)
- **Evaluated in WASM** with own registry and reverse dependency index
- **Fully additive** — no breaking changes to existing APIs

---

## TypeScript Type (`src/types/value-logic.ts` — NEW)

```typescript
export type ValueLogic<STATE, T = unknown> =
  | { IF: BoolLogic<STATE>; THEN: T; ELSE: T | ValueLogic<STATE, T> }
  | ValueLogicMatch<STATE, T>

type ValueLogicMatch<STATE, T> = {
  [P in DeepKey<STATE>]: {
    MATCH: P
    CASES: Partial<Record<`${Extract<DeepValue<STATE, P>, string | number>}`, T>>
    DEFAULT: T
  }
}[DeepKey<STATE>]
```

- `ELSE` accepts `T | ValueLogic<STATE, T>` for elif chaining
- `MATCH` uses distributed conditional type to resolve path value → strict union keys for `CASES`
- `CASES` is `Partial` — you don't have to cover all union members (DEFAULT handles the rest)
- Re-export from `src/types/index.ts`

### Usage Examples

```typescript
// Simple IF/THEN/ELSE — options list
const permissionOptions: ValueLogic<State, string[]> = {
  IF: { IS_EQUAL: ['user.role', 'admin'] },
  THEN: ['create', 'read', 'update', 'delete'],
  ELSE: ['read'],
}

// Chained conditions (elif) — label
const accessLabel: ValueLogic<State, string> = {
  IF: { IS_EQUAL: ['user.role', 'admin'] },
  THEN: 'Full Access',
  ELSE: {
    IF: { IS_EQUAL: ['user.role', 'editor'] },
    THEN: 'Edit Access',
    ELSE: 'Read Only',
  },
}

// MATCH — multi-way switch on a path's value
const roleOptions: ValueLogic<State, string[]> = {
  MATCH: 'user.role',
  CASES: {
    admin: ['create', 'read', 'update', 'delete'],
    editor: ['read', 'update'],
    viewer: ['read'],
  },
  DEFAULT: ['read'],
}
```

### Concern Registration

```typescript
useConcerns('my-reg', {
  'action.type': {
    // Existing BoolLogic concern (returns boolean)
    disabledWhen: {
      condition: { IS_EQUAL: ['user.role', 'viewer'] },
    },
    // NEW: ValueLogic concern (returns string[])
    options: {
      value_logic: {
        MATCH: 'user.role',
        CASES: {
          admin: ['create', 'read', 'update', 'delete'],
          editor: ['read', 'update'],
        },
        DEFAULT: ['read'],
      },
    },
  },
})
```

---

## Rust Implementation (`rust/src/value_logic.rs` — NEW)

### Enums

```rust
#[serde(untagged)]
enum ValueLogicNode {
    IfThenElse { IF: BoolLogicNode, THEN: Value, ELSE: Box<ValueLogicElse> },
    Match { MATCH: String, CASES: HashMap<String, Value>, DEFAULT: Value },
}

#[serde(untagged)]
enum ValueLogicElse {
    Nested(ValueLogicNode),  // tried first by serde
    Literal(Value),          // fallback
}
```

### Evaluation

- `IfThenElse`: evaluate `BoolLogicNode` condition against shadow state → return THEN or recurse into ELSE
- `Match`: get path value from shadow state → convert to string key → lookup in CASES → fallback to DEFAULT

### Path Extraction

- `IfThenElse`: delegate to `BoolLogicNode::extract_paths()` + recurse nested ELSE
- `Match`: collect the MATCH path

### Registry

- `ValueLogicRegistry` — same pattern as `BoolLogicRegistry` (`HashMap<u32, ValueLogicMetadata>`)
- Own `ReverseDependencyIndex` (separate from BoolLogic's, same pattern as validators)
- `register()` interns paths, adds to reverse dependency index
- `unregister()` cleans up registry and index

---

## Pipeline Integration (`rust/src/pipeline.rs` — MODIFY)

### New fields on `ProcessingPipeline`

- `value_logic_registry: ValueLogicRegistry`
- `value_logic_rev_index: ReverseDependencyIndex`
- `buf_affected_value_logics: HashSet<u32>`

### `mark_affected_logic()` — add third check

After BoolLogic and function rev index checks:

```rust
for id in self.value_logic_rev_index.affected_by_path(path_id) {
    self.buf_affected_value_logics.insert(id);
}
```

### `prepare_changes()` — evaluate after BoolLogic (step 8b)

```rust
for logic_id in &self.buf_affected_value_logics {
    if let Some(meta) = self.value_logic_registry.get(*logic_id) {
        let result = meta.tree.evaluate(&self.shadow);
        self.buf_concern_changes.push(Change {
            path: meta.output_path.clone(),
            value_json: serde_json::to_string(&result).unwrap_or("null".into()),
        });
    }
}
```

Results flow into same `buf_concern_changes` buffer → same finalize path. No special handling needed.

### Extend `ConcernsRegistration` / `ConcernsResult`

```rust
// Registration input (serde(default) for backward compat)
pub value_logics: Vec<ValueLogicRegistration>,

// Registration output
pub value_logic_changes: Vec<Change>,
pub registered_value_logic_ids: Vec<u32>,
```

### `register_concerns()` — add step 3

Register each ValueLogic, evaluate against current shadow state, return initial values.

---

## WASM Exports (`rust/src/lib.rs` — MODIFY)

- Add `mod value_logic;`
- Extend existing `register_concerns` / `unregister_concerns` (no new exports needed — ValueLogic piggybacks on the same batch registration)

---

## Bridge (`src/wasm/bridge.ts` — MODIFY)

Extend `ConcernsRegistration` interface:

```typescript
value_logics?: { output_path: string; tree_json: string }[]
```

Extend `ConcernsResult` interface:

```typescript
value_logic_changes: Change[]
registered_value_logic_ids: number[]
```

---

## JS Registration (`src/concerns/registration.wasm.ts` — MODIFY)

### Detection

```typescript
const isValueLogicConfig = (config: Record<string, unknown>) =>
  'value_logic' in config && config['value_logic'] != null
```

### Classification (in `collectRegistrations`)

Add before JS effect fallback:

```typescript
if (isValueLogicConfig(config)) {
  valueLogics.push({
    output_path: `${path}.${concernName}`,
    tree_json: JSON.stringify(config.value_logic),
  })
  continue
}
```

### Apply initial results

Same pattern as BoolLogic initial changes — parse path, set on `_concerns` proxy.

---

## Files Summary

| File | Action | What |
|------|--------|------|
| `src/types/value-logic.ts` | CREATE | TypeScript type definition |
| `rust/src/value_logic.rs` | CREATE | Rust enum, registry, evaluation, tests |
| `rust/src/lib.rs` | MODIFY | Add `mod value_logic;` |
| `rust/src/pipeline.rs` | MODIFY | Registry, rev index, buffers, mark/evaluate/register |
| `src/types/index.ts` | MODIFY | Re-export ValueLogic |
| `src/wasm/bridge.ts` | MODIFY | Extend registration/result interfaces |
| `src/concerns/registration.wasm.ts` | MODIFY | Detection, classification, initial apply |

---

## Verification

1. **Rust unit tests** in `value_logic.rs`:
   - Serde round-trip: IF/THEN/ELSE, nested ELSE, MATCH
   - Evaluation: simple IF, chained elif, MATCH with hit/miss/default
   - Path extraction: IF (delegates to BoolLogic), nested ELSE, MATCH
2. **WASM integration**: `npm run wasm:build` succeeds
3. **TypeScript**: `npm run code:fix && npm run code:check` passes
4. **Rust quality**: `npm run wasm:fmt && npm run wasm:lint && npm run wasm:check` passes
5. **End-to-end**: register a ValueLogic concern, change state, verify `_concerns` updates with correct value
