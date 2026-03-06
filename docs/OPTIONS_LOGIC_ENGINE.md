# OptionsLogic Engine — Dynamic Option Lists in WASM

**Status:** PLANNED
**Date:** 2026-03-01
**Author:** Architecture session

---

## Context

`BoolLogic` evaluates conditions → `boolean`. `ValueLogic` evaluates conditions → arbitrary JSON. `OptionsLogic` is a third WASM-evaluated concern type that evaluates conditions → **`OptionItem[]`** — a typed list of selectable options with per-item conditional inclusion.

**Why not ValueLogic?** ValueLogic returns a raw JSON blob. OptionsLogic adds two capabilities that require WASM awareness of the output *structure*:

1. **Auto-selection** — When the resolved list changes and the current state value is no longer in it, WASM automatically emits a state change to update the value (not a concern change).
2. **Flip cycling** — When a path in a flip pair has OptionsLogic registered, boolean negation is replaced with cycling forward through the option list.

Both behaviours require WASM to understand that the output is a list of selectable items, not opaque JSON.

**Relationship to ValueLogic:**

- OptionsLogic shares the same branch structure (IF/THEN/ELSE, MATCH, flat List)
- The difference is: branch values are `OptionItem[]` instead of `Value`, and each item may carry its own `when` BoolLogic guard
- The registry, reverse dependency index, and pipeline step follow the same pattern as `ValueLogicRegistry` / Step 8b

---

## User-Facing API

```typescript
store.useConcerns('my-concerns', {
  'user.role': {
    options: {
      optionsLogic: [
        { value: 'admin', label: 'Admin' },
        { value: 'viewer', label: 'Viewer', when: { IS_EQUAL: ['org.allowViewers', true] } },
      ],
      autoValidation: true,
    },
  },
  'plan.tier': {
    options: {
      optionsLogic: {
        IF: { IS_EQUAL: ['org.type', 'enterprise'] },
        THEN: [
          { value: 'enterprise', label: 'Enterprise' },
          { value: 'growth', label: 'Growth' },
        ],
        ELSE: [
          { value: 'basic', label: 'Basic' },
          { value: 'free', label: 'Free' },
        ],
      },
    },
  },
})

// Concern outputs:
// _concerns['user.role'].options        → OptionItem[] (filtered by `when`)
// _concerns['user.role'].optionIsValid  → boolean (if autoValidation: true)
```

---

## TypeScript Types (`src/types/options-logic.ts` — NEW)

```typescript
import type { BoolLogic } from './bool-logic'
import type { ResolvableDeepKey } from './deep-key'
import type { DeepValue } from './deep-value'

/** A single selectable option, optionally visible only when a condition is met. */
export type OptionItem<STATE, V = unknown> = {
  value: V
  label: string
  when?: BoolLogic<STATE>   // If present, item is excluded when condition is false
}

/**
 * OptionsLogic DSL for conditional option list selection.
 *
 * Three forms:
 * - Flat list: `OptionItem[]` — always returns this list (items may still be filtered by `when`)
 * - IF/THEN/ELSE: condition selects which list to return
 * - MATCH: path value selects which list to return
 */
export type OptionsLogic<STATE, V = unknown> =
  | OptionsLogicIfThenElse<STATE, V>
  | OptionsLogicMatch<STATE, V>
  | OptionItem<STATE, V>[]

/** IF/THEN/ELSE variant — nestable for elif chains */
export interface OptionsLogicIfThenElse<STATE, V> {
  IF: BoolLogic<STATE>
  THEN: OptionItem<STATE, V>[]
  ELSE: OptionItem<STATE, V>[] | OptionsLogic<STATE, V>
}

/** MATCH variant — multi-way switch on a state path value */
export type OptionsLogicMatch<STATE, V> = {
  [P in ResolvableDeepKey<STATE>]: {
    MATCH: P
    CASES: Partial<Record<`${Extract<DeepValue<STATE, P>, string | number>}`, OptionItem<STATE, V>[]>>
    DEFAULT: OptionItem<STATE, V>[]
  }
}[ResolvableDeepKey<STATE>]
```

Re-export from `src/types/index.ts`:

```typescript
export type * from './options-logic'
```

---

## Concern Config (`src/types/concerns.ts` — MODIFY)

Add `options` case to `ConcernConfigFor`:

```typescript
// In ConcernConfigFor — add alongside BoolLogic and validationState branches
: C extends { name: 'options' }
  ? { optionsLogic: OptionsLogic<DATA, Depth>; autoValidation?: boolean }
  : ...
```

---

## Rust: `rust/src/options_logic.rs` (NEW)

### Data Model

```rust
/// A single selectable option item with optional BoolLogic visibility guard.
pub struct OptionItemNode {
    pub value: Value,
    pub label: String,
    pub when: Option<BoolLogicNode>,
}

/// The ELSE branch: either a nested OptionsLogicNode or a flat item list.
/// Items variant is tried first by serde so flat arrays are never parsed as Nested.
#[serde(untagged)]
pub enum OptionsLogicElse {
    Items(Vec<OptionItemNode>),        // flat list — tried first
    Nested(Box<OptionsLogicNode>),     // elif chain
}

/// Top-level OptionsLogic expression.
#[serde(untagged)]
pub enum OptionsLogicNode {
    IfThenElse { condition: BoolLogicNode, then_items: Vec<OptionItemNode>, else_items: OptionsLogicElse },
    Match { path: String, cases: HashMap<String, Vec<OptionItemNode>>, default: Vec<OptionItemNode> },
    List(Vec<OptionItemNode>),
}
```

### Evaluation — Two Passes

`evaluate(shadow) → Vec<OptionItemNode>`:

1. **Branch selection**: pick the correct item list via IF/THEN/ELSE, MATCH case/DEFAULT, or the flat List
2. **Item filtering**: for each item in the selected list, evaluate its optional `when` guard — include only items whose guard is absent or evaluates to `true`

```
evaluate(shadow):
  items = branch_select(shadow)            // pass 1: pick list
  return items.filter(i => i.when.map(w => w.evaluate(shadow)).unwrap_or(true))
```

### Path Extraction

`extract_paths() → Vec<String>` — collects every path the expression depends on for the reverse dependency index:

| Source | What is collected |
|---|---|
| IF condition | `condition.extract_paths()` |
| THEN items | `item.when.extract_paths()` for each item with a guard |
| ELSE items (all variants) | same — recurse into nested OptionsLogicNode or scan list |
| MATCH path | the MATCH path string |
| All CASES items | `item.when.extract_paths()` for guarded items |
| **Target path** | the `output_path` itself (e.g. `user.role`) — so auto-select re-runs when the current value changes |

The target path inclusion is the key difference from ValueLogic: the auto-selection check reads `shadow[targetPath]`, so it must be in the reverse index.

### Registry

```rust
pub(crate) struct OptionsLogicMetadata {
    pub output_path: String,         // e.g. "user.role.options"
    pub target_path: String,         // e.g. "user.role"  (output_path.rsplit_once('.').0)
    pub target_path_id: u32,         // interned ID for O(1) flip lookup
    pub tree: OptionsLogicNode,
    pub auto_validation: bool,
    pub anchor_path_id: Option<u32>,
    pub registration_id: Option<String>,
}

pub(crate) struct OptionsLogicRegistry {
    logics: HashMap<u32, OptionsLogicMetadata>,
    by_target_path: HashMap<u32, u32>,   // target_path_id → logic_id (for flip cycling)
    next_id: u32,
}
```

`by_target_path` enables O(1) lookup in `process_flip_paths_into` — given a flip peer's path ID, the pipeline can immediately check whether OptionsLogic is registered there instead of scanning all logics.

---

## Pipeline Integration (`rust/src/pipeline.rs` — MODIFY)

### New fields on `ProcessingPipeline`

```rust
options_logic_registry: OptionsLogicRegistry,
options_logic_rev_index: ReverseDependencyIndex,
// PipelineContext also gets:
affected_options_logics: HashSet<u32>,
```

### `ConcernsRegistration` / `ConcernsResult` extensions

```rust
// Input
pub options_logics: Vec<OptionsLogicRegistration>,
// where: { output_path: String, tree_json: String, auto_validation: bool }

// Output
pub options_logic_changes: Vec<Change>,           // → _concerns[path].options
pub registered_options_logic_ids: Vec<u32>,
pub options_auto_select_changes: Vec<Change>,     // → state[targetPath]  (applied by JS)
```

### `register_concerns()` — Step 4 (after ValueLogic)

For each `OptionsLogicRegistration`:

1. Parse tree from `tree_json`
2. Derive `target_path = output_path.rsplit_once('.').0`
3. Intern `target_path` and add to reverse index (so `state[targetPath]` changes re-trigger)
4. Register in `options_logic_registry` (stores `target_path_id` in `by_target_path`)
5. Evaluate → emit concern change `{ path: output_path, value: OptionItem[] }`
6. **Auto-select check**: compare `shadow[target_path]` against resolved list:
   - Value absent from list **and** list non-empty → `options_auto_select_changes.push({ path: target_path, value: list[0].value })`
   - Value absent from list **and** list empty → `options_auto_select_changes.push({ path: target_path, value: UNDEFINED_SENTINEL })`
   - Value present in list → no auto-select change
7. If `auto_validation`: emit concern change `{ path: "target.optionIsValid", value: bool }`

### `unregister_concerns()` — Step 4 cleanup

Call `options_logic_registry.unregister_by_registration(registration_id)` which also cleans up `by_target_path` entries.

### `mark_affected_logic()` — add third rev index check

```rust
// After existing BoolLogic + function rev index checks:
for ol_id in self.options_logic_rev_index.affected_by_path(path_id) {
    self.ctx.affected_options_logics.insert(ol_id);
}
```

### Pipeline Step 8c (new — after Step 8b ValueLogic)

```
for ol_id in ctx.affected_options_logics:
    meta = options_logic_registry.get(ol_id)
    if !is_anchor_enabled(meta.anchor_path_id): skip

    resolved = meta.tree.evaluate(&working)  // Vec<OptionItemNode>
    concern_changes.push({ path: meta.output_path, value: resolved as JSON })

    current = working.get(&meta.target_path)
    if current not in resolved.values():
        if resolved.non_empty():
            buf_pending_state_changes.push({ path: target_path, value: resolved[0].value })
        else:
            buf_pending_state_changes.push({ path: target_path, value: UNDEFINED_SENTINEL })

    if meta.auto_validation:
        valid = resolved.any(i => i.value == current)
        concern_changes.push({ path: "target.optionIsValid", value: valid })
```

State changes from Step 8c go into `buf_pending_state_changes` so they flow through `pipeline_finalize` as normal state changes — same path as all other produced state changes.

### `process_flip_paths_into()` — flip cycling override

In the exact-match branch, before the boolean-invert check:

```rust
// Check if this peer path has OptionsLogic registered
if let Some(ol_meta) = self.options_logic_registry.get_by_target_path_id(peer_id) {
    let resolved = ol_meta.tree.evaluate(&self.shadow);
    if !resolved.is_empty() {
        let current_val = self.shadow.get(&ol_meta.target_path)
            .map(|r| r.to_json_value());
        let idx = current_val.and_then(|cv|
            resolved.iter().position(|item| item.value == cv)
        );
        let next_idx = idx.map(|i| (i + 1) % resolved.len()).unwrap_or(0);
        let next_value_json = serde_json::to_string(&resolved[next_idx].value)
            .unwrap_or_else(|_| "null".to_owned());
        // emit state change: { path: target_path, value: next_value_json }
        output.push(Change { path: ol_meta.target_path.clone(), value_json: next_value_json, ... });
    }
    // Always continue — skip boolean invert even for empty list (no-op)
    continue;
}
// else: normal boolean invert path
```

---

## Relationship to ValueLogic — Side-by-Side

| Aspect | ValueLogic | OptionsLogic |
|---|---|---|
| Branch result type | `Value` (any JSON) | `Vec<OptionItemNode>` |
| Per-item guards | — | `when?: BoolLogicNode` per item (filter pass) |
| Auto-selection | — | WASM auto-emits state change when value not in list |
| Flip peer override | — | Cycles forward through list instead of boolean invert |
| `autoValidation` | — | Emits `optionIsValid` concern |
| Rev index target path | only condition paths | condition paths **+ target path** |
| Registry extra field | — | `by_target_path: HashMap<u32, u32>` for O(1) flip lookup |
| Concern output key | user-chosen name | `options` (the concern name drives it) |
| Pipeline step | Step 8b | Step 8c (after 8b) |
| Registration result | `value_logic_changes` | `options_logic_changes` + `options_auto_select_changes` |

---

## TypeScript Files Summary

| File | Action | What |
|---|---|---|
| `src/types/options-logic.ts` | CREATE | `OptionItem`, `OptionsLogic`, `OptionsLogicIfThenElse`, `OptionsLogicMatch` |
| `src/types/index.ts` | MODIFY | `export type * from './options-logic'` |
| `src/types/concerns.ts` | MODIFY | Add `options` case to `ConcernConfigFor` |
| `src/concerns/prebuilts/options.ts` | CREATE | JS fallback `options` concern (evaluates via `evaluateBoolLogic` helpers) |
| `src/concerns/prebuilts/index.ts` | MODIFY | Add `options` to exports, `prebuilts`, `prebuiltsNamespace` |
| `src/concerns/registration.wasm-impl.ts` | MODIFY | `isOptionsLogicConfig`, `optionsLogics` bucket, classify + pass to WASM, apply `options_logic_changes` + `options_auto_select_changes` |
| `src/wasm/bridge.ts` | MODIFY | Pass `options_logics` in `registerConcerns`; return `options_logic_changes`, `registered_options_logic_ids`, `options_auto_select_changes` |

> `src/wasm/generated/types.ts` is auto-generated by `ts-rs` — updates automatically after `npm run wasm:build`. No manual edits.

---

## Rust Files Summary

| File | Action | What |
|---|---|---|
| `rust/src/options_logic.rs` | CREATE | `OptionItemNode`, `OptionsLogicNode`, `OptionsLogicElse`, `OptionsLogicRegistry`, `evaluate()`, `extract_paths()` |
| `rust/src/lib.rs` | MODIFY | `pub mod options_logic;` |
| `rust/src/pipeline.rs` | MODIFY | Registry fields, `ConcernsRegistration`/`ConcernsResult` extensions, Step 4 registration, Step 8c evaluation, flip cycling override, `mark_affected_logic`, `unregister_concerns` cleanup |

---

## Auto-Select: Registration vs Pipeline

Auto-selection emits a **state change** (not a concern change) — it writes to `state[targetPath]` to correct the selected value when the available options shift.

| When | How |
|---|---|
| Registration time | `options_auto_select_changes` field in `ConcernsResult` — JS applies via `dot.set__unsafe` to `store.state` |
| Pipeline run (Step 8c) | pushed to `buf_pending_state_changes` — surfaces via normal `pipeline_finalize` `state_changes` flow |

This means auto-selected values flow through the full pipeline: they trigger further BoolLogic/ValueLogic/OptionsLogic re-evaluation if other expressions depend on `targetPath`.

---

## `autoValidation` — `optionIsValid` Output

When `autoValidation: true`, WASM emits an additional concern change alongside the `options` list:

```
_concerns[targetPath].optionIsValid → boolean
```

- `true` when `state[targetPath]` is present in the resolved list
- `false` when it is not (includes the case where the list is empty)

This is stored at `optionIsValid` (not `validationState`) to avoid colliding with Zod-based validation that may also be registered on the same path.

---

## Verification

```bash
npm run wasm:fmt && npm run wasm:lint && npm run wasm:check
npm run wasm:build
npm run code:fix && npm run code:check
npx vitest run --reporter=vitest-llm-reporter 2>&1 | jq '.failures'
```

Integration test cases (single file `tests/concerns/options-logic.test.ts`):

| # | Case |
|---|---|
| 1 | Flat list — all items included (no `when`) |
| 2 | Flat list with `when` — some items excluded by guard |
| 3 | IF/THEN/ELSE — different list returned per state |
| 4 | MATCH — multi-way selection |
| 5 | Auto-select at registration — value not in list, selects first item |
| 6 | Auto-select on pipeline run — list changes, current value invalidated |
| 7 | Auto-select to undefined when resolved list is empty |
| 8 | Target path change re-triggers auto-select check |
| 9 | Flip cycles forward through option list |
| 10 | Flip on empty list is a no-op |
| 11 | `autoValidation: true` → `optionIsValid` false when value not in list |
