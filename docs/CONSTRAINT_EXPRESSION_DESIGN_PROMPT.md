# Expression Language for LogicNode — Design Spec

**Status:** DECIDED — ready for implementation planning
**Date:** 2026-03-05
**Prerequisite:** `docs/REACTOR_ARCHITECTURE_PROPOSAL.md` (LogicReactor, unified LogicNode)

---

## Context

The WASM pipeline processes state changes through ordered reactors:

```
Clear → Sync → Flip → Derived → Logic → Listeners → Validators
```

`LogicNode` (unified from BoolLogic + ValueLogic) currently supports:

- `IfThenElse` — condition → select **static** JSON value
- `Match` — path lookup → select **static** JSON value

**Key limitation:** `THEN/ELSE/CASES` hold static JSON. We need expressions that read **live values from other paths** and compute results.

---

## Use Cases (FX Options Trading)

### 1. Options list constraint

Logic produces `_concerns.action.type.options = ['read', 'update']` based on `user.role`. If `state.action.type = 'delete'` (no longer valid), auto-reset to first valid option.

### 2. Currency conversion

When spot rate or premium changes, recalculate premium in alt currency: `premium * spot`, rounded to pip precision.

### 3. Numeric clamping

`state.notional` must stay within `[minLot, maxLot]` per currency pair. When bounds change, clamp the value.

### 4. Dynamic table lookup

Fee rate depends on `trade.feeType` + `trade.feeQuantity` — look up from nested config map at runtime.

### 5. Fallback chains

Use preferred settlement currency, fall back to trade currency, fall back to base currency.

### 6. Conditional default

When `state.currency` changes, reset `state.amount` to a default value looked up from a rate table in state.

---

## Design Decisions

1. **Expression language** — small set of composable functions referencing live paths, extending LogicNode. Not Turing-complete (no loops, no variables, no recursion — tree eval only).
2. **Single-pass pipeline** — Logic reads `state.*`, writes to concerns/state. No re-entry. If a constraint corrects a value, re-evaluation happens on the next `processChanges` call from JS.
3. **Flip stays separate** — Flip has unique pre-mutation shadow semantics (`pre_shadow`). Cycle is an imperative JS action, not a reactive expression.
4. **CYCLE dropped** — it's an action (imperative), not a derivation (declarative). JS helper function handles cycling through dynamic lists.
5. **Null propagation** — failed lookups, divide by zero, missing keys all return `null`. `COALESCE` handles fallbacks. Clean and composable.
6. **GET is flat** — `{ GET: [root, key1, key2, ...] }` chains left-to-right, no nested GET calls needed.
7. **Two authoring modes** — JSON literals (with `satisfies`) for simple cases, builder API for type-safe complex expressions. Both produce the same JSON for WASM.
8. **Options + constraint unified** — `constrain` field on options concerns co-locates the options list and auto-correction. No separate `autoCorrect` concern, no cross-concern `_concerns.*` references.
9. **Corrections are trace-only** — no concern metadata for corrections. The state value reflects the correction, the pipeline trace logs it for debugging. Keeps concern types clean (`options` is just `T[]`).

---

## Core Type: `Expr`

### Rust representation

```rust
/// An expression that evaluates to a JSON value.
/// Can be a literal, a live path read, or a function call.
#[serde(untagged)]
enum Expr {
    Fn(ExprFn),                // try named function first
    PathRef { PATH: String },  // live path read from shadow
    Literal(Value),            // static JSON (backward compatible fallback)
}

enum ExprFn {
    // Data access
    Get(Vec<Expr>),                  // [root, key1, key2, ...] → chained lookup
    Coalesce(Vec<Expr>),             // first non-null

    // Numeric
    Add(Vec<Expr>),                  // sum
    Subtract([Box<Expr>; 2]),        // a - b
    Multiply(Vec<Expr>),             // product
    Divide([Box<Expr>; 2]),          // a / b (null if b=0)
    Negate(Box<Expr>),               // -x
    Abs(Box<Expr>),                  // |x|
    Round([Box<Expr>; 2]),           // [value, precision]
    Clamp([Box<Expr>; 3]),           // [value, min, max]
    Min(Vec<Expr>),                  // smallest
    Max(Vec<Expr>),                  // largest
    Length(Box<Expr>),               // array/string length

    // String
    Concat(Vec<Expr>),               // string concatenation
}
```

### How LogicNode changes

```rust
// Before: THEN/ELSE hold static serde_json::Value
IfThenElse { IF: BoolLogicNode, THEN: Value, ELSE: Box<LogicElse> }

// After: THEN/ELSE hold Expr (can be static Value OR live computation)
IfThenElse { IF: BoolLogicNode, THEN: Expr, ELSE: Box<LogicElse> }

// New variant: direct expression (no condition needed)
Compute(Expr)
```

Backward compatible — `Expr::Literal(Value)` deserializes identically to current static JSON via serde `untagged`.

### Evaluation

```rust
impl Expr {
    fn evaluate(&self, shadow: &ShadowState, intern: &InternTable) -> Value {
        match self {
            Expr::Literal(v) => v.clone(),
            Expr::PathRef { PATH } => shadow.get_json(PATH, intern).unwrap_or(Value::Null),
            Expr::Fn(f) => f.evaluate(shadow, intern),
        }
    }

    /// Collect all path references for reverse dependency index
    fn extract_paths(&self) -> Vec<String> {
        match self {
            Expr::Literal(_) => vec![],
            Expr::PathRef { PATH } => vec![PATH.clone()],
            Expr::Fn(f) => f.extract_paths(), // recurse into all args
        }
    }
}
```

### GET evaluation (flat chain)

```rust
// GET([root, key1, key2, ...]) — evaluate left to right
fn eval_get(args: &[Expr], shadow: &ShadowState, intern: &InternTable) -> Value {
    let mut current = args[0].evaluate(shadow, intern);
    for key_expr in &args[1..] {
        let key = match key_expr.evaluate(shadow, intern) {
            Value::String(s) => s,
            Value::Number(n) => n.to_string(),
            _ => return Value::Null, // non-string key → null
        };
        current = match current {
            Value::Object(map) => map.get(&key).cloned().unwrap_or(Value::Null),
            Value::Array(arr) => key.parse::<usize>().ok()
                .and_then(|i| arr.get(i).cloned())
                .unwrap_or(Value::Null),
            _ => return Value::Null, // can't index into non-container → null
        };
    }
    current
}
```

---

## TypeScript Types

### `DeepKeyByValue<STATE, V>` — filtered paths

```typescript
type DeepKeyByValue<T, V, Prefix extends string = ''> = T extends Record<string, unknown>
  ? {
      [K in keyof T & string]: T[K] extends V
        ? `${Prefix}${K}`
        : T[K] extends Record<string, unknown>
          ? DeepKeyByValue<T[K], V, `${Prefix}${K}.`>
          : never
    }[keyof T & string]
  : never
```

### `Expr<STATE, T>` — type-safe expressions

```typescript
type Expr<STATE, T = unknown> =
  | T                                                         // static literal
  | { PATH: DeepKeyByValue<STATE, T> }                       // only paths returning T
  | ExprCommon<STATE, T>                                      // type-agnostic ops
  | (T extends number ? ExprNumeric<STATE> : never)           // numeric-only ops
  | (T extends string ? ExprString<STATE> : never)            // string-only ops

// Works for any output type
type ExprCommon<STATE, T> =
  | { IF: BoolLogic<STATE>; THEN: Expr<STATE, T>; ELSE: Expr<STATE, T> }
  | { MATCH: DeepKey<STATE>; CASES: Record<string, Expr<STATE, T>>; DEFAULT: Expr<STATE, T> }
  | { COALESCE: Expr<STATE, T>[] }
  | { GET: Expr<STATE, unknown>[] }    // type safety via builder or satisfies

// Only available when T extends number
type ExprNumeric<STATE> =
  | { ADD: Expr<STATE, number>[] }
  | { SUBTRACT: [Expr<STATE, number>, Expr<STATE, number>] }
  | { MULTIPLY: Expr<STATE, number>[] }
  | { DIVIDE: [Expr<STATE, number>, Expr<STATE, number>] }
  | { NEGATE: Expr<STATE, number> }
  | { ABS: Expr<STATE, number> }
  | { ROUND: [Expr<STATE, number>, number] }
  | { CLAMP: [Expr<STATE, number>, Expr<STATE, number>, Expr<STATE, number>] }
  | { MIN: Expr<STATE, number>[] }
  | { MAX: Expr<STATE, number>[] }
  | { LENGTH: Expr<STATE, unknown[]> }

// Only available when T extends string
type ExprString<STATE> =
  | { CONCAT: Expr<STATE, string>[] }
```

When `T = unknown` (no type specified), only `ExprCommon` operations are available. Specifying `Expr<State, number>` unlocks arithmetic ops. `Expr<State, string>` unlocks CONCAT.

---

## Two Authoring Modes

### Mode 1: JSON literals (simple, declarative)

```typescript
useConcerns('trade-logic', {
  'trade.premium': {
    altCcyValue: {
      logic: {
        ROUND: [
          { MULTIPLY: [{ PATH: 'trade.premium' }, { PATH: 'marketData.spot' }] },
          2
        ]
      } satisfies Expr<State, number>,
    },
  },
})
```

`satisfies` validates the outer type. Dynamic GET chains rely on runtime WASM validation.

### Mode 2: Builder API (type-safe, programmatic)

```typescript
import { expr } from 'apex-state'

const e = expr<State>()

// Simple path read
e.path('trade.premium')                              // Expr<State, number>

// Arithmetic with inference
e.round(
  e.multiply(e.path('trade.premium'), e.path('marketData.spot')),
  2
)                                                      // Expr<State, number>

// Chained GET with type peeling
e.get('config.fees')
  .key('trade.feeType')
  .key('trade.feeQuantity')                            // Expr<State, number>

// Conditional
e.if({ IS_EQUAL: ['trade.ccyPair', 'EURUSD'] })
  .then(e.path('config.eurOptions'))
  .else(['read'])                                      // Expr<State, string[]>

// Coalesce
e.coalesce(
  e.get('marketData.rates').key('trade.ccyPair'),
  e.path('trade.manualRate'),
  1.0
)                                                      // Expr<State, number>
```

The builder produces the same JSON that WASM consumes. It's a TypeScript-only layer — no WASM boundary impact.

---

## Examples: FX Options Trading

### Premium conversion

```jsonc
// _concerns.trade.premiumAlt = premium * spot, rounded to 2dp
{
  "ROUND": [
    { "MULTIPLY": [{ "PATH": "trade.premium" }, { "PATH": "marketData.spot" }] },
    2
  ]
}
```

### Fee rate lookup (nested map, flat GET)

```jsonc
// state.config.fees = { DOM: { PERCENT: 0.05, FIXED: 100 }, INT: { ... } }
// Lookup: fees[feeType][feeQuantity], fallback to 0
{
  "COALESCE": [
    { "GET": [{ "PATH": "config.fees" }, { "PATH": "trade.feeType" }, { "PATH": "trade.feeQuantity" }] },
    0
  ]
}
```

GET evaluation step by step:

1. `{ PATH: 'config.fees' }` → `{ DOM: { PERCENT: 0.05, ... }, INT: { ... } }`
2. `{ PATH: 'trade.feeType' }` → `"DOM"` → lookup → `{ PERCENT: 0.05, FIXED: 100 }`
3. `{ PATH: 'trade.feeQuantity' }` → `"PERCENT"` → lookup → `0.05`

### Notional clamping

```jsonc
// Clamp notional to valid lot range
{
  "CLAMP": [
    { "PATH": "trade.notional" },
    { "GET": [{ "PATH": "config.lotSizes" }, { "PATH": "trade.ccyPair" }, "min"] },
    { "GET": [{ "PATH": "config.lotSizes" }, { "PATH": "trade.ccyPair" }, "max"] }
  ]
}
```

Note: `"min"` and `"max"` are static string keys (no PATH wrapper), mixed with dynamic keys.

### Fallback chain

```jsonc
// Use best available currency
{ "COALESCE": [{ "PATH": "user.preferredCcy" }, { "PATH": "trade.settleCcy" }, { "PATH": "trade.baseCcy" }, "USD"] }
```

### Options with auto-correction (unified `constrain`)

Options and value constraint are defined together — no separate auto-correct concern, no cross-concern `_concerns.*` references.

```typescript
// Simple values — constrain: true uses deep equality
'trade.tenor': {
  options: {
    logic: {
      MATCH: 'trade.ccyPair',
      CASES: { EURUSD: ['1W', '1M', '3M', '6M', '1Y'], USDJPY: ['1M', '3M', '1Y'] },
      DEFAULT: ['1M', '3M'],
    } satisfies Expr<State, string[]>,
    constrain: true,
    // Produces:
    //   _concerns.trade.tenor.options = ['1M', '3M', '1Y']
    //   state.trade.tenor = '1M' (if current '2Y' not in list)
    //   _concerns.trade.tenor.constrained = { corrected: true, from: '2Y', to: '1M' }
  },
}

// Object list — constrain: { by: 'value' } matches by key field
'trade.instrument': {
  options: {
    logic: {
      MATCH: 'trade.ccyPair',
      CASES: {
        EURUSD: [{ value: 1, label: 'Spot' }, { value: 2, label: 'Forward' }],
        USDJPY: [{ value: 2, label: 'Forward' }, { value: 3, label: 'Swap' }],
      },
      DEFAULT: [{ value: 1, label: 'Spot' }],
    },
    constrain: { by: 'value' },
    // Produces:
    //   _concerns.trade.instrument.options = [{ value: 2, ... }, { value: 3, ... }]
    //   state.trade.instrument = 2 (if current 1 not in options[*].value)
    //   _concerns.trade.instrument.constrained = { corrected: true, from: 1, to: 2 }
  },
}

// No constraint — options is just data for the component
'trade.strategy': {
  options: {
    logic: { ... },
    // No constrain field → no auto-correction, just produces options list
  },
}
```

---

## Registration

```typescript
useConcerns('trade-logic', {
  'trade.tenor': {
    // Options with auto-correction — unified
    options: {
      logic: {
        MATCH: 'trade.ccyPair',
        CASES: {
          EURUSD: ['1W', '1M', '3M', '6M', '1Y'],
          USDJPY: ['1M', '3M', '1Y'],
        },
        DEFAULT: ['1M', '3M'],
      } satisfies Expr<State, string[]>,
      constrain: true,  // ← auto-correct value + produce correction metadata
    },
  },
  'trade.instrument': {
    // Object list options with key-based constraint
    options: {
      logic: {
        MATCH: 'trade.ccyPair',
        CASES: {
          EURUSD: [{ value: 1, label: 'Spot' }, { value: 2, label: 'Forward' }],
          USDJPY: [{ value: 2, label: 'Forward' }, { value: 3, label: 'Swap' }],
        },
        DEFAULT: [{ value: 1, label: 'Spot' }],
      },
      constrain: { by: 'value' },  // ← match by option.value field
    },
  },
  'trade.premium': {
    // Computed concern (no constraint, just derived data)
    altCcyValue: {
      logic: {
        ROUND: [
          { MULTIPLY: [{ PATH: 'trade.premium' }, { PATH: 'marketData.spot' }] },
          2,
        ],
      } satisfies Expr<State, number>,
    },
  },
})
```

---

## Constrained Options: Output

When `constrain` is set on an options concern, WASM produces **two outputs** in one evaluation:

1. **Options list** — `_concerns.path.options = [...]` (component renders dropdown)
2. **State correction** — `state.path = correctedValue` (only if current value invalid)

No correction metadata concern — corrections are logged in the pipeline trace (debug mode). Components don't need to know whether a correction happened. The state value already reflects it, and the component reads `state.path` normally.

```typescript
// What the component sees:
const { options } = useConcerns('trade.tenor')
// options = ['1M', '3M', '6M']  — always present
// state.trade.tenor = '1M'       — already corrected if needed
```

### How WASM evaluates constrained options

```rust
fn evaluate_constrained_options(
    expr: &LogicNode,
    constrain: &ConstrainConfig,
    target_path: &str,         // e.g., "trade.tenor"
    concern_path: &str,        // e.g., "_concerns.trade.tenor"
    shadow: &ShadowState,
    intern: &InternTable,
    debug: bool,
    trace: &mut Option<StageTrace>,
) -> Vec<Change> {
    let mut changes = Vec::new();

    // 1. Evaluate options list
    let options = expr.evaluate(shadow, intern);
    changes.push(Change {
        path: format!("{}.options", concern_path),
        value_json: serde_json::to_string(&options).unwrap(),
        ..
    });

    // 2. Check current value against options
    let current = shadow.get_json(target_path, intern).unwrap_or(Value::Null);
    let corrected = match constrain {
        ConstrainConfig::Simple => eval_first_valid(&current, &options),
        ConstrainConfig::By(key) => eval_first_valid_by(&current, &options, key),
    };

    if corrected != current {
        // State correction
        changes.push(Change {
            path: target_path.to_owned(),
            value_json: serde_json::to_string(&corrected).unwrap(),
            ..
        });
        // Debug trace only — not a concern value
        if debug {
            if let Some(t) = trace {
                t.produced.push(ProducedChange {
                    path: target_path.to_owned(),
                    value: serde_json::to_string(&corrected).unwrap(),
                    source_path: Some(format!("constrain: {} → {}", current, corrected)),
                    registration_id: None,
                });
            }
        }
    }

    changes
}

// Constraint evaluation helpers
fn eval_first_valid(current: &Value, options: &Value) -> Value {
    match options {
        Value::Array(list) => {
            if list.contains(current) { current.clone() }
            else { list.first().cloned().unwrap_or(Value::Null) }
        }
        _ => Value::Null
    }
}

fn eval_first_valid_by(current: &Value, options: &Value, key: &str) -> Value {
    match options {
        Value::Array(list) => {
            if list.iter().any(|opt| opt.get(key) == Some(current)) {
                current.clone()
            } else {
                list.first()
                    .and_then(|opt| opt.get(key).cloned())
                    .unwrap_or(Value::Null)
            }
        }
        _ => Value::Null
    }
}
```

---

## TypeScript: `constrain` type

```typescript
// Constrain configuration — co-located with options
type Constrain<T> =
  | true                  // simple values — deep equality match
  | { by: keyof T }      // object list — match by key field (key's type must match state value)

// Concern registration with optional constraint
type ConcernWithLogic<STATE, T> = {
  logic: Expr<STATE, T> | Expr<STATE, T[]>
  constrain?: T extends unknown[] ? Constrain<T[number]> : never  // only on array outputs
}
```

The `constrain` field is only valid when `logic` produces an array (options list). TypeScript enforces this — you can't add `constrain` to a numeric expression.

---

## Builder API: `constrain` support

```typescript
const e = expr<State>()

// Builder for constrained options — type-safe chain
e.options('trade.tenor')                              // OptionsBuilder<State, string>
  .from({
    MATCH: 'trade.ccyPair',
    CASES: { EURUSD: ['1W', '1M', '3M'], USDJPY: ['1M', '3M'] },
    DEFAULT: ['1M'],
  })
  .constrain()                                        // simple deep equality
  .build()
// → { logic: ..., constrain: true }

// Object list with key-based constraint
e.options('trade.instrument')
  .from({ MATCH: 'trade.ccyPair', CASES: { ... }, DEFAULT: [...] })
  .constrain({ by: 'value' })                        // match by option.value
  .build()
// → { logic: ..., constrain: { by: 'value' } }

// No constraint — just options data
e.options('trade.strategy')
  .from({ ... })
  .build()                                            // no .constrain() → no auto-correction
// → { logic: ... }
```

---

## Open Questions

1. **Builder API scope**: Design the builder as part of the reactor refactor, or defer to a follow-up? The JSON representation is the core — builder is convenience.

2. **Multiple constraints per path**: Can a path have multiple constrained options concerns (e.g., `tenorOptions` and `strategyOptions` both constraining the same path)? If so, which correction wins?

---

## Reference Files

- `docs/REACTOR_ARCHITECTURE_PROPOSAL.md` — reactor architecture, LogicReactor design
- `docs/VALUE_LOGIC_ENGINE.md` — ValueLogic spec (now unified into LogicNode)
- `rust/src/value_logic.rs` — current ValueLogicNode implementation
- `rust/src/bool_logic.rs` — current BoolLogicNode (becomes condition-only type)
- `rust/src/pipeline.rs` — current pipeline, Logic evaluation (steps 8-9)
