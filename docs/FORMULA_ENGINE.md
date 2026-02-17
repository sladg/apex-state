# Formula Engine Architecture

Excel-like formula evaluation for FX Options deal fields, integrated into the WASM pipeline. Applicable to FX, equity, and cash asset classes.

---

## Overview

The formula engine lets users write expressions in trade fields (strike, expiry, notional, barrier) that reference other fields, perform arithmetic, aggregate across collections, and support tenor date math. Formulas re-evaluate reactively when dependencies change.

**Examples:**
```
= {{t1}}.strike * 2.5
= {{t1}}.strike + {{t2}}.strike
= MIN({{*.strike}})
= SUM({{g1.*.notional}}) / COUNT({{g1.*.notional}})
= {{t1}}.expiry + 2M
= {{template}}.strike + rollIndex * 0.05
```

---

## Domain Context

### State Shape

```
{
  dealSettings: { ... },
  groups: {
    [groupId]: {
      groupType,        // "strategy", "strip"
      strategyType,     // "straddle", "butterfly", etc.
      products: {
        [productId]: {
          type,          // "vanilla", "TARF", etc.
          strike, expiry, notional, barrier, ...
          legs: {        // TARF / complex products only
            [legId]: { strike, barrier, knockOut, ... }
          }
        }
      }
    }
  }
}
```

**Key structural facts:**
- Different product types have fields at **different nesting depths**: vanilla has `products.{id}.strike`, TARF has `products.{id}.legs.{n}.strike`
- **Strips** are groups with template trades. A strip specifies templates + roll parameters, then generates `templates x rolls` trades (e.g., 5 templates x 50 rolls = 250 trades). Delivery, expiry, and notional vary between rolls.
- All paths use dot-separated notation with UUID-based dynamic keys

---

## Architecture: JS / WASM Split

### WASM (Rust) — Full Formula Pipeline

| Component | Responsibility |
|-----------|---------------|
| **Parser** (nom) | Parses raw formula string into AST |
| **Alias table** | Bidirectional map: alias <-> absolute path. Registered from JS. |
| **Formula registry** | Stores parsed ASTs keyed by output path |
| **Dependency graph** | Directed graph for topological sort + cycle detection |
| **Wildcard resolver** | Traverses shadow state to find paths matching glob patterns |
| **Template engine** | Single AST x N target bindings with context variable substitution |
| **Evaluator** | Walks AST, reads shadow state values, produces results |

### TypeScript (JS) — User-Facing

| Component | Responsibility |
|-----------|---------------|
| **Alias generator** | Pluggable function that creates alias map from state structure |
| **Formula UX** | Input field, autocomplete, validation feedback display |
| **Field value storage** | `{ value, context?: { formula? } }` in valtio state |
| **Tenor resolution** | Sends tenor instructions to GraphQL backend for date resolution |

### Why Parse in WASM?

- **Single boundary crossing**: raw formula string in, evaluation results out
- **No alias round-trip**: WASM has the alias table, resolves during parsing
- **Bulk performance**: strips with 250 formulas parsed/evaluated entirely in Rust
- **JS stays thin**: just sends strings, receives resolved values

---

## Formula Language

### Syntax Grammar

```
formula     := '=' expression
expression  := term (('+' | '-') term)*
term        := factor (('*' | '/') factor)*
factor      := unary | literal | tenor | reference | contextVar
             | aggregate | '(' expression ')'
unary       := '-' factor
literal     := NUMBER                             // 1.5, 100, -3.14
tenor       := NUMBER TENOR_UNIT                  // 2D, 1M, 3Y, 2W
reference   := '{{' ALIAS '}}' ('.' FIELD)*       // {{t1}}.strike or {{t1.strike}}
contextVar  := IDENTIFIER                          // rollIndex, rollNumber, rollDate
aggregate   := AGG_FUNC '(' wildcardRef ')'
wildcardRef := '{{' WILDCARD_PATTERN '}}'          // {{*.strike}}, {{g1.*.strike}}
AGG_FUNC    := 'MIN' | 'MAX' | 'SUM' | 'AVG' | 'COUNT'
TENOR_UNIT  := 'D' | 'W' | 'M' | 'Y'
```

**Operator precedence** (standard math): `*` `/` before `+` `-`. Parentheses override.

**Formula delimiter**: leading `=` signals formula mode. Without it, the field holds a plain value.

### Reference System

Two alias modes, both supported:

| Syntax | Meaning | Example |
|--------|---------|---------|
| `{{alias.field}}` | Full-path alias | `{{t1.strike}}` resolves to `groups.abc.products.def.strike` |
| `{{alias}}.field` | Prefix alias + field access | `{{t1}}.strike` resolves to `groups.abc.products.def` + `.strike` |

Multiple references per formula: `{{t1}}.strike + {{t2}}.strike * 2` — each `{{...}}` resolved independently.

### Wildcard References

Used inside aggregate functions to reference collections across heterogeneous product types:

| Pattern | Scope | Matches |
|---------|-------|---------|
| `{{*.strike}}` | Entire deal | All paths ending in `.strike` at any depth |
| `{{g1.*.strike}}` | Group g1 | All `.strike` paths under group alias `g1` |
| `{{g1.p1.*.strike}}` | Product p1 | All `.strike` paths in legs/sub-structures of product `p1` |

Wildcards traverse shadow state recursively, handling different nesting depths naturally. A TARF's `legs.0.strike` and a vanilla's `strike` are both found by `{{*.strike}}`.

**Wildcards are allowed in template formulas.** E.g., `= SUM({{*.strike}}) + rollIndex * 0.05` evaluates `N_targets × N_matches` per cycle — acceptable since WASM arithmetic is cheap and this is an explicit user choice.

### Context Variables (Strip Templates)

Available in template formulas for strips:

| Variable | Type | Description |
|----------|------|-------------|
| `rollIndex` | integer (0-based) | Position of this roll in the strip |
| `rollNumber` | integer (1-based) | Human-readable roll number |
| `rollDate` | date string | Date of this roll period |

---

## AST Structure

The AST crosses the JS->WASM boundary as JSON (for registration/validation) and lives in WASM as Rust enums.

```rust
enum FormulaNode {
    Literal(f64),
    Tenor { amount: i32, unit: TenorUnit },
    Reference(String),                     // absolute path (after alias resolution)
    WildcardRef { prefix: String, suffix: String },
    ContextVar(String),
    UnaryOp { op: UnaryOp, operand: Box<FormulaNode> },
    BinaryOp { op: BinaryOp, left: Box<FormulaNode>, right: Box<FormulaNode> },
    AggregateFunc { func: AggFunc, source: Box<FormulaNode> },
}

enum BinaryOp { Add, Sub, Mul, Div }
enum UnaryOp { Neg }
enum TenorUnit { Days, Weeks, Months, Years }
enum AggFunc { Min, Max, Sum, Avg, Count }
```

**Extensibility**: Phase 2 adds `ScalarFunc { name, args }` and `ComparisonOp`. Phase 3 adds `ExternalRef { provider, key }` (SPOT/FWD market data). New node types only — no structural changes to existing AST.

---

## Type System

The evaluator enforces type safety at runtime. Every value has a type: `number`, `date`, or `tenor`.

### Arithmetic Type Rules

| Left | Op | Right | Result | Notes |
|------|----|-------|--------|-------|
| number | `+` `-` `*` `/` | number | number | Standard f64 arithmetic |
| date | `+` | tenor | tenor instruction | Sent to backend for calendar resolution |
| date | `-` | tenor | tenor instruction | Sent to backend for calendar resolution |
| date | `-` | date | number (days) | Calendar day difference |
| tenor | `+` `-` | tenor | tenor | Combine: `2M + 1W` |
| tenor | `*` `/` | number | tenor | Scale: `2M * 3` → `6M` |
| number | `+` `-` `*` `/` | tenor | **ERROR** | "Cannot mix number and tenor" |
| date | `+` `-` `*` `/` | number | **ERROR** | "Cannot do arithmetic on dates directly" |
| date | `+` | date | **ERROR** | "Cannot add two dates" |
| date | `*` `/` | _any_ | **ERROR** | "Cannot multiply/divide dates" |

### Runtime Error Handling

| Error | Result |
|-------|--------|
| Division by zero | `value` becomes `null`, `formula.error` set: "Division by zero" |
| Type mismatch | `value` becomes `null`, `formula.error` set with descriptive message |
| Deleted reference | `value` becomes `null`, `formula.error` set: "Reference not found: path" |

---

## Evaluation

### Output Types

WASM evaluation produces two result types:

**1. Numeric result** — fully resolved:
```json
{ "type": "resolved", "value": 3.75 }
```

**2. Tenor instruction** — needs backend resolution:
```json
{
  "type": "tenor",
  "horizonDate": { "day": 15, "month": 3, "year": 2026 },
  "tenor": { "amount": 2, "unit": "M" }
}
```

WASM resolves the horizon date from shadow state (the referenced date field) and packages the instruction. JS sends it to the GraphQL backend for business day calendar resolution, then writes the resolved date back as a plain value change.

### Evaluation Rules

- **Reference**: read `.value` from shadow state at the absolute path (formula-capable fields always use uniform shape)
- **WildcardRef**: expand against shadow state (using cached expansion), collect all matching `.value` entries
- **AggregateFunc**: apply MIN/MAX/SUM/AVG/COUNT to the collected values
- **ContextVar**: substitute from the per-instance context (strip template evaluation)
- **Tenor + Reference**: if a reference resolves to a date and the other operand is a tenor, produce a tenor instruction
- **Arithmetic**: standard f64 operations, with type checking per the type rules above

---

## Value Storage (JS State)

Formula-capable fields use a **uniform shape** — always an object, regardless of whether a formula is active:

```ts
// Formula-capable field (always this shape):
strike: {
  value: 1.5,                              // the "real" value used by everything
  context?: {
    formula?: {
      raw: "= {{t1}}.strike * 2.5",        // original user input (for re-parsing)
      error: null                            // or "Circular dependency detected"
    }
  }
}

// Plain field (not formula-capable, unchanged):
type: "vanilla"
```

**Design decisions:**
- `.value` is always the canonical value used by the rest of the system (BoolLogic, rendering, sync/flip, other formulas)
- Shadow state reads `.value` for formula-capable fields — no unwrapping ambiguity
- `context.formula` is present only when the user has entered a formula
- When user clears a formula, `context.formula` is removed, `.value` keeps the last resolved value
- AST and dependency list live in WASM only — not duplicated in JS state
- JS can query deps on demand via `wasm.getFormulaDeps(outputPath)` for UI highlighting

---

## Alias System

Aliases are **pluggable** — the platform provides a generator function:

```ts
type AliasGenerator = (state: State) => Map<string, string>
// alias -> absolute path (or path prefix)
```

**Flow:**
1. Platform generates alias map from current state structure
2. JS sends alias map to WASM: `wasm.registerAliases(aliasMapJson)`
3. When user types a formula, WASM parses it and resolves aliases internally
4. When state structure changes (products added/removed), platform re-generates aliases and re-registers

**WASM never sees aliases at evaluation time** — they are resolved to absolute paths during parsing. The alias table is only used during `register_formula` and `validate_formula`.

**Formulas store raw input** (`= {{t1}}.strike * 2.5`) so they can be re-parsed against updated alias maps when structure changes.

### Open: Re-Parse Mechanism on Structural Changes

**Unresolved**: When state structure changes (products added/removed), all formulas with aliases pointing to changed paths need re-parsing. The mechanism is not yet specified:

- **Who triggers it?** JS (detects structural change, calls unregister + register for affected formulas) or WASM (detects during `process_changes`, re-parses internally)?
- **Cost**: O(N) re-parse for N affected formulas. For strips with 250 template formulas, this could be expensive if triggered frequently.
- **Mitigation options**: batch re-registration API, dirty-flag on alias table entries, or WASM-internal re-parse with cached ASTs that only update path references.

This needs a design decision before implementation.

---

## Wildcard Resolution

### Shadow State Traversal

For `WildcardRef { prefix: "groups.abc", suffix: "strike" }`:

1. Navigate shadow state to `groups.abc`
2. Recursively traverse all children at any depth
3. Collect all paths ending with `.strike`
4. Result: `["groups.abc.products.def.strike", "groups.abc.products.ghi.legs.0.strike", ...]`

This handles heterogeneous product types naturally — vanilla `strike` at depth 1 and TARF `legs.N.strike` at depth 3 are both found.

### Structural Invalidation (Caching)

Wildcard expansion is cached to avoid traversing shadow state on every evaluation cycle:

- **Cache**: `Map<WildcardPattern, Vec<AbsolutePath>>` stored in formula registry
- **Invalidation**: when `process_changes()` detects keys being added/removed in shadow state (structural change), invalidate cached expansions whose prefix overlaps
- **Value-only changes**: skip re-expansion, just re-evaluate formulas using the cached path set
- **Re-expansion**: only when the cache entry is marked dirty (structural change detected)

**Why not re-expand every cycle?** With 10 wildcard formulas across 250 strip trades, re-expansion means ~2500 path traversals per change. Structural invalidation reduces this to zero for the common case (value changes).

**Structural changes only come from user actions** (adding/removing products, legs), never from sync/flip or formula evaluation. The pipeline detects added/removed keys during shadow state update (step 2), marks wildcard caches dirty, and formulas re-expand in step 3 of the same `process_changes` call.

---

## Strip Template Formulas

Strips generate many trades from a small number of templates. Instead of registering 250 individual formulas, the engine uses **template formulas**.

### Concept

One AST, N target bindings, each with per-instance context:

```
Template formula: "= {{template}}.strike + rollIndex * 0.05"
                          ↓
Target 0: path="groups.strip1.products.trade001.strike", context={rollIndex: 0}
Target 1: path="groups.strip1.products.trade002.strike", context={rollIndex: 1}
...
Target 249: path="groups.strip1.products.trade250.strike", context={rollIndex: 249}
```

### Registration

```js
wasm.registerTemplateFormula(
  "groups.strip1.template",
  "= {{template}}.strike + rollIndex * 0.05",
  JSON.stringify([
    { path: "groups.strip1.products.trade001.strike", context: { rollIndex: 0, rollDate: "2026-03-15" } },
    { path: "groups.strip1.products.trade002.strike", context: { rollIndex: 1, rollDate: "2026-04-15" } },
    // ... 248 more
  ])
)
```

### Evaluation

WASM iterates all targets, evaluates the same AST with different `ContextVar` values. One topological sort pass, then N evaluations. No 250x AST storage or 250x registration overhead.

### When Rolls Change

Call `wasm.updateTemplateTargets(formulaId, newTargetsJson)` with the updated target list. WASM re-binds the existing AST to new targets.

### Evaluation Strategy

**Always eager** — all 250 trades evaluate every cycle. WASM is fast enough for numeric arithmetic (no lazy evaluation complexity, no stale aggregate values).

---

## Pipeline Integration

Formula evaluation runs **before sync/flip** — formula results may need to propagate through sync/flip paths (e.g., a computed strike that syncs across legs).

```
1. Aggregation writes          (distribute target -> sources)
2. Update shadow state          (apply changes, detect structural changes)
3. Formula evaluation           ← NEW (before sync/flip)
   a. Detect structural changes → mark dirty wildcard caches
   b. Find formulas whose deps intersect changed paths
   c. Topological sort affected formulas
   d. Detect cycles → set error on cyclic formulas, skip them
   e. Evaluate in topological order:
      - Regular formulas: evaluate AST against shadow state
      - Template formulas: batch-evaluate same AST for all N targets
   f. Each result becomes a change to the .value field
   g. Downstream formulas handled by topological ordering
   h. Update shadow state with formula results
4. Sync paths                   (propagate to peers — including formula results)
5. Flip paths                   (invert booleans for peers)
6. BoolLogic evaluation         (can depend on formula + synced results)
7. Dispatch plan                (listener routing)
```

**Why before sync/flip?** Formula results (e.g., a computed strike) may need to propagate to synced peers or trigger flip logic. Running formulas first ensures sync/flip sees final computed values.

**Why before BoolLogic?** BoolLogic conditions can depend on formula results (e.g., "disable field when `strike > 2.0`").

**Structural changes and formulas**: Structural changes (keys added/removed) are detected during step 2 (shadow state update). Since structural changes only come from user actions (never from sync/flip/formula evaluation), the pipeline can safely invalidate wildcard caches and re-expand in step 3 without risk of infinite loops.

---

## Cycle Detection

WASM maintains a directed dependency graph:

- **Node**: each formula's output path
- **Edge**: output path -> each dependency path
- **On registration**: add edges, check for cycles
- **On evaluation**: topological sort; if cycle detected:
  - Mark all formulas in the cycle with error: `"Circular dependency: g1.p1.strike -> g2.p1.expiry -> g1.p1.strike"`
  - Skip evaluation of cyclic formulas
  - Non-cyclic formulas evaluate normally
  - Cyclic formulas keep their last `.value` (stale but not crashed)

---

## Error Handling

### Validation at Input Time

Formula parsing + alias resolution happens when the user confirms input. Invalid formulas are rejected immediately:

| Error | When | Effect |
|-------|------|--------|
| Parse error | Malformed syntax | Rejected, field shows error |
| Unknown alias | `{{xyz}}` not in alias table | Rejected, field shows error |
| Zero wildcard matches | `{{g99.*.strike}}` matches nothing | Rejected, field shows error |
| Type error | `= {{t1}}.strike + 2M` (number + tenor) | Rejected, field shows error |

### Runtime Errors

| Error | When | Effect |
|-------|------|--------|
| Circular dependency | Detected during topo sort in `process_changes()` | `formula.error` set, `.value` keeps last value |
| Deleted reference | Referenced field removed after formula registered | `.value` becomes null, `formula.error` set |
| Division by zero | Denominator evaluates to 0 | `.value` becomes null, `formula.error` set |
| Type mismatch | Operand types incompatible at runtime | `.value` becomes null, `formula.error` set |

---

## WASM API

```rust
/// Register alias mappings (batch). Called when state structure changes.
/// Input: JSON object { alias: absolutePath, ... }
fn register_aliases(aliases_json: &str) -> Result<(), JsValue>

/// Register a formula. WASM parses raw string, resolves aliases, builds AST.
/// Returns formula_id for later unregistration.
fn register_formula(output_path: &str, raw_formula: &str) -> Result<u32, JsValue>

/// Register a template formula for strip trades.
/// targets_json: [{ "path": "...", "context": { "rollIndex": 0, ... } }, ...]
fn register_template_formula(
    template_path: &str,
    raw_formula: &str,
    targets_json: &str,
) -> Result<u32, JsValue>

/// Unregister a formula by ID.
fn unregister_formula(formula_id: u32)

/// Update strip targets when rolls change.
fn update_template_targets(formula_id: u32, targets_json: &str) -> Result<(), JsValue>

/// Query dependencies of a formula (for UI highlighting).
/// Returns JSON array of absolute paths.
fn get_formula_deps(output_path: &str) -> Result<String, JsValue>

/// Validate a formula without registering (for input-time validation).
/// Accepts optional output_path to check for cycles before registration.
/// Returns { "valid": true, "deps": [...] } or { "valid": false, "error": "..." }
fn validate_formula(raw_formula: &str, output_path: Option<&str>) -> Result<String, JsValue>
```

---

## Data Flow

### Formula Registration

```
User types "= {{t1}}.strike * 2.5" in strike field
  → JS calls wasm.registerFormula("groups.g1.products.p1.strike", "= {{t1}}.strike * 2.5")
  → WASM: parse with nom → resolve {{t1}} via alias table → build AST
  → WASM: type-check AST → extract deps → add to dependency graph → check cycles
  → WASM: return formula_id (or error)
  → JS stores { value: null, context: { formula: { raw: "= {{t1}}.strike * 2.5", error: null } } }
```

### Formula Evaluation (during processChanges)

```
State change: groups.g2.products.p2.strike.value = 1.5
  → process_changes receives change
  → Steps 1-2: aggregation, shadow update
  → Step 3: formula evaluation
    → Find formulas depending on "groups.g2.products.p2.strike"
    → Topo sort affected formulas
    → Evaluate: AST reads 1.5 from shadow state (.value), computes 1.5 * 2.5 = 3.75
    → Emit change: groups.g1.products.p1.strike.value = 3.75
    → Update shadow state with formula result
  → Steps 4-5: sync, flip (formula results propagate to peers)
  → Steps 6-7: BoolLogic, dispatch plan
  → Return all changes to JS
```

### Strip Template Evaluation

```
State change: groups.strip1.template.strike.value = 1.0
  → Step 3: find template formula depending on template.strike
  → Evaluate for all 250 targets:
    → Target 0: 1.0 + 0 * 0.05 = 1.0
    → Target 1: 1.0 + 1 * 0.05 = 1.05
    → Target 2: 1.0 + 2 * 0.05 = 1.10
    → ...
  → Emit 250 changes to .value fields
  → Steps 4-5: sync/flip propagate any synced formula results
```

---

## Function Catalog

Comprehensive list of operations for FX, equity, and cash trading platforms. Organized by implementation phase.

### Phase 1 — Core Arithmetic & Aggregation

| # | Function | Example | Use Case |
|---|----------|---------|----------|
| 1 | `+` `-` `*` `/` | `= {{t1}}.strike * 2` | Basic arithmetic |
| 2 | `MIN(...)` | `= MIN({{*.strike}})` | Lowest strike across legs |
| 3 | `MAX(...)` | `= MAX({{*.strike}})` | Highest strike across legs |
| 4 | `SUM(...)` | `= SUM({{g1.*.notional}})` | Total notional in group |
| 5 | `AVG(...)` | `= AVG({{*.strike}})` | Average strike |
| 6 | `COUNT(...)` | `= COUNT({{g1.*.notional}})` | Number of legs/products |
| 7 | Tenor math | `= {{t1}}.expiry + 2M` | Date offset |
| 8 | Unary `-` | `= -{{t1}}.strike` | Negation |
| 9 | Parentheses | `= ({{t1}}.strike + {{t2}}.strike) / 2` | Grouping |
| 10 | Context vars | `= rollIndex * 0.05` | Strip template variables |

### Phase 2 — Scalar Functions

**Math & Rounding:**

| # | Function | Signature | Use Case |
|---|----------|-----------|----------|
| 11 | `ROUND` | `ROUND(x, decimals)` | Rounding to pip precision: `ROUND(1.23456, 4)` → `1.2346` |
| 12 | `ROUNDUP` | `ROUNDUP(x, decimals)` | Always round up — conservative notional rounding |
| 13 | `ROUNDDOWN` | `ROUNDDOWN(x, decimals)` | Always round down — conservative premium rounding |
| 14 | `ABS` | `ABS(x)` | Absolute value for delta-neutral positioning |
| 15 | `FLOOR` | `FLOOR(x, significance)` | Round to lot size: `FLOOR(notional, 100000)` |
| 16 | `CEILING` | `CEILING(x, significance)` | Round up to lot size |
| 17 | `MOD` | `MOD(x, divisor)` | Remainder — useful for periodic adjustments |
| 18 | `POWER` / `^` | `POWER(x, n)` | Compounding: `= notional * POWER(1.05, rollIndex)` |
| 19 | `SQRT` | `SQRT(x)` | Square root — vol scaling |
| 20 | `LN` / `LOG` | `LN(x)` | Natural log — log returns |
| 21 | `EXP` | `EXP(x)` | Exponential — continuous compounding |

**Comparison & Logic:**

| # | Function | Signature | Use Case |
|---|----------|-----------|----------|
| 22 | `IF` | `IF(cond, then, else)` | Conditional: `= IF({{t1}}.strike > 1.5, {{t1}}.notional, 0)` |
| 23 | `>` `<` `>=` `<=` `=` `<>` | Comparison ops | Conditions inside `IF` |
| 24 | `AND` | `AND(a, b, ...)` | `= IF(AND(strike > 1.0, strike < 2.0), ...)` |
| 25 | `OR` | `OR(a, b, ...)` | Multiple conditions |
| 26 | `NOT` | `NOT(x)` | Negate condition |
| 27 | `IFERROR` | `IFERROR(expr, fallback)` | Graceful fallback if reference is null/error |
| 28 | `ISBLANK` | `ISBLANK(ref)` | Check if field is empty |
| 29 | `COALESCE` | `COALESCE(a, b, c)` | First non-null value |

**Extended Aggregation:**

| # | Function | Signature | Use Case |
|---|----------|-----------|----------|
| 30 | `MEDIAN` | `MEDIAN({{*.strike}})` | Outlier-resistant central value |
| 31 | `STDEV` | `STDEV({{*.strike}})` | Standard deviation across strikes |
| 32 | `PERCENTILE` | `PERCENTILE({{*.strike}}, 0.95)` | 95th percentile |
| 33 | `SUMIF` | `SUMIF({{*.notional}}, "> 100000")` | Conditional sum |
| 34 | `COUNTIF` | `COUNTIF({{*.strike}}, "> 1.5")` | Conditional count |
| 35 | `MAXIF` / `MINIF` | `MINIF({{*.strike}}, "> 0")` | Filtered extremes |
| 36 | `PRODUCT` | `PRODUCT({{*.factor}})` | Multiply all values |

**Strip/Structure Helpers:**

| # | Function | Signature | Use Case |
|---|----------|-----------|----------|
| 37 | `LINSPACE` | `LINSPACE(start, end, rollIndex, rollCount)` | Linear interpolation across rolls |
| 38 | `ACCRETE` | `ACCRETE(base, rate, rollIndex)` | `base * (1 + rate)^rollIndex` — accreting notional |
| 39 | `AMORTIZE` | `AMORTIZE(base, rate, rollIndex)` | Decreasing notional |
| 40 | `STEP` | `STEP(start, stepSize, rollIndex)` | `start + stepSize * rollIndex` |
| 41 | `LOOKUP` | `LOOKUP(rollIndex, [0,1,2], [100,200,300])` | Table lookup for irregular schedules |
| 42 | `PREV` | `PREV(field)` | Previous roll's value of same field |
| 43 | `NEXT` | `NEXT(field)` | Next roll's value |
| 44 | `CUMSUM` | `CUMSUM({{strip.*.notional}}, rollIndex)` | Cumulative sum up to current roll |

**Date Functions** (produce tenor instructions for backend resolution):

| # | Function | Signature | Use Case |
|---|----------|-----------|----------|
| 45 | `WORKDAY` | `WORKDAY(date, n)` | N business days forward (needs calendar) |
| 46 | `WORKDAY_CCY` | `WORKDAY_CCY(date, n, ccyPair)` | Business days per currency pair calendar |
| 47 | `EDATE` | `EDATE(date, months)` | Exact month offset |
| 48 | `EOMONTH` | `EOMONTH(date, months)` | End of month + N months |
| 49 | `IMM_DATE` | `IMM_DATE(date, nth)` | Nth IMM date — standard futures/options expiry |
| 50 | `DAYS` | `DAYS(end, start)` | Calendar day count between dates |
| 51 | `YEARFRAC` | `YEARFRAC(start, end, basis)` | Year fraction with day count convention (ACT/360, ACT/365, 30/360) |
| 52 | `TODAY` | `TODAY()` | Current business date |
| 53 | `SETTLE_DATE` | `SETTLE_DATE(tradeDate, ccyPair)` | T+N settlement date per pair convention |

### Phase 3 — Market Data & FX-Specific

**Market Data References** (resolve via backend GraphQL, not computed locally):

| # | Function | Signature | Use Case |
|---|----------|-----------|----------|
| 54 | `SPOT` | `SPOT(ccyPair)` | Current spot rate |
| 55 | `FWD` | `FWD(ccyPair, tenor)` | Forward rate |
| 56 | `FWD_PTS` | `FWD_PTS(ccyPair, tenor)` | Forward points only |
| 57 | `DEPO` | `DEPO(ccy, tenor)` | Deposit rate |
| 58 | `VOL` | `VOL(ccyPair, tenor, delta)` | Implied volatility |
| 59 | `SWAP_RATE` | `SWAP_RATE(ccy, tenor)` | Swap rate for a tenor |
| 60 | `SOFR` | `SOFR(tenor)` | Reference rate (SOFR, EURIBOR, etc.) |

**FX-Specific Calculations:**

| # | Function | Signature | Use Case |
|---|----------|-----------|----------|
| 61 | `CCY_CONVERT` | `CCY_CONVERT(amount, fromCcy, toCcy)` | Cross-currency conversion at spot |
| 62 | `PIP_VALUE` | `PIP_VALUE(ccyPair, notional)` | Value of 1 pip |
| 63 | `DELTA_EQUIV` | `DELTA_EQUIV(notional, delta)` | Delta-equivalent notional |
| 64 | `PREMIUM_PCT` | `PREMIUM_PCT(premium, notional, spot)` | Premium as % of notional |
| 65 | `INTRINSIC` | `INTRINSIC(strike, fwd, callPut, notional)` | Intrinsic value |
| 66 | `BREAKEVEN` | `BREAKEVEN(strike, premium, callPut)` | Breakeven rate |

### Out of Scope (Pricing Backend)

These are **not** deal ticket formulas — they belong server-side:

- Black-Scholes / Garman-Kohlhagen pricing
- Greeks (delta, gamma, vega, theta, rho)
- Implied volatility solving
- Monte Carlo simulation
- Barrier pricing (knock-in/out analytics)
- Correlation/covariance matrices
- VaR / CVaR calculations
- Smile interpolation (risk reversal, butterfly, vanna-volga)

---

## Files

### New (Rust)
- `rust/src/formula.rs` — AST types, nom parser, evaluator, registry, template engine, wildcard resolver
- `rust/src/formula_graph.rs` — dependency graph, topological sort, cycle detection

### Modified (Rust)
- `rust/src/lib.rs` — module declarations, wasm_bindgen exports
- `rust/src/pipeline.rs` — formula evaluation step in ProcessingPipeline
- `rust/Cargo.toml` — add `nom` dependency

### New (TypeScript)
- `src/formula/types.ts` — FormulaValue, FormulaField, AliasGenerator types
- `src/formula/aliases.ts` — alias generator utilities

### Modified (TypeScript)
- `src/wasm/bridge.ts` — expose formula WASM functions via `wasm` namespace

---

## Phased Delivery

### Phase 1 — Core (This Document)
- Literals, references, arithmetic, parentheses, unary negation
- Alias resolution (full-path + prefix modes)
- Multiple references per formula
- Dependency graph, topological evaluation, cycle detection
- Pipeline integration (step 3: before sync/flip)
- Wildcard references + aggregate functions (MIN/MAX/SUM/AVG/COUNT)
- Strip template formulas with context variables
- Tenor arithmetic (produces instructions for backend resolution)
- Type system enforcement (number/date/tenor rules)
- Uniform value shape: `{ value, context?: { formula? } }`

### Phase 2 — Scalar Functions & Extended Operations (Future)
- Math: `ROUND`, `ROUNDUP`, `ROUNDDOWN`, `ABS`, `FLOOR`, `CEILING`, `MOD`, `POWER`/`^`, `SQRT`, `LN`/`LOG`, `EXP`
- Logic: `IF`, `AND`, `OR`, `NOT`, `IFERROR`, `ISBLANK`, `COALESCE`
- Comparisons: `>`, `<`, `>=`, `<=`, `=`, `<>`
- Extended aggregation: `MEDIAN`, `STDEV`, `PERCENTILE`, `SUMIF`, `COUNTIF`, `MAXIF`/`MINIF`, `PRODUCT`
- Strip helpers: `LINSPACE`, `ACCRETE`, `AMORTIZE`, `STEP`, `LOOKUP`, `PREV`, `NEXT`, `CUMSUM`
- Date functions: `WORKDAY`, `WORKDAY_CCY`, `EDATE`, `EOMONTH`, `IMM_DATE`, `DAYS`, `YEARFRAC`, `TODAY`, `SETTLE_DATE`
- New AST nodes: `ScalarFunc { name, args }`, `ComparisonOp { op, left, right }`

### Phase 3 — Market Data & FX Functions (Future)
- Market data: `SPOT`, `FWD`, `FWD_PTS`, `DEPO`, `VOL`, `SWAP_RATE`, `SOFR`
- FX-specific: `CCY_CONVERT`, `PIP_VALUE`, `DELTA_EQUIV`, `PREMIUM_PCT`, `INTRINSIC`, `BREAKEVEN`
- New AST node: `ExternalRef { provider, key }`
- Async resolution similar to tenor instructions

Each phase adds new AST node types only. No structural changes to parser, evaluator, or pipeline.

---

## Research Context

Architecture informed by analysis of:

- **HyperFormula** (Handsontable): dependency graph design, range composition for node reuse, incremental recalculation
- **Bloomberg BQL**: two-tier architecture (data retrieval separate from computation) — mirrors our JS/WASM split
- **MobX computed values**: transparent dependency tracking, suspension of unused computeds, push-based change propagation
- **nom parser library**: zero-copy Rust parser combinators, battle-tested for custom grammars
- **Enterprise trading platforms** (Murex, Calypso): event-driven architecture, lazy evaluation + aggressive caching patterns
- **FinOptionsXL** (Derivicom): Excel add-in for derivatives pricing — function catalog reference
- **Deriscope**: QuantLib/ORE-powered valuation with Excel integration — architecture patterns for compute-heavy spreadsheet engines
