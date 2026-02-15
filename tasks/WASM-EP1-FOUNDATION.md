# WASM-EP1: Foundation & Toolchain

**Type**: Epic
**Priority**: P0
**Goal**: Validate WASM integration, build pipeline, and consumer DX by moving BoolLogic evaluation to Rust.

---

## WASM-001: Rust toolchain setup

**Type**: Story | **Points**: 3 | **Priority**: P0
**Depends on**: —

### Description

Set up the Rust/wasm-pack/wasm-bindgen project structure inside the repository. Configure cargo workspace, wasm-pack build target, and basic CI integration.

### Key decisions

- Directory: `rust/` at repo root
- Target: `--target bundler` (for tsup/rollup consumption)
- Output: `.wasm` + `.js` glue + `.d.ts` type stubs

### Acceptance criteria

- [ ] `rust/Cargo.toml` with wasm-bindgen dependency
- [ ] `rust/src/lib.rs` with a trivial exported function (e.g., `add(a, b)`)
- [ ] `wasm-pack build --target bundler` produces valid output
- [ ] Output can be imported from TypeScript
- [ ] CI step added (GitHub Actions) with Rust toolchain cache
- [ ] README note on dev prerequisites (rustup, wasm-pack)

---

## WASM-002: String interning table

**Type**: Story | **Points**: 3 | **Priority**: P0
**Depends on**: WASM-001

### Description

Implement a bidirectional string interning table shared between JS and WASM. JS resolves `path string → u32 ID` at registration time. WASM stores `u32 ID → String` for debug/error messages.

### Key files

- `rust/src/intern.rs` — Rust-side intern table
- `src/utils/intern.ts` — JS-side lookup map (new)

### Interface

```rust
// Rust exports
fn intern(path: &str) -> u32
fn resolve(id: u32) -> String  // debug only
fn intern_batch(paths: &[&str]) -> Vec<u32>
```

```typescript
// JS-side
const pathToId: Map<string, number>  // populated at registration
```

### Acceptance criteria

- [ ] Interning is deterministic (same path → same ID across calls)
- [ ] Batch interning for registration-time efficiency
- [ ] JS Map stays in sync with WASM table
- [ ] Round-trip test: intern in WASM → resolve → matches original string
- [ ] Performance: 1000 paths interned in < 1ms

---

## WASM-003: BoolLogic evaluator in Rust

**Type**: Story | **Points**: 5 | **Priority**: P0
**Depends on**: WASM-002

### Description

Port the BoolLogic tree evaluator from `src/utils/boolLogic.ts` to Rust. The Rust evaluator operates on interned path IDs and a shadow copy of primitive values.

### Current implementation

- `src/utils/boolLogic.ts` — recursive tree walker
- Operators: IS_EQUAL, EXISTS, IS_EMPTY, AND, OR, NOT, GT, LT, GTE, LTE, IN

### Rust data model

```rust
enum BoolLogicNode {
    IsEqual(u32, ValueRepr),      // path_id, expected
    Exists(u32),                   // path_id
    IsEmpty(u32),                  // path_id
    And(Vec<BoolLogicNode>),
    Or(Vec<BoolLogicNode>),
    Not(Box<BoolLogicNode>),
    Gt(u32, f64),                  // path_id, threshold
    Lt(u32, f64),
    Gte(u32, f64),
    Lte(u32, f64),
    In(u32, Vec<ValueRepr>),       // path_id, allowed_values
}
```

### Acceptance criteria

- [ ] All operators from JS implementation ported to Rust
- [ ] Evaluator reads values from shadow state (HashMap<u32, ValueRepr>)
- [ ] Parity tests: same inputs produce same outputs as JS evaluator
- [ ] Benchmark: 1000 BoolLogic evaluations in < 100µs
- [ ] BoolLogic tree serialization from JS (JSON or structured) → Rust deserialization

---

## WASM-004: Reverse dependency index

**Type**: Story | **Points**: 3 | **Priority**: P0
**Depends on**: WASM-003

### Description

Build a reverse dependency index in Rust that maps `path_id → [BoolLogic IDs]`. When a path changes, the index returns which BoolLogic trees need re-evaluation.

### Interface

```rust
fn register_bool_logic(logic_id: u32, tree: BoolLogicNode)
// Internally: extract all path_id leaves, update reverse index

fn affected_by_change(path_id: u32) -> Vec<u32>
// Returns logic IDs that depend on this path
```

### Acceptance criteria

- [ ] Registering a BoolLogic tree extracts all referenced path IDs
- [ ] Reverse index returns correct logic IDs for a given path change
- [ ] Unregistering a BoolLogic tree cleans up the reverse index
- [ ] Multi-path BoolLogic (AND/OR with different paths) indexed correctly
- [ ] Performance: lookup in O(1) average case (HashMap)

---

## WASM-005: JS bridge — BoolLogic concerns

**Type**: Story | **Points**: 5 | **Priority**: P0
**Depends on**: WASM-004

### Description

Replace `effect()` wrapping for BoolLogic-based concerns (disabledWhen, visibleWhen, readonlyWhen, dynamicLabel, dynamicTooltip, dynamicPlaceholder) with WASM evaluation. Instead of N individual `effect()` subscriptions, register BoolLogic trees in Rust at mount time and batch-evaluate affected trees when changes arrive.

### Current flow (per concern, per path)

```
effect() → dot.get(state, path) → evaluateBoolLogic(condition, state) → write to _concerns
```

### New flow

```
Registration: wasm.registerBoolLogic(pathId, logicTree)
On change:    wasm.evaluateAffected(changedPathIds) → [(pathId, concernName, bool)]
              JS applies results to _concerns proxy
```

### Key files to modify

- `src/_internal/concerns/registration.ts` — split BoolLogic vs custom concerns
- `src/concerns/prebuilts/disabledWhen.ts` — serialize condition to WASM
- `src/concerns/prebuilts/visibleWhen.ts` — same
- `src/concerns/prebuilts/readonlyWhen.ts` — same

### Acceptance criteria

- [ ] BoolLogic concerns no longer create individual `effect()` instances
- [ ] Custom concerns (with user `evaluate()` functions) still use `effect()` — no regression
- [ ] BoolLogic evaluation results match JS implementation exactly
- [ ] Cleanup on unmount removes BoolLogic registrations from WASM
- [ ] All existing concern tests pass without modification
- [ ] Benchmark: 500 fields × 3 BoolLogic concerns, single path change < 100µs

---

## WASM-006: Inline WASM in tsup build

**Type**: Story | **Points**: 2 | **Priority**: P0
**Depends on**: WASM-001

### Description

Configure the library build to inline the compiled `.wasm` binary as base64 in the JS bundle. Consumers should see no `.wasm` files, no extra plugins, no config.

### Approach

- Use `@rollup/plugin-wasm` with `{ sync: true }` in tsup/vite config
- WASM binary (~3-8KB gzipped) inlined as base64 string
- Instantiation via `WebAssembly.compile()` + `WebAssembly.instantiate()`

### Key files

- `vite.config.ts` or `tsup.config.ts` — add WASM plugin
- `package.json` — add build step: `wasm-pack build` before `tsup`

### Acceptance criteria

- [ ] `npm run build` produces a single JS bundle with WASM inlined
- [ ] No `.wasm` files in `dist/`
- [ ] Bundle size increase documented (expected: 3-8KB gzipped)
- [ ] Consumer test: `import { createGenericStore } from './dist'` works without plugins
- [ ] Both ESM and CJS outputs work

---

## WASM-007: Phase 1 integration tests

**Type**: Story | **Points**: 3 | **Priority**: P0
**Depends on**: WASM-005, WASM-006

### Description

End-to-end tests verifying BoolLogic concerns evaluate correctly through the WASM path. These tests should exercise the full flow: registration → state change → WASM evaluation → _concerns update → React re-render.

### Test scenarios

1. Single field with `disabledWhen` — toggling the condition path
2. Multiple fields sharing a dependency path — batch evaluation
3. Nested BoolLogic (AND/OR/NOT combinations)
4. Registration and cleanup lifecycle (mount/unmount)
5. Parity test: same store config produces identical results with WASM vs JS evaluator

### Acceptance criteria

- [ ] All test scenarios pass
- [ ] Tests run in both Node (vitest) and browser (playwright or similar)
- [ ] Performance regression test: WASM path is not slower than JS for < 10 fields
- [ ] WASM module initialization does not block React render (async or cached)
