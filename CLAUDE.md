# Claude Code Configuration - Apex State

AI assistant configuration for apex-state concerns-based reactive state management library.

Built on a **Dual-Layer Architecture**: JavaScript/React owns reactivity and rendering; Rust/WASM owns heavy computational operations (graphs, dependency tracking, pipeline orchestration).

**Key Reference**: `docs/WASM_ARCHITECTURE.md` — Complete specification of the JS/WASM boundary, data flow, and ownership model.

---

## Architectural Philosophy: JS vs WASM

This is a **two-layer system**:

### JavaScript/React Layer (Everything User-Facing)

- **Valtio proxies** — State reactivity, Proxy traps, mutation tracking
- **React rendering** — Components, hooks, re-render optimization
- **Listener handlers** — User-defined functions that react to state changes
- **Zod schemas** — Validation logic (stays in JS)
- **Custom concern evaluate()** — User-defined logic that can't be declarative
- **Getter functions** — Computed properties on state objects
- **`applyBatch()`** — Final step to write changes back to valtio

### Rust/WASM Layer (Heavy Lifting)

- **Shadow state** — Getter-free copy of all values for fast diffing
- **String interning** — Path ↔ ID mapping for O(1) lookups
- **Sync/flip graphs** — Connected components for synchronized/inverted boolean paths
- **Topic router** — Listener topic hierarchy with pre-computed routes
- **Pipeline engine** — Aggregation writes, sync processing, flip processing
- **BoolLogic evaluator** — Tree walker for declarative logic
- **Reverse dependency index** — Which concerns depend on which paths
- **Listener orchestration** — Decides what to call, when, in what order

**The boundary**: String paths cross it. Complex values use slot indices. WASM never touches valtio, React, or Zod.

### Critical: Shadow State Synchronization

**WASM maintains shadow state (a getter-free copy of all values).**

- **Shadow state is automatically kept in-sync by the pipeline.** Every `processChanges()` call updates it as part of processing.
- **There is NO separate `shadowSet()` call.** Never manually sync shadow state.
- Updates work at any level: leaf values, nested objects, or entire subtrees.
- **Shadow state is WASM-internal.** JS doesn't access it directly except for testing.

### Critical: Minimize Cross-Boundary Communication

The JS ↔ WASM boundary is expensive. Every crossing should be intentional.

**WASM holds all computational state internally:**

- Sync/flip graphs
- Topic router
- Path interning table
- Reverse dependency index
- BoolLogic registry
- Listener registry

**TypeScript should NOT duplicate this data** except:

- Function references needed during execution (e.g., listener handler functions, Zod schemas)
- These are stored in `Map<id, function>` only, not as copies of WASM state

**Every piece of data crossing the boundary should answer:**

- "Why does JS need this?"
- "Could WASM handle it instead?"
- "Is there a cheaper way?"

Keep the boundary thin: paths in (strings), changes out (JSON).

### Critical: WASM Bridge as Single Export

**`src/wasm/bridge.ts` exports exactly one thing: `wasm`** — a namespace object holding all WASM functions.

All calls from TypeScript go through this single interface:

```ts
import { wasm } from './wasm/bridge'
// Call as: wasm.processChanges(...), wasm.registerBoolLogic(...), etc.
```

**Why?**

- **Single boundary entry point** — All JS→WASM crossings visible and auditable
- **Clear ownership** — You can see exactly what WASM capabilities are exposed
- **Refactoring safety** — Renaming or restructuring WASM functions touches one import everywhere
- **Prevents accidental direct imports** — No scattered imports of individual WASM functions
- **Namespace clarity** — `wasm.fn()` immediately signals you're crossing the boundary

### Data Ownership Model

**What belongs in WASM (Rust):**

- All graphs (sync, flip, topic router, etc.)
- All registries (BoolLogic, listeners, validators)
- Shadow state (getter-free value copy)
- Path interning (string ↔ ID bidirectional map)
- Reverse dependency index
- All business logic for pipeline orchestration

**What belongs in TypeScript (JS):**

- Valtio proxies (state, _concerns)
- React hooks and components
- Function references: `Map<id, handler>`, `Map<id, schema>`
- Getter functions (evaluated at render time) — **Pitfall:** no dependency tracking, re-evaluate on every access. See `docs/guides/ARCHITECTURE.md` § Valtio Pitfalls.
- Custom concern evaluate() functions
- Zod schema instances

**What crosses the boundary:**

- **JS → WASM**: String paths, JSON values, function IDs, metadata flags
- **WASM → JS**: Change plans (what to do), dispatch orders (when to call what), result data

**Never, ever duplicate:**

- Graph structures
- Registry data
- Listener/sync/flip information (except function refs)
- Computed registries or indices

If you're storing something in TypeScript that WASM also has, ask why. Probably belongs in WASM only.

---

## Quick Rules (No Exceptions)

### 0. Always Use Package.json Scripts

**CRITICAL**: Always use `npm run <script>` commands. Never run underlying tools directly.

**Why?** Package.json encodes correct flags, targets, and paths. Direct tool usage can apply wrong configurations.

**TypeScript changes:**

```bash
npm run code:fix && npm run code:check
```

**Rust/WASM changes:**

```bash
npm run wasm:fmt && npm run wasm:lint && npm run wasm:check
```

**Both layers changed:** Run both.

**DO NOT read the output** - it wastes tokens. Just run them and move on.

**Examples of what NOT to run directly:**

- ❌ `tsc --noEmit` → ✅ `npm run type-check`
- ❌ `eslint .` → ✅ `npm run code:check`
- ❌ `wasm-pack build` → ✅ `npm run wasm:build`
- ❌ `cargo fmt` → ✅ `npm run wasm:fmt`

### 1. Functional Programming Only

All code uses arrow functions. No classes, no function declarations.

### 2. Valtio Reactive Only

Use `valtio-reactive`'s `effect()` for dependency tracking. Never use derive-valtio.

### 3. Two-Proxy Pattern

In effects: read from `state`, write to `_concerns`. This prevents infinite loops.

### 4. Type-Safe Paths Always

Use `DeepKey<T>` and `DeepValue<T, P>` for every path operation. Never string literals.

### 5. Always Return Cleanup

From effects: `return () => { /* cleanup */ }`. Never forget cleanup functions.

### 6. Vitest with LLM Reporter + JQ

**CRITICAL**: When running vitest tests for analysis, ALWAYS use `vitest-llm-reporter` with `jq` to format output for LLM consumption.

**Why?** The LLM reporter provides structured JSON output that's far more efficient to parse than raw vitest output. Using `jq` extracts only the relevant test failures and context.

**Running tests:**

```bash
# Install vitest-llm-reporter if not already installed
npm install -D vitest-llm-reporter

# Run tests with LLM reporter and jq filtering
npx vitest run --reporter=vitest-llm-reporter 2>&1 | jq -r '.failures[] | "\(.file):\(.line) - \(.title)\n  \(.message)"'
```

**For specific test files:**

```bash
npx vitest run tests/specific-file.test.ts --reporter=vitest-llm-reporter 2>&1 | jq '.failures'
```

**DO NOT** run raw `vitest run` or `npm run test:all` and read the full output — it wastes tokens. Use the LLM reporter + jq to get structured, concise failure information.

**Fallback**: If `vitest-llm-reporter` is unavailable or fails, fall back to `npm run test:all 2>&1 | tail -50` to get the last 50 lines of output.

---

## Directory Structure

### JavaScript/TypeScript (`src/`)

```
src/
├── concerns/          # Concern types and registration
│   ├── types.ts              # ConcernType interface
│   ├── registration.ts       # effect() wrapping for custom concerns
│   └── prebuilts/            # Built-in concern implementations
│       ├── disabledWhen.ts           (BoolLogic-based)
│       ├── validationState.ts        (with Zod schema)
│       └── ...
├── store/
│   ├── createStore.ts        # Store factory, all React hooks
│   ├── Provider.tsx          # React context Provider
│   ├── types.ts              # StoreInstance, InternalState
│   └── executor.ts           # Change processing coordination
├── types/
│   ├── deepKey.ts            # Type-safe path generation
│   ├── deepValue.ts          # Type-safe value extraction
│   └── concerns.ts           # BoolLogic, ConcernConfig types
├── wasm/
│   └── bridge.ts             # Thin wrapper over WASM exports
└── utils/
    └── interpolation.ts      # Template string interpolation
```

### Rust/WASM (`rust/src/`)

```
rust/
└── src/
    ├── lib.rs                # WASM entry point, wasm_bindgen exports
    ├── shadow.rs             # Shadow state tree, path traversal
    ├── interning.rs          # String ↔ ID bidirectional map
    ├── boollogic.rs          # BoolLogic tree evaluation
    ├── graphs.rs             # Sync/flip graph structures
    ├── router.rs             # Topic router for listeners
    ├── pipeline.rs           # Main processChanges orchestration
    └── Cargo.toml            # Rust dependencies
```

### Build Process

**ALWAYS use package.json scripts** — Never run underlying tools (`tsc`, `eslint`, `wasm-pack`, `cargo`, etc.) directly.

```
1. npm run wasm:build                  Compile rust/ → .wasm + JS glue + .d.ts
   (wraps: wasm-pack build --target bundler rust)
2. npm run build                       Bundle src/ + inline .wasm as base64
   (wraps: tsup with @rollup/plugin-wasm)
3. npm run type-check                  TypeScript type checking
   (wraps: tsc --noEmit)
4. Output: dist/index.js + dist/index.d.ts
```

**Why?** Package.json scripts encode correct flags, paths, and configurations. Running tools directly can use wrong settings.

**Why?** Package.json scripts encode the correct build targets and paths. Running tools directly can use wrong targets (e.g., `--target node` instead of `--target bundler`).

---

## Data Flow: One State Change

**User calls**: `setValue("user.email", "alice@example.com")`

```
JS/React Layer:
  ├─ Detect change (setValue call)
  ├─ Queue change: { path: "user.email", value: "alice@example.com" }
  └─ Call WASM: processChanges([...])
         ↓
  ┌──────────────────────────────────────────────────────┐
  │          WASM/Rust Layer (single call)               │
  ├──────────────────────────────────────────────────────┤
  │ 1. Update shadow state                               │
  │ 2. Intern path ID (or lookup from cache)             │
  │ 3. Identify affected BoolLogics (reverse index)      │
  │ 4. Evaluate affected BoolLogics                      │
  │ 5. Identify affected listeners (topic router)        │
  │ 6. Prepare dispatch plan (depth-ordered)             │
  │ 7. Return: final changes + dispatch plan             │
  └──────────────────────────────────────────────────────┘
         ↓
JS/React Layer (continued):
  ├─ Apply BoolLogic results to _concerns proxy
  ├─ Execute listener handlers (if any) — multiple round trips
  ├─ Collect produced changes from handlers
  ├─ Route produced changes back to WASM (if listeners)
  ├─ Apply final changes to state proxy → triggers React re-renders
  └─ Done (valtio change subscription notification sent)
```

**Key insight**: One `processChanges` call orchestrates everything. No effect loops, no redundant evaluations.

---

## Documentation & References

**Primary reference for everything**: `docs/WASM_ARCHITECTURE.md`

- Complete data flow, ownership split, boundary crossings
- Shadow state structure, path interning, listener dispatch protocol
- WASM API specification (what JS can call, when, what it returns)

**Architecture decisions**: `tasks/WASM-EP*.md`

- WASM-EP1-FOUNDATION: String interning, BoolLogic, reverse dependencies
- WASM-EP2-PIPELINE: Aggregation, sync/flip graphs, shadow state sync
- WASM-EP3-LISTENERS: Topic router, dispatch plan, depth-ordered execution
- WASM-EP4-VALIDATION: Validator batching, reverse index
- WASM-EP5-STREAMING: Stream gateway, change filtering

---

## When Unsure or Multiple Options Exist: ASK

**You're working with an expert.** This codebase is sophisticated. When you encounter:

- ✅ Multiple valid approaches that trade off differently
- ✅ Architectural decisions that could go multiple ways
- ✅ Refactoring opportunities that might help or hurt clarity
- ✅ Boundary placements between JS and WASM
- ✅ Performance optimizations with unclear payoff

**STOP and ask.** Don't guess. Ask which direction you should take, what the user prefers, or what constraints matter most.

Example questions:

- "I see 3 ways to structure this concern. Which tradeoff matters most to you — bundle size, runtime speed, or code clarity?"
- "Should this logic live in JS (more flexible) or Rust (faster)? What's the priority?"
- "I could refactor X to reduce duplication, but it changes the architecture slightly. Should I do that?"

**DO NOT**:

- Assume the "best" approach
- Optimize for code duplication if it obscures architecture
- Make choices that prioritize tooling convenience over clarity
- Move logic around to "improve" things without explicit permission

---

## Understanding the Code

Use **grepai** as your primary tool:

- `grepai search "concern evaluation"` to find how concerns work
- `grepai search "sync graph"` to understand sync/flip logic
- `grepai trace callers "processChanges"` to see what calls the pipeline
- `grepai trace callees "registerBoolLogic"` to understand registration flow

Code is the source of truth. Implementation files are more current than docs.

---

## Documentation Practices

### TSDoc Comments

✅ **DO add TSDoc for:**

- Public exports (functions, types, constants users will import)
- Complex algorithms that need explanation
- Non-obvious tradeoffs or constraints
- When explicitly asked

❌ **DON'T add TSDoc for:**

- Private/internal functions
- Self-documenting code (clear names, simple logic)
- Every function "just in case"

### Code Comments

✅ **DO keep comments that explain:**

- Why something is done this way (not what it does)
- Tradeoffs or constraints
- References to external docs or issues
- Historical context ("we tried X, it caused Y")

❌ **DON'T remove comments:**

- For cleanup or refactoring
- Comments are context, they have value even if code changes
- If a comment is wrong, fix it, don't delete it

---

## TypeScript Best Practices

**Strict mode enforced**. No `any`, `never`, or type casts to suppress errors. Fix types properly.

### Key patterns in this codebase

1. **Generic constraints over union types**

   ```ts
   // Good: Constraint keeps type information
   const getValue = <T extends Record<string, unknown>>(obj: T, key: keyof T): T[keyof T] => ...

   // Bad: Union loses specificity
   const getValue = (obj: any, key: string): any => ...
   ```

2. **Mapped types for path safety**
   - `DeepKey<T>` — Generates all valid paths in T
   - `DeepValue<T, P>` — Gets the type of value at path P in T
   - Always use these. Never use string literals for paths.

3. **Template literal types for flexibility**
   - Paths are strings at runtime but strongly typed at compile time
   - Pattern: `type ValidPath = DeepKey<State>`

4. **Inference over explicit types where possible**
   - Let TS infer from implementation
   - Explicit types on public boundaries only
   - Reduces boilerplate, catches refactoring issues

**Reference**: [TypeScript Handbook - Generics](https://www.typescriptlang.org/docs/handbook/2/generics.html), [Conditional Types](https://www.typescriptlang.org/docs/handbook/2/conditional-types.html), [Template Literal Types](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-4-4.html#template-literal-types)

---

## Rust Best Practices

This is a performance-critical layer. Patterns matter.

### Key patterns in WASM code

1. **No allocations in hot paths**
   - Use references, not clones
   - Pre-allocate collections, reuse buffers
   - Pattern: `fn process(&mut self, input: &[T]) { self.buffer.clear(); ... }`

2. **Interior mutability for caching**
   - Use `HashMap<K, V>` for interning tables (path string ↔ ID)
   - Use `Vec<T>` for dense indexed storage (PathId → value)
   - Pattern: Intern once, reference by ID forever

3. **Enums for type safety (no magic strings/numbers)**

   ```rust
   #[derive(Deserialize)]
   enum BoolLogicNode {
       IsEqual(String, Value),
       Exists(String),
       And(Vec<BoolLogicNode>),
       // ...
   }
   ```

4. **Early returns instead of deep nesting**
   - Keep cyclomatic complexity low
   - Pattern: `let value = match self.get(key) { Some(v) => v, None => return Err(...) };`

5. **serde for JS interop (no manual serialization)**
   - `#[derive(Serialize, Deserialize)]` on all boundary types
   - JSON is the serialization format (WASM ↔ JS)
   - Pattern: JS sends `JSON.stringify(...)`, Rust does `serde_json::from_str(...)`

6. **Documentation on public exports only**
   - Every `#[wasm_bindgen]` function has doc comments
   - Internal functions: minimal comments, code is self-documenting
   - Pattern: Doc comments explain WHAT and WHY, not HOW

**Reference**: [Rust Book - Ownership](https://doc.rust-lang.org/book/ch04-00-understanding-ownership.html), [The Rust API Guidelines](https://rust-lang.github.io/api-guidelines/), [serde docs](https://serde.rs/)

---

## Critical DONTs

### JavaScript/React Layer

❌ **Never run tools directly** — Always use `npm run <script>` (tsc, eslint, wasm-pack, cargo, etc. must go through package.json)
❌ **Never skip running `npm run code:fix && npm run code:check` after TS changes**
❌ **Never read lint/format output (wastes tokens)**
❌ **Never use classes or function declarations** — always arrow functions
❌ **Never use `React.fn()` pattern** — use `fn()` directly (e.g., `useMemo()` not `React.useMemo()`)
❌ **Never use derive-valtio for dependency tracking** — use `effect()` from valtio-reactive
❌ **Never read and write to same proxy in effects** — read from `state`, write to `_concerns`
❌ **Never mutate state inside concern evaluate()** — return new values, don't modify inputs
❌ **Never skip type-safe paths** — always use `DeepKey<T>` and `DeepValue<T, P>`
❌ **Never forget cleanup functions** — always `return () => { cleanup }` from effects
❌ **Never suppress TypeScript errors with shortcuts** — no `as any`, `@ts-ignore`, etc. Fix types properly.
❌ **bridge.ts must export only one thing: `wasm`** — All WASM functions accessed as `wasm.functionName()` for clarity and single boundary entry point

### Rust/WASM Layer

❌ **Never skip running `npm run wasm:fmt && npm run wasm:lint && npm run wasm:check` after Rust changes**
❌ **Never allocate in hot paths** — (processChanges is called frequently)
❌ **Never clone when you can reference** — paths are strings, internalize to IDs
❌ **Never hardcode assumptions about path structure** — traverse shadow state, don't assume flat
❌ **Never skip serde derives on boundary types** — JS ↔ WASM uses JSON serialization
❌ **Never panic in exported functions** — use `Result<T, JsValue>` and proper error handling
❌ **Never let shadow state get out of sync** — it's updated by processChanges() automatically, never manually

### JS/TypeScript Layer

❌ **Never duplicate WASM computational state** — graphs, routers, registries are WASM-internal
❌ **Never store sync/flip/listener data in TypeScript** — store only function references (handlers, schemas)
❌ **Never manually sync shadow state** — it updates automatically during pipeline processing
❌ **Never send unnecessary data across the boundary** — minimize crossings, keep it thin

### Public API & Types

❌ **Never change public API signatures without explicit confirmation** — e.g., `createStore()`, `useStore()`, `useConcerns()`
❌ **Never change generic type constraints or structure without confirmation** — e.g., `DeepKey<T>`, `DeepValue<T, P>`, `BoolLogic<T>`
❌ **Never add optional parameters to existing function signatures** — breaking change for consumers

**Why?** Public APIs are contracts. Generic types enable type safety. Changes cascade to all consumers.

### Git

❌ **Never commit or push to git** — Only the user commits and pushes
❌ **Never run destructive git commands** (force push, hard reset, etc.) without explicit user request
❌ **Never move files without explicit permission** — use `git mv` to preserve history if user authorizes

### Code Quality

❌ **Never create more than 1 new file (testing, analysis, research) per session/task** — Always edit existing files instead of creating duplicates
❌ **If you created a file earlier in the session, edit it** — Don't create a second version of the same file
❌ **Never create multiple test files for the same task** — One test file per feature, edit it if changes are needed
❌ **Never automatically remove duplicated code** — ask first whether it should stay as reference
❌ **Never fix technical debt you spot** — add to `TECHNICAL_DEBT.md`, ask user before processing
❌ **Never refactor to "improve" code** — keep changes scoped to what was asked
❌ **Never create excessive TSDoc comments** — add docs only when explicitly asked or for public APIs
❌ **Never remove code comments for no reason** — comments exist for context; preserve them
❌ **Never remove test files or test cases without permission** — at minimum, keep test names + placeholder steps

### Tests

❌ **Never skip test names** — test names are documentation, they matter
❌ **Never delete test content entirely** — replace with placeholder listing the steps/assertions that belong there:

   ```typescript
   it('should validate user email format', () => {
     // TODO: Step 1 - Create a user object with invalid email
     // TODO: Step 2 - Call validator
     // TODO: Step 3 - Assert validation fails with correct error message
   })
   ```

❌ **Never refactor test without keeping case structure** — preserve test case names and step descriptions

❌ **Never chain optional access in expect() statements** — split into two lines for better readability and debugging:

   ```typescript
   // Bad: Hard to debug, unclear error messages
   expect(
     storeInstance._concerns['user.email']?.['validationState'],
   ).toBeDefined()

   // Good: Clear, debuggable, better error messages
   const validationState = storeInstance._concerns['user.email']?.['validationState']
   expect(validationState).toBeDefined()
   ```

✅ **Always test with nested paths** — don't just test root-level paths. Include tests with `user.profile.name`, `cart.items.price`, etc. to verify deep path handling works correctly.

✅ **Always test at different nesting levels** — verify behavior at root (`price`), one level deep (`user.age`), and deeply nested (`user.profile.settings.theme`). Bugs often hide at specific nesting depths.

✅ **Always include type definition validation tests** — when adding new types (e.g., `ComputationPair`, `SyncPair`), add compile-time type tests using `expectTypeOf` to verify the type accepts valid inputs and rejects invalid ones.

✅ **Use `@ts-expect-error` for type rejection tests** — when testing that a type correctly rejects invalid inputs, use `@ts-expect-error` instead of `expectTypeOf(...).not.toMatchTypeOf`:

   ```typescript
   // Good: Clearly documents that this SHOULD fail type checking
   // @ts-expect-error — string path should not be valid as number-only target
   const bad: ComputationPair<State> = ['SUM', 'name', 'count']

   // Also acceptable: expectTypeOf for negative type assertions
   expectTypeOf<['SUM', 'name', 'count']>().not.toMatchTypeOf<ComputationPair<State>>()
   ```

### Both Layers

❌ **Never make changes beyond what was asked**
❌ **Never assume refactoring is needed** — ask first if benefits aren't obvious

---

## Core Principles

### All Code

1. **Always format + check** - Run `npm run code:fix && npm run code:check` after TS changes, `npm run wasm:fmt && npm run wasm:lint && npm run wasm:check` after Rust changes (don't read output)
2. **Keep minimal scope** - Change only what solves the immediate problem
3. **Ask when uncertain** - Multiple valid approaches? Ask which tradeoff matters
4. **Understand before changing** - Read the code, grepai to understand intent, then modify

### JavaScript/React Layer

5. **Functional only** - Arrow functions everywhere
6. **Type-safe paths** - Always use `DeepKey<T>` and `DeepValue<T, P>`
7. **Two-proxy pattern** - Read from `state`, write to `_concerns` (prevents loops)
8. **Pure concerns** - No mutations, no side-effects in evaluate()
9. **Always cleanup** - Return cleanup functions from effects
10. **Valtio-reactive** - Use `effect()` for dependency tracking, not derive-valtio

### Rust/WASM Layer

11. **Prefer references** - Don't clone unless necessary
12. **Intern early, reference often** - Paths become IDs at boundary
13. **No panics in exports** - Use `Result<T, JsValue>` for all `#[wasm_bindgen]` functions
14. **Serialize to JSON** - serde handles JS ↔ WASM boundary
15. **Document public, not private** - Doc comments only on `#[wasm_bindgen]` exports

---

## Your Workflow

1. **Understand** - Use grepai to find relevant code, understand what it does, why it exists
2. **Ask if unsure** - Multiple valid approaches? Ask which tradeoff matters most
3. **Code** - Make the requested changes (keep scope narrow)
   - **If you see duplicated code**: Ask — should we remove it now or keep as reference?
   - **If you spot technical debt**: Add it to `TECHNICAL_DEBT.md` (one-liner), don't fix it yet
4. **Format + check** - Run `npm run code:fix && npm run code:check` for TS, `npm run wasm:fmt && npm run wasm:lint && npm run wasm:check` for Rust (don't read output)
5. **Summary** - If you added technical debt items, show them to the user so they stay informed
6. **Done** - Code is ready

## Code Duplication: Ask, Don't Assume

**When you encounter duplicated code:**

❌ **Don't automatically remove it.** Code might be duplicated intentionally (for clarity, performance, independence, or coupling avoidance).

✅ **Always ask:**

- "I see this pattern repeated in X and Y. Should I extract a helper now, or keep them separate for reference?"
- "Would removing duplication reduce clarity or add coupling?"
- "Is this duplication worth 3 lines, or would abstracting it help?"

**Wait for feedback before removing or extracting anything.**

---

## Technical Debt: Track It, Don't Ignore It

**As you work, you'll spot opportunities for improvement that aren't in scope right now.**

✅ **Do this:**

1. Add one-liner to `TECHNICAL_DEBT.md` (see format below)
2. Continue with the task at hand
3. When done, show the debt list to the user
4. **Ask** before processing any items

❌ **Don't do this:**

- Fix technical debt you spot unless explicitly asked
- Refactor to address debt during unrelated work
- Hide debt issues hoping they'll resolve themselves

### Technical Debt Tracking Format

**File: `TECHNICAL_DEBT.md`**

```markdown
## Pending Technical Debt

- **[Component/Layer]** Brief description of what should be improved. `src/file.ts:line`
- **[WASM]** Shadow state diffing could use deeper comparison for arrays. `rust/src/shadow.rs:42`
- **[JS/WASM boundary]** Multiple roundtrips for listener dispatch could be optimized. `src/wasm/bridge.ts:105`
- **[Types]** Generic constraint on DeepKey could be tightened. `src/types/deepKey.ts:8`

## Completed Items
(moved here after processing)
```

**Each line is compact**: Layer, issue, location. When you encounter debt, add it, show it to the user, ask before fixing.

---

## When Facing a Decision

**Never assume.** This codebase has sophisticated tradeoffs. When you see:

- Multiple ways to solve something
- A potential refactoring opportunity
- A choice between JS and WASM placement
- An optimization with unclear payoff
- Duplicated code
- Technical debt opportunities

**Stop and ask:**

- "What's the priority here?"
- "Do you prefer approach A or B?"
- "Is this worth the complexity?"
- "Should I change this?"
- "Should I remove this duplication or keep it as reference?"

**This prevents wrong decisions and saves iteration time.**

---

## Summary: The Two Layers

**React/Valtio (JS)** — User-facing reactivity

- State mutations, Proxy traps, React re-renders
- Listener handlers, Zod validation, getters
- Custom concern logic that can't be declarative

**Rust/WASM (Heavy Lifting)** — Orchestration & computation

- Shadow state, string interning, path ID lookups
- Sync/flip graphs, topic routing, listener dispatch
- BoolLogic evaluation, reverse dependency index
- Batches changes to minimize JS boundary crossings

**The boundary**: Paths cross as strings. Values cross as JSON. WASM decides the execution plan, JS decides what to do.

Everything follows from this split.

## grepai - Semantic Code Search

**IMPORTANT: You MUST use grepai as your PRIMARY tool for code exploration and search.**

### When to Use grepai (REQUIRED)

Use `grepai search` INSTEAD OF Grep/Glob/find for:

- Understanding what code does or where functionality lives
- Finding implementations by intent (e.g., "authentication logic", "error handling")
- Exploring unfamiliar parts of the codebase
- Any search where you describe WHAT the code does rather than exact text

### When to Use Standard Tools

Only use Grep/Glob when you need:

- Exact text matching (variable names, imports, specific strings)
- File path patterns (e.g., `**/*.go`)

### Fallback

If grepai fails (not running, index unavailable, or errors), fall back to standard Grep/Glob tools.

### Usage

```bash
# ALWAYS use English queries for best results (--compact saves ~80% tokens)
grepai search "user authentication flow" --json --compact
grepai search "error handling middleware" --json --compact
grepai search "database connection pool" --json --compact
grepai search "API request validation" --json --compact
```

### Query Tips

- **Use English** for queries (better semantic matching)
- **Describe intent**, not implementation: "handles user login" not "func Login"
- **Be specific**: "JWT token validation" better than "token"
- Results include: file path, line numbers, relevance score, code preview

### Call Graph Tracing

Use `grepai trace` to understand function relationships:

- Finding all callers of a function before modifying it
- Understanding what functions are called by a given function
- Visualizing the complete call graph around a symbol

#### Trace Commands

**IMPORTANT: Always use `--json` flag for optimal AI agent integration.**

```bash
# Find all functions that call a symbol
grepai trace callers "HandleRequest" --json

# Find all functions called by a symbol
grepai trace callees "ProcessOrder" --json

# Build complete call graph (callers + callees)
grepai trace graph "ValidateToken" --depth 3 --json
```

### Workflow

1. Start with `grepai search` to find relevant code
2. Use `grepai trace` to understand function relationships
3. Use `Read` tool to examine files from results
4. Only use Grep for exact string searches if needed
