# Claude Code Configuration - Apex State

AI assistant configuration for apex-state concerns-based reactive state management library.

---

## 🚨 CRITICAL: NO LARGE REFACTORING

**MAKE MINIMAL CHANGES ONLY**

- ✅ **DO**: Fix specific bugs, add requested features, make targeted improvements
- ❌ **DON'T**: Refactor existing code unless explicitly asked
- ❌ **DON'T**: Restructure files, rename things, or "improve" working code
- ❌ **DON'T**: Change patterns "because it would be better"

**IF YOU SEE ISSUES THAT NEED REFACTORING:**

1. **STOP** - Do NOT refactor automatically
2. **EXPLAIN** - Tell the user what you found and why it might need refactoring
3. **ASK** - Get explicit permission before making structural changes
4. **ALTERNATIVE** - If possible, find a way to solve the problem WITHOUT refactoring

**When refactoring IS allowed:**

- User explicitly says "refactor X"
- User asks "how can we improve X" and you propose refactoring
- User approves your refactoring proposal

---

## Quick Rules (No Exceptions)

### 0. Always Format Code

**CRITICAL**: After making ANY code changes, run:

```bash
npm run code:fix
```

**DO NOT read the output** - it wastes tokens. Just run it and move on.

This applies ESLint + Prettier formatting. Code must follow project style.

### 1. Functional Programming Only

**See examples in**: Any file in `src/` - all use arrow functions

### 2. Never Use derive-valtio

**Use**: `valtio-reactive`'s `effect()` for dependency tracking
**See**: `src/concerns/registration.ts:54-76` for reference implementation

### 3. Two-Proxy Pattern

**Pattern**: Read from `state`, write to `_concerns`
**See**: `src/concerns/registration.ts:54-76` for how it's done

### 4. Type-Safe Paths

**Use**: `DeepKey<T>` and `DeepValue<T, P>` for all path operations
**See**: `src/store/createStore.ts:85-106` (useStore implementation)

### 5. Always Return Cleanup

**See**: `src/concerns/registration.ts:84-110` for cleanup pattern

---

## Core Architecture

**Structure**:

```
StoreInstance {
  state: proxy({ ...data })          // User data (tracked)
  _concerns: proxy({ ...computed })  // Concern results (tracked)
  _internal: ref({ ...graphs })      // Internal state (NOT tracked)
}
```

**Read**: `src/store/types.ts:193-221` for complete type definition

**Key Insight**: Reading from `state` and writing to `_concerns` prevents infinite loops.
**See why**: `docs/guides/ARCHITECTURE.md`

---

## Directory Structure

```
src/
├── concerns/          # Reactive validation & UI logic
│   ├── types.ts              # ConcernType interface
│   ├── registration.ts       # How effect() wrapping works
│   └── prebuilts/*.ts        # Built-in concern implementations
├── store/
│   ├── createStore.ts        # Store factory, all hooks
│   ├── Provider.tsx          # React context Provider
│   ├── types.ts              # StoreInstance, InternalState
│   └── executor.ts           # Change processing pipeline
├── types/
│   ├── deepKey.ts            # Type-safe path generation
│   ├── deepValue.ts          # Type-safe value extraction
│   └── concerns.ts           # BoolLogic types
└── utils/
    ├── boolLogic.ts          # BoolLogic evaluation
    └── interpolation.ts      # Template string interpolation
```

---

## Expert Agents

**Before working on code in any of these areas, read the corresponding expert prompt first.** These contain domain-specific architecture context, key files, patterns, and rules. When spawning subagents (Task tool), include the expert prompt content in the task description.

| Files touched                                                           | Read first                          |
| ----------------------------------------------------------------------- | ----------------------------------- |
| `src/concerns/`, `src/utils/boolLogic.ts`, `src/utils/interpolation.ts` | `docs/agents/expert-concerns.md`    |
| `src/pipeline/`, `src/sideEffects/`, `src/core/pathGroups.ts`           | `docs/agents/expert-pipeline.md`    |
| `src/store/`, `src/hooks/`, `src/core/context.ts`                       | `docs/agents/expert-store-hooks.md` |
| `tests/` (any test work)                                                | `docs/agents/expert-testing.md`     |
| Cross-cutting changes, new features, architecture decisions             | `docs/agents/expert-architect.md`   |

## Documentation Navigation

**Working on concerns system?**
→ Read `docs/guides/CONCERNS_GUIDE.md`

**Understanding architecture?**
→ Read `docs/guides/ARCHITECTURE.md`

**Using store hooks?**
→ Read `docs/guides/STORE_HOOKS.md`

**Architecture decisions?**
→ Read `CONCERNS_REFERENCE.md`

---

## Where to Find Examples

| Need Example Of          | Look At                                                                                                               |
| ------------------------ | --------------------------------------------------------------------------------------------------------------------- |
| **Creating a concern**   | `src/concerns/prebuilts/validationState.ts` (validation)<br>`src/concerns/prebuilts/disabledWhen.ts` (with BoolLogic) |
| **Using effect()**       | `src/concerns/registration.ts:54-76`                                                                                  |
| **Hook implementation**  | `src/store/createStore.ts:85-106` (useStore)                                                                          |
| **Type-safe paths**      | `src/types/deepKey.ts`, `src/types/deepValue.ts`                                                                      |
| **BoolLogic evaluation** | `src/utils/boolLogic.ts`                                                                                              |
| **Testing concerns**     | `tests/concerns/*.test.ts`                                                                                            |
| **Integration tests**    | `tests/integration/*.test.tsx`                                                                                        |

---

## Critical DONTs

❌ **Never skip running `npm run code:fix` after code changes**
❌ **Never read code:fix output (wastes tokens)**
❌ **Never refactor or restructure without explicit permission**
❌ **Never make changes beyond what was asked**
❌ **Never use classes or function declarations**
❌ **Never use derive-valtio for dependency tracking**
❌ **Never read and write to same proxy in effects**
❌ **Never mutate state inside concern evaluate()**
❌ **Never skip type-safe paths (DeepKey/DeepValue)**
❌ **Never forget to return cleanup functions**
❌ **Never move files around without explicit permission, if you must, use git mv to preserve history**
❌ **Never pick shortcuts in typescript types. Do NOT use `as any`, `as never`, `@ts-ignore`, or `@ts-expect-error` to suppress real type errors. Fix the types properly.**
❌ **Never remove or modify existing code comments unless explicitly asked. Comments contain valuable context, intent, and history that is easily lost and hard to recover.**

---

## Core Principles

1. **Always format** - Run `npm run code:fix` after code changes (don't read output)
2. **Minimal changes** - Only change what was requested, nothing more
3. **Functional only** - Arrow functions everywhere (see any file in `src/`)
4. **Type-safe paths** - DeepKey<T> and DeepValue<T, P> (see `src/types/`)
5. **Automatic tracking** - Use valtio-reactive's effect() (see `src/concerns/registration.ts`)
6. **Two-proxy pattern** - Read from state, write to \_concerns (prevents infinite loops)
7. **Pure concerns** - No mutations, no side-effects (see `src/concerns/prebuilts/` for examples)
8. **Always cleanup** - Return cleanup functions (see `src/concerns/registration.ts:84-110`)
9. **Start simple** - Add complexity only when needed

---

## Workflow After Making Changes

1. **Write/edit code** - Make the requested changes
2. **Format code** - Run `npm run code:fix` (don't read output)
3. **Verify** - Run `npm run code:check` (ESLint + TypeScript type check)
4. **Done** - Code is ready

## When in Doubt

1. **Read the actual implementation files** - they are the source of truth
2. **Check TSDoc comments** - implementation details are documented in code
3. **Look at existing implementations** - follow established patterns
4. **Check tests** - they show real usage

**DO NOT**:

- Skip formatting (always run code:fix)
- Read code:fix output (wastes tokens)
- Guess or invent new patterns
- Copy examples from docs (they may be outdated)
- Refactor working code to "improve" it

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
