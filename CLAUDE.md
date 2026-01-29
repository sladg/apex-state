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
**See why**: `docs/agents/ARCHITECTURE.md`

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

## Documentation Navigation

**Working on concerns system?**
→ Read `docs/agents/CONCERNS_GUIDE.md`

**Understanding architecture?**
→ Read `docs/agents/ARCHITECTURE.md`

**Using store hooks?**
→ Read `docs/agents/STORE_HOOKS.md`

**Architecture decisions?**
→ Read `CONCERNS_REFERENCE.md`

---

## Where to Find Examples

| Need Example Of | Look At |
|-----------------|---------|
| **Creating a concern** | `src/concerns/prebuilts/validationState.ts` (validation)<br>`src/concerns/prebuilts/disabledWhen.ts` (with BoolLogic) |
| **Using effect()** | `src/concerns/registration.ts:54-76` |
| **Hook implementation** | `src/store/createStore.ts:85-106` (useStore) |
| **Type-safe paths** | `src/types/deepKey.ts`, `src/types/deepValue.ts` |
| **BoolLogic evaluation** | `src/utils/boolLogic.ts` |
| **Testing concerns** | `tests/concerns/*.test.ts` |
| **Integration tests** | `tests/integration/*.test.tsx` |

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

---

## Core Principles

1. **Always format** - Run `npm run code:fix` after code changes (don't read output)
2. **Minimal changes** - Only change what was requested, nothing more
3. **Functional only** - Arrow functions everywhere (see any file in `src/`)
4. **Type-safe paths** - DeepKey<T> and DeepValue<T, P> (see `src/types/`)
5. **Automatic tracking** - Use valtio-reactive's effect() (see `src/concerns/registration.ts`)
6. **Two-proxy pattern** - Read from state, write to _concerns (prevents infinite loops)
7. **Pure concerns** - No mutations, no side-effects (see `src/concerns/prebuilts/` for examples)
8. **Always cleanup** - Return cleanup functions (see `src/concerns/registration.ts:84-110`)
9. **Start simple** - Add complexity only when needed

---

## Workflow After Making Changes

1. **Write/edit code** - Make the requested changes
2. **Format code** - Run `npm run code:fix` (don't read output)
3. **Verify** - Check that files are properly formatted
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
