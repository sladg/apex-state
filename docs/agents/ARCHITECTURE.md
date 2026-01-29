# Architecture Guide - Apex State

Navigation guide for understanding the core architecture. **Read the actual source files for implementation details.**

---

## 🚨 IMPORTANT: Do NOT Refactor Architecture

This architecture is intentional and battle-tested. If you think something should be changed:
1. **STOP** - Do not make architectural changes
2. **EXPLAIN** - Describe the issue to the user
3. **ASK** - Get explicit permission
4. **DOCUMENT** - If approved, document why

### After Making Code Changes

**ALWAYS run** (don't read output):
```bash
npm run code:fix
```

This formats code with ESLint + Prettier. Required for all code changes.

---

## Core Architecture Files

| File | Purpose |
|------|---------|
| `src/store/types.ts` | `StoreInstance`, `InternalState`, all type definitions |
| `src/store/Provider.tsx` | How store proxy is created on mount |
| `src/store/createStore.ts` | Store factory, all hooks |
| `src/concerns/registration.ts` | How effect() wrapping works |
| `src/store/executor.ts` | Change processing pipeline |
| `src/types/deepKey.ts` | Type-safe path generation |
| `src/types/deepValue.ts` | Type-safe value extraction |

---

## The Two-Proxy System

### Why Two Proxies?

**Problem**: Reading and writing to the same proxy inside `effect()` causes infinite loops.

**Solution**: Separate proxies for reading and writing.

**Implementation**: `src/concerns/registration.ts:54-76`

**Type definition**: `src/store/types.ts:193-221`

### StoreInstance Structure

```
StoreInstance {
  state: proxy({ ...data })          // User data (READ from here)
  _concerns: proxy({ ...computed })  // Concern results (WRITE to here)
  _internal: ref({ ...graphs })      // Internal state (NOT tracked)
}
```

**Key Files**:
- Type definition: `src/store/types.ts:193-221`
- Creation: `src/store/Provider.tsx:45-76`
- Usage in concerns: `src/concerns/registration.ts:54-76`

---

## Dependency Tracking

### valtio-reactive's effect()

**Why effect() and NOT derive()**:
- `effect()` = Property-level tracking (fine-grained)
- `derive()` = Proxy-level tracking (too coarse)

**Full explanation**: `CONCERNS_REFERENCE.md` - "What We Tried That DIDN'T Work"

**Implementation**: `src/concerns/registration.ts:54-76`

### How It Works

Read the source file `src/concerns/registration.ts` for the implementation. The TSDoc comments explain:
- How `effect()` wraps concern evaluation
- What gets tracked
- Why it prevents infinite loops

---

## The _internal Object

**Purpose**: Internal state that shouldn't trigger React re-renders

**Implementation**: Wrapped in `ref()` from valtio

**Type definition**: `src/store/types.ts:158-176`

**Contains**:
- `graphs` - Side-effect processing graphs (graphology)
- `registrations` - Cleanup tracking
- `processing` - Change queue, reentrancy guard

**Created in**: `src/store/Provider.tsx:45-76`

**Used in**: `src/store/executor.ts` (change processing)

---

## Change Processing Pipeline

**Flow**:
```
User mutation
  → Valtio proxy
  → Batched by Valtio
  → processChanges() [if side-effects registered]
  → Concerns re-evaluate [automatic via effect()]
  → React re-renders [via useSnapshot]
```

**Implementation**: `src/store/executor.ts`

**Side-effect registration**: `src/sideEffects/registration.ts`

**Why iterative**: Side-effects can generate more changes (max 100 iterations prevents infinite loops)

---

## Store Creation Flow

### Factory Pattern

**File**: `src/store/createStore.ts:63-260`

Creates:
1. Provider component
2. All hooks (useStore, useConcerns, etc.)

### Provider Component

**File**: `src/store/Provider.tsx:35-135`

Creates store instance on mount with:
- `proxy()` for state and _concerns
- `ref()` for _internal

### Hook Pattern

All hooks follow same pattern:
1. Get store from context
2. Use `useSnapshot()` for reactive reads
3. Return data/setters

**Example**: `src/store/createStore.ts:85-106` (useStore)

---

## Type System

### Type-Safe Paths

**DeepKey<T>**: `src/types/deepKey.ts`
- Generates all valid dot-notation paths for a type
- Compile-time validation

**DeepValue<T, P>**: `src/types/deepValue.ts`
- Extracts the type at a given path
- Type-safe value access

**Usage**: All hooks in `src/store/createStore.ts` use these for type safety

### How They Work

**Read the implementation files** - they have detailed TSDoc comments explaining:
- How path strings are generated from types
- How TypeScript validates paths at compile time
- How value types are extracted

---

## Performance Architecture

### Batching

Valtio automatically batches changes in the same tick.

**Result**: One subscribe callback, concerns evaluate once, React re-renders once

### Deduplication

**In concerns**: `effect()` automatically deduplicates re-runs

**In side-effects**: `src/store/executor.ts` processes changes in batches

### Optimization Strategies

1. **Keep evaluate() fast** - Runs frequently
2. **Use scope wisely** - Can reduce tracked paths
3. **Use useJitStore for bulk updates** - Single batch

**See**: `src/store/createStore.ts:111-135` (useJitStore implementation)

---

## Key Architectural Decisions

### 1. Two-Proxy System
**Why**: Prevents infinite loops
**Where**: `src/concerns/registration.ts:54-76`

### 2. valtio-reactive effect()
**Why**: Property-level tracking, automatic batching
**Where**: `src/concerns/registration.ts:54-76`
**Alternative tried**: derive-valtio (failed - see `CONCERNS_REFERENCE.md`)

### 3. ref() for _internal
**Why**: Internal changes shouldn't trigger React
**Where**: `src/store/Provider.tsx:45-76`

### 4. Iterative side-effect processing
**Why**: Handles cascading changes
**Where**: `src/store/executor.ts`

### 5. Type-safe paths with DeepKey/DeepValue
**Why**: Compile-time validation, IDE autocomplete
**Where**: `src/types/deepKey.ts`, `src/types/deepValue.ts`

---

## Data Flow Summary

**User Action** → `store.state.x = y`
**Valtio** → Batches changes
**Side-effects** → `processChanges()` (if registered)
**Concerns** → Re-evaluate (automatic via effect())
**_concerns** → Updated
**React** → Re-renders (useSnapshot)

**Trace through code**:
1. Start: User calls setter from hook
2. Setter: `src/store/createStore.ts:99-103`
3. Processing: `src/store/executor.ts`
4. Concerns: Automatic re-evaluation via `effect()`
5. React: Components using `useSnapshot` re-render

---

## When Working on Architecture

1. **Read the source files** - They are well-documented
2. **Understand why before changing** - Architecture is intentional
3. **Check CONCERNS_REFERENCE.md** - Explains what didn't work and why
4. **Make minimal changes** - Don't refactor unless explicitly asked
5. **Ask permission** - Architectural changes affect entire codebase

---

## Summary

**Source of truth**: Implementation files in `src/`

**Best way to understand**: Read the actual code with TSDoc comments

**Key files**:
- `src/store/types.ts` - Type definitions
- `src/store/Provider.tsx` - Store creation
- `src/concerns/registration.ts` - Dependency tracking
- `src/store/executor.ts` - Change processing

**Don't**: Guess, refactor without permission, copy outdated examples from docs
