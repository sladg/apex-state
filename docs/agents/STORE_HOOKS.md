# Store Hooks - Reference Guide

Navigation guide for all hooks. **Read the actual implementations in `src/store/createStore.ts` for details.**

---

## 🚨 IMPORTANT: Do NOT Modify Hook Signatures

These hooks are public API:
- Do NOT change hook signatures without user approval
- Do NOT rename hooks
- Do NOT refactor implementations unless there's a bug

If you see improvements: fix bugs if requested, then suggest improvements to user.

### After Making Code Changes

**ALWAYS run** (don't read output):
```bash
npm run code:fix
```

This formats code with ESLint + Prettier. Required for all code changes.

---

## All Hooks Are Defined In One File

**File**: `src/store/createStore.ts:63-260`

**Read the actual implementation** - it has TSDoc comments explaining each hook.

---

## Hook Reference

| Hook | Line Range | Purpose |
|------|------------|---------|
| `useStore` | 85-106 | useState-like API for single path |
| `useJitStore` | 111-135 | Bulk operations, non-reactive reads |
| `useFieldStore` | 152-176 | Object-style API `{ value, setValue }` |
| `useFieldTransformedStore` | 178-221 | Bidirectional transform (e.g., units, dates) |
| `useConcerns` | 228-243 | Register reactive validation/UI logic |
| `useFieldConcerns` | 250-258 | Read concern results for a path |
| `useSideEffects` | 141-147 | Register side-effects (sync, aggregation, listeners) |

**All implementations are in**: `src/store/createStore.ts`

---

## Hook Patterns

### All Hooks Follow Same Pattern

1. Get store from context: `useStoreContext()`
2. Use reactive snapshot: `useSnapshot(store.state)`
3. Extract/compute value
4. Provide setter (if applicable)

**Read**: `src/store/createStore.ts:85-106` for the canonical example (useStore)

### Type Safety

All hooks use `DeepKey<DATA>` and `DeepValue<DATA, P>` for type-safe paths.

**Type definitions**:
- `src/types/deepKey.ts` - Path generation
- `src/types/deepValue.ts` - Value extraction

---

## Usage Examples

### Finding Examples

**DO NOT** copy examples from this file. Instead:

1. **Hook usage in tests**: `tests/integration/*.test.tsx`
2. **Type usage**: Look at hook signatures in `src/store/createStore.ts`
3. **Patterns**: Read existing test files

### Where Each Hook Is Used

| Hook | Test Files |
|------|------------|
| `useStore` | `tests/integration/*.test.tsx` (all files use it) |
| `useFieldStore` | Search for `useFieldStore` in tests |
| `useConcerns` | `tests/integration/*.test.tsx` (most files use it) |
| `useFieldConcerns` | `tests/integration/*.test.tsx` (most files use it) |
| `useJitStore` | Search for `useJitStore` in tests |

---

## Quick Hook Comparison

| Hook | Returns | Use When |
|------|---------|----------|
| `useStore` | `[value, setValue]` | useState-like API |
| `useFieldStore` | `{ value, setValue }` | Object-style API |
| `useFieldTransformedStore` | `{ value, setValue }` | Need format conversion |
| `useJitStore` | `{ proxyValue, setChanges, getState }` | Bulk operations |
| `useConcerns` | `void` | Registering validation/UI logic |
| `useFieldConcerns` | `EvaluatedConcerns` | Reading concern results |
| `useSideEffects` | `void` | Cross-field sync/aggregation |

---

## Type Signatures

**Read the actual file**: `src/store/createStore.ts`

Each hook has:
- Full type signature
- TSDoc comments
- Implementation details

**DO NOT** replicate type signatures here - they will become outdated. Read the source.

---

## Common Usage Patterns

### Pattern: Form Validation

**Example test**: Search `tests/integration/` for "validation" or "useConcerns"

**Files involved**:
- Hook: `src/store/createStore.ts` (useConcerns, useFieldConcerns)
- Concerns: `src/concerns/prebuilts/zodValidation.ts`
- Logic: `src/concerns/registration.ts`

### Pattern: Bulk Updates

**Example test**: Search `tests/integration/` for "useJitStore" or "setChanges"

**Implementation**: `src/store/createStore.ts:111-135`

### Pattern: Conditional UI

**Example test**: Search `tests/integration/` for "disabledWhen"

**Files involved**:
- Hook: `src/store/createStore.ts` (useConcerns, useFieldConcerns)
- Concern: `src/concerns/prebuilts/disabledWhen.ts`
- Logic: `src/utils/boolLogic.ts`

### Pattern: Dynamic Content

**Example test**: Search `tests/integration/` for "dynamicTooltip"

**Files involved**:
- Hook: `src/store/createStore.ts` (useConcerns, useFieldConcerns)
- Concern: `src/concerns/prebuilts/dynamicTooltip.ts`
- Logic: `src/utils/interpolation.ts`

---

## Side-Effects System

### SideEffects Type

**Definition**: `src/types/sideEffects.ts`

Contains:
- `syncPaths` - Keep two paths in sync
- `flipPaths` - Boolean flip between paths
- `aggregations` - Compute derived values
- `listeners` - React to changes

### Registration

**Hook**: `src/store/createStore.ts:141-147`
**Implementation**: `src/sideEffects/registration.ts`
**Execution**: `src/store/executor.ts`

### Examples

**DO NOT** copy examples. Instead:
- Read `src/sideEffects/registration.ts` for registration logic
- Read `src/store/executor.ts` for execution logic
- Search `tests/` for "useSideEffects" to see real usage

---

## Testing Hooks

### Test Examples

**Unit tests**: `tests/concerns/*.test.ts`
**Integration tests**: `tests/integration/*.test.tsx`

### Mock Store

**Location**: `tests/mocks/`

**Usage**: Import and use in tests

---

## When Working on Hooks

1. **Read the implementation first**: `src/store/createStore.ts`
2. **Check TSDoc comments**: Implementation details in code
3. **Look at tests**: They show real usage
4. **Make minimal changes**: Don't refactor unless asked
5. **Test your changes**: Add/update tests in `tests/integration/`
6. **Format code**: Run `npm run code:fix` after changes (don't read output)

---

## Hook Implementation Details

### Context Access

All hooks use: `useStoreContext()` from `src/hooks/useStoreContext.ts`

### Reactive Snapshots

All hooks use: `useSnapshot(store.state)` from valtio

### Change Processing

All setters call: `processChanges()` from `src/store/executor.ts`

### Type Extraction

All hooks use: `deepGet()` from `src/store/utils/deepAccess.ts`

---

## Quick Checklist

When using hooks:
- [ ] Using type-safe paths (DeepKey<DATA>)?
- [ ] Returning cleanup from useConcerns/useSideEffects?
- [ ] Following patterns from existing tests?
- [ ] Not duplicating code from docs?

When debugging hooks:
- [ ] Is Provider wrapping the component?
- [ ] Is the path valid (check DeepKey type)?
- [ ] Are concerns registered before reading results?
- [ ] Is useSnapshot being used for reactive reads?

---

## Summary

**Source of truth**: `src/store/createStore.ts`

**Best examples**: Tests in `tests/integration/*.test.tsx`

**Type system**: `src/types/deepKey.ts`, `src/types/deepValue.ts`

**Side-effects**: `src/sideEffects/registration.ts`, `src/store/executor.ts`

**Don't**: Copy examples from docs, modify public API without permission, refactor working code
