# CONCERNS System - Agent Guide

Navigation guide for working on the concerns system. **All examples are in the actual source files.**

---

## 🚨 IMPORTANT: Minimal Changes Only

- Only make changes that were explicitly requested
- Do NOT refactor existing concerns unless asked
- Do NOT restructure files or rename things
- If you see improvements needed: finish the task FIRST, then mention to user

### After Making Code Changes

**ALWAYS run** (don't read output):
```bash
npm run code:fix
```

This formats code with ESLint + Prettier. Required for all code changes.

---

## What Are Concerns?

Concerns are reactive computations that automatically track dependencies via `valtio-reactive`'s `effect()`.

**Examples**: zodValidation, disabledWhen, visibleWhen, dynamicTooltip

**How they work**: See `src/concerns/registration.ts:54-76` for the core mechanism.

---

## Key Files

| File | Purpose |
|------|---------|
| `src/concerns/types.ts` | `ConcernType<EXTRA_PROPS, RETURN_TYPE>` interface |
| `src/concerns/registration.ts` | How concerns are wrapped in `effect()` for automatic tracking |
| `src/concerns/registry.ts` | `findConcern()` utility |
| `src/concerns/index.ts` | `defaultConcerns` array (register new concerns here) |
| `src/concerns/prebuilts/*.ts` | All built-in concern implementations |
| `src/utils/boolLogic.ts` | `evaluateBoolLogic()` for conditional concerns |
| `src/utils/interpolation.ts` | Template string interpolation for dynamic content |

---

## The Two-Proxy Architecture (Critical!)

**Problem**: If concerns read and write to the same proxy → infinite loops

**Solution**: Two separate proxies
- `store.state` - Concerns READ from here
- `store._concerns` - Concerns WRITE to here

**See implementation**: `src/concerns/registration.ts:54-76`

**Detailed explanation**: `docs/agents/ARCHITECTURE.md`

---

## Concern Anatomy

**Interface**: `src/concerns/types.ts:29-44`

Every concern has:
- `name: string` - Unique identifier
- `description: string` - Human-readable explanation
- `evaluate: (props) => RETURN_TYPE` - Computation function

**evaluate() receives** (`src/concerns/types.ts:14-21`):
- `state: DATA` - Full state proxy (for cross-field access)
- `path: string` - Path being evaluated
- `value: unknown` - Value at the path
- `...config` - Additional props from registration

---

## Creating a New Concern

### Step 1: Define the Concern

**Simple example**: `src/concerns/prebuilts/zodValidation.ts`
**With BoolLogic**: `src/concerns/prebuilts/disabledWhen.ts`
**With interpolation**: `src/concerns/prebuilts/dynamicTooltip.ts`

### Step 2: Add to defaultConcerns

**File**: `src/concerns/index.ts`

Add your concern to the array.

### Step 3: Test

**Unit test example**: `tests/concerns/` (any file)
**Integration test example**: `tests/integration/` (any file)

---

## How Dependency Tracking Works

**Core mechanism**: `valtio-reactive`'s `effect()` automatically tracks ALL property accesses during evaluation.

**Implementation**: `src/concerns/registration.ts:54-76`

**Key insight**: Any `deepGet(props.state, path)` call inside `evaluate()` is automatically tracked. When those paths change, the concern re-evaluates.

**Why NOT derive-valtio**: See `CONCERNS_REFERENCE.md` - "What We Tried That DIDN'T Work"

---

## Built-in Concerns Reference

All implementations in `src/concerns/prebuilts/`

### zodValidation
**File**: `src/concerns/prebuilts/zodValidation.ts`
**Purpose**: Zod schema validation
**Returns**: `boolean` (true = valid)

### disabledWhen / visibleWhen / readonlyWhen
**Files**: `src/concerns/prebuilts/disabledWhen.ts`, `visibleWhen.ts`, `readonlyWhen.ts`
**Purpose**: Conditional UI state using BoolLogic DSL
**Returns**: `boolean`
**BoolLogic evaluator**: `src/utils/boolLogic.ts`

### dynamicTooltip / dynamicLabel / dynamicPlaceholder
**Files**: `src/concerns/prebuilts/dynamicTooltip.ts`, `dynamicLabel.ts`, `dynamicPlaceholder.ts`
**Purpose**: String interpolation with `{{path}}` syntax
**Returns**: `string`
**Interpolator**: `src/utils/interpolation.ts`

---

## BoolLogic Operators

**Type definition**: `src/types/concerns.ts`
**Evaluator**: `src/utils/boolLogic.ts`

**Available operators**:
- Comparison: `IS_EQUAL`, `GT`, `LT`, `GTE`, `LTE`, `IN`
- Existence: `EXISTS`, `IS_EMPTY`
- Boolean: `AND`, `OR`, `NOT`
- Meta: `HAS_CONCERN`

**See usage**: `src/concerns/prebuilts/disabledWhen.ts`

---

## Critical DOs and DONTs

### ✅ DO

1. **Keep evaluate() pure** - No mutations, no side-effects
2. **Use deepGet() for cross-field access** - Automatic tracking
3. **Return correct type** - Match your `ConcernType<..., RETURN_TYPE>` declaration
4. **Follow existing patterns** - Look at `src/concerns/prebuilts/` for examples

### ❌ DON'T

1. **Never mutate state in evaluate()** - Use side-effects system instead
2. **Never read from _concerns inside evaluate()** - Use `HAS_CONCERN` in BoolLogic
3. **Never use manual tracks()** - effect() handles it automatically
4. **Never use async in evaluate()** - Must be synchronous
5. **Never use derive() from derive-valtio** - Use effect() from valtio-reactive
6. **Never evaluate without checking concern exists** - Use `findConcern()` from registry

---

## Registration Flow

**User code**: Calls `store.useConcerns()` in component
**Hook**: `src/store/createStore.ts:228-243`
**Registration**: `src/concerns/registration.ts:27-111`
**Cleanup**: `src/concerns/registration.ts:84-110`

**Read the actual code** to understand the flow - it's well-documented.

---

## Testing Patterns

### Unit Test Example
**See**: Any file in `tests/concerns/*.test.ts`

### Integration Test Example
**See**: Any file in `tests/integration/*.test.tsx`

### Mock Store
**See**: `tests/mocks/`

---

## Common Patterns

**All patterns are implemented in the codebase. Find them by:**

1. **Conditional validation** - `src/concerns/prebuilts/zodValidation.ts` (check for `condition` prop)
2. **Cross-field validation** - `src/concerns/prebuilts/disabledWhen.ts` (uses BoolLogic)
3. **Scope validation** - `src/concerns/prebuilts/zodValidation.ts` (check for `scope` prop)
4. **Dynamic content** - `src/concerns/prebuilts/dynamicTooltip.ts`

**DO NOT copy examples from docs** - read the actual implementation.

---

## When Working on Concerns

1. **Read the source file first** - Understand the existing implementation
2. **Check TSDoc comments** - Implementation details are documented in code
3. **Look at similar concerns** - Follow established patterns
4. **Check tests** - They show real usage
5. **Make minimal changes** - Don't refactor unless asked
6. **Format code** - Run `npm run code:fix` after changes (don't read output)

---

## Quick Checklist

When creating a concern:
- [ ] Used arrow function syntax?
- [ ] Defined `ConcernType<EXTRA_PROPS, RETURN_TYPE>` with proper generics?
- [ ] evaluate() is pure (no mutations)?
- [ ] evaluate() is synchronous?
- [ ] Added to `defaultConcerns` array in `src/concerns/index.ts`?
- [ ] Using `deepGet()` for cross-field access?
- [ ] Tested in isolation?
- [ ] Tested integration with store?

When debugging:
- [ ] Is it registered in `defaultConcerns`?
- [ ] Is `useConcerns()` being called?
- [ ] Is cleanup being returned from `useEffect`?
- [ ] Are dependencies actually changing?
- [ ] Is evaluate() returning the expected type?

---

## Summary

**Source of truth**: The actual implementation files in `src/concerns/`

**Best examples**: `src/concerns/prebuilts/*.ts`

**How it works**: `src/concerns/registration.ts`

**Don't**: Copy examples from docs, refactor without permission, guess new patterns
