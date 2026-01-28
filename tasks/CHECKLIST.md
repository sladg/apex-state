# Implementation Checklist

## 📋 Pre-Implementation (Before Any Code)

- [ ] Read `00-ARCHITECTURE-UPDATE.md` in full
- [ ] Read `QUICK-REFERENCE.md` for patterns
- [ ] Understand the __internal state pattern
- [ ] Know when to STOP and ask for guidance
- [ ] Review `any` type rules

## 🎯 Per-Task Checklist

### Before Starting Task:
- [ ] Read the task file completely
- [ ] Check all dependencies are complete
- [ ] Understand the Worker Prompt (your role)
- [ ] Review Acceptance Criteria
- [ ] Note Expected Output (files/exports)
- [ ] Plan implementation approach

### During Implementation:
- [ ] ✅ Using factory functions (NOT classes)
- [ ] ✅ Avoiding `any` type (using `unknown` + type guards)
- [ ] ✅ Using graphology for graphs (NOT custom implementations)
- [ ] ✅ Using remeda `pipe` for multi-step operations
- [ ] ✅ Storing framework state in `__internal`
- [ ] ✅ Writing pure functions where possible
- [ ] ✅ Using immutable update patterns
- [ ] ❌ NOT guessing at unclear requirements
- [ ] ❌ NOT using `any` to bypass TypeScript errors

### If You Get Stuck:
- [ ] Document the blocker clearly
- [ ] List possible approaches/options
- [ ] Explain why you're stuck
- [ ] STOP and ask the user
- [ ] DON'T use `any` as a workaround
- [ ] DON'T make assumptions

### After Implementation:
- [ ] All Acceptance Criteria met
- [ ] All Expected Output files created
- [ ] Types are correct and exported
- [ ] No TypeScript errors
- [ ] No `any` types (except where genuinely necessary)
- [ ] Tests written and passing
- [ ] Run verification steps from task file
- [ ] Code follows functional patterns

## 🧪 Testing Checklist

### For Each Feature:
- [ ] Use-case test written (not just unit tests)
- [ ] Edge cases covered
- [ ] Type error tests use `@ts-expect-error`
- [ ] Tests pass consistently
- [ ] Realistic test data used

### Type Error Tests:
```typescript
// ✅ CORRECT
// @ts-expect-error - Testing invalid path
const [value] = store.useStore('bad.path')

// ❌ WRONG
const [value] = store.useStore('bad.path' as any)
```

## ⚡ Performance Checklist (Tasks 06, 09, 14)

### For Performance-Critical Code:
- [ ] Using graphology's optimized algorithms
- [ ] Caching expensive computations
- [ ] Invalidating cache on changes
- [ ] Early exit when no work needed
- [ ] Profiled before optimizing
- [ ] Benchmarks written and passing
- [ ] Performance targets met:
  - [ ] Sync paths: < 1ms per change
  - [ ] Aggregations: < 2ms per change
  - [ ] Full pipeline: < 5ms per change

## 📦 Code Review Self-Check

### Type Safety:
- [ ] No `any` types used carelessly
- [ ] Generics properly constrained
- [ ] Return types explicit where needed
- [ ] Type guards used for `unknown` values
- [ ] Test types with `@ts-expect-error` not `as any`

### Functional Patterns:
- [ ] Factory functions (not classes)
- [ ] Pure functions where possible
- [ ] Immutable updates (spread operators)
- [ ] Composition over complex logic
- [ ] Pipe for multi-step operations

### Architecture:
- [ ] Framework state in `__internal`
- [ ] User state in `values`
- [ ] graphology for graphs
- [ ] remeda for pipes
- [ ] No custom graph algorithms

### Code Quality:
- [ ] Clear variable/function names
- [ ] Functions are small and focused
- [ ] Complex logic documented
- [ ] No dead code
- [ ] Imports organized

## 🚀 Phase Completion Checklist

### After Each Phase:
- [ ] All phase tasks marked complete
- [ ] All phase tests passing
- [ ] Integration between tasks verified
- [ ] Package builds successfully: `npm run build`
- [ ] Type check passes: `npm run type-check`
- [ ] No TypeScript errors
- [ ] Ready to proceed to next phase

## ✅ Project Completion Checklist

### Final Verification:
- [ ] All 49 tasks complete
- [ ] All tests passing: `npm test`
- [ ] Performance benchmarks pass
- [ ] Type check passes: `npm run type-check`
- [ ] Build successful: `npm run build`
- [ ] Bundle size reasonable
- [ ] Example usage works
- [ ] TSDoc comments added
- [ ] README.md complete

### Code Quality:
- [ ] No `any` types except where necessary
- [ ] All classes replaced with factory functions
- [ ] All graphs use graphology
- [ ] All pipes use remeda
- [ ] Functional patterns throughout
- [ ] Test coverage adequate

### Documentation:
- [ ] TSDoc on all exported types
- [ ] TSDoc on all hooks
- [ ] TSDoc on createGenericStore
- [ ] README with examples
- [ ] Performance characteristics documented

## 📝 Daily Checklist

### Start of Day:
- [ ] Review previous day's work
- [ ] Check current task file
- [ ] Review QUICK-REFERENCE.md patterns
- [ ] Plan day's tasks

### End of Day:
- [ ] Run tests: `npm test`
- [ ] Run type check: `npm run type-check`
- [ ] Commit work (if complete and working)
- [ ] Update task status in README.md
- [ ] Note any blockers for tomorrow

## 🛑 Stop and Ask Checklist

Stop immediately and ask the user if:
- [ ] You're about to use `any` for complex types
- [ ] Multiple approaches seem equally valid
- [ ] Requirements are unclear
- [ ] Edge case behavior is undefined
- [ ] Significant performance trade-offs exist
- [ ] Breaking changes might be needed
- [ ] Architectural decisions are needed

## 📞 How to Ask for Help

```typescript
// TODO: BLOCKED - Need user guidance
// Question: [State the specific question clearly]
//
// Context: [Why is this important/blocking]
//
// Options considered:
//   A) [First option with pros/cons]
//   B) [Second option with pros/cons]
//   C) [Third option with pros/cons]
//
// Current blocker: [What's preventing you from proceeding]
//
// Recommendation: [If you have one]
```

## 🎯 Quality Standards

### Every Function:
- [ ] Does one thing well
- [ ] Has clear name
- [ ] Pure if possible
- [ ] Type-safe (no `any`)
- [ ] Tested

### Every Type:
- [ ] Well-named
- [ ] Documented with TSDoc
- [ ] No `any` unless necessary
- [ ] Proper generics
- [ ] Tested with type tests

### Every Test:
- [ ] Tests use case, not implementation
- [ ] Realistic scenario
- [ ] Clear test name
- [ ] Edge cases covered
- [ ] Passes consistently

---

**Print this checklist and keep it handy while implementing!** ✅
