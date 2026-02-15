# Integration Tests v2 → v1 Migration Plan

**Purpose**: This document maps v2 integration tests to existing v1 tests that can be removed once v2 tests are fully implemented.

---

## Summary Table

| v2 Test File | Status | Replaces (v1 Tests) |
|--------------|--------|---------------------|
| `sync-paths.test.tsx` | 🔴 Skeleton | `tests/integration/sync-paths.test.tsx` (ENTIRE FILE)<br>`tests/integration/pipeline-sync-flip-listeners.test.tsx` (sync tests, lines ~1-150) |
| `flip-paths.test.tsx` | 🔴 Skeleton | `tests/integration/pipeline-sync-flip-listeners.test.tsx` (flip tests, lines ~151-300) |
| `hooks-api.test.tsx` | 🔴 Skeleton | `tests/store/createStore.test.tsx` (hook tests, lines ~50-400) |
| `combined-effects.test.tsx` | 🔴 Skeleton | `tests/integration/pipeline-sync-flip-listeners.test.tsx` (combined, lines ~301-500)<br>`tests/integration/side-effects.test.tsx` (coordination, lines ~150-300)<br>`tests/integration/complex-workflows.test.tsx` (cascading, lines ~1-200) |
| `listeners.test.tsx` | 🔴 Skeleton | `tests/integration/side-effects.test.tsx` (listeners, lines ~1-150 + ~300-450)<br>`tests/integration/pipeline-sync-flip-listeners.test.tsx` (dispatch, lines ~501-650) |
| `provider.test.tsx` | 🔴 Skeleton | `tests/store/provider.test.tsx` (ENTIRE FILE)<br>`tests/integration/basic.test.tsx` (Provider tests, lines ~50-150) |
| `withConcerns.test.tsx` | 🔴 Skeleton | `tests/integration/withConcerns.test.tsx` (ENTIRE FILE)<br>`tests/integration/concerns-ui.test.tsx` (filtering, lines ~100-200) |

**Legend**:

- 🔴 Skeleton = Test structure exists, needs implementation
- 🟡 Partial = Some tests implemented
- 🟢 Complete = All tests implemented and passing

---

## Detailed Mapping

### 1. `sync-paths.test.tsx`

**Replaces:**

- ✅ `tests/integration/sync-paths.test.tsx` — **ENTIRE FILE**
  - All basic sync path test cases
  - Multiple sync pairs
  - Circular sync handling
  - Sync lifecycle tests

- ✅ `tests/integration/pipeline-sync-flip-listeners.test.tsx` — **Sync-specific tests**
  - Lines ~1-150 (approx)
  - Basic sync behavior
  - Sync registration
  - Sync with other changes

**What v2 adds:**

- More comprehensive edge cases
- Better lifecycle coverage
- Clearer test organization

---

### 2. `flip-paths.test.tsx`

**Replaces:**

- ✅ `tests/integration/pipeline-sync-flip-listeners.test.tsx` — **Flip-specific tests**
  - Lines ~151-300 (approx)
  - Basic flip behavior
  - Multiple flip pairs
  - Circular flip handling

**What v2 adds:**

- Boolean type validation
- Non-boolean truthy/falsy handling
- Better flip semantics testing

---

### 3. `hooks-api.test.tsx`

**Replaces:**

- ✅ `tests/store/createStore.test.tsx` — **Hook test cases**
  - Lines ~50-200: `useFieldStore` tests
  - Lines ~201-300: `useStore` tests
  - Lines ~301-400: `useJitStore` tests

**What v2 adds:**

- Comprehensive hook interaction tests
- More edge cases for each hook variant
- Better type safety validation

---

### 4. `combined-effects.test.tsx`

**Replaces:**

- ✅ `tests/integration/pipeline-sync-flip-listeners.test.tsx` — **Combined effects**
  - Lines ~301-500 (approx)
  - Sync + Flip together
  - Sync + Listeners
  - All three combined

- ✅ `tests/integration/side-effects.test.tsx` — **Multi-effect coordination**
  - Lines ~150-300 (approx)
  - Effect execution order
  - State consistency

- ✅ `tests/integration/complex-workflows.test.tsx` — **Cascading effects**
  - Lines ~1-200 (approx)
  - Complex cascade scenarios
  - Real-world workflows

**What v2 adds:**

- More realistic scenarios (shopping cart, form validation, e-commerce checkout)
- Better performance testing
- Error handling coverage

---

### 5. `listeners.test.tsx`

**Replaces:**

- ✅ `tests/integration/side-effects.test.tsx` — **Listener tests**
  - Lines ~1-150: Basic listener execution
  - Lines ~300-450: Listener lifecycle

- ✅ `tests/integration/pipeline-sync-flip-listeners.test.tsx` — **Listener dispatch**
  - Lines ~501-650 (approx)
  - Depth-ordered execution
  - Listener coordination

**What v2 adds:**

- Comprehensive "when NOT called" coverage
- Better scope and path testing
- Concurrency testing with other effects

---

### 6. `provider.test.tsx`

**Replaces:**

- ✅ `tests/store/provider.test.tsx` — **ENTIRE FILE**
  - All Provider component tests
  - Context isolation tests
  - Provider lifecycle tests

- ✅ `tests/integration/basic.test.tsx` — **Provider-specific tests**
  - Lines ~50-150 (approx)
  - Provider context setup
  - Hook access through Provider

**What v2 adds:**

- Better isolation testing (multiple providers, nested providers)
- TypeScript type safety validation
- Error handling (hooks without Provider)

---

### 7. `withConcerns.test.tsx`

**Replaces:**

- ✅ `tests/integration/withConcerns.test.tsx` — **ENTIRE FILE**
  - All concern filtering tests
  - Filtering scope tests

- ✅ `tests/integration/concerns-ui.test.tsx` — **withConcerns-specific tests**
  - Lines ~100-200 (approx)
  - Concern filtering behavior
  - Filtered store interactions

**What v2 adds:**

- Better filtering combination tests
- Filtering with useSideEffects validation
- Real-world filtering scenarios

---

## Migration Workflow

### Phase 1: Implementation (Per Test File)

1. Pick a v2 test file (start with simplest: `sync-paths.test.tsx`)
2. Implement test cases (replace skeleton `it()` with actual code)
3. Run tests, ensure they pass
4. Mark as 🟡 Partial or 🟢 Complete in summary table

### Phase 2: Validation

1. Run both v1 and v2 tests in parallel
2. Ensure v2 tests cover all v1 test cases
3. Verify no regressions
4. Document any edge cases v2 doesn't cover (if any)

### Phase 3: Cleanup

1. Once v2 test is 🟢 Complete, remove corresponding v1 tests
2. Update test runner configuration if needed
3. Verify all tests still pass
4. Update this document to mark v1 tests as ❌ Removed

---

## Notes

### Files to Keep (NOT replaced by v2)

- `tests/utils/**/*.test.ts` — Unit tests for utilities
- `tests/types/**/*.test.ts` — Type-level tests
- `tests/wasm/**/*.test.ts` — WASM-specific tests
- `tests/pipeline/*.test.ts` — Low-level pipeline tests
- `tests/concerns/*.test.ts` — Concern-specific unit tests
- `tests/hooks/*.test.ts` — Hook-specific utility tests

### Files to Remove (Eventually)

- `tests/integration/sync-paths.test.tsx`
- `tests/integration/pipeline-sync-flip-listeners.test.tsx`
- `tests/integration/side-effects.test.tsx`
- `tests/integration/complex-workflows.test.tsx` (partially)
- `tests/integration/withConcerns.test.tsx`
- `tests/integration/basic.test.tsx` (Provider tests only)
- `tests/integration/concerns-ui.test.tsx` (withConcerns tests only)
- `tests/store/provider.test.tsx`
- `tests/store/createStore.test.tsx` (hook tests only, keep other tests)

---

## Tracking Progress

**Last Updated**: 2026-02-15

**Status**:

- ✅ All v2 test files created with skeletons
- ✅ Documentation added to each v2 test file
- ✅ Migration plan documented
- ⏳ Implementation pending
- ⏳ Validation pending
- ⏳ v1 test removal pending

**Next Steps**:

1. Implement `sync-paths.test.tsx` (simplest, good starting point)
2. Implement `flip-paths.test.tsx` (similar to sync paths)
3. Implement `listeners.test.tsx` (more complex)
4. Implement `hooks-api.test.tsx` (API surface testing)
5. Implement `provider.test.tsx` (context testing)
6. Implement `combined-effects.test.tsx` (most complex)
7. Implement `withConcerns.test.tsx` (filtering logic)
