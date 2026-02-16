# Test Utilities Refactoring Plan

## Status: ✅ PARTIALLY COMPLETE

**Completed:**

1. ✅ Built-in `_debug` tracking on StoreInstance (`debug.track: true`)
2. ✅ Testing patterns documented (`tests/utils/TESTING_PATTERNS.md`)
3. ✅ Verified existing patterns are correct

**Key Learning:**
The existing `setValue` implementation **is correct** and matches production code. Both production and test code call `processChanges` directly - this is intentional design, not a bug.

---

## What We Learned

### `setValue` Is Correct As-Is ✅

**Initial assumption (WRONG):**
> `setValue` should use Proxy traps: `storeInstance.state.x = value`

**Reality (RIGHT):**
> Production code also calls `processChanges` directly. The store doesn't rely on Proxy traps for programmatic updates.

**Production code** (`src/store/createStore.ts`):

```typescript
const setValue = (newValue, meta) => {
  const changes = [[path, newValue, meta]]
  processChanges(store, changes)
}
```

**Test helper** (`tests/utils/react.tsx`):

```typescript
const setValue = (path, value, meta) => {
  act(() => {
    const changes = [[path, value, meta]]
    const processChanges = config.useLegacyImplementation
      ? processChangesLegacy
      : processChangesWasm
    processChanges(storeInstance, changes)
  })
}
```

Both implementations are identical in behavior. **No change needed.**

---

## Remaining Work (Optional)

### 1. Remove `createTestStore` if Unused

**Location:** `tests/utils/react.tsx` lines 73-157

**Action:** Check if any tests use it:

```bash
grep -r "createTestStore" tests/
```

If unused, delete it. If used, migrate those tests to use real `createGenericStore`.

### 2. Use `_debug` Tracking Instead of Custom Spies

**Status: ✅ AVAILABLE**

`createGenericStore` now supports `debug.track: true` which records all `processChanges` calls and their effects directly on the store instance. This replaces the need for most spy utilities.

**Setup:**

```typescript
const store = createGenericStore<TestState>({ debug: { track: true } })
// ... render with mountStore ...
```

**Available on `storeInstance._debug`:**

- `calls` — append-only log of all `processChanges` invocations
  - `input` — `[path, value, meta]` tuples passed in
  - `applied` — `{path, value}` changes written to `store.state`
  - `appliedConcerns` — `{path, value}` changes written to `store._concerns`
  - `timestamp` — when the call occurred
- `clear()` — reset tracking between tests

**Example usage in tests:**

```typescript
setValue('user.email', 'alice@example.com')
const lastCall = storeInstance._debug!.calls.at(-1)!
expect(lastCall.applied).toContainEqual({ path: 'user.email', value: 'alice@example.com' })
expect(lastCall.appliedConcerns).toHaveLength(1) // validation fired
storeInstance._debug!.clear() // reset for next assertion
```

**Replaces all previous spy utilities:**

- `spyWasm()` — no longer needed, `_debug.calls` tracks all processChanges invocations
- `getConcernResults()` — no longer needed, `_debug.calls[n].appliedConcerns` or `storeInstance._concerns[path]` directly

---

## Final Architecture

### What's Good Now ✅

1. **`mountStore`** - Uses real `createGenericStore`, wraps in Provider
2. **`setValue`** - Matches production pattern (calls `processChanges`)
3. **`_debug` tracking** - Built-in change tracking via `debug.track: true` config flag
4. **Documentation** - Clear patterns documented in `TESTING_PATTERNS.md`

### What to Avoid ❌

1. Setting state via Proxy: `storeInstance.state.x = value`
2. Mocking `processChanges` in integration tests
3. Creating custom test stores
4. Custom spies when `_debug` tracking covers the same ground

---

## Summary

**The recent sync bug was NOT caused by test utilities.** The test utilities were correctly calling `processChanges` - the bug was in the WASM implementation itself.

**Deliverables:**

- ✅ Built-in `_debug` tracking on StoreInstance (`debug.track: true`)
- ✅ Comprehensive testing patterns documentation
- ✅ Refactoring plan with lessons learned

**Next Steps (Optional):**

- Remove `createTestStore` if unused
