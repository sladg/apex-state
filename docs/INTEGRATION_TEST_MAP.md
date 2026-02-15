# Integration Test Coverage Map

## Test Files Overview

### 1. **basic.test.tsx** - Core API Integration

**Focus**: Verifies basic store functionality works end-to-end
**Test Cases**:

- `store with sync paths working` - Store creation + syncPaths side effect
- `multiple side effects registered simultaneously` - syncPaths + flipPaths together
- `hooks work correctly with Provider` - useFieldStore hook + Provider context

**Public API Surface Tested**:

- `createGenericStore<State>()`
- `store.useSideEffects('id', { syncPaths, flipPaths })`
- `store.useFieldStore(path)` → returns `{ value, setValue }`
- `<Provider>` wrapper context

---

### 2. **form-validation.test.tsx** - Form Validation with Concerns

**Focus**: Zod-based validation concerns, error UI state
**Test Cases**:

- Email format validation with validationState concern
- Password complexity validation (8+ chars, uppercase, number)
- Cross-field validation (password confirmation)
- Terms agreement (boolean validation)
- Error message display from concerns
- Auto-clear errors when field becomes valid
- Submit button visibility when all fields valid

**Public API Surface Tested**:

- `store.useConcerns('id', { field: { validationState: { schema } } })`
- `store.withConcerns({ validationState: true }).useFieldStore(path)`
- Zod schema integration with validationState concern

**Gap Identified**:

- Doesn't test custom concern registration beyond prebuilt validationState
- No testing of multiple validation schemas on single field

---

### 3. **complex-workflows.test.tsx** - Multi-Step Wizard

**Focus**: State navigation, conditional validation, progress tracking
**Test Cases**:

- Step field validation before navigation
- Validation error storage in state
- Conditionally display fields based on currentStep
- Disable navigation while validation in progress
- Display progress ("Step X of 3")
- Review step aggregates form data
- Back button behavior with validation

**Public API Surface Tested**:

- Store with nested state structure (personalInfo, addressInfo, review)
- Combined concerns + side effects in workflow
- useFieldStore with complex nested paths

---

### 4. **sync-paths.test.tsx** - Bidirectional Field Synchronization

**Focus**: Sync pair behavior, circular handling, batch operations
**Test Cases**:

- Updates fullName when firstName changes
- Updates fullName when lastName changes
- Sync pair bidirectional (displayName ↔ firstName)
- Circular sync pairs WITHOUT infinite loops
- Multiple path updates in single batch
- Sync pairs processed in correct order

**Public API Surface Tested**:

- `store.useSideEffects('id', { syncPaths: [['a', 'b']] })`
- Type-safe path pairs using typeHelpers.syncPair<State>()

---

### 5. **error-handling.test.tsx** - Error Management & Recovery

**Focus**: Error storage, display, clearing, form vs field-level
**Test Cases**:

- Store validation errors in `_errors` field
- Display errors from concerns
- Clear errors when field becomes valid
- Clear ALL errors on form reset
- Disable submit button when errors exist
- Error message templates (interpolation)
- Field-level vs form-level errors (form: '_form' key)
- Preserve errors when other fields updated

**Public API Surface Tested**:

- Manual error management via `_errors` field
- Error field storage and retrieval patterns
- Form-level vs field-level error distinction

---

### 6. **side-effects.test.tsx** - Listeners & Validators

**Focus**: Side effect listeners, validators, flipPaths, multi-effects
**Test Cases**:

- Listener updates lastUpdated on field change
- Email format validation + error storage
- Async validation (username uniqueness check)
- Clear unnecessary fields (setChanges with empty string)
- flipPaths inverts boolean relationships
- Multiple side effects execute without interference
- Side effects + concerns work together

**Public API Surface Tested**:

- `store.useSideEffects('id', { syncPaths, flipPaths })`
- `store.useJitStore()` → returns `{ proxyValue, setChanges, getState }`
- Complex multi-effect coordination

---

### 7. **aggregations.test.tsx** - Computed Values & Aggregations

**Focus**: Derived values, computed properties, cascading updates
**Test Cases**:

- Recalculate cart subtotal when item added
- Update item subtotal when quantity changes
- Update item subtotal when price changes
- (More cases in full file - only head shown)

**Public API Surface Tested**:

- Nested state structures (items: { id: { price, quantity, subtotal } })
- Computed property patterns (subtotal, tax, total)

---

### 8. **concerns-ui.test.tsx** - Dynamic UI State from Concerns

**Focus**: Conditional visibility, disabled state, dynamic labels
**Test Cases**:

- Show weight field only for physical products (visibleWhen concern)
- Show download URL field only for digital products
- (More cases in full file)

**Public API Surface Tested**:

- `store.useConcerns('id', { field: { visibleWhen: { condition } } })`
- BoolLogic condition evaluation (`IS_EQUAL`)

---

### 9. **pipeline-sync-flip-listeners.test.tsx**

**Status**: Not yet reviewed
**Expected Coverage**: Pipeline orchestration, all three side effect types together

---

### 10. **withConcerns.test.tsx**

**Status**: Not yet reviewed
**Expected Coverage**: Concern filtering, selective concern application

---

### 11. **ecommerce-catalog.test.tsx**

**Status**: Not yet reviewed
**Expected Coverage**: Real-world e-commerce scenario (products, inventory, cart)

---

### 12. **deeply-nested-pipeline.test.tsx** / **deeply-nested-execution.test.tsx**

**Status**: Not yet reviewed
**Expected Coverage**: Deep nesting, performance, boundary crossing

---

## Public API Surface for `createGenericStore<T>()`

### Return Object

```typescript
{
  Provider: React component
  useFieldStore(path: DeepKey<T>): { value, setValue }
  useStore(path: DeepKey<T>): [value, setValue]
  useJitStore(): { proxyValue, setChanges, getState }
  useSideEffects(id: string, effects: SideEffects)
  useConcerns(id: string, registrations: ConcernRegistrationMap)
  withConcerns(filter): /* subset API */
}
```

### Store Configuration

```typescript
config?: {
  debug?: boolean
  // other options...
}
```

---

## Coverage Gaps for Public API Integration Tests

### Critical Gaps (Not Well Covered)

1. **Store Creation & Initialization**
   - ✅ Basic creation tested (basic.test.tsx)
   - ❌ **Missing**: Config options (debug mode, custom settings)
   - ❌ **Missing**: Initial state injection patterns
   - ❌ **Missing**: Multiple stores in same app (context isolation)

2. **Hooks - useStore() vs useFieldStore()**
   - ✅ useFieldStore tested (form-validation, sync-paths, etc)
   - ✅ useJitStore tested (side-effects.test.tsx)
   - ❌ **Missing**: useStore (tuple return) comprehensive testing
   - ❌ **Missing**: Comparison of useFieldStore vs useStore performance/behavior

3. **Type Safety**
   - ✅ Path types used (typeHelpers.syncPair)
   - ❌ **Missing**: Type error prevention (e.g., wrong path should not compile)
   - ❌ **Missing**: DeepKey/DeepValue type correctness validation

4. **Provider & Context**
   - ✅ Basic Provider in basic.test.tsx
   - ❌ **Missing**: Multiple Provider instances
   - ❌ **Missing**: Nested Provider behavior
   - ❌ **Missing**: Context consumption without Provider (error handling)

5. **withConcerns() Filter API**
   - ❌ **Missing**: Comprehensive withConcerns testing
   - ❌ **Missing**: Multiple concern filtering
   - ❌ **Missing**: Concern filtering + hook interactions

6. **Meta Parameter**
   - ❌ **Missing**: Passing metadata through setValue
   - ❌ **Missing**: Metadata in change tracking
   - ❌ **Missing**: META type parameter usage

7. **Real-World Public API Workflows**
   - ✅ Form validation (form-validation.test.tsx)
   - ✅ Wizard/multi-step (complex-workflows.test.tsx)
   - ✅ Sync/flip (sync-paths.test.tsx)
   - ❌ **Missing**: Create store → render → interact → cleanup
   - ❌ **Missing**: Store reset/cleanup patterns
   - ❌ **Missing**: Unregistering effects

---

## Recommended Integration Test Scenario

**Test File**: `tests/integration/public-api-store.test.tsx`

**Scope**: Comprehensive public API test that exercises the complete store lifecycle

**Scenario**: Shopping List App

- ✅ Initialize store with items
- ✅ Add/remove items (mutations)
- ✅ Calculate total price (computed concern)
- ✅ Filter by category (visibleWhen concern)
- ✅ Sync primary address with billing (syncPaths)
- ✅ Toggle premium delivery (flipPaths)
- ✅ Validate item names (validationState concern)
- ✅ Display/hide checkout section based on cart state
- ✅ Track modification timestamps (listener side effect)

**Key Testing Goals**:

1. All public hooks work correctly (useFieldStore, useStore, useJitStore)
2. All side effect types integrate smoothly (syncPaths, flipPaths, listeners)
3. Concerns system delivers expected UI state
4. WASM boundary crossings work correctly
5. No memory leaks or infinite loops
6. Error cases handled gracefully

---

## Test Matrix

| Test File | Store Creation | Hooks | Concerns | SideEffects | Validation | Sync | Flip | Listeners | Nesting | Real-World |
|-----------|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|
| basic.test.tsx | ✅ | ✅ | ❌ | ✅ | ❌ | ✅ | ✅ | ❌ | ❌ | ❌ |
| form-validation.test.tsx | ✅ | ✅ | ✅ | ❌ | ✅ | ❌ | ❌ | ❌ | ❌ | ✅ |
| complex-workflows.test.tsx | ✅ | ✅ | ✅ | ✅ | ✅ | ❌ | ❌ | ❌ | ❌ | ✅ |
| sync-paths.test.tsx | ✅ | ✅ | ❌ | ✅ | ❌ | ✅ | ❌ | ❌ | ❌ | ❌ |
| error-handling.test.tsx | ✅ | ✅ | ❌ | ❌ | ✅ | ❌ | ❌ | ❌ | ❌ | ✅ |
| side-effects.test.tsx | ✅ | ✅ | ❌ | ✅ | ❌ | ✅ | ✅ | ✅ | ❌ | ❌ |
| aggregations.test.tsx | ✅ | ✅ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ✅ | ✅ |
| concerns-ui.test.tsx | ✅ | ✅ | ✅ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ✅ |
| **PUBLIC-API-STORE** | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
