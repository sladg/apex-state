# Phase 6, Task 12: Advanced Type Utilities and Form Hooks

**Task IDs**: APEX-5, APEX-8, APEX-9, APEX-17, APEX-18
**Priority**: P1-P2 (High to Medium)
**Dependencies**: Task 02 (Core Types), Task 04 (Basic Hooks)
**Phase**: Advanced Features

---

## 🎯 Worker Prompt

**YOU ARE**: A TypeScript generics specialist and React forms expert
**YOUR FOCUS**: Implement advanced type utilities and convenient form field hooks
**STAY FOCUSED**: Use functional patterns, no classes. Keep hooks simple and composable.
**SUCCESS MEANS**: Advanced types work correctly, form hooks provide great DX

---

## 📋 Task Breakdown

### APEX-5: Implement DeepKeyFiltered Type Utility

Create utility type that filters paths by their resolved value type.

**What to do:**
1. Type that returns only paths resolving to a specific type
2. Example: `DeepKeyFiltered<DATA, boolean>` returns only boolean paths
3. Build on DeepKey and DeepValue utilities

### APEX-8: Implement PathsOfSameValue Type

Create type for mapping paths to other paths with same value (for sync).

**What to do:**
1. Type: `Record<DeepKey<DATA>, Array<DeepKey<DATA>>>`
2. Used for sync paths configuration
3. Type-safe path mapping

### APEX-9: Implement OnStateChangesListenerFunction Type

Create complex type for state change listeners with conditional signatures.

**What to do:**
1. Type with conditional fn signature based on key
2. When key=null: receives full changes and state
3. When key=path: receives scoped changes and state
4. Proper type inference for both cases

### APEX-17: Implement useFieldStore Hook

Create convenient hook for form field management.

**What to do:**
1. Hook: `useFieldStore<VAL>(path)` returns `{value, setValue}`
2. setValue accepts optional meta parameter
3. Cleaner API than `[value, setValue]` for forms

### APEX-18: Implement useFieldTransformedStore Hook

Create hook for form fields with bidirectional transformations.

**What to do:**
1. Hook: `useFieldTransformedStore<VAL, CTX>(path, {toTemporary, fromTemporary, context?})`
2. Returns `{value: CTX, setValue: (newCtx: CTX) => void}`
3. Useful for date formatting, number parsing, etc.

---

## ✅ Acceptance Criteria

### APEX-5 Criteria:
- [ ] `DeepKeyFiltered<DATA, TYPE>` exported from `src/types/deepKeyFiltered.ts`
- [ ] Returns only paths that resolve to TYPE
- [ ] Works with primitives and complex types
- [ ] Test demonstrates filtering boolean, string, number paths

### APEX-8 Criteria:
- [ ] `PathsOfSameValue<DATA>` exported from `src/types/pathsOfSameValue.ts`
- [ ] Type structure: `Record<DeepKey<DATA>, Array<DeepKey<DATA>>>`
- [ ] Used in sync paths configuration
- [ ] Type-safe and well-documented

### APEX-9 Criteria:
- [ ] `OnStateChangesListenerFunction<DATA, META, Key>` exported
- [ ] Conditional type based on Key parameter
- [ ] When Key=null: fn receives `ArrayOfChanges<DATA, META>` and `DATA`
- [ ] When Key=path: fn receives scoped changes and scoped state
- [ ] Proper type inference in both cases
- [ ] Test demonstrates type checking

### APEX-17 Criteria:
- [ ] `useFieldStore<VAL>` hook exported from `src/hooks/useFieldStore.ts`
- [ ] Signature: `useFieldStore(path) => {value: VAL, setValue: (val: VAL, meta?: META) => void}`
- [ ] Built on top of useStore
- [ ] Type-safe value inference from path
- [ ] Test demonstrates form usage

### APEX-18 Criteria:
- [ ] `useFieldTransformedStore<VAL, CTX>` hook exported from `src/hooks/useFieldTransformedStore.ts`
- [ ] Signature: `useFieldTransformedStore(path, {toTemporary, fromTemporary, context?})`
- [ ] Returns: `{value: CTX, setValue: (ctx: CTX) => void}`
- [ ] toTemporary: `(val: VAL, context?: CTX) => CTX`
- [ ] fromTemporary: `(ctx: CTX, context?: CTX) => VAL`
- [ ] Useful for display formatting vs storage format
- [ ] Test demonstrates date/number transformations

---

## 📦 Expected Output

### File Structure:

```
src/
  types/
    deepKeyFiltered.ts       # DeepKeyFiltered utility
    pathsOfSameValue.ts      # PathsOfSameValue type
    listenerFunction.ts      # OnStateChangesListenerFunction (update APEX-9)
    index.ts                 # Re-export all
  hooks/
    useFieldStore.ts         # Form field hook
    useFieldTransformedStore.ts  # Transformed field hook
    index.ts
  index.ts

tests/
  types/
    deepKeyFiltered.test-d.ts
    listenerFunction.test-d.ts
  hooks/
    useFieldStore.test.tsx
    useFieldTransformedStore.test.tsx
```

### src/types/deepKeyFiltered.ts:

```typescript
import type { DeepKey, DeepValue } from './index'

/**
 * Filters DeepKey<T> to only include paths that resolve to type U.
 *
 * Example:
 *   type Data = { flag: boolean, name: string, count: number }
 *   DeepKeyFiltered<Data, boolean> = "flag"
 *   DeepKeyFiltered<Data, string> = "name"
 */
export type DeepKeyFiltered<T, U> = {
  [K in DeepKey<T>]: DeepValue<T, K> extends U ? K : never
}[DeepKey<T>]
```

### src/types/pathsOfSameValue.ts:

```typescript
import type { DeepKey } from './index'

/**
 * Record mapping each path to an array of paths with the same value.
 * Used for sync paths configuration.
 *
 * Example:
 *   {
 *     "user.email": ["profile.email", "settings.notificationEmail"],
 *     "profile.email": ["user.email", "settings.notificationEmail"]
 *   }
 */
export type PathsOfSameValue<DATA extends object> = Record<
  DeepKey<DATA>,
  Array<DeepKey<DATA>>
>
```

### src/hooks/useFieldStore.ts:

```typescript
import { useCallback } from 'react'
import type { DeepKey, DeepValue, GenericMeta } from '../types'
import { useStore } from './useStore'

/**
 * Hook for form field management with convenient object API.
 *
 * Returns {value, setValue} instead of [value, setValue] for better DX.
 */
export function useFieldStore<
  DATA extends object,
  P extends DeepKey<DATA>,
  META extends GenericMeta = GenericMeta
>(
  path: P
): {
  value: DeepValue<DATA, P>
  setValue: (newValue: DeepValue<DATA, P>, meta?: META) => void
} {
  const [value, setValue] = useStore<DATA, P, META>(path)

  return {
    value,
    setValue
  }
}
```

### src/hooks/useFieldTransformedStore.ts:

```typescript
import { useState, useCallback, useEffect } from 'react'
import type { DeepKey, DeepValue, GenericMeta } from '../types'
import { useStore } from './useStore'

export interface FieldTransformConfig<VAL, CTX> {
  toTemporary: (val: VAL, context?: CTX) => CTX
  fromTemporary: (ctx: CTX, context?: CTX) => VAL
  context?: CTX
}

/**
 * Hook for form fields with bidirectional transformations.
 *
 * Useful for:
 * - Date formatting: store as ISO string, display as formatted date
 * - Number parsing: store as number, display as formatted string
 * - Any display vs storage format difference
 *
 * Example:
 *   useFieldTransformedStore('birthDate', {
 *     toTemporary: (isoDate) => format(new Date(isoDate), 'MM/DD/YYYY'),
 *     fromTemporary: (formatted) => parse(formatted, 'MM/DD/YYYY').toISOString()
 *   })
 */
export function useFieldTransformedStore<
  DATA extends object,
  P extends DeepKey<DATA>,
  VAL extends DeepValue<DATA, P>,
  CTX,
  META extends GenericMeta = GenericMeta
>(
  path: P,
  config: FieldTransformConfig<VAL, CTX>
): {
  value: CTX
  setValue: (newContext: CTX) => void
} {
  const [storedValue, setStoredValue] = useStore<DATA, P, META>(path)
  const { toTemporary, fromTemporary, context } = config

  // Transform stored value to temporary (display) format
  const temporaryValue = toTemporary(storedValue as VAL, context)

  // Local state for temporary value (to avoid re-transforming on every render)
  const [localValue, setLocalValue] = useState<CTX>(temporaryValue)

  // Update local value when stored value changes externally
  useEffect(() => {
    setLocalValue(toTemporary(storedValue as VAL, context))
  }, [storedValue, toTemporary, context])

  const setValue = useCallback((newContext: CTX) => {
    // Update local state immediately for responsive UI
    setLocalValue(newContext)

    // Transform back and update stored value
    const newStoredValue = fromTemporary(newContext, context)
    setStoredValue(newStoredValue as DeepValue<DATA, P>)
  }, [fromTemporary, setStoredValue, context])

  return {
    value: localValue,
    setValue
  }
}
```

---

## 🧪 Verification Steps

### Type Tests:

```typescript
import { expectTypeOf } from 'vitest'
import type { DeepKeyFiltered, PathsOfSameValue } from '../src/types'

type TestData = {
  isActive: boolean
  isVerified: boolean
  name: string
  count: number
  user: {
    isAdmin: boolean
    email: string
  }
}

test('DeepKeyFiltered filters correctly', () => {
  expectTypeOf<DeepKeyFiltered<TestData, boolean>>().toEqualTypeOf<
    "isActive" | "isVerified" | "user.isAdmin"
  >()

  expectTypeOf<DeepKeyFiltered<TestData, string>>().toEqualTypeOf<
    "name" | "user.email"
  >()

  expectTypeOf<DeepKeyFiltered<TestData, number>>().toEqualTypeOf<"count">()
})
```

### Hook Tests:

```typescript
test('useFieldStore provides object API', () => {
  const store = createGenericStore<{ email: string }>()

  function Component() {
    const emailField = store.useFieldStore('email')

    return (
      <div>
        <input
          value={emailField.value}
          onChange={e => emailField.setValue(e.target.value)}
        />
      </div>
    )
  }

  const { getByRole } = render(
    <store.Provider initialState={{ email: 'test@example.com' }}>
      <Component />
    </store.Provider>
  )

  const input = getByRole('textbox')
  expect(input).toHaveValue('test@example.com')

  fireEvent.change(input, { target: { value: 'new@example.com' } })
  expect(input).toHaveValue('new@example.com')
})

test('useFieldTransformedStore transforms bidirectionally', () => {
  type State = { birthDate: string } // ISO format

  const store = createGenericStore<State>()

  function Component() {
    const dateField = store.useFieldTransformedStore('birthDate', {
      toTemporary: (iso: string) => {
        const date = new Date(iso)
        return date.toLocaleDateString() // Display format
      },
      fromTemporary: (display: string) => {
        const date = new Date(display)
        return date.toISOString() // Storage format
      }
    })

    return (
      <div>
        <input
          value={dateField.value}
          onChange={e => dateField.setValue(e.target.value)}
        />
        <span>Displayed: {dateField.value}</span>
      </div>
    )
  }

  const isoDate = '2000-01-15T00:00:00.000Z'

  const { getByRole, getByText } = render(
    <store.Provider initialState={{ birthDate: isoDate }}>
      <Component />
    </store.Provider>
  )

  // Should display formatted date, not ISO string
  expect(getByText(/Displayed:/).textContent).toContain('1/15/2000')

  // Input shows formatted date
  const input = getByRole('textbox')
  expect(input.value).not.toContain('T00:00:00')
})
```

---

## 🚨 Common Pitfalls

- **DON'T**: Make DeepKeyFiltered too complex (simple conditional mapping works)
- **DON'T**: Transform on every render in useFieldTransformedStore (use local state)
- **DON'T**: Forget to handle edge cases in transformations (null, undefined, invalid formats)
- **DO**: Keep transformations pure functions
- **DO**: Test transformations with various inputs
- **DO**: Consider performance of transformations (memoize if expensive)

---

## 💡 Implementation Tips

### DeepKeyFiltered:

Use mapped types with conditional filtering:
```typescript
{ [K in DeepKey<T>]: DeepValue<T, K> extends U ? K : never }[DeepKey<T>]
```

The `[DeepKey<T>]` at the end extracts the union of non-never values.

### useFieldTransformedStore:

Key insight: Keep local state for responsive UI, sync with stored value on changes.

This prevents flicker when user types (don't re-transform their input immediately).

---

## ➡️ Next Steps

Once this task is complete, proceed to:
- **Task 13**: `13-testing.md` - Comprehensive testing suite
