# Phase 1, Task 02: Core Type Utilities

**Task IDs**: APEX-3, APEX-4, APEX-6, APEX-7
**Priority**: P0 (Critical)
**Dependencies**: Task 01 (Project Setup)
**Phase**: Foundation

---

## 🎯 Worker Prompt

**YOU ARE**: A TypeScript type system expert
**YOUR FOCUS**: Implement advanced type utilities for deep path access and change tracking
**STAY FOCUSED**: Do NOT implement any runtime logic, hooks, or store. Only TypeScript types.
**SUCCESS MEANS**: Type utilities correctly infer deep paths and values with full type safety

---

## 📋 Task Breakdown

### APEX-3: Implement DeepKey Type Utility

Create a utility type that generates a union of all possible dot-notation paths for nested objects.

**Example:**
```typescript
type User = {
  name: string
  address: {
    street: string
    city: string
  }
}

// DeepKey<User> should be:
// "name" | "address" | "address.street" | "address.city"
```

### APEX-4: Implement DeepValue Type Utility

Create a utility type that extracts the value type for a given path string.

**Example:**
```typescript
// DeepValue<User, "address.street"> should be: string
// DeepValue<User, "address"> should be: { street: string, city: string }
```

### APEX-6: Implement GenericMeta Type

Create the base metadata type with standard properties for change tracking.

**Properties:**
- `isSyncPathChange?`: boolean - indicates change from sync path side-effect
- `isFlipPathChange?`: boolean - indicates change from flip path side-effect
- `isProgramaticChange?`: boolean - indicates change from code (not user action)
- `sender?`: string - identifies who initiated the change

### APEX-7: Implement ArrayOfChanges Type

Create a type representing an array of changes with paths, values, and metadata.

**Structure:**
```typescript
// Each change is a tuple: [path, value, meta]
// ArrayOfChanges<User, GenericMeta> should be:
// Array<[DeepKey<User>, DeepValue<User, DeepKey<User>>, GenericMeta]>
```

---

## ✅ Acceptance Criteria

### APEX-3 Criteria:
- [ ] `DeepKey<T>` type utility exported from `src/types/deepKey.ts`
- [ ] Works with nested objects (at least 5 levels deep)
- [ ] Works with arrays: `array.0.property` format
- [ ] Works with optional properties
- [ ] Test file demonstrates usage: `tests/types/deepKey.test-d.ts`

### APEX-4 Criteria:
- [ ] `DeepValue<T, Path>` type utility exported from `src/types/deepValue.ts`
- [ ] Correctly resolves types for any valid DeepKey
- [ ] Returns correct types for nested paths
- [ ] Handles optional properties correctly (returns T | undefined)
- [ ] Test file demonstrates usage: `tests/types/deepValue.test-d.ts`

### APEX-6 Criteria:
- [ ] `GenericMeta` interface exported from `src/types/meta.ts`
- [ ] Contains all four specified properties as optional
- [ ] Extensible: can be extended with custom properties
- [ ] Well-documented with TSDoc comments

### APEX-7 Criteria:
- [ ] `ArrayOfChanges<DATA, META>` type exported from `src/types/changes.ts`
- [ ] Correctly types the tuple structure
- [ ] Works with any DATA type that has DeepKey
- [ ] Works with GenericMeta and extensions of it
- [ ] Test file demonstrates usage

---

## 📦 Expected Output

### File Structure:

```
src/
  types/
    index.ts          # Re-exports all types
    deepKey.ts        # DeepKey utility
    deepValue.ts      # DeepValue utility
    meta.ts           # GenericMeta interface
    changes.ts        # ArrayOfChanges type

tests/
  types/
    deepKey.test-d.ts    # Type tests for DeepKey
    deepValue.test-d.ts  # Type tests for DeepValue
    changes.test-d.ts    # Type tests for ArrayOfChanges
```

### src/types/index.ts exports:

```typescript
export type { DeepKey } from './deepKey'
export type { DeepValue } from './deepValue'
export type { GenericMeta } from './meta'
export type { ArrayOfChanges } from './changes'
```

### Example implementation hints:

**DeepKey** (recursive type):
```typescript
// Use template literal types and conditional types
// Consider: object keys, nested objects, arrays
type DeepKey<T, Prefix extends string = ''> =
  T extends object
    ? { [K in keyof T]: /* recursive magic here */ }[keyof T]
    : never
```

**DeepValue** (path resolution):
```typescript
// Use template literal types to split paths
// Handle dot notation: "a.b.c" -> T["a"]["b"]["c"]
type DeepValue<T, Path extends string> =
  Path extends `${infer First}.${infer Rest}`
    ? /* handle nested path */
    : /* handle single key */
```

**GenericMeta**:
```typescript
export interface GenericMeta {
  isSyncPathChange?: boolean
  isFlipPathChange?: boolean
  isProgramaticChange?: boolean
  sender?: string
}
```

**ArrayOfChanges**:
```typescript
export type ArrayOfChanges<DATA, META extends GenericMeta = GenericMeta> =
  Array<[DeepKey<DATA>, any, META]> // Need to properly type the value
```

---

## 🧪 Verification Steps

### Type Tests:

Use type testing with `expectTypeOf` or similar:

```typescript
import { expectTypeOf } from 'vitest'
import type { DeepKey, DeepValue } from '../src/types'

type TestData = {
  user: {
    name: string
    age: number
  }
  count: number
}

// Test DeepKey
expectTypeOf<DeepKey<TestData>>().toEqualTypeOf<
  "user" | "user.name" | "user.age" | "count"
>()

// Test DeepValue
expectTypeOf<DeepValue<TestData, "user.name">>().toEqualTypeOf<string>()
expectTypeOf<DeepValue<TestData, "count">>().toEqualTypeOf<number>()
```

### Build verification:

```bash
# Type check should pass
npm run type-check

# Build should include type definitions
npm run build

# Check that .d.ts files are generated
ls dist/types/
```

---

## 🚨 Common Pitfalls

- **DON'T**: Implement runtime logic - these are types only
- **DON'T**: Make types too complex - balance utility with compilation speed
- **DON'T**: Use `any` as a shortcut - if types get complex, ask for guidance
- **DON'T**: Forget to handle edge cases: optional properties, arrays, never types
- **DO**: Add JSDoc comments explaining what each type does
- **DO**: Test types with complex nested structures
- **DO**: Keep types in separate files for better organization
- **DO**: Consider TypeScript's recursion limits (keep depth reasonable)
- **DO**: Use `unknown` instead of `any` when the type is truly unknown

---

## 💡 Implementation Tips

1. **DeepKey**: Look at libraries like `type-fest` for inspiration, but implement from scratch
2. **Arrays**: Decide whether to support `array.0.prop` or just `array` as a whole
3. **Circular references**: Types don't need to handle circular data structures
4. **Performance**: Complex recursive types can slow down TS - test with large objects
5. **Optional properties**: `T[K]` vs `T[K] | undefined` - be consistent

---

## 📚 Reference Materials

- TypeScript Handbook: Template Literal Types
- TypeScript Handbook: Conditional Types
- TypeScript Handbook: Mapped Types
- Valtio source code: Look at their path utilities for inspiration

---

## ➡️ Next Steps

Once this task is complete, proceed to:
- **Task 03**: `03-base-store.md` - Implement core store with valtio integration
