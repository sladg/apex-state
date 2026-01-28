# useFieldConcerns Hook API Documentation

## Overview

The `useFieldConcerns` hook provides reactive access to computed concern values for a specific field path in React components. It is part of the store instance returned by `createGenericStore`.

## Location

- **File**: `src/store/createStore.ts` (lines 233-245)
- **Part of**: Store instance API

## API Signature

```typescript
store.useFieldConcerns<P extends DeepKey<DATA>>(
  path: P
): EvaluatedConcerns<CONCERNS>
```

## Parameters

- `path`: A type-safe deep path to the field (e.g., `'user.email'`, `'products.leg-1.strike'`)

## Returns

An object containing concern values for the specified path:

```typescript
{
  zodValidation?: boolean
  disabled?: boolean
  visible?: boolean
  tooltip?: string
  validationState?: ValidationStateResult
  placeholder?: string
  label?: string
  // ... other registered concerns
}
```

Returns an empty object `{}` if no concerns are registered for the path.

## Implementation Details

1. Uses `useStoreContext()` to access the store instance
2. Uses `useSnapshot(store._concerns)` for reactive updates
3. Reads from the `_concerns` proxy at the specified path
4. Returns typed concerns based on the CONCERNS type parameter

## Key Features

- **Reactive**: Automatically re-renders when concern values change
- **Type-safe**: Return type is based on registered concern types
- **Safe**: Returns empty object for non-existent paths (no null checks needed)
- **Efficient**: Uses Valtio's snapshot mechanism for optimal reactivity

## Usage Example

```typescript
const MyComponent = () => {
  // Get concern values for a specific field
  const emailConcerns = store.useFieldConcerns('email')

  return (
    <div>
      <input
        disabled={emailConcerns.disabled}
        placeholder={emailConcerns.placeholder}
        className={emailConcerns.zodValidation ? '' : 'error'}
      />
      {emailConcerns.visible !== false && (
        <span>{emailConcerns.tooltip}</span>
      )}
    </div>
  )
}
```

## Test Coverage

- **React Integration Tests**: `tests/concerns/react-integration.test.tsx` (5 tests)
- **Form Validation Tests**: `tests/integration/form-validation.test.tsx` (7 tests)
- **Concerns UI Tests**: `tests/integration/concerns-ui.test.tsx`

All tests pass successfully, confirming the hook works correctly with:
- Reactive updates when state changes
- Conditional rendering based on concern values
- Complex nested paths
- Multiple concern types simultaneously

## Related Hooks

- `store.useStore(path)` - Read/write field values
- `store.useFieldStore(path)` - Field operations (value, setValue, reset, etc.)
- `store.useFieldEffect(id, registration, concerns)` - Register concerns for a field

## Architecture Notes

The hook follows the unified architecture pattern:
- **state proxy**: User writes to this (field values)
- **_concerns proxy**: Reactive effects write to this (computed concern values)
- **useFieldConcerns**: React components read from _concerns using useSnapshot

This separation ensures concerns are computed reactively and components re-render only when concern values change.
