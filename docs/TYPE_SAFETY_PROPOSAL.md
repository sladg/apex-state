---
created: unstaged
updated: unstaged
status: active
---

# Type Safety Architecture for Concerns v4

## Problem

Current proposal would create `PathOrPattern<DATA> = DeepKey<DATA> | WildcardPattern`, which doubles the type union complexity:
- DeepKey already generates all possible paths (e.g., 500+ paths for medium state)
- Adding patterns would add ~100+ additional union members
- TypeScript struggles with unions >1000 members, causing performance/compilation issues

## Solution: Three-Tier Type Strategy

### Tier 1: Component-Level Concerns (STRICT - No Change)

**Location**: `useConcerns()` hook, component-level registration

**Type Definition** (NO CHANGE from current):
```typescript
export type ConcernRegistrationMap<DATA extends object> = Partial<
  Record<DeepKey<DATA>, Partial<Record<string, unknown>>>
>
```

**Why this works**:
- Component concerns use concrete paths only - no patterns
- Developers get full autocomplete: `'cart.items.0'` is type-safe
- No new type complexity
- Strict validation at the call site

**Example**:
```typescript
useConcerns('cart', {
  'cart.items.0': { disabledWhen: {...} },  // ✅ Full type safety
  'cart.items.wrong': {...}                 // ❌ Type error - caught early
})
```

---

### Tier 2: Global Concerns (LOOSE - Runtime Validation)

**Location**: `<Provider globalConcerns={...}>`, defined once at startup

**Type Definition** (NEW):
```typescript
/**
 * Global concern registration - looser typing for provider-level definitions
 * Accepts both concrete paths and wildcard patterns.
 * Patterns validated at registration time (runtime), not compile time.
 */
export type GlobalConcernRegistration<DATA extends object> = Partial<
  Record<string, Partial<Record<string, unknown>>>
>
```

**Why this works**:
- Global concerns defined once at Provider time (less prone to typos)
- String keys accept both concrete paths AND patterns
- Runtime validation catches pattern errors (invalid patterns throw)
- No type union explosion - just `Record<string, ...>`
- JSDocs guide users on valid pattern syntax

**Example**:
```typescript
<Provider
  globalConcerns={{
    'cart.items.[*]': { disabledWhen: {...} },       // ✅ Pattern accepted
    'cart.items': { disabledWhen: {...} },           // ✅ Concrete path OK
    'cart.items.[*].invalid': { disabledWhen: {...} }, // ❌ Runtime error: invalid pattern
  }}
>
```

---

### Tier 3: Internal Merged Registration (CONCRETE - Type-Safe After Expansion)

**Location**: Post-merge, post-expansion (internal to `registerConcernEffectsImpl`)

**Type Definition** (no new type, just how it works):
```typescript
// After merging global + component concerns
// After expanding all patterns to concrete paths
// Result: ConcernRegistrationMap<DATA> with only concrete paths
const expandedRegistration: ConcernRegistrationMap<DATA> = {
  'cart.items.0': { ... },
  'cart.items.1': { ... },
  'cart.items.2': { ... },
  // ... all patterns expanded
}
```

**Why this works**:
- Expansion happens at runtime, not type-check time
- Output is concrete paths only - type-safe
- Effects register against a fully-expanded, typed registration map

---

## Type Flow Diagram

```
┌─────────────────────────────────────────────────────────────┐
│ Component Level (Strict)                                     │
│ useConcerns('path', {                                        │
│   'cart.items.0': { ... }  ← DeepKey<DATA> - type-checked   │
│ })                                                           │
└──────────────────┬──────────────────────────────────────────┘
                   │ merges with
                   ↓
┌─────────────────────────────────────────────────────────────┐
│ Global Level (Loose)                                         │
│ <Provider globalConcerns={{                                  │
│   'cart.items.[*]': { ... }  ← string - runtime validated    │
│ }}>                                                          │
└──────────────────┬──────────────────────────────────────────┘
                   │ mergeConcerns()
                   ↓
┌─────────────────────────────────────────────────────────────┐
│ Merged Registration (Loose)                                  │
│ ConcernRegistrationMap + wildcard patterns                   │
└──────────────────┬──────────────────────────────────────────┘
                   │ expandConcernWildcards()
                   ↓
┌─────────────────────────────────────────────────────────────┐
│ Expanded Registration (Strict)                               │
│ ConcernRegistrationMap<DATA> with concrete paths only        │
│ {                                                            │
│   'cart.items.0': { ... },                                   │
│   'cart.items.1': { ... }                                    │
│ }                                                            │
└──────────────────┬──────────────────────────────────────────┘
                   │
                   ↓
              registerConcernEffectsImpl()
              (type-safe, fully concrete paths)
```

---

## Implementation Details

### File: `src/types/concerns.ts`

```typescript
/**
 * Component-level concern registration (STRICT)
 * Uses DeepKey<DATA> for full type safety
 * Concrete paths only - no patterns allowed
 */
export type ConcernRegistrationMap<DATA extends object> = Partial<
  Record<DeepKey<DATA>, Partial<Record<string, unknown>>>
>

/**
 * Global concern registration (LOOSE)
 * Uses string keys to accept both concrete paths and patterns
 * Patterns validated at runtime
 *
 * @example
 * ```typescript
 * const globalConcerns: GlobalConcernRegistration<State> = {
 *   'cart.items.[*]': { disabledWhen: {...} },      // ✅ Valid pattern
 *   'user.email': { validationState: {...} },       // ✅ Valid path
 *   'cart.[*].quantity': { ... }                     // ❌ Invalid pattern (mid-path)
 * }
 * ```
 */
export type GlobalConcernRegistration<DATA extends object> = Partial<
  Record<string, Partial<Record<string, unknown>>>
>
```

### File: `src/utils/patternValidation.ts` (NEW)

```typescript
/**
 * Validate wildcard pattern syntax
 * Throws descriptive error for invalid patterns
 */
export const validateWildcardPattern = (
  pattern: string,
): void => {
  if (!pattern.includes('[*]')) {
    // Not a pattern, assumed to be a concrete path - valid
    return
  }

  // Patterns must be end-of-path only
  if (!pattern.endsWith('.[*]') && pattern !== '[*]') {
    throw new Error(
      `Invalid wildcard pattern "${pattern}". ` +
      `Wildcards must be end-of-path (e.g., "cart.items.[*]" or "[*]"). ` +
      `Mid-path wildcards like "cart.items.[*].quantity" are not supported.`
    )
  }

  // Only one wildcard allowed per pattern
  const wildcardCount = (pattern.match(/\[\*\]/g) || []).length
  if (wildcardCount > 1) {
    throw new Error(
      `Invalid wildcard pattern "${pattern}". ` +
      `Only one [*] allowed per pattern.`
    )
  }
}
```

### File: `src/utils/mergeConcerns.ts` (UPDATE)

```typescript
export const mergeConcerns = <DATA extends object>(
  global: GlobalConcernRegistration<DATA> | undefined,  // ← Loose type
  local: ConcernRegistrationMap<DATA>,                   // ← Strict type
  mode: ConcernMergeMode = 'MERGE',
): ConcernRegistrationMap<DATA> | GlobalConcernRegistration<DATA> => {
  // After merge, result has both concrete paths and patterns
  // Type is GlobalConcernRegistration (loose) because patterns still present
  // Next step (expansion) will convert to ConcernRegistrationMap (strict)

  // IMPLEMENTATION: runtime merge logic
}
```

---

## Benefits of This Approach

| Aspect | Benefit |
|--------|---------|
| **Type Complexity** | No union explosion - stays at current levels |
| **Component Safety** | ✅ Full autocomplete, type errors caught early |
| **Global Flexibility** | ✅ Patterns supported, validated at runtime |
| **Compilation Speed** | ✅ No new union types to calculate |
| **Error Messages** | ✅ Runtime validation gives clear pattern syntax errors |
| **Backward Compatible** | ✅ Component-level API unchanged |
| **Migration Path** | ✅ Existing code works as-is, no refactoring needed |

---

## Edge Cases & Validation

### Valid Patterns
```typescript
globalConcerns: {
  '[*]': { ... }                          // ✅ Root level
  'cart.[*]': { ... }                     // ✅ End of path
  'cart.items.[*]': { ... }               // ✅ End of path
  'users.123.settings.[*]': { ... }       // ✅ End of path
}
```

### Invalid Patterns (Throw at Registration)
```typescript
globalConcerns: {
  'cart.items.[*].quantity': { ... }      // ❌ Mid-path wildcard
  'cart.[*].items.[*]': { ... }           // ❌ Multiple wildcards
  '[*].items': { ... }                    // ❌ Mid-path wildcard
}
```

---

## Type Definition Summary

| Layer | Type | Safety | Validation |
|-------|------|--------|-----------|
| Component | `ConcernRegistrationMap<DATA>` | ✅ Strict | Compile-time |
| Global | `GlobalConcernRegistration<DATA>` | ⚠️ Loose | Runtime |
| Expanded | `ConcernRegistrationMap<DATA>` | ✅ Strict | Runtime expansion |
| Internal Effects | `ConcernRegistrationMap<DATA>` | ✅ Strict | Already typed |

---

## Implementation Sequence

1. ✅ **Phase 1**: Add `GlobalConcernRegistration<DATA>` type to `src/types/concerns.ts`
2. ✅ **Phase 2**: Add `validateWildcardPattern()` to `src/utils/patternValidation.ts`
3. ✅ **Phase 3**: Update `mergeConcerns()` to accept `GlobalConcernRegistration`
4. ✅ **Phase 4**: Update Provider to accept `globalConcerns?: GlobalConcernRegistration<DATA>`
5. ✅ **Phase 5**: Remaining implementation (expansion, self-referencing, etc.)

This ensures type safety is maintained without adding complexity.
