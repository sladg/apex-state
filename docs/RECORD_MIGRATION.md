---
created: 2026-02-04 (15bee6f)
updated: 2026-02-04 (15bee6f)
status: active
---

# Record Support Migration Guide

## Overview
Record support has been added to DeepKey/DeepValue, which now correctly enforces that `Record<string, V>` types can only be accessed with wildcard paths using `[*]` notation.

## What Changed
- `DeepKey<Record<string, V>>` now generates `"[*]" | "[*].property" | ...` instead of concrete paths
- `DeepValue` correctly resolves `[*]` paths in Record types
- `WILDCARD` constant exported for convenience: `import { WILDCARD } from '@sladg/apex-state'`

## Migration Patterns

### Pattern 0: Use Wildcard Path Utilities (Recommended for Tests)
Convert concrete fixture paths to wildcard notation:

```typescript
import { toWildcardPathAuto, toWildcardPath } from '@sladg/apex-state'

// Automatically detect ID-like segments (letter + digits)
const path1 = toWildcardPathAuto('portfolio.books.b1.products.p1.title')
// → "portfolio.books.[*].products.[*].title"

// Or explicitly specify which indices should be wildcards
const path2 = toWildcardPath('books.b1.products.p1.title', [1, 3])
// → "books.[*].products.[*].title"

// Use in tests:
store.useConcerns('my-concern', {
  [toWildcardPathAuto('portfolio.books.b1.products.p1.legs.l1.strike')]: {
    validationState: { schema: z.number().positive() },
  },
})
```

### Pattern 1: Use Unsafe Accessors for Runtime IDs
For accessing Record entries with IDs known only at runtime:

```typescript
// BEFORE (now type-unsafe):
dot.set(state, `books.${bookId}.title`, 'New Title')  // ❌ bookId is runtime

// AFTER (using unsafe):
dot.set__unsafe(state, `books.${bookId}.title`, 'New Title')  // ✅
```

### Pattern 2: Use Wildcard Paths for Generic Logic
For code that processes any entry in a Record:

```typescript
// Process all books
type BookPaths = DeepKey<State>  // includes "books.[*]", "books.[*].title", etc.
```

### Pattern 3: Restructure Types (Recommended)
If you have a defined set of keys, use specific keys instead of Record:

```typescript
// BEFORE:
interface Portfolio {
  books: Record<string, Book>  // Too generic if you know the keys
}

// AFTER:
interface Portfolio {
  books: {
    b1: Book
    b2: Book
    b3: Book
  }
}
```

## Integration Test Fixes
Tests using fixture data with known Record keys should use `dot.set__unsafe` or restructure their fixture types.

Example:
```typescript
// BEFORE (type error with new Record support):
dot.set(state, 'portfolio.books.b1.products.p1.title', 'Product A')

// AFTER:
dot.set__unsafe(state, 'portfolio.books.b1.products.p1.title', 'Product A')
```

## Benefits
- ✅ Type-safe access to Record fields using `[*]` wildcards
- ✅ Clear distinction between concrete keys and dynamic Record keys
- ✅ Better IDE support and autocomplete for Record paths
- ✅ Catches mistakes at compile time instead of runtime
