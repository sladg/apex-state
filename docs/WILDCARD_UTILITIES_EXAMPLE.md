---
created: 2026-02-04 (15bee6f)
updated: 2026-02-04 (15bee6f)
status: active
---

# Wildcard Utilities Example

The new `toWildcardPath` and `toWildcardPathAuto` utilities help convert concrete Record paths to wildcard paths.

## Usage Examples

### Auto-Conversion (Recommended)

The `toWildcardPathAuto` function automatically detects ID-like segments (letter + digits) and converts them to `[*]`:

```typescript
import { toWildcardPathAuto } from '@sladg/apex-state'

// Automatically detect ID patterns like b1, p1, l1, etc.
const path = toWildcardPathAuto('portfolio.books.b1.products.p1.legs.l1.strike')
// Result: "portfolio.books.[*].products.[*].legs.[*].strike"
```

### Explicit Index-Based Conversion

When you need more control, use `toWildcardPath` to specify exact indices:

```typescript
import { toWildcardPath } from '@sladg/apex-state'

// Replace segments at indices 2 and 4
const path = toWildcardPath('portfolio.books.b1.products.p1.title', [2, 4])
// Result: "portfolio.books.[*].products.[*].title"
```

## Before and After: Integration Test Fix

### BEFORE (Type Error)
```typescript
it('validates strike price', async () => {
  function StrikeValidator() {
    // ❌ Error: "portfolio.books.b1.products.p1..." is not a valid DeepKey
    const strikeField = store.useFieldStore(
      'portfolio.books.b1.products.p1.legGroups.g1.legs.l1.strike'
    )

    store.useConcerns('validation', {
      // ❌ Error: concrete path not allowed for Record types
      ['portfolio.books.b1.products.p1.legGroups.g1.legs.l1.strike']: {
        validationState: { schema: z.number().positive() }
      }
    })
  }
})
```

### AFTER (Using Wildcard Utilities)
```typescript
import { toWildcardPathAuto } from '@sladg/apex-state'

it('validates strike price', async () => {
  const STRIKE_PATH = toWildcardPathAuto(
    'portfolio.books.b1.products.p1.legGroups.g1.legs.l1.strike'
  )
  // STRIKE_PATH = "portfolio.books.[*].products.[*].legGroups.[*].legs.[*].strike"

  function StrikeValidator() {
    // ✅ Works! Using wildcard path
    const strikeField = store.useFieldStore(STRIKE_PATH)

    store.useConcerns('validation', {
      [STRIKE_PATH]: {
        validationState: { schema: z.number().positive() }
      }
    })
  }
})
```

## Pattern Recognition

`toWildcardPathAuto` recognizes ID segments by this regex pattern: `/^[a-z]+\d+$/i`

This matches:
- ✅ `b1`, `book1`, `B1` (letter + digits)
- ✅ `p1`, `product2`, `P99`
- ✅ `l1`, `leg1`, `L5`
- ✅ `g1`, `group1`, `G10`
- ✅ `obs1`, `observation99`
- ❌ `0` (digits only)
- ❌ `name` (letters only)
- ❌ `_id`, `0-1` (special chars)

## Use Cases

1. **Test Fixtures**: Convert fixture paths with known IDs to wildcard paths
2. **Dynamic Path Building**: Build paths at runtime with actual record IDs
3. **Configuration**: Create configuration objects with wildcard paths for Records

## Export

Both utilities are exported from the main package:

```typescript
import { toWildcardPath, toWildcardPathAuto, WILDCARD } from '@sladg/apex-state'
```

They're also available from the utils module:

```typescript
import { toWildcardPath, toWildcardPathAuto, WILDCARD } from '@sladg/apex-state/utils/dot'
```
