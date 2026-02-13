---
created: 2026-02-04 (15bee6f)
updated: 2026-02-04 (15bee6f)
status: active
---

# Wild() - Inline Wildcard Template Strings

The `Wild()` utility provides the cleanest way to construct Record paths with `[*]` wildcards directly in template strings.

## Basic Usage

```typescript
import { Wild } from '@sladg/apex-state'

const NOTIONAL_PATH = `portfolio.books.b1.products.p1.legGroups.g1.legs.${Wild('l1')}.notional`
// → "portfolio.books.b1.products.p1.legGroups.g1.legs.[*].notional"
```

## How It Works

The `Wild()` function takes a concrete ID as a parameter (for documentation purposes) and returns `[*]`:

```typescript
Wild('l1')           // → '[*]'
Wild('any-string')   // → '[*]'
Wild('123')          // → '[*]'
Wild('')             // → '[*]'
```

The parameter is purely for **readability and documentation** - it shows which concrete ID is being replaced with a wildcard.

## Real-World Examples

### Single Wildcard Path
```typescript
const userEmailPath = `users.${Wild('u1')}.email`
// → "users.[*].email"

store.useFieldStore(userEmailPath)
```

### Multiple Wildcards
```typescript
const commentTextPath = `users.${Wild('u1')}.posts.${Wild('p1')}.comments.${Wild('c1')}.text`
// → "users.[*].posts.[*].comments.[*].text"

store.useConcerns('validation', {
  [commentTextPath]: {
    validationState: { schema: z.string().min(1) }
  }
})
```

### Complex Nested Paths
```typescript
const strikePath = `portfolio.books.${Wild('b1')}.products.${Wild('p1')}.legGroups.${Wild('g1')}.legs.${Wild('l1')}.strike`
// → "portfolio.books.[*].products.[*].legGroups.[*].legs.[*].strike"

const levelPath = `portfolio.books.${Wild('b1')}.products.${Wild('p1')}.legGroups.${Wild('g1')}.legs.${Wild('l1')}.barrier.schedule.${Wild('obs1')}.level`
// → "portfolio.books.[*].products.[*].legGroups.[*].legs.[*].barrier.schedule.[*].level"
```

## Comparison with Other Approaches

### Using Wild() (Recommended)
```typescript
const path = `portfolio.books.${Wild('b1')}.products.${Wild('p1')}.legs.${Wild('l1')}.notional`
```
✅ Clean and readable
✅ Inline in template strings
✅ Shows original structure with concrete IDs
✅ No separate function calls needed

### Using toWildcardPathAuto()
```typescript
const path = toWildcardPathAuto('portfolio.books.b1.products.p1.legs.l1.notional')
```
✅ Automatic ID detection
✅ Single function call
❌ Less readable what's being wildcarded
❌ Requires separate line/variable

### Using toWildcardPath()
```typescript
const path = toWildcardPath('portfolio.books.b1.products.p1.legs.l1.notional', [2, 4, 6])
```
✅ Explicit control
❌ Need to track indices
❌ Verbose for many wildcards
❌ Not as readable

## Type Safety

The returned value is properly typed as a valid `DeepKey`:

```typescript
const path = `portfolio.books.${Wild('b1')}.products.${Wild('p1')}.strike`
// path: string (but is valid DeepKey for Record types)

store.useFieldStore(path)  // ✅ Type-safe
store.useConcerns('concern', { [path]: { ... } })  // ✅ Type-safe
```

## Best Practice

Use `Wild()` when:
- ✅ Building paths with multiple Record wildcards
- ✅ Documenting which concrete IDs are being generalized
- ✅ Writing inline path definitions in test code
- ✅ Want readability and clarity

Use `toWildcardPathAuto()` when:
- ✅ Converting existing concrete paths to wildcards
- ✅ Migrating old code
- ✅ One-off path conversions

Use `toWildcardPath()` when:
- ✅ Need precise control over which indices become wildcards
- ✅ Building paths programmatically
