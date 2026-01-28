# Quick Reference Card

## ⚡ Essential Rules

### TypeScript
```typescript
// ❌ NEVER
const x: any = getValue()
const y = something as any

// ✅ ALWAYS
const x: unknown = getValue()
if (typeof x === 'string') { /* use x */ }

// In tests for type errors:
// @ts-expect-error - Testing invalid type
const invalid = store.useStore('bad.path')
```

### Architecture
```typescript
// ❌ NEVER: Classes
class MyRegistry { }

// ✅ ALWAYS: Factory functions
const createMyRegistry = () => {
  const state = new Map()
  return { register, unregister }
}
```

### Graphs
```typescript
// ❌ NEVER: Custom graph
const visited = new Set()
const queue = [start]
// ... manual BFS

// ✅ ALWAYS: graphology
import { connectedComponents } from 'graphology-components'
const components = connectedComponents(graph)
```

### Pipeline
```typescript
// ❌ NEVER: Sequential calls
let result = step1(changes)
result = step2(result)
result = step3(result)

// ✅ ALWAYS: Pipe composition
import { pipe } from 'remeda'
const result = pipe(step1, step2, step3)(changes)
```

### State Structure
```typescript
// ✅ ALWAYS use __internal pattern
const store = proxy({
  values: userData,        // User's state
  __internal: {            // Framework only
    sideEffects: { ... },
    graphs: { ... }
  }
})
```

---

## 🛑 When to STOP and ASK

**STOP immediately if:**
- Types are complex and you want to use `any`
- Multiple approaches seem equally valid
- Requirements are unclear or ambiguous
- Edge case behavior is undefined
- Making architectural decisions

**How to ask:**
```typescript
// TODO: BLOCKED - Need user guidance
// Question: [Clear question]
// Options:
//   A) [Option 1]
//   B) [Option 2]
// Current blocker: [Why you're stuck]
```

---

## 📦 Package Patterns

### Factory Function Template
```typescript
function createRegistry<T>() {
  // Private state (closure)
  const items = new Map<string, T>()

  // Public API
  return {
    add: (id: string, item: T) => {
      items.set(id, item)
    },

    remove: (id: string) => {
      items.delete(id)
    },

    get: (id: string) => {
      return items.get(id)
    }
  }
}
```

### Synchronizer Template
```typescript
import { pipe } from 'remeda'

type Synchronizer<DATA, META> = (
  changes: ArrayOfChanges<DATA, META>,
  store: StoreInstance<DATA, META>
) => ArrayOfChanges<DATA, META>

const mySynchronizer: Synchronizer<DATA, META> = (changes, store) => {
  const { __internal } = store
  const registry = __internal.sideEffects.myFeature

  const newChanges: ArrayOfChanges<DATA, META> = []

  for (const [path, value, meta] of changes) {
    // Process and add new changes
  }

  return [...changes, ...newChanges]
}
```

---

## 🎯 Common Patterns

### Type Guards (instead of `any`)
```typescript
function isString(val: unknown): val is string {
  return typeof val === 'string'
}

function processValue(val: unknown) {
  if (isString(val)) {
    return val.toUpperCase() // TypeScript knows it's string
  }
  return undefined
}
```

### Immutable Updates
```typescript
// Arrays
const newArray = [...oldArray, newItem]
const filtered = oldArray.filter(x => x !== removed)

// Objects
const updated = { ...oldObject, key: newValue }

// Nested
const nested = {
  ...state,
  user: {
    ...state.user,
    email: newEmail
  }
}
```

### Early Exits (performance)
```typescript
const synchronizer = (changes, store) => {
  // Exit early if no work needed
  if (changes.length === 0) return changes

  const registry = store.__internal.sideEffects.feature
  if (!registry.hasRegistrations()) return changes

  // Do work...
}
```

---

## 📊 Performance Checklist

✅ Use graphology's built-in algorithms
✅ Cache expensive computations (invalidate on change)
✅ Early exit when no work needed
✅ Batch operations where possible
✅ Profile before optimizing
✅ Test that optimizations actually help

---

## 🧪 Testing Patterns

### Use-Case Tests (preferred)
```typescript
test('complex form with sync, validation, aggregation', () => {
  // Setup realistic scenario
  const store = createGenericStore<FormState>()

  // Register multiple side effects
  store.useSideEffects('all-features', {
    syncPaths: { ... },
    validators: { ... },
    aggregations: { ... }
  })

  // Test user workflow
  // ...
})
```

### Type Error Tests
```typescript
test('rejects invalid path', () => {
  // @ts-expect-error - Testing type safety
  const [value] = store.useStore('invalid.path')
})
```

---

## 📁 File Organization

```
src/
  types/                 # Type utilities
  store/                 # Core store, utils
  hooks/                 # React hooks
  sideEffects/          # Side effect registries
    syncPaths/
    flipPaths/
    listeners/
    validators/
    aggregations/
    clearPaths/
  pipeline/             # Synchronizers
    synchronizers/
  index.ts              # Public API
```

---

## 🔗 Quick Links

- Architecture: `00-ARCHITECTURE-UPDATE.md`
- Task Nav: `README.md`
- Summary: `IMPLEMENTATION-SUMMARY.md`
- graphology: https://graphology.github.io/
- remeda: https://remedajs.com/

---

**Remember**: When in doubt, ASK! 🙋‍♂️
