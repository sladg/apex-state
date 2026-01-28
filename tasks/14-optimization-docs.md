# Phase 8, Task 14: Performance Optimization & Documentation

**Task IDs**: APEX-47, APEX-48, APEX-49
**Priority**: P1-P3
**Dependencies**: All implementation and testing tasks
**Phase**: Optimization & Polish

---

## 🎯 Worker Prompt

**YOU ARE**: A performance optimization expert and technical writer
**YOUR FOCUS**: Optimize critical hot paths and add minimal documentation
**STAY FOCUSED**: Profile first, optimize second. Keep docs minimal (TSDoc only for now).
**SUCCESS MEANS**: Fast execution on common operations, clear API documentation

---

## 📋 Task Breakdown

### APEX-49: Performance Optimization Pass ⚡

**Critical hot paths to optimize:**
1. Sync paths resolution (MOST CRITICAL - runs on nearly every change)
2. Aggregation processing (HIGH PRIORITY - frequent with sync)
3. Change pipeline execution
4. Graph lookups (use graphology's optimized methods)

**Optimization techniques:**
- Memoization of expensive computations
- Early exits when no work needed
- Batch operations where possible
- Use graphology's optimized algorithms
- Profile before and after

### APEX-47: TSDoc Comments

**What to document:**
- All exported types with examples
- All hooks with usage examples
- createGenericStore function
- Side effect configurations
- Parameter descriptions

**Keep it minimal:**
- Brief description
- Usage example
- Important notes only

### APEX-48: Basic README

**Sections:**
- Installation
- Quick start example
- Basic usage
- Link to types as docs
- Contribution guide link

---

## ✅ Acceptance Criteria

### APEX-49 Criteria:
- [ ] Profiling results documented (before/after)
- [ ] Sync paths resolution optimized (target: < 1ms for 100 paths)
- [ ] Aggregation processing optimized (target: < 2ms for 50 aggregations)
- [ ] Graph lookups use graphology's optimized methods
- [ ] Memoization added to expensive functions
- [ ] Early exit logic in synchronizers
- [ ] Benchmark tests pass performance targets
- [ ] Code comments explain optimizations

### APEX-47 Criteria:
- [ ] All exported types have TSDoc
- [ ] All hooks have TSDoc with examples
- [ ] createGenericStore documented
- [ ] Side effect configs documented
- [ ] Examples are runnable and correct
- [ ] Generated types documentation looks good

### APEX-48 Criteria:
- [ ] README.md in root
- [ ] Installation instructions clear
- [ ] Quick start example works
- [ ] Feature overview included
- [ ] Links to TypeScript types
- [ ] Professional appearance
- [ ] No marketing fluff - technical focus

---

## 📦 Expected Output

### File Structure:

```
/
  README.md                    # Basic README
  src/
    (all files updated with TSDoc)
  tests/
    performance/
      benchmarks.test.ts       # Performance benchmarks
  docs/
    (optional: generated from TSDoc later)
```

---

## 🎯 APEX-49: Performance Optimization

### Profiling Setup:

```typescript
// tests/performance/benchmarks.test.ts
import { describe, test, expect } from 'vitest'
import { performance } from 'perf_hooks'

describe('Performance Benchmarks', () => {
  test('sync paths: 100 paths resolve in < 1ms', () => {
    // Setup store with 100 sync pairs
    // ...

    const iterations = 1000
    const start = performance.now()

    for (let i = 0; i < iterations; i++) {
      // Trigger sync path resolution
      setChanges([['field0', i, {}]])
    }

    const duration = (performance.now() - start) / iterations
    expect(duration).toBeLessThan(1) // < 1ms per operation
  })

  test('aggregations: 50 aggregations process in < 2ms', () => {
    // Similar benchmark for aggregations
  })

  test('full pipeline: 100 side effects in < 5ms', () => {
    // End-to-end pipeline benchmark
  })
})
```

### Optimization: Sync Paths (CRITICAL)

**Before:**
```typescript
// Naive approach: recompute transitive closure every time
function getSyncedPaths(path: DeepKey<DATA>): Set<DeepKey<DATA>> {
  const visited = new Set()
  const queue = [path]
  // BFS traversal every call... SLOW!
}
```

**After:**
```typescript
// Use graphology's optimized connected components
import { connectedComponents } from 'graphology-components'

const createSyncPathsRegistry = () => {
  const graph = new Graph({ type: 'undirected' })
  let componentCache: Map<string, Set<string>> | null = null

  return {
    register: (id, path1, path2) => {
      graph.addEdge(path1, path2)
      componentCache = null // Invalidate cache
    },

    getSyncedPaths: (path) => {
      // Lazy recompute components
      if (!componentCache) {
        componentCache = new Map()
        const components = connectedComponents(graph)

        components.forEach(component => {
          component.forEach(node => {
            componentCache.set(node, new Set(component))
          })
        })
      }

      return componentCache.get(path) || new Set()
    }
  }
}
```

**Why this is faster:**
- graphology's connectedComponents is highly optimized
- We cache the result and only recompute when graph changes
- O(1) lookup after initial computation instead of O(V+E) every time

### Optimization: Aggregations

**Before:**
```typescript
// Check all aggregations on every change
for (const aggregation of allAggregations) {
  const sourceValues = aggregation.sourcePaths.map(p => deepGet(state, p))
  // ... expensive lookups
}
```

**After:**
```typescript
// Only check affected aggregations
const createAggregationsRegistry = () => {
  const sourceToTargets = new Map<string, Set<string>>()

  return {
    getAffectedTargets: (changedPath) => {
      // O(1) lookup instead of iterating all
      return sourceToTargets.get(changedPath) || new Set()
    }
  }
}

// In synchronizer: only process affected aggregations
const affectedTargets = registry.getAffectedTargets(path)
for (const target of affectedTargets) {
  // Only check this target's aggregations
}
```

### Optimization: Pipeline Execution

**Early Exit:**
```typescript
const executePipeline = pipe(
  // Add early exit logic
  (changes, store) => {
    if (changes.length === 0) return changes // No work needed
    return syncPathsSynchronizer(changes, store)
  },
  (changes, store) => {
    if (!store.__internal.sideEffects.flipPaths.hasAnyRegistrations()) {
      return changes // Skip if no flip paths registered
    }
    return flipPathsSynchronizer(changes, store)
  }
  // ... more synchronizers with early exits
)
```

---

## 📝 APEX-47: TSDoc Examples

### src/index.ts:

```typescript
/**
 * @sladg/apex-state - Advanced state management for React with valtio
 *
 * A powerful wrapper around valtio providing:
 * - Type-safe deep path access
 * - Automatic derived values
 * - Side effects: sync, flip, listeners, validators, aggregations, clear
 * - Form field hooks with transformations
 * - Optimized for performance
 *
 * @example
 * ```tsx
 * import { createGenericStore } from '@sladg/apex-state'
 * import { z } from 'zod'
 *
 * type AppState = {
 *   user: {
 *     email: string
 *     isActive: boolean
 *   }
 * }
 *
 * const store = createGenericStore<AppState>()
 *
 * function App() {
 *   return (
 *     <store.Provider initialState={{ user: { email: '', isActive: false } }}>
 *       <UserForm />
 *     </store.Provider>
 *   )
 * }
 *
 * function UserForm() {
 *   const email = store.useFieldStore('user.email')
 *   const [isActive, setActive] = store.useStore('user.isActive')
 *
 *   return (
 *     <div>
 *       <input value={email.value} onChange={e => email.setValue(e.target.value)} />
 *       <button onClick={() => setActive(!isActive)}>Toggle</button>
 *     </div>
 *   )
 * }
 * ```
 *
 * @packageDocumentation
 */

/**
 * Creates a generic store with type-safe state management.
 *
 * @typeParam DATA - The shape of your application state
 * @typeParam META - Metadata type for changes (extends GenericMeta)
 *
 * @param config - Optional configuration
 * @param config.errorStorePath - Path where validation errors are stored (default: "_errors")
 *
 * @returns Store instance with Provider, hooks, and side effects
 *
 * @example
 * ```typescript
 * type State = { count: number }
 * const store = createGenericStore<State>({ errorStorePath: 'errors' })
 * ```
 */
export function createGenericStore<
  DATA extends object,
  META extends GenericMeta = GenericMeta
>(config?: StoreConfig): StoreReturn<DATA, META>
```

### src/hooks/useStore.ts:

```typescript
/**
 * Hook for accessing and updating a specific path in the store.
 *
 * Similar to React's useState but for store paths.
 * Returns a tuple: [value, setValue]
 *
 * @typeParam DATA - Store state type
 * @typeParam P - Path type (automatically inferred)
 * @typeParam META - Metadata type
 *
 * @param path - Dot-notation path to the value
 *
 * @returns Tuple of [currentValue, setValueFunction]
 *
 * @example
 * ```tsx
 * function Component() {
 *   const [email, setEmail] = store.useStore('user.email')
 *   const [count, setCount] = store.useStore('count')
 *
 *   return (
 *     <div>
 *       <input value={email} onChange={e => setEmail(e.target.value)} />
 *       <button onClick={() => setCount(count + 1)}>{count}</button>
 *     </div>
 *   )
 * }
 * ```
 */
export function useStore<
  DATA extends object,
  P extends DeepKey<DATA>,
  META extends GenericMeta = GenericMeta
>(path: P): [DeepValue<DATA, P>, (value: DeepValue<DATA, P>, meta?: META) => void]
```

### src/types/deepKey.ts:

```typescript
/**
 * Generates a union of all possible dot-notation paths for nested objects.
 *
 * Recursively explores the object structure to create type-safe path strings.
 *
 * @typeParam T - The object type to generate paths for
 *
 * @example
 * ```typescript
 * type User = {
 *   name: string
 *   address: {
 *     street: string
 *     city: string
 *   }
 * }
 *
 * // DeepKey<User> is: "name" | "address" | "address.street" | "address.city"
 *
 * const path: DeepKey<User> = "address.street" // ✅ Valid
 * const invalid: DeepKey<User> = "address.zipcode" // ❌ Type error
 * ```
 */
export type DeepKey<T> = // ... implementation
```

---

## 📄 APEX-48: README.md

```markdown
# @sladg/apex-state

Advanced state management for React, built on valtio with powerful side-effects.

## Features

- 🎯 **Type-safe deep paths** - Access nested state with dot notation and full TypeScript inference
- ⚡ **Automatic derived values** - Getters are automatically optimized and reactive
- 🔄 **Powerful side effects** - Sync, flip, aggregate, validate, and clear paths declaratively
- 📝 **Form-friendly hooks** - Built-in hooks for common form patterns
- 🚀 **Optimized performance** - Fast even with hundreds of side effects
- 🧩 **Functional design** - Composable, testable, predictable

## Installation

```bash
npm install @sladg/apex-state valtio zod react
```

## Quick Start

```tsx
import { createGenericStore } from '@sladg/apex-state'
import { z } from 'zod'

// Define your state type
type AppState = {
  user: {
    email: string
    password: string
  }
  _errors?: Record<string, Array<{ id: string, message: string }>>
}

// Create store
const store = createGenericStore<AppState>()

function App() {
  return (
    <store.Provider initialState={{
      user: { email: '', password: '' }
    }}>
      <LoginForm />
    </store.Provider>
  )
}

function LoginForm() {
  // Use side effects
  store.useSideEffects('validation', {
    validators: {
      validators: [{
        id: 'email-validator',
        scope: 'user.email',
        schema: z.string().email(),
        errorPath: 'user.email'
      }]
    }
  })

  // Access state
  const email = store.useFieldStore('user.email')
  const password = store.useFieldStore('user.password')
  const errors = store.useErrors('user.email')

  return (
    <form>
      <input
        value={email.value}
        onChange={e => email.setValue(e.target.value)}
      />
      {errors.map(err => <span key={err}>{err}</span>)}

      <input
        type="password"
        value={password.value}
        onChange={e => password.setValue(e.target.value)}
      />

      <button type="submit">Login</button>
    </form>
  )
}
```

## Core Concepts

### Side Effects

Declare side effects using `useSideEffects`:

**Sync Paths** - Keep paths in sync
```tsx
store.useSideEffects('sync', {
  syncPaths: {
    pairs: [
      { id: 'email-sync', path1: 'user.email', path2: 'profile.email' }
    ]
  }
})
```

**Validators** - Zod schema validation
```tsx
store.useSideEffects('validation', {
  validators: {
    validators: [{
      id: 'age-validator',
      scope: 'user.age',
      schema: z.number().min(18),
      errorPath: 'user.age'
    }]
  }
})
```

**Aggregations** - Multi-way sync with logic
```tsx
store.useSideEffects('aggregate', {
  aggregations: {
    rules: [{
      id: 'all-agreed',
      targetPath: 'allAgreements',
      sourcePaths: ['agreeToTerms', 'agreeToPrivacy']
      // When all sources equal → target = that value
      // When sources differ → target = undefined
      // When target changes → all sources = target value
    }]
  }
})
```

See [full documentation](./docs) for more side effects: flip, listeners, clear.

### Hooks

- `useStore(path)` - Get and set value at path
- `useFieldStore(path)` - Object API for forms: `{value, setValue}`
- `useFieldTransformedStore(path, config)` - Transform values for display
- `useJitStore()` - Batch operations: `{proxyValue, setChanges, getState}`
- `useErrors(path)` - Get validation errors for path
- `useSideEffects(id, effects)` - Register side effects

## TypeScript

Full type safety with deep path inference:

```typescript
type State = {
  user: {
    profile: {
      name: string
    }
  }
}

// ✅ Valid
const [name] = store.useStore('user.profile.name') // Type: string

// ❌ Type error
const [invalid] = store.useStore('user.profile.age')
```

## Performance

Optimized for real-world usage:
- 100 sync paths: < 1ms per change
- 50 aggregations: < 2ms per change
- Full pipeline with all side effects: < 5ms

Built on battle-tested libraries:
- [valtio](https://github.com/pmndrs/valtio) - Proxy-based reactivity
- [graphology](https://graphology.github.io/) - High-performance graphs
- [zod](https://zod.dev/) - Schema validation

## Contributing

See [CONTRIBUTING.md](./CONTRIBUTING.md)

## License

MIT © sladg
```

---

## 🧪 Verification Steps

```bash
# Run performance benchmarks
npm run test:performance

# Verify benchmarks pass
# - Sync paths: < 1ms
# - Aggregations: < 2ms
# - Full pipeline: < 5ms

# Generate TypeScript docs (optional)
npm run docs

# Build and check bundle size
npm run build
du -sh dist/

# Verify exports work
npm pack
npm install apex-state-0.1.0.tgz
```

---

## 🚨 Common Pitfalls

- **DON'T**: Optimize prematurely - profile first
- **DON'T**: Micro-optimize at expense of readability
- **DON'T**: Write novels in TSDoc - keep it concise
- **DO**: Use graphology's built-in algorithms (they're optimized)
- **DO**: Add early exits to avoid unnecessary work
- **DO**: Cache expensive computations
- **DO**: Test that optimizations actually improve performance

---

## 💡 Optimization Checklist

- [ ] Profile hot paths with real workloads
- [ ] Use graphology's optimized algorithms
- [ ] Add memoization where appropriate
- [ ] Implement early exits in synchronizers
- [ ] Cache computed values (invalidate on change)
- [ ] Batch operations where possible
- [ ] Verify improvements with benchmarks
- [ ] Document why optimizations were made

---

## 🎉 Project Complete!

Once this task is done, the package is ready for use:
- ✅ Full feature set implemented
- ✅ Comprehensive tests passing
- ✅ Performance optimized
- ✅ Documented with TSDoc
- ✅ README with examples

Next steps (outside this task list):
- CI/CD setup
- npm publish workflow
- Contribution guidelines
- Advanced documentation site (optional)
- Example projects
