# Architecture Update - Functional Programming Approach

**IMPORTANT**: This document supersedes class-based approaches in other task files.

---

## 🎯 Key Architectural Decisions

### 1. **Functional Programming - NO Classes**

All implementations must use:
- Pure functions with scoped closures
- Factory functions instead of classes
- Immutable data structures where possible
- Composition over inheritance

### 2. **High-Performance Graph Libraries**

For graph operations (sync paths, aggregations), use established libraries:

**Recommended: `graphology`**
- Fast, well-tested graph library
- Excellent TypeScript support
- Built-in algorithms: BFS, DFS, cycle detection, connected components
- Efficient data structures
- npm: `graphology` + `graphology-types`

**Alternative: `graphlib`**
- Lightweight directed graph library
- Good for dependency graphs
- Simple API

**Why not build from scratch?**
- Graph algorithms are complex and error-prone
- Performance optimizations already done
- Battle-tested in production
- Better TypeScript types

### 3. **Internal State Pattern**

Store structure using valtio proxy:

```typescript
const store = proxy({
  // User data
  values: initialState as DATA,

  // Internal framework state (not reactive to users)
  __internal: {
    // Side effects registries
    sideEffects: {
      syncPaths: createSyncPathsRegistry(),
      flipPaths: createFlipPathsRegistry(),
      listeners: createListenersRegistry(),
      validators: createValidatorsRegistry(),
      aggregations: createAggregationsRegistry(),
      clearPaths: createClearPathsRegistry(),
    },

    // Graphs for dependencies
    graphs: {
      syncPaths: new Graph(), // from graphology
      aggregations: new DirectedGraph(),
    },

    // Pipeline state
    incomingChanges: [] as ArrayOfChanges<DATA, META>,
    processingChanges: false,

    // Configuration
    config: {
      errorStorePath: '_errors',
      maxPipelineIterations: 100,
    }
  }
})
```

### 4. **Pipe-Style Functions**

When processing changes through pipeline, use functional composition:

```typescript
import { pipe } from 'remeda' // or 'ramda' or build simple pipe

const processChanges = pipe(
  syncPathsSynchronizer,
  flipPathsSynchronizer,
  listenersSynchronizer,
  validatorsSynchronizer,
  aggregationsSynchronizer,
  clearPathsSynchronizer
)

const finalChanges = processChanges(initialChanges, store)
```

### 5. **TypeScript Strictness - Avoid `any`**

**CRITICAL RULE**: The `any` type should be used ONLY when absolutely necessary for type matching.

**When `any` is acceptable:**
- Generic type constraints where exact type is unknown: `T extends Record<string, any>`
- Working with external libraries with poor types
- Truly dynamic runtime values where type cannot be known

**When `any` is NOT acceptable:**
- Laziness or taking shortcuts
- "I'll fix the types later"
- Avoiding TypeScript errors instead of solving them

**In tests - use `@ts-expect-error`:**
```typescript
// ✅ CORRECT: Testing invalid types
test('should reject invalid path', () => {
  // @ts-expect-error - Testing type error for invalid path
  const [value] = store.useStore('invalid.path.that.does.not.exist')
})

// ❌ WRONG: Using any to bypass types
test('should reject invalid path', () => {
  const [value] = store.useStore('invalid.path' as any) // BAD!
})
```

**Prefer `unknown` over `any`:**
```typescript
// ✅ CORRECT: Force type checking
function processValue(val: unknown) {
  if (typeof val === 'string') {
    return val.toUpperCase()
  }
  // Must check type before using
}

// ❌ WRONG: Skip type safety
function processValue(val: any) {
  return val.toUpperCase() // No safety!
}
```

### 6. **When Stuck - ASK FOR GUIDANCE**

**IMPORTANT PROTOCOL**: If you encounter any of these situations, STOP and ask the user for guidance:

**Stop and ask when:**
- Type inference is complex and you're tempted to use `any`
- Multiple implementation approaches seem valid
- Requirements are ambiguous or unclear
- Performance trade-offs are significant
- Breaking changes might be needed
- Architectural decisions affect multiple tasks
- Circular dependencies or complex coupling emerges
- Edge cases have unclear expected behavior

**How to ask:**
```typescript
// Don't do this:
const result: any = complexOperation() // I'll figure it out later

// Do this instead:
// TODO: BLOCKED - Need user guidance
// Question: Should deepGet return undefined or throw for invalid paths?
// Options:
//   A) Return undefined (graceful)
//   B) Throw error (fail fast)
//   C) Return Result type with success/error
// Current blocker: Affects error handling across all hooks
```

**Better to ask than to:**
- Implement the wrong thing
- Use `any` everywhere
- Make assumptions about requirements
- Create technical debt

---

## 📚 Recommended Packages

### Core Dependencies:
- **`graphology`** (^0.25.0) - Graph data structures
- **`graphology-shortest-path`** - Path algorithms
- **`graphology-components`** - Connected components
- **`graphology-cycles`** - Cycle detection
- **`remeda`** or **`ramda`** - Functional utilities with pipe

### Why these packages?

**graphology**:
- Used by Observable, Sigma.js, and other production systems
- ~20k downloads/week
- Excellent performance benchmarks
- TypeScript-first design

**remeda** (preferred over ramda):
- TypeScript-native (better type inference)
- Tree-shakeable
- Modern API (ramda is older)
- Similar pipe functionality

---

## 🔄 Updated Patterns

### Factory Function Pattern (not classes):

```typescript
// ❌ OLD: Class-based
class SyncPathsRegistry {
  private graph = new Map()
  register() { }
}

// ✅ NEW: Functional with closure
function createSyncPathsRegistry<DATA>() {
  const graph = new Graph()
  const edgeIds = new Map<string, string>()

  return {
    register: (id: string, path1: DeepKey<DATA>, path2: DeepKey<DATA>) => {
      // Implementation with closure over graph, edgeIds
    },

    unregister: (id: string) => {
      // Implementation
    },

    getSyncedPaths: (path: DeepKey<DATA>) => {
      // Implementation
    }
  }
}
```

### Pipeline with Pipe:

```typescript
import { pipe } from 'remeda'

// Each synchronizer is a pure function
type Synchronizer<DATA, META> = (
  changes: ArrayOfChanges<DATA, META>,
  store: StoreInstance<DATA, META>
) => ArrayOfChanges<DATA, META>

// Create pipeline
const pipeline = pipe(
  (changes, store) => syncPathsSynchronizer(changes, store),
  (changes, store) => flipPathsSynchronizer(changes, store),
  (changes, store) => listenersSynchronizer(changes, store),
  // ... more synchronizers
)

// Execute
const finalChanges = pipeline(initialChanges, store)
```

### Accessing Internal State:

```typescript
// In synchronizers, access internal state
function syncPathsSynchronizer<DATA, META>(
  changes: ArrayOfChanges<DATA, META>,
  store: StoreInstance<DATA, META>
): ArrayOfChanges<DATA, META> {
  const { __internal } = store
  const registry = __internal.sideEffects.syncPaths
  const graph = __internal.graphs.syncPaths

  // Use graph algorithms from graphology
  const connectedPaths = connectedComponents(graph)

  // Process changes...
}
```

---

## 🎨 Updated Code Style

### Prefer Composition:

```typescript
// Compose small functions
const processSyncPaths = pipe(
  findAffectedPaths,
  generateSyncChanges,
  filterDuplicates,
  addMetaFlags
)
```

### Use Currying for Configuration:

```typescript
// Configure once, use many times
const createValidator = (schema: ZodSchema) => (data: unknown) =>
  schema.safeParse(data)

const emailValidator = createValidator(z.string().email())
const passwordValidator = createValidator(z.string().min(8))
```

### Immutable Updates:

```typescript
// Use spread operators, not mutations
const addChange = (changes: ArrayOfChanges, newChange: Change) => [
  ...changes,
  newChange
]

// For arrays, use methods that don't mutate
const filtered = changes.filter(c => !c.meta.isSyncChange)
```

---

## 📝 Updated Task Guidelines

### When implementing tasks:

1. **Replace classes with factory functions**
   - Any `class X` should become `function createX()`
   - Use closures for private state
   - Return object with methods

2. **Use graphology for graphs**
   - Don't implement graph algorithms from scratch
   - Import from graphology packages
   - Reference their docs for API

3. **Use pipe for multi-step operations**
   - Import from remeda: `import { pipe } from 'remeda'`
   - Chain synchronizers with pipe
   - Keep functions pure

4. **Access __internal for framework state**
   - Never expose __internal to users
   - Store graphs, registries, config here
   - Keep user data in `values`

5. **Write functional, composable code**
   - Small, single-purpose functions
   - Pure functions when possible
   - Use currying for configuration

6. **Avoid `any` type - maintain type safety**
   - Use `unknown` and type guards instead
   - Only use `any` for genuine type matching needs
   - In tests: use `@ts-expect-error` for invalid type tests
   - Ask for guidance if types become too complex

7. **When stuck - STOP and ask**
   - Don't guess at requirements
   - Don't use `any` as a workaround
   - Don't make architectural decisions alone
   - Document the blocker clearly and ask the user

---

## 🔧 Updated Dependencies

Add to package.json:

```json
{
  "dependencies": {
    "valtio": "latest",
    "lodash": "^4.17.21",
    "deepdash": "^5.3.9",
    "graphology": "^0.25.4",
    "graphology-shortest-path": "^2.0.2",
    "graphology-components": "^1.5.2",
    "graphology-cycles": "^1.0.0",
    "remeda": "^2.0.0"
  },
  "peerDependencies": {
    "react": "^18.0.0",
    "zod": "^3.0.0"
  },
  "devDependencies": {
    "@types/lodash": "^4.14.0",
    "graphology-types": "^0.24.0"
  }
}
```

---

## 💡 Benefits of This Approach

### Performance:
- graphology is highly optimized (C++ compiled in some cases)
- Functional pipes can be optimized by JS engines
- Immutable operations enable better optimization

### Maintainability:
- Pure functions are easy to test
- No hidden state in classes
- Clear data flow through pipes
- Composable and reusable

### Type Safety:
- graphology has excellent TypeScript support
- remeda has better type inference than ramda
- Functional patterns work well with TS

---

## 🚀 Migration Notes for Existing Tasks

For tasks 06-11 (side effects):
- Replace `class XRegistry` with `function createXRegistry()`
- Use graphology Graph/DirectedGraph instead of custom graph implementations
- Import cycle detection, path finding from graphology packages
- Use remeda's pipe for synchronizer composition
- Access registries from `store.__internal.sideEffects.X`

---

## 📚 Learning Resources

- graphology docs: https://graphology.github.io/
- graphology algorithms: https://graphology.github.io/standard-library/
- remeda docs: https://remedajs.com/
- Functional programming patterns in TypeScript

---

## ⚠️ CRITICAL RULES SUMMARY

### 🚫 NEVER:
1. ❌ **Use classes** - Use factory functions with closures
2. ❌ **Use `any` carelessly** - Use `unknown` + type guards, or ask for guidance
3. ❌ **Guess at requirements** - Stop and ask the user when unclear
4. ❌ **Build custom graphs** - Use graphology
5. ❌ **Skip tests** - Write use-case tests as you go
6. ❌ **Mutate data** - Use immutable patterns
7. ❌ **Use `any` in tests** - Use `@ts-expect-error` for type error tests

### ✅ ALWAYS:
1. ✅ **Factory functions** - `createX()` pattern with closures
2. ✅ **Type safety** - Proper generics, avoid `any`, prefer `unknown`
3. ✅ **Ask when stuck** - Document blocker and ask user
4. ✅ **Use graphology** - For sync paths and aggregation graphs
5. ✅ **Pipe composition** - Use remeda's `pipe` for multi-step operations
6. ✅ **Functional style** - Pure functions, immutability, composition
7. ✅ **__internal pattern** - Keep framework state separate from user data

### 🎯 WHEN TO ASK USER:
- Complex types tempting you to use `any`
- Multiple valid implementation approaches
- Ambiguous requirements
- Significant performance trade-offs
- Breaking changes needed
- Architectural decisions affecting multiple tasks
- Unclear edge case behavior

**Remember**: Better to ask and get it right than to implement wrong assumptions!

---

This architecture update should be applied to all subsequent tasks!
