# Phase 2, Task 03: Base Store Implementation

**Task IDs**: APEX-10, APEX-11, APEX-12, APEX-13
**Priority**: P0 (Critical)
**Dependencies**: Task 01 (Project Setup), Task 02 (Core Types)
**Phase**: Core Store

---

## 🎯 Worker Prompt

**YOU ARE**: A Valtio integration specialist
**YOUR FOCUS**: Create the core store with valtio proxy, derived value support, and React context
**STAY FOCUSED**: Do NOT implement hooks yet or side-effects. Focus on store creation and Provider component.
**SUCCESS MEANS**: Store initializes with valtio, derived getters work automatically, Provider wraps context

---

## 📋 Task Breakdown

### APEX-10: Create Base Store with Valtio Proxy

Implement the `createGenericStore<DATA, META>()` function that initializes the core store.

**What to do:**
1. Create function that accepts optional configuration
2. Initialize valtio proxy with state
3. Set up React Context for Provider/Consumer pattern
4. Return object with Provider and store access methods

### APEX-11: Implement Derived Values Auto-Detection

Automatically detect and optimize getter properties for reactive derived values.

**What to do:**
1. Scan initialState object for getter properties (`get myValue() {}`)
2. Track `this.x.y.z` accesses within getters
3. Use valtio's derive or proxyWithComputed for optimization
4. Ensure re-evaluation only when dependencies change
5. Handle nested objects recursively

### APEX-12: Implement Deep Get/Set Utilities with Lodash

Create safe utilities for deep path access and mutation.

**What to do:**
1. Wrap lodash `_.get` for safe deep reads
2. Wrap lodash `_.set` for safe deep writes
3. Integrate with valtio proxy to maintain reactivity
4. Handle undefined paths gracefully
5. Type-safe path access using DeepKey/DeepValue

### APEX-13: Implement Store Provider Component

Create the React Provider component that wraps context.

**What to do:**
1. Provider component accepts `initialState` and optional `errorStorePath`
2. Initialize store with derived value detection
3. Provide store via React Context
4. Support multiple Provider instances with different states

---

## ✅ Acceptance Criteria

### APEX-10 Criteria:
- [ ] `createGenericStore<DATA, META>()` exported from `src/store/createStore.ts`
- [ ] Function returns object with `Provider` component
- [ ] Valtio `proxy()` used for state management
- [ ] React Context created for store access
- [ ] Configuration accepts optional `errorStorePath` parameter
- [ ] Internal store reference maintained for hook access

### APEX-11 Criteria:
- [ ] Getter properties automatically detected on initialization
- [ ] `this.property.access` within getters tracked for reactivity
- [ ] Derived values re-compute only when dependencies change
- [ ] Works with nested objects (recursively processes all levels)
- [ ] Uses valtio's recommended pattern (proxyWithComputed, derive, or subscribeKey)
- [ ] Test demonstrates: getter with dependencies updates reactively

### APEX-12 Criteria:
- [ ] `deepGet(state, path)` utility in `src/store/utils/deepAccess.ts`
- [ ] `deepSet(state, path, value)` utility maintains valtio reactivity
- [ ] Type-safe: path parameter typed as `DeepKey<DATA>`
- [ ] Value parameter typed as `DeepValue<DATA, Path>`
- [ ] Handles undefined/null paths without errors
- [ ] Uses lodash `_.get` and `_.set` internally

### APEX-13 Criteria:
- [ ] Provider component properly typed with React.FC or similar
- [ ] Accepts `initialState: DATA` prop
- [ ] Accepts optional `errorStorePath?: string` prop (default: "_errors")
- [ ] Initializes store on mount with derived value detection
- [ ] Provides store via Context.Provider
- [ ] Test renders Provider without errors

---

## 📦 Expected Output

### File Structure:

```
src/
  store/
    createStore.ts           # Main createGenericStore function
    StoreContext.tsx         # React Context definition
    Provider.tsx             # Provider component
    utils/
      deepAccess.ts          # deepGet/deepSet utilities
      deriveValues.ts        # Derived value detection logic
    types.ts                 # Store-specific types
  index.ts                   # Re-export createGenericStore

tests/
  store/
    createStore.test.tsx     # Basic store creation tests
    derived.test.tsx         # Derived values tests
    provider.test.tsx        # Provider component tests
```

### src/store/createStore.ts signature:

```typescript
import type { GenericMeta } from '../types'

export interface StoreConfig {
  errorStorePath?: string  // Default: "_errors"
}

export interface StoreReturn<DATA, META extends GenericMeta> {
  Provider: React.FC<ProviderProps<DATA>>
  // More exports added in later tasks
}

export function createGenericStore<
  DATA extends object,
  META extends GenericMeta = GenericMeta
>(config?: StoreConfig): StoreReturn<DATA, META>
```

### Example derived value detection:

```typescript
// Input state:
const initialState = {
  firstName: 'John',
  lastName: 'Doe',
  get fullName() {
    return `${this.firstName} ${this.lastName}`
  }
}

// Should automatically track: firstName, lastName
// When either changes, fullName re-computes
```

### Example usage after completion:

```typescript
import { createGenericStore } from '@sladg/apex-state'

type AppState = {
  user: {
    name: string
    age: number
  }
  count: number
}

const store = createGenericStore<AppState>()

function App() {
  return (
    <store.Provider initialState={{ user: { name: 'Alice', age: 30 }, count: 0 }}>
      {/* children */}
    </store.Provider>
  )
}
```

---

## 🧪 Verification Steps

### Unit Tests:

```typescript
import { render } from '@testing-library/react'
import { createGenericStore } from '../src/store/createStore'

test('creates store and Provider', () => {
  const store = createGenericStore<{ count: number }>()
  expect(store.Provider).toBeDefined()
})

test('derived values work', () => {
  type State = {
    a: number
    b: number
    get sum() { return this.a + this.b }
  }

  const store = createGenericStore<State>()
  // Test that sum updates when a or b changes
})

test('Provider accepts initialState', () => {
  const store = createGenericStore<{ value: string }>()
  const { container } = render(
    <store.Provider initialState={{ value: 'test' }}>
      <div>Child</div>
    </store.Provider>
  )
  expect(container).toBeInTheDocument()
})
```

### Manual Verification:

```bash
# Type check
npm run type-check

# Build
npm run build

# Run tests
npm test
```

---

## 🚨 Common Pitfalls

- **DON'T**: Implement hooks yet - that's task 04
- **DON'T**: Implement side-effects - that comes later
- **DON'T**: Over-engineer derived value detection - keep it simple with valtio's patterns
- **DO**: Follow valtio's latest documentation for proxy patterns
- **DO**: Test that derived values actually re-compute correctly
- **DO**: Ensure deepSet maintains valtio's reactivity (don't break the proxy)
- **DO**: Handle edge cases: empty objects, null values, undefined paths

---

## 💡 Implementation Tips

### Valtio Derived Values:

Valtio has multiple patterns for derived values:

1. **proxyWithComputed** (recommended):
```typescript
import { proxyWithComputed } from 'valtio/utils'

const state = proxyWithComputed(
  { count: 1 },
  { doubled: snap => snap.count * 2 }
)
```

2. **derive** utility:
```typescript
import { derive } from 'valtio/utils'

const derived = derive({
  fullName: (get) => `${get(state).firstName} ${get(state).lastName}`
})
```

Choose the pattern that best handles auto-detection of getters.

### Deep Access with Lodash:

```typescript
import _get from 'lodash/get'
import _set from 'lodash/set'

export function deepGet<T, P extends DeepKey<T>>(
  obj: T,
  path: P
): DeepValue<T, P> | undefined {
  return _get(obj, path)
}

export function deepSet<T, P extends DeepKey<T>>(
  obj: T,
  path: P,
  value: DeepValue<T, P>
): void {
  _set(obj, path, value)
  // Ensure valtio reactivity is maintained
}
```

### Provider Pattern:

```typescript
const StoreContext = createContext<StoreInstance | null>(null)

export const Provider: React.FC<ProviderProps<DATA>> = ({
  initialState,
  errorStorePath = '_errors',
  children
}) => {
  const store = useMemo(() =>
    initializeStore(initialState, errorStorePath),
    [] // Only initialize once
  )

  return (
    <StoreContext.Provider value={store}>
      {children}
    </StoreContext.Provider>
  )
}
```

---

## 📚 Reference Materials

- Valtio documentation: https://github.com/pmndrs/valtio
- Valtio utils: proxyWithComputed, derive, subscribeKey
- Lodash get/set: https://lodash.com/docs/#get
- React Context: https://react.dev/reference/react/createContext

---

## ➡️ Next Steps

Once this task is complete, proceed to:
- **Task 04**: `04-basic-hooks.md` - Implement useStore, useJitStore, useSideEffects
