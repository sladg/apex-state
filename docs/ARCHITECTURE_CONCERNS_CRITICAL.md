---
created: 2026-01-28 (37db2cf)
updated: 2026-02-11 (bd07f7d)
status: active
---

# Critical Architecture: Valtio Execution Order & Concerns Storage

**Document Type**: Architecture Decision & Patterns
**Priority**: P0 (Critical)
**Dependencies**: None
**Related**: concerns-clean.ts, concerns-working-v2.ts

---

## 🎯 Purpose

This document answers critical questions about valtio execution order and concerns storage architecture to prevent infinite loops and race conditions.

---

## ❓ Critical Questions

### Q1: What is the order of valtio's execution? Can there be race conditions?

**Valtio Execution Order**:

```typescript
// Synchronous execution flow
store.proxy.strike = 150         // 1. Proxy setter (synchronous)
  └─> Internal proxy update      // 2. Valtio updates internal state
      └─> effect() callbacks     // 3. Effects run synchronously
          └─> React re-renders   // 4. React schedules re-render (async via scheduler)
```

**Key Points**:

1. **Proxy mutations are synchronous**: When you write `store.proxy.strike = 150`, the proxy updates immediately
2. **Effects run synchronously**: valtio-reactive's `effect()` runs synchronously after mutations
3. **Effects run in registration order**: Effects execute in the order they were registered
4. **React re-renders are async**: React batches re-renders (microtask queue)

**Race Conditions**:

```typescript
// ✅ SAFE: Synchronous batch
store.proxy.strike = 150
store.proxy.status = 'locked'
// Both effects run after both mutations complete (batched)

// ✅ SAFE: Effect execution is synchronous
effect(() => {
  const strike = state.strike
  const status = state.status
  // Both values are always consistent (no intermediate state)
})

// ⚠️ POTENTIAL ISSUE: Async effects
effect(async () => {
  const premium = await fetchPremium(state.strike)  // ❌ Async!
  // state.strike might have changed by the time this resolves
})
```

**Verdict**: No race conditions in synchronous effects, but async effects need careful handling.

---

### Q2: Concerns should be "virtual" fields, right?

**YES! This is absolutely critical.**

#### ❌ WRONG: Storing Concerns in Proxy State

```typescript
// ❌ DON'T DO THIS - Infinite loop!
state = proxy({
  products: {
    'leg-1': { strike: 100 }
  },
  concerns: {
    'products.leg-1.strike': {
      validationState: null,  // ❌ Stored in proxy
      disabled: null
    }
  }
})

effect(() => {
  const strike = state.products['leg-1'].strike
  const isValid = validateStrike(strike)

  // ❌ Writing back to proxy triggers this effect again!
  state.concerns['products.leg-1.strike'].validationState = isValid
  // → Infinite loop! 💥
})
```

**What happens**:
1. User changes `strike` → effect runs
2. Effect writes to `state.concerns` → triggers effect again
3. Effect writes to `state.concerns` → triggers effect again
4. **Stack overflow!** 💥

#### ✅ CORRECT: Virtual Fields (Stored Outside Proxy)

```typescript
// ✅ Concerns stored OUTSIDE the proxy
const dataProxy = proxy({
  products: {
    'leg-1': { strike: 100 }
  }
  // No concerns here!
})

// ✅ Separate cache (not a proxy)
const concernCache = new Map<string, any>()

effect(() => {
  const strike = dataProxy.products['leg-1'].strike
  const isValid = validateStrike(strike)

  // ✅ Writing to cache doesn't trigger effects
  concernCache.set('products.leg-1.strike:validationState', isValid)
})

// Read concerns from cache
const getFieldConcerns = (path: string) => {
  return {
    validationState: concernCache.get(`${path}:validationState`),
    disabled: concernCache.get(`${path}:disabled`)
  }
}
```

**Why this works**:
- ✅ No writes to proxy → no infinite loops
- ✅ Effects only read from proxy (one-way data flow)
- ✅ Concerns are derived/computed values, not state
- ✅ React can still read concerns via `getFieldConcerns()`

---

### Q3: Concerns should be end-state and nothing else listens on them?

**YES! Concerns are terminal/leaf values.**

#### Data Flow Diagram

```
┌─────────────────────────────────────────────────────────────┐
│                         USER INPUT                          │
└─────────────────┬───────────────────────────────────────────┘
                  │
                  ▼
┌─────────────────────────────────────────────────────────────┐
│                    VALTIO PROXY STATE                       │
│  (Single source of truth)                                   │
│                                                             │
│  state.products['leg-1'].strike = 150  ◄─── Write here     │
└─────────────────┬───────────────────────────────────────────┘
                  │
                  │ (effect() reads proxy, triggers on changes)
                  │
                  ▼
┌─────────────────────────────────────────────────────────────┐
│                  CONCERN EVALUATIONS                        │
│  (Read-only computations)                                   │
│                                                             │
│  effect(() => {                                             │
│    const strike = state.products['leg-1'].strike  ◄─ Read  │
│    const isValid = schema.safeParse(strike).success         │
│    concernCache.set(key, isValid)  ◄─────────── Write      │
│  })                                              to cache   │
└─────────────────┬───────────────────────────────────────────┘
                  │
                  │ (React reads from cache)
                  │
                  ▼
┌─────────────────────────────────────────────────────────────┐
│                         REACT UI                            │
│  (Renders based on state + concerns)                        │
│                                                             │
│  const concerns = store.getFieldConcerns(path)  ◄─ Read    │
│  <input disabled={concerns.disabled} />                     │
└─────────────────────────────────────────────────────────────┘
```

**Key Principles**:

1. **One-way data flow**: State → Concerns → UI (never backwards)
2. **Concerns are read-only**: UI reads concerns, never writes
3. **Concerns don't trigger state changes**: No side effects
4. **Concerns are terminal**: Nothing depends on concern values

#### ❌ ANTI-PATTERN: Concerns Triggering State Changes

```typescript
// ❌ DON'T DO THIS
effect(() => {
  const strike = state.products['leg-1'].strike
  const isValid = validateStrike(strike)

  // ❌ Concern triggering state change
  if (!isValid) {
    state.products['leg-1'].strike = 0  // ❌ Side effect!
  }
})
```

**Why this is bad**:
- Violates one-way data flow
- Creates action-at-a-distance (hard to debug)
- Can cause infinite loops
- Concerns should describe state, not change it

#### ✅ CORRECT: Concerns Describe, Actions Change

```typescript
// ✅ Concern describes state
effect(() => {
  const strike = state.products['leg-1'].strike
  const isValid = validateStrike(strike)
  concernCache.set(key, isValid)  // Just describe, don't change
})

// ✅ Separate action to fix invalid state
const fixInvalidStrike = (path: string) => {
  const concerns = getFieldConcerns(path)
  if (concerns.validationState?.isError) {
    store.proxy.products['leg-1'].strike = 0  // Explicit action
  }
}

// User explicitly calls action
<button onClick={() => fixInvalidStrike('...')}>Reset Invalid</button>
```

---

## ✅ Correct Architecture

### 📦 Data Structures

```typescript
// ✅ Proxy state (single source of truth)
const dataProxy = proxy<AppState>({
  products: {
    'leg-1': { strike: 100, status: 'active' }
  },
  market: { spot: 102 }
  // NO concerns here!
})

// ✅ Concern cache (outside proxy, not reactive)
const concernCache = new Map<string, any>()
//    ^^^ Plain Map, not proxy!

// ✅ Registry tracks effect disposers
const concernsRegistry = new Map<string, ConcernRegistration[]>()

type ConcernRegistration = {
  id: string
  path: string
  concernName: string
  dispose: () => void  // Cleanup function from effect()
}
```

### 🔧 Registration (Setup Effects)

```typescript
const useConcerns = (id: string, registration: Record<string, any>) => {
  const disposeCallbacks: Array<() => void> = []

  Object.entries(registration).forEach(([path, concerns]) => {
    Object.entries(concerns).forEach(([concernName, config]) => {
      const concern = findConcern(concernName)
      const concernKey = `${id}:${path}:${concernName}`

      // ✅ effect() reads from proxy, writes to cache
      const dispose = effect(() => {
        // READ from proxy (triggers tracking)
        const value = getDeepValue(dataProxy, path)

        // EVALUATE (pure function)
        const result = concern.evaluate({
          state: dataProxy,  // Pass proxy for reading
          path,
          value,
          ...config
        })

        // WRITE to cache (doesn't trigger effects)
        concernCache.set(concernKey, result)
      })

      disposeCallbacks.push(dispose)
    })
  })

  // Return cleanup function
  return () => disposeCallbacks.forEach(d => d())
}
```

### ⚛️ Reading Concerns (React)

> **Note:** The early design used a standalone `useFieldConcerns` hook. The final API uses `withConcerns(selection)` which returns a `useFieldStore` with selected concern values merged in.

```typescript
// ✅ withConcerns selects which concern results to include with field value
const { useFieldStore } = store.withConcerns({ disabled: true, validationState: true })

// Usage in React
const MyComponent = () => {
  const { value, setValue, disabled, validationState } = useFieldStore('products.leg-1.strike')

  return (
    <input
      value={value}                     // ✅ Read state
      disabled={disabled}               // ✅ Read concern
      onChange={e => setValue(Number(e.target.value))}
    />
  )
}
```

---

## ⚠️ Potential Pitfall: React Re-renders for Concerns

### 🚨 Problem: How does React know when concerns change?

```typescript
// ❌ This won't trigger re-renders
const concernCache = new Map<string, any>()  // Plain Map, not reactive

effect(() => {
  const result = evaluate(...)
  concernCache.set(key, result)  // React doesn't know this changed!
})
```

### ✅ Solution 1: Separate Reactive Concerns Proxy (RECOMMENDED)

```typescript
// ✅ Separate proxy just for concerns (not part of data state)
const concernsProxy = proxy<Record<string, any>>({})

effect(() => {
  const result = evaluate(...)

  // Writing to proxy triggers React re-renders
  concernsProxy[concernKey] = result
})

// Final API: withConcerns returns useFieldStore with concerns merged in
const { useFieldStore } = store.withConcerns({ validationState: true, disabled: true })
const field = useFieldStore('products.leg-1.strike')
// field.value, field.setValue, field.validationState, field.disabled
```

**Key insight**: We need TWO separate proxies:
1. **dataProxy**: Application state (products, market, etc.)
2. **concernsProxy**: Computed concerns (validations, tooltips, etc.)

**Why separate?**
- ✅ Prevents infinite loops (effects read from dataProxy, write to concernsProxy)
- ✅ React can subscribe to both independently
- ✅ Clear separation of concerns (state vs derived)

### 🤔 Solution 2: Derive Concerns Proxy from Data Proxy (COMPLEX)

```typescript
import { derive } from 'valtio/utils'

// ✅ Concerns proxy derived from data proxy
const concernsProxy = derive({
  'products.leg-1.strike:validationState': (get) => {
    const strike = get(dataProxy).products['leg-1'].strike
    return schema.safeParse(strike).success
  },
  'products.leg-1.strike:disabled': (get) => {
    const status = get(dataProxy).products['leg-1'].status
    return status === 'locked'
  }
  // ... more concerns
})

// Reading concerns via withConcerns (final API)
const { useFieldStore } = store.withConcerns({ validationState: true, disabled: true })
const field = useFieldStore('products.leg-1.strike')
```

**Wait, this brings back derive() issues!**

Actually, with derive(), each concern is a separate derived property, so:
- ✅ Each concern tracks its own dependencies (fine-grained)
- ✅ No infinite loops (derive is read-only)
- ✅ React integration built-in

But we'd need to dynamically create derived properties, which is complex.

### 🎯 Recommended: Solution 1 (Two Proxies)

```typescript
// ✅ Final architecture
const dataProxy = proxy<AppState>({ ... })      // Application state
const concernsProxy = proxy<ConcernsState>({})  // Computed concerns

effect(() => {
  // Read from dataProxy (tracked)
  const strike = dataProxy.products['leg-1'].strike

  // Evaluate (pure)
  const isValid = schema.safeParse(strike).success

  // Write to concernsProxy (triggers React, not this effect)
  concernsProxy['products.leg-1.strike:validationState'] = isValid
})

// React
const MyComponent = () => {
  const data = useSnapshot(dataProxy)        // Subscribe to state
  const concerns = useSnapshot(concernsProxy) // Subscribe to concerns

  return <input value={data.products['leg-1'].strike} />
}
```

---

## 📝 Summary: Critical Rules

### ✅ DO

1. **Store concerns in separate proxy** (not in data proxy)
2. **Effects read from data proxy** (triggers tracking)
3. **Effects write to concerns proxy** (triggers React)
4. **One-way data flow**: Data → Concerns → UI
5. **Concerns are terminal**: Nothing depends on concern values
6. **Concerns are pure functions**: No side effects, no state changes

### ❌ DON'T

1. **Don't store concerns in data proxy** (infinite loops!)
2. **Don't write to data proxy from effects** (side effects!)
3. **Don't let concerns trigger state changes** (violates one-way flow)
4. **Don't make async effects without careful handling** (race conditions)
5. **Don't subscribe to concerns proxy in effects** (circular dependency)

---

## 📊 Updated Architecture Diagram

```
┌─────────────────────────────────────────────────────────────┐
│                     DATA PROXY STATE                        │
│  (Application state - single source of truth)               │
│                                                             │
│  const dataProxy = proxy({                                  │
│    products: { ... },                                       │
│    market: { ... }                                          │
│  })                                                         │
└──────────────┬──────────────────────────────────────────────┘
               │
               │ effect() reads (tracked)
               │
               ▼
┌─────────────────────────────────────────────────────────────┐
│                    CONCERN EFFECTS                          │
│  (Pure computations, no side effects)                       │
│                                                             │
│  effect(() => {                                             │
│    const strike = dataProxy.products['leg-1'].strike        │
│    const isValid = validate(strike)                         │
│    concernsProxy[key] = isValid  ◄── Write to concerns     │
│  })                                                         │
└──────────────┬──────────────────────────────────────────────┘
               │
               │ Writes to
               │
               ▼
┌─────────────────────────────────────────────────────────────┐
│                   CONCERNS PROXY STATE                      │
│  (Derived/computed values - read-only for UI)               │
│                                                             │
│  const concernsProxy = proxy({                              │
│    'products.leg-1.strike:validationState': true,             │
│    'products.leg-1.strike:disabled': false,                 │
│    ...                                                      │
│  })                                                         │
└──────────────┬──────────────────────────────────────────────┘
               │
               │ useSnapshot() (React subscribes)
               │
               ▼
┌─────────────────────────────────────────────────────────────┐
│                         REACT UI                            │
│                                                             │
│  const data = useSnapshot(dataProxy)                        │
│  const concerns = useSnapshot(concernsProxy)                │
│                                                             │
│  return <input                                              │
│    value={data.products['leg-1'].strike}                    │
│    disabled={concerns['....:disabled']}                     │
│    onChange={e => dataProxy.products['leg-1'].strike = ...} │
│  />                                                         │
└─────────────────────────────────────────────────────────────┘
```

**Two separate proxies prevent infinite loops while maintaining reactivity!**
