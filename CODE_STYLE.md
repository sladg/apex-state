# @sladg/apex-state Code Style Guide

## Core Principles

### 1. Functional Programming Only
- **NO CLASSES** - Use factory functions with closures
- **Arrow functions everywhere** - All functions must be arrow functions
- **Immutability** - Avoid mutations where possible
- **Pure functions** - Functions should be pure when possible
- **Composition** - Prefer function composition over inheritance

### 2. Factory Pattern
Instead of classes, use factory functions that return objects:

```typescript
// ❌ BAD: Class-based
export class Registry {
  private state = new Map()
  method() { return this.state.get() }
}

// ✅ GOOD: Factory with closure
export const createRegistry = () => {
  const state = new Map()
  return {
    method: () => state.get()
  }
}
```

### 3. Arrow Functions Only

```typescript
// ❌ BAD: Function declaration
export function createStore(config) {
  return { ... }
}

// ✅ GOOD: Arrow function
export const createStore = (config) => {
  return { ... }
}

// ❌ BAD: Named function expression
const helper = function doSomething(x) {
  return x * 2
}

// ✅ GOOD: Arrow function
const helper = (x) => {
  return x * 2
}

// ✅ BETTER: Concise arrow (single expression)
const helper = (x) => x * 2
```

### 4. Type Safety

```typescript
// ✅ Always use explicit types for exported functions
export const createStore = <T extends object>(
  config: StoreConfig
): StoreReturn<T> => {
  // ...
}

// ✅ Use type guards instead of `any`
const isString = (x: unknown): x is string => typeof x === 'string'

// ✅ Prefer `unknown` over `any`
const parse = (data: unknown) => {
  // validate and narrow type
}
```

### 5. Immutable Data Structures

```typescript
// ✅ Use built-in immutable operations
const newArray = [...oldArray, newItem]
const newObject = { ...oldObject, key: newValue }

// ✅ Use Map and Set for collections
const registry = new Map<string, Config>()
const processed = new Set<string>()

// ❌ Avoid mutations
array.push(item) // Bad
object.field = value // Bad
```

### 6. Closures for State Encapsulation

```typescript
// ✅ Use closures to hide internal state
export const createCounter = () => {
  let count = 0 // Private via closure

  return {
    increment: () => count++,
    get: () => count
  }
}

// ❌ Don't expose internal state
export const createCounter = () => {
  return {
    count: 0, // Exposed, can be mutated
    increment: function() { this.count++ }
  }
}
```

### 7. Consistent Patterns

#### Registry Pattern
```typescript
export interface Registry<T> {
  register: (id: string, item: T) => void
  get: (id: string) => T | undefined
  has: (id: string) => boolean
  clear: () => void
}

export const createRegistry = <T>(): Registry<T> => {
  const items = new Map<string, T>()

  return {
    register: (id, item) => items.set(id, item),
    get: (id) => items.get(id),
    has: (id) => items.has(id),
    clear: () => items.clear()
  }
}
```

#### Factory Pattern
```typescript
// Always return an object with methods as arrow functions
export const createThing = <T>(config: Config<T>) => {
  const state = initializeState(config)

  return {
    method1: (arg: string) => {
      // use state via closure
    },
    method2: () => {
      // use state via closure
    }
  }
}
```

### 8. Testing Patterns

```typescript
// ✅ Fast, synchronous tests preferred
test('does something', () => {
  const result = doSomething()
  expect(result).toBe(expected)
})

// ✅ Use async only when necessary
test('updates state', async () => {
  setState(newValue)
  await new Promise(resolve => setTimeout(resolve, 0))
  expect(state).toBe(newValue)
})

// ❌ Avoid waitFor() - use direct assertions
// (waitFor can hide timing issues)
```

### 9. Module Organization

```typescript
// Each file should export one primary thing
// File: createStore.ts
export const createStore = () => { ... }
export type StoreReturn = { ... }

// File: registry.ts
export const createRegistry = () => { ... }
export interface Registry { ... }
```

### 10. Documentation

```typescript
/**
 * Creates a store with reactive state management
 *
 * @example
 * ```typescript
 * const store = createStore<AppState>()
 * ```
 */
export const createStore = <T extends object>() => {
  // ...
}
```

## Naming Conventions

- **Factory functions**: `createX` (e.g., `createStore`, `createRegistry`)
- **Hooks**: `useX` (e.g., `useStore`, `useFieldStore`)
- **Types/Interfaces**: PascalCase (e.g., `StoreConfig`, `Registry`)
- **Constants**: SCREAMING_SNAKE_CASE for truly constant values
- **Variables/Functions**: camelCase
- **Generic type params**: Single letter or descriptive (e.g., `T`, `DATA`, `META`)

## File Naming

- **Types**: `types.ts`, `sideEffects.ts`
- **Implementations**: `registry.ts`, `graph.ts`, `executor.ts`
- **Hooks**: `useStore.ts`, `useFieldStore.ts`
- **Tests**: `*.test.ts`, `*.test.tsx`

## Import Order

1. React imports
2. Third-party library imports
3. Type imports (using `import type`)
4. Local imports (relative)

```typescript
import React from 'react'
import { proxy } from 'valtio'
import type { SideEffects } from '../types'
import { createRegistry } from './registry'
```

## Anti-Patterns to Avoid

❌ Classes
❌ Function declarations
❌ Named function expressions
❌ `this` keyword (except in getter examples)
❌ Mutations of parameters
❌ Global state
❌ `any` type (use `unknown` instead)
❌ Inheritance
❌ `waitFor()` in tests (use direct assertions)

## Preferred Patterns

✅ Factory functions with closures
✅ Arrow functions everywhere
✅ Immutable operations
✅ Type safety with generics
✅ Composition over inheritance
✅ Pure functions
✅ `Map` and `Set` for collections
✅ Direct assertions in tests
✅ `unknown` over `any`
✅ Explicit return types for exports

## Architecture Principles

1. **No Classes**: Everything is factory functions returning objects
2. **Closures for Privacy**: Use closures to encapsulate state
3. **Composition**: Build complex behavior from simple functions
4. **Type Safety**: Leverage TypeScript's type system fully
5. **Functional**: Prefer pure functions and immutability
6. **Explicit**: Make types and return values explicit

This codebase is a showcase of functional programming patterns in TypeScript!
