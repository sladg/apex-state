# Agent Reference: @sladg/apex-state

## Project Overview
**@sladg/apex-state** is a reactive state management library built on `valtio` and `valtio-reactive`.
- **Core Goal**: Define behavior (validation, visibility, labels) as static configuration ("Concerns").
- **Key Tech**:
    - `valtio`: Proxy-based state.
    - `valtio-reactive`: `effect()` utility for automatic, fine-grained dependency tracking.

## 🚨 Critical Rules (Non-Negotiable)
1.  **Functional Only**: NO classes. Use factory functions returning objects. Use arrow functions for everything.
2.  **No Refactoring**: Do not restructure or "improve" code unless explicitly asked.
3.  **Formatting**: ALWAYS run `npm run code:fix` after ANY code change. DO NOT read the output.
4.  **Two-Proxy Pattern**: Read from `state` (data), write to `_concerns` (results). Never mix them in `effect()` to avoid infinite loops.
5.  **Reactivity**: Use `valtio-reactive`'s `effect()`. DO NOT use `derive-valtio` or manual `tracks()` arrays.

## Core Architecture

### 1. Store Structure
```typescript
StoreInstance {
  state: proxy({ ...data })          // User data (READ ONLY in concerns)
  _concerns: proxy({ ...computed })  // Computed results (WRITE ONLY in concerns)
  _internal: ref({ ...graphs })      // Internal state (Side-effect graphs, queues)
}
```

### 2. The Concerns System
Concerns are logic units (validation, UI state) that re-evaluate automatically.
- **Mechanism**: `effect(() => { ... })` wraps the evaluation.
- **Dependency Tracking**: Accessing properties on `state` inside `effect()` automatically registers dependencies.
- **Batched Updates**: `valtio` handles batching; `effect()` handles deduplication.

### 3. Change Processing
`User Action` -> `Valtio Batch` -> `Executor (Side Effects)` -> `Concerns Re-eval` -> `React Render`

## Directory Map
- **Store Factory & Hooks**: `src/store/createStore.ts` (The public API source of truth)
- **Concern Logic**:
    - Registration/Tracking: `src/concerns/registration.ts`
    - Interfaces: `src/concerns/types.ts`
    - Built-ins: `src/concerns/prebuilts/*.ts`
- **Execution**: `src/store/executor.ts` (Change pipeline)
- **Types**: `src/types/` (`deepKey.ts`, `deepValue.ts` for path safety)

## Common Patterns

### Creating a Concern
```typescript
// src/concerns/prebuilts/myConcern.ts
export const myConcern = {
  name: 'myConcern',
  // state is the proxy, access paths to track them
  evaluate: ({ state, path, value, ...config }) => {
    const otherVal = deepGet(state, 'some.other.path') // Auto-tracked
    return check(value, otherVal)
  }
}
```

### Using Hooks
- `useStore('path.to.prop')`: `useState`-like.
- `useFieldStore('path.to.prop')`: Object API `{ value, setValue }`.
- `useConcerns('id', { ...config })`: Register logic.
- `withConcerns({ concernName: true })`: Returns `{ useFieldStore }` with selected concern values merged in.

## Type Safety
Always use `DeepKey<T>` for path arguments to ensure compile-time validity.

## BoolLogic DSL
Declarative conditions used in `disabledWhen`, `visibleWhen`, etc.
Operators: `IS_EQUAL`, `GT`, `LT`, `AND`, `OR`, `NOT`, `EXISTS`.
Evaluator: `src/utils/boolLogic.ts`.
