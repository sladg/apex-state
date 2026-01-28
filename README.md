# @sladg/apex-state

Advanced state management wrapper around Valtio with powerful side effects system.

## Features

- **Valtio-powered**: Built on Valtio's efficient proxy-based reactivity
- **Sync Paths**: Bidirectional field synchronization with cycle detection
- **Flip Paths**: Boolean/enum flipping for toggle patterns
- **Listeners**: Global and scoped state change listeners
- **Validators**: Zod-based validation with error storage
- **Aggregations**: Computed state across multiple sources
- **Clear Paths**: Automatic field clearing on triggers
- **Form Hooks**: Convenient form field management with transformations
- **TypeScript**: Full type safety with deep path inference
- **Functional**: Factory functions, no classes

## Installation

```bash
npm install @sladg/apex-state
```

### Peer Dependencies

```bash
npm install react zod valtio graphology
```

## Quick Start

```typescript
import { createGenericStore } from '@sladg/apex-state'
import { z } from 'zod'

// Define your state type
type AppState = {
  email: string
  password: string
  confirmPassword: string
  _errors?: Record<string, Array<{ id: string; message: string }>>
}

// Create a store
const store = createGenericStore<AppState>()

// Use in React component
function RegistrationForm() {
  // Register side effects
  store.useSideEffects('registration', {
    // Sync password fields
    syncPaths: {
      pairs: [
        { id: 'pw-sync', path1: 'password', path2: 'confirmPassword' }
      ]
    },

    // Validate email
    validators: {
      validators: [{
        id: 'email-validation',
        scope: 'email',
        schema: z.string().email(),
        errorPath: 'email'
      }]
    }
  })

  const email = store.useFieldStore('email')
  const password = store.useFieldStore('password')
  const emailErrors = store.useErrors('email')

  return (
    <form>
      <input
        value={email.value}
        onChange={e => email.setValue(e.target.value)}
      />
      {emailErrors.map(err => <span key={err}>{err}</span>)}

      <input
        type="password"
        value={password.value}
        onChange={e => password.setValue(e.target.value)}
      />

      <button type="submit">Register</button>
    </form>
  )
}

// Wrap your app with Provider
function App() {
  return (
    <store.Provider initialState={{
      email: '',
      password: '',
      confirmPassword: ''
    }}>
      <RegistrationForm />
    </store.Provider>
  )
}
```

## Core Concepts

### Store Creation

```typescript
const store = createGenericStore<StateType>({
  errorStorePath: '_errors' // optional, default is '_errors'
})
```

### Hooks

- **`useStore(path)`**: Get/set value at path (like useState)
- **`useFieldStore(path)`**: Object API for form fields `{value, setValue}`
- **`useFieldTransformedStore(path, config)`**: Bidirectional transformations
- **`useJitStore()`**: Bulk operations and non-reactive reads
- **`useSideEffects(id, effects)`**: Register side effects
- **`useErrors(errorPath)`**: Get validation errors

### Side Effects

All side effects are registered via `useSideEffects`:

```typescript
store.useSideEffects('my-effects', {
  // Bidirectional sync
  syncPaths: {
    pairs: [
      { id: 'sync-1', path1: 'field1', path2: 'field2' }
    ]
  },

  // Boolean/enum flipping
  flipPaths: {
    pairs: [
      { id: 'flip-1', path1: 'showA', path2: 'showB', bidirectional: true }
    ]
  },

  // State change listeners
  listeners: {
    listeners: [
      { key: 'user', fn: (changes, state) => console.log('User changed') }
    ]
  },

  // Zod validation
  validators: {
    validators: [{
      id: 'validator-1',
      scope: 'email',
      schema: z.string().email(),
      errorPath: 'emailError'
    }]
  },

  // Multi-source aggregation
  aggregations: {
    rules: [{
      id: 'agg-1',
      targetPath: 'allChecked',
      sourcePaths: ['check1', 'check2', 'check3']
    }]
  },

  // Clear on trigger
  clearPaths: {
    rules: [{
      id: 'clear-1',
      triggerPath: 'currentStep',
      clearPaths: ['field1', 'field2']
    }]
  }
})
```

## Type Safety

Full TypeScript support with deep path inference:

```typescript
type State = {
  user: {
    profile: {
      name: string
    }
  }
}

const [name, setName] = store.useStore('user.profile.name')
// name: string (inferred!)
// setName: (value: string) => void
```

## Architecture

- **Factory Functions**: No classes, pure functional approach
- **Valtio Proxies**: Efficient reactivity with minimal re-renders
- **Graph-based**: Uses graphology for efficient relationship tracking
- **Pipeline**: Synchronizer pipeline processes changes with side effects
- **Immutable Updates**: All changes go through pipeline for consistency

### Tech Stack

- **Valtio** for reactive state management
- **Graphology** for dependency graph operations
- **Remeda** for functional utilities and pipe operations
- **Zod** for validation schemas
- **TypeScript** for type safety

## Performance

- Optimized synchronizers with early exits
- Efficient graph lookups using graphology
- Minimal re-renders via valtio's proxy tracking
- Batch updates supported via `useJitStore`

## Development

```bash
# Install dependencies
npm install

# Type check
npm run type-check

# Build
npm run build

# Test
npm test
npm run test:watch
```

## Testing

443+ tests covering all features with Vitest and React Testing Library.

```bash
npm test
```

## Status

✅ Complete - All 8 phases implemented
- 443 tests passing
- Full TypeScript support
- Comprehensive side effects system
- Production ready

## License

MIT

## Contributing

See contribution guidelines in the repository.
