# @sladg/apex-state

Advanced state management wrapper around Valtio with powerful side effects system.

## Features

- **Valtio-powered**: Built on Valtio's efficient proxy-based reactivity
- **Concerns System**: Reactive validation and UI hints with automatic dependency tracking
- **Pre-built Concerns**: zodValidation, disabledWhen, visibleWhen, dynamicTooltip, and more
- **BoolLogic DSL**: Declarative conditions for dynamic field behavior
- **String Interpolation**: Template-based dynamic text with `{{path}}` syntax
- **Side Effects**: Sync paths, flip paths, listeners, validators, aggregations, clear paths
- **Form Hooks**: Convenient form field management with transformations
- **TypeScript**: Full type safety with deep path inference
- **Functional**: Factory functions, no classes
- **Production Ready**: 227+ passing tests with comprehensive coverage

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
import { createGenericStore, prebuilts } from '@sladg/apex-state'
import { z } from 'zod'

// Define your state type
type AppState = {
  email: string
  password: string
  status: 'editing' | 'locked'
}

// Create a store
const store = createGenericStore<AppState>()

// Use in React component
function RegistrationForm() {
  // Register concerns for reactive validation and UI hints
  store.useConcerns('registration', {
    email: {
      zodValidation: { schema: z.string().email() },
      visibleWhen: { condition: { IS_EQUAL: ['status', 'editing'] } }
    },
    password: {
      zodValidation: { schema: z.string().min(8) },
      disabledWhen: { condition: { IS_EQUAL: ['status', 'locked'] } },
      dynamicTooltip: { template: 'Min 8 chars, status: {{status}}' }
    }
  })

  const email = store.useFieldStore('email')
  const emailConcerns = store.useFieldConcerns('email')

  const password = store.useFieldStore('password')
  const passwordConcerns = store.useFieldConcerns('password')

  return (
    <form>
      {emailConcerns.visibleWhen && (
        <input
          value={email.value}
          onChange={e => email.setValue(e.target.value)}
          className={emailConcerns.zodValidation ? 'valid' : 'invalid'}
        />
      )}

      <input
        type="password"
        value={password.value}
        onChange={e => password.setValue(e.target.value)}
        disabled={passwordConcerns.disabledWhen}
        title={passwordConcerns.dynamicTooltip}
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
      status: 'editing'
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
  errorStorePath: '_errors', // optional, default is '_errors'
  maxIterations: 100 // optional, max side-effect iterations
})
```

### Hooks

- **`useStore(path)`**: Get/set value at path (like useState)
- **`useFieldStore(path)`**: Object API for form fields `{value, setValue}`
- **`useFieldTransformedStore(path, config)`**: Bidirectional transformations
- **`useJitStore()`**: Bulk operations and non-reactive reads
- **`useConcerns(id, config)`**: Register reactive concerns at paths
- **`useFieldConcerns(path)`**: Get evaluated concerns for a path
- **`useSideEffects(id, effects)`**: Register side effects (legacy)

### Concerns System

Concerns provide reactive validation, conditional logic, and UI hints that automatically track dependencies:

```typescript
store.useConcerns('my-concerns', {
  'products.leg1.strike': {
    // Validation
    zodValidation: {
      schema: z.number().min(0).max(200)
    },

    // Conditional visibility
    visibleWhen: {
      condition: { IS_EQUAL: ['products.leg1.enabled', true] }
    },

    // Conditional disable
    disabledWhen: {
      condition: {
        OR: [
          { IS_EQUAL: ['status', 'locked'] },
          { GT: ['market.spot', 110] }
        ]
      }
    },

    // Dynamic text
    dynamicTooltip: {
      template: 'Strike: {{market.spot}}, Status: {{status}}'
    },

    dynamicLabel: {
      template: 'Strike (Spot: {{market.spot}})'
    }
  }
})

// Access evaluated concerns
const concerns = store.useFieldConcerns('products.leg1.strike')
// concerns = { zodValidation: true, visibleWhen: true, disabledWhen: false, dynamicTooltip: '...', ... }
```

### Pre-built Concerns

Import from `prebuilts`:
- **zodValidation**: Zod schema validation
- **disabledWhen**: Conditional disable via BoolLogic
- **readonlyWhen**: Conditional readonly via BoolLogic
- **visibleWhen**: Conditional visibility via BoolLogic
- **dynamicLabel**: String interpolation for labels
- **dynamicPlaceholder**: String interpolation for placeholders
- **dynamicTooltip**: String interpolation for tooltips
- **validationState**: Full validation state with errors

### BoolLogic DSL

Declarative conditions for dynamic behavior:

```typescript
type BoolLogic<STATE> =
  | { IS_EQUAL: [path, value] }
  | { EXISTS: path }
  | { IS_EMPTY: path }
  | { AND: BoolLogic[] }
  | { OR: BoolLogic[] }
  | { NOT: BoolLogic }
  | { GT: [path, number] }
  | { LT: [path, number] }
  | { IN: [path, values[]] }
```

### Side Effects (Legacy)

Side effects for advanced patterns (being superseded by concerns):

```typescript
store.useSideEffects('my-effects', {
  syncPaths: { pairs: [{ id: 'sync-1', path1: 'field1', path2: 'field2' }] },
  flipPaths: { pairs: [{ id: 'flip-1', path1: 'showA', path2: 'showB' }] },
  listeners: { listeners: [{ key: 'user', fn: (changes, state) => {} }] },
  validators: { validators: [{ id: 'val-1', scope: 'email', schema: z.string().email() }] },
  aggregations: { rules: [{ id: 'agg-1', targetPath: 'all', sourcePaths: ['a', 'b'] }] },
  clearPaths: { rules: [{ id: 'clr-1', triggerPath: 'step', clearPaths: ['f1'] }] }
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
- **Two-Proxy Pattern**: Separate `state` and `_concerns` proxies for optimal reactivity
- **Automatic Dependency Tracking**: Concerns track dependencies via explicit `tracks()` declarations
- **Batch Processing**: Valtio batches changes, we deduplicate and evaluate concerns once per batch
- **Graph-based**: Uses graphology for side-effect relationship tracking
- **Pipeline**: Synchronizer pipeline processes changes with side effects
- **Immutable Updates**: All changes go through pipeline for consistency

### Tech Stack

- **Valtio** for reactive state management (v2.3+)
- **valtio-reactive** for concerns dependency tracking
- **Graphology** for dependency graph operations
- **Remeda** for functional utilities and pipe operations
- **Zod** for validation schemas
- **TypeScript** for type safety

## Performance

- **Smart Re-evaluation**: Concerns deduplicated per batch, each evaluates once
- **Dependency Tracking**: Only affected concerns recalculate on changes
- **Optimized Synchronizers**: Early exits and efficient graph lookups
- **Minimal Re-renders**: Valtio's proxy tracking ensures surgical updates
- **Batch Updates**: Supported via `useJitStore` for bulk operations
- **227+ Tests**: Comprehensive test coverage validates performance optimizations

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

227+ tests covering all features with Vitest and React Testing Library.

```bash
npm test        # Run all tests
npm run test:watch  # Watch mode
npm run type-check  # TypeScript validation
```

## Status

✅ Production Ready
- 227+ tests passing
- Full TypeScript support with deep path inference
- Concerns system with automatic dependency tracking
- Comprehensive side effects system
- Functional architecture (no classes)
- Battle-tested patterns from real-world usage

## Documentation

- **README.md** (this file): Quick start and API overview
- **CONCERNS_REFERENCE.md**: Complete concerns system guide
- **CODE_STYLE.md**: Code conventions and patterns
- **src/index.ts**: Full TypeScript API exports

## License

MIT

## Contributing

See contribution guidelines in the repository.
