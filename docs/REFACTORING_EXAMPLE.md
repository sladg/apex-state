# Test Refactoring Example: Using Real Implementation

This document shows how to refactor tests from using custom `createTestStore` utilities to using the real `createGenericStore` implementation.

## Example: batch-updates.test.ts → batch-updates-refactored.test.tsx

### Before (Custom Test Utilities)

```typescript
// Old approach
import { createTestStore } from './test-utils'

// Custom concern implementations with spies
const createTestStoreWithConcerns = (initialData: AppState) => {
  const validationState: ConcernType = {
    name: 'validationState',
    evaluate: (props) => {
      spies.validationState(props.path)
      // Custom implementation...
    },
  }

  const concerns = { validationState, tooltip }
  return createTestStore(initialData, concerns)
}

// Non-React usage
const store = createTestStoreWithConcerns({...})
store.useConcerns('test', {...})
store.proxy.products['leg-1'].strike = 150
const result = store.getFieldConcerns('products.leg-1.strike')['tooltip']
```

### After (Real Implementation)

```typescript
// New approach
import { createGenericStore } from '../../src'
import { useStoreContext } from '../../src/core/context'
import { flushSync, renderWithStore } from '../utils/react'

const createAppStore = () => createGenericStore<AppState>()

// React-based test
const store = createAppStore()
let storeInstance: StoreInstance<AppState> | null = null

function TestComponent() {
  storeInstance = useStoreContext<AppState>()

  // Use real prebuilt concerns
  store.useConcerns('test', {
    'products.leg-1.strike': {
      validationState: { schema: z.number().min(0) },
      dynamicTooltip: {
        template: 'Strike: {{products.leg-1.strike}} @ {{market.spot}}',
      },
    },
  })

  return <div>Test</div>
}

renderWithStore(<TestComponent />, store, initialState)
await flushSync()

// Access via storeInstance
storeInstance.state.products['leg-1'].strike = 150
await flushSync()

const result = storeInstance._concerns['products.leg-1.strike']?.['dynamicTooltip']
```

## Key Changes

### 1. Import Real Implementation
```typescript
// Before
import { createTestStore } from './test-utils'

// After
import { createGenericStore } from '../../src'
import { useStoreContext } from '../../src/core/context'
import type { StoreInstance } from '../../src/core/types'
```

### 2. Use Prebuilt Concerns
```typescript
// Before: Custom concern implementations
const tooltip: ConcernType = {
  name: 'tooltip',
  evaluate: (props) => {
    // Custom template interpolation
  },
}

// After: Use prebuilt concerns
store.useConcerns('test', {
  'path': {
    dynamicTooltip: { template: '{{path}}' },
    validationState: { schema: z.string() },
  },
})
```

### 3. React-Based Tests
```typescript
// Before: Direct non-React API
const store = createTestStore(initialData, concerns)
store.useConcerns('test', {...})

// After: Component with hooks
function TestComponent() {
  const storeInstance = useStoreContext<AppState>()
  store.useConcerns('test', {...})
  return <div>Test</div>
}

renderWithStore(<TestComponent />, store, initialState)
```

### 4. Access Store Instance via renderWithStore
```typescript
// Before: Direct access
store.proxy.field = value
const concerns = store.getFieldConcerns('field')

// After: Get properly typed storeInstance from renderWithStore
const { storeInstance } = renderWithStore(
  <Component />,
  store,
  initialState,
  {
    concerns: { /* ... */ }
  }
)

// TypeScript knows: storeInstance is StoreInstance<AppState> (NOT undefined!)
// No ! needed - function overloads guarantee it exists when concerns are provided
storeInstance.state.field = value
const concerns = storeInstance._concerns['field']
```

### 5. Use flushSync for Effects
```typescript
// Before: Synchronous (fake implementation)
store.proxy.field = value
expect(store.getFieldConcerns('field').concern).toBe(expected)

// After: Wait for effects
storeInstance.state.field = value
await flushSync()
expect(storeInstance._concerns['field']?.['concern']).toBe(expected)
```

## Concern Name Mappings

| Old Test Name | Real Prebuilt Name   |
|---------------|---------------------|
| `tooltip`     | `dynamicTooltip`    |
| `label`       | `dynamicLabel`      |
| `placeholder` | `dynamicPlaceholder`|
| `disabled`    | `disabledWhen`      |
| `readonly`    | `readonlyWhen`      |
| `visible`     | `visibleWhen`       |
| `validationState` | `validationState` |

## Performance Expectations

Note: Tests using real implementation include React rendering overhead:
- **Before**: Pure concern evaluation (< 15ms)
- **After**: Includes React render cycle (< 25ms)

Adjust performance thresholds accordingly.

## Files to Refactor

### Concern Tests (tests/concerns/)
- ✅ `batch-updates.test.ts` → `batch-updates-refactored.test.tsx` (example)
- [ ] `cross-field-deps.test.ts` → convert to React
- [ ] `selective-recalc.test.ts` → convert to React
- [ ] `react-integration.test.tsx` → update imports

### Integration Tests (tests/integration/)
Most already use real implementation via mocks/stores.ts factories.
Just need to verify they don't import from test-utils.

### Utilities to Remove
- [ ] Remove `createTestStore` from `tests/concerns/test-utils.ts`
- [ ] Remove `createTestStore` from `tests/utils/react.ts`
- [ ] Keep other utilities (PerformanceBenchmark, createRenderTracker, etc.)

## Next Steps

1. Review this example to confirm the approach
2. Refactor remaining concern tests following this pattern
3. Remove `createTestStore` from test utilities
4. Update any integration tests still using test utilities
5. Run full test suite to verify
