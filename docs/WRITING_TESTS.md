---
created: 2026-01-29 (2d06fb2)
updated: 2026-01-29 (2d06fb2)
status: active
---

# Writing Tests for Apex State

A practical guide for writing tests using the real store implementation.

## Quick Start

### 1. Basic Test Setup

```typescript
import { describe, expect, it } from 'vitest'
import { z } from 'zod'

import { createGenericStore } from '../src'
import { flushSync, renderWithStore } from '../utils/react'

interface AppState {
  email: string
  password: string
}

describe('My Feature', () => {
  const createAppStore = () => createGenericStore<AppState>()

  it('should validate email', async () => {
    const store = createAppStore()

    const { storeInstance } = renderWithStore(
      <div data-testid="app">Test</div>,
      store,
      { email: '', password: '' },
      {
        concerns: {
          email: {
            validationState: { schema: z.string().email() }
          }
        }
      }
    )

    await flushSync()

    // Update state
    storeInstance.state.email = 'invalid'
    await flushSync()

    // Check concern result
    const emailConcerns = storeInstance._concerns['email']
    expect(emailConcerns?.['validationState']?.isError).toBe(true)
  })
})
```

## Core Patterns

### Pattern 1: State Updates

```typescript
const { storeInstance } = renderWithStore(
  <div>Test</div>,
  store,
  initialState,
  { concerns: { /* ... */ } }
)

await flushSync()

// Direct state mutation
storeInstance.state.field = 'new value'
storeInstance.state.nested.deep.path = 123

await flushSync()  // Wait for effects to process
```

### Pattern 2: Reading Concerns

```typescript
// Read concern values
const concerns = storeInstance._concerns['field']
expect(concerns?.['validationState']).toBeDefined()
expect(concerns?.['validationState']?.isError).toBe(false)

// Check specific concern properties
const validation = concerns?.['validationState']
expect(validation?.errors).toHaveLength(0)

// Dynamic concerns (tooltip, label, etc.)
const tooltip = concerns?.['dynamicTooltip']
expect(tooltip).toBe('Expected tooltip text')
```

### Pattern 3: Multiple Concerns per Field

```typescript
const { storeInstance } = renderWithStore(
  <div>Test</div>,
  store,
  { price: 100, status: 'active' },
  {
    concerns: {
      price: {
        validationState: { schema: z.number().min(0) },
        dynamicTooltip: { template: 'Price: ${{price}}' },
        disabledWhen: { condition: { IS_EQUAL: ['status', 'locked'] } }
      }
    }
  }
)

await flushSync()

// Access multiple concerns
const priceConcerns = storeInstance._concerns['price']
expect(priceConcerns?.['validationState']).toBeDefined()
expect(priceConcerns?.['dynamicTooltip']).toBe('Price: $100')
expect(priceConcerns?.['disabledWhen']).toBe(false)
```

### Pattern 4: Cross-Field Dependencies

```typescript
const { storeInstance } = renderWithStore(
  <div>Test</div>,
  store,
  { field1: 'value', field2: 'locked' },
  {
    concerns: {
      field1: {
        // Depends on field2's value
        disabledWhen: { condition: { IS_EQUAL: ['field2', 'locked'] } },
        dynamicTooltip: { template: 'Field1: {{field1}}, Field2: {{field2}}' }
      }
    }
  }
)

await flushSync()

// Verify cross-field dependency
expect(storeInstance._concerns['field1']?.['disabledWhen']).toBe(true)

// Update dependency field
storeInstance.state.field2 = 'unlocked'
await flushSync()

// Verify concern updated
expect(storeInstance._concerns['field1']?.['disabledWhen']).toBe(false)
```

## Available Prebuilt Concerns

### validationState

Validates field value with Zod schema.

```typescript
concerns: {
  email: {
    validationState: {
      schema: z.string().email('Invalid email')
    }
  }
}

// Result type:
interface ValidationStateResult {
  isError: boolean
  errors: Array<{
    field?: string
    message: string
  }>
  message?: string
  timestamp: number
}
```

### dynamicTooltip

Template-based tooltip with state interpolation.

```typescript
concerns: {
  price: {
    dynamicTooltip: {
      template: 'Price: ${{price}} (Status: {{status}})'
    }
  }
}

// Result type: string
```

### dynamicLabel

Template-based label text.

```typescript
concerns: {
  field: {
    dynamicLabel: {
      template: '{{firstName}} {{lastName}}'
    }
  }
}

// Result type: string
```

### dynamicPlaceholder

Template-based placeholder text.

```typescript
concerns: {
  field: {
    dynamicPlaceholder: {
      template: 'Enter {{fieldType}}...'
    }
  }
}

// Result type: string
```

### disabledWhen

Conditionally disable field based on state.

```typescript
concerns: {
  field: {
    disabledWhen: {
      condition: { IS_EQUAL: ['status', 'locked'] }
    }
  }
}

// Result type: boolean
```

### readonlyWhen

Conditionally make field readonly.

```typescript
concerns: {
  field: {
    readonlyWhen: {
      condition: { IS_EQUAL: ['mode', 'view'] }
    }
  }
}

// Result type: boolean
```

### visibleWhen

Conditionally show/hide field.

```typescript
concerns: {
  field: {
    visibleWhen: {
      condition: { IS_EQUAL: ['showAdvanced', true] }
    }
  }
}

// Result type: boolean
```

## BoolLogic Conditions

All conditional concerns (`disabledWhen`, `readonlyWhen`, `visibleWhen`) support BoolLogic:

```typescript
// Simple equality
{ IS_EQUAL: ['field', 'value'] }

// Logical operators
{ AND: [
  { IS_EQUAL: ['status', 'active'] },
  { IS_EQUAL: ['type', 'premium'] }
]}

{ OR: [
  { IS_EQUAL: ['status', 'locked'] },
  { IS_EQUAL: ['status', 'archived'] }
]}

{ NOT: { IS_EQUAL: ['enabled', true] } }

// Comparisons
{ GT: ['price', 100] }
{ GTE: ['price', 100] }
{ LT: ['price', 1000] }
{ LTE: ['price', 1000] }

// Type checks
{ IS_NIL: ['field'] }
{ IS_EMPTY: ['field'] }
{ IS_NUMBER: ['field'] }

// String operations
{ INCLUDES: ['tags', 'important'] }
```

## Common Test Scenarios

### Testing Validation

```typescript
it('validates required fields', async () => {
  const { storeInstance } = renderWithStore(
    <div>Test</div>,
    store,
    { email: '', password: '' },
    {
      concerns: {
        email: {
          validationState: { schema: z.string().min(1, 'Required') }
        },
        password: {
          validationState: { schema: z.string().min(8, 'Too short') }
        }
      }
    }
  )

  await flushSync()

  // Both should error on empty
  expect(storeInstance._concerns['email']?.['validationState']?.isError).toBe(true)
  expect(storeInstance._concerns['password']?.['validationState']?.isError).toBe(true)

  // Fill valid values
  storeInstance.state.email = 'test@example.com'
  storeInstance.state.password = 'SecurePass123'
  await flushSync()

  // Both should pass
  expect(storeInstance._concerns['email']?.['validationState']?.isError).toBe(false)
  expect(storeInstance._concerns['password']?.['validationState']?.isError).toBe(false)
})
```

### Testing Dynamic Content

```typescript
it('updates dynamic tooltip when dependencies change', async () => {
  const { storeInstance } = renderWithStore(
    <div>Test</div>,
    store,
    { firstName: 'John', lastName: 'Doe' },
    {
      concerns: {
        firstName: {
          dynamicTooltip: {
            template: 'Name: {{firstName}} {{lastName}}'
          }
        }
      }
    }
  )

  await flushSync()

  expect(storeInstance._concerns['firstName']?.['dynamicTooltip']).toBe('Name: John Doe')

  storeInstance.state.lastName = 'Smith'
  await flushSync()

  expect(storeInstance._concerns['firstName']?.['dynamicTooltip']).toBe('Name: John Smith')
})
```

### Testing Conditional Logic

```typescript
it('disables field based on status', async () => {
  const { storeInstance } = renderWithStore(
    <div>Test</div>,
    store,
    { price: 100, status: 'active' },
    {
      concerns: {
        price: {
          disabledWhen: { condition: { IS_EQUAL: ['status', 'locked'] } }
        }
      }
    }
  )

  await flushSync()

  // Should be enabled (not locked)
  expect(storeInstance._concerns['price']?.['disabledWhen']).toBe(false)

  // Lock it
  storeInstance.state.status = 'locked'
  await flushSync()

  // Should be disabled
  expect(storeInstance._concerns['price']?.['disabledWhen']).toBe(true)
})
```

### Testing Performance

```typescript
it('propagates changes within performance budget', async () => {
  const { storeInstance } = renderWithStore(
    <div>Test</div>,
    store,
    { field: 'initial' },
    {
      concerns: {
        field: {
          validationState: { schema: z.string() },
          dynamicTooltip: { template: 'Value: {{field}}' }
        }
      }
    }
  )

  await flushSync()

  const start = performance.now()

  storeInstance.state.field = 'updated'
  await flushSync()

  const duration = performance.now() - start

  // Should propagate quickly (includes React overhead)
  expect(duration).toBeLessThan(25)

  // Verify value updated
  expect(storeInstance._concerns['field']?.['dynamicTooltip']).toBe('Value: updated')
})
```

## Tips & Best Practices

### 1. Always await flushSync()

```typescript
// ❌ Bad - concern values may not be updated yet
storeInstance.state.field = 'value'
expect(storeInstance._concerns['field']?.['validationState']).toBeDefined()

// ✅ Good - wait for effects to process
storeInstance.state.field = 'value'
await flushSync()
expect(storeInstance._concerns['field']?.['validationState']).toBeDefined()
```

### 2. Use Index Signature Access for Concerns

```typescript
// ❌ Bad - TypeScript error
const validation = storeInstance._concerns['field'].validationState

// ✅ Good - bracket notation
const validation = storeInstance._concerns['field']?.['validationState']
```

### 3. Check for Undefined

```typescript
// Concerns may not exist if not registered
const concerns = storeInstance._concerns['field']
const validation = concerns?.['validationState']

if (validation) {
  expect(validation.isError).toBe(false)
}
```

### 4. Test Type-Safe Paths

```typescript
interface AppState {
  user: {
    profile: {
      name: string
    }
  }
}

// ✅ TypeScript validates paths
storeInstance.state.user.profile.name = 'John'

// Use in concerns
concerns: {
  'user.profile.name': {
    validationState: { schema: z.string().min(1) }
  }
}
```

### 5. Structure Tests Clearly

```typescript
it('should validate email format', async () => {
  // Setup
  const { storeInstance } = renderWithStore(/*...*/)
  await flushSync()

  // Action
  storeInstance.state.email = 'invalid-email'
  await flushSync()

  // Assert
  const emailConcerns = storeInstance._concerns['email']
  expect(emailConcerns?.['validationState']?.isError).toBe(true)
})
```

## Migration from Old Pattern

### Before (Custom Test Utils)

```typescript
import { createTestStore } from './test-utils'

const store = createTestStoreWithConcerns(initialData)
store.useConcerns('test', {...})
store.proxy.field = value
const result = store.getFieldConcerns('field').concern
```

### After (Real Implementation)

```typescript
import { createGenericStore } from '../src'
import { renderWithStore, flushSync } from '../utils/react'

const store = createGenericStore<AppState>()

const { storeInstance } = renderWithStore(
  <div>Test</div>,
  store,
  initialData,
  {
    concerns: {...}
  }
)

await flushSync()

storeInstance.state.field = value
await flushSync()

const result = storeInstance._concerns['field']?.['concern']
```

## Next Steps

- See [REFACTORING_EXAMPLE.md](../REFACTORING_EXAMPLE.md) for detailed migration guide
- Check [tests/concerns/batch-updates-refactored-v2.test.tsx](../tests/concerns/batch-updates-refactored-v2.test.tsx) for complete examples
- Read [CONCERNS_GUIDE.md](./agents/CONCERNS_GUIDE.md) for concern system details
