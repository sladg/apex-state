# Test Mocks Documentation

Centralized test utilities, fixtures, and store factories for integration testing.

## Purpose

The `tests/mocks/` directory eliminates duplication across integration tests by providing:

1. **Store Factories** - Pre-configured typed stores for each test scenario
2. **Fixtures** - Initial state data (empty, partial, complete, with errors)
3. **Helpers** - Validators, generators, DOM queries, test patterns
4. **Types** - TypeScript definitions for all test scenarios

## Quick Start

```typescript
import {
  createRegistrationFormStore,
  registrationFormFixtures,
  validators,
  errorMessages,
  domHelpers,
} from '../mocks'

describe('My Form Test', () => {
  const store = createRegistrationFormStore()

  it('validates email', () => {
    render(
      <store.Provider initialState={registrationFormFixtures.empty}>
        <FormComponent />
      </store.Provider>
    )

    // Use helpers
    expect(validators.email('test@example.com')).toBe(true)
    expect(domHelpers.hasErrors()).toBe(false)
  })
})
```

## File Structure

```
tests/mocks/
├── index.ts       # Central export (import from here)
├── types.ts       # TypeScript types for test scenarios
├── stores.ts      # Store factory functions
├── fixtures.ts    # Initial state data
├── helpers.ts     # Validators, generators, DOM utilities
└── README.md      # This file
```

## When to Use What

### Use `tests/mocks/` for:

- ✅ Integration tests with React components
- ✅ Full store setup with Provider
- ✅ Form validation testing
- ✅ Initial state data
- ✅ Common validators and error messages
- ✅ DOM queries and assertions

### Use `tests/concerns/test-utils.ts` for:

- ✅ Concern performance benchmarking
- ✅ Evaluation tracking (which concerns fired)
- ✅ Concern spy creation
- ✅ Render count tracking

### Use `tests/utils/react.ts` for:

- ✅ Lower-level React testing without full stores
- ✅ Custom test store creation
- ✅ Effect flushing utilities

**Note:** Some overlap exists between `mocks/helpers.ts` and `utils/react.ts`
(assertions, domHelpers). **Prefer mocks for integration tests** as it's the
established pattern.

## Available Store Factories

Import from `tests/mocks`:

| Factory | Purpose | Example Scenario |
|---------|---------|------------------|
| `createRegistrationFormStore` | Basic validation, dependent fields | User signup form |
| `createProfileFormStore` | Sync paths (firstName + lastName → fullName) | Profile editor |
| `createShoppingCartStore` | Aggregations (item subtotals → total) | E-commerce cart |
| `createNestedCartStore` | Deep paths, category subtotals | Multi-category cart |
| `createProductFormStore` | Conditional UI (digital vs physical) | Admin product form |
| `createUserProfileStore` | Side effects, timestamps | User settings |
| `createWizardFormStore` | Multi-step workflows | Checkout wizard |
| `createFormWithErrorsStore` | Error handling patterns | Error display testing |
| `createOptimizationStore` | Render optimization | Performance testing |

Each factory creates a fully-typed store with IDE autocomplete for paths.

## Available Fixtures

Each store factory has corresponding fixtures. Import from `tests/mocks`:

```typescript
import { registrationFormFixtures } from '../mocks'

// Available fixtures per store:
registrationFormFixtures.empty      // All fields empty
registrationFormFixtures.partial    // Some fields filled
registrationFormFixtures.complete   // All fields valid

shoppingCartFixtures.empty          // Empty cart
shoppingCartFixtures.singleItem     // One item
shoppingCartFixtures.multipleItems  // Multiple items

// ... and more (see fixtures.ts)
```

### Fixture Naming Convention

- **empty** - All fields empty/default values
- **partial** - Some fields populated
- **complete / filled** - All required fields valid
- **withErrors** - Pre-populated with validation errors
- **updated** - Modified state for testing updates
- **step[N]** - Multi-step workflow states (e.g., wizardFormFixtures.step1Empty)

## Helper Utilities

### Validators

Common validation functions matching real business logic:

```typescript
import { validators } from '../mocks'

validators.email('test@example.com')  // true
validators.password('StrongPass123')  // true (8+ chars, uppercase, number)
validators.username('john_doe')       // true (3+ chars, alphanumeric + _)
validators.url('https://example.com') // true
```

### Error Messages

Standardized error strings matching validators:

```typescript
import { errorMessages } from '../mocks'

errorMessages.emailInvalid      // 'Please enter a valid email address'
errorMessages.passwordTooShort  // 'Password must be at least 8 characters'
errorMessages.passwordWeak      // 'Password must contain uppercase letter and number'
// ... and more
```

### Generators

Random data generators for test cases:

```typescript
import { generators } from '../mocks'

generators.email()        // 'test-abc123@example.com'
generators.username()     // 'user_xyz789'
generators.password()     // 'Passabc123'
generators.productName()  // 'Premium Widget'
generators.price(10, 100) // Random price between 10-100
```

### DOM Helpers

Query utilities for finding elements and checking errors:

```typescript
import { domHelpers } from '../mocks'

domHelpers.getAllErrors()           // ['Email is required', 'Password too short']
domHelpers.hasErrors()              // true
domHelpers.getErrorCount()          // 2
domHelpers.getField('email-input')  // HTMLInputElement
domHelpers.getButton('submit-btn')  // HTMLButtonElement
```

### Assertions

DOM state checks that return booleans:

```typescript
import { assertions } from '../mocks'

const input = domHelpers.getField('email-input')
assertions.fieldValue(input, 'test@example.com')  // true
assertions.isDisabled(input)                       // false
assertions.isEnabled(input)                        // true
assertions.isReadOnly(input)                       // false
assertions.isVisible(element)                      // true (element !== null)
assertions.isHidden(element)                       // true (element === null)
```

### Test Patterns

Common user interaction patterns:

```typescript
import { testPatterns } from '../mocks'

// Fill multiple form fields
testPatterns.fillForm({
  email: { element: emailInput, value: 'test@example.com' },
  password: { element: passwordInput, value: 'Pass123' },
})

// Click and wait for async effects
await testPatterns.clickAndWait(submitButton, 50)

// Submit a form
testPatterns.submitForm(formElement)
```

### Type Helpers

Type-safe wrappers for runtime/dynamic paths in tests:

```typescript
import { typeHelpers } from '../mocks'

// For template literal paths
store.applyChanges([
  typeHelpers.change(`items.${itemId}.qty`, 10, {})
])

// For sync/flip pairs
store.syncPaths([
  typeHelpers.syncPair('firstName', 'lastName')
])

// For multiple changes
store.applyChanges(
  typeHelpers.changes([
    [`items.${id1}.price`, 20],
    [`items.${id2}.price`, 30],
  ])
)
```

**Why Type Helpers?** TypeScript can't infer template literals or runtime
strings as literal types. Type helpers cast safely for tests where runtime
behavior is verified anyway.

## Common Patterns

### Pattern 1: Basic Integration Test

```typescript
import {
  createRegistrationFormStore,
  registrationFormFixtures,
  validators,
} from '../mocks'

describe('Registration Form', () => {
  const store = createRegistrationFormStore()

  it('validates email format', () => {
    render(
      <store.Provider initialState={registrationFormFixtures.empty}>
        <FormComponent />
      </store.Provider>
    )

    // Test with validators
    expect(validators.email('invalid')).toBe(false)
    expect(validators.email('test@example.com')).toBe(true)
  })
})
```

### Pattern 2: Testing with Pre-filled Data

```typescript
import {
  createShoppingCartStore,
  shoppingCartFixtures,
} from '../mocks'

it('calculates cart total correctly', () => {
  const store = createShoppingCartStore()

  render(
    <store.Provider initialState={shoppingCartFixtures.multipleItems}>
      <CartComponent />
    </store.Provider>
  )

  // Cart starts with items already in it
  expect(screen.getByText('Total: $77')).toBeInTheDocument()
})
```

### Pattern 3: Testing Error States

```typescript
import {
  createUserProfileStore,
  userProfileFixtures,
  domHelpers,
} from '../mocks'

it('displays validation errors', async () => {
  const store = createUserProfileStore()

  render(
    <store.Provider initialState={userProfileFixtures.withErrors}>
      <ProfileForm />
    </store.Provider>
  )

  // Check errors using domHelpers
  expect(domHelpers.hasErrors()).toBe(true)
  expect(domHelpers.getErrorCount()).toBe(2)
  expect(domHelpers.getAllErrors()).toContain('Username is taken')
})
```

### Pattern 4: Using Generators

```typescript
import { generators, validators } from '../mocks'

it('generates valid test data', () => {
  const email = generators.email()
  const username = generators.username()
  const password = generators.password()

  // All generated data passes validation
  expect(validators.email(email)).toBe(true)
  expect(validators.username(username)).toBe(true)
  expect(validators.password(password)).toBe(true)
})
```

## Relationship with Other Test Utils

### `tests/concerns/test-utils.ts`

**Purpose:** Concern-specific performance and tracking utilities

**No overlap.** Use alongside mocks for concern testing:

```typescript
import { createRegistrationFormStore } from '../mocks'
import { PerformanceBenchmark, createEvaluationTracker } from '../concerns/test-utils'

const store = createRegistrationFormStore()
const benchmark = new PerformanceBenchmark()
const tracker = createEvaluationTracker()

// Use both together
benchmark.start('validation')
// ... trigger validation
const duration = benchmark.end('validation')
```

### `tests/utils/react.ts`

**Purpose:** Lower-level React testing utilities

**Overlap:** `assertions` and `domHelpers` are duplicated

**Resolution:** Prefer `tests/mocks/` for integration tests (established pattern).
Use `tests/utils/react.ts` for lower-level testing without full store setup.

## Best Practices

1. **Always import from index** - `import { ... } from '../mocks'`
2. **Use fixtures for initial state** - Don't create inline state objects
3. **Prefer validators over regex** - Use `validators.email()` not `/regex/`
4. **Use error messages constants** - Don't hardcode error strings
5. **Type-safe paths when possible** - Only use `typeHelpers` for runtime paths
6. **Combine helpers** - Use domHelpers + assertions + validators together

## Adding New Mocks

When adding a new test scenario:

1. **Add type to `types.ts`** - Define the state shape
2. **Add factory to `stores.ts`** - Create the store factory function
3. **Add fixtures to `fixtures.ts`** - Create empty/partial/complete fixtures
4. **Export in `index.ts`** - Re-export for consumers
5. **Update this README** - Document the new scenario

Example:

```typescript
// types.ts
export interface CheckoutForm {
  cardNumber: string
  cvv: string
  _errors: Record<string, string[]>
}

// stores.ts
export const createCheckoutFormStore = () => {
  return createGenericStore<CheckoutForm>()
}

// fixtures.ts
export const checkoutFormFixtures = {
  empty: {
    cardNumber: '',
    cvv: '',
    _errors: {},
  } satisfies CheckoutForm,
}

// index.ts
export { createCheckoutFormStore } from './stores'
export { checkoutFormFixtures } from './fixtures'
```

## FAQ

**Q: Should I use mocks or create my own fixtures?**
A: Use mocks. They're type-safe, consistent, and prevent duplication.

**Q: Can I modify fixtures?**
A: Yes, spread them: `{ ...registrationFormFixtures.empty, email: 'custom' }`

**Q: When should I use typeHelpers?**
A: Only for runtime/template literal paths like `` `items.${id}.qty` ``

**Q: What if mocks don't have what I need?**
A: Add it! Follow the "Adding New Mocks" section above.

**Q: Can I use mocks with vitest benchmarking?**
A: Yes, combine with `tests/concerns/test-utils.ts` for benchmarking.

**Q: Why duplicate assertions/domHelpers in react.ts?**
A: Newer utils file. Prefer mocks for integration tests (established pattern).
