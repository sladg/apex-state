# Phase 7, Task 13: Comprehensive Testing Suite

**Task IDs**: APEX-39 through APEX-46
**Priority**: P1-P2 (High to Medium)
**Dependencies**: All implementation tasks (02-12)
**Phase**: Testing

---

## 🎯 Worker Prompt

**YOU ARE**: A testing specialist focused on use-case and integration testing
**YOUR FOCUS**: Write realistic use-case tests that verify features work together correctly
**STAY FOCUSED**: Test user workflows, not individual functions. Integration over units.
**SUCCESS MEANS**: Confidence that the library works in real-world scenarios

---

## 📋 Testing Philosophy

**Use-Case Driven Testing**: Write tests that simulate real user scenarios, not just unit tests for every function.

**Integration First**: Test that features work together correctly through the public API.

**Realistic Scenarios**: Complex forms, multi-step workflows, edge cases users will hit.

---

## 📋 Task Breakdown

### APEX-39: Test Suite - Basic Store Operations

**Scenarios to test:**
- Creating store and rendering Provider
- useStore reads and updates values reactively
- useJitStore bulk operations apply atomically
- Derived values recalculate automatically
- Multiple Providers with different states

### APEX-40: Test Suite - Sync Paths

**Scenarios to test:**
- Bidirectional sync between two paths
- Transitive sync (A→B→C all sync)
- Cycle detection prevents registration
- Unregistration stops syncing
- Sync with derived values

### APEX-41: Test Suite - Flip Paths

**Scenarios to test:**
- Boolean flip works correctly
- Enum flip (two-value) works
- No infinite flip loops
- Unregistration stops flipping

### APEX-42: Test Suite - Listeners

**Scenarios to test:**
- Global listener receives all changes
- Scoped listener receives only relevant changes
- Nested change breakdown triggers correctly
- Multiple listeners work together
- Listener errors don't break pipeline

### APEX-43: Test Suite - Validators

**Scenarios to test:**
- Zod validation runs on changes
- Errors stored correctly with IDs
- Multiple validators per path
- useErrors hook returns current errors
- Validation errors cleared when valid

### APEX-44: Test Suite - Aggregations

**Scenarios to test:**
- Sources all equal → target gets value
- Sources different → target becomes undefined
- Target changes → sources get value
- Multiple aggregations per target
- Cycle detection works

### APEX-45: Test Suite - Clear Paths

**Scenarios to test:**
- Direct trigger clears paths
- Nested trigger when clearOnNested=true
- No trigger when clearOnNested=false
- Multiple clear rules work together

### APEX-46: Test Suite - Complete Integration

**Complex scenarios:**
- Form with sync, validation, and aggregation
- Multi-step wizard with clear paths
- Settings panel with flip paths and sync
- Performance: many side effects don't slow down
- No excessive re-renders

---

## ✅ Acceptance Criteria

### All Test Suites:
- [ ] Use Vitest with React Testing Library
- [ ] Tests are use-case driven, not unit tests
- [ ] Each test has clear scenario description
- [ ] Tests use realistic data structures
- [ ] Edge cases covered
- [ ] Performance tests where relevant
- [ ] All tests pass consistently

### APEX-46 Integration Tests:
- [ ] Complex form scenario with multiple features
- [ ] Performance benchmark: 100+ side effects
- [ ] Re-render count validation (no excessive renders)
- [ ] Error handling: side effects fail gracefully
- [ ] Memory leak check: unregistration cleans up

---

## 📦 Expected Output

### File Structure:

```
tests/
  integration/
    01-basic-store.test.tsx
    02-sync-paths.test.tsx
    03-flip-paths.test.tsx
    04-listeners.test.tsx
    05-validators.test.tsx
    06-aggregations.test.tsx
    07-clear-paths.test.tsx
    08-complex-form.test.tsx        # APEX-46
    09-wizard-flow.test.tsx         # APEX-46
    10-performance.test.tsx         # APEX-46
  fixtures/
    testData.ts                     # Reusable test data
    testHelpers.tsx                 # Test utilities
  setup.ts
```

### Example: Complex Form Test (APEX-46)

```typescript
import { describe, test, expect } from 'vitest'
import { render, fireEvent, waitFor } from '@testing-library/react'
import { createGenericStore } from '../src'
import { z } from 'zod'

describe('Complex Form Integration', () => {
  test('user registration form with all features', async () => {
    type FormState = {
      // User data
      email: string
      password: string
      confirmPassword: string
      agreeToTerms: boolean
      agreeToNewsletter: boolean
      showPassword: boolean
      showConfirmPassword: boolean

      // Aggregation
      allAgreements: boolean | undefined

      // Errors
      _errors?: Record<string, Array<{ id: string, message: string }>>
    }

    const store = createGenericStore<FormState>({
      errorStorePath: '_errors'
    })

    function RegistrationForm() {
      // Setup side effects
      store.useSideEffects('form-effects', {
        // Sync: password confirmation
        syncPaths: {
          pairs: [
            { id: 'password-sync', path1: 'password', path2: 'confirmPassword' }
          ]
        },

        // Flip: show/hide password toggles
        flipPaths: {
          pairs: [
            { id: 'show-pw', path1: 'showPassword', path2: 'showConfirmPassword' }
          ]
        },

        // Aggregation: all agreements must be checked
        aggregations: {
          rules: [{
            id: 'all-agreements',
            targetPath: 'allAgreements',
            sourcePaths: ['agreeToTerms', 'agreeToNewsletter']
          }]
        },

        // Validation
        validators: {
          validators: [
            {
              id: 'email-valid',
              scope: 'email',
              schema: z.string().email('Invalid email'),
              errorPath: 'email'
            },
            {
              id: 'password-min',
              scope: 'password',
              schema: z.string().min(8, 'Password must be 8+ characters'),
              errorPath: 'password'
            }
          ]
        }
      })

      const email = store.useFieldStore('email')
      const password = store.useFieldStore('password')
      const agreeToTerms = store.useFieldStore('agreeToTerms')
      const agreeToNewsletter = store.useFieldStore('agreeToNewsletter')
      const [allAgreements] = store.useStore('allAgreements')
      const [showPassword] = store.useStore('showPassword')

      const emailErrors = store.useErrors('email')
      const passwordErrors = store.useErrors('password')

      return (
        <form>
          <div>
            <input
              name="email"
              value={email.value}
              onChange={e => email.setValue(e.target.value)}
            />
            {emailErrors.map(err => <span key={err} className="error">{err}</span>)}
          </div>

          <div>
            <input
              name="password"
              type={showPassword ? 'text' : 'password'}
              value={password.value}
              onChange={e => password.setValue(e.target.value)}
            />
            {passwordErrors.map(err => <span key={err} className="error">{err}</span>)}
          </div>

          <label>
            <input
              type="checkbox"
              checked={agreeToTerms.value}
              onChange={e => agreeToTerms.setValue(e.target.checked)}
            />
            Agree to Terms
          </label>

          <label>
            <input
              type="checkbox"
              checked={agreeToNewsletter.value}
              onChange={e => agreeToNewsletter.setValue(e.target.checked)}
            />
            Agree to Newsletter
          </label>

          <button type="submit" disabled={!allAgreements || emailErrors.length > 0 || passwordErrors.length > 0}>
            Register
          </button>
        </form>
      )
    }

    const { getByLabelText, getByRole, queryByText } = render(
      <store.Provider
        initialState={{
          email: '',
          password: '',
          confirmPassword: '',
          agreeToTerms: false,
          agreeToNewsletter: false,
          showPassword: false,
          showConfirmPassword: false,
          allAgreements: undefined
        }}
      >
        <RegistrationForm />
      </store.Provider>
    )

    // Initially: button disabled (no agreements, invalid email)
    const submitButton = getByRole('button', { name: /register/i })
    expect(submitButton).toBeDisabled()

    // Enter invalid email → validation error appears
    fireEvent.change(getByLabelText(/email/i), { target: { value: 'invalid' } })
    await waitFor(() => {
      expect(queryByText(/invalid email/i)).toBeInTheDocument()
    })

    // Fix email
    fireEvent.change(getByLabelText(/email/i), { target: { value: 'user@example.com' } })
    await waitFor(() => {
      expect(queryByText(/invalid email/i)).not.toBeInTheDocument()
    })

    // Short password → validation error
    fireEvent.change(getByLabelText(/password/i), { target: { value: 'short' } })
    await waitFor(() => {
      expect(queryByText(/8\+ characters/i)).toBeInTheDocument()
    })

    // Long enough password
    fireEvent.change(getByLabelText(/password/i), { target: { value: 'longpassword' } })
    await waitFor(() => {
      expect(queryByText(/8\+ characters/i)).not.toBeInTheDocument()
    })

    // Check one agreement → allAgreements still undefined
    fireEvent.click(getByLabelText(/agree to terms/i))
    await waitFor(() => {
      expect(submitButton).toBeDisabled() // allAgreements is undefined (not all checked)
    })

    // Check both agreements → allAgreements becomes true
    fireEvent.click(getByLabelText(/agree to newsletter/i))
    await waitFor(() => {
      expect(submitButton).not.toBeDisabled() // Now enabled!
    })

    // Uncheck one → allAgreements becomes undefined again
    fireEvent.click(getByLabelText(/agree to terms/i))
    await waitFor(() => {
      expect(submitButton).toBeDisabled()
    })
  })
})
```

### Example: Performance Test (APEX-46)

```typescript
describe('Performance', () => {
  test('handles 100 sync paths without slowdown', () => {
    type State = Record<string, number>

    const store = createGenericStore<State>()

    function Component() {
      // Register 100 sync pairs
      const syncPairs = Array.from({ length: 100 }, (_, i) => ({
        id: `sync-${i}`,
        path1: `field${i}` as any,
        path2: `field${i + 100}` as any
      }))

      store.useSideEffects('many-syncs', {
        syncPaths: { pairs: syncPairs }
      })

      const { setChanges } = store.useJitStore()

      return (
        <button onClick={() => {
          // Change one field → should sync to its pair quickly
          const start = performance.now()
          setChanges([['field0', 42, {}]])
          const duration = performance.now() - start

          // Should complete in < 10ms even with 100 sync paths
          expect(duration).toBeLessThan(10)
        }}>
          Test
        </button>
      )
    }

    const initialState = Object.fromEntries(
      Array.from({ length: 200 }, (_, i) => [`field${i}`, 0])
    )

    render(
      <store.Provider initialState={initialState}>
        <Component />
      </store.Provider>
    )
  })

  test('no excessive re-renders on batch changes', () => {
    type State = { a: number, b: number, c: number }
    const store = createGenericStore<State>()

    let renderCount = 0

    function Component() {
      renderCount++
      const { proxyValue, setChanges } = store.useJitStore()

      return (
        <div>
          <span>{proxyValue.a + proxyValue.b + proxyValue.c}</span>
          <button onClick={() => {
            const before = renderCount
            setChanges([
              ['a', 1, {}],
              ['b', 2, {}],
              ['c', 3, {}]
            ])
            // Should only trigger ONE additional render
            requestIdleCallback(() => {
              expect(renderCount - before).toBe(1)
            })
          }}>
            Update All
          </button>
        </div>
      )
    }

    render(
      <store.Provider initialState={{ a: 0, b: 0, c: 0 }}>
        <Component />
      </store.Provider>
    )
  })
})
```

---

## 🚨 Common Pitfalls

- **DON'T**: Write isolated unit tests for every function
- **DON'T**: Mock out the framework - test through public API
- **DON'T**: Skip edge cases and error scenarios
- **DO**: Write realistic, multi-feature integration tests
- **DO**: Test performance with realistic loads
- **DO**: Verify no excessive re-renders
- **DO**: Test cleanup (unmounting, unregistration)

---

## ➡️ Next Steps

Once this task is complete, proceed to:
- **Task 14**: `14-optimization-docs.md` - Performance optimization and documentation
