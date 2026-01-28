# Phase 5, Task 10: Validators Side-Effect

**Task IDs**: APEX-27, APEX-28, APEX-29, APEX-30
**Priority**: P2 (Medium)
**Dependencies**: Task 03 (Base Store), Task 05 (Synchronizer Pipeline)
**Phase**: Side Effects - Secondary

---

## 🎯 Worker Prompt

**YOU ARE**: A data validation specialist
**YOUR FOCUS**: Implement Zod schema validation with error storage in state
**STAY FOCUSED**: Store errors at configurable path within DATA, support multiple validators per path
**SUCCESS MEANS**: Validation runs on changes, errors stored with IDs, useErrors hook retrieves them

---

## 📋 Task Breakdown

### APEX-27: Design Error Storage System

Design how validation errors are stored within DATA structure at configurable path.

**What to do:**
1. `createGenericStore` accepts optional `errorStorePath: string` parameter
2. Errors stored at `DATA[errorStorePath]` as `Record<DeepKey<DATA>, Array<{id: string, message: string}>>`
3. Each validator ID stores its error with ID for proper unregistration
4. Path can be accessed like any other state path

### APEX-28: Implement Validator Registration System

Allow registering Zod schema validators for paths/scopes.

**What to do:**
1. API: `registerValidator(id, {scope, schema, path})`
2. scope: null (global) or specific path to validate
3. schema: Zod schema
4. path: where to store validation errors
5. Unregister by ID

### APEX-29: Implement Validation Processor

Run validators when relevant data changes and store errors.

**What to do:**
1. When data in validator scope changes, run `schema.safeParse()`
2. Store error messages at specified path with validator ID
3. Multiple validators can target same path (array of errors)
4. Remove errors when validation passes
5. Efficient: only run validators for changed scopes

### APEX-30: Implement useErrors Hook

Provide hook to retrieve validation errors for a path.

**What to do:**
1. Hook signature: `useErrors(path: string)` returns `string[]`
2. Reactive: re-renders when errors change
3. Returns empty array if no errors

---

## ✅ Acceptance Criteria

### APEX-27 Criteria:
- [ ] `createGenericStore` accepts `errorStorePath?: string` config (default: "_errors")
- [ ] Errors stored in DATA at errorStorePath as `Record<string, Array<{id: string, message: string}>>`
- [ ] Error structure allows multiple validators per path
- [ ] Errors accessible like any other state path
- [ ] Type-safe error storage path

### APEX-28 Criteria:
- [ ] `ValidatorsRegistry<DATA, META>` class in `src/sideEffects/validators/registry.ts`
- [ ] Method: `register(id, {scope, schema, errorPath})`
- [ ] Method: `unregister(id)`
- [ ] Method: `getValidatorsForScope(scope)` returns relevant validators
- [ ] Supports null scope (global validation)
- [ ] Stores Zod schemas for validation
- [ ] Test demonstrates registration

### APEX-29 Criteria:
- [ ] `validatorsSynchronizer` function in `src/pipeline/synchronizers/validators.ts`
- [ ] Signature: `Synchronizer<DATA, META>`
- [ ] Runs validators when scope data changes
- [ ] Uses `schema.safeParse()` for validation
- [ ] Adds error changes to ArrayOfChanges with validator ID
- [ ] Removes errors when validation passes
- [ ] Only runs affected validators (not all)
- [ ] Integrated into pipeline
- [ ] Test demonstrates validation

### APEX-30 Criteria:
- [ ] `useErrors` hook exported from `src/hooks/useErrors.ts`
- [ ] Signature: `useErrors(path: DeepKey<DATA>): string[]`
- [ ] Returns array of error messages for path
- [ ] Reactive: re-renders when errors change
- [ ] Returns empty array if no errors
- [ ] Test demonstrates reactive error display

---

## 📦 Expected Output

### File Structure:

```
src/
  sideEffects/
    validators/
      registry.ts        # ValidatorsRegistry class
      types.ts           # Validator config types
      index.ts
  pipeline/
    synchronizers/
      validators.ts      # validatorsSynchronizer
  hooks/
    useErrors.ts         # useErrors hook
  store/
    createStore.ts       # Updated with errorStorePath config
  types/
    sideEffects.ts       # Updated with validators config

tests/
  sideEffects/
    validators/
      registry.test.ts
      synchronizer.test.ts
      integration.test.tsx
  hooks/
    useErrors.test.tsx
```

### src/sideEffects/validators/types.ts:

```typescript
import type { DeepKey } from '../../types'
import type { z } from 'zod'

export interface ValidatorConfig<DATA> {
  id: string
  scope: DeepKey<DATA> | null  // null = validate entire state
  schema: z.ZodSchema
  errorPath: DeepKey<DATA>     // Where to store errors for this validator
}

export interface StoredError {
  id: string      // Validator ID
  message: string // Error message from Zod
}

// Type for error storage structure
export type ErrorStore<DATA> = {
  [K in DeepKey<DATA>]?: StoredError[]
}
```

### src/sideEffects/validators/registry.ts:

```typescript
import type { DeepKey, GenericMeta } from '../../types'
import type { ValidatorConfig } from './types'

export class ValidatorsRegistry<
  DATA extends object,
  META extends GenericMeta
> {
  private validators = new Map<string, ValidatorConfig<DATA>>()
  private scopeIndex = new Map<string, Set<string>>() // scope → Set<validatorId>

  register(config: ValidatorConfig<DATA>): void {
    this.validators.set(config.id, config)

    // Index by scope
    const scopeKey = config.scope === null ? '__global__' : (config.scope as string)
    if (!this.scopeIndex.has(scopeKey)) {
      this.scopeIndex.set(scopeKey, new Set())
    }
    this.scopeIndex.get(scopeKey)!.add(config.id)
  }

  unregister(id: string): void {
    const config = this.validators.get(id)
    if (!config) return

    const scopeKey = config.scope === null ? '__global__' : (config.scope as string)
    this.scopeIndex.get(scopeKey)?.delete(id)

    this.validators.delete(id)
  }

  getValidator(id: string): ValidatorConfig<DATA> | undefined {
    return this.validators.get(id)
  }

  getValidatorsForScope(scope: DeepKey<DATA> | null): ValidatorConfig<DATA>[] {
    const scopeKey = scope === null ? '__global__' : (scope as string)
    const ids = this.scopeIndex.get(scopeKey) || new Set()
    return Array.from(ids).map(id => this.validators.get(id)!)
  }

  getAllValidators(): ValidatorConfig<DATA>[] {
    return Array.from(this.validators.values())
  }

  getValidatorsAffectedByChange(path: DeepKey<DATA>): ValidatorConfig<DATA>[] {
    const affected: ValidatorConfig<DATA>[] = []

    // Check global validators
    affected.push(...this.getValidatorsForScope(null))

    // Check validators with scope matching or parent of this path
    for (const [scopeKey, ids] of this.scopeIndex.entries()) {
      if (scopeKey === '__global__') continue

      const pathStr = path as string
      // Validator scope matches or is parent of changed path
      if (pathStr === scopeKey || pathStr.startsWith(scopeKey + '.')) {
        ids.forEach(id => {
          const validator = this.validators.get(id)
          if (validator) affected.push(validator)
        })
      }
    }

    return affected
  }
}
```

### src/pipeline/synchronizers/validators.ts:

```typescript
import type { Synchronizer } from '../types'
import type { ArrayOfChanges, DeepKey, GenericMeta } from '../../types'
import type { ValidatorsRegistry } from '../../sideEffects/validators/registry'
import type { StoredError } from '../../sideEffects/validators/types'
import { deepGet } from '../../store/utils/deepAccess'

/**
 * Synchronizer for validators side-effect.
 *
 * Runs Zod validations when relevant data changes.
 * Stores errors in state at configured error paths.
 */
export function createValidatorsSynchronizer<
  DATA extends object,
  META extends GenericMeta
>(
  registry: ValidatorsRegistry<DATA, META>,
  errorStorePath: string
): Synchronizer<DATA, META> {
  return (changes, state) => {
    const newChanges: ArrayOfChanges<DATA, META> = []
    const processedValidators = new Set<string>()

    for (const [path, value, meta] of changes) {
      // Find validators affected by this change
      const validators = registry.getValidatorsAffectedByChange(path)

      for (const validator of validators) {
        if (processedValidators.has(validator.id)) continue
        processedValidators.add(validator.id)

        // Get data to validate
        const dataToValidate = validator.scope === null
          ? state
          : deepGet(state, validator.scope)

        // Run validation
        const result = validator.schema.safeParse(dataToValidate)

        // Get current errors for this validator's error path
        const errorPath = `${errorStorePath}.${validator.errorPath}` as DeepKey<DATA>
        const currentErrors = (deepGet(state, errorPath) as StoredError[]) || []

        let updatedErrors: StoredError[]

        if (result.success) {
          // Validation passed - remove this validator's errors
          updatedErrors = currentErrors.filter(e => e.id !== validator.id)
        } else {
          // Validation failed - add/update this validator's error
          const errorMessage = result.error.errors.map(e => e.message).join(', ')
          const otherErrors = currentErrors.filter(e => e.id !== validator.id)
          updatedErrors = [
            ...otherErrors,
            { id: validator.id, message: errorMessage }
          ]
        }

        // Add change to update errors
        newChanges.push([
          errorPath,
          updatedErrors.length > 0 ? updatedErrors : undefined,
          { ...meta, isProgramaticChange: true }
        ])
      }
    }

    return [...changes, ...newChanges]
  }
}
```

### src/hooks/useErrors.ts:

```typescript
import { useCallback } from 'react'
import type { DeepKey } from '../types'
import { useStore } from './useStore'
import type { StoredError } from '../sideEffects/validators/types'

/**
 * Hook to retrieve validation errors for a specific path.
 *
 * Returns array of error messages.
 * Reactive: re-renders when errors change.
 */
export function useErrors<DATA extends object>(
  path: DeepKey<DATA>,
  errorStorePath = '_errors'
): string[] {
  const errorPath = `${errorStorePath}.${path}` as DeepKey<DATA>
  const [errors] = useStore(errorPath)

  if (!errors || !Array.isArray(errors)) {
    return []
  }

  return (errors as StoredError[]).map(e => e.message)
}
```

### Updated createGenericStore:

```typescript
export interface StoreConfig {
  errorStorePath?: string  // Default: "_errors"
}

export function createGenericStore<
  DATA extends object,
  META extends GenericMeta = GenericMeta
>(config?: StoreConfig): StoreReturn<DATA, META> {
  const errorStorePath = config?.errorStorePath || '_errors'

  // ... rest of implementation
  // Pass errorStorePath to validatorsSynchronizer
}
```

### Updated SideEffects type:

```typescript
// src/types/sideEffects.ts
export interface ValidatorsConfig<DATA> {
  validators: Array<ValidatorConfig<DATA>>
}

export interface SideEffects<DATA> {
  syncPaths?: SyncPathConfig<DATA>
  flipPaths?: FlipPathConfig<DATA>
  aggregations?: AggregationConfig<DATA>
  validators?: ValidatorsConfig<DATA>
  // ... other side effects
}
```

---

## 🧪 Verification Steps

```typescript
import { z } from 'zod'

test('validators: validation errors stored', () => {
  type State = {
    email: string
    _errors?: Record<string, StoredError[]>
  }

  const store = createGenericStore<State>()

  function Component() {
    store.useSideEffects('validation', {
      validators: {
        validators: [{
          id: 'email-validator',
          scope: 'email',
          schema: z.string().email(),
          errorPath: 'email'
        }]
      }
    })

    const [email, setEmail] = store.useStore('email')
    const errors = store.useErrors('email')

    return (
      <div>
        <input value={email} onChange={e => setEmail(e.target.value)} />
        {errors.map((err, i) => <div key={i}>{err}</div>)}
      </div>
    )
  }

  const { getByRole, queryByText } = render(
    <store.Provider initialState={{ email: 'valid@email.com' }}>
      <Component />
    </store.Provider>
  )

  // Initial: valid email, no errors
  expect(queryByText(/invalid/i)).not.toBeInTheDocument()

  // Change to invalid email
  const input = getByRole('textbox')
  fireEvent.change(input, { target: { value: 'invalid-email' } })

  // Should show error
  waitFor(() => {
    expect(queryByText(/invalid/i)).toBeInTheDocument()
  })

  // Change to valid email
  fireEvent.change(input, { target: { value: 'valid@email.com' } })

  // Error should disappear
  waitFor(() => {
    expect(queryByText(/invalid/i)).not.toBeInTheDocument()
  })
})

test('validators: multiple validators per path', () => {
  type State = {
    password: string
    _errors?: Record<string, StoredError[]>
  }

  const store = createGenericStore<State>()

  function Component() {
    store.useSideEffects('validation', {
      validators: {
        validators: [
          {
            id: 'password-min',
            scope: 'password',
            schema: z.string().min(8),
            errorPath: 'password'
          },
          {
            id: 'password-pattern',
            scope: 'password',
            schema: z.string().regex(/[A-Z]/),
            errorPath: 'password'
          }
        ]
      }
    })

    const [password, setPassword] = store.useStore('password')
    const errors = store.useErrors('password')

    return (
      <div>
        <input value={password} onChange={e => setPassword(e.target.value)} />
        <div>Errors: {errors.length}</div>
      </div>
    )
  }

  const { getByRole, getByText } = render(
    <store.Provider initialState={{ password: 'ab' }}>
      <Component />
    </store.Provider>
  )

  // Initial: fails both validators
  expect(getByText('Errors: 2')).toBeInTheDocument()

  // Longer but no uppercase
  fireEvent.change(getByRole('textbox'), { target: { value: 'abcdefgh' } })
  waitFor(() => expect(getByText('Errors: 1')).toBeInTheDocument())

  // Valid
  fireEvent.change(getByRole('textbox'), { target: { value: 'Abcdefgh' } })
  waitFor(() => expect(getByText('Errors: 0')).toBeInTheDocument())
})
```

---

## 🚨 Common Pitfalls

- **DON'T**: Validate unchanged data (only run affected validators)
- **DON'T**: Forget to remove errors when validation passes
- **DON'T**: Lose other validator errors when updating (preserve errors from other IDs)
- **DO**: Use Zod's `safeParse()` (not `parse()`) to avoid throwing
- **DO**: Handle arrays of errors correctly (multiple validators per path)
- **DO**: Test error removal when validation passes

---

## 💡 Implementation Tips

### Error Storage Pattern:

```typescript
// Errors stored at: state._errors.email = [
//   { id: 'validator-1', message: 'Invalid email' },
//   { id: 'validator-2', message: 'Email too short' }
// ]
```

### Zod Integration:

```typescript
const result = schema.safeParse(data)

if (!result.success) {
  const message = result.error.errors
    .map(e => e.message)
    .join(', ')
  // Store error
}
```

---

## ➡️ Next Steps

Once this task is complete, proceed to:
- **Task 11**: `11-clear-paths.md` - Implement clear paths side-effect
