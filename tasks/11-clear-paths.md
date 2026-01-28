# Phase 5, Task 11: Clear Paths Side-Effect

**Task IDs**: APEX-34, APEX-35
**Priority**: P2 (Medium)
**Dependencies**: Task 05 (Synchronizer Pipeline)
**Phase**: Side Effects - Secondary

---

## 🎯 Worker Prompt

**YOU ARE**: A state cleanup specialist
**YOUR FOCUS**: Implement clear paths that set values to undefined when trigger conditions met
**STAY FOCUSED**: Simple trigger→clear logic with optional nested change detection
**SUCCESS MEANS**: Paths cleared on trigger change, nested option works, clean implementation

---

## 📋 Task Breakdown

### APEX-34: Implement Clear Paths Registration System

Allow registering paths that should be cleared when trigger path changes.

**What to do:**
1. API: `registerClearPaths(id, triggerPath, clearPaths[], clearOnNested?)`
2. triggerPath: path that triggers the clear
3. clearPaths: paths to set to undefined
4. clearOnNested: boolean flag for nested change clearing
5. Unregister by ID

### APEX-35: Implement Clear Paths Processor

Clear paths when trigger conditions met.

**What to do:**
1. When triggerPath changes, set all clearPaths to undefined
2. If clearOnNested=true, also trigger on nested changes within triggerPath
3. Add cleared paths to ArrayOfChanges

---

## ✅ Acceptance Criteria

### APEX-34 Criteria:
- [ ] `ClearPathsRegistry<DATA>` class in `src/sideEffects/clearPaths/registry.ts`
- [ ] Method: `register(id, triggerPath, clearPaths[], clearOnNested)`
- [ ] Method: `unregister(id)`
- [ ] Method: `getClearRules(path)` returns rules triggered by path
- [ ] Stores clear configurations
- [ ] Test demonstrates registration

### APEX-35 Criteria:
- [ ] `clearPathsSynchronizer` function in `src/pipeline/synchronizers/clearPaths.ts`
- [ ] Signature: `Synchronizer<DATA, META>`
- [ ] When trigger path changes: add changes to clear target paths
- [ ] When clearOnNested=true: trigger on `triggerPath.nested` changes too
- [ ] Cleared paths set to undefined
- [ ] Integrated into pipeline
- [ ] Test demonstrates both direct and nested triggers

---

## 📦 Expected Output

### File Structure:

```
src/
  sideEffects/
    clearPaths/
      registry.ts        # ClearPathsRegistry class
      types.ts           # ClearPathConfig type
      index.ts
  pipeline/
    synchronizers/
      clearPaths.ts      # clearPathsSynchronizer
  types/
    sideEffects.ts       # Updated with clearPaths config

tests/
  sideEffects/
    clearPaths/
      registry.test.ts
      synchronizer.test.ts
      integration.test.tsx
```

### src/sideEffects/clearPaths/types.ts:

```typescript
import type { DeepKey } from '../../types'

export interface ClearPathConfig<DATA> {
  id: string
  triggerPath: DeepKey<DATA>
  clearPaths: DeepKey<DATA>[]
  clearOnNested?: boolean  // If true, nested changes also trigger
}
```

### src/sideEffects/clearPaths/registry.ts:

```typescript
import type { DeepKey } from '../../types'
import type { ClearPathConfig } from './types'

export class ClearPathsRegistry<DATA extends object> {
  private rules = new Map<string, ClearPathConfig<DATA>>()
  private triggerIndex = new Map<string, Set<string>>() // trigger → Set<ruleId>

  register(config: ClearPathConfig<DATA>): void {
    this.rules.set(config.id, config)

    const triggerKey = config.triggerPath as string
    if (!this.triggerIndex.has(triggerKey)) {
      this.triggerIndex.set(triggerKey, new Set())
    }
    this.triggerIndex.get(triggerKey)!.add(config.id)
  }

  unregister(id: string): void {
    const config = this.rules.get(id)
    if (!config) return

    const triggerKey = config.triggerPath as string
    this.triggerIndex.get(triggerKey)?.delete(id)

    this.rules.delete(id)
  }

  getClearRulesTriggeredBy(path: DeepKey<DATA>): ClearPathConfig<DATA>[] {
    const pathStr = path as string
    const triggeredRules: ClearPathConfig<DATA>[] = []

    // Check exact match
    const exactIds = this.triggerIndex.get(pathStr) || new Set()
    exactIds.forEach(id => {
      const rule = this.rules.get(id)
      if (rule) triggeredRules.push(rule)
    })

    // Check if path is nested under a trigger with clearOnNested=true
    for (const [triggerPath, ids] of this.triggerIndex.entries()) {
      if (pathStr.startsWith(triggerPath + '.')) {
        ids.forEach(id => {
          const rule = this.rules.get(id)
          if (rule && rule.clearOnNested) {
            triggeredRules.push(rule)
          }
        })
      }
    }

    return triggeredRules
  }
}
```

### src/pipeline/synchronizers/clearPaths.ts:

```typescript
import type { Synchronizer } from '../types'
import type { ArrayOfChanges, DeepKey, GenericMeta } from '../../types'
import type { ClearPathsRegistry } from '../../sideEffects/clearPaths/registry'

/**
 * Synchronizer for clear paths side-effect.
 *
 * When trigger paths change, clear specified paths (set to undefined).
 * Optionally trigger on nested changes too.
 */
export function createClearPathsSynchronizer<
  DATA extends object,
  META extends GenericMeta
>(
  registry: ClearPathsRegistry<DATA>
): Synchronizer<DATA, META> {
  return (changes, state) => {
    const newChanges: ArrayOfChanges<DATA, META> = []
    const clearedPaths = new Set<string>() // Avoid duplicate clears

    for (const [path, value, meta] of changes) {
      // Find clear rules triggered by this change
      const rules = registry.getClearRulesTriggeredBy(path)

      for (const rule of rules) {
        // Clear all target paths
        for (const clearPath of rule.clearPaths) {
          const clearPathStr = clearPath as string
          if (clearedPaths.has(clearPathStr)) continue
          clearedPaths.add(clearPathStr)

          newChanges.push([
            clearPath,
            undefined,
            { ...meta, isProgramaticChange: true }
          ])
        }
      }
    }

    return [...changes, ...newChanges]
  }
}
```

### Updated SideEffects type:

```typescript
// src/types/sideEffects.ts
export interface ClearPathsConfig<DATA> {
  rules: Array<ClearPathConfig<DATA>>
}

export interface SideEffects<DATA> {
  syncPaths?: SyncPathConfig<DATA>
  flipPaths?: FlipPathConfig<DATA>
  aggregations?: AggregationConfig<DATA>
  validators?: ValidatorsConfig<DATA>
  clearPaths?: ClearPathsConfig<DATA>
}
```

---

## 🧪 Verification Steps

```typescript
test('clearPaths: direct trigger clears paths', () => {
  const store = createGenericStore<{
    selectedUser: string | null
    userData: any
    userPreferences: any
  }>()

  function Component() {
    store.useSideEffects('clear', {
      clearPaths: {
        rules: [{
          id: 'clear-user-data',
          triggerPath: 'selectedUser',
          clearPaths: ['userData', 'userPreferences'],
          clearOnNested: false
        }]
      }
    })

    const [selectedUser, setSelectedUser] = store.useStore('selectedUser')
    const [userData] = store.useStore('userData')
    const [userPreferences] = store.useStore('userPreferences')

    return (
      <div>
        <span>user: {selectedUser}</span>
        <span>data: {JSON.stringify(userData)}</span>
        <span>prefs: {JSON.stringify(userPreferences)}</span>
        <button onClick={() => setSelectedUser('alice')}>Select Alice</button>
      </div>
    )
  }

  const { getByText } = render(
    <store.Provider initialState={{
      selectedUser: 'bob',
      userData: { name: 'Bob' },
      userPreferences: { theme: 'dark' }
    }}>
      <Component />
    </store.Provider>
  )

  // Change selected user → should clear userData and userPreferences
  getByText('Select Alice').click()

  waitFor(() => {
    expect(getByText('data: undefined')).toBeInTheDocument()
    expect(getByText('prefs: undefined')).toBeInTheDocument()
  })
})

test('clearPaths: nested trigger when clearOnNested=true', () => {
  const store = createGenericStore<{
    form: { field1: string, field2: string }
    formError: string | null
  }>()

  function Component() {
    store.useSideEffects('clear', {
      clearPaths: {
        rules: [{
          id: 'clear-error',
          triggerPath: 'form',
          clearPaths: ['formError'],
          clearOnNested: true
        }]
      }
    })

    const [field1, setField1] = store.useStore('form.field1')
    const [formError] = store.useStore('formError')

    return (
      <div>
        <input value={field1} onChange={e => setField1(e.target.value)} />
        <span>error: {formError || 'none'}</span>
      </div>
    )
  }

  const { getByRole, getByText } = render(
    <store.Provider initialState={{
      form: { field1: '', field2: '' },
      formError: 'Please fill form'
    }}>
      <Component />
    </store.Provider>
  )

  // Initial: error exists
  expect(getByText('error: Please fill form')).toBeInTheDocument()

  // Change nested field → should clear error (clearOnNested=true)
  const input = getByRole('textbox')
  fireEvent.change(input, { target: { value: 'test' } })

  waitFor(() => {
    expect(getByText('error: none')).toBeInTheDocument()
  })
})

test('clearPaths: no trigger when clearOnNested=false', () => {
  const registry = new ClearPathsRegistry()

  registry.register({
    id: 'test',
    triggerPath: 'form',
    clearPaths: ['error'],
    clearOnNested: false
  })

  // Nested change should NOT trigger
  const rules = registry.getClearRulesTriggeredBy('form.field1')
  expect(rules).toHaveLength(0)

  // Direct change SHOULD trigger
  const rules2 = registry.getClearRulesTriggeredBy('form')
  expect(rules2).toHaveLength(1)
})
```

---

## 🚨 Common Pitfalls

- **DON'T**: Clear the same path multiple times in one pipeline execution
- **DON'T**: Forget to check clearOnNested flag
- **DON'T**: Clear paths that don't need clearing (check if already undefined for optimization)
- **DO**: Use Set to track cleared paths and avoid duplicates
- **DO**: Test both clearOnNested=true and false scenarios
- **DO**: Handle edge cases: clearing already undefined values

---

## 💡 Implementation Tips

### Nested Path Detection:

```typescript
// Check if 'user.profile.name' is nested under 'user'
const isNested = 'user.profile.name'.startsWith('user' + '.')
```

### Avoiding Duplicate Clears:

Use a Set to track paths that have already been cleared in this pipeline execution.

---

## ➡️ Next Steps

Once this task is complete, proceed to:
- **Task 12**: `12-advanced-types-hooks.md` - Implement advanced type utilities and form hooks
