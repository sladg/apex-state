---
created: 2026-01-28 (37db2cf)
updated: 2026-02-04 (f11ceca)
status: archived
---

# Should We Switch to valtio-reactive?

**Document Type**: Technical Analysis & Decision
**Priority**: P0 (Critical)
**Dependencies**: ARCHITECTURE_CONCERNS_CRITICAL.md
**Related**: concerns-clean.ts, concerns-working-v2.ts

---

## 🎯 TL;DR

**✅ YES** - valtio-reactive solves our dependency tracking problem with automatic property-level tracking, eliminating ~50% of our code.

---

## 💡 What valtio-reactive Provides

### Automatic Property-Level Dependency Tracking

```typescript
import { effect } from 'valtio-reactive'

effect(() => {
  // Automatically tracks ONLY: state.products['leg-1'].strike
  const value = state.products['leg-1'].strike
  console.log('Strike:', value)
})

state.products['leg-1'].strike = 150  // ✅ Re-runs
state.products['leg-1'].status = 'locked'  // ❌ Doesn't re-run (not accessed)
state.market.spot = 120  // ❌ Doesn't re-run (not accessed)
```

**This is exactly what we need!** Property-level tracking without manual `tracks()` functions.

---

## 📊 Code Comparison

### ❌ Current Approach (Manual Tracking)

```typescript
// ❌ Must manually declare dependencies
const validationState = {
  name: 'validationState',

  tracks: (path, input) => {
    // 10+ lines to extract all dependencies
    const paths = [path]
    if ('scope' in input) paths.push(input.scope)
    return paths
  },

  evaluate: (props) => {
    return props.schema.safeParse(props.value).success
  }
}

// ❌ Complex subscribe() with ops parsing
subscribe(dataProxy, (ops) => {
  // 40+ lines to:
  // - Parse operations
  // - Match paths
  // - Deduplicate concerns
  // - Evaluate each
})
```

**Problems**:
- Manual dependency declaration (error-prone)
- Complex path matching logic
- Need deduplication logic
- ~200 lines of boilerplate

### ✅ With valtio-reactive (Automatic Tracking)

```typescript
// ✅ No tracks() needed!
const validationState = {
  name: 'validationState',
  evaluate: (props) => {
    // Automatically tracks accessed properties
    return props.schema.safeParse(props.value).success
  }
}

// ✅ Simple effect() wrapping
useConcerns: (id, registration) => {
  Object.entries(registration).forEach(([path, concerns]) => {
    Object.entries(concerns).forEach(([name, config]) => {
      const concern = AppConcerns.find(c => c.name === name)

      // THE MAGIC: effect() automatically tracks
      effect(() => {
        const value = getDeepValue(dataProxy, path)
        const result = concern.evaluate({ state: dataProxy, path, value, ...config })
        cache.set(key, result)
      })
    })
  })
}
```

**Benefits**:
- No manual dependency tracking
- No path matching logic needed
- No deduplication needed (effect() handles it)
- ~100 lines total (50% reduction!)

---

## 🗑️ Line-by-Line Elimination

### ❌ What We Can DELETE

#### 1. tracks() Functions (~30 lines per concern type)

```typescript
// ❌ DELETE THIS
tracks: (path, input) => {
  const paths = [path]
  if ('scope' in input) paths.push(input.scope)
  return paths
}

// ❌ DELETE THIS
tracks: (path, input) => extractPathsFromBoolLogic(input.condition)

// ❌ DELETE THIS
tracks: (path, input) => extractPathsFromTemplate(input.template)
```

#### 2. Path Extraction Helpers (~60 lines)

```typescript
// ❌ DELETE THIS
const extractPathsFromBoolLogic = (logic) => {
  if ('IS_EQUAL' in logic) return [logic.IS_EQUAL[0]]
  if ('AND' in logic) return logic.AND.flatMap(extractPathsFromBoolLogic)
  // ... 20+ more lines
}

// ❌ DELETE THIS
const extractPathsFromTemplate = (template) => {
  const regex = /\{\{(\w+(?:\.\w+)*)\}\}/g
  // ... 10+ more lines
}
```

#### 3. Path Matching Logic (~20 lines)

```typescript
// ❌ DELETE THIS
const pathMatches = (changedPath: string, trackedPaths: string[]): boolean => {
  for (const tracked of trackedPaths) {
    if (changedPath === tracked) return true
    if (changedPath.startsWith(tracked + '.')) return true
    if (tracked.startsWith(changedPath + '.')) return true
  }
  return false
}
```

#### 4. subscribe() with Deduplication (~40 lines)

```typescript
// ❌ DELETE THIS
subscribe(dataProxy, (ops) => {
  const affectedConcerns = new Map()

  ops.forEach(([op, pathArray]) => {
    const changedPath = pathArray.join('.')

    concernsRegistry.forEach(regs => {
      regs.forEach(reg => {
        if (pathMatches(changedPath, reg.trackedPaths)) {
          affectedConcerns.set(key, reg)
        }
      })
    })
  })

  affectedConcerns.forEach(reg => evaluateConcern(reg))
})
```

#### 5. trackedPaths Storage and Pre-computation

```typescript
// ❌ DELETE THIS
type ConcernRegistration = {
  // ...
  trackedPaths: string[]  // No longer needed
}

// ❌ DELETE THIS
const trackedPaths = concern.tracks(path, config)
```

#### 6. unstable_enableOp() Requirement

```typescript
// ❌ DELETE THIS
import { unstable_enableOp } from 'valtio'
unstable_enableOp()  // Not needed with effect()
```

### ✅ What We ADD (Much Simpler)

```typescript
// ✅ ADD THIS (5 lines)
import { effect } from 'valtio-reactive'

effect(() => {
  const result = concern.evaluate({ state: dataProxy, ... })
  cache.set(key, result)
})
```

---

## ⚡ Performance Comparison

| Aspect | Manual (Current) | valtio-reactive | Winner |
|--------|------------------|-----------------|--------|
| **Dependency Tracking** | Manual declaration | Automatic | 🏆 valtio-reactive |
| **Code Complexity** | ~200 lines | ~100 lines | 🏆 valtio-reactive |
| **Runtime Overhead** | subscribe() loop | effect() tracking | 🤔 Similar |
| **Memory Usage** | Store tracked paths | effect() internal | 🤔 Similar |
| **Re-evaluation** | O(k) after dedup | O(k) automatic | 🤝 Same |
| **Debuggability** | Explicit tracks() | Magic tracking | 🏆 Manual (explicit) |
| **Error-Proneness** | Easy to miss deps | Impossible to miss | 🏆 valtio-reactive |

**Overall**: valtio-reactive wins on simplicity, correctness, and maintainability.

---

## ⚠️ Potential Concerns

### 🤔 1. Magic vs Explicit

**Manual tracks()**:
- ✅ Explicit dependencies (grep for paths)
- ✅ Clear what's tracked
- ❌ Easy to forget dependencies

**valtio-reactive**:
- ✅ Impossible to miss dependencies
- ❌ "Magic" tracking (less explicit)
- ✅ Correct by construction

**Verdict**: The correctness guarantee outweighs explicitness.

### 📚 2. Library Maturity

**valtio-reactive**:
- ⚠️ Newer than valtio core
- ⚠️ Smaller community
- ✅ Maintained by valtiojs org
- ✅ Simple API (less to break)

**Verdict**: Worth the risk for 50% code reduction.

### 📦 3. Bundle Size

**Current**: valtio only (~3.5kb)
**With valtio-reactive**: valtio + valtio-reactive (~4.5kb?)

**Verdict**: Negligible increase for massive DX improvement.

### 🔧 4. Migration Effort

**What Changes**:
- Remove all tracks() functions
- Remove subscribe() logic
- Wrap evaluations in effect()

**Estimated Effort**: 2-3 hours to migrate + test

**Verdict**: Very low effort for high reward.

---

## 🧪 Test Migration

Our test suite would become SIMPLER:

### ❌ Before (Current)

```typescript
test('should only recalculate validationState when strike changes', () => {
  let evalCount = 0

  // Override tracks() for testing
  const testConcern = {
    ...validationState,
    evaluate: (props) => {
      evalCount++
      return validationState.evaluate(props)
    }
  }

  // Test that tracks() returns correct paths
  expect(testConcern.tracks('strike', { schema: z.number() })).toEqual(['strike'])

  // Test re-evaluation
  store.proxy.products['leg-1'].strike = 150
  expect(evalCount).toBe(2)  // Initial + 1 change
})
```

### ✅ After (valtio-reactive)

```typescript
test('should only recalculate validationState when strike changes', () => {
  let evalCount = 0

  const testConcern = {
    ...validationState,
    evaluate: (props) => {
      evalCount++
      return validationState.evaluate(props)
    }
  }

  store.useConcerns('test', {
    'products.leg-1.strike': {
      validationState: { schema: z.number().min(0) }
    }
  })

  expect(evalCount).toBe(1)  // Initial

  store.proxy.products['leg-1'].strike = 150
  expect(evalCount).toBe(2)  // Re-evaluated

  store.proxy.products['leg-1'].status = 'locked'
  expect(evalCount).toBe(2)  // NOT re-evaluated (correct!)
})
```

**Simpler and more direct!**

---

## ✅ Migration Checklist

### 🔧 Phase 1: Setup (5 min)
- [ ] Install valtio-reactive: `npm install valtio-reactive`
- [ ] Add import: `import { effect } from 'valtio-reactive'`

### 🗑️ Phase 2: Remove Old Code (30 min)
- [ ] Delete all `tracks()` functions from concerns
- [ ] Delete `extractPathsFromBoolLogic()`
- [ ] Delete `extractPathsFromTemplate()`
- [ ] Delete `pathMatches()`
- [ ] Delete `subscribe()` with ops parsing
- [ ] Delete `trackedPaths` from ConcernRegistration type
- [ ] Delete `unstable_enableOp()` call

### ⚡ Phase 3: Add effect() Wrapper (30 min)
- [ ] Wrap concern evaluation in `effect()`
- [ ] Return dispose function from effect
- [ ] Store dispose in registration
- [ ] Call dispose on cleanup

### 🧪 Phase 4: Test (60 min)
- [ ] Update test suite (simpler!)
- [ ] Verify all 7 tests pass
- [ ] Add test for automatic tracking
- [ ] Performance benchmark (should be similar)

### 📝 Phase 5: Documentation (30 min)
- [ ] Update CONCERNS_REFERENCE.md
- [ ] Remove "tracks() function" section
- [ ] Add "Automatic tracking via effect()" section
- [ ] Update examples

**Total: ~2.5 hours**

---

## 🎯 Final Recommendation

### ✅ YES - Migrate to valtio-reactive

**Reasons**:
1. **50% code reduction** (~200 → ~100 lines)
2. **Automatic correctness** (can't forget dependencies)
3. **Simpler mental model** (no manual tracking)
4. **Same performance** (O(k) re-evaluations)
5. **Low migration cost** (~2.5 hours)
6. **Better DX** (less boilerplate, more focus on logic)

**The only downside** is slightly less explicit dependencies, but the correctness guarantee far outweighs this.

---

## ➡️ Next Steps

1. Install valtio-reactive
2. Create a new branch for migration
3. Implement the changes (see concerns-reactive-proposal.ts)
4. Update tests
5. Verify performance is equivalent
6. Update documentation
7. Merge!

This is a clear win for code quality and maintainability. 🚀
