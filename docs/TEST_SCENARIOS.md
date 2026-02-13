---
created: 2026-01-28 (37db2cf)
updated: 2026-02-11 (bd07f7d)
status: active
---

# Valtio-Reactive Concerns: Test Scenarios

**Document Type**: Test Specifications
**Priority**: P0 (Critical)
**Dependencies**: ARCHITECTURE_CONCERNS_CRITICAL.md, VALTIO_REACTIVE_ANALYSIS.md
**Related**: concerns-reactive-proposal.ts

---

## 🎯 Overview

These test scenarios validate the valtio-reactive concerns system meets three critical requirements:

1. **Selective Re-calculation**: Only relevant concerns recalculate when state changes
2. **Batched Updates**: All updates happen in one batch, even with multiple concerns
3. **Performance**: Quick and snappy (<16ms for 60fps, <50ms for good UX)

---

## 🧪 TEST-001: Single Field Change - Selective Re-calculation

**Priority**: P0 (Critical)
**Performance Target**: < 5ms

### 📖 User Story
As a trader, when I change a single strike price, I want only that field's validations to recalculate, not unrelated fields, so the UI stays responsive.

### 🔧 Use Case: Two-Leg Option Spread

**Setup**:
```typescript
state = {
  products: {
    'leg-1': { strike: 100, expiry: '2024-12-31', premium: null, status: 'active' },
    'leg-2': { strike: 105, expiry: '2024-12-31', premium: null, status: 'active' }
  },
  market: { spot: 102, volatility: 0.15 }
}

concerns = {
  'products.leg-1.strike': {
    validationState: { schema: z.number().min(0).max(200) },
    disabled: { condition: { IS_EQUAL: ['products.leg-1.status', 'locked'] } },
    tooltip: { template: 'Leg 1 Strike: {{products.leg-1.strike}}' }
  },
  'products.leg-2.strike': {
    validationState: { schema: z.number().min(0).max(200) },
    disabled: { condition: { IS_EQUAL: ['products.leg-2.status', 'locked'] } },
    tooltip: { template: 'Leg 2 Strike: {{products.leg-2.strike}}' }
  }
}
```

### 📝 Test Steps

1. **Instrument evaluation tracking**:
   ```typescript
   const evaluationLog = []

   // Wrap each concern's evaluate to track calls
   const trackEvaluations = (concernName, originalEvaluate) => {
     return (props) => {
       evaluationLog.push({
         concern: concernName,
         path: props.path,
         timestamp: performance.now()
       })
       return originalEvaluate(props)
     }
   }
   ```

2. **Register concerns** (initial evaluation happens)
   ```typescript
   store.useConcerns('test', concerns)
   ```

3. **Clear evaluation log**
   ```typescript
   evaluationLog = []
   ```

4. **Change leg-1 strike**
   ```typescript
   store.proxy.products['leg-1'].strike = 150
   ```

5. **Wait for effects to settle**
   ```typescript
   await new Promise(resolve => setTimeout(resolve, 100))
   ```

### ✅ Acceptance Criteria

#### AC1: Only leg-1 concerns recalculate
```typescript
const leg1Evals = evaluationLog.filter(e => e.path === 'products.leg-1.strike')
const leg2Evals = evaluationLog.filter(e => e.path === 'products.leg-2.strike')

expect(leg1Evals.length).toBe(3)  // validationState + disabled + tooltip
expect(leg2Evals.length).toBe(0)  // Should NOT recalculate
```

#### AC2: All leg-1 concerns recalculate
```typescript
const concernNames = leg1Evals.map(e => e.concern)
expect(concernNames).toContain('validationState')
expect(concernNames).toContain('disabled')
expect(concernNames).toContain('tooltip')
```

#### AC3: Correct values after recalculation
```typescript
const concerns = store.getFieldConcerns('products.leg-1.strike')
expect(concerns.validationState?.isError).toBe(false)  // Valid (no error)
expect(concerns.tooltip).toBe('Leg 1 Strike: 150')
```

### ⚡ Performance Target
- Total time for re-evaluation: **< 5ms**
- No UI jank (stays at 60fps)

---

## 🧪 TEST-002: Cross-Field Validation - Dependency Tracking

**Priority**: P0 (Critical)
**Performance Target**: < 2ms

### 📖 User Story
As a trader, when I change leg-1's status to "locked", I want only the disabled concern for leg-1 to recalculate, not leg-2's concerns or leg-1's validation.

### 🔧 Use Case: Lock One Leg of Spread

**Setup**:
```typescript
state = {
  products: {
    'leg-1': { strike: 100, status: 'active' },
    'leg-2': { strike: 105, status: 'active' }
  }
}

concerns = {
  'products.leg-1.strike': {
    validationState: { schema: z.number().min(0) },
    disabled: { condition: { IS_EQUAL: ['products.leg-1.status', 'locked'] } }
  },
  'products.leg-2.strike': {
    validationState: { schema: z.number().min(0) },
    disabled: { condition: { IS_EQUAL: ['products.leg-2.status', 'locked'] } }
  }
}
```

### 📝 Test Steps

1. Register concerns (initial evaluation)
2. Clear evaluation log
3. Change leg-1 status:
   ```typescript
   store.proxy.products['leg-1'].status = 'locked'
   ```
4. Wait for effects

### ✅ Acceptance Criteria

#### AC1: Only leg-1 disabled concern recalculates
```typescript
expect(evaluationLog).toEqual([
  { concern: 'disabled', path: 'products.leg-1.strike' }
])
```

#### AC2: Leg-1 validationState does NOT recalculate
```typescript
const validationEvals = evaluationLog.filter(e => e.concern === 'validationState')
expect(validationEvals.length).toBe(0)  // Doesn't depend on status
```

#### AC3: Leg-2 concerns do NOT recalculate
```typescript
const leg2Evals = evaluationLog.filter(e => e.path.includes('leg-2'))
expect(leg2Evals.length).toBe(0)
```

#### AC4: Correct disabled state
```typescript
const leg1Concerns = store.getFieldConcerns('products.leg-1.strike')
const leg2Concerns = store.getFieldConcerns('products.leg-2.strike')

expect(leg1Concerns.disabled).toBe(true)   // Locked
expect(leg2Concerns.disabled).toBe(false)  // Not locked
```

### ⚡ Performance Target
- Total time: **< 2ms** (single concern evaluation)

---

## 🧪 TEST-003: Batch Updates - Multiple Changes

**Priority**: P0 (Critical)
**Performance Target**: < 30ms

### 📖 User Story
As a developer, when I change multiple fields in one synchronous block, I want all concern evaluations to happen once in a single batch, not multiple times.

### 🔧 Use Case: Bulk Update from Server

**Setup**:
```typescript
state = {
  products: {
    'leg-1': { strike: 100, status: 'active' },
    'leg-2': { strike: 105, status: 'active' }
  },
  market: { spot: 102 }
}

concerns = {
  'products.leg-1.strike': {
    validationState: { schema: z.number().min(0) },
    tooltip: { template: 'Strike: {{products.leg-1.strike}} @ {{market.spot}}' }
  },
  'products.leg-2.strike': {
    validationState: { schema: z.number().min(0) },
    tooltip: { template: 'Strike: {{products.leg-2.strike}} @ {{market.spot}}' }
  }
}
```

### 📝 Test Steps

1. Register concerns
2. Track React re-renders using counter:
   ```typescript
   let renderCount = 0

   const TestComponent = () => {
     const { useFieldStore } = store.withConcerns({ tooltip: true })
     const leg1 = useFieldStore('products.leg-1.strike')
     const leg2 = useFieldStore('products.leg-2.strike')

     renderCount++

     return <div>{leg1.tooltip} / {leg2.tooltip}</div>
   }
   ```

3. Clear evaluation log and render count

4. **Synchronous bulk update**:
   ```typescript
   store.proxy.products['leg-1'].strike = 150
   store.proxy.products['leg-2'].strike = 155
   store.proxy.market.spot = 120
   ```

5. Wait for effects and re-renders

### ✅ Acceptance Criteria

#### AC1: All concerns evaluate (correctness)
```typescript
expect(evaluationLog.length).toBe(4)  // 2 concerns × 2 paths
```

#### AC2: Each concern evaluates only once
```typescript
const leg1ValidationEvals = evaluationLog.filter(
  e => e.concern === 'validationState' && e.path === 'products.leg-1.strike'
)
expect(leg1ValidationEvals.length).toBe(1)  // NOT 2 or 3!
```

#### AC3: React renders only once (batched)
```typescript
// With React 18, should be 1 render
expect(renderCount).toBe(1)
```

#### AC4: Final state is correct
```typescript
const leg1 = store.getFieldConcerns('products.leg-1.strike')
const leg2 = store.getFieldConcerns('products.leg-2.strike')

expect(leg1.tooltip).toBe('Strike: 150 @ 120')
expect(leg2.tooltip).toBe('Strike: 155 @ 120')
```

### ⚡ Performance Target
- Total evaluation time: **< 10ms**
- React render time: **< 16ms** (60fps)
- Total end-to-end: **< 30ms**

---

## 🧪 TEST-004: Same Field Multiple Changes - Deduplication

**Priority**: P1 (High)
**Performance Target**: < 100ms

### 📖 User Story
As a developer, when the same field changes multiple times in a loop (e.g., animation, slider), I want the concern to evaluate efficiently without wasted cycles.

### 🔧 Use Case: Strike Price Slider

**Setup**:
```typescript
state = {
  products: {
    'leg-1': { strike: 100 }
  }
}

concerns = {
  'products.leg-1.strike': {
    validationState: { schema: z.number().min(0).max(200) },
    tooltip: { template: 'Strike: {{products.leg-1.strike}}' }
  }
}
```

### 📝 Test Steps

1. Register concerns
2. Clear evaluation log
3. **Rapid updates in loop**:
   ```typescript
   for (let i = 0; i < 100; i++) {
     store.proxy.products['leg-1'].strike = 100 + i
   }
   ```

4. Wait for effects to settle

### ✅ Acceptance Criteria

#### AC1: Concerns evaluate efficiently (not 100 times)
```typescript
// With valtio-reactive batching, should be much less than 100
// Exact count depends on effect() implementation, but should be << 100
expect(evaluationLog.length).toBeLessThan(20)
```

#### AC2: Final value is correct (last update wins)
```typescript
const concerns = store.getFieldConcerns('products.leg-1.strike')
expect(concerns.tooltip).toBe('Strike: 199')
```

#### AC3: Validation reflects final state
```typescript
expect(concerns.validationState?.isError).toBe(false)  // 199 is valid (no error)
```

### ⚡ Performance Target
- Total time for 100 updates: **< 100ms** (< 1ms per update amortized)
- UI stays responsive (no jank)

---

## 🧪 TEST-005: Complex Dependencies - Multi-Level Tracking

**Priority**: P1 (High)
**Performance Target**: < 20ms

### 📖 User Story
As a trader, when market data changes, I want all pricing-dependent fields to recalculate, but not unrelated validations.

### 🔧 Use Case: Premium Calculation with Market Data

**Setup**:
```typescript
state = {
  products: {
    'leg-1': { strike: 100, expiry: '2024-12-31', premium: null },
    'leg-2': { strike: 105, expiry: '2024-12-31', premium: null }
  },
  market: { spot: 102, volatility: 0.15, rates: 0.05 },
  ui: { showAdvanced: false }
}

concerns = {
  'products.leg-1.strike': {
    validationState: { schema: z.number().min(0) }
  },
  'products.leg-1.premium': {
    computed: {
      evaluate: (props) => {
        // Depends on: strike, expiry, market.spot, market.volatility, market.rates
        const { strike, expiry } = props.state.products['leg-1']
        const { spot, volatility, rates } = props.state.market
        return calculatePremium({ strike, expiry, spot, volatility, rates })
      }
    }
  },
  'products.leg-2.premium': {
    computed: {
      evaluate: (props) => {
        const { strike, expiry } = props.state.products['leg-2']
        const { spot, volatility, rates } = props.state.market
        return calculatePremium({ strike, expiry, spot, volatility, rates })
      }
    }
  }
}
```

### 📝 Test Steps

1. Register concerns
2. Clear evaluation log
3. **Change market volatility**:
   ```typescript
   store.proxy.market.volatility = 0.20
   ```
4. Wait for effects

### ✅ Acceptance Criteria

#### AC1: Both premium computations recalculate
```typescript
const premiumEvals = evaluationLog.filter(e => e.concern === 'computed')
expect(premiumEvals.length).toBe(2)  // leg-1 and leg-2 premiums
```

#### AC2: Strike validation does NOT recalculate
```typescript
const validationEvals = evaluationLog.filter(e => e.concern === 'validationState')
expect(validationEvals.length).toBe(0)  // Doesn't depend on market
```

#### AC3: Unrelated UI state doesn't trigger recalc
```typescript
evaluationLog = []
store.proxy.ui.showAdvanced = true
await waitForEffects()

expect(evaluationLog.length).toBe(0)  // Nothing depends on UI
```

#### AC4: Premiums updated correctly
```typescript
const leg1Premium = store.getFieldConcerns('products.leg-1.premium')
const leg2Premium = store.getFieldConcerns('products.leg-2.premium')

expect(leg1Premium.computed).toBeGreaterThan(0)  // Calculated
expect(leg2Premium.computed).toBeGreaterThan(0)  // Calculated
expect(leg2Premium.computed).toBeGreaterThan(leg1Premium.computed)  // Higher strike = higher premium
```

### ⚡ Performance Target
- Premium calculation time: **< 20ms** (both legs)
- UI stays responsive during calculation

---

## 🧪 TEST-006: Aggregation Pattern - Sum of All Legs

**Priority**: P2 (Medium)
**Performance Target**: < 5ms

### 📖 User Story
As a trader, when I change any leg's notional, I want the total notional to update automatically, but only when individual notionals change.

### 🔧 Use Case: Strategy-Level Aggregation

**Setup**:
```typescript
state = {
  products: {
    'leg-1': { notional: 1000000 },
    'leg-2': { notional: 1500000 },
    'leg-3': { notional: 2000000 }
  },
  aggregated: {
    totalNotional: null
  }
}

concerns = {
  'aggregated.totalNotional': {
    computed: {
      evaluate: (props) => {
        const { products } = props.state
        return Object.values(products).reduce((sum, leg) => sum + leg.notional, 0)
      }
    }
  }
}
```

### 📝 Test Steps

1. Register concerns
2. Verify initial calculation:
   ```typescript
   const initial = store.getFieldConcerns('aggregated.totalNotional')
   expect(initial.computed).toBe(4500000)
   ```

3. Clear evaluation log

4. **Change single leg notional**:
   ```typescript
   store.proxy.products['leg-1'].notional = 2000000
   ```

5. Wait for effects

### ✅ Acceptance Criteria

#### AC1: Aggregation recalculates
```typescript
const aggregationEvals = evaluationLog.filter(e => e.path === 'aggregated.totalNotional')
expect(aggregationEvals.length).toBe(1)
```

#### AC2: Correct sum
```typescript
const concerns = store.getFieldConcerns('aggregated.totalNotional')
expect(concerns.computed).toBe(5500000)  // 2M + 1.5M + 2M
```

#### AC3: Multiple changes in batch
```typescript
evaluationLog = []

store.proxy.products['leg-1'].notional = 1000000
store.proxy.products['leg-2'].notional = 1000000
store.proxy.products['leg-3'].notional = 1000000

await waitForEffects()

// Should evaluate once (batched), not 3 times
expect(evaluationLog.length).toBe(1)

const concerns = store.getFieldConcerns('aggregated.totalNotional')
expect(concerns.computed).toBe(3000000)
```

### ⚡ Performance Target
- Aggregation calculation: **< 5ms**
- Works efficiently with 10+ legs

---

## 🧪 TEST-007: React Integration - useSnapshot Batching

**Priority**: P0 (Critical)
**Performance Target**: < 16ms

### 📖 User Story
As a user, when multiple fields change, I want the UI to update once smoothly, not flash or stutter with intermediate states.

### 🔧 Use Case: Complete Trade Entry Form

**Setup**:
```typescript
state = {
  products: {
    'leg-1': { strike: 100, notional: 1000000, status: 'active' }
  },
  market: { spot: 102 }
}

const TradeForm = () => {
  const strikeValue = useSnapshot(store.proxy).products['leg-1'].strike
  const { useFieldStore } = store.withConcerns({ validationState: true })
  const { validationState } = useFieldStore('products.leg-1.strike')

  renderLog.push({
    timestamp: performance.now(),
    strike: strikeValue,
    valid: validationState
  })

  return (
    <input
      value={strikeValue}
      disabled={strikeConcerns.disabled}
      className={strikeConcerns.validationState ? '' : 'error'}
    />
  )
}
```

### 📝 Test Steps

1. Render component
2. Clear render log
3. **Bulk update**:
   ```typescript
   store.proxy.products['leg-1'].strike = 150
   store.proxy.products['leg-1'].notional = 2000000
   store.proxy.products['leg-1'].status = 'locked'
   ```
4. Wait for re-renders

### ✅ Acceptance Criteria

#### AC1: Single re-render (React 18 batching)
```typescript
expect(renderLog.length).toBe(1)
```

#### AC2: No intermediate states visible
```typescript
// Should only see final state, not intermediate values
expect(renderLog[0].strike).toBe(150)
```

#### AC3: Concerns reflect final state
```typescript
expect(renderLog[0].valid).toBe(true)  // Validation passed for 150
```

### ⚡ Performance Target
- Render time: **< 16ms** (60fps)
- No flashing or visual glitches

---

## 🧪 TEST-008: Performance Stress Test

**Priority**: P2 (Medium)
**Performance Target**: < 50ms

### 📖 User Story
As a developer, I want to ensure the concerns system scales to complex multi-leg strategies with many fields.

### 🔧 Use Case: 10-Leg Butterfly Strategy

**Setup**:
```typescript
state = {
  products: {
    'leg-1': { strike: 90, notional: 1000000 },
    'leg-2': { strike: 95, notional: 1000000 },
    'leg-3': { strike: 100, notional: 1000000 },
    'leg-4': { strike: 105, notional: 1000000 },
    'leg-5': { strike: 110, notional: 1000000 },
    'leg-6': { strike: 115, notional: 1000000 },
    'leg-7': { strike: 120, notional: 1000000 },
    'leg-8': { strike: 125, notional: 1000000 },
    'leg-9': { strike: 130, notional: 1000000 },
    'leg-10': { strike: 135, notional: 1000000 }
  },
  market: { spot: 102, volatility: 0.15 }
}

// Each leg has 4 concerns: validation, disabled, tooltip, computed premium
// Total: 40 concerns
```

### 📝 Test Steps

1. Register all 40 concerns
2. Measure initial evaluation time
3. **Bulk update scenario**:
   ```typescript
   const startTime = performance.now()

   store.proxy.market.spot = 110
   store.proxy.market.volatility = 0.20

   await waitForEffects()

   const duration = performance.now() - startTime
   ```

### ✅ Acceptance Criteria

#### AC1: Initial registration completes quickly
```typescript
expect(registrationTime).toBeLessThan(100)  // < 100ms
```

#### AC2: Bulk update completes in reasonable time
```typescript
expect(duration).toBeLessThan(50)  // < 50ms (good UX threshold)
```

#### AC3: Only relevant concerns recalculate
```typescript
// Market change should trigger only computed premiums (10 concerns)
// Not validations or tooltips (unless they depend on market)
const computedEvals = evaluationLog.filter(e => e.concern === 'computed')
expect(computedEvals.length).toBe(10)
```

#### AC4: Memory stays reasonable
```typescript
const memoryUsage = performance.memory?.usedJSHeapSize || 0
expect(memoryUsage).toBeLessThan(50 * 1024 * 1024)  // < 50MB
```

### ⚡ Performance Targets
- Initial registration: **< 100ms**
- Bulk update: **< 50ms**
- Memory usage: **< 50MB**
- No memory leaks after cleanup

---

## 🔧 Performance Benchmarking Harness

```typescript
import { performance } from 'perf_hooks'

class PerformanceBenchmark {
  private marks: Map<string, number> = new Map()
  private measurements: Array<{ name: string; duration: number }> = []

  start(label: string) {
    this.marks.set(label, performance.now())
  }

  end(label: string): number {
    const startTime = this.marks.get(label)
    if (!startTime) throw new Error(`No start mark for ${label}`)

    const duration = performance.now() - startTime
    this.measurements.push({ name: label, duration })
    this.marks.delete(label)

    return duration
  }

  report() {
    console.table(this.measurements)

    const summary = {
      count: this.measurements.length,
      total: this.measurements.reduce((sum, m) => sum + m.duration, 0),
      average: this.measurements.reduce((sum, m) => sum + m.duration, 0) / this.measurements.length,
      max: Math.max(...this.measurements.map(m => m.duration)),
      min: Math.min(...this.measurements.map(m => m.duration))
    }

    console.log('Summary:', summary)
    return summary
  }

  clear() {
    this.marks.clear()
    this.measurements = []
  }
}

// Usage in tests
const benchmark = new PerformanceBenchmark()

test('TEST-001: Selective re-calculation', async () => {
  benchmark.start('registration')
  store.useConcerns('test', concerns)
  benchmark.end('registration')

  benchmark.start('single-field-update')
  store.proxy.products['leg-1'].strike = 150
  await waitForEffects()
  benchmark.end('single-field-update')

  const report = benchmark.report()

  expect(report.measurements.find(m => m.name === 'registration')!.duration).toBeLessThan(10)
  expect(report.measurements.find(m => m.name === 'single-field-update')!.duration).toBeLessThan(5)
})
```

---

## 📊 Summary Table

| Test ID | Focus Area | Priority | Target Time | Key Metric |
|---------|-----------|----------|-------------|------------|
| TEST-001 | Selective re-calc | P0 | < 5ms | Only affected concerns run |
| TEST-002 | Cross-field deps | P0 | < 2ms | Correct dependency tracking |
| TEST-003 | Batch updates | P0 | < 30ms | Single React render |
| TEST-004 | Deduplication | P1 | < 100ms | << 100 evaluations for 100 changes |
| TEST-005 | Complex deps | P1 | < 20ms | Multi-level tracking works |
| TEST-006 | Aggregation | P2 | < 5ms | Batched aggregation |
| TEST-007 | React integration | P0 | < 16ms | 60fps, no flashing |
| TEST-008 | Performance stress | P2 | < 50ms | Scales to 10+ legs |

---

## 🧪 Automated Test Suite Structure

```typescript
describe('Valtio-Reactive Concerns System', () => {
  describe('Selective Re-calculation (TEST-001, TEST-002)', () => {
    it('should only recalculate affected concerns when single field changes')
    it('should track cross-field dependencies correctly')
    it('should not recalculate unrelated concerns')
  })

  describe('Batched Updates (TEST-003, TEST-004)', () => {
    it('should batch multiple synchronous changes')
    it('should deduplicate rapid same-field updates')
    it('should trigger single React re-render for batched changes')
  })

  describe('Complex Dependencies (TEST-005, TEST-006)', () => {
    it('should track multi-level dependencies')
    it('should handle aggregation patterns efficiently')
    it('should ignore unrelated state changes')
  })

  describe('React Integration (TEST-007)', () => {
    it('should batch React re-renders with useSnapshot')
    it('should not show intermediate states')
    it('should maintain 60fps during updates')
  })

  describe('Performance & Scale (TEST-008)', () => {
    it('should handle 10-leg strategies efficiently')
    it('should complete bulk updates in < 50ms')
    it('should not leak memory')
  })
})
```

---

## ✅ Definition of Success

The valtio-reactive approach is validated if:

1. ✅ **All P0 tests pass** with performance targets met
2. ✅ **At least 75% of P1 tests pass** on first implementation
3. ✅ **No performance regressions** vs current manual tracking approach
4. ✅ **Code is simpler** (~50% reduction verified)
5. ✅ **Developer experience improved** (easier to write concerns, less error-prone)

If any P0 test fails or shows >2x performance degradation, we reconsider the approach.
