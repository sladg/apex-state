/**
 * No-Op Change Filtering Benchmark
 *
 * Tests whether filtering out no-op changes (where value === currentValue)
 * is faster than just applying all changes.
 *
 * Trade-offs:
 * - Cost: _get() + equality check for every change
 * - Benefit: Skip unnecessary _set() calls and potential re-renders
 *
 * Scenarios:
 * 1. High no-op rate (80% redundant updates)
 * 2. Low no-op rate (20% redundant updates)
 * 3. All real changes (0% redundant)
 * 4. Deep paths (expensive _get lookup)
 * 5. Mixed real-world scenario
 *
 * Run: node benchmarks/no-op-filtering.js
 */

import _get from 'lodash/get.js'
import _set from 'lodash/set.js'
import { proxy } from 'valtio/vanilla'

// ============================================================================
// Implementation Approaches
// ============================================================================

// Current: Apply all changes without checking
const applyBatchDirect = (changes, state) => {
  for (const [path, value] of changes) {
    _set(state, path, value)
  }
}

// Proposed: Check before applying
const applyBatchFiltered = (changes, state) => {
  for (const [path, value] of changes) {
    const current = _get(state, path)
    if (current !== value) {
      _set(state, path, value)
    }
  }
}

// Alternative: Pre-filter the changes array
const filterNoOpChanges = (changes, state) => {
  return changes.filter(([path, value]) => {
    const current = _get(state, path)
    return current !== value
  })
}

const applyBatchPreFiltered = (changes, state) => {
  const filtered = filterNoOpChanges(changes, state)
  for (const [path, value] of filtered) {
    _set(state, path, value)
  }
}

// ============================================================================
// Benchmark Helpers
// ============================================================================

const benchmark = (fn, iterations = 1000) => {
  const start = performance.now()
  for (let i = 0; i < iterations; i++) {
    fn()
  }
  return (performance.now() - start) / iterations
}

// ============================================================================
// Test Data Setup
// ============================================================================

const createInitialState = () => ({
  groups: {
    'fx-options': {
      products: {
        'EURUSD-1M': {
          pricing: {
            greeks: {
              delta: 0.5,
              gamma: 0.03,
              vega: 800,
              theta: -50,
              rho: 30,
            },
            spot: 1.08,
            vol: 0.12,
          },
          trading: {
            enabled: true,
            notional: 1000000,
          },
        },
        'GBPUSD-1M': {
          pricing: {
            greeks: {
              delta: 0.45,
              gamma: 0.025,
              vega: 750,
              theta: -45,
              rho: 28,
            },
            spot: 1.27,
            vol: 0.11,
          },
          trading: {
            enabled: true,
            notional: 800000,
          },
        },
        'USDJPY-1M': {
          pricing: {
            greeks: {
              delta: 0.55,
              gamma: 0.035,
              vega: 850,
              theta: -55,
              rho: 32,
            },
            spot: 149.5,
            vol: 0.13,
          },
          trading: {
            enabled: false,
            notional: 1200000,
          },
        },
      },
      config: {
        ccyPair: 'EURUSD',
        notionalCcy: 'USD',
      },
    },
    'ir-swaps': {
      products: {
        'USD-5Y': {
          pricing: {
            rate: 0.045,
            spread: 0.002,
          },
          trading: {
            enabled: true,
            notional: 5000000,
          },
        },
      },
    },
  },
  enterprise: {
    risk: {
      limits: {
        delta: 100000,
        vega: 500000,
      },
    },
  },
})

console.log('=== NO-OP CHANGE FILTERING BENCHMARK ===\n')

// ============================================================================
// SCENARIO 1: High No-Op Rate (80% redundant)
// ============================================================================

console.log('SCENARIO 1: High No-Op Rate (80% redundant updates)')
console.log('  20 changes, 16 are no-ops (setting same value)')
console.log()

const state1Direct = proxy(createInitialState())
const state1Filtered = proxy(createInitialState())
const state1PreFiltered = proxy(createInitialState())

// 80% of changes set the same value
const changes1 = [
  // Real changes (20%)
  ['groups.fx-options.products.EURUSD-1M.pricing.greeks.delta', 0.52],
  ['groups.fx-options.products.GBPUSD-1M.pricing.spot', 1.28],
  ['groups.ir-swaps.products.USD-5Y.pricing.rate', 0.046],
  ['enterprise.risk.limits.delta', 110000],

  // No-op changes (80%) - setting existing values
  ['groups.fx-options.products.EURUSD-1M.pricing.greeks.gamma', 0.03],
  ['groups.fx-options.products.EURUSD-1M.pricing.greeks.vega', 800],
  ['groups.fx-options.products.EURUSD-1M.pricing.greeks.theta', -50],
  ['groups.fx-options.products.EURUSD-1M.pricing.greeks.rho', 30],
  ['groups.fx-options.products.EURUSD-1M.pricing.spot', 1.08],
  ['groups.fx-options.products.EURUSD-1M.pricing.vol', 0.12],
  ['groups.fx-options.products.EURUSD-1M.trading.enabled', true],
  ['groups.fx-options.products.EURUSD-1M.trading.notional', 1000000],
  ['groups.fx-options.products.GBPUSD-1M.pricing.greeks.delta', 0.45],
  ['groups.fx-options.products.GBPUSD-1M.pricing.greeks.gamma', 0.025],
  ['groups.fx-options.products.GBPUSD-1M.pricing.vol', 0.11],
  ['groups.fx-options.products.USDJPY-1M.pricing.greeks.delta', 0.55],
  ['groups.fx-options.products.USDJPY-1M.trading.enabled', false],
  ['groups.ir-swaps.products.USD-5Y.pricing.spread', 0.002],
  ['groups.ir-swaps.products.USD-5Y.trading.enabled', true],
  ['enterprise.risk.limits.vega', 500000],
]

const time1Direct = benchmark(() => {
  applyBatchDirect(changes1, state1Direct)
})

const time1Filtered = benchmark(() => {
  applyBatchFiltered(changes1, state1Filtered)
})

const time1PreFiltered = benchmark(() => {
  applyBatchPreFiltered(changes1, state1PreFiltered)
})

console.log(`  Direct (no check):     ${time1Direct.toFixed(4)}ms`)
console.log(`  Inline filter:         ${time1Filtered.toFixed(4)}ms`)
console.log(`  Pre-filter:            ${time1PreFiltered.toFixed(4)}ms`)
console.log(
  `  Best approach:         ${time1Filtered < time1Direct ? 'INLINE FILTER' : 'DIRECT'} (${((1 - Math.min(time1Filtered, time1PreFiltered) / time1Direct) * 100).toFixed(1)}% improvement)`,
)
console.log(
  `  Savings vs direct:     ${((time1Direct - Math.min(time1Filtered, time1PreFiltered)) * 1000).toFixed(1)}μs`,
)
console.log()

// ============================================================================
// SCENARIO 2: Low No-Op Rate (20% redundant)
// ============================================================================

console.log('SCENARIO 2: Low No-Op Rate (20% redundant updates)')
console.log('  20 changes, 4 are no-ops')
console.log()

const state2Direct = proxy(createInitialState())
const state2Filtered = proxy(createInitialState())
const state2PreFiltered = proxy(createInitialState())

// 80% real changes, 20% no-ops
const changes2 = [
  // Real changes (80%)
  ['groups.fx-options.products.EURUSD-1M.pricing.greeks.delta', 0.52],
  ['groups.fx-options.products.EURUSD-1M.pricing.greeks.gamma', 0.032],
  ['groups.fx-options.products.EURUSD-1M.pricing.greeks.vega', 820],
  ['groups.fx-options.products.EURUSD-1M.pricing.greeks.theta', -52],
  ['groups.fx-options.products.EURUSD-1M.pricing.spot', 1.09],
  ['groups.fx-options.products.GBPUSD-1M.pricing.greeks.delta', 0.47],
  ['groups.fx-options.products.GBPUSD-1M.pricing.spot', 1.28],
  ['groups.fx-options.products.USDJPY-1M.pricing.greeks.delta', 0.57],
  ['groups.fx-options.products.USDJPY-1M.pricing.spot', 150.2],
  ['groups.ir-swaps.products.USD-5Y.pricing.rate', 0.046],
  ['groups.ir-swaps.products.USD-5Y.pricing.spread', 0.0022],
  ['enterprise.risk.limits.delta', 110000],
  ['enterprise.risk.limits.vega', 520000],
  ['groups.fx-options.config.ccyPair', 'EURGBP'],
  ['groups.fx-options.products.EURUSD-1M.trading.notional', 1100000],
  ['groups.fx-options.products.GBPUSD-1M.trading.notional', 850000],

  // No-ops (20%)
  ['groups.fx-options.products.EURUSD-1M.pricing.greeks.rho', 30],
  ['groups.fx-options.products.EURUSD-1M.trading.enabled', true],
  ['groups.fx-options.products.USDJPY-1M.trading.enabled', false],
  ['groups.ir-swaps.products.USD-5Y.trading.enabled', true],
]

const time2Direct = benchmark(() => {
  applyBatchDirect(changes2, state2Direct)
})

const time2Filtered = benchmark(() => {
  applyBatchFiltered(changes2, state2Filtered)
})

const time2PreFiltered = benchmark(() => {
  applyBatchPreFiltered(changes2, state2PreFiltered)
})

console.log(`  Direct (no check):     ${time2Direct.toFixed(4)}ms`)
console.log(`  Inline filter:         ${time2Filtered.toFixed(4)}ms`)
console.log(`  Pre-filter:            ${time2PreFiltered.toFixed(4)}ms`)
console.log(
  `  Winner:                ${time2Filtered < time2Direct ? 'INLINE FILTER' : 'DIRECT'} (${time2Filtered < time2Direct ? ((1 - time2Filtered / time2Direct) * 100).toFixed(1) + '% improvement' : ((time2Filtered / time2Direct - 1) * 100).toFixed(1) + '% overhead'})`,
)
console.log(
  `  Diff vs direct:        ${((time2Direct - time2Filtered) * 1000).toFixed(1)}μs`,
)
console.log()

// ============================================================================
// SCENARIO 3: All Real Changes (0% redundant)
// ============================================================================

console.log('SCENARIO 3: All Real Changes (0% redundant)')
console.log('  20 changes, all modify values')
console.log()

const state3Direct = proxy(createInitialState())
const state3Filtered = proxy(createInitialState())
const state3PreFiltered = proxy(createInitialState())

// All changes modify values
const changes3 = [
  ['groups.fx-options.products.EURUSD-1M.pricing.greeks.delta', 0.52],
  ['groups.fx-options.products.EURUSD-1M.pricing.greeks.gamma', 0.032],
  ['groups.fx-options.products.EURUSD-1M.pricing.greeks.vega', 820],
  ['groups.fx-options.products.EURUSD-1M.pricing.greeks.theta', -52],
  ['groups.fx-options.products.EURUSD-1M.pricing.greeks.rho', 32],
  ['groups.fx-options.products.EURUSD-1M.pricing.spot', 1.09],
  ['groups.fx-options.products.EURUSD-1M.pricing.vol', 0.13],
  ['groups.fx-options.products.GBPUSD-1M.pricing.greeks.delta', 0.47],
  ['groups.fx-options.products.GBPUSD-1M.pricing.greeks.gamma', 0.027],
  ['groups.fx-options.products.GBPUSD-1M.pricing.spot', 1.28],
  ['groups.fx-options.products.USDJPY-1M.pricing.greeks.delta', 0.57],
  ['groups.fx-options.products.USDJPY-1M.pricing.spot', 150.2],
  ['groups.ir-swaps.products.USD-5Y.pricing.rate', 0.046],
  ['groups.ir-swaps.products.USD-5Y.pricing.spread', 0.0022],
  ['enterprise.risk.limits.delta', 110000],
  ['enterprise.risk.limits.vega', 520000],
  ['groups.fx-options.config.ccyPair', 'EURGBP'],
  ['groups.fx-options.config.notionalCcy', 'GBP'],
  ['groups.fx-options.products.EURUSD-1M.trading.notional', 1100000],
  ['groups.fx-options.products.GBPUSD-1M.trading.notional', 850000],
]

const time3Direct = benchmark(() => {
  applyBatchDirect(changes3, state3Direct)
})

const time3Filtered = benchmark(() => {
  applyBatchFiltered(changes3, state3Filtered)
})

const time3PreFiltered = benchmark(() => {
  applyBatchPreFiltered(changes3, state3PreFiltered)
})

console.log(`  Direct (no check):     ${time3Direct.toFixed(4)}ms`)
console.log(`  Inline filter:         ${time3Filtered.toFixed(4)}ms`)
console.log(`  Pre-filter:            ${time3PreFiltered.toFixed(4)}ms`)
console.log(
  `  Winner:                ${time3Filtered < time3Direct ? 'INLINE FILTER' : 'DIRECT'} (${time3Filtered < time3Direct ? ((1 - time3Filtered / time3Direct) * 100).toFixed(1) + '% improvement' : ((time3Filtered / time3Direct - 1) * 100).toFixed(1) + '% overhead'})`,
)
console.log(
  `  Diff vs direct:        ${((time3Direct - time3Filtered) * 1000).toFixed(1)}μs`,
)
console.log()

// ============================================================================
// SCENARIO 4: Deep Paths (expensive _get lookup)
// ============================================================================

console.log('SCENARIO 4: Very Deep Paths (expensive lookups)')
console.log('  Testing with 8-10 level deep paths')
console.log()

const deepState = proxy({
  level1: {
    level2: {
      level3: {
        level4: {
          level5: {
            level6: {
              level7: {
                level8: {
                  level9: {
                    level10: {
                      value: 100,
                    },
                  },
                },
              },
            },
          },
        },
      },
    },
  },
})

const state4Direct = deepState
const state4Filtered = deepState

// Mix of deep paths with 50% no-ops
const changes4 = [
  // No-ops
  [
    'level1.level2.level3.level4.level5.level6.level7.level8.level9.level10.value',
    100,
  ],
  [
    'level1.level2.level3.level4.level5.level6.level7.level8.level9.level10.value',
    100,
  ],
  [
    'level1.level2.level3.level4.level5.level6.level7.level8.level9.level10.value',
    100,
  ],

  // Real changes
  [
    'level1.level2.level3.level4.level5.level6.level7.level8.level9.level10.value',
    101,
  ],
  [
    'level1.level2.level3.level4.level5.level6.level7.level8.level9.level10.value',
    102,
  ],
  [
    'level1.level2.level3.level4.level5.level6.level7.level8.level9.level10.value',
    103,
  ],
]

const time4Direct = benchmark(() => {
  applyBatchDirect(changes4, state4Direct)
}, 5000)

const time4Filtered = benchmark(() => {
  applyBatchFiltered(changes4, state4Filtered)
}, 5000)

console.log(`  Direct (no check):     ${time4Direct.toFixed(4)}ms`)
console.log(`  Inline filter:         ${time4Filtered.toFixed(4)}ms`)
console.log(
  `  Winner:                ${time4Filtered < time4Direct ? 'INLINE FILTER' : 'DIRECT'} (${time4Filtered < time4Direct ? ((1 - time4Filtered / time4Direct) * 100).toFixed(1) + '% improvement' : ((time4Filtered / time4Direct - 1) * 100).toFixed(1) + '% overhead'})`,
)
console.log(
  `  Diff vs direct:        ${((time4Direct - time4Filtered) * 1000).toFixed(1)}μs`,
)
console.log()

// ============================================================================
// SCENARIO 5: Listener Returns (realistic use case)
// ============================================================================

console.log('SCENARIO 5: Listener-Generated Changes')
console.log('  Simulates listener returning changes, some redundant')
console.log()

const state5Direct = proxy(createInitialState())
const state5Filtered = proxy(createInitialState())

// Listener might return changes that don't actually change anything
// e.g., conditional logic that sometimes returns same value
const listenerChanges = [
  // Aggregation results (may or may not change)
  ['groups.fx-options.products.EURUSD-1M.computed.totalGreeks', 1383], // no-op
  ['groups.fx-options.products.GBPUSD-1M.computed.totalGreeks', 1200], // changed
  ['groups.fx-options.products.USDJPY-1M.computed.totalGreeks', 1437], // no-op

  // Validation results (often no-op when already valid)
  ['groups.fx-options.products.EURUSD-1M.validation.isValid', true], // no-op
  ['groups.fx-options.products.GBPUSD-1M.validation.isValid', true], // no-op
  ['groups.fx-options.products.USDJPY-1M.validation.isValid', false], // changed

  // Derived state (conditional updates)
  ['groups.fx-options.config.activeProducts', 2], // changed
  ['enterprise.risk.breached', false], // no-op
]

// Initialize computed fields
state5Direct.groups['fx-options'].products['EURUSD-1M'].computed = {
  totalGreeks: 1383,
}
state5Direct.groups['fx-options'].products['GBPUSD-1M'].computed = {
  totalGreeks: 1148,
}
state5Direct.groups['fx-options'].products['USDJPY-1M'].computed = {
  totalGreeks: 1437,
}
state5Direct.groups['fx-options'].products['EURUSD-1M'].validation = {
  isValid: true,
}
state5Direct.groups['fx-options'].products['GBPUSD-1M'].validation = {
  isValid: true,
}
state5Direct.groups['fx-options'].products['USDJPY-1M'].validation = {
  isValid: true,
}
state5Direct.groups['fx-options'].config.activeProducts = 2
state5Direct.enterprise.risk.breached = false

state5Filtered.groups['fx-options'].products['EURUSD-1M'].computed = {
  totalGreeks: 1383,
}
state5Filtered.groups['fx-options'].products['GBPUSD-1M'].computed = {
  totalGreeks: 1148,
}
state5Filtered.groups['fx-options'].products['USDJPY-1M'].computed = {
  totalGreeks: 1437,
}
state5Filtered.groups['fx-options'].products['EURUSD-1M'].validation = {
  isValid: true,
}
state5Filtered.groups['fx-options'].products['GBPUSD-1M'].validation = {
  isValid: true,
}
state5Filtered.groups['fx-options'].products['USDJPY-1M'].validation = {
  isValid: true,
}
state5Filtered.groups['fx-options'].config.activeProducts = 2
state5Filtered.enterprise.risk.breached = false

const time5Direct = benchmark(() => {
  applyBatchDirect(listenerChanges, state5Direct)
})

const time5Filtered = benchmark(() => {
  applyBatchFiltered(listenerChanges, state5Filtered)
})

console.log(`  Direct (no check):     ${time5Direct.toFixed(4)}ms`)
console.log(`  Inline filter:         ${time5Filtered.toFixed(4)}ms`)
console.log(
  `  Winner:                ${time5Filtered < time5Direct ? 'INLINE FILTER' : 'DIRECT'} (${time5Filtered < time5Direct ? ((1 - time5Filtered / time5Direct) * 100).toFixed(1) + '% improvement' : ((time5Filtered / time5Direct - 1) * 100).toFixed(1) + '% overhead'})`,
)
console.log(
  `  Diff vs direct:        ${((time5Direct - time5Filtered) * 1000).toFixed(1)}μs`,
)
console.log()

// ============================================================================
// Summary Table
// ============================================================================

console.log('=== SUMMARY TABLE ===')
console.log(
  '┌────────────────────────────────┬──────────────┬───────────────┬──────────────┬──────────┐',
)
console.log(
  '│ Scenario                       │ Direct       │ Inline Filter │ Winner       │ Impact   │',
)
console.log(
  '├────────────────────────────────┼──────────────┼───────────────┼──────────────┼──────────┤',
)

const scenarios = [
  {
    name: 'High no-op rate (80%)',
    direct: time1Direct,
    filtered: time1Filtered,
  },
  {
    name: 'Low no-op rate (20%)',
    direct: time2Direct,
    filtered: time2Filtered,
  },
  {
    name: 'All real changes (0%)',
    direct: time3Direct,
    filtered: time3Filtered,
  },
  { name: 'Deep paths', direct: time4Direct, filtered: time4Filtered },
  {
    name: 'Listener-generated',
    direct: time5Direct,
    filtered: time5Filtered,
  },
]

scenarios.forEach((scenario) => {
  const winner =
    scenario.filtered < scenario.direct ? 'Filter      ' : 'Direct      '
  const impact =
    scenario.filtered < scenario.direct
      ? `+${((1 - scenario.filtered / scenario.direct) * 100).toFixed(0)}%`
      : `-${((scenario.filtered / scenario.direct - 1) * 100).toFixed(0)}%`
  console.log(
    `│ ${scenario.name.padEnd(30)} │ ${scenario.direct.toFixed(4)}ms    │ ${scenario.filtered.toFixed(4)}ms     │ ${winner} │ ${impact.padEnd(8)} │`,
  )
})

console.log(
  '└────────────────────────────────┴──────────────┴───────────────┴──────────────┴──────────┘',
)
console.log()

// ============================================================================
// Analysis & Recommendation
// ============================================================================

console.log('=== ANALYSIS ===')
console.log()

let filterWins = 0
let directWins = 0

scenarios.forEach((s) => {
  if (s.filtered < s.direct) {
    filterWins++
  } else {
    directWins++
  }
})

console.log(`Filter wins:  ${filterWins}/${scenarios.length} scenarios`)
console.log(`Direct wins:  ${directWins}/${scenarios.length} scenarios`)
console.log()

const avgDirect =
  scenarios.reduce((sum, s) => sum + s.direct, 0) / scenarios.length
const avgFiltered =
  scenarios.reduce((sum, s) => sum + s.filtered, 0) / scenarios.length

console.log('Key findings:')
console.log()
console.log('1. Trade-off depends on no-op rate and path depth')
console.log(
  `2. Average overhead: ${((avgFiltered / avgDirect - 1) * 100).toFixed(1)}%`,
)
console.log(
  `3. Best case (high no-ops): ${((1 - time1Filtered / time1Direct) * 100).toFixed(1)}% faster`,
)
console.log(
  `4. Worst case (all real): ${((time3Filtered / time3Direct - 1) * 100).toFixed(1)}% slower`,
)
console.log()

console.log('=== RECOMMENDATION ===')
console.log()

if (filterWins > directWins) {
  console.log(
    '✅ IMPLEMENT no-op filtering when no-op rate is typically high (>30%)',
  )
  console.log('   Benefits:')
  console.log('   - Reduces unnecessary state updates')
  console.log('   - May reduce re-renders in React/Vue')
  console.log('   - Prevents infinite loops from redundant changes')
  console.log()
  console.log('   Cost:')
  console.log('   - _get() + equality check overhead on all changes')
  console.log('   - Only beneficial when no-op rate is high')
} else {
  console.log('❌ DO NOT implement no-op filtering for all changes')
  console.log()
  console.log('   Instead, consider:')
  console.log(
    '   1. SELECTIVE filtering: Only check listener-generated changes',
  )
  console.log(
    '   2. BATCHING: Check at queue level before processing entire batch',
  )
  console.log(
    '   3. LISTENER responsibility: Listeners should avoid returning no-ops',
  )
  console.log()
  console.log('   The overhead is not worth it for typical scenarios')
}
