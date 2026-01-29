/**
 * Listener Filtering Performance Benchmark
 *
 * Tests whether ListenerConfig filtering is faster than calling functions directly
 * and letting them return early.
 *
 * Scenarios:
 * 1. Simple path matching (HAS_PATHS)
 * 2. Metadata matching (HAS_META_OF)
 * 3. Complex boolean combinators (AND/OR/NOT)
 * 4. Mixed batch with many listeners
 *
 * Run: node benchmarks/listener-filtering.js
 */

// ============================================================================
// Filter Evaluation (duplicated from src/utils/listenerFilter.ts)
// ============================================================================

const evaluateListenerFilter = (rule, changePath, changeMeta) => {
  // Check if path matches
  if ('HAS_PATHS' in rule) {
    return rule.HAS_PATHS.includes(changePath)
  }

  // Check if metadata matches
  if ('HAS_META_OF' in rule) {
    return Object.entries(rule.HAS_META_OF).every(
      ([key, value]) => changeMeta[key] === value,
    )
  }

  // Boolean combinators
  if ('AND' in rule) {
    return rule.AND.every((subRule) =>
      evaluateListenerFilter(subRule, changePath, changeMeta),
    )
  }

  if ('OR' in rule) {
    return rule.OR.some((subRule) =>
      evaluateListenerFilter(subRule, changePath, changeMeta),
    )
  }

  if ('NOT' in rule) {
    return !evaluateListenerFilter(rule.NOT, changePath, changeMeta)
  }

  return false
}

// ============================================================================
// Mock Listener Functions
// ============================================================================

// Simple path-based listeners (return early if path doesn't match)
const createSimpleListener = (interestedPaths) => {
  return (change) => {
    const [path] = change
    if (!interestedPaths.includes(path)) {
      return [] // Early return
    }
    // Do some work
    return [[`${path}.computed`, true, {}]]
  }
}

// Metadata-based listeners (return early if meta doesn't match)
const createMetaListener = (requiredMeta) => {
  return (change) => {
    const [path, , meta] = change
    const matches = Object.entries(requiredMeta).every(
      ([key, value]) => meta[key] === value,
    )
    if (!matches) {
      return [] // Early return
    }
    // Do some work
    return [[`${path}.processed`, true, {}]]
  }
}

// Complex listeners (multiple conditions)
const createComplexListener = (interestedPaths, requiredMeta) => {
  return (change) => {
    const [path, , meta] = change

    // Check path
    if (!interestedPaths.includes(path)) {
      return []
    }

    // Check meta
    const matches = Object.entries(requiredMeta).every(
      ([key, value]) => meta[key] === value,
    )
    if (!matches) {
      return []
    }

    // Do some work
    return [[`${path}.complex`, true, {}]]
  }
}

// ============================================================================
// Benchmark Helpers
// ============================================================================

const benchmark = (fn, iterations = 10000) => {
  const start = performance.now()
  for (let i = 0; i < iterations; i++) {
    fn()
  }
  return (performance.now() - start) / iterations
}

// ============================================================================
// SCENARIO 1: Simple Path Matching (10 listeners, 1 match per change)
// ============================================================================

console.log('=== LISTENER FILTERING BENCHMARK ===\n')
console.log('SCENARIO 1: Simple Path Matching')
console.log('  10 listeners, each interested in 1-2 paths')
console.log('  Changes trigger 1 listener each')
console.log()

// Setup: 10 listeners, each interested in different paths
const listeners1 = []

for (let i = 0; i < 10; i++) {
  const path = `product.${i}.price`
  listeners1.push({
    fn: createSimpleListener([path]),
    filterRule: { HAS_PATHS: [path] },
  })
}

// Test changes (each triggers 1 listener)
const changes1 = [
  ['product.0.price', 100, { isUserChange: true }],
  ['product.5.price', 200, { isUserChange: true }],
  ['product.9.price', 300, { isUserChange: true }],
]

// Approach A: Filter THEN call
const withFilterTime1 = benchmark(() => {
  for (const change of changes1) {
    const [path, , meta] = change
    for (const listener of listeners1) {
      if (evaluateListenerFilter(listener.filterRule, path, meta)) {
        listener.fn(change)
      }
    }
  }
})

// Approach B: Call directly (early return)
const withoutFilterTime1 = benchmark(() => {
  for (const change of changes1) {
    for (const listener of listeners1) {
      listener.fn(change)
    }
  }
})

console.log(`  With filter:    ${withFilterTime1.toFixed(4)}ms`)
console.log(`  Without filter: ${withoutFilterTime1.toFixed(4)}ms`)
console.log(
  `  Winner:         ${withFilterTime1 < withoutFilterTime1 ? 'WITH filter' : 'WITHOUT filter'} (${Math.abs(1 - withFilterTime1 / withoutFilterTime1).toFixed(1)}% ${withFilterTime1 < withoutFilterTime1 ? 'faster' : 'slower'})`,
)
console.log(
  `  Time diff:      ${((withoutFilterTime1 - withFilterTime1) * 1000).toFixed(1)}μs`,
)
console.log()

// ============================================================================
// SCENARIO 2: Metadata Matching (20 listeners, various meta conditions)
// ============================================================================

console.log('SCENARIO 2: Metadata Matching')
console.log('  20 listeners with different meta requirements')
console.log('  Changes have various metadata flags')
console.log()

// Setup: 20 listeners with different meta requirements
const listeners2 = []
const metaConditions = [
  { isUserChange: true },
  { isMarketDataUpdate: true },
  { isRiskBreach: true },
  { isHedgeExecution: true },
]

for (let i = 0; i < 20; i++) {
  const metaCondition = metaConditions[i % metaConditions.length]
  listeners2.push({
    fn: createMetaListener(metaCondition),
    filterRule: { HAS_META_OF: metaCondition },
  })
}

// Test changes (mix of metadata)
const changes2 = [
  ['product.0.price', 100, { isUserChange: true }],
  ['product.1.price', 200, { isMarketDataUpdate: true }],
  ['product.2.price', 300, { isRiskBreach: true }],
  ['product.3.price', 400, { isHedgeExecution: true }],
  ['product.4.price', 500, { isUserChange: true }],
]

// Approach A: Filter THEN call
const withFilterTime2 = benchmark(() => {
  for (const change of changes2) {
    const [path, , meta] = change
    for (const listener of listeners2) {
      if (evaluateListenerFilter(listener.filterRule, path, meta)) {
        listener.fn(change)
      }
    }
  }
}, 5000)

// Approach B: Call directly (early return)
const withoutFilterTime2 = benchmark(() => {
  for (const change of changes2) {
    for (const listener of listeners2) {
      listener.fn(change)
    }
  }
}, 5000)

console.log(`  With filter:    ${withFilterTime2.toFixed(4)}ms`)
console.log(`  Without filter: ${withoutFilterTime2.toFixed(4)}ms`)
console.log(
  `  Winner:         ${withFilterTime2 < withoutFilterTime2 ? 'WITH filter' : 'WITHOUT filter'} (${Math.abs(1 - withFilterTime2 / withoutFilterTime2).toFixed(1)}% ${withFilterTime2 < withoutFilterTime2 ? 'faster' : 'slower'})`,
)
console.log(
  `  Time diff:      ${((withoutFilterTime2 - withFilterTime2) * 1000).toFixed(1)}μs`,
)
console.log()

// ============================================================================
// SCENARIO 3: Complex Boolean Combinators (AND/OR/NOT)
// ============================================================================

console.log('SCENARIO 3: Complex Boolean Combinators')
console.log('  10 listeners with complex AND/OR conditions')
console.log('  Filter evaluation is more expensive')
console.log()

// Setup: Complex filter rules
const listeners3 = []
for (let i = 0; i < 10; i++) {
  const path = `product.${i}.price`
  listeners3.push({
    fn: createComplexListener([path], { isUserChange: true }),
    filterRule: {
      AND: [
        { HAS_PATHS: [path] },
        { HAS_META_OF: { isUserChange: true } },
        { NOT: { HAS_META_OF: { isSyncPathChange: true } } },
      ],
    },
  })
}

// Test changes
const changes3 = [
  ['product.0.price', 100, { isUserChange: true }],
  ['product.5.price', 200, { isUserChange: true, isSyncPathChange: true }],
  ['product.9.price', 300, { isMarketDataUpdate: true }],
]

// Approach A: Filter THEN call
const withFilterTime3 = benchmark(() => {
  for (const change of changes3) {
    const [path, , meta] = change
    for (const listener of listeners3) {
      if (evaluateListenerFilter(listener.filterRule, path, meta)) {
        listener.fn(change)
      }
    }
  }
})

// Approach B: Call directly (early return)
const withoutFilterTime3 = benchmark(() => {
  for (const change of changes3) {
    for (const listener of listeners3) {
      listener.fn(change)
    }
  }
})

console.log(`  With filter:    ${withFilterTime3.toFixed(4)}ms`)
console.log(`  Without filter: ${withoutFilterTime3.toFixed(4)}ms`)
console.log(
  `  Winner:         ${withFilterTime3 < withoutFilterTime3 ? 'WITH filter' : 'WITHOUT filter'} (${Math.abs(1 - withFilterTime3 / withoutFilterTime3).toFixed(1)}% ${withFilterTime3 < withoutFilterTime3 ? 'faster' : 'slower'})`,
)
console.log(
  `  Time diff:      ${((withoutFilterTime3 - withFilterTime3) * 1000).toFixed(1)}μs`,
)
console.log()

// ============================================================================
// SCENARIO 4: Real-World Mix (100 listeners, 50 changes)
// ============================================================================

console.log('SCENARIO 4: Real-World Mix')
console.log('  100 listeners (mix of path/meta/complex filters)')
console.log('  50 changes with various paths and metadata')
console.log()

// Setup: Mix of listener types
const listeners4 = []

// 40 path-based listeners
for (let i = 0; i < 40; i++) {
  const path = `group.${Math.floor(i / 4)}.product.${i}.price`
  listeners4.push({
    fn: createSimpleListener([path]),
    filterRule: { HAS_PATHS: [path] },
  })
}

// 40 meta-based listeners
for (let i = 0; i < 40; i++) {
  const metaCondition = metaConditions[i % metaConditions.length]
  listeners4.push({
    fn: createMetaListener(metaCondition),
    filterRule: { HAS_META_OF: metaCondition },
  })
}

// 20 complex listeners
for (let i = 0; i < 20; i++) {
  const path = `group.${i}.aggregate`
  listeners4.push({
    fn: createComplexListener([path], { isUserChange: true }),
    filterRule: {
      AND: [{ HAS_PATHS: [path] }, { HAS_META_OF: { isUserChange: true } }],
    },
  })
}

// Test changes: 50 random changes
const changes4 = []
for (let i = 0; i < 50; i++) {
  const groupId = Math.floor(Math.random() * 10)
  const productId = Math.floor(Math.random() * 40)
  const path = `group.${groupId}.product.${productId}.price`
  const meta = metaConditions[i % metaConditions.length]
  changes4.push([path, Math.random() * 1000, meta])
}

// Approach A: Filter THEN call
const withFilterTime4 = benchmark(() => {
  for (const change of changes4) {
    const [path, , meta] = change
    for (const listener of listeners4) {
      if (evaluateListenerFilter(listener.filterRule, path, meta)) {
        listener.fn(change)
      }
    }
  }
}, 100)

// Approach B: Call directly (early return)
const withoutFilterTime4 = benchmark(() => {
  for (const change of changes4) {
    for (const listener of listeners4) {
      listener.fn(change)
    }
  }
}, 100)

console.log(`  With filter:    ${withFilterTime4.toFixed(4)}ms`)
console.log(`  Without filter: ${withoutFilterTime4.toFixed(4)}ms`)
console.log(
  `  Winner:         ${withFilterTime4 < withoutFilterTime4 ? 'WITH filter' : 'WITHOUT filter'} (${Math.abs(1 - withFilterTime4 / withoutFilterTime4).toFixed(1)}% ${withFilterTime4 < withoutFilterTime4 ? 'faster' : 'slower'})`,
)
console.log(
  `  Time diff:      ${((withoutFilterTime4 - withFilterTime4) * 1000).toFixed(1)}μs`,
)
console.log()

// ============================================================================
// SCENARIO 5: High Miss Rate (most listeners don't match)
// ============================================================================

console.log('SCENARIO 5: High Miss Rate (realistic scenario)')
console.log('  100 listeners, changes only trigger 5-10% of them')
console.log('  Tests filter efficiency when most listeners are skipped')
console.log()

// Setup: 100 listeners with specific paths
const listeners5 = []
for (let i = 0; i < 100; i++) {
  const path = `product.${i}.price`
  listeners5.push({
    fn: createSimpleListener([path]),
    filterRule: { HAS_PATHS: [path] },
  })
}

// Test changes: Only affect 5 out of 100 listeners
const changes5 = [
  ['product.0.price', 100, { isUserChange: true }],
  ['product.25.price', 200, { isUserChange: true }],
  ['product.50.price', 300, { isUserChange: true }],
  ['product.75.price', 400, { isUserChange: true }],
  ['product.99.price', 500, { isUserChange: true }],
]

// Approach A: Filter THEN call
const withFilterTime5 = benchmark(() => {
  for (const change of changes5) {
    const [path, , meta] = change
    for (const listener of listeners5) {
      if (evaluateListenerFilter(listener.filterRule, path, meta)) {
        listener.fn(change)
      }
    }
  }
}, 1000)

// Approach B: Call directly (early return)
const withoutFilterTime5 = benchmark(() => {
  for (const change of changes5) {
    for (const listener of listeners5) {
      listener.fn(change)
    }
  }
}, 1000)

console.log(`  With filter:    ${withFilterTime5.toFixed(4)}ms`)
console.log(`  Without filter: ${withoutFilterTime5.toFixed(4)}ms`)
console.log(
  `  Winner:         ${withFilterTime5 < withoutFilterTime5 ? 'WITH filter' : 'WITHOUT filter'} (${Math.abs(1 - withFilterTime5 / withoutFilterTime5).toFixed(1)}% ${withFilterTime5 < withoutFilterTime5 ? 'faster' : 'slower'})`,
)
console.log(
  `  Time diff:      ${((withoutFilterTime5 - withFilterTime5) * 1000).toFixed(1)}μs`,
)
console.log()

// ============================================================================
// Summary Table
// ============================================================================

console.log('=== SUMMARY TABLE ===')
console.log(
  '┌────────────────────────────────┬──────────────┬───────────────┬──────────┬──────────┐',
)
console.log(
  '│ Scenario                       │ With Filter  │ Without Filter│ Winner   │ Diff     │',
)
console.log(
  '├────────────────────────────────┼──────────────┼───────────────┼──────────┼──────────┤',
)

const scenarios = [
  {
    name: 'Simple path matching (10)',
    with: withFilterTime1,
    without: withoutFilterTime1,
  },
  {
    name: 'Metadata matching (20)',
    with: withFilterTime2,
    without: withoutFilterTime2,
  },
  {
    name: 'Complex combinators (10)',
    with: withFilterTime3,
    without: withoutFilterTime3,
  },
  {
    name: 'Real-world mix (100)',
    with: withFilterTime4,
    without: withoutFilterTime4,
  },
  {
    name: 'High miss rate (100)',
    with: withFilterTime5,
    without: withoutFilterTime5,
  },
]

scenarios.forEach((scenario) => {
  const winner = scenario.with < scenario.without ? 'Filter  ' : 'Direct  '
  const diff = Math.abs(scenario.with - scenario.without) * 1000
  console.log(
    `│ ${scenario.name.padEnd(30)} │ ${scenario.with.toFixed(4)}ms    │ ${scenario.without.toFixed(4)}ms     │ ${winner} │ ${diff.toFixed(1)}μs    │`,
  )
})

console.log(
  '└────────────────────────────────┴──────────────┴───────────────┴──────────┴──────────┘',
)
console.log()

// ============================================================================
// Analysis & Recommendation
// ============================================================================

console.log('=== ANALYSIS ===')
console.log()

const avgWithFilter =
  scenarios.reduce((sum, s) => sum + s.with, 0) / scenarios.length
const avgWithoutFilter =
  scenarios.reduce((sum, s) => sum + s.without, 0) / scenarios.length

console.log('Key findings:')
console.log()

// Determine which approach wins in each category
let filterWins = 0
let directWins = 0

scenarios.forEach((s) => {
  if (s.with < s.without) {
    filterWins++
  } else {
    directWins++
  }
})

console.log(
  `1. Filter approach wins: ${filterWins}/${scenarios.length} scenarios`,
)
console.log(
  `2. Direct call wins:     ${directWins}/${scenarios.length} scenarios`,
)
console.log()

if (filterWins > directWins) {
  console.log(
    '✅ Filtering is BENEFICIAL - it reduces unnecessary function calls',
  )
  console.log('   Especially effective when:')
  console.log('   - High miss rate (most listeners do not match)')
  console.log('   - Simple filter rules (path/meta checks)')
  console.log('   - Many listeners registered')
} else {
  console.log('❌ Filtering adds OVERHEAD - direct calls are faster')
  console.log('   Consider removing filtering when:')
  console.log('   - Complex filter rules (AND/OR/NOT nesting)')
  console.log('   - High match rate (most listeners fire)')
  console.log('   - Listener functions return early anyway')
}

console.log()
console.log(`Average time with filter:    ${avgWithFilter.toFixed(4)}ms`)
console.log(`Average time without filter: ${avgWithoutFilter.toFixed(4)}ms`)
console.log(
  `Overall difference:          ${((avgWithoutFilter - avgWithFilter) * 1000).toFixed(1)}μs (${((1 - avgWithFilter / avgWithoutFilter) * 100).toFixed(1)}% ${avgWithFilter < avgWithoutFilter ? 'faster with filter' : 'slower with filter'})`,
)
console.log()

console.log('=== RECOMMENDATION ===')
console.log()

if (avgWithFilter < avgWithoutFilter) {
  const improvement = ((1 - avgWithFilter / avgWithoutFilter) * 100).toFixed(1)
  console.log(
    `✅ KEEP listener filtering - provides ${improvement}% average improvement`,
  )
  console.log('   The filter overhead is lower than function call overhead')
  console.log(
    '   Especially beneficial with many listeners and low match rates',
  )
} else {
  const overhead = ((avgWithFilter / avgWithoutFilter - 1) * 100).toFixed(1)
  console.log(
    `❌ REMOVE listener filtering - adds ${overhead}% average overhead`,
  )
  console.log('   Direct function calls are faster')
  console.log('   Let listener functions handle early returns')
}
