/**
 * Listener Changes Array Pre-Filtering Benchmark
 *
 * Tests whether pre-filtering the changes array for each listener
 * is faster than passing the full array and letting listeners search.
 *
 * Current approach:
 * - Pass all changes to listener
 * - Listener does changes.find(c => c[0] === 'myPath') or changes.filter()
 *
 * Proposed approach:
 * - Pre-filter changes to only relevant paths for each listener
 * - Pass smaller array to listener
 * - Listener processes only relevant changes
 *
 * Scenarios:
 * 1. Small changes array (10 changes, 1 relevant)
 * 2. Medium changes array (50 changes, 5 relevant)
 * 3. Large changes array (200 changes, 10 relevant)
 * 4. Many listeners (100 listeners, 50 changes)
 * 5. Listener with multiple paths
 *
 * Run: node benchmarks/listener-changes-filtering.js
 */

// ============================================================================
// Implementation Approaches
// ============================================================================

// Current: Listener searches through full changes array
const createListenerWithSearch = (interestedPaths) => {
  return (changes) => {
    const results = []
    for (const change of changes) {
      const [path, value] = change
      if (interestedPaths.includes(path)) {
        // Do some work
        results.push([`${path}.computed`, value * 2, {}])
      }
    }
    return results
  }
}

// Proposed: Pre-filter changes before passing to listener
const preFilterChangesForListener = (changes, interestedPaths) => {
  return changes.filter((change) => interestedPaths.includes(change[0]))
}

const createListenerWithoutSearch = (_interestedPaths) => {
  return (changes) => {
    const results = []
    // Changes are already filtered, just process them all
    for (const change of changes) {
      const [path, value] = change
      // Do some work
      results.push([`${path}.computed`, value * 2, {}])
    }
    return results
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

console.log('=== LISTENER CHANGES ARRAY PRE-FILTERING BENCHMARK ===\n')

// ============================================================================
// SCENARIO 1: Small Changes Array (10 changes, 1 relevant)
// ============================================================================

console.log('SCENARIO 1: Small Changes Array')
console.log('  10 changes total, listener interested in 1 path')
console.log('  90% of changes are irrelevant to this listener')
console.log()

const changes1 = [
  ['product.0.price', 100, {}],
  ['product.1.price', 101, {}],
  ['product.2.price', 102, {}], // ← listener wants this
  ['product.3.price', 103, {}],
  ['product.4.price', 104, {}],
  ['product.5.price', 105, {}],
  ['product.6.price', 106, {}],
  ['product.7.price', 107, {}],
  ['product.8.price', 108, {}],
  ['product.9.price', 109, {}],
]

const listener1WithSearch = createListenerWithSearch(['product.2.price'])
const listener1WithoutSearch = createListenerWithoutSearch(['product.2.price'])

// Current: Pass full array, listener searches
const time1Current = benchmark(() => {
  listener1WithSearch(changes1)
})

// Proposed: Pre-filter, then pass to listener
const time1Proposed = benchmark(() => {
  const filtered = preFilterChangesForListener(changes1, ['product.2.price'])
  listener1WithoutSearch(filtered)
})

console.log(`  Current (listener searches): ${time1Current.toFixed(4)}ms`)
console.log(`  Proposed (pre-filtered):     ${time1Proposed.toFixed(4)}ms`)
console.log(
  `  Winner:                      ${time1Proposed < time1Current ? 'PRE-FILTER' : 'LISTENER SEARCH'} (${time1Proposed < time1Current ? ((1 - time1Proposed / time1Current) * 100).toFixed(1) + '% faster' : ((time1Proposed / time1Current - 1) * 100).toFixed(1) + '% slower'})`,
)
console.log(
  `  Time diff:                   ${((time1Current - time1Proposed) * 1000).toFixed(1)}μs`,
)
console.log()

// ============================================================================
// SCENARIO 2: Medium Changes Array (50 changes, 5 relevant)
// ============================================================================

console.log('SCENARIO 2: Medium Changes Array')
console.log('  50 changes total, listener interested in 5 paths')
console.log('  90% of changes are irrelevant')
console.log()

const changes2 = []
for (let i = 0; i < 50; i++) {
  changes2.push([`product.${i}.price`, 100 + i, {}])
}

const interestedPaths2 = [
  'product.5.price',
  'product.15.price',
  'product.25.price',
  'product.35.price',
  'product.45.price',
]

const listener2WithSearch = createListenerWithSearch(interestedPaths2)
const listener2WithoutSearch = createListenerWithoutSearch(interestedPaths2)

const time2Current = benchmark(() => {
  listener2WithSearch(changes2)
})

const time2Proposed = benchmark(() => {
  const filtered = preFilterChangesForListener(changes2, interestedPaths2)
  listener2WithoutSearch(filtered)
})

console.log(`  Current (listener searches): ${time2Current.toFixed(4)}ms`)
console.log(`  Proposed (pre-filtered):     ${time2Proposed.toFixed(4)}ms`)
console.log(
  `  Winner:                      ${time2Proposed < time2Current ? 'PRE-FILTER' : 'LISTENER SEARCH'} (${time2Proposed < time2Current ? ((1 - time2Proposed / time2Current) * 100).toFixed(1) + '% faster' : ((time2Proposed / time2Current - 1) * 100).toFixed(1) + '% slower'})`,
)
console.log(
  `  Time diff:                   ${((time2Current - time2Proposed) * 1000).toFixed(1)}μs`,
)
console.log()

// ============================================================================
// SCENARIO 3: Large Changes Array (200 changes, 10 relevant)
// ============================================================================

console.log('SCENARIO 3: Large Changes Array')
console.log('  200 changes total, listener interested in 10 paths')
console.log('  95% of changes are irrelevant')
console.log()

const changes3 = []
for (let i = 0; i < 200; i++) {
  changes3.push([`product.${i}.price`, 100 + i, {}])
}

const interestedPaths3 = [
  'product.10.price',
  'product.30.price',
  'product.50.price',
  'product.70.price',
  'product.90.price',
  'product.110.price',
  'product.130.price',
  'product.150.price',
  'product.170.price',
  'product.190.price',
]

const listener3WithSearch = createListenerWithSearch(interestedPaths3)
const listener3WithoutSearch = createListenerWithoutSearch(interestedPaths3)

const time3Current = benchmark(() => {
  listener3WithSearch(changes3)
}, 5000)

const time3Proposed = benchmark(() => {
  const filtered = preFilterChangesForListener(changes3, interestedPaths3)
  listener3WithoutSearch(filtered)
}, 5000)

console.log(`  Current (listener searches): ${time3Current.toFixed(4)}ms`)
console.log(`  Proposed (pre-filtered):     ${time3Proposed.toFixed(4)}ms`)
console.log(
  `  Winner:                      ${time3Proposed < time3Current ? 'PRE-FILTER' : 'LISTENER SEARCH'} (${time3Proposed < time3Current ? ((1 - time3Proposed / time3Current) * 100).toFixed(1) + '% faster' : ((time3Proposed / time3Current - 1) * 100).toFixed(1) + '% slower'})`,
)
console.log(
  `  Time diff:                   ${((time3Current - time3Proposed) * 1000).toFixed(1)}μs`,
)
console.log()

// ============================================================================
// SCENARIO 4: Many Listeners (100 listeners, 50 changes each)
// ============================================================================

console.log('SCENARIO 4: Many Listeners Processing Same Changes')
console.log('  100 listeners, each interested in 1-2 specific paths')
console.log('  50 changes total')
console.log()

const changes4 = []
for (let i = 0; i < 50; i++) {
  changes4.push([`product.${i}.price`, 100 + i, {}])
}

// Setup 100 listeners, each interested in 1-2 paths
const listeners4 = []
for (let i = 0; i < 100; i++) {
  const paths = [`product.${i % 50}.price`]
  listeners4.push({
    interestedPaths: paths,
    listenerWithSearch: createListenerWithSearch(paths),
    listenerWithoutSearch: createListenerWithoutSearch(paths),
  })
}

// Current: Each listener searches through full 50-change array
const time4Current = benchmark(() => {
  for (const listener of listeners4) {
    listener.listenerWithSearch(changes4)
  }
}, 100)

// Proposed: Pre-filter for each listener
const time4Proposed = benchmark(() => {
  for (const listener of listeners4) {
    const filtered = preFilterChangesForListener(
      changes4,
      listener.interestedPaths,
    )
    listener.listenerWithoutSearch(filtered)
  }
}, 100)

console.log(`  Current (listener searches): ${time4Current.toFixed(4)}ms`)
console.log(`  Proposed (pre-filtered):     ${time4Proposed.toFixed(4)}ms`)
console.log(
  `  Winner:                      ${time4Proposed < time4Current ? 'PRE-FILTER' : 'LISTENER SEARCH'} (${time4Proposed < time4Current ? ((1 - time4Proposed / time4Current) * 100).toFixed(1) + '% faster' : ((time4Proposed / time4Current - 1) * 100).toFixed(1) + '% slower'})`,
)
console.log(
  `  Time diff:                   ${((time4Current - time4Proposed) * 1000).toFixed(1)}μs`,
)
console.log()

// ============================================================================
// SCENARIO 5: Listener With Many Interested Paths
// ============================================================================

console.log('SCENARIO 5: Listener Interested in Many Paths')
console.log('  Listener interested in 20 paths')
console.log('  100 changes total, 15 are relevant')
console.log()

const changes5 = []
for (let i = 0; i < 100; i++) {
  changes5.push([`product.${i}.price`, 100 + i, {}])
}

const interestedPaths5 = []
for (let i = 0; i < 20; i++) {
  interestedPaths5.push(`product.${i * 5}.price`)
}

const listener5WithSearch = createListenerWithSearch(interestedPaths5)
const listener5WithoutSearch = createListenerWithoutSearch(interestedPaths5)

const time5Current = benchmark(() => {
  listener5WithSearch(changes5)
}, 1000)

const time5Proposed = benchmark(() => {
  const filtered = preFilterChangesForListener(changes5, interestedPaths5)
  listener5WithoutSearch(filtered)
}, 1000)

console.log(`  Current (listener searches): ${time5Current.toFixed(4)}ms`)
console.log(`  Proposed (pre-filtered):     ${time5Proposed.toFixed(4)}ms`)
console.log(
  `  Winner:                      ${time5Proposed < time5Current ? 'PRE-FILTER' : 'LISTENER SEARCH'} (${time5Proposed < time5Current ? ((1 - time5Proposed / time5Current) * 100).toFixed(1) + '% faster' : ((time5Proposed / time5Current - 1) * 100).toFixed(1) + '% slower'})`,
)
console.log(
  `  Time diff:                   ${((time5Current - time5Proposed) * 1000).toFixed(1)}μs`,
)
console.log()

// ============================================================================
// SCENARIO 6: High Match Rate (listener needs most changes)
// ============================================================================

console.log('SCENARIO 6: High Match Rate')
console.log('  Listener interested in 40 out of 50 paths (80% match rate)')
console.log('  Pre-filtering overhead may not be worth it')
console.log()

const changes6 = []
for (let i = 0; i < 50; i++) {
  changes6.push([`product.${i}.price`, 100 + i, {}])
}

const interestedPaths6 = []
for (let i = 0; i < 40; i++) {
  interestedPaths6.push(`product.${i}.price`)
}

const listener6WithSearch = createListenerWithSearch(interestedPaths6)
const listener6WithoutSearch = createListenerWithoutSearch(interestedPaths6)

const time6Current = benchmark(() => {
  listener6WithSearch(changes6)
}, 1000)

const time6Proposed = benchmark(() => {
  const filtered = preFilterChangesForListener(changes6, interestedPaths6)
  listener6WithoutSearch(filtered)
}, 1000)

console.log(`  Current (listener searches): ${time6Current.toFixed(4)}ms`)
console.log(`  Proposed (pre-filtered):     ${time6Proposed.toFixed(4)}ms`)
console.log(
  `  Winner:                      ${time6Proposed < time6Current ? 'PRE-FILTER' : 'LISTENER SEARCH'} (${time6Proposed < time6Current ? ((1 - time6Proposed / time6Current) * 100).toFixed(1) + '% faster' : ((time6Proposed / time6Current - 1) * 100).toFixed(1) + '% slower'})`,
)
console.log(
  `  Time diff:                   ${((time6Current - time6Proposed) * 1000).toFixed(1)}μs`,
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
  '│ Scenario                       │ Current      │ Pre-Filtered  │ Winner       │ Impact   │',
)
console.log(
  '├────────────────────────────────┼──────────────┼───────────────┼──────────────┼──────────┤',
)

const scenarios = [
  {
    name: 'Small array (10 changes)',
    current: time1Current,
    proposed: time1Proposed,
  },
  {
    name: 'Medium array (50 changes)',
    current: time2Current,
    proposed: time2Proposed,
  },
  {
    name: 'Large array (200 changes)',
    current: time3Current,
    proposed: time3Proposed,
  },
  {
    name: 'Many listeners (100)',
    current: time4Current,
    proposed: time4Proposed,
  },
  {
    name: 'Many interested paths (20)',
    current: time5Current,
    proposed: time5Proposed,
  },
  {
    name: 'High match rate (80%)',
    current: time6Current,
    proposed: time6Proposed,
  },
]

scenarios.forEach((scenario) => {
  const winner =
    scenario.proposed < scenario.current ? 'Pre-filter  ' : 'Current     '
  const impact =
    scenario.proposed < scenario.current
      ? `+${((1 - scenario.proposed / scenario.current) * 100).toFixed(0)}%`
      : `-${((scenario.proposed / scenario.current - 1) * 100).toFixed(0)}%`
  console.log(
    `│ ${scenario.name.padEnd(30)} │ ${scenario.current.toFixed(4)}ms    │ ${scenario.proposed.toFixed(4)}ms     │ ${winner} │ ${impact.padEnd(8)} │`,
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

let preFilterWins = 0
let currentWins = 0

scenarios.forEach((s) => {
  if (s.proposed < s.current) {
    preFilterWins++
  } else {
    currentWins++
  }
})

console.log(`Pre-filter wins:  ${preFilterWins}/${scenarios.length} scenarios`)
console.log(`Current wins:     ${currentWins}/${scenarios.length} scenarios`)
console.log()

const avgCurrent =
  scenarios.reduce((sum, s) => sum + s.current, 0) / scenarios.length
const avgProposed =
  scenarios.reduce((sum, s) => sum + s.proposed, 0) / scenarios.length

console.log('Key findings:')
console.log()

if (preFilterWins > currentWins) {
  console.log('1. Pre-filtering is faster in most scenarios')
  console.log('2. Best gains when:')
  console.log('   - Large changes arrays (100+ changes)')
  console.log('   - Low match rate (<20% relevant)')
  console.log('   - Many listeners processing same changes')
  console.log()
  console.log('3. Trade-off point:')
  console.log('   - Pre-filtering overhead increases with match rate')
  console.log('   - Not worth it when >60% of changes are relevant')
} else {
  console.log('1. Current approach (listener searches) is faster')
  console.log('2. Pre-filtering overhead exceeds search cost')
  console.log('3. JS array operations are very optimized')
}

console.log()
console.log(`Average current:  ${avgCurrent.toFixed(4)}ms`)
console.log(`Average proposed: ${avgProposed.toFixed(4)}ms`)
console.log(
  `Overall impact:   ${avgProposed < avgCurrent ? '+' : ''}${((1 - avgProposed / avgCurrent) * 100).toFixed(1)}%`,
)
console.log()

console.log('=== RECOMMENDATION ===')
console.log()

if (avgProposed < avgCurrent) {
  const improvement = ((1 - avgProposed / avgCurrent) * 100).toFixed(1)
  console.log(
    `✅ IMPLEMENT pre-filtering - provides ${improvement}% average improvement`,
  )
  console.log()
  console.log('Implementation approach:')
  console.log('```typescript')
  console.log('const processListeners = (changes, store) => {')
  console.log('  for (const [path, pathListeners] of store.listeners) {')
  console.log('    // Pre-filter changes to only relevant ones')
  console.log('    const relevantChanges = changes.filter(c => c[0] === path)')
  console.log('    if (relevantChanges.length === 0) continue')
  console.log()
  console.log('    for (const listener of pathListeners) {')
  console.log('      // Listener only sees relevant changes!')
  console.log('      const result = listener.fn(relevantChanges)')
  console.log('      if (result?.length) queue.push(...result)')
  console.log('    }')
  console.log('  }')
  console.log('}')
  console.log('```')
  console.log()
  console.log('Benefits:')
  console.log('  - Listeners receive smaller arrays')
  console.log('  - No need for changes.find() in listener code')
  console.log('  - Simpler listener implementations')
  console.log('  - Scales better with large change batches')
} else {
  const overhead = ((avgProposed / avgCurrent - 1) * 100).toFixed(1)
  console.log(`❌ SKIP pre-filtering - adds ${overhead}% overhead`)
  console.log()
  console.log('Reasons:')
  console.log('  - JS array iteration is highly optimized')
  console.log('  - Pre-filtering adds extra filter() call overhead')
  console.log('  - Listener search with early-exit is fast enough')
  console.log()
  console.log('Alternative:')
  console.log('  - Keep current approach')
  console.log('  - Optimize listener code to use early returns')
  console.log('  - Consider indexed lookups for path-based listeners')
}
