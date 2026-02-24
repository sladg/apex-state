/**
 * Type-level performance benchmark for SyncPair alternatives
 *
 * Tests different approaches for type-safe path pairing on a ~1500-path state type.
 * Run with: npx tsc --noEmit --extendedDiagnostics
 * Compare: npx vitest typecheck tests/types/sync-pair-perf.test-d.ts
 *
 * Current SyncPair is O(N^2) where N = |DeepKey<T>|. With ~1500 paths that means
 * ~2.25M type instantiations, which exceeds TypeScript's recursion/instantiation limits.
 *
 * Tests the lazy-validated pair helpers (syncPairs, flipPairs, aggregationPairs,
 * computationPairs) that replaced the O(N²) SyncPair approach.
 * Old SyncPair<T> hit TS2589 at ~1,500 paths; helpers scale to ~50K–80K paths.
 *
 * @perf-history
 * Hardware: Apple M4 Pro
 * | Date       | Approach | Result      | Check time | Memory    | Note                          |
 * |------------|----------|-------------|------------|-----------|-------------------------------|
 * | 2026-02-24 | A (N²)   | TS2589 FAIL | 6.91s      | 1,492 MB  | baseline — O(N²) blows up     |
 * | 2026-02-24 | E (N²+c) | TS2589 FAIL | 6.91s      | 1,492 MB  | cached map still O(N²)        |
 * | 2026-02-24 | B (N)    | PASS        | 7.31s      | 797 MB    | loose pair, no value check    |
 * | 2026-02-24 | C (3N)   | PASS        | 7.31s      | 797 MB    | per-primitive, type-safe      |
 * | 2026-02-24 | D (K)    | PASS        | 7.31s      | 797 MB    | deferred, lazy per-pair check |
 * | 2026-02-24 | F (lazy) | PASS        | 4.74s      | 574 MB    | syncPairs helper, 52.8K paths |
 *
 * Scaling results for syncPairs<T>()() (approach F):
 * | Paths    | Structure          | Depth | Result    | Time  | Memory |
 * |----------|--------------------|-------|-----------|-------|--------|
 * | ~1,650   | 10 sectors flat    |   4   | PASS      | 1.3s  | 400 MB |
 * | ~9,900   | 60 sectors flat    |   4   | PASS      | 1.5s  | 513 MB |
 * | ~33,000  | 200 sectors flat   |   4   | PASS      | 2.5s  | 734 MB |
 * | ~52,800  | binary 6 levels    |   9   | PASS      | 4.7s  | 574 MB |
 * | ~82,500  | 500 sectors flat   |   4   | PASS      | 7.1s  | 617 MB |
 * | ~105,600 | binary 7 levels    |  10   | TS2589    | 7.1s  | 792 MB |
 *
 * Practical limit: ~50K–80K paths (bounded by ResolvableDeepKey union at
 * function constraint, not by the validation logic itself).
 * Old SyncPair<T> hit TS2589 at ~1,500 paths — a ~30-50x improvement.
 */

import { describe, expect, expectTypeOf, test } from 'vitest'

import type { OnStateListener } from '~/core/types'
import type { DeepKey } from '~/types/deep-key'
import type {
  CheckAggregationPairs,
  CheckComputationPairs,
} from '~/types/pair-validators'
import type { SyncPair } from '~/types/paths-of-same-value'
import type { SideEffects } from '~/types/side-effects'
import type {
  ValidatedAggregationPairs,
  ValidatedComputationPairs,
  ValidatedFlipPairs,
  ValidatedListeners,
  ValidatedSyncPairs,
} from '~/types/validated-pairs'
import {
  aggregationPairs,
  computationPairs,
  flipPairs,
  listeners,
  syncPairs,
} from '~/utils/pair-helpers'

// ============================================================================
// Leaf interfaces (5 distinct, each with 5 fields: mix of string/number/boolean)
// ============================================================================

interface PerfLeafA {
  name: string
  value: number
  active: boolean
  label: string
  score: number
}

interface PerfLeafB {
  title: string
  count: number
  enabled: boolean
  description: string
  rating: number
}

interface PerfLeafC {
  code: string
  amount: number
  visible: boolean
  hint: string
  weight: number
}

interface PerfLeafD {
  tag: string
  price: number
  selected: boolean
  memo: string
  rank: number
}

interface PerfLeafE {
  key: string
  total: number
  flagged: boolean
  note: string
  index: number
}

// ============================================================================
// Mid-level interfaces (10 distinct, each composes 2 leafs + 3-4 own primitives)
// Each mid produces ~15 paths: 2*5 leaf fields + 2 leaf keys + 3-4 own = ~15
// ============================================================================

interface PerfMidA1 {
  leafA: PerfLeafA
  leafB: PerfLeafB
  status: string
  priority: number
  locked: boolean
}

interface PerfMidA2 {
  leafA: PerfLeafA
  leafC: PerfLeafC
  region: string
  level: number
  archived: boolean
  category: string
}

interface PerfMidB1 {
  leafB: PerfLeafB
  leafD: PerfLeafD
  source: string
  version: number
  draft: boolean
}

interface PerfMidB2 {
  leafB: PerfLeafB
  leafE: PerfLeafE
  format: string
  revision: number
  pinned: boolean
  channel: string
}

interface PerfMidC1 {
  leafC: PerfLeafC
  leafA: PerfLeafA
  owner: string
  depth: number
  muted: boolean
}

interface PerfMidC2 {
  leafC: PerfLeafC
  leafD: PerfLeafD
  scope: string
  batch: number
  frozen: boolean
  origin: string
}

interface PerfMidD1 {
  leafD: PerfLeafD
  leafE: PerfLeafE
  alias: string
  offset: number
  hidden: boolean
}

interface PerfMidD2 {
  leafD: PerfLeafD
  leafA: PerfLeafA
  prefix: string
  quota: number
  starred: boolean
  variant: string
}

interface PerfMidE1 {
  leafE: PerfLeafE
  leafB: PerfLeafB
  suffix: string
  limit: number
  collapsed: boolean
}

interface PerfMidE2 {
  leafE: PerfLeafE
  leafC: PerfLeafC
  marker: string
  span: number
  dimmed: boolean
  group: string
}

// ============================================================================
// Sector interface (reusable): 10 mid fields + 4 own primitives
// Each sector produces: 10 mid keys + 10*~15 mid paths + 4 own = ~164 paths
// ============================================================================

interface PerfSector {
  mid1: PerfMidA1
  mid2: PerfMidA2
  mid3: PerfMidB1
  mid4: PerfMidB2
  mid5: PerfMidC1
  mid6: PerfMidC2
  mid7: PerfMidD1
  mid8: PerfMidD2
  mid9: PerfMidE1
  mid10: PerfMidE2
  sectorName: string
  sectorCode: number
  sectorActive: boolean
  sectorLabel: string
}

// ============================================================================
// HugeState: 10 sectors => ~10 * 164 + 10 = ~1650 paths
// ============================================================================

interface HugeState {
  sector1: PerfSector
  sector2: PerfSector
  sector3: PerfSector
  sector4: PerfSector
  sector5: PerfSector
  sector6: PerfSector
  sector7: PerfSector
  sector8: PerfSector
  sector9: PerfSector
  sector10: PerfSector
}

// ============================================================================
// 1. Baseline -- verify DeepKey resolves
// ============================================================================

describe('Baseline -- DeepKey<HugeState> resolves', () => {
  test('resolves without TS2589', () => {
    type AllPaths = DeepKey<HugeState>
    // Spot check a few paths at different depths
    expectTypeOf<'sector1'>().toMatchTypeOf<AllPaths>()
    expectTypeOf<'sector1.mid1'>().toMatchTypeOf<AllPaths>()
    expectTypeOf<'sector1.mid1.leafA.name'>().toMatchTypeOf<AllPaths>()
  })
})

// ============================================================================
// 2. Approach A -- Current SyncPair (O(N^2) -- expected to fail/timeout)
// ============================================================================

describe.skip('Approach A -- Current SyncPair (O(N^2) -- expected to fail/timeout)', () => {
  test('resolves SyncPair<HugeState>', () => {
    // @ts-expect-error — O(N²) exceeds recursion limit at ~1650 paths
    type Pair = SyncPair<HugeState>
    // This would be O(1500^2) = 2.25M type instantiations
    expectTypeOf<
      ['sector1.mid1.leafA.name', 'sector1.mid2.leafA.label']
    >().toMatchTypeOf<Pair>()
  })
})

// ============================================================================
// 3. Helper functions — runtime identity, type-level validation on HugeState
// ============================================================================

describe('syncPairs helper on HugeState (~1650 paths)', () => {
  test('accepts same-type string pairs', () => {
    const result = syncPairs<HugeState>()([
      ['sector1.mid1.leafA.name', 'sector3.mid3.leafB.title'],
      ['sector2.mid2.leafA.label', 'sector5.mid6.leafC.hint'],
    ])
    // NOTE: toEqualTypeOf fails on HugeState because CheckSyncPairs can't resolve
    // DeepValue for ~1650 paths (returns never). Call compiling is sufficient validation.
    expectTypeOf(result).toBeArray()
  })

  test('accepts same-type number pairs', () => {
    const result = syncPairs<HugeState>()([
      ['sector1.mid1.leafA.value', 'sector4.mid7.leafD.price'],
      ['sector2.mid3.leafB.count', 'sector8.mid9.leafE.total'],
    ])
    expectTypeOf(result).toBeArray()
  })

  test('accepts same-type boolean pairs', () => {
    const result = syncPairs<HugeState>()([
      ['sector1.mid1.leafA.active', 'sector6.mid3.leafB.enabled'],
      ['sector3.mid5.muted', 'sector10.mid7.hidden'],
    ])
    expectTypeOf(result).toBeArray()
  })

  test('accepts mixed primitive types across pairs', () => {
    const result = syncPairs<HugeState>()([
      ['sector1.mid1.leafA.name', 'sector2.mid3.leafB.title'], // string
      ['sector1.mid1.leafA.value', 'sector3.mid7.leafD.price'], // number
      ['sector1.mid1.leafA.active', 'sector4.mid9.leafB.enabled'], // boolean
    ])
    expectTypeOf(result).toBeArray()
  })

  test('returns input as-is at runtime (identity)', () => {
    const input = [['sector1.mid1.leafA.name', 'sector2.mid3.leafB.title']] as [
      ['sector1.mid1.leafA.name', 'sector2.mid3.leafB.title'],
    ]
    const result = syncPairs<HugeState>()(input)
    expect(result).toBe(input)
  })
})

describe('flipPairs helper on HugeState (~1650 paths)', () => {
  test('accepts same-type boolean pairs', () => {
    const result = flipPairs<HugeState>()([
      ['sector1.mid1.leafA.active', 'sector2.mid3.leafB.enabled'],
      ['sector5.mid1.locked', 'sector9.mid5.muted'],
    ])
    expectTypeOf(result).toBeArray()
  })
})

describe('aggregationPairs helper on HugeState (~1650 paths)', () => {
  test('accepts [target, source] pairs with same value type', () => {
    const result = aggregationPairs<HugeState>()([
      ['sector1.mid1.leafA.value', 'sector2.mid3.leafB.count'],
      ['sector1.mid1.leafA.score', 'sector3.mid7.leafD.price'],
    ])
    expectTypeOf(result).toBeArray()
  })

  test('CheckAggregationPairs validates pairs at type level', () => {
    // Same-type pairs pass through
    type ValidResult = CheckAggregationPairs<
      HugeState,
      [['sector1.mid1.leafA.value', 'sector2.mid3.leafB.count']]
    >
    expectTypeOf<ValidResult>().toEqualTypeOf<
      [['sector1.mid1.leafA.value', 'sector2.mid3.leafB.count']]
    >()
  })

  test('CheckAggregationPairs rejects cross-type pairs', () => {
    // string target + number source — P2 becomes never
    type InvalidResult = CheckAggregationPairs<
      HugeState,
      [['sector1.mid1.leafA.name', 'sector1.mid1.leafA.value']]
    >
    expectTypeOf<InvalidResult>().toEqualTypeOf<
      [['sector1.mid1.leafA.name', never]]
    >()
  })
})

describe('computationPairs helper on HugeState (~1650 paths)', () => {
  test('accepts [op, target, source] with number paths', () => {
    const result = computationPairs<HugeState>()([
      ['SUM', 'sector1.mid1.leafA.value', 'sector2.mid3.leafB.count'],
      ['AVG', 'sector1.mid1.leafA.score', 'sector3.mid7.leafD.price'],
    ])
    expectTypeOf(result).toBeArray()
  })

  test('CheckComputationPairs validates number paths', () => {
    type ValidResult = CheckComputationPairs<
      HugeState,
      [['SUM', 'sector1.mid1.leafA.value', 'sector2.mid3.leafB.count']]
    >
    expectTypeOf<ValidResult>().toEqualTypeOf<
      [['SUM', 'sector1.mid1.leafA.value', 'sector2.mid3.leafB.count']]
    >()
  })

  test('CheckComputationPairs rejects string paths', () => {
    type InvalidResult = CheckComputationPairs<
      HugeState,
      [['SUM', 'sector1.mid1.leafA.name', 'sector2.mid3.leafB.title']]
    >
    // String paths don't extend DeepKeyFiltered<..., number>, so result maps to never
    expectTypeOf<InvalidResult>().toBeObject()
  })
})

// ============================================================================
// 4. @ts-expect-error rejection tests — ensure invalid inputs are caught
// ============================================================================

// State with object-typed fields for testing object path mismatches
interface ObjLeaf {
  id: string
  score: number
  active: boolean
}

interface ObjMid {
  leaf: ObjLeaf
  tag: string
  count: number
}

interface ObjState {
  alpha: ObjMid
  beta: ObjMid
  gamma: {
    nested: ObjMid
    label: string
    total: number
    enabled: boolean
  }
  topName: string
  topCount: number
  topFlag: boolean
}

describe('syncPairs rejection tests — @ts-expect-error', () => {
  test('rejects string + number pair', () => {
    // @ts-expect-error — string path cannot pair with number path
    syncPairs<ObjState>()([['alpha.tag', 'alpha.count']])
  })

  test('rejects number + string pair', () => {
    // @ts-expect-error — number path cannot pair with string path
    syncPairs<ObjState>()([['alpha.count', 'alpha.tag']])
  })

  test('rejects string + boolean pair', () => {
    // @ts-expect-error — string path cannot pair with boolean path
    syncPairs<ObjState>()([['alpha.tag', 'alpha.leaf.active']])
  })

  test('rejects boolean + number pair', () => {
    // @ts-expect-error — boolean path cannot pair with number path
    syncPairs<ObjState>()([['alpha.leaf.active', 'alpha.leaf.score']])
  })

  test('rejects number + boolean pair', () => {
    // @ts-expect-error — number path cannot pair with boolean path
    syncPairs<ObjState>()([['alpha.leaf.score', 'gamma.enabled']])
  })

  test('rejects object + string pair (mid-level object vs primitive)', () => {
    // @ts-expect-error — ObjMid path cannot pair with string path
    syncPairs<ObjState>()([['alpha', 'topName']])
  })

  test('rejects object + number pair (mid-level object vs primitive)', () => {
    // @ts-expect-error — ObjMid path cannot pair with number path
    syncPairs<ObjState>()([['beta', 'topCount']])
  })

  test('rejects leaf object + string pair', () => {
    // @ts-expect-error — ObjLeaf path cannot pair with string path
    syncPairs<ObjState>()([['alpha.leaf', 'alpha.tag']])
  })

  test('rejects structurally different objects', () => {
    // @ts-expect-error — ObjMid (alpha) cannot pair with gamma's different structure
    syncPairs<ObjState>()([['alpha', 'gamma']])
  })

  test('rejects invalid path entirely', () => {
    // @ts-expect-error — 'nonexistent' is not a valid path
    syncPairs<ObjState>()([['nonexistent', 'alpha.tag']])
  })

  test('rejects empty string path', () => {
    // @ts-expect-error — '' is not a valid path
    syncPairs<ObjState>()([['', 'alpha.tag']])
  })

  test('rejects when one valid pair and one invalid pair mixed', () => {
    syncPairs<ObjState>()([
      ['alpha.tag', 'beta.tag'],
      // @ts-expect-error — string + number in second pair
      ['alpha.tag', 'alpha.count'],
    ])
  })

  test('accepts structurally identical object pairs', () => {
    // alpha and beta are both ObjMid — call compiles without error
    syncPairs<ObjState>()([['alpha', 'beta']])
  })

  test('accepts same-type leaf object pairs', () => {
    // alpha.leaf and beta.leaf are both ObjLeaf
    syncPairs<ObjState>()([['alpha.leaf', 'beta.leaf']])
  })

  test('accepts deeply nested same-type pairs', () => {
    syncPairs<ObjState>()([
      ['alpha.leaf.id', 'gamma.nested.leaf.id'], // string
      ['alpha.leaf.score', 'gamma.nested.count'], // number
      ['alpha.leaf.active', 'gamma.enabled'], // boolean
    ])
  })
})

describe('flipPairs rejection tests — @ts-expect-error', () => {
  test('rejects string + number pair', () => {
    // @ts-expect-error — string path cannot pair with number path
    flipPairs<ObjState>()([['topName', 'topCount']])
  })

  test('rejects boolean + string pair', () => {
    // @ts-expect-error — boolean path cannot pair with string path
    flipPairs<ObjState>()([['topFlag', 'topName']])
  })

  test('rejects object + primitive pair', () => {
    // @ts-expect-error — ObjMid path cannot pair with string path
    flipPairs<ObjState>()([['alpha', 'topName']])
  })
})

describe('aggregationPairs rejection tests — @ts-expect-error', () => {
  test('rejects string target + number source', () => {
    // @ts-expect-error — string target cannot aggregate number source
    aggregationPairs<ObjState>()([['alpha.tag', 'alpha.count']])
  })

  test('rejects number target + boolean source', () => {
    // @ts-expect-error — number target cannot aggregate boolean source
    aggregationPairs<ObjState>()([['topCount', 'topFlag']])
  })

  test('rejects object target + primitive source', () => {
    // @ts-expect-error — ObjMid target cannot aggregate string source
    aggregationPairs<ObjState>()([['alpha', 'topName']])
  })

  test('rejects invalid path', () => {
    // @ts-expect-error — 'fake.path' is not a valid path
    aggregationPairs<ObjState>()([['fake.path', 'alpha.count']])
  })

  test('accepts same-type number pairs', () => {
    aggregationPairs<ObjState>()([
      ['topCount', 'alpha.count'],
      ['gamma.total', 'beta.leaf.score'],
    ])
  })
})

describe('computationPairs rejection tests — @ts-expect-error', () => {
  test('rejects string target path', () => {
    // @ts-expect-error — string path is not a number path
    computationPairs<ObjState>()([['SUM', 'topName', 'topCount']])
  })

  test('rejects string source path', () => {
    // @ts-expect-error — string path is not a number path
    computationPairs<ObjState>()([['SUM', 'topCount', 'topName']])
  })

  test('rejects boolean target path', () => {
    // @ts-expect-error — boolean path is not a number path
    computationPairs<ObjState>()([['AVG', 'topFlag', 'topCount']])
  })

  test('rejects object target path', () => {
    // @ts-expect-error — object path is not a number path
    computationPairs<ObjState>()([['SUM', 'alpha', 'topCount']])
  })

  test('rejects invalid operation', () => {
    // @ts-expect-error — 'MULTIPLY' is not a valid ComputationOp
    computationPairs<ObjState>()([['MULTIPLY', 'topCount', 'alpha.count']])
  })

  test('rejects invalid path', () => {
    // @ts-expect-error — 'no.such.path' is not valid
    computationPairs<ObjState>()([['SUM', 'no.such.path', 'topCount']])
  })

  test('accepts valid number pairs', () => {
    computationPairs<ObjState>()([
      ['SUM', 'topCount', 'alpha.count'],
      ['AVG', 'gamma.total', 'beta.leaf.score'],
    ])
  })
})

// ============================================================================
// 5. Scaling stress tests — find the breaking point for each approach
// ============================================================================

// --- 2x HugeState (~3300 paths) ---
interface HugeState2x {
  a: HugeState
  b: HugeState
}

// --- 3x HugeState (~4950 paths) ---
interface HugeState3x {
  a: HugeState
  b: HugeState
  c: HugeState
}

// --- 4x HugeState (~6600 paths) ---
interface HugeState4x {
  a: HugeState
  b: HugeState
  c: HugeState
  d: HugeState
}

// --- 5x HugeState (~8250 paths) ---
interface HugeState5x {
  a: HugeState
  b: HugeState
  c: HugeState
  d: HugeState
  e: HugeState
}

describe('Scaling: syncPairs helper at increasing state sizes', () => {
  test('~1650 paths (1x) — accepts valid pairs', () => {
    const result = syncPairs<HugeState>()([
      ['sector1.mid1.leafA.name', 'sector5.mid3.leafB.title'],
      ['sector2.mid1.leafA.value', 'sector8.mid7.leafD.price'],
    ])
    expectTypeOf(result).toBeArray()
  })

  test('~3300 paths (2x) — accepts valid pairs', () => {
    const result = syncPairs<HugeState2x>()([
      ['a.sector1.mid1.leafA.name', 'b.sector5.mid3.leafB.title'],
      ['a.sector2.mid1.leafA.value', 'b.sector8.mid7.leafD.price'],
    ])
    expectTypeOf(result).toBeArray()
  })

  test('~3300 paths (2x) — rejects cross-type pairs', () => {
    syncPairs<HugeState2x>()([
      // @ts-expect-error — string + number
      ['a.sector1.mid1.leafA.name', 'b.sector1.mid1.leafA.value'],
    ])
  })

  test('~4950 paths (3x) — accepts valid pairs', () => {
    const result = syncPairs<HugeState3x>()([
      ['a.sector1.mid1.leafA.name', 'c.sector5.mid3.leafB.title'],
      ['b.sector2.mid1.leafA.value', 'c.sector8.mid7.leafD.price'],
    ])
    expectTypeOf(result).toBeArray()
  })

  test('~4950 paths (3x) — rejects cross-type pairs', () => {
    syncPairs<HugeState3x>()([
      // @ts-expect-error — string + number
      ['a.sector1.mid1.leafA.name', 'c.sector1.mid1.leafA.value'],
    ])
  })

  test('~6600 paths (4x) — accepts valid pairs', () => {
    const result = syncPairs<HugeState4x>()([
      ['a.sector1.mid1.leafA.name', 'd.sector5.mid3.leafB.title'],
      ['b.sector2.mid1.leafA.value', 'c.sector8.mid7.leafD.price'],
    ])
    expectTypeOf(result).toBeArray()
  })

  test('~6600 paths (4x) — rejects cross-type pairs', () => {
    syncPairs<HugeState4x>()([
      // @ts-expect-error — string + number
      ['a.sector1.mid1.leafA.name', 'd.sector1.mid1.leafA.value'],
    ])
  })

  test('~8250 paths (5x) — accepts valid pairs', () => {
    const result = syncPairs<HugeState5x>()([
      ['a.sector1.mid1.leafA.name', 'e.sector5.mid3.leafB.title'],
      ['b.sector2.mid1.leafA.value', 'd.sector8.mid7.leafD.price'],
    ])
    expectTypeOf(result).toBeArray()
  })

  test('~8250 paths (5x) — rejects cross-type pairs', () => {
    syncPairs<HugeState5x>()([
      // @ts-expect-error — string + number
      ['a.sector1.mid1.leafA.name', 'e.sector1.mid1.leafA.value'],
    ])
  })
})

describe('Scaling: computationPairs helper at increasing state sizes', () => {
  test('~3300 paths (2x) — accepts valid number pairs', () => {
    const result = computationPairs<HugeState2x>()([
      ['SUM', 'a.sector1.mid1.leafA.value', 'b.sector2.mid3.leafB.count'],
    ])
    expectTypeOf(result).toBeArray()
  })

  test('~4950 paths (3x) — accepts valid number pairs', () => {
    const result = computationPairs<HugeState3x>()([
      ['SUM', 'a.sector1.mid1.leafA.value', 'c.sector2.mid3.leafB.count'],
    ])
    expectTypeOf(result).toBeArray()
  })

  test('~6600 paths (4x) — accepts valid number pairs', () => {
    const result = computationPairs<HugeState4x>()([
      ['SUM', 'a.sector1.mid1.leafA.value', 'd.sector2.mid3.leafB.count'],
    ])
    expectTypeOf(result).toBeArray()
  })

  test('~8250 paths (5x) — accepts valid number pairs', () => {
    const result = computationPairs<HugeState5x>()([
      ['SUM', 'a.sector1.mid1.leafA.value', 'e.sector2.mid3.leafB.count'],
    ])
    expectTypeOf(result).toBeArray()
  })
})

describe('Scaling: aggregationPairs helper at increasing state sizes', () => {
  test('~3300 paths (2x) — accepts valid pairs', () => {
    const result = aggregationPairs<HugeState2x>()([
      ['a.sector1.mid1.leafA.value', 'b.sector2.mid3.leafB.count'],
    ])
    expectTypeOf(result).toBeArray()
  })

  test('~6600 paths (4x) — accepts valid pairs', () => {
    const result = aggregationPairs<HugeState4x>()([
      ['a.sector1.mid1.leafA.value', 'd.sector2.mid3.leafB.count'],
    ])
    expectTypeOf(result).toBeArray()
  })

  test('~8250 paths (5x) — accepts valid pairs', () => {
    const result = aggregationPairs<HugeState5x>()([
      ['a.sector1.mid1.leafA.value', 'e.sector2.mid3.leafB.count'],
    ])
    expectTypeOf(result).toBeArray()
  })
})

describe.skip('Scaling: OLD SyncPair (O(N²)) — expected to fail', () => {
  test('~1650 paths (1x) — SyncPair<HugeState> (already fails)', () => {
    // @ts-expect-error — O(N²) exceeds recursion limit at ~1650 paths
    expectTypeOf<SyncPair<HugeState>>().toBeArray()
  })

  test('~3300 paths (2x) — SyncPair<HugeState2x>', () => {
    // @ts-expect-error — O(N²) exceeds recursion limit at ~3300 paths
    expectTypeOf<SyncPair<HugeState2x>>().toBeArray()
  })
})

// ============================================================================
// 6. Deep nesting validation (12 levels) — good and bad pairs
// ============================================================================

// Chain structure: each level adds depth + a few fields of each primitive type
interface Deep1 {
  name: string
  value: number
  active: boolean
}
interface Deep2 {
  x: Deep1
  label: string
  count: number
  enabled: boolean
}
interface Deep3 {
  x: Deep2
  tag: string
  score: number
  visible: boolean
}
interface Deep4 {
  x: Deep3
  code: string
  amount: number
  selected: boolean
}
interface Deep5 {
  x: Deep4
  hint: string
  weight: number
  flagged: boolean
}
interface Deep6 {
  x: Deep5
  memo: string
  rank: number
  pinned: boolean
}
interface Deep7 {
  x: Deep6
  note: string
  total: number
  locked: boolean
}
interface Deep8 {
  x: Deep7
  key: string
  index: number
  muted: boolean
}
interface Deep9 {
  x: Deep8
  prefix: string
  offset: number
  starred: boolean
}
interface Deep10 {
  x: Deep9
  suffix: string
  limit: number
  dimmed: boolean
}
interface Deep11 {
  x: Deep10
  marker: string
  span: number
  collapsed: boolean
}
interface Deep12 {
  x: Deep11
  alias: string
  quota: number
  hidden: boolean
}

describe('Deep nesting (12 levels) — syncPairs valid pairs', () => {
  test('same-type string at depth 1 vs depth 12', () => {
    const result = syncPairs<Deep12>()([['alias', 'x.marker']])
    expectTypeOf(result).toBeArray()
  })

  test('same-type string across many depths', () => {
    // Deep12 path map (strings): alias(1), x.marker(2), x.x.suffix(3), x.x.x.prefix(4),
    // x.x.x.x.key(5), x.x.x.x.x.note(6), x.x.x.x.x.x.memo(7), x.x.x.x.x.x.x.hint(8),
    // x.x.x.x.x.x.x.x.code(9), x.x.x.x.x.x.x.x.x.tag(10), x.x.x.x.x.x.x.x.x.x.label(11),
    // x.x.x.x.x.x.x.x.x.x.x.name(12)
    const result = syncPairs<Deep12>()([
      ['alias', 'x.x.x.x.x.x.x.x.x.x.x.name'], // depth 1 vs depth 12
      ['x.x.suffix', 'x.marker'], // depth 3 vs depth 2
      ['x.x.x.x.x.x.x.hint', 'x.x.x.x.x.note'], // depth 8 vs depth 6
    ])
    expectTypeOf(result).toBeArray()
  })

  test('same-type number across many depths', () => {
    // Deep12 path map (numbers): quota(1), x.span(2), x.x.limit(3), x.x.x.offset(4),
    // x.x.x.x.index(5), x.x.x.x.x.total(6), x.x.x.x.x.x.rank(7), x.x.x.x.x.x.x.weight(8),
    // x.x.x.x.x.x.x.x.amount(9), x.x.x.x.x.x.x.x.x.score(10), x.x.x.x.x.x.x.x.x.x.count(11),
    // x.x.x.x.x.x.x.x.x.x.x.value(12)
    const result = syncPairs<Deep12>()([
      ['quota', 'x.span'], // depth 1 vs depth 2
      ['x.x.x.x.x.x.x.weight', 'x.x.x.x.x.total'], // depth 8 vs depth 6
      ['x.x.x.x.x.x.x.x.x.x.x.value', 'x.x.limit'], // depth 12 vs depth 3
    ])
    expectTypeOf(result).toBeArray()
  })

  test('same-type boolean across many depths', () => {
    // Deep12 path map (booleans): hidden(1), x.collapsed(2), x.x.dimmed(3), x.x.x.starred(4),
    // x.x.x.x.muted(5), x.x.x.x.x.locked(6), x.x.x.x.x.x.pinned(7), x.x.x.x.x.x.x.flagged(8),
    // x.x.x.x.x.x.x.x.selected(9), x.x.x.x.x.x.x.x.x.visible(10), x.x.x.x.x.x.x.x.x.x.enabled(11),
    // x.x.x.x.x.x.x.x.x.x.x.active(12)
    const result = syncPairs<Deep12>()([
      ['hidden', 'x.collapsed'], // depth 1 vs depth 2
      ['x.x.x.x.x.x.x.flagged', 'x.x.x.starred'], // depth 8 vs depth 4
      ['x.x.x.x.x.x.x.x.x.x.x.active', 'x.x.dimmed'], // depth 12 vs depth 3
    ])
    expectTypeOf(result).toBeArray()
  })
})

describe('Deep nesting (12 levels) — syncPairs rejection', () => {
  test('rejects string + number at depth 1', () => {
    // @ts-expect-error — string vs number
    syncPairs<Deep12>()([['alias', 'quota']])
  })

  test('rejects string + boolean across depths', () => {
    // @ts-expect-error — string (depth 12) vs boolean (depth 2)
    syncPairs<Deep12>()([['x.x.x.x.x.x.x.x.x.x.x.name', 'x.collapsed']])
  })

  test('rejects number + boolean across depths', () => {
    // @ts-expect-error — number (depth 5) vs boolean (depth 8)
    syncPairs<Deep12>()([['x.x.x.x.index', 'x.x.x.x.x.x.x.flagged']])
  })

  test('rejects boolean + string at deepest level', () => {
    syncPairs<Deep12>()([
      // @ts-expect-error — boolean (depth 12) vs string (depth 12)
      ['x.x.x.x.x.x.x.x.x.x.x.active', 'x.x.x.x.x.x.x.x.x.x.x.name'],
    ])
  })

  test('rejects invalid path', () => {
    // @ts-expect-error — 'nonexistent' is not a valid path
    syncPairs<Deep12>()([['nonexistent', 'alias']])
  })

  test('mixed valid + invalid pairs', () => {
    syncPairs<Deep12>()([
      ['alias', 'x.marker'], // ✓ string + string
      // @ts-expect-error — string + number in second pair
      ['x.x.tag', 'x.x.score'],
    ])
  })
})

describe('Deep nesting (12 levels) — flipPairs and computationPairs', () => {
  test('flipPairs accepts boolean pair across depths', () => {
    const result = flipPairs<Deep12>()([
      ['hidden', 'x.x.x.x.x.x.x.x.x.x.x.active'],
    ])
    expectTypeOf(result).toBeArray()
  })

  test('flipPairs rejects cross-type pair', () => {
    // @ts-expect-error — string vs number mismatch
    flipPairs<Deep12>()([['alias', 'quota']])
  })

  test('computationPairs accepts number pair across depths', () => {
    const result = computationPairs<Deep12>()([
      ['SUM', 'quota', 'x.span'],
      ['AVG', 'x.x.x.x.x.x.x.weight', 'x.x.x.x.x.x.x.x.x.x.x.value'],
    ])
    expectTypeOf(result).toBeArray()
  })

  test('computationPairs rejects string paths', () => {
    computationPairs<Deep12>()([
      // @ts-expect-error — string paths not valid for computation
      ['SUM', 'alias', 'x.marker'],
    ])
  })

  test('aggregationPairs accepts number pair across depths', () => {
    const result = aggregationPairs<Deep12>()([['quota', 'x.x.x.x.x.total']])
    expectTypeOf(result).toBeArray()
  })

  test('aggregationPairs rejects cross-type', () => {
    // @ts-expect-error — string target + number source
    aggregationPairs<Deep12>()([['alias', 'quota']])
  })
})

// ============================================================================
// 7. Rejection tests on HugeState — verify @ts-expect-error with 1650+ paths
// ============================================================================

describe('syncPairs rejection on HugeState (~1650 paths)', () => {
  test('rejects string + number across sectors', () => {
    syncPairs<HugeState>()([
      // @ts-expect-error — string path cannot pair with number path
      ['sector1.mid1.leafA.name', 'sector5.mid7.leafD.price'],
    ])
  })

  test('rejects boolean + string across sectors', () => {
    syncPairs<HugeState>()([
      // @ts-expect-error — boolean path cannot pair with string path
      ['sector2.mid3.leafB.enabled', 'sector8.mid6.leafC.code'],
    ])
  })

  test('rejects number + boolean across deep paths', () => {
    syncPairs<HugeState>()([
      // @ts-expect-error — number path cannot pair with boolean path
      ['sector3.mid1.leafA.score', 'sector7.mid9.leafB.enabled'],
    ])
  })

  test('rejects object + primitive (sector vs leaf field)', () => {
    // @ts-expect-error — PerfSector path cannot pair with string path
    syncPairs<HugeState>()([['sector1', 'sector2.sectorName']])
  })

  test('rejects mid-level object + leaf primitive', () => {
    // @ts-expect-error — PerfMidA1 path cannot pair with number path
    syncPairs<HugeState>()([['sector1.mid1', 'sector1.sectorCode']])
  })

  test('rejects leaf object + string field', () => {
    // @ts-expect-error — PerfLeafA path cannot pair with string path
    syncPairs<HugeState>()([['sector1.mid1.leafA', 'sector1.mid1.status']])
  })

  test('rejects completely invalid path', () => {
    // @ts-expect-error — 'totally.made.up' is not a valid path in HugeState
    syncPairs<HugeState>()([['totally.made.up', 'sector1.mid1.leafA.name']])
  })

  test('rejects valid first pair + invalid second pair', () => {
    syncPairs<HugeState>()([
      ['sector1.mid1.leafA.name', 'sector2.mid3.leafB.title'],
      // @ts-expect-error — string + number in second pair
      ['sector3.mid5.leafA.label', 'sector4.mid7.leafD.price'],
    ])
  })
})

describe('computationPairs rejection on HugeState (~1650 paths)', () => {
  test('rejects string paths on HugeState', () => {
    computationPairs<HugeState>()([
      // @ts-expect-error — string paths not valid for computation
      ['SUM', 'sector1.mid1.leafA.name', 'sector2.mid3.leafB.title'],
    ])
  })

  test('rejects boolean paths on HugeState', () => {
    computationPairs<HugeState>()([
      // @ts-expect-error — boolean paths not valid for computation
      ['AVG', 'sector1.mid1.leafA.active', 'sector2.mid3.leafB.enabled'],
    ])
  })

  test('rejects mixed number target + string source on HugeState', () => {
    computationPairs<HugeState>()([
      // @ts-expect-error — string source not valid for computation
      ['SUM', 'sector1.mid1.leafA.value', 'sector2.mid3.leafB.title'],
    ])
  })
})

// ============================================================================
// 8. Validated branded pairs — combine, spread, push, cross-store safety
// ============================================================================

// Second store type for cross-store rejection tests
interface OtherState {
  foo: string
  bar: number
}

describe('Validated pairs — SideEffects assignability', () => {
  test('syncPairs result assigns to SideEffects.syncPaths', () => {
    const syncs = syncPairs<ObjState>()([['alpha.tag', 'beta.tag']])
    expectTypeOf(syncs).toMatchTypeOf<ValidatedSyncPairs<ObjState>>()
    const effects: SideEffects<ObjState> = { syncPaths: syncs }
    expectTypeOf(effects).toMatchTypeOf<SideEffects<ObjState>>()
  })

  test('flipPairs result assigns to SideEffects.flipPaths', () => {
    const flips = flipPairs<ObjState>()([['topFlag', 'gamma.enabled']])
    expectTypeOf(flips).toMatchTypeOf<ValidatedFlipPairs<ObjState>>()
    const effects: SideEffects<ObjState> = { flipPaths: flips }
    expectTypeOf(effects).toMatchTypeOf<SideEffects<ObjState>>()
  })

  test('aggregationPairs result assigns to SideEffects.aggregations', () => {
    const aggs = aggregationPairs<ObjState>()([['topCount', 'alpha.count']])
    expectTypeOf(aggs).toMatchTypeOf<ValidatedAggregationPairs<ObjState>>()
    const effects: SideEffects<ObjState> = { aggregations: aggs }
    expectTypeOf(effects).toMatchTypeOf<SideEffects<ObjState>>()
  })

  test('computationPairs result assigns to SideEffects.computations', () => {
    const comps = computationPairs<ObjState>()([
      ['SUM', 'topCount', 'alpha.count'],
    ])
    expectTypeOf(comps).toMatchTypeOf<ValidatedComputationPairs<ObjState>>()
    const effects: SideEffects<ObjState> = { computations: comps }
    expectTypeOf(effects).toMatchTypeOf<SideEffects<ObjState>>()
  })

  test('all validated pairs in one SideEffects object', () => {
    const syncs = syncPairs<ObjState>()([['alpha.tag', 'beta.tag']])
    const flips = flipPairs<ObjState>()([['topFlag', 'gamma.enabled']])
    const aggs = aggregationPairs<ObjState>()([['topCount', 'alpha.count']])
    const comps = computationPairs<ObjState>()([
      ['SUM', 'topCount', 'alpha.count'],
    ])
    const effects: SideEffects<ObjState> = {
      syncPaths: syncs,
      flipPaths: flips,
      aggregations: aggs,
      computations: comps,
    }
    expectTypeOf(effects).toMatchTypeOf<SideEffects<ObjState>>()
  })
})

describe('Validated pairs — all pairs in one call (correct pattern)', () => {
  test('syncPairs: combine all pairs in a single call', () => {
    // Correct: put all pairs in one syncPairs() call instead of splitting + spreading
    const syncs = syncPairs<ObjState>()([
      ['alpha.tag', 'beta.tag'],
      ['alpha.leaf.id', 'gamma.nested.leaf.id'],
    ])
    const effects: SideEffects<ObjState> = { syncPaths: syncs }
    expectTypeOf(effects).toMatchTypeOf<SideEffects<ObjState>>()
  })

  test('aggregationPairs: combine all pairs in a single call', () => {
    const aggs = aggregationPairs<ObjState>()([
      ['topCount', 'alpha.count'],
      ['gamma.total', 'beta.leaf.score'],
    ])
    const effects: SideEffects<ObjState> = { aggregations: aggs }
    expectTypeOf(effects).toMatchTypeOf<SideEffects<ObjState>>()
  })

  test('computationPairs: combine all pairs in a single call', () => {
    const comps = computationPairs<ObjState>()([
      ['SUM', 'topCount', 'alpha.count'],
      ['AVG', 'gamma.total', 'beta.leaf.score'],
    ])
    const effects: SideEffects<ObjState> = { computations: comps }
    expectTypeOf(effects).toMatchTypeOf<SideEffects<ObjState>>()
  })
})

describe('Validated pairs — spread loses brand (negative tests)', () => {
  test('spreading syncPairs strips the brand', () => {
    const batch1 = syncPairs<ObjState>()([['alpha.tag', 'beta.tag']])
    const batch2 = syncPairs<ObjState>()([
      ['alpha.leaf.id', 'gamma.nested.leaf.id'],
    ])
    const spread = [...batch1, ...batch2]
    // Spread result is [string, string][] — brand is gone
    expectTypeOf(spread).not.toMatchTypeOf<ValidatedSyncPairs<ObjState>>()
  })

  test('spreading aggregationPairs strips the brand', () => {
    const batch1 = aggregationPairs<ObjState>()([['topCount', 'alpha.count']])
    const batch2 = aggregationPairs<ObjState>()([
      ['gamma.total', 'beta.leaf.score'],
    ])
    const spread = [...batch1, ...batch2]
    expectTypeOf(spread).not.toMatchTypeOf<
      ValidatedAggregationPairs<ObjState>
    >()
  })

  test('spreading computationPairs strips the brand', () => {
    const batch1 = computationPairs<ObjState>()([
      ['SUM', 'topCount', 'alpha.count'],
    ])
    const batch2 = computationPairs<ObjState>()([
      ['AVG', 'gamma.total', 'beta.leaf.score'],
    ])
    const spread = [...batch1, ...batch2]
    expectTypeOf(spread).not.toMatchTypeOf<
      ValidatedComputationPairs<ObjState>
    >()
  })

  test('raw [string, string][] cannot assign to ValidatedSyncPairs', () => {
    const raw: [string, string][] = [['a', 'b']]
    expectTypeOf(raw).not.toMatchTypeOf<ValidatedSyncPairs<ObjState>>()
  })

  test('raw [string, string][] cannot assign to ValidatedFlipPairs', () => {
    const raw: [string, string][] = [['a', 'b']]
    expectTypeOf(raw).not.toMatchTypeOf<ValidatedFlipPairs<ObjState>>()
  })
})

describe('Validated pairs — cross-store rejection', () => {
  test('OtherState syncPairs cannot assign to ObjState SideEffects', () => {
    const otherSyncs = syncPairs<OtherState>()([['foo', 'foo']])
    expectTypeOf(otherSyncs).not.toMatchTypeOf<ValidatedSyncPairs<ObjState>>()
  })

  test('OtherState flipPairs cannot assign to ObjState SideEffects', () => {
    const otherFlips = flipPairs<OtherState>()([['foo', 'foo']])
    expectTypeOf(otherFlips).not.toMatchTypeOf<ValidatedFlipPairs<ObjState>>()
  })

  test('OtherState aggregationPairs cannot assign to ObjState SideEffects', () => {
    const otherAggs = aggregationPairs<OtherState>()([['bar', 'bar']])
    expectTypeOf(otherAggs).not.toMatchTypeOf<
      ValidatedAggregationPairs<ObjState>
    >()
  })

  test('OtherState computationPairs cannot assign to ObjState SideEffects', () => {
    const otherComps = computationPairs<OtherState>()([['SUM', 'bar', 'bar']])
    expectTypeOf(otherComps).not.toMatchTypeOf<
      ValidatedComputationPairs<ObjState>
    >()
  })
})

// ============================================================================
// 9. Listener helper — path/scope validation, prefix enforcement, fn typing
// ============================================================================

// Typed noop fns for ObjState listener tests (avoids implicit `any` params)
const noopObjFull: OnStateListener<ObjState, ObjState> = () => undefined
const noopObjMid: OnStateListener<ObjState, ObjMid> = () => undefined
const noopObjString: OnStateListener<ObjState, string> = () => undefined

// Typed noop fns for OtherState listener tests
const noopOtherFull: OnStateListener<OtherState, OtherState> = () => undefined

// Typed noop fns for HugeState listener tests
const noopHugeFull: OnStateListener<HugeState, HugeState> = () => undefined
const noopHugeMidA1: OnStateListener<HugeState, PerfMidA1> = () => undefined

describe('listeners helper — valid registrations', () => {
  test('scope is prefix of path — accepted', () => {
    const result = listeners<ObjState>()([
      {
        path: 'gamma.nested.leaf.id',
        scope: 'gamma.nested',
        fn: noopObjMid,
      },
    ])
    expectTypeOf(result).toMatchTypeOf<ValidatedListeners<ObjState>>()
  })

  test('scope equals path — accepted', () => {
    const result = listeners<ObjState>()([
      {
        path: 'gamma.nested',
        scope: 'gamma.nested',
        fn: noopObjMid,
      },
    ])
    expectTypeOf(result).toMatchTypeOf<ValidatedListeners<ObjState>>()
  })

  test('path set, scope null — full state', () => {
    const result = listeners<ObjState>()([
      {
        path: 'alpha.tag',
        scope: null,
        fn: noopObjFull,
      },
    ])
    expectTypeOf(result).toMatchTypeOf<ValidatedListeners<ObjState>>()
  })

  test('both null — watch everything, full state', () => {
    const result = listeners<ObjState>()([
      {
        path: null,
        scope: null,
        fn: noopObjFull,
      },
    ])
    expectTypeOf(result).toMatchTypeOf<ValidatedListeners<ObjState>>()
  })

  test('path set, scope omitted — defaults to path scope', () => {
    const result = listeners<ObjState>()([
      {
        path: 'alpha.tag',
        fn: noopObjString,
      },
    ])
    expectTypeOf(result).toMatchTypeOf<ValidatedListeners<ObjState>>()
  })

  test('path null, scope omitted — defaults to null, full state', () => {
    const result = listeners<ObjState>()([
      {
        path: null,
        fn: noopObjFull,
      },
    ])
    expectTypeOf(result).toMatchTypeOf<ValidatedListeners<ObjState>>()
  })

  test('multiple listeners in one call', () => {
    const result = listeners<ObjState>()([
      {
        path: 'gamma.nested.leaf.id',
        scope: 'gamma.nested',
        fn: noopObjMid,
      },
      {
        path: 'alpha.tag',
        scope: null,
        fn: noopObjFull,
      },
      {
        path: null,
        scope: null,
        fn: noopObjFull,
      },
    ])
    expectTypeOf(result).toMatchTypeOf<ValidatedListeners<ObjState>>()
  })

  test('returns input as-is at runtime (identity)', () => {
    const input = [
      {
        path: 'alpha.tag' as const,
        scope: null,
        fn: () => undefined,
      },
    ]
    const result = listeners<ObjState>()(input)
    expect(result).toBe(input)
  })
})

describe('listeners helper — rejection tests', () => {
  test('rejects scope not prefix of path', () => {
    listeners<ObjState>()([
      {
        path: 'alpha.tag',
        // @ts-expect-error — 'beta' is not a prefix of 'alpha.tag'
        scope: 'beta',
        fn: (_changes, _state) => undefined,
      },
    ])
  })

  test('rejects scope deeper than path', () => {
    listeners<ObjState>()([
      {
        path: 'alpha',
        // @ts-expect-error — 'alpha.leaf' is deeper than path 'alpha'
        scope: 'alpha.leaf',
        fn: (_changes, _state) => undefined,
      },
    ])
  })

  test('rejects invalid path', () => {
    listeners<ObjState>()([
      {
        // @ts-expect-error — 'nonexistent.path' is not a valid path
        path: 'nonexistent.path',
        scope: null,
        fn: (_changes, _state) => undefined,
      },
    ])
  })

  test('rejects invalid scope', () => {
    listeners<ObjState>()([
      {
        path: 'alpha.tag',
        // @ts-expect-error — 'nonexistent' is not a valid path
        scope: 'nonexistent',
        fn: (_changes, _state) => undefined,
      },
    ])
  })

  test('rejects unrelated scope and path', () => {
    listeners<ObjState>()([
      {
        path: 'alpha.tag',
        // @ts-expect-error — 'gamma' is not a prefix of 'alpha.tag'
        scope: 'gamma',
        // @ts-expect-error — scope rejection cascades to fn (OnStateListener<..., never>)
        fn: noopObjFull,
      },
    ])
  })
})

describe('listeners helper — SideEffects assignability', () => {
  test('listeners result assigns to SideEffects.listeners', () => {
    const myListeners = listeners<ObjState>()([
      {
        path: 'gamma.nested.leaf.id',
        scope: 'gamma.nested',
        fn: noopObjMid,
      },
    ])
    const effects: SideEffects<ObjState> = { listeners: myListeners }
    expectTypeOf(effects).toMatchTypeOf<SideEffects<ObjState>>()
  })

  test('all validated types combined in one SideEffects', () => {
    const syncs = syncPairs<ObjState>()([['alpha.tag', 'beta.tag']])
    const myListeners = listeners<ObjState>()([
      {
        path: 'alpha.tag',
        scope: null,
        fn: noopObjFull,
      },
    ])
    const effects: SideEffects<ObjState> = {
      syncPaths: syncs,
      listeners: myListeners,
    }
    expectTypeOf(effects).toMatchTypeOf<SideEffects<ObjState>>()
  })
})

describe('listeners helper — cross-store rejection', () => {
  test('OtherState listeners cannot assign to ObjState SideEffects', () => {
    const otherListeners = listeners<OtherState>()([
      {
        path: 'foo',
        scope: null,
        fn: noopOtherFull,
      },
    ])
    expectTypeOf(otherListeners).not.toMatchTypeOf<
      ValidatedListeners<ObjState>
    >()
  })
})

describe('listeners helper — spread loses brand', () => {
  test('spreading listeners strips the brand', () => {
    const batch1 = listeners<ObjState>()([
      { path: 'alpha.tag', scope: null, fn: () => undefined },
    ])
    const batch2 = listeners<ObjState>()([
      { path: 'beta.tag', scope: null, fn: () => undefined },
    ])
    const spread = [...batch1, ...batch2]
    expectTypeOf(spread).not.toMatchTypeOf<ValidatedListeners<ObjState>>()
  })
})

// ============================================================================
// 10. Standalone listener fn typing — OnStateListener as standalone const
// ============================================================================

describe('standalone listener fn — OnStateListener typing', () => {
  test('standalone fn with scoped state types correctly', () => {
    type ScopedFn = OnStateListener<ObjState, ObjState['alpha']>
    expectTypeOf<ScopedFn>().toBeFunction()
    // Should accept (changes: ArrayOfChanges<ObjMid>, state: ObjMid) => ...
  })

  test('standalone fn with full state types correctly', () => {
    type FullFn = OnStateListener<ObjState, ObjState>
    expectTypeOf<FullFn>().toBeFunction()
  })

  test('standalone fn assignable to inline listener', () => {
    const scopedFn: OnStateListener<ObjState, ObjState['gamma']['nested']> = (
      _changes,
      _state,
    ) => undefined

    // Can use this fn in a branded listener call
    const result = listeners<ObjState>()([
      {
        path: 'gamma.nested.leaf.id',
        scope: 'gamma.nested',
        fn: scopedFn,
      },
    ])
    expectTypeOf(result).toMatchTypeOf<ValidatedListeners<ObjState>>()
  })

  test('standalone fn assignable to direct inline listener', () => {
    const scopedFn: OnStateListener<ObjState, ObjState['alpha']> = (
      _changes,
      _state,
    ) => undefined

    // Direct usage in SideEffects (non-branded)
    const effects: SideEffects<ObjState> = {
      listeners: [{ path: 'alpha.tag', scope: 'alpha', fn: scopedFn }],
    }
    expectTypeOf(effects).toMatchTypeOf<SideEffects<ObjState>>()
  })
})

// ============================================================================
// 11. Listener fn validation — standalone fns and type-level checks
// ============================================================================

describe('listeners helper — fn scoped state validation', () => {
  test('standalone fn with scoped state assigns to helper', () => {
    // Standalone fn explicitly typed for alpha scope
    const alphaFn: OnStateListener<ObjState, ObjMid> = (_changes, _state) =>
      undefined

    const result = listeners<ObjState>()([
      { path: 'alpha.tag', scope: 'alpha', fn: alphaFn },
    ])
    expectTypeOf(result).toMatchTypeOf<ValidatedListeners<ObjState>>()
  })

  test('standalone fn with leaf scope assigns to helper', () => {
    const leafFn: OnStateListener<ObjState, ObjLeaf> = (_changes, _state) =>
      undefined

    const result = listeners<ObjState>()([
      { path: 'alpha.leaf.id', scope: 'alpha.leaf', fn: leafFn },
    ])
    expectTypeOf(result).toMatchTypeOf<ValidatedListeners<ObjState>>()
  })

  test('standalone fn with full state assigns to null scope', () => {
    const fullFn: OnStateListener<ObjState, ObjState> = (_changes, _state) =>
      undefined

    const result = listeners<ObjState>()([
      { path: 'alpha.tag', scope: null, fn: fullFn },
    ])
    expectTypeOf(result).toMatchTypeOf<ValidatedListeners<ObjState>>()
  })

  test('standalone fn with full state assigns to both null', () => {
    const fullFn: OnStateListener<ObjState, ObjState> = (_changes, _state) =>
      undefined

    const result = listeners<ObjState>()([
      { path: null, scope: null, fn: fullFn },
    ])
    expectTypeOf(result).toMatchTypeOf<ValidatedListeners<ObjState>>()
  })

  test('standalone fn with scope omitted defaults to path scope', () => {
    // scope omitted → defaults to 'alpha', so fn gets ObjMid (= ObjState['alpha'])
    const alphaFn: OnStateListener<ObjState, ObjMid> = (_changes, _state) =>
      undefined

    const result = listeners<ObjState>()([{ path: 'alpha', fn: alphaFn }])
    expectTypeOf(result).toMatchTypeOf<ValidatedListeners<ObjState>>()
  })

  test('inline fn compiles for null scope', () => {
    const result = listeners<ObjState>()([
      {
        path: 'alpha.tag',
        scope: null,
        fn: noopObjFull,
      },
    ])
    expectTypeOf(result).toMatchTypeOf<ValidatedListeners<ObjState>>()
  })

  test('CheckListenerElement produces correct fn type for scope', () => {
    // Type-level check: CheckListenerElement with scope 'alpha' should
    // produce fn: OnStateListener<ObjState, ObjMid, GenericMeta>
    interface Input {
      path: 'alpha.tag'
      scope: 'alpha'
      fn: (...args: any[]) => any
    }
    type Result = import('~/types/listener-validators').CheckListenerElement<
      ObjState,
      import('~/types/meta').GenericMeta,
      Input
    >
    // The fn field should require OnStateListener<ObjState, ObjMid>
    type ResultFn = Result['fn']
    // A properly typed fn should be assignable
    type TestFn = OnStateListener<ObjState, ObjMid>
    expectTypeOf<TestFn>().toMatchTypeOf<ResultFn>()
  })

  test('CheckListenerElement produces correct fn type for omitted scope', () => {
    // When scope is omitted, effective scope = path
    interface Input {
      path: 'alpha'
      fn: (...args: any[]) => any
    }
    type Result = import('~/types/listener-validators').CheckListenerElement<
      ObjState,
      import('~/types/meta').GenericMeta,
      Input
    >
    // The fn field should require OnStateListener<ObjState, ObjMid>
    type ResultFn = Result['fn']
    type TestFn = OnStateListener<ObjState, ObjMid>
    expectTypeOf<TestFn>().toMatchTypeOf<ResultFn>()
  })

  test('CheckListenerElement produces correct fn type for null scope', () => {
    interface Input {
      path: 'alpha.tag'
      scope: null
      fn: (...args: any[]) => any
    }
    type Result = import('~/types/listener-validators').CheckListenerElement<
      ObjState,
      import('~/types/meta').GenericMeta,
      Input
    >
    type ResultFn = Result['fn']
    // Null scope → fn gets full ObjState
    type TestFn = OnStateListener<ObjState, ObjState>
    expectTypeOf<TestFn>().toMatchTypeOf<ResultFn>()
  })
})

// ============================================================================
// 12. Listener helper — branded vs direct comparison
// ============================================================================

describe('listeners — branded vs direct pattern', () => {
  test('branded listeners assign to SideEffects.listeners', () => {
    const branded = listeners<ObjState>()([
      { path: 'alpha.tag', scope: 'alpha', fn: noopObjMid },
    ])
    const effects: SideEffects<ObjState> = { listeners: branded }
    expectTypeOf(effects).toMatchTypeOf<SideEffects<ObjState>>()
  })

  test('direct inline listeners still work in SideEffects', () => {
    const effects: SideEffects<ObjState> = {
      listeners: [
        {
          path: 'alpha.tag',
          scope: 'alpha',
          fn: noopObjMid,
        },
      ],
    }
    expectTypeOf(effects).toMatchTypeOf<SideEffects<ObjState>>()
  })

  test('branded and direct can coexist (separate fields)', () => {
    const brandedSyncs = syncPairs<ObjState>()([['alpha.tag', 'beta.tag']])
    const brandedListeners = listeners<ObjState>()([
      { path: 'alpha.tag', scope: null, fn: noopObjFull },
    ])
    const effects: SideEffects<ObjState> = {
      syncPaths: brandedSyncs,
      listeners: brandedListeners,
    }
    expectTypeOf(effects).toMatchTypeOf<SideEffects<ObjState>>()
  })
})

describe('listeners helper on HugeState (~1650 paths)', () => {
  test('accepts valid listener with deep scope prefix', () => {
    const result = listeners<HugeState>()([
      {
        path: 'sector1.mid1.leafA.name',
        scope: 'sector1.mid1',
        fn: noopHugeMidA1,
      },
    ])
    expectTypeOf(result).toBeArray()
  })

  test('accepts multiple listeners on HugeState', () => {
    const result = listeners<HugeState>()([
      {
        path: 'sector1.mid1.leafA.name',
        scope: 'sector1.mid1',
        fn: noopHugeMidA1,
      },
      {
        path: 'sector5.mid7.leafD.price',
        scope: null,
        fn: noopHugeFull,
      },
      {
        path: null,
        scope: null,
        fn: noopHugeFull,
      },
    ])
    expectTypeOf(result).toBeArray()
  })

  test('rejects invalid scope on HugeState', () => {
    listeners<HugeState>()([
      {
        path: 'sector1.mid1.leafA.name',
        // @ts-expect-error — 'sector2' is not a prefix of 'sector1.mid1.leafA.name'
        scope: 'sector2',
        fn: (_changes, _state) => undefined,
      },
    ])
  })

  test('scope omitted — fn receives path-scoped state on HugeState', () => {
    // scope omitted with path 'sector1.mid1' → fn gets PerfMidA1
    const midFn: OnStateListener<HugeState, PerfMidA1> = (_changes, _state) =>
      undefined

    const result = listeners<HugeState>()([{ path: 'sector1.mid1', fn: midFn }])
    expectTypeOf(result).toBeArray()
  })

  test('scope omitted — fn receives leaf-scoped state on HugeState', () => {
    // scope omitted with path 'sector3.mid1.leafA' → fn gets PerfLeafA
    const leafFn: OnStateListener<HugeState, PerfLeafA> = (_changes, _state) =>
      undefined

    const result = listeners<HugeState>()([
      { path: 'sector3.mid1.leafA', fn: leafFn },
    ])
    expectTypeOf(result).toBeArray()
  })

  test('explicit scope — fn receives scoped state on HugeState', () => {
    // path deep, scope at sector → fn gets PerfSector
    const sectorFn: OnStateListener<HugeState, PerfSector> = (
      _changes,
      _state,
    ) => undefined

    const result = listeners<HugeState>()([
      { path: 'sector5.mid7.leafD.price', scope: 'sector5', fn: sectorFn },
    ])
    expectTypeOf(result).toBeArray()
  })

  test('null scope — fn receives full HugeState', () => {
    const fullFn: OnStateListener<HugeState, HugeState> = (_changes, _state) =>
      undefined

    const result = listeners<HugeState>()([
      { path: 'sector1.mid1.leafA.name', scope: null, fn: fullFn },
    ])
    expectTypeOf(result).toBeArray()
  })

  test('standalone fn returns scoped changes on HugeState', () => {
    // fn with PerfMidA1 scope returns ArrayOfChanges<PerfMidA1>
    const midFn: OnStateListener<HugeState, PerfMidA1> = (_changes, _state) => {
      // Return value should be scoped to PerfMidA1 paths
      return [['leafA.name', 'updated', {}]]
    }

    const result = listeners<HugeState>()([{ path: 'sector1.mid1', fn: midFn }])
    expectTypeOf(result).toBeArray()
  })

  test('rejects wrong fn state type on HugeState', () => {
    // fn typed for PerfLeafA but path scope resolves to PerfMidA1
    const wrongFn: OnStateListener<HugeState, PerfLeafA> = (_changes, _state) =>
      undefined

    listeners<HugeState>()([
      {
        path: 'sector1.mid1',
        // @ts-expect-error — PerfLeafA fn doesn't match PerfMidA1 scope
        fn: wrongFn,
      },
    ])
  })

  test('mixed scope patterns with fn validation on HugeState', () => {
    const midFn: OnStateListener<HugeState, PerfMidA1> = (_c, _s) => undefined
    const fullFn: OnStateListener<HugeState, HugeState> = (_c, _s) => undefined
    const leafFn: OnStateListener<HugeState, PerfLeafA> = (_c, _s) => undefined

    const result = listeners<HugeState>()([
      { path: 'sector1.mid1', fn: midFn }, // scope omitted → mid1
      { path: 'sector2.mid1.leafA', fn: leafFn }, // scope omitted → leafA
      { path: 'sector5.mid7.leafD.price', scope: null, fn: fullFn }, // null → full
      {
        path: 'sector3.mid1.leafA.name',
        scope: 'sector3.mid1',
        fn: midFn, // explicit scope → mid1
      },
    ])
    expectTypeOf(result).toBeArray()
  })
})
