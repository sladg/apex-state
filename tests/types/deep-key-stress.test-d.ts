/**
 * Stress test for DeepKey / DeepValue / DeepKeyFiltered on large deeply nested types.
 *
 * Purpose: Verify that the recursive type utilities do NOT cause OOM, TS2589,
 * or hangs during type-checking with realistic deeply nested state shapes.
 *
 * Finding: With loop-unrolled DeepKey, 18+ levels with wide branching (2-3 children
 * per level) works without TS2589. This test validates deep nesting support.
 */

import { describe, expectTypeOf, test } from 'vitest'

import type { BoolLogic } from '~/types/bool-logic'
import type { DeepKey } from '~/types/deep-key'
import type { DeepKeyFiltered } from '~/types/deep-key-filtered'
import type { DeepValue } from '~/types/deep-value'
import type {
  PathsWithSameValueAs,
  SyncPair,
} from '~/types/paths-of-same-value'

// ============================================================================
// Leaf types (deepest level)
// ============================================================================

interface Leaf {
  id: string
  value: number
  label: string
  active: boolean
  score: number
}

// ============================================================================
// Deep-and-wide state: 10 levels, 2-3 branches per level
// This is the practical limit with wide branching before TS2589.
// ============================================================================

// Level 10 (bottom)
interface D10A {
  leafA: Leaf
  leafB: Leaf
  name: string
  count: number
}

interface D10B {
  leafC: Leaf
  leafD: Leaf
  title: string
  total: number
}

interface D10C {
  leafE: Leaf
  leafF: Leaf
  description: string
  amount: number
}

// Level 9
interface D9A {
  section1: D10A
  section2: D10B
  enabled: boolean
  priority: number
}

interface D9B {
  section3: D10A
  section4: D10C
  visible: boolean
  weight: number
}

interface D9C {
  section5: D10B
  section6: D10C
  locked: boolean
  ratio: number
}

// Level 8
interface D8A {
  groupA: D9A
  groupB: D9B
  tag: string
  index: number
}

interface D8B {
  groupC: D9B
  groupD: D9C
  label: string
  rank: number
}

interface D8C {
  groupE: D9A
  groupF: D9C
  code: string
  level: number
}

// Level 7
interface D7A {
  cluster1: D8A
  cluster2: D8B
  status: string
  metric: number
}

interface D7B {
  cluster3: D8B
  cluster4: D8C
  phase: string
  score: number
}

// Level 6
interface D6A {
  zone1: D7A
  zone2: D7B
  region: string
  capacity: number
}

interface D6B {
  zone3: D7A
  zone4: D7B
  area: string
  throughput: number
}

// Level 5
interface D5A {
  tier1: D6A
  tier2: D6B
  classification: string
  bandwidth: number
}

interface D5B {
  tier3: D6A
  tier4: D6B
  category: string
  latency: number
}

// Level 4
interface D4A {
  ring1: D5A
  ring2: D5B
  identifier: string
  frequency: number
}

interface D4B {
  ring3: D5A
  ring4: D5B
  reference: string
  amplitude: number
}

// Level 3
interface D3A {
  layer1: D4A
  layer2: D4B
  protocol: string
  version: number
  isPublic: boolean
}

interface D3B {
  layer3: D4A
  layer4: D4B
  format: string
  revision: number
  isInternal: boolean
}

// Level 2
interface D2A {
  module1: D3A
  module2: D3B
  department: string
  budget: number
}

interface D2B {
  module3: D3A
  module4: D3B
  unit: string
  allocation: number
}

// Level 1 — Top level (wide state)
interface WideDeepState {
  // Deep branches (2 wide × ~10 deep = significant path explosion)
  enterprise1: D2A
  enterprise2: D2B

  // Wide flat fields at top level
  globalName: string
  globalCount: number
  globalActive: boolean
  globalLabel: string
  globalScore: number
  globalEnabled: boolean

  // Optional fields (?: syntax)
  optionalName?: string
  optionalCount?: number
  optionalFlag?: boolean

  // Nullable fields
  nullableName: string | null
  nullableCount: number | undefined
}

// ============================================================================
// Deep-and-narrow state: 15 levels, single branch (narrow chain)
// Narrow branching allows deeper nesting.
// ============================================================================

interface N15 {
  id: string
  value: number
  flag: boolean
}

interface N14 {
  child: N15
  name: string
  count: number
}

interface N13 {
  child: N14
  label: string
  score: number
}

interface N12 {
  child: N13
  tag: string
  weight: number
}

interface N11 {
  child: N12
  code: string
  rank: number
}

interface N10 {
  child: N11
  status: string
  metric: number
}

interface N9 {
  child: N10
  phase: string
  index: number
}

interface N8 {
  child: N9
  region: string
  capacity: number
}

interface N7 {
  child: N8
  area: string
  throughput: number
}

interface N6 {
  child: N7
  tier: string
  bandwidth: number
}

interface N5 {
  child: N6
  ring: string
  frequency: number
}

interface N4 {
  child: N5
  layer: string
  version: number
}

interface N3 {
  child: N4
  module: string
  build: number
}

interface N2 {
  child: N3
  department: string
  budget: number
}

interface NarrowDeepState {
  root: N2
  topName: string
  topCount: number
  topFlag: boolean
  topOptional?: string
}

// ============================================================================
// Tests — Wide-and-deep (10 levels, 2-3 branches)
// ============================================================================

describe('DeepKey stress — wide branching (10 levels)', () => {
  test('resolves top-level paths', () => {
    type AllPaths = DeepKey<WideDeepState>

    expectTypeOf<'globalName'>().toMatchTypeOf<AllPaths>()
    expectTypeOf<'globalCount'>().toMatchTypeOf<AllPaths>()
    expectTypeOf<'enterprise1'>().toMatchTypeOf<AllPaths>()
    expectTypeOf<'enterprise2'>().toMatchTypeOf<AllPaths>()
  })

  test('rejects invalid paths — proves string is NOT in the union (autocomplete preserved)', () => {
    type AllPaths = DeepKey<WideDeepState>

    // If the depth-limit `string` fallback leaked into the union,
    // ALL of these would pass — which would kill IDE autocomplete.
    expectTypeOf<'nonexistent'>().not.toMatchTypeOf<AllPaths>()
    expectTypeOf<'enterprise1.fake.path'>().not.toMatchTypeOf<AllPaths>()
    expectTypeOf<'totally.made.up.garbage'>().not.toMatchTypeOf<AllPaths>()
  })

  test('resolves paths at depth 5', () => {
    type AllPaths = DeepKey<WideDeepState>

    // enterprise1(D2A) > module1(D3A) > layer1(D4A) > ring1(D5A) > classification
    expectTypeOf<'enterprise1.module1.layer1.ring1.classification'>().toMatchTypeOf<AllPaths>()
    // enterprise2(D2B) > module4(D3B) > layer4(D4B) > ring4(D5B) > category
    expectTypeOf<'enterprise2.module4.layer4.ring4.category'>().toMatchTypeOf<AllPaths>()
  })

  test('resolves paths at depth 10 (leaf fields)', () => {
    type AllPaths = DeepKey<WideDeepState>

    // enterprise1 > module1 > layer1 > ring1 > tier1 > zone1 > cluster1 > groupA > section1 > leafA > id
    expectTypeOf<'enterprise1.module1.layer1.ring1.tier1.zone1.cluster1.groupA.section1.leafA.id'>().toMatchTypeOf<AllPaths>()
    expectTypeOf<'enterprise1.module1.layer1.ring1.tier1.zone1.cluster1.groupA.section1.leafA.score'>().toMatchTypeOf<AllPaths>()
  })

  test('resolves paths through second branch at depth 10', () => {
    type AllPaths = DeepKey<WideDeepState>

    // enterprise2(D2B) > module4(D3B) > layer4(D4B) > ring4(D5B) > tier4(D6B) > zone4(D7B) > cluster4(D8C) > groupF(D9C) > section6(D10C) > leafF(Leaf) > active
    expectTypeOf<'enterprise2.module4.layer4.ring4.tier4.zone4.cluster4.groupF.section6.leafF.active'>().toMatchTypeOf<AllPaths>()
  })

  test('resolves optional and nullable fields', () => {
    type AllPaths = DeepKey<WideDeepState>

    expectTypeOf<'optionalName'>().toMatchTypeOf<AllPaths>()
    expectTypeOf<'optionalCount'>().toMatchTypeOf<AllPaths>()
    expectTypeOf<'nullableName'>().toMatchTypeOf<AllPaths>()
    expectTypeOf<'nullableCount'>().toMatchTypeOf<AllPaths>()
  })
})

describe('DeepValue stress — wide branching (10 levels)', () => {
  test('resolves leaf string at depth 10', () => {
    type Val = DeepValue<
      WideDeepState,
      'enterprise1.module1.layer1.ring1.tier1.zone1.cluster1.groupA.section1.leafA.id'
    >

    expectTypeOf<Val>().toEqualTypeOf<string>()
  })

  test('resolves leaf number at depth 10', () => {
    type Val = DeepValue<
      WideDeepState,
      'enterprise1.module1.layer1.ring1.tier1.zone1.cluster1.groupA.section1.leafA.score'
    >

    expectTypeOf<Val>().toEqualTypeOf<number>()
  })

  test('resolves leaf boolean at depth 10', () => {
    type Val = DeepValue<
      WideDeepState,
      'enterprise1.module1.layer1.ring1.tier1.zone1.cluster1.groupA.section1.leafA.active'
    >

    expectTypeOf<Val>().toEqualTypeOf<boolean>()
  })

  test('resolves intermediate object at depth 5', () => {
    type Val = DeepValue<
      WideDeepState,
      'enterprise1.module1.layer1.ring1.tier1'
    >

    expectTypeOf<Val>().toEqualTypeOf<D6A>()
  })

  test('resolves optional field type', () => {
    type Val = DeepValue<WideDeepState, 'optionalName'>

    expectTypeOf<Val>().toEqualTypeOf<string | undefined>()
  })
})

describe('DeepKeyFiltered stress — wide branching (10 levels)', () => {
  test('filters boolean paths', () => {
    type BoolPaths = DeepKeyFiltered<WideDeepState, boolean>

    // Top-level booleans
    expectTypeOf<'globalActive'>().toMatchTypeOf<BoolPaths>()
    expectTypeOf<'globalEnabled'>().toMatchTypeOf<BoolPaths>()
    expectTypeOf<'optionalFlag'>().toMatchTypeOf<BoolPaths>()

    // Nested booleans
    expectTypeOf<'enterprise1.module1.isPublic'>().toMatchTypeOf<BoolPaths>()
    expectTypeOf<'enterprise2.module4.isInternal'>().toMatchTypeOf<BoolPaths>()

    // Deep leaf boolean
    expectTypeOf<'enterprise1.module1.layer1.ring1.tier1.zone1.cluster1.groupA.section1.leafA.active'>().toMatchTypeOf<BoolPaths>()

    // String path must NOT be in boolean paths
    expectTypeOf<'globalName'>().not.toMatchTypeOf<BoolPaths>()
  })

  test('filters number paths', () => {
    type NumPaths = DeepKeyFiltered<WideDeepState, number>

    expectTypeOf<'globalCount'>().toMatchTypeOf<NumPaths>()
    expectTypeOf<'optionalCount'>().toMatchTypeOf<NumPaths>()
    expectTypeOf<'nullableCount'>().toMatchTypeOf<NumPaths>()

    // Deep leaf number
    expectTypeOf<'enterprise1.module1.layer1.ring1.tier1.zone1.cluster1.groupA.section1.leafA.score'>().toMatchTypeOf<NumPaths>()

    // Boolean path must NOT be in number paths
    expectTypeOf<'globalActive'>().not.toMatchTypeOf<NumPaths>()
  })
})

describe('PathsWithSameValueAs stress — wide branching (10 levels)', () => {
  test('finds matching string paths across deep branches', () => {
    type NamePaths = PathsWithSameValueAs<WideDeepState, 'globalName'>

    // Other top-level strings
    expectTypeOf<'globalLabel'>().toMatchTypeOf<NamePaths>()

    // Optional string matches
    expectTypeOf<'optionalName'>().toMatchTypeOf<NamePaths>()

    // Nullable string matches
    expectTypeOf<'nullableName'>().toMatchTypeOf<NamePaths>()

    // Deep leaf string
    expectTypeOf<'enterprise1.module1.layer1.ring1.tier1.zone1.cluster1.groupA.section1.leafA.id'>().toMatchTypeOf<NamePaths>()

    // Number path must NOT match
    expectTypeOf<'globalCount'>().not.toMatchTypeOf<NamePaths>()
  })

  test('finds matching number paths across deep branches', () => {
    type CountPaths = PathsWithSameValueAs<WideDeepState, 'globalCount'>

    expectTypeOf<'globalScore'>().toMatchTypeOf<CountPaths>()
    expectTypeOf<'optionalCount'>().toMatchTypeOf<CountPaths>()
    expectTypeOf<'nullableCount'>().toMatchTypeOf<CountPaths>()

    // String path must NOT match
    expectTypeOf<'globalName'>().not.toMatchTypeOf<CountPaths>()
  })
})

// ============================================================================
// Tests — Narrow-and-deep (15 levels, single branch)
// ============================================================================

describe('DeepKey stress — narrow branching (15 levels)', () => {
  test('resolves top-level paths', () => {
    type AllPaths = DeepKey<NarrowDeepState>

    expectTypeOf<'topName'>().toMatchTypeOf<AllPaths>()
    expectTypeOf<'root'>().toMatchTypeOf<AllPaths>()
  })

  test('resolves paths at depth 5', () => {
    type AllPaths = DeepKey<NarrowDeepState>

    // root(N2) > child(N3) > child(N4) > child(N5) > child(N6) > tier
    expectTypeOf<'root.child.child.child.child.tier'>().toMatchTypeOf<AllPaths>()
  })

  test('resolves paths at depth 10', () => {
    type AllPaths = DeepKey<NarrowDeepState>

    // root > child(N3) > child(N4) > child(N5) > child(N6) > child(N7) > child(N8) > child(N9) > child(N10) > child(N11) > code
    expectTypeOf<'root.child.child.child.child.child.child.child.child.child.code'>().toMatchTypeOf<AllPaths>()
  })

  test('resolves paths at depth 15 (deepest leaf)', () => {
    type AllPaths = DeepKey<NarrowDeepState>

    // root(N2) > child(N3) > child(N4) > child(N5) > child(N6) > child(N7) > child(N8) > child(N9) > child(N10) > child(N11) > child(N12) > child(N13) > child(N14) > child(N15) > id
    expectTypeOf<'root.child.child.child.child.child.child.child.child.child.child.child.child.child.id'>().toMatchTypeOf<AllPaths>()
  })
})

describe('DeepValue stress — narrow branching (15 levels)', () => {
  test('resolves leaf string at depth 15', () => {
    type Val = DeepValue<
      NarrowDeepState,
      'root.child.child.child.child.child.child.child.child.child.child.child.child.child.id'
    >

    expectTypeOf<Val>().toEqualTypeOf<string>()
  })

  test('resolves leaf number at depth 15', () => {
    type Val = DeepValue<
      NarrowDeepState,
      'root.child.child.child.child.child.child.child.child.child.child.child.child.child.value'
    >

    expectTypeOf<Val>().toEqualTypeOf<number>()
  })

  test('resolves leaf boolean at depth 15', () => {
    type Val = DeepValue<
      NarrowDeepState,
      'root.child.child.child.child.child.child.child.child.child.child.child.child.child.flag'
    >

    expectTypeOf<Val>().toEqualTypeOf<boolean>()
  })

  test('resolves intermediate type at depth 8', () => {
    type Val = DeepValue<
      NarrowDeepState,
      'root.child.child.child.child.child.child.child'
    >

    // root(N2).child(N3).child(N4).child(N5).child(N6).child(N7).child(N8).child = N9
    expectTypeOf<Val>().toEqualTypeOf<N9>()
  })
})

describe('DeepKeyFiltered stress — narrow branching (15 levels)', () => {
  test('filters boolean paths at depth 15', () => {
    type BoolPaths = DeepKeyFiltered<NarrowDeepState, boolean>

    expectTypeOf<'topFlag'>().toMatchTypeOf<BoolPaths>()
    expectTypeOf<'root.child.child.child.child.child.child.child.child.child.child.child.child.child.flag'>().toMatchTypeOf<BoolPaths>()
    expectTypeOf<'topName'>().not.toMatchTypeOf<BoolPaths>()
  })
})

describe('PathsWithSameValueAs stress — narrow branching (15 levels)', () => {
  test('finds matching string paths at depth 15', () => {
    type NamePaths = PathsWithSameValueAs<NarrowDeepState, 'topName'>

    // Optional string matches
    expectTypeOf<'topOptional'>().toMatchTypeOf<NamePaths>()

    // Deep leaf string
    expectTypeOf<'root.child.child.child.child.child.child.child.child.child.child.child.child.child.id'>().toMatchTypeOf<NamePaths>()

    // Number must NOT match
    expectTypeOf<'topCount'>().not.toMatchTypeOf<NamePaths>()
  })
})

// ============================================================================
// 20-level wide branching — tests the accumulator optimization
// Previously triggered TS2589 "excessively deep" before the optimization.
// ============================================================================

// Extend the wide hierarchy deeper: add levels 1-10 on top of the existing D2A/D2B

interface E2A {
  division1: D2A
  division2: D2B
  department: string
  budget: number
  isPublic: boolean
}

interface E2B {
  division3: D2A
  division4: D2B
  unit: string
  allocation: number
  isInternal: boolean
}

interface E3A {
  org1: E2A
  org2: E2B
  tenant: string
  quota: number
}

interface E3B {
  org3: E2A
  org4: E2B
  workspace: string
  limit: number
}

interface E4A {
  platform1: E3A
  platform2: E3B
  environment: string
  instance: number
}

interface E4B {
  platform3: E3A
  platform4: E3B
  deployment: string
  replica: number
}

interface E5A {
  infra1: E4A
  infra2: E4B
  datacenter: string
  rack: number
}

// 15 levels wide (E5A > E4A > E3A > E2A > D2A > D3A > D4A > D5A > D6A > D7A > D8A > D9A > D10A > Leaf)
interface Ultra15State {
  mega1: E5A
  topName: string
  topCount: number
  topFlag: boolean
}

describe('DeepKey stress — 15-level wide branching (loop-unrolled optimization)', () => {
  test('resolves top-level paths without TS2589', () => {
    type AllPaths = DeepKey<Ultra15State>

    expectTypeOf<'topName'>().toMatchTypeOf<AllPaths>()
    expectTypeOf<'topCount'>().toMatchTypeOf<AllPaths>()
    expectTypeOf<'mega1'>().toMatchTypeOf<AllPaths>()
  })

  test('resolves paths at depth 8', () => {
    type AllPaths = DeepKey<Ultra15State>

    // mega1(E5A) > infra1(E4A) > platform1(E3A) > org1(E2A) > division1(D2A) > module1(D3A) > layer1(D4A) > ring1(D5A) > classification
    expectTypeOf<'mega1.infra1.platform1.org1.division1.module1.layer1.ring1.classification'>().toMatchTypeOf<AllPaths>()
  })

  test('resolves paths at depth 15 (leaf fields)', () => {
    type AllPaths = DeepKey<Ultra15State>

    // mega1(E5A) > infra1(E4A) > platform1(E3A) > org1(E2A) > division1(D2A) > module1(D3A) > layer1(D4A) > ring1(D5A) > tier1(D6A) > zone1(D7A) > cluster1(D8A) > groupA(D9A) > section1(D10A) > leafA(Leaf) > id
    expectTypeOf<'mega1.infra1.platform1.org1.division1.module1.layer1.ring1.tier1.zone1.cluster1.groupA.section1.leafA.id'>().toMatchTypeOf<AllPaths>()
  })

  test('rejects invalid paths at 15-level depth — autocomplete preserved', () => {
    type AllPaths = DeepKey<Ultra15State>

    expectTypeOf<'nonexistent'>().not.toMatchTypeOf<AllPaths>()
    expectTypeOf<'mega1.fake'>().not.toMatchTypeOf<AllPaths>()
    expectTypeOf<'mega1.infra1.platform1.org1.division1.module1.layer1.ring1.tier1.zone1.cluster1.groupA.section1.leafA.NOPE'>().not.toMatchTypeOf<AllPaths>()
  })
})

describe('DeepValue stress — 15-level wide branching', () => {
  test('resolves leaf string at depth 15', () => {
    type Val = DeepValue<
      Ultra15State,
      'mega1.infra1.platform1.org1.division1.module1.layer1.ring1.tier1.zone1.cluster1.groupA.section1.leafA.id'
    >

    expectTypeOf<Val>().toEqualTypeOf<string>()
  })

  test('resolves leaf number at depth 15', () => {
    type Val = DeepValue<
      Ultra15State,
      'mega1.infra1.platform1.org1.division1.module1.layer1.ring1.tier1.zone1.cluster1.groupA.section1.leafA.score'
    >

    expectTypeOf<Val>().toEqualTypeOf<number>()
  })
})

// ============================================================================
// 18-level wide branching — reserved for future testing
// With loop-unrolled DeepKey, 18 levels avoids TS2589 (recursion depth halved)
// but the ~295K unique paths exceed TypeScript's default heap limit (~4GB).
// 15-level wide branching (~74K paths) is the practical limit.
// ============================================================================

// ============================================================================
// Depth limit marker (??) tests
// When DeepKey exhausts its depth, object paths end with `??` to signal cutoff.
// ============================================================================

describe('DeepKey depth limit marker (??)', () => {
  // Simple 4-level type for testing with reduced depth
  interface L4 {
    x: string
    y: number
  }

  interface L3 {
    child: L4
    name: string
  }

  interface L2 {
    child: L3
    label: string
  }

  interface L1 {
    child: L2
    top: string
  }

  interface ShallowRoot {
    branch: L1
    flag: boolean
  }

  test('no ?? marker when type fits within depth', () => {
    // Default depth 20 — ShallowRoot is only 5 levels, no marker needed
    type AllPaths = DeepKey<ShallowRoot>

    expectTypeOf<'branch.child.child.child.x'>().toMatchTypeOf<AllPaths>()
    expectTypeOf<'branch.child.child.child.y'>().toMatchTypeOf<AllPaths>()

    // ?? should NOT be in the union
    expectTypeOf<'??'>().not.toMatchTypeOf<AllPaths>()
    expectTypeOf<'branch.??'>().not.toMatchTypeOf<AllPaths>()
  })

  test('?? marker appears when depth limit is hit', () => {
    // Depth 4: can enumerate 4 levels (2 calls × 2 levels each)
    // ShallowRoot(1) > branch/L1(2) > child/L2(3) > child/L3(4) > child/L4(5) — L4 is beyond limit
    type ShallowPaths = DeepKey<ShallowRoot, 4>

    // Level 1-4 paths should still be fully typed
    expectTypeOf<'branch'>().toMatchTypeOf<ShallowPaths>()
    expectTypeOf<'flag'>().toMatchTypeOf<ShallowPaths>()
    expectTypeOf<'branch.child'>().toMatchTypeOf<ShallowPaths>()
    expectTypeOf<'branch.top'>().toMatchTypeOf<ShallowPaths>()
    expectTypeOf<'branch.child.child'>().toMatchTypeOf<ShallowPaths>()
    expectTypeOf<'branch.child.label'>().toMatchTypeOf<ShallowPaths>()
    expectTypeOf<'branch.child.child.name'>().toMatchTypeOf<ShallowPaths>()

    // L4 object at depth 5 should produce ?? marker
    expectTypeOf<'branch.child.child.child.??'>().toMatchTypeOf<ShallowPaths>()

    // Leaf fields of L4 should NOT be in union (beyond depth limit)
    expectTypeOf<'branch.child.child.child.x'>().not.toMatchTypeOf<ShallowPaths>()
    expectTypeOf<'branch.child.child.child.y'>().not.toMatchTypeOf<ShallowPaths>()
  })

  test('?? marker with depth 2 — very shallow cutoff', () => {
    type VeryShallowPaths = DeepKey<ShallowRoot, 2>

    // Level 1 paths
    expectTypeOf<'branch'>().toMatchTypeOf<VeryShallowPaths>()
    expectTypeOf<'flag'>().toMatchTypeOf<VeryShallowPaths>()

    // Level 2 paths
    expectTypeOf<'branch.child'>().toMatchTypeOf<VeryShallowPaths>()
    expectTypeOf<'branch.top'>().toMatchTypeOf<VeryShallowPaths>()

    // L2 is an object at depth 3 — should show ??
    expectTypeOf<'branch.child.??'>().toMatchTypeOf<VeryShallowPaths>()

    // Deeper paths should NOT be in union
    expectTypeOf<'branch.child.child'>().not.toMatchTypeOf<VeryShallowPaths>()
    expectTypeOf<'branch.child.label'>().not.toMatchTypeOf<VeryShallowPaths>()
  })

  test('invalid paths still rejected even with ?? marker present', () => {
    type ShallowPaths = DeepKey<ShallowRoot, 4>

    // Random garbage rejected
    expectTypeOf<'nonexistent'>().not.toMatchTypeOf<ShallowPaths>()
    expectTypeOf<'branch.TYPO'>().not.toMatchTypeOf<ShallowPaths>()
    expectTypeOf<'branch.child.FAKE'>().not.toMatchTypeOf<ShallowPaths>()
  })
})

// ============================================================================
// Custom Depth parameter flows through SyncPair, BoolLogic, etc.
// ============================================================================

describe('Depth parameter propagation to dependent types', () => {
  interface SimpleState {
    price: number
    cost: number
    name: string
    active: boolean
    nested: {
      deep: {
        value: number
        label: string
      }
    }
  }

  test('SyncPair accepts custom Depth', () => {
    // Default depth — should include nested paths
    type DefaultSync = SyncPair<SimpleState>
    expectTypeOf<['name', 'nested.deep.label']>().toMatchTypeOf<DefaultSync>()

    // Depth 2 — nested.deep is beyond limit
    type ShallowSync = SyncPair<SimpleState, 2>
    expectTypeOf<['name', 'name']>().toMatchTypeOf<ShallowSync>()
  })

  test('DeepKeyFiltered accepts custom Depth', () => {
    // Default depth — should find deep number paths
    type DefaultNumbers = DeepKeyFiltered<SimpleState, number>
    expectTypeOf<'nested.deep.value'>().toMatchTypeOf<DefaultNumbers>()

    // Depth 2 — only top-level numbers
    type ShallowNumbers = DeepKeyFiltered<SimpleState, number, 2>
    expectTypeOf<'price'>().toMatchTypeOf<ShallowNumbers>()
    expectTypeOf<'cost'>().toMatchTypeOf<ShallowNumbers>()
    // Deep number path should NOT be in shallow filtered
    expectTypeOf<'nested.deep.value'>().not.toMatchTypeOf<ShallowNumbers>()
  })

  test('BoolLogic accepts custom Depth', () => {
    // Default depth — nested paths work
    type DefaultLogic = BoolLogic<SimpleState>
    expectTypeOf<{
      EXISTS: 'nested.deep.value'
    }>().toMatchTypeOf<DefaultLogic>()

    // Depth 2 — top-level paths only
    type ShallowLogic = BoolLogic<SimpleState, 2>
    expectTypeOf<{ EXISTS: 'price' }>().toMatchTypeOf<ShallowLogic>()
  })
})

// ============================================================================
// ?? marker with `any`-typed properties
// When a property is typed as `any`, DeepKey emits `??` to signal unknowable structure.
// ============================================================================

describe('DeepKey ?? marker for any-typed properties', () => {
  interface WithAny {
    name: string
    data: any
    nested: {
      info: any
      label: string
    }
  }

  test('any at top-level property emits ?? marker', () => {
    type Paths = DeepKey<WithAny>

    // Concrete paths work normally
    expectTypeOf<'name'>().toMatchTypeOf<Paths>()
    expectTypeOf<'data'>().toMatchTypeOf<Paths>()
    expectTypeOf<'nested'>().toMatchTypeOf<Paths>()
    expectTypeOf<'nested.label'>().toMatchTypeOf<Paths>()

    // any-typed properties emit ?? marker
    expectTypeOf<'data.??'>().toMatchTypeOf<Paths>()
    expectTypeOf<'nested.info.??'>().toMatchTypeOf<Paths>()
  })

  test('any marker does not leak — invalid paths still rejected', () => {
    type Paths = DeepKey<WithAny>

    expectTypeOf<'fake'>().not.toMatchTypeOf<Paths>()
    expectTypeOf<'data.someKey'>().not.toMatchTypeOf<Paths>()
    expectTypeOf<'nested.info.someKey'>().not.toMatchTypeOf<Paths>()
  })
})

// ============================================================================
// ?? marker with Record<string, V> (hash key) paths
// When Record values contain `any` or depth runs out, ?? should appear after [*].
// ============================================================================

describe('DeepKey ?? marker with hash key (Record) paths', () => {
  interface RecordWithAny {
    items: Record<string, any>
    name: string
  }

  test('Record<string, any> emits [*] then ?? marker', () => {
    type Paths = DeepKey<RecordWithAny>

    // Top-level paths
    expectTypeOf<'name'>().toMatchTypeOf<Paths>()
    expectTypeOf<'items'>().toMatchTypeOf<Paths>()

    // Hash key path for the Record
    expectTypeOf<'items.[*]'>().toMatchTypeOf<Paths>()

    // any-typed Record values emit ?? after the hash key
    expectTypeOf<'items.[*].??'>().toMatchTypeOf<Paths>()
  })

  interface RecordWithNestedAny {
    catalog: Record<
      string,
      {
        title: string
        metadata: any
      }
    >
  }

  test('Record with nested any emits ?? after concrete path through [*]', () => {
    type Paths = DeepKey<RecordWithNestedAny>

    // Hash key into the Record
    expectTypeOf<'catalog.[*]'>().toMatchTypeOf<Paths>()

    // Concrete fields inside Record values
    expectTypeOf<'catalog.[*].title'>().toMatchTypeOf<Paths>()
    expectTypeOf<'catalog.[*].metadata'>().toMatchTypeOf<Paths>()

    // any-typed field emits ??
    expectTypeOf<'catalog.[*].metadata.??'>().toMatchTypeOf<Paths>()
  })

  interface RecordDepthLimit {
    deep: Record<
      string,
      {
        child: {
          value: number
          nested: {
            inner: string
          }
        }
      }
    >
  }

  test('Record paths hit depth limit and emit ??', () => {
    // Depth 4: deep(1) > [*](2) > child(3) > value/nested(4) — nested.inner is beyond
    type ShallowPaths = DeepKey<RecordDepthLimit, 4>

    expectTypeOf<'deep'>().toMatchTypeOf<ShallowPaths>()
    expectTypeOf<'deep.[*]'>().toMatchTypeOf<ShallowPaths>()
    expectTypeOf<'deep.[*].child'>().toMatchTypeOf<ShallowPaths>()
    expectTypeOf<'deep.[*].child.value'>().toMatchTypeOf<ShallowPaths>()
    expectTypeOf<'deep.[*].child.nested'>().toMatchTypeOf<ShallowPaths>()

    // Depth limit marker at the cutoff
    expectTypeOf<'deep.[*].child.nested.??'>().toMatchTypeOf<ShallowPaths>()

    // Beyond depth limit — should NOT be in union
    expectTypeOf<'deep.[*].child.nested.inner'>().not.toMatchTypeOf<ShallowPaths>()
  })

  interface NestedRecords {
    matrix: Record<string, Record<string, any>>
  }

  test('nested Record<string, Record<string, any>> emits [*].[*].??', () => {
    type Paths = DeepKey<NestedRecords>

    expectTypeOf<'matrix'>().toMatchTypeOf<Paths>()
    expectTypeOf<'matrix.[*]'>().toMatchTypeOf<Paths>()
    expectTypeOf<'matrix.[*].[*]'>().toMatchTypeOf<Paths>()

    // Inner Record value is any — emit ??
    expectTypeOf<'matrix.[*].[*].??'>().toMatchTypeOf<Paths>()
  })
})

// ============================================================================
// Regression: `any` must NOT be silently swallowed by `extends Primitive`
//
// Bug: `any extends Primitive | readonly any[]` is true, so any-typed properties
// were silently dropped (returned `never`) instead of emitting `??`.
// Fix: check IsAny<T[K]> BEFORE the Primitive guard at every level.
//
// Each test below targets a specific guard point in DeepKeyUnrolled / Level2 types.
// ============================================================================

describe('Regression: any not silently swallowed at any nesting level', () => {
  // --- Level 1: concrete key with any value ---
  interface Level1Any {
    concrete: string
    wild: any
  }

  test('level 1 concrete key: any value emits ??, not never', () => {
    type Paths = DeepKey<Level1Any>

    expectTypeOf<'wild'>().toMatchTypeOf<Paths>()
    expectTypeOf<'wild.??'>().toMatchTypeOf<Paths>()

    // Must NOT silently drop — wild.?? must be present
    // (before the fix, only 'wild' appeared, no 'wild.??')
    type WildMarker = 'wild.??' extends Paths ? true : false
    expectTypeOf<WildMarker>().toEqualTypeOf<true>()
  })

  // --- Level 1: Record<string, any> ---
  interface Level1RecordAny {
    lookup: Record<string, any>
  }

  test('level 1 Record<string, any>: emits [*].??, not just [*]', () => {
    type Paths = DeepKey<Level1RecordAny>

    expectTypeOf<'lookup.[*]'>().toMatchTypeOf<Paths>()
    expectTypeOf<'lookup.[*].??'>().toMatchTypeOf<Paths>()

    type MarkerPresent = 'lookup.[*].??' extends Paths ? true : false
    expectTypeOf<MarkerPresent>().toEqualTypeOf<true>()
  })

  // --- Level 2: concrete key inside concrete object ---
  interface Level2Any {
    parent: {
      child: any
      name: string
    }
  }

  test('level 2 concrete key: any value inside object emits ??, not never', () => {
    type Paths = DeepKey<Level2Any>

    expectTypeOf<'parent.child'>().toMatchTypeOf<Paths>()
    expectTypeOf<'parent.child.??'>().toMatchTypeOf<Paths>()
    expectTypeOf<'parent.name'>().toMatchTypeOf<Paths>()

    type MarkerPresent = 'parent.child.??' extends Paths ? true : false
    expectTypeOf<MarkerPresent>().toEqualTypeOf<true>()
  })

  // --- Level 2: Record value inside concrete object ---
  interface Level2RecordAny {
    parent: {
      items: Record<string, any>
    }
  }

  test('level 2 Record<string, any> inside object: emits [*].??', () => {
    type Paths = DeepKey<Level2RecordAny>

    expectTypeOf<'parent.items'>().toMatchTypeOf<Paths>()
    expectTypeOf<'parent.items.[*]'>().toMatchTypeOf<Paths>()
    expectTypeOf<'parent.items.[*].??'>().toMatchTypeOf<Paths>()

    type MarkerPresent = 'parent.items.[*].??' extends Paths ? true : false
    expectTypeOf<MarkerPresent>().toEqualTypeOf<true>()
  })

  // --- Level 2: concrete key inside Record value ---
  interface Level2AnyInsideRecord {
    catalog: Record<
      string,
      {
        meta: any
        title: string
      }
    >
  }

  test('level 2 any-typed key inside Record value: emits [*].meta.??', () => {
    type Paths = DeepKey<Level2AnyInsideRecord>

    expectTypeOf<'catalog.[*].meta'>().toMatchTypeOf<Paths>()
    expectTypeOf<'catalog.[*].meta.??'>().toMatchTypeOf<Paths>()
    expectTypeOf<'catalog.[*].title'>().toMatchTypeOf<Paths>()

    type MarkerPresent = 'catalog.[*].meta.??' extends Paths ? true : false
    expectTypeOf<MarkerPresent>().toEqualTypeOf<true>()
  })

  // --- Level 2: Record<string, any> inside Record value ---
  interface Level2RecordAnyInsideRecord {
    grid: Record<string, Record<string, any>>
  }

  test('level 2 Record<string, any> inside Record: emits [*].[*].??', () => {
    type Paths = DeepKey<Level2RecordAnyInsideRecord>

    expectTypeOf<'grid.[*]'>().toMatchTypeOf<Paths>()
    expectTypeOf<'grid.[*].[*]'>().toMatchTypeOf<Paths>()
    expectTypeOf<'grid.[*].[*].??'>().toMatchTypeOf<Paths>()

    type MarkerPresent = 'grid.[*].[*].??' extends Paths ? true : false
    expectTypeOf<MarkerPresent>().toEqualTypeOf<true>()
  })

  // --- Mixed: some keys are any, others are concrete ---
  interface MixedAny {
    config: {
      known: { x: number; y: number }
      unknown: any
      items: Record<string, any>
    }
  }

  test('mixed any and concrete siblings: only any paths get ??', () => {
    type Paths = DeepKey<MixedAny>

    // Concrete paths expand normally
    expectTypeOf<'config.known.x'>().toMatchTypeOf<Paths>()
    expectTypeOf<'config.known.y'>().toMatchTypeOf<Paths>()

    // any-typed paths get ??
    expectTypeOf<'config.unknown.??'>().toMatchTypeOf<Paths>()
    expectTypeOf<'config.items.[*].??'>().toMatchTypeOf<Paths>()

    // Concrete paths must NOT get ??
    type KnownNoMarker = 'config.known.??' extends Paths ? true : false
    expectTypeOf<KnownNoMarker>().toEqualTypeOf<false>()
  })
})
