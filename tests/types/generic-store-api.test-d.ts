/**
 * Regression test: GenericStoreApi explicit interface prevents TS length overflow
 *
 * Without the explicit GenericStoreApi interface on createGenericStore's return type,
 * TypeScript tries to inline-expand every O(N²) pair type. With 2800+ DeepKey paths
 * this causes "inferred type of this node exceeds the maximum length capacity".
 *
 * If GenericStoreApi is ever replaced with `ReturnType<typeof createGenericStore<...>>`
 * again, these tests will fail with that error during type-checking.
 */

import { describe, expectTypeOf, test } from 'vitest'

import type { createGenericStore, GenericStoreApi } from '~/store/create-store'

// ============================================================================
// Large state type: ~2,800 DeepKey paths
//
// Structure: 17 sectors × ~164 paths each ≈ 2,788 paths
// Each sector has 10 mid-level objects, each mid has 2 leaf objects.
// ============================================================================

interface Leaf {
  name: string
  value: number
  active: boolean
  label: string
  score: number
}

interface MidA {
  leaf1: Leaf
  leaf2: Leaf
  status: string
  priority: number
  locked: boolean
}

interface MidB {
  leaf1: Leaf
  leaf2: Leaf
  region: string
  level: number
  archived: boolean
  category: string
}

interface MidC {
  leaf1: Leaf
  leaf2: Leaf
  source: string
  version: number
  draft: boolean
}

interface MidD {
  leaf1: Leaf
  leaf2: Leaf
  format: string
  revision: number
  pinned: boolean
  channel: string
}

interface MidE {
  leaf1: Leaf
  leaf2: Leaf
  owner: string
  depth: number
  muted: boolean
}

interface Sector {
  mid1: MidA
  mid2: MidB
  mid3: MidC
  mid4: MidD
  mid5: MidE
  mid6: MidA
  mid7: MidB
  mid8: MidC
  mid9: MidD
  mid10: MidE
  sectorName: string
  sectorCode: number
  sectorActive: boolean
  sectorLabel: string
}

// 17 sectors × ~164 paths each ≈ 2,788 paths
interface VeryLargeState {
  s01: Sector
  s02: Sector
  s03: Sector
  s04: Sector
  s05: Sector
  s06: Sector
  s07: Sector
  s08: Sector
  s09: Sector
  s10: Sector
  s11: Sector
  s12: Sector
  s13: Sector
  s14: Sector
  s15: Sector
  s16: Sector
  s17: Sector
}

// ============================================================================
// Tests
// ============================================================================

describe('GenericStoreApi return type on large DATA (~2800 paths)', () => {
  test('ReturnType resolves without TS length overflow', () => {
    // Core regression test: if GenericStoreApi is removed or replaced with
    // an inferred ReturnType, this line triggers TS "exceeds maximum length".
    type StoreApi = ReturnType<typeof createGenericStore<VeryLargeState>>
    expectTypeOf<StoreApi>().toMatchTypeOf<GenericStoreApi<VeryLargeState>>()
  })

  test('has all expected keys', () => {
    type Api = GenericStoreApi<VeryLargeState>
    type ApiKeys = keyof Api

    // Every method from createGenericStore must be present
    expectTypeOf<'Provider'>().toMatchTypeOf<ApiKeys>()
    expectTypeOf<'useFieldStore'>().toMatchTypeOf<ApiKeys>()
    expectTypeOf<'useStore'>().toMatchTypeOf<ApiKeys>()
    expectTypeOf<'useJitStore'>().toMatchTypeOf<ApiKeys>()
    expectTypeOf<'useSideEffects'>().toMatchTypeOf<ApiKeys>()
    expectTypeOf<'useConcerns'>().toMatchTypeOf<ApiKeys>()
    expectTypeOf<'withConcerns'>().toMatchTypeOf<ApiKeys>()
    expectTypeOf<'withMeta'>().toMatchTypeOf<ApiKeys>()
    expectTypeOf<'syncPairs'>().toMatchTypeOf<ApiKeys>()
    expectTypeOf<'flipPairs'>().toMatchTypeOf<ApiKeys>()
    expectTypeOf<'aggregationPairs'>().toMatchTypeOf<ApiKeys>()
    expectTypeOf<'computationPairs'>().toMatchTypeOf<ApiKeys>()
    expectTypeOf<'listeners'>().toMatchTypeOf<ApiKeys>()
  })

  test('warm pair helpers are callable', () => {
    type Api = GenericStoreApi<VeryLargeState>

    expectTypeOf<Api['syncPairs']>().toBeFunction()
    expectTypeOf<Api['flipPairs']>().toBeFunction()
    expectTypeOf<Api['aggregationPairs']>().toBeFunction()
    expectTypeOf<Api['computationPairs']>().toBeFunction()
    expectTypeOf<Api['listeners']>().toBeFunction()
  })

  test('hooks are callable', () => {
    type Api = GenericStoreApi<VeryLargeState>

    expectTypeOf<Api['useFieldStore']>().toBeFunction()
    expectTypeOf<Api['useStore']>().toBeFunction()
    expectTypeOf<Api['useJitStore']>().toBeFunction()
    expectTypeOf<Api['useSideEffects']>().toBeFunction()
    expectTypeOf<Api['useConcerns']>().toBeFunction()
    expectTypeOf<Api['withConcerns']>().toBeFunction()
    expectTypeOf<Api['withMeta']>().toBeFunction()
  })
})
