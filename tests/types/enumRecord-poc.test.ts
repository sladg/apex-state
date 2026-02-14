/**
 * PoC: EnumRecord<K, V> — enum-keyed Records that collapse to HASH_KEY in DeepKey
 *
 * Problem: Record<Enum, V> where Enum has N members produces N × fields union members
 * in DeepKey, causing type explosion for large enums.
 *
 * Solution: EnumRecord<K, V> = { [P in K]: V } & { [key: string]: V }
 * - The mapped part { [P in K]: V } gives enum-specific autocomplete
 * - The index signature { [key: string]: V } makes `string extends keyof T` true
 * - DeepKey already collapses Record<string, V> to HASH_KEY paths
 * - Zero runtime cost — it's purely a type-level construct
 */

import { describe, expectTypeOf, it } from 'vitest'

import type { DeepKey } from '~/types/deepKey'
import type { DeepValue } from '~/types/deepValue'
import type { HASH_KEY } from '~/types/hashKey'

// ─── The proposed type ───────────────────────────────────────────────

/**
 * EnumRecord<K, V> — a Record indexed by enum K with value V.
 *
 * Unlike plain Record<K, V>, this type collapses to HASH_KEY in DeepKey
 * while preserving enum-specific autocomplete for direct property access.
 *
 * How it works:
 * - { [P in K]: V }    → provides autocomplete for known enum keys
 * - { [key: string]: V } → makes `string extends keyof T` true for DeepKey
 */
// eslint-disable-next-line @typescript-eslint/consistent-indexed-object-style
type EnumRecord<K extends string, V> = { [P in K]: V } & { [key: string]: V }

// ─── Test setup ──────────────────────────────────────────────────────

// Simulates a large enum (50 members would be the same, just longer)
enum Status {
  Active = 'active',
  Inactive = 'inactive',
  Pending = 'pending',
  Suspended = 'suspended',
  Archived = 'archived',
  Deleted = 'deleted',
  Draft = 'draft',
  Published = 'published',
  Reviewing = 'reviewing',
  Approved = 'approved',
}

interface ItemData {
  name: string
  count: number
  meta: {
    createdAt: string
    tags: string[]
  }
}

// ─── Demonstrate the problem ─────────────────────────────────────────

describe('EnumRecord PoC', () => {
  describe('PROBLEM: plain Record<Enum, V> explodes in DeepKey', () => {
    interface ExplodedState {
      items: Record<Status, ItemData>
    }

    it('enumerates every enum member as a separate path', () => {
      type Keys = DeepKey<ExplodedState>

      // Each enum value × each field = 10 × 3 leaf paths + 10 intermediate + 1 root = 41 union members
      // With 50 enum values × deeper nesting, this becomes thousands
      type HasActive = 'items.active' extends Keys ? true : false
      type HasInactive = 'items.inactive' extends Keys ? true : false
      type HasActiveName = 'items.active.name' extends Keys ? true : false
      type HasInactiveName = 'items.inactive.name' extends Keys ? true : false

      expectTypeOf<HasActive>().toEqualTypeOf<true>()
      expectTypeOf<HasInactive>().toEqualTypeOf<true>()
      expectTypeOf<HasActiveName>().toEqualTypeOf<true>()
      expectTypeOf<HasInactiveName>().toEqualTypeOf<true>()

      // No HASH_KEY — every path is expanded
      type HasWildcard = 'items.[*]' extends Keys ? true : false
      expectTypeOf<HasWildcard>().toEqualTypeOf<false>()
    })
  })

  // ─── SOLUTION: EnumRecord ────────────────────────────────────────

  describe('SOLUTION: EnumRecord<Enum, V> collapses to HASH_KEY', () => {
    interface CollapsedState {
      items: EnumRecord<Status, ItemData>
    }

    it('produces HASH_KEY paths instead of expanding enum members', () => {
      type Keys = DeepKey<CollapsedState>

      // Collapsed: only [*] paths, regardless of enum size
      type HasItems = 'items' extends Keys ? true : false
      type HasWildcard = 'items.[*]' extends Keys ? true : false
      type HasWildcardName = 'items.[*].name' extends Keys ? true : false
      type HasWildcardCount = 'items.[*].count' extends Keys ? true : false
      type HasWildcardMeta = 'items.[*].meta' extends Keys ? true : false
      type HasWildcardMetaCreated = 'items.[*].meta.createdAt' extends Keys
        ? true
        : false

      expectTypeOf<HasItems>().toEqualTypeOf<true>()
      expectTypeOf<HasWildcard>().toEqualTypeOf<true>()
      expectTypeOf<HasWildcardName>().toEqualTypeOf<true>()
      expectTypeOf<HasWildcardCount>().toEqualTypeOf<true>()
      expectTypeOf<HasWildcardMeta>().toEqualTypeOf<true>()
      expectTypeOf<HasWildcardMetaCreated>().toEqualTypeOf<true>()
    })

    it('does NOT produce individual enum paths', () => {
      type Keys = DeepKey<CollapsedState>

      // Individual enum members are NOT in the union
      // (they'd still work at runtime via _() helper, but DeepKey doesn't enumerate them)
      type HasActive = 'items.active' extends Keys ? true : false
      type HasActiveName = 'items.active.name' extends Keys ? true : false

      expectTypeOf<HasActive>().toEqualTypeOf<false>()
      expectTypeOf<HasActiveName>().toEqualTypeOf<false>()
    })

    it('DeepValue resolves correctly through HASH_KEY', () => {
      interface CollapsedState {
        items: EnumRecord<Status, ItemData>
      }

      type V1 = DeepValue<CollapsedState, 'items'>
      type V2 = DeepValue<CollapsedState, `items.${HASH_KEY}`>
      type V3 = DeepValue<CollapsedState, `items.${HASH_KEY}.name`>
      type V4 = DeepValue<CollapsedState, `items.${HASH_KEY}.count`>
      type V5 = DeepValue<CollapsedState, `items.${HASH_KEY}.meta.createdAt`>

      expectTypeOf<V1>().toMatchTypeOf<EnumRecord<Status, ItemData>>()
      expectTypeOf<V2>().toEqualTypeOf<ItemData>()
      expectTypeOf<V3>().toEqualTypeOf<string>()
      expectTypeOf<V4>().toEqualTypeOf<number>()
      expectTypeOf<V5>().toEqualTypeOf<string>()
    })
  })

  // ─── Autocomplete preservation ───────────────────────────────────

  describe('preserves enum autocomplete for direct property access', () => {
    it('known enum keys are accessible with full type safety', () => {
      type Items = EnumRecord<Status, ItemData>

      // Direct access with enum keys is fully typed
      type ActiveItem = Items[Status.Active]
      type InactiveItem = Items[Status.Inactive]

      expectTypeOf<ActiveItem>().toEqualTypeOf<ItemData>()
      expectTypeOf<InactiveItem>().toEqualTypeOf<ItemData>()
    })

    it('arbitrary string keys also resolve (from index signature)', () => {
      type Items = EnumRecord<Status, ItemData>

      // Any string key works (from the index signature)
      type ArbitraryItem = Items['anything']
      expectTypeOf<ArbitraryItem>().toEqualTypeOf<ItemData>()
    })

    it('enum keys appear in keyof (autocomplete)', () => {
      type Items = EnumRecord<Status, ItemData>
      type Keys = keyof Items

      // keyof includes string (from index sig) which subsumes the enum keys
      // but IDE autocomplete still shows specific enum values
      type IsString = string extends Keys ? true : false
      expectTypeOf<IsString>().toEqualTypeOf<true>()
    })
  })

  // ─── Works with _() helper ──────────────────────────────────────

  describe('works with existing _() helper', () => {
    it('runtime paths use concrete IDs, type system sees HASH_KEY', () => {
      // At runtime: `items.${_('active')}.name` = "items.active.name"
      // At type level: the path is `items.${HASH_KEY}.name` = "items.[*].name"
      // This already works — no changes needed to _()

      interface CollapsedState {
        items: EnumRecord<Status, ItemData>
      }
      type Keys = DeepKey<CollapsedState>

      type PathWorks = `items.${HASH_KEY}.name` extends Keys ? true : false
      expectTypeOf<PathWorks>().toEqualTypeOf<true>()
    })
  })

  // ─── Nested EnumRecord ──────────────────────────────────────────

  describe('nested EnumRecord', () => {
    enum Region {
      US = 'us',
      EU = 'eu',
      APAC = 'apac',
    }

    interface NestedState {
      regions: EnumRecord<
        Region,
        {
          label: string
          items: EnumRecord<Status, ItemData>
        }
      >
    }

    it('collapses both levels to HASH_KEY', () => {
      type Keys = DeepKey<NestedState>

      type HasRegions = 'regions' extends Keys ? true : false
      type HasRegionWildcard = 'regions.[*]' extends Keys ? true : false
      type HasRegionLabel = 'regions.[*].label' extends Keys ? true : false
      type HasRegionItems = 'regions.[*].items' extends Keys ? true : false
      type HasNestedWildcard = 'regions.[*].items.[*]' extends Keys
        ? true
        : false
      type HasNestedName = 'regions.[*].items.[*].name' extends Keys
        ? true
        : false

      expectTypeOf<HasRegions>().toEqualTypeOf<true>()
      expectTypeOf<HasRegionWildcard>().toEqualTypeOf<true>()
      expectTypeOf<HasRegionLabel>().toEqualTypeOf<true>()
      expectTypeOf<HasRegionItems>().toEqualTypeOf<true>()
      expectTypeOf<HasNestedWildcard>().toEqualTypeOf<true>()
      expectTypeOf<HasNestedName>().toEqualTypeOf<true>()
    })
  })

  // ─── Mixed with concrete keys ────────────────────────────────────

  describe('mixed with concrete keys', () => {
    interface MixedState {
      title: string
      items: EnumRecord<Status, ItemData>
      settings: {
        theme: string
      }
    }

    it('concrete keys work normally, EnumRecord collapses', () => {
      type Keys = DeepKey<MixedState>

      // Concrete keys
      type HasTitle = 'title' extends Keys ? true : false
      type HasSettings = 'settings' extends Keys ? true : false
      type HasTheme = 'settings.theme' extends Keys ? true : false

      // Collapsed enum paths
      type HasItemsWildcard = 'items.[*]' extends Keys ? true : false
      type HasItemsName = 'items.[*].name' extends Keys ? true : false

      expectTypeOf<HasTitle>().toEqualTypeOf<true>()
      expectTypeOf<HasSettings>().toEqualTypeOf<true>()
      expectTypeOf<HasTheme>().toEqualTypeOf<true>()
      expectTypeOf<HasItemsWildcard>().toEqualTypeOf<true>()
      expectTypeOf<HasItemsName>().toEqualTypeOf<true>()
    })
  })

  // ─── Value creation (runtime ergonomics) ─────────────────────────

  describe('value creation ergonomics', () => {
    it('can be created from a plain Record (zero runtime cost)', () => {
      // EnumRecord is structurally compatible with Record<K, V>
      // No wrapper function needed — just a type assertion or satisfies
      const items: EnumRecord<Status, ItemData> = {
        [Status.Active]: {
          name: 'a',
          count: 1,
          meta: { createdAt: '', tags: [] },
        },
        [Status.Inactive]: {
          name: 'b',
          count: 2,
          meta: { createdAt: '', tags: [] },
        },
        [Status.Pending]: {
          name: 'c',
          count: 3,
          meta: { createdAt: '', tags: [] },
        },
        [Status.Suspended]: {
          name: 'd',
          count: 4,
          meta: { createdAt: '', tags: [] },
        },
        [Status.Archived]: {
          name: 'e',
          count: 5,
          meta: { createdAt: '', tags: [] },
        },
        [Status.Deleted]: {
          name: 'f',
          count: 6,
          meta: { createdAt: '', tags: [] },
        },
        [Status.Draft]: {
          name: 'g',
          count: 7,
          meta: { createdAt: '', tags: [] },
        },
        [Status.Published]: {
          name: 'h',
          count: 8,
          meta: { createdAt: '', tags: [] },
        },
        [Status.Reviewing]: {
          name: 'i',
          count: 9,
          meta: { createdAt: '', tags: [] },
        },
        [Status.Approved]: {
          name: 'j',
          count: 10,
          meta: { createdAt: '', tags: [] },
        },
      }

      // Direct enum access works
      expectTypeOf(items[Status.Active]).toEqualTypeOf<ItemData>()
      expectTypeOf(items[Status.Pending]).toEqualTypeOf<ItemData>()
    })
  })
})
