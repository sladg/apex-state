/**
 * Type tests for DeepPartial<T>
 *
 * Key scenario: nullable array properties must have their element types made
 * partial too — the previous implementation treated arrays as plain objects
 * (since arrays extend object) and mapped over their keys, which was wrong.
 */

import { describe, expectTypeOf, test } from 'vitest'

import type { DeepPartial } from '../../src/types'

// ── Fixtures ─────────────────────────────────────────────────────────────────

interface Attribute {
  id: number
  value: string
  active: boolean
}

interface Deal {
  name: string
  score: number
  attributes: (Attribute | null)[] | null | undefined
  tags: string[]
  nested: {
    label: string
    items: Attribute[]
  }
}

type PartialDeal = DeepPartial<Deal>

// ── Top-level properties become optional ─────────────────────────────────────

describe('DeepPartial top-level properties', () => {
  test('plain string property becomes string | undefined', () => {
    expectTypeOf<PartialDeal['name']>().toEqualTypeOf<string | undefined>()
  })

  test('plain number property becomes number | undefined', () => {
    expectTypeOf<PartialDeal['score']>().toEqualTypeOf<number | undefined>()
  })
})

// ── Nullable array: the reported bug ─────────────────────────────────────────

describe('DeepPartial nullable array property', () => {
  // Input:    (Attribute | null)[] | null | undefined
  // Expected: (DeepPartial<Attribute> | null | undefined)[] | null | undefined

  test('nullable array property is still an array (not a mapped object)', () => {
    type AttrArray = PartialDeal['attributes']
    // Must extend an array — before the fix this was a mapped object with array method keys
    expectTypeOf<NonNullable<NonNullable<AttrArray>>>().toMatchTypeOf<
      unknown[]
    >()
  })

  test('array element type does not add undefined (conditional type distributes, not maps)', () => {
    type AttrArray = PartialDeal['attributes']
    type Element = NonNullable<AttrArray> extends (infer E)[] ? E : never
    // With conditional type distribution, DeepPartial<Attribute | null> → DeepPartial<Attribute> | null.
    // undefined is NOT added to element union — only null from the original type is preserved.
    // @ts-expect-error — undefined is not in the element union
    expectTypeOf<undefined>().toEqualTypeOf<Element>()
  })

  test('array element type preserves null from original', () => {
    type AttrArray = PartialDeal['attributes']
    type Element = NonNullable<AttrArray> extends (infer E)[] ? E : never
    // null was in the original element union — must be preserved
    expectTypeOf<null>().toMatchTypeOf<Element>()
  })

  test('array element Attribute properties are made partial', () => {
    type AttrArray = PartialDeal['attributes']
    type Element = NonNullable<AttrArray> extends (infer E)[] ? E : never
    type AttrElement = Exclude<Element, null | undefined>
    // Attribute.id should now be number | undefined
    expectTypeOf<AttrElement['id']>().toEqualTypeOf<number | undefined>()
  })
})

// ── Non-nullable array: string[] ──────────────────────────────────────────────

describe('DeepPartial plain array property', () => {
  test('string[] elements stay string (primitives fall through conditional, no undefined added)', () => {
    type Tags = PartialDeal['tags']
    type Element = NonNullable<Tags> extends (infer E)[] ? E : never
    // DeepPartial<string> = string — primitives hit the `: T` branch, no wrapping
    expectTypeOf<Element>().toEqualTypeOf<string>()
  })

  test('plain array stays an array', () => {
    type Tags = PartialDeal['tags']
    expectTypeOf<NonNullable<Tags>>().toMatchTypeOf<unknown[]>()
  })
})

// ── Nested object ─────────────────────────────────────────────────────────────

describe('DeepPartial nested object', () => {
  test('nested object properties become optional', () => {
    type Nested = PartialDeal['nested']
    type NestedLabel = NonNullable<Nested>['label']
    expectTypeOf<NestedLabel>().toEqualTypeOf<string | undefined>()
  })

  test('nested array elements are made partial', () => {
    type Nested = PartialDeal['nested']
    type Items = NonNullable<Nested>['items']
    type Element = NonNullable<Items> extends (infer E)[] ? E : never
    type AttrElement = Exclude<Element, undefined>
    expectTypeOf<AttrElement['id']>().toEqualTypeOf<number | undefined>()
  })
})

// ── DeepPartial is assignable from partial shapes ─────────────────────────────

describe('DeepPartial accepts partial shapes', () => {
  test('empty object is assignable', () => {
    expectTypeOf<Record<never, never>>().toMatchTypeOf<PartialDeal>()
  })

  test('partial deal with only name is assignable', () => {
    expectTypeOf<{ name: string }>().toMatchTypeOf<PartialDeal>()
  })

  test('fully populated deal is assignable', () => {
    const full: Deal = {
      name: 'Summer Flash Sale — 30% Off Electronics',
      score: 0.94,
      attributes: [
        { id: 1, value: 'Black Friday', active: true },
        { id: 2, value: 'Clearance', active: false },
        null,
      ],
      tags: ['electronics', 'flash-sale', 'featured', 'limited-stock'],
      nested: {
        label: 'Eligible Products',
        items: [
          { id: 101, value: 'Wireless Headphones', active: true },
          { id: 102, value: 'USB-C Hub', active: true },
        ],
      },
    }
    expectTypeOf(full).toMatchTypeOf<PartialDeal>()
  })
})
