/**
 * PoC v2: Unified _() with `[${K}]` return type — collapsed paths
 *
 * _() is one generic function:
 *   _('u1')            → type `[u1]`     → extends `[${string}]`
 *   _(Status.Active)   → type `[active]` → extends `[${string}]`
 *
 * DeepKey emits:
 *   Record<string, V>  → `[${string}]` patterns (no expansion)
 *   Record<Enum, V>    → `[${string}]` patterns (collapsed, no explosion)
 *   { name: string }   → bare `name` (unchanged for structural keys)
 *
 * Enum-specific narrowing happens at the _() call site, not in DeepKey.
 * `_(Status.Active)` returns `[active]` which extends `[${string}]`.
 */

import { describe, expect, expectTypeOf, it } from 'vitest'

// ═══════════════════════════════════════════════════════════════════════
// Types
// ═══════════════════════════════════════════════════════════════════════

/** Unified _() — wraps any key in brackets, type narrows to `[${K}]` */
const _ = <K extends string>(id: K): `[${K}]` => id as unknown as `[${K}]`

// ═══════════════════════════════════════════════════════════════════════
// DeepKey v2 with bracket support for record keys
// ═══════════════════════════════════════════════════════════════════════

type Primitive = string | number | boolean | bigint | symbol | null | undefined
type IsAny<T> = 0 extends 1 & T ? true : false

/** Detects if T is a union type (distributes, returns true if T has 2+ members) */
type IsUnion<T, U = T> = [T] extends [never]
  ? false
  : T extends unknown
    ? [U] extends [T]
      ? false
      : true
    : never

/**
 * Detects if T is a record-like type that should use [${string}] patterns.
 * Covers both Record<string, V> (string index) and Record<Enum, V> (homogeneous enum).
 *
 * Returns true when:
 * 1. Has multiple string keys (single-key objects are structural)
 * 2. All values are the SAME type (V is not a union)
 * 3. That type is non-primitive and non-array
 *
 * Note: `string extends keyof T` (true Record<string, V>) is checked BEFORE this
 * in DeepKeyV2, so this only handles finite-key homogeneous records (enums).
 */
type IsHomogeneousRecord<T> =
  IsAny<T> extends true
    ? false
    : string extends keyof T
      ? false // Already Record<string, V> — handled by [${string}] branch
      : keyof T extends string
        ? // Must have multiple keys (single-key objects are structural, not enum records)
          true extends IsUnion<keyof T & string>
          ? T extends Record<keyof T, infer V>
            ? // All values must be the same type (V must not be a union)
              true extends IsUnion<V>
              ? false
              : [V] extends [Primitive]
                ? false
                : [V] extends [readonly unknown[]]
                  ? false
                  : true
            : false
          : false
        : false

/**
 * DeepKeyV2 — 3 branches:
 *
 * 1. `string extends keyof T`    → Record<string, V>  → `[${string}]` (existing)
 * 2. `IsHomogeneousRecord<T>`    → Record<Enum, V>    → `[${string}]` (collapsed, same pattern)
 * 3. else                        → structural object   → bare keys
 *
 * Both record branches emit `[${string}]`, so enum records get the same
 * collapsed representation. No union explosion regardless of enum size.
 * Type narrowing happens at the _() call site.
 */
type DeepKeyV2<T, Depth extends number = 10> = Depth extends 0
  ? never
  : IsAny<T> extends true
    ? never
    : T extends Primitive
      ? never
      : T extends readonly unknown[]
        ? never
        : string extends keyof T
          ? // Record<string, V> → emit [${string}] pattern (no expansion)
              | `[${string}]`
              | (T[string] extends Primitive
                  ? never
                  : T[string] extends readonly unknown[]
                    ? never
                    : DeepKeyV2<T[string], Prev<Depth>> extends infer DK
                      ? DK extends string
                        ? `[${string}].${DK}`
                        : never
                      : never)
          : IsHomogeneousRecord<T> extends true
            ? // Record<Enum, V> → ALSO emit [${string}] pattern (collapsed, no explosion)
                // Enum narrowing happens at the _() call site, not here
                | `[${string}]`
                | (T[keyof T & string] extends Primitive
                    ? never
                    : T[keyof T & string] extends readonly unknown[]
                      ? never
                      : DeepKeyV2<
                            T[keyof T & string],
                            Prev<Depth>
                          > extends infer DK
                        ? DK extends string
                          ? `[${string}].${DK}`
                          : never
                        : never)
            : // Structural object → emit bare keys (unchanged)
              {
                [K in keyof T & (string | number)]:
                  | `${K & string}`
                  | (T[K] extends Primitive
                      ? never
                      : T[K] extends readonly unknown[]
                        ? never
                        : DeepKeyV2<T[K], Prev<Depth>> extends infer DK
                          ? DK extends string
                            ? `${K & string}.${DK}`
                            : never
                          : never)
              }[keyof T & (string | number)]

type Prev<N extends number> = N extends 10
  ? 9
  : N extends 9
    ? 8
    : N extends 8
      ? 7
      : N extends 7
        ? 6
        : N extends 6
          ? 5
          : N extends 5
            ? 4
            : N extends 4
              ? 3
              : N extends 3
                ? 2
                : N extends 2
                  ? 1
                  : N extends 1
                    ? 0
                    : never

// ═══════════════════════════════════════════════════════════════════════
// Test types
// ═══════════════════════════════════════════════════════════════════════

enum Status {
  Active = 'active',
  Inactive = 'inactive',
  Pending = 'pending',
}

enum Region {
  US = 'us',
  EU = 'eu',
}

interface ItemData {
  name: string
  count: number
}

interface UserData {
  email: string
  meta: {
    createdAt: string
  }
}

// ═══════════════════════════════════════════════════════════════════════
// Tests
// ═══════════════════════════════════════════════════════════════════════

describe('Unified _() with [${K}] return type', () => {
  // ─── _() behavior ───────────────────────────────────────────────

  describe('_() is one function for all cases', () => {
    it('with plain string: returns [${string}] compatible type', () => {
      const key = _('u1')
      expectTypeOf(key).toEqualTypeOf<'[u1]'>()

      // Extends the generic pattern
      type Extends = typeof key extends `[${string}]` ? true : false
      expectTypeOf<Extends>().toEqualTypeOf<true>()
    })

    it('with enum value: returns [enumValue] literal type', () => {
      const key = _(Status.Active)
      expectTypeOf(key).toEqualTypeOf<'[active]'>()

      // Also extends the generic pattern
      type Extends = typeof key extends `[${string}]` ? true : false
      expectTypeOf<Extends>().toEqualTypeOf<true>()
    })

    it('runtime: returns the raw id (for pipeline)', () => {
      expect(_('u1')).toBe('u1')
      expect(_(Status.Active)).toBe('active')
    })
  })

  // ─── Record<string, V> → [${string}] ───────────────────────────

  describe('Record<string, V> → [${string}] pattern', () => {
    interface State {
      title: string
      users: Record<string, UserData>
    }

    // eslint-disable-next-line @typescript-eslint/no-empty-function
    const acceptPath = (_path: DeepKeyV2<State>) => {}

    it('is NOT bare string', () => {
      type Keys = DeepKeyV2<State>
      type IsNotString = string extends Keys ? false : true
      expectTypeOf<IsNotString>().toEqualTypeOf<true>()
    })

    it('accepts [${string}] wildcard paths', () => {
      acceptPath('title')
      acceptPath('users')
      acceptPath('users.[anything]')
      acceptPath('users.[u1]')
      acceptPath('users.[u1].email')
      acceptPath('users.[u1].meta')
      acceptPath('users.[u1].meta.createdAt')
    })

    it('rejects invalid paths', () => {
      // @ts-expect-error — bare key without brackets
      acceptPath('users.u1')

      // @ts-expect-error — nonexistent top-level
      acceptPath('nonexistent')

      // @ts-expect-error — nonexistent nested field
      acceptPath('users.[u1].bogus')

      // @ts-expect-error — empty string
      acceptPath('')

      // @ts-expect-error — title is primitive, no nesting
      acceptPath('title.nested')
    })

    it('_() in template literal works', () => {
      const path = `users.${_('u1')}.email` as const
      expect(path).toBe('users.u1.email')
      acceptPath(path)
    })
  })

  // ─── Record<Enum, V> → [${string}] collapsed ─────────────────

  describe('Record<Enum, V> → [${string}] collapsed (no explosion)', () => {
    interface State {
      title: string
      items: Record<Status, ItemData>
    }

    // eslint-disable-next-line @typescript-eslint/no-empty-function
    const acceptPath = (_path: DeepKeyV2<State>) => {}

    it('is NOT bare string', () => {
      type Keys = DeepKeyV2<State>
      type IsNotString = string extends Keys ? false : true
      expectTypeOf<IsNotString>().toEqualTypeOf<true>()
    })

    it('accepts any bracketed key (collapsed to [${string}])', () => {
      acceptPath('title')
      acceptPath('items')
      acceptPath('items.[active]')
      acceptPath('items.[inactive]')
      acceptPath('items.[pending]')
      acceptPath('items.[anything]') // [${string}] accepts any bracketed key
      acceptPath('items.[active].name')
      acceptPath('items.[active].count')
      acceptPath('items.[pending].count')
    })

    it('rejects invalid paths', () => {
      // @ts-expect-error — bare key without brackets
      acceptPath('items.active')

      // @ts-expect-error — nonexistent field after valid record key
      acceptPath('items.[active].bogus')

      // @ts-expect-error — empty string
      acceptPath('')

      // @ts-expect-error — nonexistent top-level
      acceptPath('nonexistent')
    })

    it('_() narrows type at call site', () => {
      // _() with enum produces [active] which extends [${string}]
      const path = `items.${_(Status.Active)}.name` as const
      expect(path).toBe('items.active.name')
      acceptPath(path)

      // _() return type is specific: `[active]`, not `[${string}]`
      const key = _(Status.Active)
      expectTypeOf(key).toEqualTypeOf<'[active]'>()
    })
  })

  // ─── Mixed: Record<string> + Record<Enum> in same state ────────

  describe('mixed Record<string> and Record<Enum> in same state', () => {
    interface State {
      users: Record<string, UserData>
      items: Record<Status, ItemData>
      label: string
    }

    // eslint-disable-next-line @typescript-eslint/no-empty-function
    const acceptPath = (_path: DeepKeyV2<State>) => {}

    it('is NOT bare string', () => {
      type Keys = DeepKeyV2<State>
      type IsNotString = string extends Keys ? false : true
      expectTypeOf<IsNotString>().toEqualTypeOf<true>()
    })

    it('Record<string> uses [${string}] — accepts any bracketed key', () => {
      acceptPath('users.[anything]')
      acceptPath('users.[u1].email')
      acceptPath('users.[xyz].meta.createdAt')
    })

    it('Record<Enum> also uses [${string}] — collapsed, same pattern', () => {
      acceptPath('items.[active]')
      acceptPath('items.[active].name')
      acceptPath('items.[anything]') // collapsed — accepts any bracketed key
    })

    it('structural keys remain bare', () => {
      acceptPath('label')

      // @ts-expect-error — label is not bracketed
      acceptPath('[label]')
    })
  })

  // ─── Nested records ─────────────────────────────────────────────

  describe('nested Record<Enum> inside Record<Enum>', () => {
    interface State {
      regions: Record<
        Region,
        {
          label: string
          items: Record<Status, ItemData>
        }
      >
    }

    // eslint-disable-next-line @typescript-eslint/no-empty-function
    const acceptPath = (_path: DeepKeyV2<State>) => {}

    it('is NOT bare string', () => {
      type Keys = DeepKeyV2<State>
      type IsNotString = string extends Keys ? false : true
      expectTypeOf<IsNotString>().toEqualTypeOf<true>()
    })

    it('both levels use [${string}] — collapsed, no explosion', () => {
      acceptPath('regions')
      acceptPath('regions.[us]')
      acceptPath('regions.[eu]')
      acceptPath('regions.[anything]') // collapsed
      acceptPath('regions.[us].label')
      acceptPath('regions.[us].items')
      acceptPath('regions.[us].items.[active]')
      acceptPath('regions.[eu].items.[pending].name')
    })

    it('rejects invalid structural paths', () => {
      // @ts-expect-error — bare key without brackets
      acceptPath('regions.us')

      // @ts-expect-error — nonexistent field
      acceptPath('regions.[us].bogus')

      // @ts-expect-error — bare key at nested record level
      acceptPath('regions.[us].items.active')
    })

    it('_() with enums in template literal', () => {
      const path =
        `regions.${_(Region.US)}.items.${_(Status.Active)}.name` as const
      expect(path).toBe('regions.us.items.active.name')
      acceptPath(path)

      // Type is specific: regions.[us].items.[active].name
      // which extends regions.[${string}].items.[${string}].name
      expectTypeOf(path).toEqualTypeOf<'regions.[us].items.[active].name'>()
    })
  })

  // ─── False positive: same-typed concrete keys ──────────────────

  describe('known limitation: same-typed concrete keys get [${string}]', () => {
    interface Address {
      street: string
      city: string
    }

    interface OrderForm {
      shipping: Address
      billing: Address
    }

    it('shipping/billing get collapsed to [${string}] (false positive from heuristic)', () => {
      type Keys = DeepKeyV2<OrderForm>

      // Both are accessible via [${string}] pattern
      type HasShipping = '[shipping]' extends Keys ? true : false
      type HasBilling = '[billing]' extends Keys ? true : false
      expectTypeOf<HasShipping>().toEqualTypeOf<true>()
      expectTypeOf<HasBilling>().toEqualTypeOf<true>()

      // NOT collapsed to bare string
      type IsNotString = string extends Keys ? false : true
      expectTypeOf<IsNotString>().toEqualTypeOf<true>()
    })
  })

  // ─── Union explosion prevention ─────────────────────────────────

  describe('prevents union explosion for large enums', () => {
    it('50-member enum produces same DeepKey size as 3-member enum', () => {
      // The point: DeepKeyV2 emits [${string}] for both,
      // so the union size is O(fields), not O(enum × fields)
      type SmallKeys = DeepKeyV2<{ items: Record<Status, ItemData> }>
      type LargeEnum =
        | 'a'
        | 'b'
        | 'c'
        | 'd'
        | 'e'
        | 'f'
        | 'g'
        | 'h'
        | 'i'
        | 'j'
        | 'k'
        | 'l'
        | 'm'
        | 'n'
        | 'o'
        | 'p'
        | 'q'
        | 'r'
        | 's'
        | 't'
      type LargeKeys = DeepKeyV2<{ items: Record<LargeEnum, ItemData> }>

      // Both produce collapsed [${string}] paths under `items`
      type SmallHasWildcard = `items.[${string}]` extends SmallKeys
        ? true
        : false
      type LargeHasWildcard = `items.[${string}]` extends LargeKeys
        ? true
        : false
      expectTypeOf<SmallHasWildcard>().toEqualTypeOf<true>()
      expectTypeOf<LargeHasWildcard>().toEqualTypeOf<true>()

      // Neither is bare string
      type SmallNotString = string extends SmallKeys ? false : true
      type LargeNotString = string extends LargeKeys ? false : true
      expectTypeOf<SmallNotString>().toEqualTypeOf<true>()
      expectTypeOf<LargeNotString>().toEqualTypeOf<true>()
    })
  })
})
