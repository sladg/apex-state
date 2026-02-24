/**
 * Type narrowing tests for the `is` utility
 *
 * Verifies that all type guards produce correct TypeScript narrowings
 * when used in conditional branches.
 */

import { describe, expectTypeOf, test } from 'vitest'

import { is } from '~/utils/is'

// ============================================================================
// Positive guards — narrow to the specific type
// ============================================================================

describe('is.nil', () => {
  test('narrows to null | undefined in true branch', () => {
    const value = 'hello' as string | null | undefined
    if (is.nil(value)) {
      expectTypeOf(value).toEqualTypeOf<null | undefined>()
    }
  })

  test('narrows out null and undefined in false branch', () => {
    const value = 'hello' as string | null | undefined
    if (!is.nil(value)) {
      expectTypeOf(value).toEqualTypeOf<string>()
    }
  })
})

describe('is.undefined', () => {
  test('narrows to undefined in true branch', () => {
    const value = 'hello' as string | undefined
    if (is.undefined(value)) {
      expectTypeOf(value).toEqualTypeOf<undefined>()
    }
  })

  test('narrows out undefined in false branch', () => {
    const value = 'hello' as string | undefined
    if (!is.undefined(value)) {
      expectTypeOf(value).toEqualTypeOf<string>()
    }
  })
})

describe('is.null', () => {
  test('narrows to null in true branch', () => {
    const value = 'hello' as string | null
    if (is.null(value)) {
      expectTypeOf(value).toEqualTypeOf<null>()
    }
  })

  test('narrows out null in false branch', () => {
    const value = 'hello' as string | null
    if (!is.null(value)) {
      expectTypeOf(value).toEqualTypeOf<string>()
    }
  })
})

describe('is.string', () => {
  test('narrows to string in true branch', () => {
    const value = 'hello' as string | number | null
    if (is.string(value)) {
      expectTypeOf(value).toEqualTypeOf<string>()
    }
  })

  test('narrows out string in false branch', () => {
    const value = 'hello' as string | number | null
    if (!is.string(value)) {
      expectTypeOf(value).toEqualTypeOf<number | null>()
    }
  })
})

describe('is.number', () => {
  test('narrows to number in true branch', () => {
    const value = 42 as string | number | boolean
    if (is.number(value)) {
      expectTypeOf(value).toEqualTypeOf<number>()
    }
  })

  test('narrows out number in false branch', () => {
    const value = 42 as string | number | boolean
    if (!is.number(value)) {
      expectTypeOf(value).toEqualTypeOf<string | boolean>()
    }
  })
})

describe('is.boolean', () => {
  test('narrows to boolean in true branch', () => {
    const value = true as string | number | boolean
    if (is.boolean(value)) {
      expectTypeOf(value).toEqualTypeOf<boolean>()
    }
  })

  test('narrows out boolean in false branch', () => {
    const value = true as string | number | boolean
    if (!is.boolean(value)) {
      expectTypeOf(value).toEqualTypeOf<string | number>()
    }
  })
})

describe('is.symbol', () => {
  test('narrows to symbol in true branch', () => {
    const value = Symbol() as string | symbol
    if (is.symbol(value)) {
      expectTypeOf(value).toEqualTypeOf<symbol>()
    }
  })

  test('narrows out symbol in false branch', () => {
    const value = Symbol() as string | symbol
    if (!is.symbol(value)) {
      expectTypeOf(value).toEqualTypeOf<string>()
    }
  })
})

describe('is.function', () => {
  test('narrows to function signature in true branch', () => {
    const value = (() => 0) as string | ((...args: unknown[]) => unknown)
    if (is.function(value)) {
      expectTypeOf(value).toEqualTypeOf<(...args: unknown[]) => unknown>()
    }
  })

  test('narrows out function in false branch', () => {
    const value = (() => 0) as string | ((...args: unknown[]) => unknown)
    if (!is.function(value)) {
      expectTypeOf(value).toEqualTypeOf<string>()
    }
  })
})

describe('is.date', () => {
  test('narrows to Date in true branch', () => {
    const value = new Date() as Date | string
    if (is.date(value)) {
      expectTypeOf(value).toEqualTypeOf<Date>()
    }
  })

  test('narrows out Date in false branch', () => {
    const value = new Date() as Date | string
    if (!is.date(value)) {
      expectTypeOf(value).toEqualTypeOf<string>()
    }
  })
})

describe('is.regexp', () => {
  test('narrows to RegExp in true branch', () => {
    const value = /test/ as RegExp | string
    if (is.regexp(value)) {
      expectTypeOf(value).toEqualTypeOf<RegExp>()
    }
  })

  test('narrows out RegExp in false branch', () => {
    const value = /test/ as RegExp | string
    if (!is.regexp(value)) {
      expectTypeOf(value).toEqualTypeOf<string>()
    }
  })
})

describe('is.object', () => {
  test('narrows to Record<string, unknown> in true branch', () => {
    const value = {} as Record<string, unknown> | unknown[] | string
    if (is.object(value)) {
      expectTypeOf(value).toEqualTypeOf<Record<string, unknown>>()
    }
  })

  test('narrows out Record in false branch', () => {
    const value = {} as Record<string, unknown> | unknown[] | string
    if (!is.object(value)) {
      expectTypeOf(value).toEqualTypeOf<unknown[] | string>()
    }
  })
})

describe('is.array', () => {
  test('narrows to unknown[] in true branch', () => {
    const value = [] as Record<string, unknown> | unknown[] | string
    if (is.array(value)) {
      expectTypeOf(value).toEqualTypeOf<unknown[]>()
    }
  })

  test('narrows out array in false branch', () => {
    const value = [] as Record<string, unknown> | unknown[] | string
    if (!is.array(value)) {
      expectTypeOf(value).toEqualTypeOf<Record<string, unknown> | string>()
    }
  })
})

describe('is.objectOrArray', () => {
  test('narrows to Record | unknown[] in true branch', () => {
    const value = {} as Record<string, unknown> | unknown[] | string
    if (is.objectOrArray(value)) {
      expectTypeOf(value).toEqualTypeOf<Record<string, unknown> | unknown[]>()
    }
  })

  test('narrows out both object and array in false branch', () => {
    const value = {} as Record<string, unknown> | unknown[] | string
    if (!is.objectOrArray(value)) {
      expectTypeOf(value).toEqualTypeOf<string>()
    }
  })
})

describe('is.primitive', () => {
  test('narrows to Primitive in true branch', () => {
    const value = 'hello' as string | Date | object
    if (is.primitive(value)) {
      expectTypeOf(value).toEqualTypeOf<string>()
    }
  })

  test('narrows out Primitive in false branch', () => {
    const value = 'hello' as string | Date
    if (!is.primitive(value)) {
      expectTypeOf(value).toEqualTypeOf<Date>()
    }
  })
})

// ============================================================================
// Negated guards (is.not.*) — Exclude<T, Type> narrowing
// ============================================================================

describe('is.not.nil', () => {
  test('narrows out null and undefined', () => {
    const value = 'hello' as string | null | undefined
    if (is.not.nil(value)) {
      expectTypeOf(value).toEqualTypeOf<string>()
    }
  })
})

describe('is.not.undefined', () => {
  test('narrows out undefined', () => {
    const value = 'hello' as string | number | undefined
    if (is.not.undefined(value)) {
      expectTypeOf(value).toEqualTypeOf<string | number>()
    }
  })
})

describe('is.not.null', () => {
  test('narrows out null', () => {
    const value = 'hello' as string | number | null
    if (is.not.null(value)) {
      expectTypeOf(value).toEqualTypeOf<string | number>()
    }
  })
})

describe('is.not.string', () => {
  test('excludes string via Exclude<T, string>', () => {
    const value = 'hello' as string | number | boolean
    if (is.not.string(value)) {
      expectTypeOf(value).toEqualTypeOf<number | boolean>()
    }
  })
})

describe('is.not.number', () => {
  test('excludes number via Exclude<T, number>', () => {
    const value = 42 as string | number | boolean
    if (is.not.number(value)) {
      expectTypeOf(value).toEqualTypeOf<string | boolean>()
    }
  })
})

describe('is.not.boolean', () => {
  test('excludes boolean via Exclude<T, boolean>', () => {
    const value = true as string | number | boolean
    if (is.not.boolean(value)) {
      expectTypeOf(value).toEqualTypeOf<string | number>()
    }
  })
})

describe('is.not.symbol', () => {
  test('excludes symbol via Exclude<T, symbol>', () => {
    const value = Symbol() as string | symbol | number
    if (is.not.symbol(value)) {
      expectTypeOf(value).toEqualTypeOf<string | number>()
    }
  })
})

describe('is.not.function', () => {
  test('excludes function via Exclude<T, (...args) => unknown>', () => {
    const value = (() => 0) as string | ((...args: unknown[]) => unknown)
    if (is.not.function(value)) {
      expectTypeOf(value).toEqualTypeOf<string>()
    }
  })
})

describe('is.not.date', () => {
  test('excludes Date via Exclude<T, Date>', () => {
    const value = new Date() as Date | string | number
    if (is.not.date(value)) {
      expectTypeOf(value).toEqualTypeOf<string | number>()
    }
  })
})

describe('is.not.regexp', () => {
  test('excludes RegExp via Exclude<T, RegExp>', () => {
    const value = /test/ as RegExp | string | number
    if (is.not.regexp(value)) {
      expectTypeOf(value).toEqualTypeOf<string | number>()
    }
  })
})

describe('is.not.object', () => {
  test('excludes Record via Exclude<T, Record<string, unknown>>', () => {
    const value = {} as Record<string, unknown> | string | number
    if (is.not.object(value)) {
      expectTypeOf(value).toEqualTypeOf<string | number>()
    }
  })
})

describe('is.not.array', () => {
  test('excludes unknown[] via Exclude<T, unknown[]>', () => {
    const value = [] as unknown[] | string | number
    if (is.not.array(value)) {
      expectTypeOf(value).toEqualTypeOf<string | number>()
    }
  })
})

describe('is.not.objectOrArray', () => {
  test('excludes both Record and unknown[] via Exclude', () => {
    const value = {} as Record<string, unknown> | unknown[] | string | number
    if (is.not.objectOrArray(value)) {
      expectTypeOf(value).toEqualTypeOf<string | number>()
    }
  })
})

describe('is.not.primitive', () => {
  test('excludes Primitive via Exclude<T, Primitive>', () => {
    const value = 'hello' as string | Date | Record<string, unknown>
    if (is.not.primitive(value)) {
      expectTypeOf(value).toEqualTypeOf<Date | Record<string, unknown>>()
    }
  })
})

// ============================================================================
// Practical narrowing — simulates real usage patterns
// ============================================================================

describe('practical narrowing patterns', () => {
  test('filter array of mixed types to only strings', () => {
    const values = ['a', 1, true, null, 'b'] as (
      | string
      | number
      | boolean
      | null
    )[]
    const strings = values.filter(is.string)
    expectTypeOf(strings).toEqualTypeOf<string[]>()
  })

  test('filter array to non-nil values', () => {
    const values = ['a', null, 'b', undefined] as (string | null | undefined)[]
    const nonNil = values.filter(is.not.nil)
    expectTypeOf(nonNil).toEqualTypeOf<string[]>()
  })

  test('filter array to non-null values', () => {
    const values = [1, null, 2, null] as (number | null)[]
    const nonNull = values.filter(is.not.null)
    expectTypeOf(nonNull).toEqualTypeOf<number[]>()
  })

  test('narrows primitives in conditional branch (is.primitive takes unknown, not generic)', () => {
    // is.primitive is typed as (value: unknown): value is Primitive — not generic.
    // Array.filter's narrowing overload requires S extends T, but Primitive (which
    // includes boolean/symbol/etc.) doesn't extend the specific union, so use a
    // conditional branch to verify narrowing instead.
    const value = 'hello' as string | Date
    if (is.primitive(value)) {
      expectTypeOf(value).toEqualTypeOf<string>()
    }
    if (!is.primitive(value)) {
      expectTypeOf(value).toEqualTypeOf<Date>()
    }
  })

  test('filter array to non-primitives', () => {
    const values = ['a', {}, new Date()] as (
      | string
      | Record<string, unknown>
      | Date
    )[]
    const nonPrimitives = values.filter(is.not.primitive)
    expectTypeOf(nonPrimitives).toEqualTypeOf<
      (Record<string, unknown> | Date)[]
    >()
  })
})
