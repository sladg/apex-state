/// <reference types="vitest" />

/**
 * Tests for string interpolation utilities
 */

import { describe, it, expect, expectTypeOf } from 'vitest'
import { extractPlaceholders, interpolateTemplate } from '../../src/utils/interpolation'
import type { ExtractPlaceholders, ValidatedTemplate } from '../../src/types/interpolation'

// ============================================================================
// Test State Types
// ============================================================================

type SimpleState = {
  user: {
    name: string
    email: string
  }
  app: {
    version: string
    beta: boolean
  }
  count: number
}

type ComplexState = {
  user: {
    profile: {
      firstName: string
      lastName: string
    }
  }
  order: {
    total: number
  }
}

// ============================================================================
// TYPE TESTS: ExtractPlaceholders
// ============================================================================

describe('ExtractPlaceholders type', () => {
  it('extracts single placeholder', () => {
    type Extracted = ExtractPlaceholders<'Hello {{user.name}}'>
    expectTypeOf<Extracted>().toEqualTypeOf<'user.name'>()
  })

  it('extracts multiple placeholders', () => {
    type Extracted = ExtractPlaceholders<'{{user.name}} - {{user.email}}'>
    // Union type - check both members exist
    expectTypeOf<'user.name'>().toMatchTypeOf<Extracted>()
    expectTypeOf<'user.email'>().toMatchTypeOf<Extracted>()
  })

  it('extracts nested paths', () => {
    type Extracted = ExtractPlaceholders<'Name: {{user.profile.firstName}}'>
    expectTypeOf<Extracted>().toEqualTypeOf<'user.profile.firstName'>()
  })

  it('returns never for no placeholders', () => {
    type Extracted = ExtractPlaceholders<'Hello World'>
    expectTypeOf<Extracted>().toEqualTypeOf<never>()
  })
})

// ============================================================================
// TYPE TESTS: ValidatedTemplate
// ============================================================================

describe('ValidatedTemplate type', () => {
  it('accepts valid single path', () => {
    type Template = ValidatedTemplate<'Hello {{user.name}}', SimpleState>
    expectTypeOf<Template>().toEqualTypeOf<'Hello {{user.name}}'>()
  })

  it('accepts valid multiple paths', () => {
    type Template = ValidatedTemplate<'{{user.name}} ({{user.email}})', SimpleState>
    expectTypeOf<Template>().toEqualTypeOf<'{{user.name}} ({{user.email}})'>()
  })

  it('accepts templates without placeholders', () => {
    type Template = ValidatedTemplate<'Plain text', SimpleState>
    expectTypeOf<Template>().toEqualTypeOf<'Plain text'>()
  })

  it('accepts nested paths', () => {
    type Template = ValidatedTemplate<'Name: {{user.profile.firstName}}', ComplexState>
    expectTypeOf<Template>().toEqualTypeOf<'Name: {{user.profile.firstName}}'>()
  })

  it('accepts number paths', () => {
    type Template = ValidatedTemplate<'Count: {{count}}', SimpleState>
    expectTypeOf<Template>().toEqualTypeOf<'Count: {{count}}'>()
  })

  it('accepts boolean paths', () => {
    type Template = ValidatedTemplate<'Beta: {{app.beta}}', SimpleState>
    expectTypeOf<Template>().toEqualTypeOf<'Beta: {{app.beta}}'>()
  })

  it('rejects invalid paths', () => {
    type Template = ValidatedTemplate<'Hello {{invalid.path}}', SimpleState>
    expectTypeOf<Template>().toEqualTypeOf<never>()
  })

  it('rejects when any path is invalid', () => {
    type Template = ValidatedTemplate<'{{user.name}} {{invalid}}', SimpleState>
    expectTypeOf<Template>().toEqualTypeOf<never>()
  })
})

// ============================================================================
// RUNTIME TESTS: extractPlaceholders
// ============================================================================

describe('extractPlaceholders', () => {
  it('extracts single placeholder', () => {
    expect(extractPlaceholders('Hello {{user.name}}')).toEqual(['user.name'])
  })

  it('extracts multiple placeholders', () => {
    expect(extractPlaceholders('{{a}} and {{b}} and {{c}}')).toEqual(['a', 'b', 'c'])
  })

  it('extracts nested paths', () => {
    expect(extractPlaceholders('{{user.profile.name}}')).toEqual(['user.profile.name'])
  })

  it('returns empty array for no placeholders', () => {
    expect(extractPlaceholders('Hello World')).toEqual([])
  })

  it('handles adjacent placeholders', () => {
    expect(extractPlaceholders('{{a}}{{b}}')).toEqual(['a', 'b'])
  })
})

// ============================================================================
// RUNTIME TESTS: interpolateTemplate
// ============================================================================

describe('interpolateTemplate', () => {
  const state: SimpleState = {
    user: { name: 'Alice', email: 'alice@test.com' },
    app: { version: '1.0.0', beta: true },
    count: 42,
  }

  it('interpolates string values', () => {
    expect(interpolateTemplate('Hello {{user.name}}', state)).toBe('Hello Alice')
  })

  it('interpolates number values', () => {
    expect(interpolateTemplate('Count: {{count}}', state)).toBe('Count: 42')
  })

  it('interpolates boolean values', () => {
    expect(interpolateTemplate('Beta: {{app.beta}}', state)).toBe('Beta: true')
  })

  it('interpolates multiple placeholders', () => {
    expect(interpolateTemplate('{{user.name}} ({{user.email}})', state))
      .toBe('Alice (alice@test.com)')
  })

  it('leaves invalid paths unchanged for debugging', () => {
    expect(interpolateTemplate('Hello {{invalid.path}}', state))
      .toBe('Hello {{invalid.path}}')
  })

  it('leaves null/undefined values unchanged', () => {
    const stateWithNull = { value: null, empty: undefined }
    expect(interpolateTemplate('{{value}} {{empty}}', stateWithNull))
      .toBe('{{value}} {{empty}}')
  })

  it('leaves object values unchanged', () => {
    const stateWithObj = { user: { nested: { deep: 'value' } } }
    // Trying to interpolate the object itself, not the deep value
    expect(interpolateTemplate('{{user.nested}}', stateWithObj))
      .toBe('{{user.nested}}')
  })

  it('handles templates without placeholders', () => {
    expect(interpolateTemplate('No placeholders here', state))
      .toBe('No placeholders here')
  })

  it('handles empty template', () => {
    expect(interpolateTemplate('', state)).toBe('')
  })

  it('handles nested paths', () => {
    const nested: ComplexState = {
      user: { profile: { firstName: 'Bob', lastName: 'Smith' } },
      order: { total: 99.99 },
    }
    expect(interpolateTemplate('{{user.profile.firstName}} - ${{order.total}}', nested))
      .toBe('Bob - $99.99')
  })
})

// ============================================================================
// MULTIPLE PLACEHOLDERS (up to 5)
// ============================================================================

describe('Multiple placeholders (up to 5)', () => {
  type MultiState = {
    a: string
    b: string
    c: string
    d: string
    e: string
    nested: { x: number; y: number }
  }

  const state: MultiState = {
    a: 'A', b: 'B', c: 'C', d: 'D', e: 'E',
    nested: { x: 1, y: 2 },
  }

  // Type tests
  it('type: validates 2 placeholders', () => {
    type T = ValidatedTemplate<'{{a}} {{b}}', MultiState>
    expectTypeOf<T>().toEqualTypeOf<'{{a}} {{b}}'>()
  })

  it('type: validates 3 placeholders', () => {
    type T = ValidatedTemplate<'{{a}} {{b}} {{c}}', MultiState>
    expectTypeOf<T>().toEqualTypeOf<'{{a}} {{b}} {{c}}'>()
  })

  it('type: validates 4 placeholders', () => {
    type T = ValidatedTemplate<'{{a}} {{b}} {{c}} {{d}}', MultiState>
    expectTypeOf<T>().toEqualTypeOf<'{{a}} {{b}} {{c}} {{d}}'>()
  })

  it('type: validates 5 placeholders', () => {
    type T = ValidatedTemplate<'{{a}} {{b}} {{c}} {{d}} {{e}}', MultiState>
    expectTypeOf<T>().toEqualTypeOf<'{{a}} {{b}} {{c}} {{d}} {{e}}'>()
  })

  it('type: validates 5 nested placeholders', () => {
    type T = ValidatedTemplate<'{{a}}-{{nested.x}}-{{b}}-{{nested.y}}-{{c}}', MultiState>
    expectTypeOf<T>().toEqualTypeOf<'{{a}}-{{nested.x}}-{{b}}-{{nested.y}}-{{c}}'>()
  })

  it('type: rejects if any of 5 is invalid', () => {
    type T = ValidatedTemplate<'{{a}} {{b}} {{invalid}} {{d}} {{e}}', MultiState>
    expectTypeOf<T>().toEqualTypeOf<never>()
  })

  // Runtime tests
  it('runtime: interpolates 2 placeholders', () => {
    expect(interpolateTemplate('{{a}} {{b}}', state)).toBe('A B')
  })

  it('runtime: interpolates 3 placeholders', () => {
    expect(interpolateTemplate('{{a}} {{b}} {{c}}', state)).toBe('A B C')
  })

  it('runtime: interpolates 4 placeholders', () => {
    expect(interpolateTemplate('{{a}} {{b}} {{c}} {{d}}', state)).toBe('A B C D')
  })

  it('runtime: interpolates 5 placeholders', () => {
    expect(interpolateTemplate('{{a}} {{b}} {{c}} {{d}} {{e}}', state)).toBe('A B C D E')
  })

  it('runtime: interpolates 5 with mixed types', () => {
    expect(interpolateTemplate('{{a}}-{{nested.x}}-{{b}}-{{nested.y}}-{{c}}', state))
      .toBe('A-1-B-2-C')
  })

  it('runtime: extracts 5 placeholders', () => {
    expect(extractPlaceholders('{{a}} {{b}} {{c}} {{d}} {{e}}'))
      .toEqual(['a', 'b', 'c', 'd', 'e'])
  })

  it('runtime: partial failure leaves invalid paths visible', () => {
    expect(interpolateTemplate('{{a}} {{invalid}} {{c}}', state))
      .toBe('A {{invalid}} C')
  })
})

// ============================================================================
// REAL-WORLD SCENARIOS
// ============================================================================

describe('Real-world scenarios', () => {
  it('dynamic label with price', () => {
    const state = { product: { name: 'Widget', price: 29.99 } }
    const result = interpolateTemplate('{{product.name}}: ${{product.price}}', state)
    expect(result).toBe('Widget: $29.99')
  })

  it('validation error message', () => {
    const state = { field: { name: 'email', min: 5, max: 100 } }
    const result = interpolateTemplate(
      '{{field.name}} must be between {{field.min}} and {{field.max}} characters',
      state
    )
    expect(result).toBe('email must be between 5 and 100 characters')
  })

  it('partial interpolation shows missing paths', () => {
    const state = { user: { name: 'Alice' } }
    const result = interpolateTemplate(
      'Hello {{user.name}}, your balance is {{user.balance}}',
      state
    )
    // Missing path stays visible for debugging
    expect(result).toBe('Hello Alice, your balance is {{user.balance}}')
  })
})
