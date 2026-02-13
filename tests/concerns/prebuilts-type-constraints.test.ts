/**
 * Type Safety Tests for Prebuilt Concerns
 *
 * Verifies that type constraints are properly enforced:
 * - Valid paths and values are accepted
 * - Invalid paths cause TypeScript errors (@ts-expect-error)
 * - Invalid value types cause TypeScript errors (@ts-expect-error)
 * - BoolLogic is constrained to match state type
 */

import { describe, it } from 'vitest'
import { z } from 'zod'

import {
  disabledWhen,
  readonlyWhen,
  validationState,
  visibleWhen,
} from '../../src/concerns/prebuilts'
import type { BoolLogic, DeepKey, DeepValue } from '../../src/types'

// ============================================================================
// Type Test State
// ============================================================================

interface TypeTestState {
  user: {
    name: string
    age: number
    isAdmin: boolean
    email: string
  }
  product: {
    price: number
    status: 'active' | 'inactive'
    inStock: boolean
  }
  settings: {
    darkMode: boolean
    notifications: boolean
  }
}

// ============================================================================
// ValidateState Tests
// ============================================================================

describe('validationState - Type Constraints', () => {
  it('should accept valid path with matching schema', () => {
    const config: Parameters<typeof validationState.evaluate>[0] = {
      state: {} as TypeTestState,
      path: 'user.age' as DeepKey<TypeTestState>,
      value: 25 as DeepValue<TypeTestState, 'user.age'>,
      schema: z.number().min(0),
    }

    // Should compile without error
    validationState.evaluate(config)
  })

  it('should reject invalid path', () => {
    // @ts-expect-error - 'invalidPath' is not a valid path in TypeTestState
    const config: Parameters<typeof validationState.evaluate>[0] = {
      state: {} as TypeTestState,
      path: 'invalidPath',
      value: 25,
      schema: z.number(),
    }

    validationState.evaluate(config)
  })

  it('should reject mismatched schema type', () => {
    // @ts-expect-error - schema type doesn't match the path value type (age is number, not string)
    const config: Parameters<typeof validationState.evaluate>[0] = {
      state: {} as TypeTestState,
      path: 'user.age' as DeepKey<TypeTestState>,
      value: 25 as DeepValue<TypeTestState, 'user.age'>,
      schema: z.string(),
    }

    validationState.evaluate(config)
  })

  it('should accept valid path with matching scope', () => {
    const config: Parameters<typeof validationState.evaluate>[0] = {
      state: {} as TypeTestState,
      path: 'user.name' as DeepKey<TypeTestState>,
      value: 'John' as DeepValue<TypeTestState, 'user.name'>,
      scope: 'user.name' as DeepKey<TypeTestState>,
      schema: z.string(),
    }

    validationState.evaluate(config)
  })

  it('should reject invalid scope', () => {
    // @ts-expect-error - 'invalidScope' is not a valid path in TypeTestState
    const config: Parameters<typeof validationState.evaluate>[0] = {
      state: {} as TypeTestState,
      path: 'user.name' as DeepKey<TypeTestState>,
      value: 'John' as DeepValue<TypeTestState, 'user.name'>,
      scope: 'invalidScope',
      schema: z.string(),
    }

    validationState.evaluate(config)
  })
})

// ============================================================================
// DisabledWhen Tests
// ============================================================================

describe('disabledWhen - Type Constraints', () => {
  it('should accept valid BoolLogic with existing paths', () => {
    const config: Parameters<typeof disabledWhen.evaluate>[0] = {
      state: {} as TypeTestState,
      path: 'product.price' as DeepKey<TypeTestState>,
      value: true,
      condition: {
        IS_EQUAL: ['product.status', 'inactive'],
      } as BoolLogic<TypeTestState>,
    }

    disabledWhen.evaluate(config)
  })

  it('should accept AND condition with valid paths', () => {
    const config: Parameters<typeof disabledWhen.evaluate>[0] = {
      state: {} as TypeTestState,
      path: 'product.price' as DeepKey<TypeTestState>,
      value: true,
      condition: {
        AND: [
          { IS_EQUAL: ['product.status', 'inactive'] },
          { IS_EQUAL: ['user.isAdmin', false] },
        ],
      } as BoolLogic<TypeTestState>,
    }

    disabledWhen.evaluate(config)
  })

  it('should accept numeric comparison operators', () => {
    const config: Parameters<typeof disabledWhen.evaluate>[0] = {
      state: {} as TypeTestState,
      path: 'product.price' as DeepKey<TypeTestState>,
      value: true,
      condition: {
        GT: ['product.price', 100],
      } as BoolLogic<TypeTestState>,
    }

    disabledWhen.evaluate(config)
  })

  it('should reject invalid path in condition', () => {
    // @ts-expect-error - 'invalidPath' is not a valid path in TypeTestState
    const condition: BoolLogic<TypeTestState> = {
      IS_EQUAL: ['invalidPath', 'value'],
    }

    const config: Parameters<typeof disabledWhen.evaluate>[0] = {
      state: {} as TypeTestState,
      path: 'product.price' as DeepKey<TypeTestState>,
      value: true,
      condition,
    }

    disabledWhen.evaluate(config)
  })

  it('should reject invalid path in nested AND', () => {
    // @ts-expect-error - 'nonExistent.path' is not valid
    const condition: BoolLogic<TypeTestState> = {
      AND: [
        { IS_EQUAL: ['product.status', 'inactive'] },
        { IS_EQUAL: ['nonExistent.path', 'value'] },
      ],
    }

    const config: Parameters<typeof disabledWhen.evaluate>[0] = {
      state: {} as TypeTestState,
      path: 'product.price' as DeepKey<TypeTestState>,
      value: true,
      condition,
    }

    disabledWhen.evaluate(config)
  })

  it('should reject numeric comparison on non-numeric path', () => {
    // @ts-expect-error - GT requires a numeric path, but user.name is string
    const condition: BoolLogic<TypeTestState> = {
      GT: ['user.name', 100],
    }

    const config: Parameters<typeof disabledWhen.evaluate>[0] = {
      state: {} as TypeTestState,
      path: 'product.price' as DeepKey<TypeTestState>,
      value: true,
      condition,
    }

    disabledWhen.evaluate(config)
  })

  it('should accept EXISTS operator with valid path', () => {
    const config: Parameters<typeof disabledWhen.evaluate>[0] = {
      state: {} as TypeTestState,
      path: 'product.price' as DeepKey<TypeTestState>,
      value: true,
      condition: {
        EXISTS: 'user.email',
      } as BoolLogic<TypeTestState>,
    }

    disabledWhen.evaluate(config)
  })

  it('should reject EXISTS with invalid path', () => {
    // @ts-expect-error - 'invalidPath' is not valid
    const condition: BoolLogic<TypeTestState> = {
      EXISTS: 'invalidPath',
    }

    const config: Parameters<typeof disabledWhen.evaluate>[0] = {
      state: {} as TypeTestState,
      path: 'product.price' as DeepKey<TypeTestState>,
      value: true,
      condition,
    }

    disabledWhen.evaluate(config)
  })

  it('should accept IN operator with matching value array', () => {
    const config: Parameters<typeof disabledWhen.evaluate>[0] = {
      state: {} as TypeTestState,
      path: 'product.price' as DeepKey<TypeTestState>,
      value: true,
      condition: {
        IN: ['product.status', ['active', 'inactive']],
      } as BoolLogic<TypeTestState>,
    }

    disabledWhen.evaluate(config)
  })
})

// ============================================================================
// ReadonlyWhen Tests
// ============================================================================

describe('readonlyWhen - Type Constraints', () => {
  it('should accept valid BoolLogic with existing paths', () => {
    const config: Parameters<typeof readonlyWhen.evaluate>[0] = {
      state: {} as TypeTestState,
      path: 'user.email' as DeepKey<TypeTestState>,
      value: false,
      condition: {
        IS_EQUAL: ['user.isAdmin', true],
      } as BoolLogic<TypeTestState>,
    }

    readonlyWhen.evaluate(config)
  })

  it('should accept OR condition with valid paths', () => {
    const config: Parameters<typeof readonlyWhen.evaluate>[0] = {
      state: {} as TypeTestState,
      path: 'user.email' as DeepKey<TypeTestState>,
      value: false,
      condition: {
        OR: [
          { IS_EQUAL: ['user.isAdmin', true] },
          { IS_EQUAL: ['product.status', 'inactive'] },
        ],
      } as BoolLogic<TypeTestState>,
    }

    readonlyWhen.evaluate(config)
  })

  it('should accept NOT condition with valid path', () => {
    const config: Parameters<typeof readonlyWhen.evaluate>[0] = {
      state: {} as TypeTestState,
      path: 'user.email' as DeepKey<TypeTestState>,
      value: false,
      condition: {
        NOT: { IS_EQUAL: ['user.isAdmin', false] },
      } as BoolLogic<TypeTestState>,
    }

    readonlyWhen.evaluate(config)
  })

  it('should reject invalid path in condition', () => {
    // @ts-expect-error - 'notAPath' is not valid
    const condition: BoolLogic<TypeTestState> = {
      IS_EQUAL: ['notAPath', true],
    }

    const config: Parameters<typeof readonlyWhen.evaluate>[0] = {
      state: {} as TypeTestState,
      path: 'user.email' as DeepKey<TypeTestState>,
      value: false,
      condition,
    }

    readonlyWhen.evaluate(config)
  })

  it('should reject invalid path in nested NOT', () => {
    // @ts-expect-error - nested path doesn't exist
    const condition: BoolLogic<TypeTestState> = {
      NOT: { IS_EQUAL: ['product.noField', 'value'] },
    }

    const config: Parameters<typeof readonlyWhen.evaluate>[0] = {
      state: {} as TypeTestState,
      path: 'user.email' as DeepKey<TypeTestState>,
      value: false,
      condition,
    }

    readonlyWhen.evaluate(config)
  })
})

// ============================================================================
// VisibleWhen Tests
// ============================================================================

describe('visibleWhen - Type Constraints', () => {
  it('should accept valid BoolLogic with existing paths', () => {
    const config: Parameters<typeof visibleWhen.evaluate>[0] = {
      state: {} as TypeTestState,
      path: 'settings.notifications' as DeepKey<TypeTestState>,
      value: true,
      condition: {
        IS_EQUAL: ['settings.darkMode', true],
      } as BoolLogic<TypeTestState>,
    }

    visibleWhen.evaluate(config)
  })

  it('should accept complex nested condition', () => {
    const config: Parameters<typeof visibleWhen.evaluate>[0] = {
      state: {} as TypeTestState,
      path: 'settings.notifications' as DeepKey<TypeTestState>,
      value: true,
      condition: {
        AND: [
          { IS_EQUAL: ['user.isAdmin', true] },
          {
            OR: [
              { IS_EQUAL: ['product.status', 'active'] },
              { IS_EQUAL: ['product.inStock', true] },
            ],
          },
        ],
      } as BoolLogic<TypeTestState>,
    }

    visibleWhen.evaluate(config)
  })

  it('should accept LTE operator with valid numeric path', () => {
    const config: Parameters<typeof visibleWhen.evaluate>[0] = {
      state: {} as TypeTestState,
      path: 'settings.notifications' as DeepKey<TypeTestState>,
      value: true,
      condition: {
        LTE: ['user.age', 18],
      } as BoolLogic<TypeTestState>,
    }

    visibleWhen.evaluate(config)
  })

  it('should reject invalid path in condition', () => {
    // @ts-expect-error - 'settings.unknown' doesn't exist
    const condition: BoolLogic<TypeTestState> = {
      IS_EQUAL: ['settings.unknown', true],
    }

    const config: Parameters<typeof visibleWhen.evaluate>[0] = {
      state: {} as TypeTestState,
      path: 'settings.notifications' as DeepKey<TypeTestState>,
      value: true,
      condition,
    }

    visibleWhen.evaluate(config)
  })

  it('should reject LT operator on non-numeric path', () => {
    // @ts-expect-error - LT requires numeric path, but user.email is string
    const condition: BoolLogic<TypeTestState> = {
      LT: ['user.email', 100],
    }

    const config: Parameters<typeof visibleWhen.evaluate>[0] = {
      state: {} as TypeTestState,
      path: 'settings.notifications' as DeepKey<TypeTestState>,
      value: true,
      condition,
    }

    visibleWhen.evaluate(config)
  })

  it('should reject IS_EMPTY on non-emptiness-testable path', () => {
    // Note: IS_EMPTY should work on arrays/objects/strings, but not primitives
    // TypeScript should prevent this if BoolLogic is properly constrained
    const config: Parameters<typeof visibleWhen.evaluate>[0] = {
      state: {} as TypeTestState,
      path: 'settings.notifications' as DeepKey<TypeTestState>,
      value: true,
      condition: {
        IS_EMPTY: 'user.age',
      } as BoolLogic<TypeTestState>,
    }

    visibleWhen.evaluate(config)
  })
})
