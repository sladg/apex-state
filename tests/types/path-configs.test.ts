/**
 * Type Unit Tests for SyncPair, FlipPair, and AggregationPair
 *
 * Validates that path pair configurations enforce type safety at compile time.
 * Uses tuple format: [path1, path2]
 *
 * These are compile-time tests that verify types without runtime assertions.
 */

import { describe, expectTypeOf, it } from 'vitest'

import type {
  AggregationPair,
  ComputationPair,
  FlipPair,
  PathsWithSameValueAs,
  SyncPair,
} from '../../src/types/paths-of-same-value'
import type { SideEffects } from '../../src/types/side-effects'

// ============================================================================
// Test State Definitions
// ============================================================================

/**
 * Simple flat state for basic type tests
 */
interface SimpleFlatState {
  email: string
  username: string
  age: number
  isActive: boolean
}

/**
 * Nested state for deep path tests
 */
interface NestedState {
  user: {
    profile: {
      name: string
      email: string
    }
    settings: {
      theme: string
      notifications: boolean
    }
  }
  app: {
    version: string
    beta: boolean
  }
}

/**
 * Mixed types state with optional fields
 */
interface MixedTypesState {
  firstName: string
  lastName: string
  fullName: string
  displayName: string
  age: number
  count: number
  isAdmin: boolean
  isActive: boolean
  tags: string[]
  metadata: Record<string, unknown>
}

// ============================================================================
// SyncPair Type Tests (tuple format)
// ============================================================================

describe('SyncPair', () => {
  it('accepts two string paths as tuple', () => {
    type TestPair = SyncPair<SimpleFlatState>

    const pair: TestPair = ['email', 'username']

    expectTypeOf(pair).toMatchTypeOf<TestPair>()
  })

  it('accepts two number paths as tuple', () => {
    type TestPair = SyncPair<SimpleFlatState>

    const pair: TestPair = ['age', 'age']

    expectTypeOf(pair).toMatchTypeOf<TestPair>()
  })

  it('accepts two boolean paths as tuple', () => {
    type TestPair = SyncPair<SimpleFlatState>

    const pair: TestPair = ['isActive', 'isActive']

    expectTypeOf(pair).toMatchTypeOf<TestPair>()
  })

  it('accepts nested paths with matching string types', () => {
    type TestPair = SyncPair<NestedState>

    const pair: TestPair = ['user.profile.name', 'user.profile.email']

    expectTypeOf(pair).toMatchTypeOf<TestPair>()
  })

  it('accepts nested paths with matching boolean types', () => {
    type TestPair = SyncPair<NestedState>

    const pair: TestPair = ['user.settings.notifications', 'app.beta']

    expectTypeOf(pair).toMatchTypeOf<TestPair>()
  })
})

// ============================================================================
// FlipPair Type Tests (tuple format)
// ============================================================================

describe('FlipPair', () => {
  it('accepts two boolean paths as tuple', () => {
    type TestPair = FlipPair<SimpleFlatState>

    const pair: TestPair = ['isActive', 'isActive']

    expectTypeOf(pair).toMatchTypeOf<TestPair>()
  })

  it('accepts different boolean paths', () => {
    type TestPair = FlipPair<MixedTypesState>

    const pair: TestPair = ['isAdmin', 'isActive']

    expectTypeOf(pair).toMatchTypeOf<TestPair>()
  })

  it('accepts nested boolean paths', () => {
    type TestPair = FlipPair<NestedState>

    const pair: TestPair = ['user.settings.notifications', 'app.beta']

    expectTypeOf(pair).toMatchTypeOf<TestPair>()
  })
})

// ============================================================================
// AggregationPair Type Tests (tuple format: [target, source])
// ============================================================================

describe('AggregationPair', () => {
  it('accepts matching number paths as tuple [target, source]', () => {
    interface State {
      price1: number
      price2: number
      total: number
    }

    type TestPair = AggregationPair<State>

    // target <- source (target always first)
    const pair: TestPair = ['total', 'price1']

    expectTypeOf(pair).toMatchTypeOf<TestPair>()
  })

  it('accepts string aggregation paths', () => {
    interface State {
      user: {
        firstName: string
        lastName: string
        fullName: string
      }
    }

    type TestPair = AggregationPair<State>

    const pair: TestPair = ['user.fullName', 'user.firstName']

    expectTypeOf(pair).toMatchTypeOf<TestPair>()
  })
})

// ============================================================================
// ComputationPair Type Tests (tuple format: [op, target, source])
// ============================================================================

describe('ComputationPair', () => {
  it('accepts number paths for SUM operation', () => {
    interface State {
      price1: number
      price2: number
      total: number
    }

    type TestPair = ComputationPair<State>

    const pair: TestPair = ['SUM', 'total', 'price1']

    expectTypeOf(pair).toMatchTypeOf<TestPair>()
  })

  it('accepts number paths for AVG operation', () => {
    interface State {
      score1: number
      score2: number
      average: number
    }

    type TestPair = ComputationPair<State>

    const pair: TestPair = ['AVG', 'average', 'score1']

    expectTypeOf(pair).toMatchTypeOf<TestPair>()
  })

  it('accepts computation with excludeWhen condition', () => {
    interface State {
      price1: number
      price2: number
      total: number
      price2Disabled: boolean
    }

    type TestPair = ComputationPair<State>

    const pair: TestPair = [
      'SUM',
      'total',
      'price2',
      { IS_EQUAL: ['price2Disabled', true] },
    ]

    expectTypeOf(pair).toMatchTypeOf<TestPair>()
  })

  it('rejects string paths as computation target', () => {
    interface State {
      name: string
      count: number
    }

    type TestPair = ComputationPair<State>

    // 'name' is a string path, not a number path — should NOT be valid as target
    expectTypeOf<['SUM', 'name', 'count']>().not.toMatchTypeOf<TestPair>()
  })

  it('rejects boolean paths as computation source', () => {
    interface State {
      isActive: boolean
      count: number
    }

    type TestPair = ComputationPair<State>

    // 'isActive' is a boolean path — should NOT be valid as source
    expectTypeOf<['SUM', 'count', 'isActive']>().not.toMatchTypeOf<TestPair>()
  })
})

// ============================================================================
// SideEffects Type Tests (full config)
// ============================================================================

describe('SideEffects', () => {
  it('accepts syncPaths as array of tuples', () => {
    type TestConfig = SideEffects<SimpleFlatState>

    const config: TestConfig = {
      syncPaths: [['email', 'username']],
    }

    expectTypeOf(config).toMatchTypeOf<TestConfig>()
  })

  it('accepts multiple sync pairs', () => {
    type TestConfig = SideEffects<MixedTypesState>

    const config: TestConfig = {
      syncPaths: [
        ['firstName', 'lastName'],
        ['isAdmin', 'isActive'],
        ['age', 'count'],
      ],
    }

    expectTypeOf(config).toMatchTypeOf<TestConfig>()
  })

  it('accepts flipPaths as array of tuples', () => {
    type TestConfig = SideEffects<SimpleFlatState>

    const config: TestConfig = {
      flipPaths: [['isActive', 'isActive']],
    }

    expectTypeOf(config).toMatchTypeOf<TestConfig>()
  })

  it('accepts aggregations as array of [target, source] tuples', () => {
    interface State {
      price1: number
      price2: number
      total: number
    }

    type TestConfig = SideEffects<State>

    const config: TestConfig = {
      aggregations: [
        ['total', 'price1'],
        ['total', 'price2'],
      ],
    }

    expectTypeOf(config).toMatchTypeOf<TestConfig>()
  })

  it('accepts aggregations with optional excludeWhen BoolLogic condition', () => {
    interface State {
      price1: number
      price2: number
      total: number
      price2Disabled: boolean
    }

    type TestConfig = SideEffects<State>

    // Mix of 2-element and 3-element tuples
    const config: TestConfig = {
      aggregations: [
        ['total', 'price1'],
        ['total', 'price2', { IS_EQUAL: ['price2Disabled', true] }],
      ],
    }

    expectTypeOf(config).toMatchTypeOf<TestConfig>()
  })

  it('accepts combined side effects', () => {
    interface State {
      firstName: string
      lastName: string
      isActive: boolean
      isInactive: boolean
      price1: number
      price2: number
      total: number
    }

    type TestConfig = SideEffects<State>

    const config: TestConfig = {
      syncPaths: [['firstName', 'lastName']],
      flipPaths: [['isActive', 'isInactive']],
      aggregations: [
        ['total', 'price1'],
        ['total', 'price2'],
      ],
    }

    expectTypeOf(config).toMatchTypeOf<TestConfig>()
  })

  it('accepts empty arrays', () => {
    type TestConfig = SideEffects<SimpleFlatState>

    const config: TestConfig = {
      syncPaths: [],
      flipPaths: [],
      aggregations: [],
    }

    expectTypeOf(config).toMatchTypeOf<TestConfig>()
  })
})

// ============================================================================
// Explicit Union Membership Tests
// ============================================================================

describe('Union membership - explicit checks', () => {
  interface TestState {
    name: string
    email: string
    title: string
    age: number
    count: number
    isActive: boolean
    isVisible: boolean
  }

  type StringPaths = PathsWithSameValueAs<TestState, 'name'>
  type NumberPaths = PathsWithSameValueAs<TestState, 'age'>
  type BooleanPaths = PathsWithSameValueAs<TestState, 'isActive'>

  // --- String paths union ---
  it('string union includes "name"', () => {
    expectTypeOf<'name'>().toMatchTypeOf<StringPaths>()
  })

  it('string union includes "email"', () => {
    expectTypeOf<'email'>().toMatchTypeOf<StringPaths>()
  })

  it('string union includes "title"', () => {
    expectTypeOf<'title'>().toMatchTypeOf<StringPaths>()
  })

  it('string union excludes "age" (number)', () => {
    expectTypeOf<'age'>().not.toMatchTypeOf<StringPaths>()
  })

  it('string union excludes "isActive" (boolean)', () => {
    expectTypeOf<'isActive'>().not.toMatchTypeOf<StringPaths>()
  })

  // --- Number paths union ---
  it('number union includes "age"', () => {
    expectTypeOf<'age'>().toMatchTypeOf<NumberPaths>()
  })

  it('number union includes "count"', () => {
    expectTypeOf<'count'>().toMatchTypeOf<NumberPaths>()
  })

  it('number union excludes "name" (string)', () => {
    expectTypeOf<'name'>().not.toMatchTypeOf<NumberPaths>()
  })

  it('number union excludes "isActive" (boolean)', () => {
    expectTypeOf<'isActive'>().not.toMatchTypeOf<NumberPaths>()
  })

  // --- Boolean paths union ---
  it('boolean union includes "isActive"', () => {
    expectTypeOf<'isActive'>().toMatchTypeOf<BooleanPaths>()
  })

  it('boolean union includes "isVisible"', () => {
    expectTypeOf<'isVisible'>().toMatchTypeOf<BooleanPaths>()
  })

  it('boolean union excludes "name" (string)', () => {
    expectTypeOf<'name'>().not.toMatchTypeOf<BooleanPaths>()
  })

  it('boolean union excludes "age" (number)', () => {
    expectTypeOf<'age'>().not.toMatchTypeOf<BooleanPaths>()
  })

  // --- Exact union equality ---
  it('string paths union equals exactly name | email | title', () => {
    type Expected = 'name' | 'email' | 'title'
    expectTypeOf<StringPaths>().toEqualTypeOf<Expected>()
  })

  it('number paths union equals exactly age | count', () => {
    type Expected = 'age' | 'count'
    expectTypeOf<NumberPaths>().toEqualTypeOf<Expected>()
  })

  it('boolean paths union equals exactly isActive | isVisible', () => {
    type Expected = 'isActive' | 'isVisible'
    expectTypeOf<BooleanPaths>().toEqualTypeOf<Expected>()
  })
})

describe('Union membership - nested paths', () => {
  interface NestedTestState {
    user: {
      profile: { firstName: string; lastName: string }
      settings: { theme: string }
      age: number
    }
    meta: {
      title: string
      count: number
    }
  }

  type NestedStringPaths = PathsWithSameValueAs<
    NestedTestState,
    'user.profile.firstName'
  >
  type NestedNumberPaths = PathsWithSameValueAs<NestedTestState, 'user.age'>

  it('nested string union includes user.profile.firstName', () => {
    expectTypeOf<'user.profile.firstName'>().toMatchTypeOf<NestedStringPaths>()
  })

  it('nested string union includes user.profile.lastName', () => {
    expectTypeOf<'user.profile.lastName'>().toMatchTypeOf<NestedStringPaths>()
  })

  it('nested string union includes user.settings.theme', () => {
    expectTypeOf<'user.settings.theme'>().toMatchTypeOf<NestedStringPaths>()
  })

  it('nested string union includes meta.title', () => {
    expectTypeOf<'meta.title'>().toMatchTypeOf<NestedStringPaths>()
  })

  it('nested string union excludes user.age (number)', () => {
    expectTypeOf<'user.age'>().not.toMatchTypeOf<NestedStringPaths>()
  })

  it('nested string union excludes meta.count (number)', () => {
    expectTypeOf<'meta.count'>().not.toMatchTypeOf<NestedStringPaths>()
  })

  it('nested number union includes user.age', () => {
    expectTypeOf<'user.age'>().toMatchTypeOf<NestedNumberPaths>()
  })

  it('nested number union includes meta.count', () => {
    expectTypeOf<'meta.count'>().toMatchTypeOf<NestedNumberPaths>()
  })

  it('nested number union excludes string paths', () => {
    expectTypeOf<'user.profile.firstName'>().not.toMatchTypeOf<NestedNumberPaths>()
  })

  it('nested string paths equals exact union', () => {
    type Expected =
      | 'user.profile.firstName'
      | 'user.profile.lastName'
      | 'user.settings.theme'
      | 'meta.title'
    expectTypeOf<NestedStringPaths>().toEqualTypeOf<Expected>()
  })

  it('nested number paths equals exact union', () => {
    type Expected = 'user.age' | 'meta.count'
    expectTypeOf<NestedNumberPaths>().toEqualTypeOf<Expected>()
  })
})

// ============================================================================
// PathsWithSameValueAs Type Equality Tests
// ============================================================================

describe('PathsWithSameValueAs type equality', () => {
  it('resolves string paths to union of all string paths', () => {
    interface State {
      email: string
      username: string
      age: number
    }

    type EmailPaths = PathsWithSameValueAs<State, 'email'>
    type ExpectedUnion = 'email' | 'username'

    expectTypeOf<EmailPaths>().toEqualTypeOf<ExpectedUnion>()
  })

  it('resolves number paths to union of all number paths', () => {
    interface State {
      count: number
      age: number
      price: number
      isActive: boolean
    }

    type CountPaths = PathsWithSameValueAs<State, 'count'>
    type ExpectedUnion = 'count' | 'age' | 'price'

    expectTypeOf<CountPaths>().toEqualTypeOf<ExpectedUnion>()
  })

  it('resolves boolean paths to union of all boolean paths', () => {
    interface State {
      isActive: boolean
      isVisible: boolean
      count: number
    }

    type ActivePaths = PathsWithSameValueAs<State, 'isActive'>
    type ExpectedUnion = 'isActive' | 'isVisible'

    expectTypeOf<ActivePaths>().toEqualTypeOf<ExpectedUnion>()
  })

  it('resolves nested string paths correctly', () => {
    interface State {
      user: { name: string; email: string }
      app: { title: string }
      count: number
    }

    type NamePaths = PathsWithSameValueAs<State, 'user.name'>
    type ExpectedUnion = 'user.name' | 'user.email' | 'app.title'

    expectTypeOf<NamePaths>().toEqualTypeOf<ExpectedUnion>()
  })

  it('resolves nested number paths correctly', () => {
    interface State {
      cart: {
        subtotal: number
        tax: number
        total: number
      }
      price: number
    }

    type SubtotalPaths = PathsWithSameValueAs<State, 'cart.subtotal'>
    type ExpectedUnion = 'cart.subtotal' | 'cart.tax' | 'cart.total' | 'price'

    expectTypeOf<SubtotalPaths>().toEqualTypeOf<ExpectedUnion>()
  })

  it('handles single matching path', () => {
    interface State {
      count: number
      name: string
    }

    type CountPaths = PathsWithSameValueAs<State, 'count'>
    type ExpectedUnion = 'count'

    expectTypeOf<CountPaths>().toEqualTypeOf<ExpectedUnion>()
  })

  it('handles deeply nested paths with multiple levels', () => {
    interface State {
      user: {
        profile: {
          firstName: string
          lastName: string
        }
        contact: {
          email: string
        }
      }
    }

    type FirstNamePaths = PathsWithSameValueAs<State, 'user.profile.firstName'>
    type ExpectedUnion =
      | 'user.profile.firstName'
      | 'user.profile.lastName'
      | 'user.contact.email'

    expectTypeOf<FirstNamePaths>().toEqualTypeOf<ExpectedUnion>()
  })
})

// ============================================================================
// Real-world scenarios
// ============================================================================

describe('Real-world scenarios', () => {
  it('form with synced email fields', () => {
    interface FormState {
      email: string
      emailConfirm: string
      password: string
      age: number
    }

    type TestConfig = SideEffects<FormState>

    const config: TestConfig = {
      syncPaths: [['email', 'emailConfirm']],
    }

    expectTypeOf(config).toMatchTypeOf<TestConfig>()
  })

  it('toggle states with flip', () => {
    interface UIState {
      menuOpen: boolean
      menuClosed: boolean
      sidebarVisible: boolean
      sidebarHidden: boolean
    }

    type TestConfig = SideEffects<UIState>

    const config: TestConfig = {
      flipPaths: [
        ['menuOpen', 'menuClosed'],
        ['sidebarVisible', 'sidebarHidden'],
      ],
    }

    expectTypeOf(config).toMatchTypeOf<TestConfig>()
  })

  it('complex nested state sync', () => {
    interface ComplexState {
      user: {
        primary: {
          name: string
          email: string
        }
        secondary: {
          displayName: string
          contactEmail: string
        }
      }
    }

    type TestConfig = SideEffects<ComplexState>

    const config: TestConfig = {
      syncPaths: [
        ['user.primary.name', 'user.secondary.displayName'],
        ['user.primary.email', 'user.secondary.contactEmail'],
      ],
    }

    expectTypeOf(config).toMatchTypeOf<TestConfig>()
  })

  it('shopping cart with aggregations', () => {
    interface CartState {
      price1: number
      price2: number
      quantity1: number
      quantity2: number
      itemsSubtotal: number
      itemCount: number
    }

    type TestConfig = SideEffects<CartState>

    const config: TestConfig = {
      aggregations: [
        ['itemsSubtotal', 'price1'],
        ['itemsSubtotal', 'price2'],
        ['itemCount', 'quantity1'],
        ['itemCount', 'quantity2'],
      ],
    }

    expectTypeOf(config).toMatchTypeOf<TestConfig>()
  })
})
