/**
 * Shared test fixtures and initial states
 *
 * Pre-built initial state configurations for each test scenario,
 * reducing duplication across test files.
 *
 * ## Purpose
 * Provides consistent, type-safe initial state data for stores in
 * integration tests. Each fixture represents a specific test scenario
 * state (empty form, partially filled, complete, with errors, etc.).
 *
 * ## Fixture Naming Convention
 * Each fixture set follows a consistent naming pattern:
 * - **empty** - All fields empty/default
 * - **partial** - Some fields filled
 * - **complete/filled** - All required fields valid
 * - **populated** - All fields with representative values
 * - **initial** - Default starting state
 */

import type {
  AggregationTestState,
  BasicTestState,
  DeeplyNestedState,
  ListenerTestState,
  RegistrationForm,
  SyncFlipState,
  ValidationTestState,
} from './types'

/**
 * Registration Form fixtures
 */
export const registrationFormFixtures = {
  empty: {
    email: '',
    password: '',
    confirmPassword: '',
    agreeToTerms: false,
    _errors: {},
  } satisfies RegistrationForm,

  partial: {
    email: 'john@example.com',
    password: 'Test123',
    confirmPassword: 'Test123',
    agreeToTerms: false,
    _errors: {},
  } satisfies RegistrationForm,

  complete: {
    email: 'john@example.com',
    password: 'StrongPass123',
    confirmPassword: 'StrongPass123',
    agreeToTerms: true,
    _errors: {},
  } satisfies RegistrationForm,
}

// ---------------------------------------------------------------------------
// V2 Integration Test Fixtures
// ---------------------------------------------------------------------------

/**
 * Basic test state — flat, covers most v2 scenarios
 */
export const basicTestFixtures = {
  empty: {
    fieldA: '',
    fieldB: '',
    fieldC: 0,
    source: '',
    target: '',
    boolA: false,
    boolB: true,
    email: '',
    age: 0,
    _errors: {},
  } satisfies BasicTestState,

  populated: {
    fieldA: 'value-a',
    fieldB: 'value-b',
    fieldC: 42,
    source: 'source-value',
    target: 'target-value',
    boolA: true,
    boolB: false,
    email: 'test@example.com',
    age: 25,
    _errors: {},
  } satisfies BasicTestState,
}

/**
 * Sync/Flip state — paired fields for bidirectional tests
 */
export const syncFlipFixtures = {
  initial: {
    source: 'A',
    target: '',
    source2: 'B',
    target2: '',
    flag1: false,
    flag2: true,
    flag3: false,
    flag4: true,
  } satisfies SyncFlipState,

  allFalse: {
    source: '',
    target: '',
    source2: '',
    target2: '',
    flag1: false,
    flag2: false,
    flag3: false,
    flag4: false,
  } satisfies SyncFlipState,
}

/**
 * Validation state
 */
export const validationTestFixtures = {
  empty: {
    email: '',
    age: 0,
    username: '',
    password: '',
    confirmPassword: '',
    _errors: {},
  } satisfies ValidationTestState,

  valid: {
    email: 'user@example.com',
    age: 25,
    username: 'johndoe',
    password: 'StrongPass123',
    confirmPassword: 'StrongPass123',
    _errors: {},
  } satisfies ValidationTestState,

  invalid: {
    email: 'not-an-email',
    age: -5,
    username: '',
    password: 'short',
    confirmPassword: 'mismatch',
    _errors: {},
  } satisfies ValidationTestState,
}

/**
 * Aggregation state — sources and target
 */
export const aggregationTestFixtures = {
  empty: {
    sourceA: '',
    sourceB: '',
    sourceC: '',
    target: '',
    numA: 0,
    numB: 0,
    numTotal: 0,
  } satisfies AggregationTestState,

  allSame: {
    sourceA: 'shared',
    sourceB: 'shared',
    sourceC: 'shared',
    target: 'shared',
    numA: 10,
    numB: 10,
    numTotal: 10,
  } satisfies AggregationTestState,

  mixed: {
    sourceA: 'alpha',
    sourceB: 'beta',
    sourceC: 'alpha',
    target: '',
    numA: 10,
    numB: 20,
    numTotal: 0,
  } satisfies AggregationTestState,
}

/**
 * Listener tracking state
 */
export const listenerTestFixtures = {
  initial: {
    user: {
      name: 'Alice',
      email: 'alice@example.com',
      age: 30,
    },
    callCount: 0,
    lastChange: '',
    derived: '',
  } satisfies ListenerTestState,
}

/**
 * Deeply nested state — 5 levels
 */
export const deeplyNestedFixtures = {
  initial: {
    level1: {
      value: 'L1',
      level2: {
        value: 'L2',
        level3: {
          value: 'L3',
          level4: {
            value: 'L4',
            level5: {
              value: 'L5',
              flag: false,
            },
          },
        },
      },
    },
  } satisfies DeeplyNestedState,
}

/**
 * Deep getter fixtures — 10 getters across nesting levels 0-5
 *
 * - 5 share the name "summary" (levels 0,1,2,3,5) to test name collision handling
 * - l3.summary reads l4.product (cross-level getter dependency)
 * - Getters use `this` which gets rebound to the valtio proxy at runtime
 */
export const deepGetterFixtures = {
  standard: {
    a: 1,
    get summary() {
      return `root:${this.a}`
    },
    get total() {
      return this.a + this.l1.b
    },
    l1: {
      b: 2,
      get summary() {
        return `l1:${this.b}`
      },
      get doubled() {
        return this.b * 2
      },
      l2: {
        c: 3,
        get summary() {
          return `l2:${this.c}`
        },
        l3: {
          d: 4,
          // Cross-level dependency: reads own `d` AND child's `product` getter
          get summary() {
            return `l3:${this.d}+product=${this.l4.product}`
          },
          l4: {
            e: 5,
            get product() {
              return this.e * 10
            },
            l5: {
              f: 6,
              get summary() {
                return `l5:${this.f}`
              },
            },
          },
        },
      },
    },
  },
}
