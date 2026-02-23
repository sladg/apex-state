/**
 * Shared type definitions for integration tests
 *
 * Centralized types used across all test scenarios to reduce duplication
 * and maintain consistency across the test suite.
 */

/**
 * Form Validation Scenario
 */
export interface RegistrationForm {
  email: string
  password: string
  confirmPassword: string
  agreeToTerms: boolean
  _errors: Record<string, any[]>
}

// ---------------------------------------------------------------------------
// V2 Integration Test Types
// ---------------------------------------------------------------------------

/**
 * Basic flat state for v2 tests — covers sync, flip, listeners, validation
 */
export interface BasicTestState {
  fieldA: string
  fieldB: string
  fieldC: number
  source: string
  target: string
  boolA: boolean
  boolB: boolean
  email: string
  age: number
  _errors: Record<string, string[]>
}

/**
 * Sync/Flip state — dedicated boolean and string pairs
 */
export interface SyncFlipState {
  source: string
  target: string
  source2: string
  target2: string
  flag1: boolean
  flag2: boolean
  flag3: boolean
  flag4: boolean
}

/**
 * Validation-focused state
 */
export interface ValidationTestState {
  email: string
  age: number
  username: string
  password: string
  confirmPassword: string
  _errors: Record<string, string[]>
}

/**
 * Aggregation state — sources and target for value coordination
 */
export interface AggregationTestState {
  sourceA: string
  sourceB: string
  sourceC: string
  target: string
  numA: number
  numB: number
  numTotal: number
}

/**
 * Listener tracking state — fields + counter for listener output
 */
export interface ListenerTestState {
  user: {
    name: string
    email: string
    age: number
  }
  callCount: number
  lastChange: string
  derived: string
}

/**
 * Deeply nested state — 5 levels deep for nesting tests
 */
export interface DeeplyNestedState {
  level1: {
    value: string
    level2: {
      value: string
      level3: {
        value: string
        level4: {
          value: string
          level5: {
            value: string
            flag: boolean
          }
        }
      }
    }
  }
}
