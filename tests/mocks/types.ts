/**
 * Shared type definitions for integration tests
 *
 * Centralized types used across all test scenarios to reduce duplication
 * and maintain consistency across the test suite.
 */

/**
 * Optimization Scenario (for render optimization test)
 */
export interface OptimizationState {
  val: string
  isInternal: boolean
}

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

/**
 * Sync Paths Scenario
 */
export interface ProfileForm {
  firstName: string
  lastName: string
  fullName: string
  displayName: string
}

/**
 * Aggregations Scenario
 */
export interface CartItem {
  name: string
  price: number
  quantity: number
  subtotal: number
}

export interface ShoppingCart {
  items: Record<string, CartItem>
  subtotal: number
  tax: number
  total: number
  itemCount: number
}

export interface NestedCart {
  categories: Record<
    string,
    {
      name: string
      items: Record<string, { price: number; qty: number }>
      categorySubtotal: number
    }
  >
  total: number
}

/**
 * Concerns UI Scenario
 */
export interface ProductForm {
  type: 'digital' | 'physical'
  name: string
  price: number
  weight?: number
  downloadUrl?: string
  requiresShipping: boolean
  taxable: boolean
  isPublished: boolean
  _errors: Record<string, any[]>
}

/**
 * Side Effects Scenario
 */
export interface UserProfile {
  username: string
  email: string
  age: number
  bio: string
  isActive: boolean
  lastUpdated: number
  _errors: Record<string, any[]>
}

/**
 * Complex Workflows Scenario
 */
export interface PersonalInfo {
  firstName: string
  lastName: string
}

export interface AddressInfo {
  street: string
  city: string
  zipCode: string
}

export interface WizardForm {
  currentStep: 1 | 2 | 3
  personalInfo: PersonalInfo
  addressInfo: AddressInfo
  reviewData: {
    allFilled: boolean
    isValid: boolean
    submittedAt?: number
  }
  _errors: Record<string, any[]>
}

/**
 * Error Handling Scenario
 */
export interface FormWithErrors {
  email: string
  password: string
  confirmPassword: string
  submitted: boolean
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

/**
 * E-commerce order state for benchmark and combined-effects tests
 */
export interface OrderState {
  orders: Record<
    string,
    {
      currency: string
      confirmed: boolean
      status: string
      subtotal: number
      tax: number
      total: number
    }
  >
  invoices: Record<string, { pending: boolean }>
}
