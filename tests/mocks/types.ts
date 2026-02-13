/**
 * Shared type definitions for integration tests
 *
 * Centralized types used across all test scenarios to reduce duplication
 * and maintain consistency across the test suite.
 */

/**
 * Companion sub-types used within TestState
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

export interface CartItem {
  name: string
  price: number
  quantity: number
  subtotal: number
}

/**
 * Unified test state type
 *
 * Combines all domain-specific fields into a single type used across
 * all integration tests. Each test scenario uses a subset of fields
 * via fixtures that provide appropriate defaults and overrides.
 */
export interface TestState {
  // Form fields
  email: string
  password: string
  confirmPassword: string
  agreeToTerms: boolean
  submitted: boolean

  // User profile fields
  username: string
  age: number
  bio: string
  isActive: boolean
  lastUpdated: number

  // Product form fields
  type: 'digital' | 'physical'
  name: string
  price: number
  weight?: number
  downloadUrl?: string
  requiresShipping: boolean
  taxable: boolean
  isPublished: boolean

  // Shopping cart fields
  items: Record<string, CartItem>
  subtotal: number
  tax: number
  total: number
  itemCount: number

  // Wizard form fields
  currentStep: 1 | 2 | 3
  personalInfo: PersonalInfo
  addressInfo: AddressInfo
  reviewData: {
    allFilled: boolean
    isValid: boolean
    submittedAt?: number
  }

  // Optimization fields
  val: string
  isInternal: boolean

  // Profile sync fields
  firstName: string
  lastName: string
  fullName: string
  displayName: string

  // Shared error storage
  _errors: Record<string, string[]>
}

/**
 * Nested cart type (separate domain, used only in TC3.8 with its own store)
 */
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
