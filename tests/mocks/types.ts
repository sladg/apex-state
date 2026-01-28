/**
 * Shared type definitions for integration tests
 *
 * Centralized types used across all test scenarios to reduce duplication
 * and maintain consistency across the test suite.
 */

/**
 * Form Validation Scenario
 */
export type RegistrationForm = {
  email: string
  password: string
  confirmPassword: string
  agreeToTerms: boolean
  _errors: Record<string, any[]>
}

/**
 * Sync Paths Scenario
 */
export type ProfileForm = {
  firstName: string
  lastName: string
  fullName: string
  displayName: string
}

/**
 * Aggregations Scenario
 */
export type CartItem = {
  name: string
  price: number
  quantity: number
  subtotal: number
}

export type ShoppingCart = {
  items: Record<string, CartItem>
  subtotal: number
  tax: number
  total: number
  itemCount: number
}

export type NestedCart = {
  categories: Record<string, {
    name: string
    items: Record<string, { price: number; qty: number }>
    categorySubtotal: number
  }>
  total: number
}

/**
 * Concerns UI Scenario
 */
export type ProductForm = {
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
export type UserProfile = {
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
export type PersonalInfo = {
  firstName: string
  lastName: string
}

export type AddressInfo = {
  street: string
  city: string
  zipCode: string
}

export type WizardForm = {
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
export type FormWithErrors = {
  email: string
  password: string
  confirmPassword: string
  submitted: boolean
  _errors: Record<string, any[]>
}
