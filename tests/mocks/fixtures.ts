/**
 * Shared test fixtures and initial states
 *
 * All fixtures are based on the unified TestState type. A `defaults` object
 * provides sensible zero-values for every field. Each named fixture spreads
 * `defaults` and overrides the domain-specific fields it needs.
 *
 * ## Usage
 * ```typescript
 * import { testStateFixtures } from '../mocks'
 * import { createGenericStore } from '../../src'
 * import type { TestState } from '../mocks'
 *
 * const store = createGenericStore<TestState>()
 * render(<store.Provider initialState={testStateFixtures.formEmpty}>...)
 * ```
 */

import type { NestedCart, TestState } from './types'

/**
 * Default values for all TestState fields
 */
export const defaults: TestState = {
  // Form fields
  email: '',
  password: '',
  confirmPassword: '',
  agreeToTerms: false,
  submitted: false,

  // User profile fields
  username: '',
  age: 0,
  bio: '',
  isActive: false,
  lastUpdated: 0,

  // Product form fields
  type: 'physical',
  name: '',
  price: 0,
  requiresShipping: true,
  taxable: true,
  isPublished: false,

  // Shopping cart fields
  items: {},
  subtotal: 0,
  tax: 0,
  total: 0,
  itemCount: 0,

  // Wizard form fields
  currentStep: 1,
  personalInfo: { firstName: '', lastName: '' },
  addressInfo: { street: '', city: '', zipCode: '' },
  reviewData: { allFilled: false, isValid: false },

  // Optimization fields
  val: '',
  isInternal: false,

  // Profile sync fields
  firstName: '',
  lastName: '',
  fullName: '',
  displayName: '',

  // Shared error storage
  _errors: {},
}

/**
 * Unified test state fixtures
 */
export const testStateFixtures = {
  // --- Form Validation & Error Handling ---
  formEmpty: {
    ...defaults,
  } satisfies TestState,

  formPartial: {
    ...defaults,
    email: 'john@example.com',
    password: 'Test123',
    confirmPassword: 'Test123',
  } satisfies TestState,

  formComplete: {
    ...defaults,
    email: 'john@example.com',
    password: 'StrongPass123',
    confirmPassword: 'StrongPass123',
    agreeToTerms: true,
  } satisfies TestState,

  formWithErrors: {
    ...defaults,
    email: 'invalid',
    password: 'weak',
    confirmPassword: 'weak',
    _errors: {
      email: ['Invalid email format'],
      password: ['Password too short'],
    },
  } satisfies TestState,

  formValid: {
    ...defaults,
    email: 'valid@example.com',
    password: 'StrongPass123',
    confirmPassword: 'StrongPass123',
  } satisfies TestState,

  formSubmitted: {
    ...defaults,
    email: 'valid@example.com',
    password: 'StrongPass123',
    confirmPassword: 'StrongPass123',
    submitted: true,
  } satisfies TestState,

  // --- User Profile (Side Effects) ---
  profileEmpty: {
    ...defaults,
  } satisfies TestState,

  profileFilled: {
    ...defaults,
    username: 'john_doe',
    email: 'john@example.com',
    age: 30,
    bio: 'Software developer',
    isActive: true,
    lastUpdated: Date.now(),
  } satisfies TestState,

  profileWithErrors: {
    ...defaults,
    username: 'admin',
    email: 'invalid-email',
    age: 25,
    isActive: false,
    lastUpdated: Date.now(),
    _errors: {
      username: ['Username is taken'],
      email: ['Invalid email format'],
    },
  } satisfies TestState,

  // --- Profile Sync ---
  profileSyncEmpty: {
    ...defaults,
  } satisfies TestState,

  profileSyncFilled: {
    ...defaults,
    firstName: 'John',
    lastName: 'Doe',
    fullName: 'John Doe',
    displayName: 'John',
  } satisfies TestState,

  profileSyncUpdated: {
    ...defaults,
    firstName: 'Jane',
    lastName: 'Smith',
    fullName: 'Jane Smith',
    displayName: 'Jane',
  } satisfies TestState,

  // --- Product Form (Concerns UI) ---
  productEmpty: {
    ...defaults,
    type: 'physical' as const,
    requiresShipping: true,
    taxable: true,
  } satisfies TestState,

  productDigital: {
    ...defaults,
    type: 'digital' as const,
    name: 'Software Suite',
    price: 99,
    downloadUrl: 'https://download.example.com/software.zip',
    requiresShipping: false,
    taxable: false,
  } satisfies TestState,

  productPhysical: {
    ...defaults,
    type: 'physical' as const,
    name: 'Widget',
    price: 29.99,
    weight: 2.5,
    requiresShipping: true,
    taxable: true,
    isPublished: true,
  } satisfies TestState,

  // --- Shopping Cart (Aggregations) ---
  cartEmpty: {
    ...defaults,
  } satisfies TestState,

  cartSingleItem: {
    ...defaults,
    items: {
      'item-1': {
        name: 'Product A',
        price: 20,
        quantity: 1,
        subtotal: 20,
      },
    },
    subtotal: 20,
    tax: 2,
    total: 22,
    itemCount: 1,
  } satisfies TestState,

  cartMultipleItems: {
    ...defaults,
    items: {
      'item-1': {
        name: 'Product A',
        price: 20,
        quantity: 2,
        subtotal: 40,
      },
      'item-2': {
        name: 'Product B',
        price: 30,
        quantity: 1,
        subtotal: 30,
      },
    },
    subtotal: 70,
    tax: 7,
    total: 77,
    itemCount: 2,
  } satisfies TestState,

  // --- Wizard Form (Complex Workflows) ---
  wizardStep1Empty: {
    ...defaults,
    currentStep: 1 as const,
    personalInfo: { firstName: '', lastName: '' },
    addressInfo: { street: '', city: '', zipCode: '' },
    reviewData: { allFilled: false, isValid: false },
  } satisfies TestState,

  wizardStep1Filled: {
    ...defaults,
    currentStep: 1 as const,
    personalInfo: { firstName: 'John', lastName: 'Doe' },
    addressInfo: { street: '', city: '', zipCode: '' },
    reviewData: { allFilled: false, isValid: false },
  } satisfies TestState,

  wizardStep2Filled: {
    ...defaults,
    currentStep: 2 as const,
    personalInfo: { firstName: 'John', lastName: 'Doe' },
    addressInfo: {
      street: '123 Main St',
      city: 'Springfield',
      zipCode: '12345',
    },
    reviewData: { allFilled: false, isValid: false },
  } satisfies TestState,

  wizardStep3Review: {
    ...defaults,
    currentStep: 3 as const,
    personalInfo: { firstName: 'John', lastName: 'Doe' },
    addressInfo: {
      street: '123 Main St',
      city: 'Springfield',
      zipCode: '12345',
    },
    reviewData: { allFilled: true, isValid: true },
  } satisfies TestState,

  // --- Optimization ---
  optimizationInitial: {
    ...defaults,
    val: 'A',
  } satisfies TestState,
}

/**
 * Nested Cart fixtures (separate domain, TC3.8 only)
 */
export const nestedCartFixtures = {
  withElectronics: {
    categories: {
      electronics: {
        name: 'Electronics',
        items: { 'item-1': { price: 100, qty: 1 } },
        categorySubtotal: 100,
      },
    },
    total: 100,
  } satisfies NestedCart,

  withMultipleCategories: {
    categories: {
      electronics: {
        name: 'Electronics',
        items: {
          'item-1': { price: 100, qty: 1 },
          'item-2': { price: 50, qty: 2 },
        },
        categorySubtotal: 200,
      },
      books: {
        name: 'Books',
        items: {
          'book-1': { price: 20, qty: 3 },
        },
        categorySubtotal: 60,
      },
    },
    total: 260,
  } satisfies NestedCart,
}
