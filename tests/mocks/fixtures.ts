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
 * ## Usage Pattern
 * ```typescript
 * import { createRegistrationFormStore, registrationFormFixtures } from '../mocks'
 *
 * const store = createRegistrationFormStore()
 *
 * // Start with empty form
 * render(<store.Provider initialState={registrationFormFixtures.empty}>...)
 *
 * // Or start with partial data
 * render(<store.Provider initialState={registrationFormFixtures.partial}>...)
 * ```
 *
 * ## Fixture Naming Convention
 * Each fixture set follows a consistent naming pattern:
 * - **empty** - All fields empty/default
 * - **partial** - Some fields filled
 * - **complete/filled** - All required fields valid
 * - **withErrors** - Pre-populated with validation errors
 * - **updated** - Modified state for testing updates
 * - **step[N]** - Multi-step workflow states (wizard forms)
 *
 * ## Benefits
 * - Type-safe: Uses `satisfies` to catch schema mismatches
 * - Consistent: Same structure across all tests
 * - DRY: No repeated initial state objects
 * - Discoverable: IDE autocomplete shows all fixtures
 *
 * ## Companion Files
 * - Use with `stores.ts` factory functions
 * - Combine with `helpers.ts` for validation and test patterns
 */

import type {
  FormWithErrors,
  NestedCart,
  OptimizationState,
  ProductForm,
  ProfileForm,
  RegistrationForm,
  ShoppingCart,
  UserProfile,
  WizardForm,
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

/**
 * Profile Form fixtures (Sync Paths)
 */
export const profileFormFixtures = {
  empty: {
    firstName: '',
    lastName: '',
    fullName: '',
    displayName: '',
  } satisfies ProfileForm,

  filled: {
    firstName: 'John',
    lastName: 'Doe',
    fullName: 'John Doe',
    displayName: 'John',
  } satisfies ProfileForm,

  updated: {
    firstName: 'Jane',
    lastName: 'Smith',
    fullName: 'Jane Smith',
    displayName: 'Jane',
  } satisfies ProfileForm,
}

/**
 * Shopping Cart fixtures (Aggregations)
 */
export const shoppingCartFixtures = {
  empty: {
    items: {},
    subtotal: 0,
    tax: 0,
    total: 0,
    itemCount: 0,
  } satisfies ShoppingCart,

  singleItem: {
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
  } satisfies ShoppingCart,

  multipleItems: {
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
  } satisfies ShoppingCart,
}

/**
 * Nested Cart fixtures
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

/**
 * Product Form fixtures (Concerns UI)
 */
export const productFormFixtures = {
  empty: {
    type: 'physical' as const,
    name: '',
    price: 0,
    requiresShipping: true,
    taxable: true,
    isPublished: false,
    _errors: {},
  } satisfies ProductForm,

  digitalProduct: {
    type: 'digital' as const,
    name: 'Software Suite',
    price: 99,
    downloadUrl: 'https://download.example.com/software.zip',
    requiresShipping: false,
    taxable: false,
    isPublished: false,
    _errors: {},
  } satisfies ProductForm,

  physicalProduct: {
    type: 'physical' as const,
    name: 'Widget',
    price: 29.99,
    weight: 2.5,
    requiresShipping: true,
    taxable: true,
    isPublished: true,
    _errors: {},
  } satisfies ProductForm,
}

/**
 * User Profile fixtures (Side Effects)
 */
export const userProfileFixtures = {
  empty: {
    username: '',
    email: '',
    age: 0,
    bio: '',
    isActive: false,
    lastUpdated: 0,
    _errors: {},
  } satisfies UserProfile,

  filled: {
    username: 'john_doe',
    email: 'john@example.com',
    age: 30,
    bio: 'Software developer',
    isActive: true,
    lastUpdated: Date.now(),
    _errors: {},
  } satisfies UserProfile,

  withErrors: {
    username: 'admin',
    email: 'invalid-email',
    age: 25,
    bio: '',
    isActive: false,
    lastUpdated: Date.now(),
    _errors: {
      username: ['Username is taken'],
      email: ['Invalid email format'],
    },
  } satisfies UserProfile,
}

/**
 * Wizard Form fixtures (Complex Workflows)
 */
export const wizardFormFixtures = {
  step1Empty: {
    currentStep: 1,
    personalInfo: { firstName: '', lastName: '' },
    addressInfo: { street: '', city: '', zipCode: '' },
    reviewData: { allFilled: false, isValid: false },
    _errors: {},
  } satisfies WizardForm,

  step1Filled: {
    currentStep: 1,
    personalInfo: { firstName: 'John', lastName: 'Doe' },
    addressInfo: { street: '', city: '', zipCode: '' },
    reviewData: { allFilled: false, isValid: false },
    _errors: {},
  } satisfies WizardForm,

  step2Filled: {
    currentStep: 2,
    personalInfo: { firstName: 'John', lastName: 'Doe' },
    addressInfo: {
      street: '123 Main St',
      city: 'Springfield',
      zipCode: '12345',
    },
    reviewData: { allFilled: false, isValid: false },
    _errors: {},
  } satisfies WizardForm,

  step3Review: {
    currentStep: 3,
    personalInfo: { firstName: 'John', lastName: 'Doe' },
    addressInfo: {
      street: '123 Main St',
      city: 'Springfield',
      zipCode: '12345',
    },
    reviewData: { allFilled: true, isValid: true },
    _errors: {},
  } satisfies WizardForm,
}

/**
 * Form with Errors fixtures (Error Handling)
 */
export const formWithErrorsFixtures = {
  empty: {
    email: '',
    password: '',
    confirmPassword: '',
    submitted: false,
    _errors: {},
  } satisfies FormWithErrors,

  withValidationErrors: {
    email: 'invalid',
    password: 'weak',
    confirmPassword: 'weak',
    submitted: false,
    _errors: {
      email: ['Invalid email format'],
      password: ['Password too short'],
    },
  } satisfies FormWithErrors,

  valid: {
    email: 'valid@example.com',
    password: 'StrongPass123',
    confirmPassword: 'StrongPass123',
    submitted: false,
    _errors: {},
  } satisfies FormWithErrors,

  submitted: {
    email: 'valid@example.com',
    password: 'StrongPass123',
    confirmPassword: 'StrongPass123',
    submitted: true,
    _errors: {},
  } satisfies FormWithErrors,
}

/**
 * Optimization Scenario fixtures
 */
export const optimizationFixtures = {
  initial: {
    val: 'A',
    isInternal: false,
  } satisfies OptimizationState,
}
