/**
 * Test mocks and utilities - central export
 *
 * Re-exports all test utilities, fixtures, types, and store factories
 * for convenient importing in test files.
 *
 * Usage:
 * ```typescript
 * import {
 *   createRegistrationFormStore,
 *   registrationFormFixtures,
 *   validators,
 *   errorMessages,
 * } from '../mocks'
 * ```
 */

// Types
export type {
  AddressInfo,
  CartItem,
  FormWithErrors,
  NestedCart,
  PersonalInfo,
  ProductForm,
  ProfileForm,
  RegistrationForm,
  ShoppingCart,
  UserProfile,
  WizardForm,
} from './types'

// Store factories
export {
  createFormWithErrorsStore,
  createNestedCartStore,
  createProductFormStore,
  createProfileFormStore,
  createRegistrationFormStore,
  createShoppingCartStore,
  createUserProfileStore,
  createWizardFormStore,
  storeFactories,
} from './stores'

// Fixtures
export {
  formWithErrorsFixtures,
  nestedCartFixtures,
  productFormFixtures,
  profileFormFixtures,
  registrationFormFixtures,
  shoppingCartFixtures,
  userProfileFixtures,
  wizardFormFixtures,
} from './fixtures'

// Helpers
export {
  assertions,
  domHelpers,
  errorMessages,
  generators,
  testPatterns,
  typeHelpers,
  validators,
} from './helpers'
