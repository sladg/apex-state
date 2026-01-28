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
  RegistrationForm,
  ProfileForm,
  CartItem,
  ShoppingCart,
  NestedCart,
  ProductForm,
  UserProfile,
  PersonalInfo,
  AddressInfo,
  WizardForm,
  FormWithErrors,
} from './types'

// Store factories
export {
  createRegistrationFormStore,
  createProfileFormStore,
  createShoppingCartStore,
  createNestedCartStore,
  createProductFormStore,
  createUserProfileStore,
  createWizardFormStore,
  createFormWithErrorsStore,
  storeFactories,
} from './stores'

// Fixtures
export {
  registrationFormFixtures,
  profileFormFixtures,
  shoppingCartFixtures,
  nestedCartFixtures,
  productFormFixtures,
  userProfileFixtures,
  wizardFormFixtures,
  formWithErrorsFixtures,
} from './fixtures'

// Helpers
export {
  validators,
  errorMessages,
  assertions,
  testPatterns,
  domHelpers,
  generators,
} from './helpers'
