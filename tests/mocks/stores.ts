/**
 * Store factory functions for integration tests
 *
 * Provides typed store creation for each test scenario.
 * Eliminates repetitive store setup across test files.
 */

import { createGenericStore } from '../../src'
import type {
  RegistrationForm,
  ProfileForm,
  ShoppingCart,
  NestedCart,
  ProductForm,
  UserProfile,
  WizardForm,
  FormWithErrors,
} from './types'

/**
 * Create a Registration Form store
 */
export const createRegistrationFormStore = () => {
  return createGenericStore<RegistrationForm>()
}

/**
 * Create a Profile Form store (Sync Paths)
 */
export const createProfileFormStore = () => {
  return createGenericStore<ProfileForm>()
}

/**
 * Create a Shopping Cart store (Aggregations)
 */
export const createShoppingCartStore = () => {
  return createGenericStore<ShoppingCart>()
}

/**
 * Create a Nested Cart store
 */
export const createNestedCartStore = () => {
  return createGenericStore<NestedCart>()
}

/**
 * Create a Product Form store (Concerns UI)
 */
export const createProductFormStore = () => {
  return createGenericStore<ProductForm>()
}

/**
 * Create a User Profile store (Side Effects)
 */
export const createUserProfileStore = () => {
  return createGenericStore<UserProfile>()
}

/**
 * Create a Wizard Form store (Complex Workflows)
 */
export const createWizardFormStore = () => {
  return createGenericStore<WizardForm>()
}

/**
 * Create a Form with Errors store (Error Handling)
 */
export const createFormWithErrorsStore = () => {
  return createGenericStore<FormWithErrors>()
}

/**
 * Convenience export for all store creators
 */
export const storeFactories = {
  registrationForm: createRegistrationFormStore,
  profileForm: createProfileFormStore,
  shoppingCart: createShoppingCartStore,
  nestedCart: createNestedCartStore,
  productForm: createProductFormStore,
  userProfile: createUserProfileStore,
  wizardForm: createWizardFormStore,
  formWithErrors: createFormWithErrorsStore,
}
