import { createGenericStore } from '../../src'
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

// Types
export type {
  CartItem,
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

// Store factories
export const createRegistrationFormStore = () => {
  return createGenericStore<RegistrationForm>()
}

export const createProfileFormStore = () => {
  return createGenericStore<ProfileForm>()
}

export const createShoppingCartStore = () => {
  return createGenericStore<ShoppingCart>()
}

export const createNestedCartStore = () => {
  return createGenericStore<NestedCart>()
}

export const createProductFormStore = () => {
  return createGenericStore<ProductForm>()
}

export const createUserProfileStore = () => {
  return createGenericStore<UserProfile>()
}

export const createWizardFormStore = () => {
  return createGenericStore<WizardForm>()
}

export const createFormWithErrorsStore = () => {
  return createGenericStore<FormWithErrors>()
}

export const createOptimizationStore = () => {
  return createGenericStore<OptimizationState>()
}

// Fixtures
export {
  formWithErrorsFixtures,
  nestedCartFixtures,
  optimizationFixtures,
  productFormFixtures,
  profileFormFixtures,
  registrationFormFixtures,
  shoppingCartFixtures,
  userProfileFixtures,
  wizardFormFixtures,
} from './fixtures'

// Helpers
export { typeHelpers } from './helpers'
