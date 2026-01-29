/**
 * Store factory functions for integration tests
 *
 * Provides typed store creation for each test scenario.
 * Eliminates repetitive store setup across test files.
 *
 * ## Purpose
 * Each factory creates a fully-typed store for a specific test scenario.
 * Factories ensure consistent store configuration and enable IDE autocomplete
 * for paths and values.
 *
 * ## Usage Pattern
 * ```typescript
 * import { createRegistrationFormStore, registrationFormFixtures } from '../mocks'
 *
 * const store = createRegistrationFormStore()
 * render(
 *   <store.Provider initialState={registrationFormFixtures.empty}>
 *     <FormComponent />
 *   </store.Provider>
 * )
 * ```
 *
 * ## Available Stores
 * - **Registration Form** - Basic validation, dependent fields
 * - **Profile Form** - Sync paths (firstName + lastName → fullName)
 * - **Shopping Cart** - Aggregations (item subtotals → cart total)
 * - **Nested Cart** - Deep paths, category subtotals
 * - **Product Form** - Conditional UI (digital vs physical)
 * - **User Profile** - Side effects, timestamps
 * - **Wizard Form** - Multi-step workflows
 * - **Form with Errors** - Error handling patterns
 * - **Optimization** - Render optimization scenarios
 *
 * ## Companion Files
 * - Use with `fixtures.ts` for initial state data
 * - Use with `helpers.ts` for validators and test patterns
 */

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
 * Create an Optimization store (Render Optimization)
 */
export const createOptimizationStore = () => {
  return createGenericStore<OptimizationState>()
}
