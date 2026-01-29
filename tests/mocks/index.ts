/**
 * Test mocks and utilities - central export
 *
 * Re-exports test utilities, fixtures, types, and store factories
 * for convenient importing in test files.
 *
 * ## Usage
 *
 * ### Store Setup
 * ```typescript
 * import { createRegistrationFormStore, registrationFormFixtures } from '../mocks'
 *
 * const store = createRegistrationFormStore()
 * render(
 *   <store.Provider initialState={registrationFormFixtures.empty}>
 *     <MyForm />
 *   </store.Provider>
 * )
 * ```
 *
 * ### Type-Safe Dynamic Paths
 * ```typescript
 * import { typeHelpers } from '../mocks'
 *
 * // For runtime/template literal paths
 * store.applyChanges([
 *   typeHelpers.change(`items.${itemId}.qty`, 10, {})
 * ])
 * ```
 *
 * ## File Organization
 *
 * - `types.ts` - TypeScript types for all test scenarios
 * - `stores.ts` - Store factory functions (createXStore)
 * - `fixtures.ts` - Initial state data for each scenario
 * - `helpers.ts` - Type-safe helpers for dynamic paths
 */

// Types
export type {
  CartItem,
  NestedCart,
  ProductForm,
  ProfileForm,
  ShoppingCart,
  WizardForm,
} from './types'

// Store factories
export {
  createFormWithErrorsStore,
  createNestedCartStore,
  createOptimizationStore,
  createProductFormStore,
  createProfileFormStore,
  createRegistrationFormStore,
  createShoppingCartStore,
  createUserProfileStore,
  createWizardFormStore,
} from './stores'

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
