/**
 * Test mocks and utilities - central export
 *
 * Re-exports all test utilities, fixtures, types, and store factories
 * for convenient importing in test files.
 *
 * ## When to Use Mocks vs Utils
 *
 * ### Use `tests/mocks/` (THIS FILE) for:
 * - **Integration tests** - Full React component testing with stores
 * - **Store factories** - Pre-configured typed stores for scenarios
 * - **Fixtures** - Initial state data for forms, carts, profiles
 * - **Test helpers** - Validators, generators, DOM queries, assertions
 * - **Form testing patterns** - Common form filling, submission, validation flows
 *
 * ### Use `tests/concerns/test-utils.ts` for:
 * - **Concern benchmarking** - Performance measurement of concern evaluations
 * - **Evaluation tracking** - Logging which concerns fire and when
 * - **Concern spies** - Mock concern evaluators for unit testing
 * - **Render tracking** - React component render count monitoring
 *
 * ### Note on `tests/utils/react.ts`
 * The `tests/utils/react.ts` file contains newer React testing utilities
 * (createTestStore, renderWithStore, flushEffects). Some functions overlap
 * with this mocks file (assertions, domHelpers). **Prefer using mocks for
 * integration tests**, as it's the established pattern. The react.ts utils
 * are designed for lower-level React testing without full store setup.
 *
 * ## Quick Reference
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
 * ### Form Validation Testing
 * ```typescript
 * import { validators, errorMessages, domHelpers } from '../mocks'
 *
 * // Use validators in concern schemas
 * expect(validators.email('test@example.com')).toBe(true)
 *
 * // Check for validation errors
 * expect(domHelpers.hasErrors()).toBe(true)
 * expect(domHelpers.getAllErrors()).toContain(errorMessages.emailInvalid)
 * ```
 *
 * ### Data Generation
 * ```typescript
 * import { generators } from '../mocks'
 *
 * const email = generators.email() // 'test-xyz123@example.com'
 * const username = generators.username() // 'user_abc456'
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
 * - `helpers.ts` - Validators, generators, DOM helpers, assertions
 */

// Types
export type {
  AddressInfo,
  CartItem,
  FormWithErrors,
  NestedCart,
  OptimizationState,
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
  createOptimizationStore,
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
  optimizationFixtures,
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
