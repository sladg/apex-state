/**
 * Shared setup for integrations_v2 tests
 *
 * Re-exports everything v2 tests need from centralized locations.
 * No custom store factories — use createGenericStore directly.
 *
 * See tests/utils/TESTING_PATTERNS.md for usage guide.
 */

// Types — use these for store creation
export type {
  AggregationTestState,
  BasicTestState,
  DeeplyNestedState,
  ListenerTestState,
  OrderState,
  SyncFlipState,
  ValidationTestState,
} from '../mocks'

// Fixtures — typed constants, no builders
export {
  aggregationTestFixtures,
  basicTestFixtures,
  deeplyNestedFixtures,
  listenerTestFixtures,
  orderFixtures,
  syncFlipFixtures,
  validationTestFixtures,
} from '../mocks'

// React test utilities
export {
  fireEvent,
  flushEffects,
  flushSync,
  MODES,
  mountStore,
} from '../utils/react'
