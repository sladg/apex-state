/**
 * Shared setup, fixtures, and types for integrations_v2 tests
 */

import { createGenericStore } from '../../src'

/**
 * Simple test state interface - flat structure
 * to focus on API behavior without complex nesting
 */
export interface TestState {
  // Basic fields
  fieldA: string
  fieldB: string
  fieldC: number

  // Fields for sync testing
  syncSource: string
  syncTarget: string

  // Fields for flip testing
  boolA: boolean
  boolB: boolean

  // Counter for listener validation
  listenerCallCount: number

  // Errors/validation
  _errors: Record<string, string[]>
}

/**
 * Fixture: Empty state
 */
export const emptyState: TestState = {
  fieldA: '',
  fieldB: '',
  fieldC: 0,
  syncSource: '',
  syncTarget: '',
  boolA: false,
  boolB: false,
  listenerCallCount: 0,
  _errors: {},
}

/**
 * Fixture: Populated state
 */
export const populatedState: TestState = {
  fieldA: 'value-a',
  fieldB: 'value-b',
  fieldC: 42,
  syncSource: 'source-value',
  syncTarget: 'old-target',
  boolA: true,
  boolB: false,
  listenerCallCount: 0,
  _errors: {},
}

/**
 * Create store with proper typing
 */
export const createTestStore = () => {
  return createGenericStore<TestState>()
}

/**
 * Helper to assert listener was called N times
 * (used in listener tests)
 */
export const trackListenerCalls = (
  initialState: TestState,
  expectedCallCount: number,
) => {
  return {
    // Return object with state that has incrementing counter
    expectedCount: expectedCallCount,
  }
}
