/// <reference types="@testing-library/jest-dom" />

/**
 * Vitest test setup
 *
 * This file runs before all tests to configure the testing environment
 */

import * as matchers from '@testing-library/jest-dom/matchers'
import { cleanup } from '@testing-library/react'
import { afterEach, expect } from 'vitest'

// Enable jest-dom matchers for vitest
expect.extend(matchers)

// Suppress React act() warnings for valtio async updates
// Valtio's subscribe() triggers async state updates that are expected behavior
const originalError = console.error
console.error = (...args: unknown[]) => {
  const message = args[0]
  if (
    typeof message === 'string' &&
    message.includes('was not wrapped in act')
  ) {
    return
  }
  originalError.apply(console, args)
}

// Cleanup after each test
afterEach(() => {
  cleanup()
})
