/**
 * Vitest test setup
 *
 * This file runs before all tests to configure the testing environment
 */

import { expect, afterEach } from 'vitest'
import { cleanup } from '@testing-library/react'

// Cleanup after each test
afterEach(() => {
  cleanup()
})

// Extend expect if needed in the future
// import * as matchers from '@testing-library/jest-dom/matchers'
// expect.extend(matchers)
