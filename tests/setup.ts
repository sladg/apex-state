/// <reference types="@testing-library/jest-dom" />

/**
 * Vitest test setup
 *
 * This file runs before all tests to configure the testing environment
 */

import { expect, afterEach, vi } from 'vitest'
import { cleanup } from '@testing-library/react'
import * as matchers from '@testing-library/jest-dom/matchers'

// Enable jest-dom matchers for vitest
expect.extend(matchers)

// Cleanup after each test
afterEach(() => {
  cleanup()
})

/**
 * Flush all pending updates (microtasks, valtio changes, React renders)
 * Used as a replacement for waitFor() for better performance and determinism
 *
 * This flushes:
 * - All pending microtasks (Promises, queueMicrotask, valtio subscribers)
 * - Multiple rounds to catch cascading effects
 */
async function flushEffects() {
  // Run multiple rounds to handle cascading effects (concerns triggering more changes)
  for (let i = 0; i < 3; i++) {
    await new Promise<void>(resolve => {
      // Use queueMicrotask to wait for all synchronous and scheduled updates
      queueMicrotask(() => {
        // Additional queueMicrotask to ensure we're after React's batch cycle
        queueMicrotask(() => resolve())
      })
    })
  }
}

// Make flushEffects available globally in tests without imports
declare global {
  function flushEffects(): Promise<void>
}

globalThis.flushEffects = flushEffects
