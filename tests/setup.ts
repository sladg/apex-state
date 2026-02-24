/// <reference types="@testing-library/jest-dom" />

/**
 * Vitest test setup
 *
 * This file runs before all tests to configure the testing environment
 */

import { act } from 'react'

import * as matchers from '@testing-library/jest-dom/matchers'
import { cleanup, fireEvent as rtlFireEvent } from '@testing-library/react'
import { afterEach, beforeEach, expect } from 'vitest'

import { loadWasm } from '~/wasm/lifecycle'

// Enable jest-dom matchers for vitest
expect.extend(matchers)

// Initialize WASM for all tests
beforeEach(async () => {
  await loadWasm()
})

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

// Cleanup after each test: React component unmounts call pipeline.destroy() automatically.
afterEach(() => {
  cleanup()
})

/**
 * Flush all pending updates (microtasks, macrotasks, valtio changes, React renders)
 * Used as a replacement for waitFor() for better performance and determinism
 *
 * This flushes:
 * - All pending microtasks (Promises, queueMicrotask, valtio subscribers)
 * - All pending macrotasks (setTimeout, setInterval)
 * - Multiple rounds to catch cascading effects
 *
 * All async updates are wrapped in act() to prevent React warnings
 */
async function flushEffects() {
  // Run multiple rounds to handle cascading effects (concerns triggering more changes)
  for (let i = 0; i < 3; i++) {
    await act(async () => {
      await new Promise<void>((resolve) => {
        // Use setTimeout to flush macrotasks (like async validation with setTimeout)
        // Add small delay to ensure all pending timers are queued
        setTimeout(() => {
          // Then use queueMicrotask to flush microtasks (React batching, valtio subscribers)
          queueMicrotask(() => {
            // Additional queueMicrotask to ensure we're after React's batch cycle
            queueMicrotask(() => resolve())
          })
        }, 10)
      })
    })
  }
}

/**
 * Wrapped fireEvent that uses React's act()
 * Ensures React batching works correctly with valtio updates
 */
const fireEvent = {
  change: (element: Element, data: { target: { value: unknown } }) => {
    act(() => {
      rtlFireEvent.change(element, data)
    })
  },
  click: (element: Element) => {
    act(() => {
      rtlFireEvent.click(element)
    })
  },
  blur: (element: Element) => {
    act(() => {
      rtlFireEvent.blur(element)
    })
  },
  focus: (element: Element) => {
    act(() => {
      rtlFireEvent.focus(element)
    })
  },
}

// Type for the wrapped fireEvent
interface FireEventType {
  change: (element: Element, data: { target: { value: unknown } }) => void
  click: (element: Element) => void
  blur: (element: Element) => void
  focus: (element: Element) => void
}

// Make fireEvent available globally in tests without imports
declare global {
  function flushEffects(): Promise<void>
  const fireEvent: FireEventType
}

Object.assign(globalThis, { flushEffects, fireEvent })
