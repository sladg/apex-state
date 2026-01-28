/**
 * Global test utilities available in all test files
 */

declare global {
  /**
   * Flush all pending effects, microtasks, and valtio updates
   * More efficient than waitFor() - synchronously processes all pending state changes
   */
  function flushEffects(): Promise<void>
}

export {}
