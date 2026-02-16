/**
 * WASM Implementation - Listener Registration
 *
 * Registers listeners in WASM router, skips JS listeners Map and sortedListenerPaths.
 * Keeps listenerHandlers Map (needed by execution plan).
 */

import { wasm } from '../../wasm/bridge'

/** Auto-incrementing subscriber ID counter for O(1) handler lookup. */
let nextSubscriberId = 0

/** Reset the subscriber ID counter (testing only). */
export const resetSubscriberIdCounter = (): void => {
  nextSubscriberId = 0
}

/**
 * Helper to validate that scope is a parent/ancestor of path
 */
const validateScopeAndPath = (
  path: string | null,
  scope: string | null,
): void => {
  // If either is null, validation passes
  if (path === null || scope === null) return

  // If scope === path, that's valid
  if (path === scope) return

  // Scope must be a prefix of path (parent/ancestor)
  // e.g., path: 'a.b.c', scope: 'a.b' ✅
  // e.g., path: 'a.b.c', scope: '1.2.3' ❌
  if (!path.startsWith(scope + '.')) {
    throw new Error(
      `Invalid listener: scope '${scope}' must be a parent/ancestor of path '${path}', or one must be null`,
    )
  }
}

export const registerListener: typeof import('./listeners').registerListener = (
  store,
  registration,
) => {
  const { listenerHandlers } = store._internal.graphs

  // Validate that scope is a parent/ancestor of path
  validateScopeAndPath(registration.path, registration.scope)

  // Assign a unique subscriber_id for O(1) handler lookup
  const subscriberId = nextSubscriberId++

  // Wrap fn with timing measurement (no-op when timing is disabled)
  const originalFn = registration.fn
  const mapKey = registration.path ?? ''
  registration.fn = (changes, state) =>
    store._internal.timing.run('listeners', () => originalFn(changes, state), {
      path: mapKey,
      name: 'listener',
    })

  // BOTH modes: Store in flat handler map for O(1) dispatch lookup
  // (executeFullExecutionPlan in processChanges.ts needs this)
  listenerHandlers.set(subscriberId, {
    scope: registration.scope,
    fn: registration.fn as (...args: unknown[]) => unknown,
  })

  // WASM mode: register in WASM only, skip listeners Map and sortedListenerPaths
  wasm.registerListenersBatch([
    {
      subscriber_id: subscriberId,
      topic_path: registration.path ?? '',
      scope_path: registration.scope ?? '',
    },
  ])

  // Return WASM-only cleanup
  return () => {
    // Remove from flat handler map
    listenerHandlers.delete(subscriberId)
    // Unregister from WASM
    wasm.unregisterListenersBatch([subscriberId])
  }
}
