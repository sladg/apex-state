import type { ListenerRegistration, StoreInstance } from '../../core/types'
import type { GenericMeta } from '../../types'
import { getPathDepth } from '../../utils/path-utils'

/** Auto-incrementing subscriber ID counter for O(1) handler lookup. */
let nextSubscriberId = 0

/** Reset the subscriber ID counter (testing only). */
export const resetSubscriberIdCounter = (): void => {
  nextSubscriberId = 0
}

const updateSortedListenerPaths = (graphs: {
  listeners: Map<string, unknown[]>
  sortedListenerPaths: string[]
}): void => {
  graphs.sortedListenerPaths = Array.from(graphs.listeners.keys()).sort(
    (a, b) => getPathDepth(b) - getPathDepth(a),
  )
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

/** Legacy JS implementation - uses JS listener maps and sorted paths */
export const registerListenerLegacy = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  store: StoreInstance<DATA, META>,
  registration: ListenerRegistration<DATA, META>,
): (() => void) => {
  const { graphs } = store._internal
  const { listeners, listenerHandlers } = graphs

  // Default scope to path when omitted (undefined)
  if (registration.scope === undefined) {
    registration.scope = registration.path
  }

  // Validate that scope is a parent/ancestor of path
  validateScopeAndPath(registration.path, registration.scope ?? null)

  // Assign a unique subscriber_id for O(1) handler lookup
  const subscriberId = nextSubscriberId++

  // Use path as the map key (empty string for null)
  const mapKey = registration.path ?? ''
  const existing = listeners.get(mapKey) ?? []

  // Wrap fn with timing measurement (no-op when timing is disabled)
  const originalFn = registration.fn
  registration.fn = (changes, state) =>
    store._internal.timing.run('listeners', () => originalFn(changes, state), {
      path: mapKey,
      name: 'listener',
    })

  listeners.set(mapKey, [...existing, registration])

  // Store in flat handler map for O(1) dispatch lookup
  listenerHandlers.set(subscriberId, {
    scope: registration.scope,
    fn: registration.fn as (...args: unknown[]) => unknown,
    name: originalFn.name || '(anonymous)',
  })

  // Update sorted paths cache
  updateSortedListenerPaths(graphs)

  return () => {
    const list = listeners.get(mapKey)
    if (list) {
      const filtered = list.filter((l) => l !== registration)
      if (filtered.length > 0) {
        listeners.set(mapKey, filtered)
      } else {
        listeners.delete(mapKey)
      }
      // Update sorted paths cache after removal
      updateSortedListenerPaths(graphs)
    }
    // Remove from flat handler map
    listenerHandlers.delete(subscriberId)
  }
}
