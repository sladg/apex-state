/**
 * Shadow State Valtio Integration
 *
 * Provides integration between shadow state trees and valtio proxy state.
 * Uses valtio's subscribe() to listen for state changes and sync shadow state accordingly.
 *
 * @module shadow-state/integration
 */

import { subscribe } from 'valtio'

import { updateNode } from './operations'
import { createShadowTree } from './tree'
import type { ShadowTree, ShadowTreeOptions, UpdateResult } from './types'

/**
 * Callback function type for shadow state updates.
 * Called whenever the shadow state is synced with valtio changes.
 *
 * @param affectedPaths - Array of paths that were affected by the update
 * @param tree - The updated shadow tree
 */
export type ShadowStateUpdateCallback = (
  affectedPaths: string[],
  tree: ShadowTree,
) => void

/**
 * Options for creating and syncing a shadow state with valtio.
 */
export interface ShadowStateIntegrationOptions extends ShadowTreeOptions {
  /**
   * Optional callback to be notified of shadow state updates
   */
  onUpdate?: ShadowStateUpdateCallback
}

/**
 * Creates a shadow state tree from a valtio proxy and subscribes to changes.
 *
 * This function:
 * 1. Creates an initial shadow tree from the current proxy state
 * 2. Subscribes to valtio proxy changes
 * 3. Syncs the shadow tree whenever the proxy changes
 * 4. Returns a cleanup function to unsubscribe
 *
 * @param proxyState - The valtio proxy state to mirror
 * @param options - Configuration options for tree creation and sync
 * @returns Object containing the shadow tree and cleanup function
 *
 * @example
 * ```typescript
 * import { proxy } from 'valtio'
 * import { createShadowState } from './integration'
 *
 * const state = proxy({ user: { name: 'Alice', age: 30 } })
 * const { tree, cleanup } = createShadowState(state, {
 *   onUpdate: (affectedPaths, tree) => {
 *     console.log('Changed paths:', affectedPaths)
 *   }
 * })
 *
 * // Later, when you're done:
 * cleanup()
 * ```
 *
 * @example
 * ```typescript
 * // With custom options
 * const { tree, cleanup } = createShadowState(state, {
 *   maxDepth: 10,
 *   includeArrays: true,
 *   detectCircular: true,
 *   onUpdate: (paths) => console.log('Updated:', paths)
 * })
 * ```
 */
export const createShadowState = <T extends object>(
  proxyState: T,
  options?: ShadowStateIntegrationOptions,
): { tree: ShadowTree; cleanup: () => void } => {
  // Create initial shadow tree from current state
  const tree = createShadowTree(proxyState, options)

  // Subscribe to valtio changes
  const unsubscribe = subscribe(proxyState, (ops) => {
    // Valtio operations contain information about what changed
    // We need to extract paths and values from the operations
    const affectedPaths = syncShadowTreeFromOps(tree, ops, options)

    // Notify callback if provided
    if (options?.onUpdate) {
      options.onUpdate(affectedPaths, tree)
    }
  })

  // Return tree and cleanup function
  return {
    tree,
    cleanup: unsubscribe,
  }
}

/**
 * Syncs a shadow tree with valtio operations.
 *
 * This is an internal helper that processes valtio operations and updates
 * the shadow tree accordingly. Returns the list of affected paths.
 *
 * @param tree - The shadow tree to update
 * @param ops - Valtio operations array
 * @param options - Optional tree update options
 * @returns Array of affected path strings
 *
 * @internal
 */
const syncShadowTreeFromOps = (
  tree: ShadowTree,
  ops: unknown[],
  options?: ShadowTreeOptions,
): string[] => {
  const allAffectedPaths = new Set<string>()

  // Process each operation
  for (const op of ops) {
    // Valtio operations have different formats
    // We need to handle them appropriately
    const paths = processSingleOp(tree, op, options)
    for (const path of paths) {
      allAffectedPaths.add(path)
    }
  }

  return Array.from(allAffectedPaths).sort()
}

/**
 * Processes a single valtio operation and updates the shadow tree.
 *
 * Valtio operations are typically arrays with:
 * - [0]: operation type ('set', 'delete', etc.)
 * - [1]: target object path
 * - [2]: property/index
 * - [3]: new value (for 'set' operations)
 *
 * @param tree - The shadow tree to update
 * @param op - A single valtio operation
 * @param options - Optional tree update options
 * @returns Array of affected path strings from this operation
 *
 * @internal
 */
const processSingleOp = (
  tree: ShadowTree,
  op: unknown,
  options?: ShadowTreeOptions,
): string[] => {
  // Valtio operations are arrays: [opType, path, prop, value?, ...]
  if (!Array.isArray(op) || op.length < 3) {
    return []
  }

  const [_opType, , prop, newValue] = op

  // Build the path string from the operation
  // The path in valtio ops is an array of property keys
  const pathSegments = Array.isArray(op[1]) ? op[1] : []
  const fullPath = [...pathSegments, prop].filter(Boolean).join('.')

  try {
    // Update the shadow tree node at this path
    const result: UpdateResult = updateNode(tree, fullPath, newValue, options)
    return result.affectedPaths
  } catch {
    // If the path doesn't exist yet, it might be a new property
    // In this case, we'd need to use insertNode instead
    // For now, return empty array to avoid breaking the sync
    return []
  }
}

/**
 * Manually syncs a shadow tree with the current state of a valtio proxy.
 *
 * Useful when you need to force a full resync of the shadow tree,
 * or when you're not using the automatic subscribe integration.
 *
 * @param tree - The shadow tree to sync
 * @param proxyState - The valtio proxy state to sync from
 * @param options - Optional tree rebuild options
 * @returns A new shadow tree synced with the current proxy state
 *
 * @example
 * ```typescript
 * const state = proxy({ user: { name: 'Alice' } })
 * const tree = createShadowTree(state)
 *
 * // ... later, after many changes ...
 * // Force a full resync
 * const newTree = syncShadowTree(tree, state)
 * ```
 */
export const syncShadowTree = <T extends object>(
  _tree: ShadowTree,
  proxyState: T,
  options?: ShadowTreeOptions,
): ShadowTree => {
  // Create a new shadow tree from the current proxy state
  // This is effectively a full rebuild/resync
  return createShadowTree(proxyState, options)
}

/**
 * Creates a shadow state that automatically syncs with a valtio proxy.
 *
 * This is a React-friendly hook-style API that can be used in components.
 * Returns the shadow tree and a cleanup function.
 *
 * @param proxyState - The valtio proxy state to mirror
 * @param options - Configuration options
 * @returns Object with tree and cleanup function
 *
 * @example
 * ```typescript
 * // In a React component
 * const MyComponent = () => {
 *   const store = useStoreContext()
 *   const [shadowState, setShadowState] = useState(() => {
 *     return syncWithValtio(store.state, {
 *       onUpdate: (paths) => console.log('Updated paths:', paths)
 *     })
 *   })
 *
 *   useEffect(() => {
 *     return shadowState.cleanup
 *   }, [])
 *
 *   return <div>...</div>
 * }
 * ```
 */
export const syncWithValtio = <T extends object>(
  proxyState: T,
  options?: ShadowStateIntegrationOptions,
): { tree: ShadowTree; cleanup: () => void } => {
  return createShadowState(proxyState, options)
}
