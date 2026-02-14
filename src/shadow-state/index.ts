/**
 * Shadow State Public API
 *
 * Provides a nested object tree that mirrors valtio state structure.
 * Unlike flat HashMaps, this maintains hierarchical relationships and enables
 * efficient subtree operations with automatic valtio integration.
 *
 * @module shadow-state
 *
 * @example
 * ```typescript
 * import { proxy } from 'valtio'
 * import { createShadowState } from './shadow-state'
 *
 * const state = proxy({ user: { name: 'Alice', age: 30 } })
 *
 * const { tree, cleanup } = createShadowState(state, {
 *   onUpdate: (affectedPaths) => {
 *     console.log('Changed paths:', affectedPaths)
 *   }
 * })
 *
 * // Later, when done:
 * cleanup()
 * ```
 */

// ============================================================================
// Types
// ============================================================================

export type {
  ShadowStateIntegrationOptions,
  ShadowStateUpdateCallback,
} from './integration'
export type {
  ShadowNode,
  ShadowTree,
  ShadowTreeOptions,
  UpdateResult,
} from './types'

// ============================================================================
// Core Integration API
// ============================================================================

/**
 * Primary API for creating and syncing shadow state with valtio.
 * Automatically subscribes to valtio proxy changes and keeps shadow tree in sync.
 */
export {
  createShadowState,
  syncShadowTree,
  syncWithValtio,
} from './integration'

// ============================================================================
// Tree Creation & Management
// ============================================================================

/**
 * Low-level tree creation and manipulation functions.
 * Most users should use createShadowState() instead for automatic valtio integration.
 */
export { createShadowTree, insertNode, removeNode } from './tree'

// ============================================================================
// Path Utilities
// ============================================================================

/**
 * Path parsing and manipulation utilities.
 * Supports both dot notation (a.b.c) and bracket notation (a[0].b).
 */
export { isValidPath, joinPath, parsePath } from './pathParser'

// ============================================================================
// Tree Operations
// ============================================================================

/**
 * Operations for querying and traversing shadow state trees.
 */
export {
  collectPaths,
  countDescendants,
  getChildren,
  getDepth,
  getNode,
  getNodeBySegments,
  getParent,
  getValue,
  hasPath,
  isLeaf,
  isRoot,
  traversePath,
} from './operations'

// ============================================================================
// Update Operations
// ============================================================================

/**
 * Operations for updating shadow state trees and calculating affected paths.
 */
export {
  calculateAffectedPaths,
  getAncestorPaths,
  getDescendantPaths,
  isAncestorOf,
  isDescendantOf,
  mergeAffectedPaths,
  replaceSubtree,
  setValue,
  updateNode,
} from './operations'
