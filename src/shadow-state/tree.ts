/**
 * Shadow State Tree Creation and Management
 *
 * Provides functions for creating and managing nested shadow state trees
 * that mirror valtio state structure. Maintains hierarchical relationships
 * and supports efficient subtree operations.
 *
 * @module shadow-state/tree
 */

import { is } from '../utils/is'
import type { ShadowNode, ShadowTree, ShadowTreeOptions } from './types'

/**
 * Default options for tree creation
 */
const DEFAULT_OPTIONS: Required<ShadowTreeOptions> = {
  maxDepth: Infinity,
  includeArrays: true,
  detectCircular: true,
}

/**
 * Builds children nodes for an object value
 */
const buildObjectChildren = (
  obj: Record<string, unknown>,
  currentPath: string[],
  node: ShadowNode,
  buildNode: (
    value: unknown,
    path: string[],
    parent?: ShadowNode,
  ) => ShadowNode,
): Map<string | number, ShadowNode> | undefined => {
  const children = new Map<string | number, ShadowNode>()
  for (const key in obj) {
    if (Object.prototype.hasOwnProperty.call(obj, key)) {
      const childPath = [...currentPath, key]
      const childValue = obj[key]
      const childNode = buildNode(childValue, childPath, node)
      children.set(key, childNode)
    }
  }
  return children.size > 0 ? children : undefined
}

/**
 * Builds children nodes for an array value
 */
const buildArrayChildren = (
  arr: unknown[],
  currentPath: string[],
  node: ShadowNode,
  buildNode: (
    value: unknown,
    path: string[],
    parent?: ShadowNode,
  ) => ShadowNode,
): Map<string | number, ShadowNode> | undefined => {
  const children = new Map<string | number, ShadowNode>()
  for (let i = 0; i < arr.length; i++) {
    const childPath = [...currentPath, String(i)]
    const childValue = arr[i]
    const childNode = buildNode(childValue, childPath, node)
    children.set(i, childNode)
  }
  return children.size > 0 ? children : undefined
}

/**
 * Creates a shadow state tree from a nested object or array.
 *
 * Recursively traverses the input value and builds a tree structure
 * where each node maintains references to its parent and children.
 * Supports circular reference detection and depth limiting.
 *
 * @param value - The valtio state object to mirror as a shadow tree
 * @param options - Configuration options for tree creation
 * @returns A ShadowTree with root node and metadata
 *
 * @example
 * ```typescript
 * const state = { user: { name: 'Alice', age: 30 }, todos: ['Buy milk'] }
 * const shadowTree = createShadowTree(state)
 * // shadowTree.root.children.get('user') contains the user subtree
 * // shadowTree.nodeCount === 5 (root + user + name + age + todos + todos[0])
 * ```
 *
 * @example
 * ```typescript
 * // With options
 * const shadowTree = createShadowTree(state, {
 *   maxDepth: 5,
 *   includeArrays: true,
 *   detectCircular: true
 * })
 * ```
 */
export const createShadowTree = <T = unknown>(
  value: T,
  options?: ShadowTreeOptions,
): ShadowTree => {
  const opts = { ...DEFAULT_OPTIONS, ...options }
  const visited = opts.detectCircular ? new WeakSet<object>() : null
  let nodeCount = 0
  let maxDepth = 0

  /**
   * Recursively builds a shadow node and its children
   */
  const buildNode = (
    currentValue: unknown,
    currentPath: string[],
    parent?: ShadowNode,
  ): ShadowNode => {
    nodeCount++
    const depth = currentPath.length
    if (depth > maxDepth) {
      maxDepth = depth
    }

    const node: ShadowNode = {
      value: currentValue,
      path: currentPath,
      ...(parent && { parent }),
    }

    // Check depth limit
    if (depth >= opts.maxDepth) {
      return node
    }

    // Handle circular references
    if (visited && is.not.primitive(currentValue) && is.not.nil(currentValue)) {
      if (visited.has(currentValue as object)) {
        // Circular reference detected - return node without children
        return node
      }
      visited.add(currentValue as object)
    }

    // Build children for objects and arrays
    if (is.object(currentValue)) {
      const children = buildObjectChildren(
        currentValue,
        currentPath,
        node,
        buildNode,
      )
      if (children) {
        node.children = children
      }
    } else if (is.array(currentValue) && opts.includeArrays) {
      const children = buildArrayChildren(
        currentValue,
        currentPath,
        node,
        buildNode,
      )
      if (children) {
        node.children = children
      }
    } else {
      // Primitive value - no children
    }

    return node
  }

  const root = buildNode(value, [])

  return {
    root,
    nodeCount,
    depth: maxDepth,
  }
}

/**
 * Inserts a new node into the shadow tree at the specified path.
 *
 * Creates parent nodes if they don't exist (similar to mkdir -p).
 * Updates tree metadata (nodeCount, depth) after insertion.
 * Maintains bidirectional parent-child links.
 *
 * @param tree - The shadow tree to modify
 * @param path - Array of path segments to the new node
 * @param value - Value to store in the new node
 * @param options - Options for building the node subtree
 * @returns The inserted node
 *
 * @example
 * ```typescript
 * const tree = createShadowTree({})
 * insertNode(tree, ['user', 'profile', 'name'], 'Alice')
 * // Creates: root -> user -> profile -> name
 * ```
 */
export const insertNode = <T = unknown>(
  tree: ShadowTree,
  path: string[],
  value: T,
  options?: ShadowTreeOptions,
): ShadowNode<T> => {
  if (path.length === 0) {
    throw new Error('Cannot insert at root path')
  }

  const opts = { ...DEFAULT_OPTIONS, ...options }
  let current = tree.root
  let depth = 0

  // Traverse to the parent of the target node, creating nodes as needed
  for (let i = 0; i < path.length - 1; i++) {
    depth++
    const key = path[i]!
    if (!current.children) {
      current.children = new Map()
    }

    let next = current.children.get(key)
    if (!next) {
      // Create intermediate node
      tree.nodeCount++
      next = {
        value: {},
        path: path.slice(0, i + 1),
        parent: current,
        children: new Map(),
      }
      current.children.set(key, next)
    }
    current = next
  }

  // Insert the target node
  depth++
  const key = path[path.length - 1]!
  if (!current.children) {
    current.children = new Map()
  }

  // Build the new node (may have children if value is object/array)
  const visited = opts.detectCircular ? new WeakSet<object>() : null
  let addedNodeCount = 0
  let maxChildDepth = depth

  const buildNode = (
    currentValue: unknown,
    currentPath: string[],
    parent?: ShadowNode,
  ): ShadowNode => {
    addedNodeCount++
    const nodeDepth = currentPath.length
    if (nodeDepth > maxChildDepth) {
      maxChildDepth = nodeDepth
    }

    const node: ShadowNode = {
      value: currentValue,
      path: currentPath,
      ...(parent && { parent }),
    }

    // Check depth limit
    if (nodeDepth >= opts.maxDepth) {
      return node
    }

    // Handle circular references
    if (visited && is.not.primitive(currentValue) && is.not.nil(currentValue)) {
      if (visited.has(currentValue as object)) {
        return node
      }
      visited.add(currentValue as object)
    }

    // Build children for objects and arrays
    if (is.object(currentValue)) {
      const children = buildObjectChildren(
        currentValue,
        currentPath,
        node,
        buildNode,
      )
      if (children) {
        node.children = children
      }
    } else if (is.array(currentValue) && opts.includeArrays) {
      const children = buildArrayChildren(
        currentValue,
        currentPath,
        node,
        buildNode,
      )
      if (children) {
        node.children = children
      }
    } else {
      // Primitive value - no children
    }

    return node
  }

  const newNode = buildNode(value, path, current)
  current.children.set(key, newNode)

  // Update tree metadata
  tree.nodeCount += addedNodeCount
  if (maxChildDepth > tree.depth) {
    tree.depth = maxChildDepth
  }

  return newNode as ShadowNode<T>
}

/**
 * Removes a node and all its descendants from the shadow tree.
 *
 * Updates parent's children map and tree metadata (nodeCount, depth).
 * Returns the number of nodes removed.
 *
 * @param tree - The shadow tree to modify
 * @param path - Array of path segments to the node to remove
 * @returns Number of nodes removed (including descendants)
 *
 * @example
 * ```typescript
 * const tree = createShadowTree({ user: { name: 'Alice', age: 30 } })
 * const removed = removeNode(tree, ['user', 'name'])
 * // removed === 1 (just the name node)
 * const removed2 = removeNode(tree, ['user'])
 * // removed2 === 2 (user node + age node)
 * ```
 */
export const removeNode = (tree: ShadowTree, path: string[]): number => {
  if (path.length === 0) {
    throw new Error('Cannot remove root node')
  }

  // Traverse to the parent of the target node
  let current = tree.root
  for (let i = 0; i < path.length - 1; i++) {
    const key = path[i]!
    if (!current.children) {
      return 0 // Path doesn't exist
    }
    const next = current.children.get(key)
    if (!next) {
      return 0 // Path doesn't exist
    }
    current = next
  }

  // Remove the target node
  const key = path[path.length - 1]!
  if (!current.children) {
    return 0
  }

  const nodeToRemove = current.children.get(key)
  if (!nodeToRemove) {
    return 0
  }

  // Count nodes in subtree
  const countNodes = (node: ShadowNode): number => {
    let count = 1
    if (node.children) {
      for (const child of Array.from(node.children.values())) {
        count += countNodes(child)
      }
    }
    return count
  }

  const removedCount = countNodes(nodeToRemove)
  current.children.delete(key)

  // Update tree metadata
  tree.nodeCount -= removedCount

  // Recalculate depth if needed (only if we might have removed the deepest node)
  if (nodeToRemove.path.length === tree.depth) {
    const calculateDepth = (node: ShadowNode): number => {
      let maxDepth = node.path.length
      if (node.children) {
        for (const child of Array.from(node.children.values())) {
          const childDepth = calculateDepth(child)
          if (childDepth > maxDepth) {
            maxDepth = childDepth
          }
        }
      }
      return maxDepth
    }
    tree.depth = calculateDepth(tree.root)
  }

  return removedCount
}
