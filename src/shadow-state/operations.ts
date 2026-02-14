/**
 * Shadow State Tree Operations
 *
 * Provides operations for traversing and querying shadow state trees,
 * including circular reference detection and path-based node retrieval.
 *
 * @module shadow-state/operations
 */

import { parsePath } from './pathParser'
import type { ShadowNode, ShadowTree } from './types'

/**
 * Gets a node from the shadow tree by path string.
 *
 * Parses the path string and traverses the tree to find the target node.
 * Returns undefined if the path doesn't exist in the tree.
 *
 * @param tree - The shadow tree to search
 * @param path - Dot or bracket notation path string (e.g., "user.profile.name" or "todos[0]")
 * @returns The node at the path, or undefined if not found
 *
 * @example
 * ```typescript
 * const tree = createShadowTree({ user: { name: 'Alice' } })
 * const node = getNode(tree, 'user.name')
 * // node.value === 'Alice'
 * ```
 */
export const getNode = (
  tree: ShadowTree,
  path: string,
): ShadowNode | undefined => {
  const segments = parsePath(path)
  return getNodeBySegments(tree, segments)
}

/**
 * Gets a node from the shadow tree by path segments array.
 *
 * Traverses the tree following the path segments to find the target node.
 * Returns undefined if the path doesn't exist in the tree.
 *
 * @param tree - The shadow tree to search
 * @param segments - Array of path segments
 * @returns The node at the path, or undefined if not found
 *
 * @example
 * ```typescript
 * const tree = createShadowTree({ user: { name: 'Alice' } })
 * const node = getNodeBySegments(tree, ['user', 'name'])
 * // node.value === 'Alice'
 * ```
 */
export const getNodeBySegments = (
  tree: ShadowTree,
  segments: string[],
): ShadowNode | undefined => {
  if (segments.length === 0) {
    return tree.root
  }

  let current: ShadowNode | undefined = tree.root

  for (const segment of segments) {
    if (!current || !current.children) {
      return undefined
    }

    // Try both string and numeric keys for array indices
    const children: Map<string | number, ShadowNode> = current.children
    current = children.get(segment)
    if (!current) {
      // Try numeric key if segment is a number string
      const numericKey = Number(segment)
      if (!isNaN(numericKey)) {
        current = children.get(numericKey)
      }
    }

    if (!current) {
      return undefined
    }
  }

  return current
}

/**
 * Traverses the tree from a node, executing a callback for each visited node.
 *
 * Performs depth-first traversal with circular reference detection.
 * The visited Set prevents infinite loops when circular references exist.
 *
 * @param node - The starting node for traversal
 * @param callback - Function called for each visited node (receives node and depth)
 * @param options - Optional configuration for traversal
 * @returns Total number of nodes visited
 *
 * @example
 * ```typescript
 * const tree = createShadowTree({ user: { name: 'Alice', age: 30 } })
 * const visited: string[] = []
 * traversePath(tree.root, (node) => {
 *   visited.push(node.path.join('.'))
 * })
 * // visited contains all node paths in the tree
 * ```
 */
export const traversePath = (
  node: ShadowNode,
  callback: (node: ShadowNode, depth: number) => void,
  options?: {
    /**
     * Maximum depth to traverse (prevents stack overflow)
     * @default Infinity
     */
    maxDepth?: number
    /**
     * Set of visited nodes (for circular reference detection)
     * If not provided, a new Set will be created
     */
    visited?: WeakSet<object>
  },
): number => {
  const maxDepth = options?.maxDepth ?? Infinity
  const visited = options?.visited ?? new WeakSet<object>()
  let visitedCount = 0

  const traverse = (currentNode: ShadowNode, depth: number): void => {
    // Check depth limit
    if (depth > maxDepth) {
      return
    }

    // Check for circular reference
    // We use the node's value as the key for circular detection
    // since the same object might appear multiple times in the tree
    if (currentNode.value && typeof currentNode.value === 'object') {
      if (visited.has(currentNode.value as object)) {
        // Circular reference detected - stop traversal here
        return
      }
      visited.add(currentNode.value as object)
    }

    // Execute callback for current node
    callback(currentNode, depth)
    visitedCount++

    // Recursively traverse children
    if (currentNode.children) {
      for (const child of currentNode.children.values()) {
        traverse(child, depth + 1)
      }
    }
  }

  traverse(node, 0)
  return visitedCount
}

/**
 * Checks if a path exists in the shadow tree.
 *
 * @param tree - The shadow tree to check
 * @param path - Dot or bracket notation path string
 * @returns True if the path exists, false otherwise
 *
 * @example
 * ```typescript
 * const tree = createShadowTree({ user: { name: 'Alice' } })
 * hasPath(tree, 'user.name')      // true
 * hasPath(tree, 'user.email')     // false
 * ```
 */
export const hasPath = (tree: ShadowTree, path: string): boolean => {
  return getNode(tree, path) !== undefined
}

/**
 * Gets the value at a path in the shadow tree.
 *
 * Convenience function that returns the value property of the node at the path.
 * Returns undefined if the path doesn't exist.
 *
 * @param tree - The shadow tree to query
 * @param path - Dot or bracket notation path string
 * @returns The value at the path, or undefined if not found
 *
 * @example
 * ```typescript
 * const tree = createShadowTree({ user: { name: 'Alice', age: 30 } })
 * getValue(tree, 'user.name')  // 'Alice'
 * getValue(tree, 'user.age')   // 30
 * getValue(tree, 'user.email') // undefined
 * ```
 */
export const getValue = <T = unknown>(
  tree: ShadowTree,
  path: string,
): T | undefined => {
  const node = getNode(tree, path)
  return node?.value as T | undefined
}

/**
 * Collects all paths in the tree as an array of path strings.
 *
 * Performs depth-first traversal and collects the path of each node.
 * Useful for debugging and testing.
 *
 * @param tree - The shadow tree to traverse
 * @param options - Optional traversal options (maxDepth, etc.)
 * @returns Array of path strings (in depth-first order)
 *
 * @example
 * ```typescript
 * const tree = createShadowTree({ user: { name: 'Alice' }, todos: ['Buy milk'] })
 * const paths = collectPaths(tree)
 * // paths includes: '', 'user', 'user.name', 'todos', 'todos[0]'
 * ```
 */
export const collectPaths = (
  tree: ShadowTree,
  options?: { maxDepth?: number },
): string[] => {
  const paths: string[] = []

  traversePath(
    tree.root,
    (node) => {
      // Join path segments with dot notation
      // (joinPath from pathParser handles bracket notation for numeric indices)
      const pathStr = node.path.join('.')
      paths.push(pathStr)
    },
    options,
  )

  return paths
}

/**
 * Gets all child nodes of a node at a given path.
 *
 * Returns an array of child nodes in insertion order (Map preserves insertion order).
 * Returns empty array if the path doesn't exist or has no children.
 *
 * @param tree - The shadow tree to query
 * @param path - Dot or bracket notation path string
 * @returns Array of child nodes
 *
 * @example
 * ```typescript
 * const tree = createShadowTree({ user: { name: 'Alice', age: 30 } })
 * const children = getChildren(tree, 'user')
 * // children.length === 2 (name and age nodes)
 * ```
 */
export const getChildren = (tree: ShadowTree, path: string): ShadowNode[] => {
  const node = getNode(tree, path)
  if (!node || !node.children) {
    return []
  }
  return Array.from(node.children.values())
}

/**
 * Gets the parent node of a node at a given path.
 *
 * Returns undefined if the path doesn't exist or is the root.
 *
 * @param tree - The shadow tree to query
 * @param path - Dot or bracket notation path string
 * @returns The parent node, or undefined if none exists
 *
 * @example
 * ```typescript
 * const tree = createShadowTree({ user: { name: 'Alice' } })
 * const parent = getParent(tree, 'user.name')
 * // parent.path === ['user']
 * ```
 */
export const getParent = (
  tree: ShadowTree,
  path: string,
): ShadowNode | undefined => {
  const node = getNode(tree, path)
  return node?.parent
}

/**
 * Counts the total number of descendants of a node (including the node itself).
 *
 * Useful for understanding the size of subtrees and for validation.
 *
 * @param node - The node to count from
 * @param options - Optional traversal options
 * @returns Total count of nodes (including the starting node)
 *
 * @example
 * ```typescript
 * const tree = createShadowTree({ user: { name: 'Alice', age: 30 } })
 * const userNode = getNode(tree, 'user')
 * const count = countDescendants(userNode)
 * // count === 3 (user + name + age)
 * ```
 */
export const countDescendants = (
  node: ShadowNode,
  options?: { maxDepth?: number },
): number => {
  return traversePath(
    node,
    () => {
      // Just count, no action needed
    },
    options,
  )
}

/**
 * Gets the depth of a node in the tree.
 *
 * Depth is the number of edges from root to the node.
 * Root has depth 0, its children have depth 1, etc.
 *
 * @param node - The node to measure
 * @returns The depth of the node
 *
 * @example
 * ```typescript
 * const tree = createShadowTree({ user: { profile: { name: 'Alice' } } })
 * const nameNode = getNode(tree, 'user.profile.name')
 * const depth = getDepth(nameNode)
 * // depth === 3
 * ```
 */
export const getDepth = (node: ShadowNode): number => {
  return node.path.length
}

/**
 * Checks if a node is a leaf node (has no children).
 *
 * @param node - The node to check
 * @returns True if the node is a leaf, false otherwise
 *
 * @example
 * ```typescript
 * const tree = createShadowTree({ user: { name: 'Alice' } })
 * const nameNode = getNode(tree, 'user.name')
 * const userNode = getNode(tree, 'user')
 * isLeaf(nameNode)  // true
 * isLeaf(userNode)  // false
 * ```
 */
export const isLeaf = (node: ShadowNode): boolean => {
  return !node.children || node.children.size === 0
}

/**
 * Checks if a node is the root node.
 *
 * @param node - The node to check
 * @returns True if the node is the root, false otherwise
 *
 * @example
 * ```typescript
 * const tree = createShadowTree({ user: { name: 'Alice' } })
 * isRoot(tree.root)  // true
 * const userNode = getNode(tree, 'user')
 * isRoot(userNode)   // false
 * ```
 */
export const isRoot = (node: ShadowNode): boolean => {
  return node.path.length === 0
}
