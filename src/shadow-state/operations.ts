/**
 * Shadow State Tree Operations
 *
 * Provides operations for traversing and querying shadow state trees,
 * including circular reference detection and path-based node retrieval.
 *
 * @module shadow-state/operations
 */

import { is } from '../utils/is'
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
      for (const child of Array.from(currentNode.children.values())) {
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

/**
 * Helper to build children for an object value
 */
const buildObjectChildren = (
  obj: Record<string, unknown>,
  parentPath: string[],
  parent: ShadowNode,
  buildFn: (value: unknown, path: string[], parent?: ShadowNode) => ShadowNode,
): { children: Map<string | number, ShadowNode>; count: number } => {
  const children = new Map<string | number, ShadowNode>()
  let count = 0

  for (const key in obj) {
    if (Object.prototype.hasOwnProperty.call(obj, key)) {
      const childPath = [...parentPath, key]
      const childValue = obj[key]
      const childNode = buildFn(childValue, childPath, parent)
      children.set(key, childNode)
      count++
    }
  }

  return { children, count }
}

/**
 * Helper to build children for an array value
 */
const buildArrayChildren = (
  arr: unknown[],
  parentPath: string[],
  parent: ShadowNode,
  buildFn: (value: unknown, path: string[], parent?: ShadowNode) => ShadowNode,
): { children: Map<string | number, ShadowNode>; count: number } => {
  const children = new Map<string | number, ShadowNode>()
  let count = 0

  for (let i = 0; i < arr.length; i++) {
    const childPath = [...parentPath, String(i)]
    const childValue = arr[i]
    const childNode = buildFn(childValue, childPath, parent)
    children.set(i, childNode)
    count++
  }

  return { children, count }
}

/**
 * Helper to recalculate tree depth
 */
const recalculateTreeDepth = (tree: ShadowTree): void => {
  const calculateDepth = (n: ShadowNode): number => {
    let maxDepth = n.path.length
    if (n.children) {
      for (const child of Array.from(n.children.values())) {
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

/**
 * Updates a node's value and rebuilds its children if the value is an object/array.
 *
 * This is a partial update operation that modifies only the target node and its descendants
 * without recreating unaffected parts of the tree. Returns the affected paths for tracking changes.
 *
 * @param tree - The shadow tree to modify
 * @param path - Dot or bracket notation path string to the node to update
 * @param value - New value to set at the node
 * @param options - Optional configuration for rebuilding children
 * @returns Update result with affected paths and updated metadata
 *
 * @example
 * ```typescript
 * const tree = createShadowTree({ user: { name: 'Alice', age: 30 } })
 * const result = updateNode(tree, 'user.name', 'Bob')
 * // result.affectedPaths === ['user.name']
 * // Only the name node was updated, age node was not recreated
 * ```
 *
 * @example
 * ```typescript
 * const tree = createShadowTree({ user: { name: 'Alice' } })
 * const result = updateNode(tree, 'user', { name: 'Bob', email: 'bob@example.com' })
 * // result.affectedPaths === ['user', 'user.name', 'user.email']
 * // User node and its children were rebuilt, but other tree parts unchanged
 * ```
 */
export const updateNode = <T = unknown>(
  tree: ShadowTree,
  path: string,
  value: T,
  options?: {
    maxDepth?: number
    includeArrays?: boolean
    detectCircular?: boolean
  },
): import('./types').UpdateResult => {
  const opts = {
    maxDepth: options?.maxDepth ?? Infinity,
    includeArrays: options?.includeArrays ?? true,
    detectCircular: options?.detectCircular ?? true,
  }

  const segments = parsePath(path)
  const node = getNodeBySegments(tree, segments)

  if (!node) {
    throw new Error(`Cannot update non-existent path: ${path}`)
  }

  // Track old subtree size to update tree metadata
  const oldSubtreeSize = countDescendants(node)

  // Collect affected paths: descendants + ancestors
  const descendantPaths: string[] = []
  traversePath(node, (n) => {
    descendantPaths.push(n.path.join('.'))
  })
  const ancestorPaths = getAncestorPaths(path)
  const affectedPaths = mergeAffectedPaths(descendantPaths, ancestorPaths)

  // Update node value
  node.value = value

  // Remove old children (if any)
  const oldChildren = node.children
  if (node.children) {
    delete node.children
  }

  // Build new children if value is object/array
  const visited = opts.detectCircular ? new WeakSet<object>() : null
  let addedNodeCount = 1 // Count the updated node itself
  let maxChildDepth = node.path.length

  const buildNode = (
    currentValue: unknown,
    currentPath: string[],
    parent?: ShadowNode,
  ): ShadowNode => {
    const depth = currentPath.length
    if (depth > maxChildDepth) {
      maxChildDepth = depth
    }

    const newNode: ShadowNode = {
      value: currentValue,
      path: currentPath,
      ...(parent && { parent }),
    }

    // Check depth limit
    if (depth >= opts.maxDepth) {
      return newNode
    }

    // Handle circular references
    if (visited && is.not.primitive(currentValue) && is.not.nil(currentValue)) {
      if (visited.has(currentValue as object)) {
        return newNode
      }
      visited.add(currentValue as object)
    }

    // Build children based on value type
    if (is.object(currentValue)) {
      const obj = currentValue as Record<string, unknown>
      const result = buildObjectChildren(obj, currentPath, newNode, buildNode)
      addedNodeCount += result.count
      if (result.children.size > 0) {
        newNode.children = result.children
      }
      return newNode
    }

    if (is.array(currentValue) && opts.includeArrays) {
      const arr = currentValue as unknown[]
      const result = buildArrayChildren(arr, currentPath, newNode, buildNode)
      addedNodeCount += result.count
      if (result.children.size > 0) {
        newNode.children = result.children
      }
      return newNode
    }

    // Primitive value - no children
    return newNode
  }

  // If the new value is an object or array, rebuild children
  if (is.object(value)) {
    const obj = value as Record<string, unknown>
    const result = buildObjectChildren(obj, node.path, node, buildNode)
    addedNodeCount += result.count
    if (result.children.size > 0) {
      node.children = result.children
    }
  } else if (is.array(value) && opts.includeArrays) {
    const arr = value as unknown[]
    const result = buildArrayChildren(arr, node.path, node, buildNode)
    addedNodeCount += result.count
    if (result.children.size > 0) {
      node.children = result.children
    }
  } else {
    // Primitive value - no children to rebuild
  }

  // Update tree metadata
  tree.nodeCount = tree.nodeCount - oldSubtreeSize + addedNodeCount

  // Recalculate tree depth if the update might have changed it
  if (
    maxChildDepth > tree.depth ||
    (oldChildren && node.path.length === tree.depth)
  ) {
    recalculateTreeDepth(tree)
  }

  return {
    affectedPaths,
    nodeCount: tree.nodeCount,
    depth: tree.depth,
  }
}

/**
 * Sets a value at a path in the shadow tree (convenience wrapper for updateNode).
 *
 * Provides a simpler API for updating values when you don't need the full update result.
 * If the path doesn't exist, throws an error.
 *
 * @param tree - The shadow tree to modify
 * @param path - Dot or bracket notation path string
 * @param value - New value to set
 *
 * @example
 * ```typescript
 * const tree = createShadowTree({ user: { name: 'Alice' } })
 * setValue(tree, 'user.name', 'Bob')
 * getValue(tree, 'user.name')  // 'Bob'
 * ```
 */
export const setValue = <T = unknown>(
  tree: ShadowTree,
  path: string,
  value: T,
): void => {
  updateNode(tree, path, value)
}

/**
 * Calculates all ancestor paths for a given path.
 *
 * Returns an array of all parent paths from the root to the immediate parent.
 * Does not include the path itself.
 *
 * @param path - Path string or path segments array
 * @returns Array of ancestor path strings (from root to immediate parent)
 *
 * @example
 * ```typescript
 * getAncestorPaths('user.profile.name')
 * // Returns: ['', 'user', 'user.profile']
 *
 * getAncestorPaths(['todos', '0', 'title'])
 * // Returns: ['', 'todos', 'todos.0']
 * ```
 */
export const getAncestorPaths = (path: string | string[]): string[] => {
  const segments = typeof path === 'string' ? parsePath(path) : path
  const ancestors: string[] = []

  // Add root path
  ancestors.push('')

  // Add each intermediate path
  for (let i = 1; i < segments.length; i++) {
    const ancestorSegments = segments.slice(0, i)
    ancestors.push(ancestorSegments.join('.'))
  }

  return ancestors
}

/**
 * Calculates all descendant paths for a node.
 *
 * Returns an array of all paths in the subtree rooted at the given node.
 * Includes the node itself and all nested descendants.
 *
 * @param node - The node to collect descendants from
 * @param options - Optional traversal options
 * @returns Array of descendant path strings
 *
 * @example
 * ```typescript
 * const tree = createShadowTree({ user: { name: 'Alice', age: 30 } })
 * const userNode = getNode(tree, 'user')
 * const descendants = getDescendantPaths(userNode)
 * // Returns: ['user', 'user.name', 'user.age']
 * ```
 */
export const getDescendantPaths = (
  node: ShadowNode,
  options?: { maxDepth?: number },
): string[] => {
  const paths: string[] = []

  traversePath(
    node,
    (n) => {
      paths.push(n.path.join('.'))
    },
    options,
  )

  return paths
}

/**
 * Calculates all affected paths for a given path and its changes.
 *
 * This includes:
 * - The path itself
 * - All ancestor paths (parent, grandparent, etc. up to root)
 * - All descendant paths (children, grandchildren, etc.)
 *
 * This is the complete set of paths that should be considered "affected"
 * when a value at the given path changes.
 *
 * @param tree - The shadow tree to query
 * @param path - Path string that changed
 * @param includeDescendants - Whether to include descendant paths (default: true)
 * @returns Array of all affected path strings (deduplicated)
 *
 * @example
 * ```typescript
 * const tree = createShadowTree({ user: { profile: { name: 'Alice', age: 30 } } })
 * const affected = calculateAffectedPaths(tree, 'user.profile.name')
 * // Returns: ['', 'user', 'user.profile', 'user.profile.name']
 * // All ancestors are affected because their child values changed
 * ```
 *
 * @example
 * ```typescript
 * const tree = createShadowTree({ user: { profile: { name: 'Alice' } } })
 * const affected = calculateAffectedPaths(tree, 'user')
 * // Returns: ['', 'user', 'user.profile', 'user.profile.name']
 * // Includes descendants because changing 'user' affects all its children
 * ```
 */
export const calculateAffectedPaths = (
  tree: ShadowTree,
  path: string,
  options?: { includeDescendants?: boolean },
): string[] => {
  const includeDescendants = options?.includeDescendants ?? true
  const affectedPaths = new Set<string>()

  // Add the path itself
  affectedPaths.add(path)

  // Add all ancestor paths
  const ancestors = getAncestorPaths(path)
  for (const ancestor of ancestors) {
    affectedPaths.add(ancestor)
  }

  // Add all descendant paths if requested
  if (includeDescendants) {
    const node = getNode(tree, path)
    if (node) {
      const descendants = getDescendantPaths(node)
      for (const descendant of descendants) {
        affectedPaths.add(descendant)
      }
    }
  }

  return Array.from(affectedPaths).sort()
}

/**
 * Merges multiple arrays of affected paths into a single deduplicated array.
 *
 * Useful when multiple operations occur and you need to track the complete
 * set of affected paths across all operations.
 *
 * @param pathArrays - Variable number of path string arrays to merge
 * @returns Deduplicated array of all unique paths, sorted
 *
 * @example
 * ```typescript
 * const paths1 = ['user.name', 'user']
 * const paths2 = ['user.age', 'user']
 * const merged = mergeAffectedPaths(paths1, paths2)
 * // Returns: ['user', 'user.age', 'user.name']
 * ```
 */
export const mergeAffectedPaths = (...pathArrays: string[][]): string[] => {
  const allPaths = new Set<string>()

  for (const pathArray of pathArrays) {
    for (const path of pathArray) {
      allPaths.add(path)
    }
  }

  return Array.from(allPaths).sort()
}

/**
 * Checks if a path is an ancestor of another path.
 *
 * @param ancestorPath - The potential ancestor path
 * @param descendantPath - The potential descendant path
 * @returns True if ancestorPath is an ancestor of descendantPath
 *
 * @example
 * ```typescript
 * isAncestorOf('user', 'user.profile.name')  // true
 * isAncestorOf('user.profile', 'user.name')  // false
 * isAncestorOf('', 'user')  // true (root is ancestor of all)
 * ```
 */
export const isAncestorOf = (
  ancestorPath: string,
  descendantPath: string,
): boolean => {
  // Root is ancestor of all non-root paths
  if (ancestorPath === '' && descendantPath !== '') {
    return true
  }

  // Path cannot be its own ancestor
  if (ancestorPath === descendantPath) {
    return false
  }

  // Check if descendantPath starts with ancestorPath followed by a dot
  return descendantPath.startsWith(ancestorPath + '.')
}

/**
 * Checks if a path is a descendant of another path.
 *
 * @param descendantPath - The potential descendant path
 * @param ancestorPath - The potential ancestor path
 * @returns True if descendantPath is a descendant of ancestorPath
 *
 * @example
 * ```typescript
 * isDescendantOf('user.profile.name', 'user')  // true
 * isDescendantOf('user.name', 'user.profile')  // false
 * ```
 */
export const isDescendantOf = (
  descendantPath: string,
  ancestorPath: string,
): boolean => {
  return isAncestorOf(ancestorPath, descendantPath)
}

/**
 * Replaces an entire subtree at a given path with a new subtree built from a value.
 *
 * This is similar to updateNode but more explicit about replacing the entire branch.
 * Removes the old node and all its descendants, then creates a new subtree from the
 * provided value. Maintains parent-child links and updates tree metadata.
 *
 * @param tree - The shadow tree to modify
 * @param path - Dot or bracket notation path string to the node to replace
 * @param value - New value to build the subtree from
 * @param options - Optional configuration for building the new subtree
 * @returns Update result with affected paths and updated metadata
 *
 * @example
 * ```typescript
 * const tree = createShadowTree({ user: { name: 'Alice', age: 30 } })
 * const result = replaceSubtree(tree, 'user', { name: 'Bob', email: 'bob@example.com', age: 25 })
 * // result.affectedPaths includes 'user', 'user.name', 'user.age', 'user.email'
 * // The entire user subtree was replaced, old 'age' node removed, new 'email' node added
 * ```
 *
 * @example
 * ```typescript
 * const tree = createShadowTree({ todos: ['Task 1', 'Task 2'] })
 * const result = replaceSubtree(tree, 'todos', ['New Task 1', 'New Task 2', 'New Task 3'])
 * // result.affectedPaths includes 'todos', 'todos[0]', 'todos[1]', 'todos[2]'
 * // Entire array subtree replaced with new array
 * ```
 */
export const replaceSubtree = <T = unknown>(
  tree: ShadowTree,
  path: string,
  value: T,
  options?: {
    maxDepth?: number
    includeArrays?: boolean
    detectCircular?: boolean
  },
): import('./types').UpdateResult => {
  const opts = {
    maxDepth: options?.maxDepth ?? Infinity,
    includeArrays: options?.includeArrays ?? true,
    detectCircular: options?.detectCircular ?? true,
  }

  const segments = parsePath(path)
  const node = getNodeBySegments(tree, segments)

  if (!node) {
    throw new Error(`Cannot replace subtree at non-existent path: ${path}`)
  }

  // Collect all affected paths from the old subtree before replacing
  const oldDescendantPaths: string[] = []
  traversePath(node, (n) => {
    oldDescendantPaths.push(n.path.join('.'))
  })

  // Include ancestor paths since they're also affected
  const ancestorPaths = getAncestorPaths(path)

  // Count old subtree size
  const oldSubtreeSize = countDescendants(node)

  // Build the new subtree
  const visited = opts.detectCircular ? new WeakSet<object>() : null
  let addedNodeCount = 0
  let maxChildDepth = node.path.length

  const buildNode = (
    currentValue: unknown,
    currentPath: string[],
    parent?: ShadowNode,
  ): ShadowNode => {
    addedNodeCount++
    const depth = currentPath.length
    if (depth > maxChildDepth) {
      maxChildDepth = depth
    }

    const newNode: ShadowNode = {
      value: currentValue,
      path: currentPath,
      ...(parent && { parent }),
    }

    // Check depth limit
    if (depth >= opts.maxDepth) {
      return newNode
    }

    // Handle circular references
    if (visited && is.not.primitive(currentValue) && is.not.nil(currentValue)) {
      if (visited.has(currentValue as object)) {
        return newNode
      }
      visited.add(currentValue as object)
    }

    // Build children based on value type
    if (is.object(currentValue)) {
      const obj = currentValue as Record<string, unknown>
      const result = buildObjectChildren(obj, currentPath, newNode, buildNode)
      addedNodeCount += result.count
      if (result.children.size > 0) {
        newNode.children = result.children
      }
      return newNode
    }

    if (is.array(currentValue) && opts.includeArrays) {
      const arr = currentValue as unknown[]
      const result = buildArrayChildren(arr, currentPath, newNode, buildNode)
      addedNodeCount += result.count
      if (result.children.size > 0) {
        newNode.children = result.children
      }
      return newNode
    }

    // Primitive value - no children
    return newNode
  }

  // Build the new subtree (starting from the current node's path and parent)
  const newSubtree = buildNode(value, node.path, node.parent)

  // Replace the node in its parent's children map
  if (node.parent && node.parent.children) {
    const key = node.path[node.path.length - 1]!
    // Try to determine the correct key type
    const numericKey = Number(key)
    const actualKey = isNaN(numericKey) ? key : numericKey
    node.parent.children.set(actualKey, newSubtree)
  } else {
    // Replacing the root node
    tree.root = newSubtree
  }

  // Collect affected paths from the new subtree
  // (in case new subtree has different structure)
  const newDescendantPaths: string[] = []
  traversePath(newSubtree, (n) => {
    newDescendantPaths.push(n.path.join('.'))
  })

  // Merge all affected paths: old descendants + new descendants + ancestors
  const allAffectedPaths = mergeAffectedPaths(
    oldDescendantPaths,
    newDescendantPaths,
    ancestorPaths,
  )

  // Update tree metadata
  tree.nodeCount = tree.nodeCount - oldSubtreeSize + addedNodeCount

  // Recalculate tree depth if necessary
  if (
    maxChildDepth > tree.depth ||
    (node.path.length <= tree.depth &&
      oldSubtreeSize > 0 &&
      addedNodeCount === 0)
  ) {
    recalculateTreeDepth(tree)
  }

  return {
    affectedPaths: allAffectedPaths,
    nodeCount: tree.nodeCount,
    depth: tree.depth,
  }
}
