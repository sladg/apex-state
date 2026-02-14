/**
 * Shadow State Type Definitions
 *
 * Defines types for the shadow state tree structure that mirrors valtio state.
 * The shadow state maintains hierarchical relationships and enables efficient
 * subtree operations through a nested object tree.
 *
 * @module shadow-state/types
 */

/**
 * Represents a single node in the shadow state tree.
 *
 * Each node stores its value, path segments, and references to parent/children.
 * Children are stored in a Map to support both string keys (objects) and
 * numeric keys (arrays).
 *
 * @template T - The type of value stored in this node
 *
 * @example
 * ```typescript
 * // Object node
 * const userNode: ShadowNode<{ name: string; age: number }> = {
 *   value: { name: 'Alice', age: 30 },
 *   path: ['user'],
 *   children: new Map([
 *     ['name', { value: 'Alice', path: ['user', 'name'] }],
 *     ['age', { value: 30, path: ['user', 'age'] }]
 *   ]),
 *   parent: rootNode
 * }
 *
 * // Array node
 * const todosNode: ShadowNode<string[]> = {
 *   value: ['Buy milk', 'Walk dog'],
 *   path: ['todos'],
 *   children: new Map([
 *     [0, { value: 'Buy milk', path: ['todos', '0'] }],
 *     [1, { value: 'Walk dog', path: ['todos', '1'] }]
 *   ]),
 *   parent: rootNode
 * }
 * ```
 */
export interface ShadowNode<T = unknown> {
  /**
   * The value stored at this node.
   * For leaf nodes, this is the primitive value.
   * For branch nodes (objects/arrays), this is the object/array reference.
   */
  value: T

  /**
   * Array of path segments from root to this node.
   * Used for reconstructing the full path string.
   *
   * @example ['user', 'profile', 'name']
   */
  path: string[]

  /**
   * Map of child nodes, keyed by property name (string) or array index (number).
   * Undefined for leaf nodes (primitives).
   *
   * Using Map instead of plain object to support numeric keys for arrays
   * and to maintain insertion order.
   */
  children?: Map<string | number, ShadowNode>

  /**
   * Reference to parent node.
   * Undefined only for the root node.
   *
   * Enables bidirectional traversal for efficient tree operations.
   */
  parent?: ShadowNode
}

/**
 * Represents the complete shadow state tree structure.
 *
 * Contains the root node and metadata about the tree structure.
 * Metadata is updated during tree mutations to track tree characteristics.
 *
 * @example
 * ```typescript
 * const shadowTree: ShadowTree = {
 *   root: {
 *     value: { user: { name: 'Alice' }, todos: ['Buy milk'] },
 *     path: [],
 *     children: new Map([...])
 *   },
 *   nodeCount: 5,  // root + user + user.name + todos + todos[0]
 *   depth: 2       // maximum nesting level
 * }
 * ```
 */
export interface ShadowTree {
  /**
   * The root node of the tree.
   * Contains the entire state object and references to top-level children.
   */
  root: ShadowNode

  /**
   * Total number of nodes in the tree, including the root.
   * Updated when nodes are added or removed.
   *
   * Useful for performance monitoring and debugging.
   */
  nodeCount: number

  /**
   * Maximum depth of the tree (number of nesting levels).
   * Root is depth 0, immediate children are depth 1, etc.
   *
   * Useful for detecting deeply nested structures that might
   * impact performance or cause stack overflow issues.
   */
  depth: number
}

/**
 * Options for creating a shadow tree from a valtio state object.
 *
 * @example
 * ```typescript
 * const options: ShadowTreeOptions = {
 *   maxDepth: 10,  // Prevent stack overflow on very deep objects
 *   includeArrays: true,
 *   detectCircular: true
 * }
 * ```
 */
export interface ShadowTreeOptions {
  /**
   * Maximum depth to traverse when building the tree.
   * Prevents stack overflow on pathologically deep structures.
   *
   * @default Infinity
   */
  maxDepth?: number

  /**
   * Whether to create child nodes for array elements.
   * If false, arrays are treated as leaf nodes.
   *
   * @default true
   */
  includeArrays?: boolean

  /**
   * Whether to detect and handle circular references.
   * If true, uses a WeakSet to track visited objects.
   *
   * @default true
   */
  detectCircular?: boolean
}

/**
 * Result of a tree update operation, including affected paths.
 *
 * @example
 * ```typescript
 * const result: UpdateResult = {
 *   affectedPaths: [
 *     'user.profile.name',
 *     'user.profile.updatedAt'
 *   ],
 *   nodeCount: 150,
 *   depth: 5
 * }
 * ```
 */
export interface UpdateResult {
  /**
   * Array of path strings that were affected by the update.
   * Includes the updated path and all ancestor paths.
   */
  affectedPaths: string[]

  /**
   * Updated total node count after the operation.
   */
  nodeCount: number

  /**
   * Updated maximum depth after the operation.
   */
  depth: number
}
