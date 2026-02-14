/**
 * WASM Bridge for BoolLogic Evaluation
 *
 * Provides utilities for registering BoolLogic trees with WASM,
 * tracking dependencies, and batch-evaluating affected trees.
 */

import type { BoolLogic } from '~/types'

import * as wasm from '../../rust/pkg/apex_state_wasm'
import { is } from './is'

/**
 * BoolLogic operator keys
 */
const BOOL_LOGIC_OPERATORS = [
  'IS_EQUAL',
  'EXISTS',
  'IS_EMPTY',
  'AND',
  'OR',
  'NOT',
  'GT',
  'LT',
  'GTE',
  'LTE',
  'IN',
] as const

/**
 * Check if a value is a BoolLogic tree
 */
export const isBoolLogic = (value: unknown): value is BoolLogic<any> => {
  if (!is.object(value)) return false
  const keys = Object.keys(value)
  return (
    keys.length > 0 &&
    keys.some((key) => BOOL_LOGIC_OPERATORS.includes(key as any))
  )
}

/**
 * Check if a concern config has a BoolLogic condition
 */
export const hasBoolLogicCondition = (
  config: Record<string, unknown>,
): boolean => {
  return 'condition' in config && isBoolLogic(config['condition'])
}

/**
 * Global counter for logic IDs
 * Each BoolLogic registration gets a unique ID
 */
let nextLogicId = 1

/**
 * Map: logic_id -> { path: string, concernName: string, tree: BoolLogic }
 * Used for cleanup and evaluation
 */
const logicRegistry = new Map<
  number,
  { path: string; concernName: string; tree: BoolLogic<any> }
>()

/**
 * Map: path -> path_id (cached interned IDs)
 */
const pathIdCache = new Map<string, number>()

/**
 * Get or create path ID for a given path string
 */
const getPathId = (path: string): number => {
  const cached = pathIdCache.get(path)
  if (cached !== undefined) return cached

  const id = wasm.intern(path)
  pathIdCache.set(path, id)
  return id
}

/**
 * Register a BoolLogic tree with WASM
 * Returns a logic ID that can be used for cleanup
 */
export const registerBoolLogic = (
  path: string,
  concernName: string,
  tree: BoolLogic<any>,
): number => {
  const logicId = nextLogicId++
  const treeJson = JSON.stringify(tree)

  // Register with WASM for reverse dependency tracking
  wasm.register_bool_logic(logicId, treeJson)

  // Store in registry for later evaluation/cleanup
  logicRegistry.set(logicId, { path, concernName, tree })

  return logicId
}

/**
 * Unregister a BoolLogic tree from WASM
 */
export const unregisterBoolLogic = (logicId: number): void => {
  wasm.unregister_bool_logic(logicId)
  logicRegistry.delete(logicId)
}

/**
 * Extract all path strings from a BoolLogic tree
 */
const extractPaths = (
  tree: any,
  paths: Set<string> = new Set(),
): Set<string> => {
  if (!is.object(tree)) return paths

  for (const [key, value] of Object.entries(tree)) {
    if (key === 'IS_EQUAL' && Array.isArray(value) && value.length >= 1) {
      paths.add(value[0] as string)
    } else if (
      (key === 'EXISTS' || key === 'IS_EMPTY') &&
      typeof value === 'string'
    ) {
      paths.add(value)
    } else if (
      (key === 'GT' ||
        key === 'LT' ||
        key === 'GTE' ||
        key === 'LTE' ||
        key === 'IN') &&
      Array.isArray(value)
    ) {
      paths.add(value[0] as string)
    } else if (key === 'AND' || key === 'OR') {
      ;(value as any[]).forEach((subtree) => extractPaths(subtree, paths))
    } else if (key === 'NOT') {
      extractPaths(value, paths)
    } else {
      // Unknown operator - ignore
    }
  }

  return paths
}

/**
 * Get value from state at a dot-notation path
 */
const getValueAtPath = (state: any, path: string): any => {
  const parts = path.split('.')
  let current = state
  for (const part of parts) {
    if (current == null) return undefined
    current = current[part]
  }
  return current
}

/**
 * Transform a BoolLogic tree by replacing path strings with path IDs
 * Uses WASM format: { IS_EQUAL: { path_id, expected }, ... }
 */
const transformBoolLogicTree = (tree: any): any => {
  if (!is.object(tree)) return tree

  const result: any = {}

  for (const [key, value] of Object.entries(tree)) {
    if (key === 'IS_EQUAL' && Array.isArray(value) && value.length >= 2) {
      // IS_EQUAL: [path, expected] → { path_id, expected }
      result[key] = {
        path_id: getPathId(value[0] as string),
        expected: value[1],
      }
    } else if (key === 'EXISTS' && typeof value === 'string') {
      // EXISTS: path → { path_id }
      result[key] = { path_id: getPathId(value) }
    } else if (key === 'IS_EMPTY' && typeof value === 'string') {
      // IS_EMPTY: path → { path_id }
      result[key] = { path_id: getPathId(value) }
    } else if (
      (key === 'GT' || key === 'LT' || key === 'GTE' || key === 'LTE') &&
      Array.isArray(value)
    ) {
      // GT/LT/GTE/LTE: [path, threshold] → { path_id, threshold }
      result[key] = {
        path_id: getPathId(value[0] as string),
        threshold: value[1],
      }
    } else if (key === 'IN' && Array.isArray(value) && value.length >= 2) {
      // IN: [path, allowed_values] → { path_id, allowed_values }
      result[key] = {
        path_id: getPathId(value[0] as string),
        allowed_values: value[1],
      }
    } else if (key === 'AND' || key === 'OR') {
      // AND/OR: array of trees
      result[key] = (value as any[]).map(transformBoolLogicTree)
    } else if (key === 'NOT') {
      // NOT: single tree
      result[key] = transformBoolLogicTree(value)
    } else {
      result[key] = value
    }
  }

  return result
}

/**
 * Evaluate a single BoolLogic tree against current state
 */
export const evaluateBoolLogicWasm = (
  tree: BoolLogic<any>,
  state: any,
): boolean => {
  // Transform tree to use path IDs
  const transformedTree = transformBoolLogicTree(tree)

  // Extract all paths and create shadow state
  const paths = extractPaths(tree)
  const shadowState: Record<number, any> = {}

  for (const path of paths) {
    const pathId = getPathId(path)
    const value = getValueAtPath(state, path)
    shadowState[pathId] = value
  }

  const treeJson = JSON.stringify(transformedTree)
  const stateJson = JSON.stringify(shadowState)

  return wasm.evaluate_boollogic(treeJson, stateJson)
}

/**
 * Get all logic IDs affected by a path change
 */
export const getAffectedLogicIds = (path: string): number[] => {
  const pathId = getPathId(path)
  const affectedIds = wasm.affected_by_change(pathId)
  return Array.from(affectedIds)
}

/**
 * Get all registered logic entries
 * Used by the change processor to evaluate affected trees
 */
export const getLogicRegistry = () => logicRegistry

/**
 * Clear all registered BoolLogic trees
 * Used for cleanup and testing
 */
export const clearAllBoolLogic = (): void => {
  logicRegistry.forEach((_, logicId) => {
    wasm.unregister_bool_logic(logicId)
  })
  logicRegistry.clear()
  pathIdCache.clear()
  wasm.clear_rev_deps()
}
