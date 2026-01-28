/**
 * Side effects registration and graph building
 *
 * Handles the internal logic for registering side effects
 * (syncPaths, flipPaths, aggregations, listeners) with the store's graph system.
 *
 * @internal Used internally by useSideEffects hook
 */

import { connectedComponents } from 'graphology-components'

import { processChanges } from '../store/executor'
import type {
  AggregationRule,
  OnStateListener,
  StoreInstance,
} from '../store/types'
import { deepGetUnsafe } from '../store/utils/deepAccess'
import type { ArrayOfChanges, GenericMeta } from '../types'
import type { SideEffects } from '../types/sideEffects'

// ============================================================================
// Main Registration Function
// ============================================================================

/**
 * Register all side effects from a configuration and return cleanup function
 *
 * @internal
 * @param store - Store instance containing state and graphs
 * @param id - Unique identifier for this registration
 * @param effects - Side effects configuration
 * @returns Cleanup function that removes all registered effects
 */
export const registerSideEffects = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  store: StoreInstance<DATA, META>,
  id: string,
  effects: SideEffects<DATA>,
): (() => void) => {
  const cleanups: (() => void)[] = []

  // Register sync paths: [path1, path2]
  if (effects.syncPaths) {
    for (const [path1, path2] of effects.syncPaths) {
      const cleanup = registerSyncPair(store, path1 as string, path2 as string)
      cleanups.push(cleanup)
    }
  }

  // Register flip paths: [path1, path2]
  if (effects.flipPaths) {
    for (const [path1, path2] of effects.flipPaths) {
      const cleanup = registerFlipPair(store, path1 as string, path2 as string)
      cleanups.push(cleanup)
    }
  }

  // Register aggregations: [target, source] - target always first
  // Validate: target cannot also be a source (circular dependency)
  if (effects.aggregations) {
    // Collect all targets and sources for validation
    const targets = new Set<string>()
    const sources = new Set<string>()

    for (const [target, source] of effects.aggregations) {
      targets.add(target as string)
      sources.add(source as string)
    }

    // Validate no circular dependencies
    for (const target of targets) {
      if (sources.has(target)) {
        throw new Error(
          `[apex-state] Circular aggregation: "${target}" cannot be both target and source`,
        )
      }
    }

    // Group by target for multi-source aggregations
    const byTarget = new Map<string, string[]>()
    for (const [target, source] of effects.aggregations) {
      const targetStr = target as string
      const sourceStr = source as string
      const existing = byTarget.get(targetStr) ?? []
      existing.push(sourceStr)
      byTarget.set(targetStr, existing)
    }

    // Register each aggregation
    for (const [targetPath, sourcePaths] of byTarget) {
      const cleanup = registerAggregation(store, {
        id: `${id}:${targetPath}`,
        targetPath,
        sourcePaths,
        aggregate: (state, paths) => {
          // Default aggregation: collect values (paths are runtime strings)
          return paths.map((p) => deepGetUnsafe(state, p))
        },
      })
      cleanups.push(cleanup)
    }
  }

  // Store cleanup reference
  const combinedCleanup = () => cleanups.forEach((fn) => fn())
  store._internal.registrations.sideEffectCleanups.set(id, combinedCleanup)

  return () => {
    combinedCleanup()
    store._internal.registrations.sideEffectCleanups.delete(id)
  }
}

// ============================================================================
// Graph Builder Functions
// ============================================================================

/**
 * Register a sync pair - paths will be kept synchronized
 * On registration, finds the most common non-null value and syncs all paths to it
 * Uses graphology for efficient connected component detection
 * Returns cleanup function
 */
export const registerSyncPair = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  store: StoreInstance<DATA, META>,
  path1: string,
  path2: string,
): (() => void) => {
  const { sync } = store._internal.graphs

  // Add nodes if they don't exist
  if (!sync.hasNode(path1)) sync.addNode(path1)
  if (!sync.hasNode(path2)) sync.addNode(path2)

  // Add edge if it doesn't exist
  const edgeKey = `${path1}--${path2}`
  if (!sync.hasEdge(path1, path2)) {
    sync.addEdge(path1, path2, { key: edgeKey })
  }

  // Find all paths in this sync group using connected components
  const components = connectedComponents(sync)
  const component = components.find((c) => c.includes(path1)) ?? [path1, path2]

  // Get values and count occurrences (excluding null/undefined)
  const valueCounts = new Map<unknown, number>()
  for (const path of component) {
    const value = deepGetUnsafe(store.state, path)
    if (value !== null && value !== undefined) {
      const count = valueCounts.get(value) ?? 0
      valueCounts.set(value, count + 1)
    }
  }

  // Find most common value
  let mostCommonValue: unknown = undefined
  let maxCount = 0
  for (const [value, count] of valueCounts) {
    if (count > maxCount) {
      maxCount = count
      mostCommonValue = value
    }
  }

  // Sync all paths to most common value (if one exists)
  if (mostCommonValue !== undefined) {
    const changes: ArrayOfChanges<DATA, META> = []
    for (const path of component) {
      const currentValue = deepGetUnsafe(store.state, path)
      if (currentValue !== mostCommonValue) {
        // Type cast needed: path is validated at registration, isSyncPathChange is GenericMeta property
        changes.push([
          path,
          mostCommonValue,
          { isSyncPathChange: true },
        ] as ArrayOfChanges<DATA, META>[number])
      }
    }
    if (changes.length > 0) {
      processChanges(store, changes)
    }
  }

  return () => {
    // Remove edge
    if (sync.hasEdge(path1, path2)) {
      sync.dropEdge(path1, path2)
    }
    // Remove isolated nodes
    if (sync.hasNode(path1) && sync.degree(path1) === 0) sync.dropNode(path1)
    if (sync.hasNode(path2) && sync.degree(path2) === 0) sync.dropNode(path2)
  }
}

/**
 * Register a flip pair - paths will have inverse/opposite values
 * Supports booleans (true/false) and any enum-like values (Call/Put)
 * Note: Does NOT process on registration - flip mapping must be provided separately
 * Uses graphology for graph management
 * Returns cleanup function
 */
export const registerFlipPair = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  store: StoreInstance<DATA, META>,
  path1: string,
  path2: string,
): (() => void) => {
  const { flip } = store._internal.graphs

  // Add nodes if they don't exist
  if (!flip.hasNode(path1)) flip.addNode(path1)
  if (!flip.hasNode(path2)) flip.addNode(path2)

  // Add edge if it doesn't exist
  if (!flip.hasEdge(path1, path2)) {
    flip.addEdge(path1, path2)
  }

  return () => {
    // Remove edge
    if (flip.hasEdge(path1, path2)) {
      flip.dropEdge(path1, path2)
    }
    // Remove isolated nodes
    if (flip.hasNode(path1) && flip.degree(path1) === 0) flip.dropNode(path1)
    if (flip.hasNode(path2) && flip.degree(path2) === 0) flip.dropNode(path2)
  }
}

/**
 * Register an aggregation rule
 * Uses directed graph: source paths → target path
 * Rules stored as node attribute on target
 * Returns cleanup function
 */
export const registerAggregation = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  store: StoreInstance<DATA, META>,
  rule: AggregationRule<DATA>,
): (() => void) => {
  const { aggregations } = store._internal.graphs
  const { targetPath, sourcePaths } = rule

  // Add target node if needed, with rules array
  if (!aggregations.hasNode(targetPath)) {
    aggregations.addNode(targetPath, { rules: [] })
  }

  // Add rule to target node's rules array
  const targetRules = aggregations.getNodeAttribute(
    targetPath,
    'rules',
  ) as AggregationRule<object>[]
  targetRules.push(rule as AggregationRule<object>)

  // Add edges from each source → target
  for (const sourcePath of sourcePaths) {
    if (!aggregations.hasNode(sourcePath)) {
      aggregations.addNode(sourcePath)
    }
    if (!aggregations.hasEdge(sourcePath, targetPath)) {
      aggregations.addEdge(sourcePath, targetPath)
    }
  }

  return () => {
    // Remove rule from target's rules array
    const rules = aggregations.getNodeAttribute(
      targetPath,
      'rules',
    ) as AggregationRule<object>[]
    const filtered = rules.filter((r) => r.id !== rule.id)
    aggregations.setNodeAttribute(targetPath, 'rules', filtered)

    // Remove edges for this rule's source paths (only if no other rules use them)
    for (const sourcePath of sourcePaths) {
      // Check if any remaining rule uses this source
      const stillUsed = filtered.some((r) => r.sourcePaths.includes(sourcePath))
      if (!stillUsed && aggregations.hasEdge(sourcePath, targetPath)) {
        aggregations.dropEdge(sourcePath, targetPath)
      }
      // Remove isolated source nodes
      if (
        aggregations.hasNode(sourcePath) &&
        aggregations.degree(sourcePath) === 0
      ) {
        aggregations.dropNode(sourcePath)
      }
    }

    // Remove target node if no rules remain
    if (filtered.length === 0 && aggregations.hasNode(targetPath)) {
      aggregations.dropNode(targetPath)
    }
  }
}

/**
 * Register a listener for a path
 * Returns cleanup function
 */
export const registerListener = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  store: StoreInstance<DATA, META>,
  path: string,
  listener: OnStateListener<DATA, META>,
): (() => void) => {
  const { listeners } = store._internal.graphs
  const existing = listeners.get(path) ?? []
  // Cast to any to avoid complex generic variance issues in internal implementation
  listeners.set(path, [...existing, listener])

  return () => {
    const list = listeners.get(path)
    if (list) {
      const filtered = list.filter((l) => l !== listener)
      if (filtered.length > 0) {
        listeners.set(path, filtered)
      } else {
        listeners.delete(path)
      }
    }
  }
}
