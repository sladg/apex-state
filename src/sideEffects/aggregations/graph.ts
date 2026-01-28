/**
 * Efficient graph for tracking aggregation dependencies using graphology.
 * Optimized for frequent reads and provides cycle detection.
 *
 * Aggregation direction: sourcePaths → targetPath (one-to-many)
 * Uses graphology for directed graph with cycle detection.
 */

import Graph from 'graphology'
import type { DeepKey } from '../../types'
import type { Aggregation } from './types'

/**
 * Internal state for aggregation graph management
 */
interface AggregationGraphState<DATA extends object> {
  graph: Graph
  aggregations: Map<string, Aggregation<DATA>>
  targetToAggregations: Map<string, Set<string>>
  sourceToTargets: Map<string, Set<string>>
}

/**
 * Efficient graph for tracking aggregation dependencies.
 * Factory function pattern for optimal memory management.
 */
export interface AggregationGraph<DATA extends object> {
  /**
   * Add aggregation relationship: sourcePaths → targetPath.
   * @throws Error if adding this aggregation would create a cycle
   */
  addAggregation(
    id: string,
    targetPath: DeepKey<DATA>,
    sourcePaths: DeepKey<DATA>[]
  ): void

  /**
   * Remove aggregation by ID
   */
  removeAggregation(id: string): void

  /**
   * Get all aggregations where the given path is the target.
   * Returns all source sets for this target.
   *
   * PERFORMANCE CRITICAL: This is called on every state change.
   */
  getAggregationsForTarget(targetPath: DeepKey<DATA>): Aggregation<DATA>[]

  /**
   * Get all target paths affected when the given source path changes.
   *
   * PERFORMANCE CRITICAL: This is called on every state change.
   */
  getTargetsForSource(sourcePath: DeepKey<DATA>): Set<string>
}

/**
 * Create a new AggregationGraph instance
 */
export function createAggregationGraph<
  DATA extends object
>(): AggregationGraph<DATA> {
  const state: AggregationGraphState<DATA> = {
    // Directed graph: edge from source to target represents aggregation flow
    graph: new Graph({ multi: false, type: 'directed' }),
    aggregations: new Map(),
    targetToAggregations: new Map(),
    sourceToTargets: new Map(),
  }

  /**
   * Check if adding this aggregation would create a cycle.
   * A cycle exists if any source can reach the target through existing edges.
   */
  function wouldCreateCycle(
    newTarget: string,
    newSources: string[]
  ): boolean {
    // Check for self-referential aggregation first
    for (const source of newSources) {
      if (source === newTarget) {
        return true
      }
    }

    // For each new source, check if we can reach the source starting from target
    // If target can reach source, then adding source→target edge creates a cycle
    for (const source of newSources) {
      if (canReach(newTarget, source)) {
        return true
      }
    }

    return false
  }

  /**
   * BFS to check if we can reach 'to' starting from 'from' via aggregation edges.
   * Edge direction: source → target
   */
  function canReach(from: string, to: string): boolean {
    if (!state.graph.hasNode(from)) return false

    const visited = new Set<string>()
    const queue = [from]

    while (queue.length > 0) {
      const current = queue.shift()!
      if (current === to) return true
      if (visited.has(current)) continue

      visited.add(current)

      // Follow outgoing edges (current is a source)
      if (state.graph.hasNode(current)) {
        state.graph.forEachOutNeighbor(current, (neighbor) => {
          if (!visited.has(neighbor)) {
            queue.push(neighbor)
          }
        })
      }
    }

    return false
  }

  return {
    addAggregation(
      id: string,
      targetPath: DeepKey<DATA>,
      sourcePaths: DeepKey<DATA>[]
    ): void {
      const target = targetPath as string
      const sources = sourcePaths.map((p) => p as string)

      // Validate no empty sources
      if (sources.length === 0) {
        throw new Error(
          `Cannot create aggregation: sourcePaths cannot be empty for target "${target}"`
        )
      }

      // Cycle detection: ensure no source depends on target
      if (wouldCreateCycle(target, sources)) {
        throw new Error(
          `Cannot create aggregation: would create a cycle involving "${target}"`
        )
      }

      // Store aggregation
      const aggregation: Aggregation<DATA> = { id, targetPath, sourcePaths }
      state.aggregations.set(id, aggregation)

      // Index by target
      if (!state.targetToAggregations.has(target)) {
        state.targetToAggregations.set(target, new Set())
      }
      state.targetToAggregations.get(target)!.add(id)

      // Add nodes and edges to graph
      if (!state.graph.hasNode(target)) {
        state.graph.addNode(target)
      }

      for (const source of sources) {
        if (!state.graph.hasNode(source)) {
          state.graph.addNode(source)
        }

        // Add directed edge: source → target
        if (!state.graph.hasDirectedEdge(source, target)) {
          state.graph.addDirectedEdge(source, target)
        }

        // Index by source
        if (!state.sourceToTargets.has(source)) {
          state.sourceToTargets.set(source, new Set())
        }
        state.sourceToTargets.get(source)!.add(target)
      }
    },

    removeAggregation(id: string): void {
      const aggregation = state.aggregations.get(id)
      if (!aggregation) return

      const target = aggregation.targetPath as string
      const sources = aggregation.sourcePaths.map((p) => p as string)

      // Remove from target index
      state.targetToAggregations.get(target)?.delete(id)
      if (state.targetToAggregations.get(target)?.size === 0) {
        state.targetToAggregations.delete(target)
      }

      // Remove from source indices and graph edges
      for (const source of sources) {
        // Remove edge if no other aggregations use it
        const otherAggregationsUsingEdge = Array.from(
          state.targetToAggregations.get(target) || []
        ).some((otherId) => {
          const otherAgg = state.aggregations.get(otherId)
          return otherAgg?.sourcePaths.some((p) => (p as string) === source)
        })

        if (!otherAggregationsUsingEdge) {
          if (state.graph.hasDirectedEdge(source, target)) {
            state.graph.dropDirectedEdge(source, target)
          }

          // Remove from source→targets index if no longer used
          state.sourceToTargets.get(source)?.delete(target)
          if (state.sourceToTargets.get(source)?.size === 0) {
            state.sourceToTargets.delete(source)
          }
        }
      }

      state.aggregations.delete(id)
    },

    getAggregationsForTarget(targetPath: DeepKey<DATA>): Aggregation<DATA>[] {
      const target = targetPath as string
      const ids = state.targetToAggregations.get(target) || new Set()
      return Array.from(ids).map((id) => state.aggregations.get(id)!)
    },

    getTargetsForSource(sourcePath: DeepKey<DATA>): Set<string> {
      return state.sourceToTargets.get(sourcePath as string) || new Set()
    },
  }
}
