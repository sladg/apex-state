/**
 * Graph Data Structure
 *
 * A custom data structure that provides O(1) connected component lookups
 * instead of O(V+E) recomputation on every change like graphology's connectedComponents.
 *
 * Operations complexity:
 * - getAllComponents(): O(C) where C is number of components (typically small)
 * - getComponent(node): O(1) lookup
 * - addEdge(n1, n2): O(n) where n is smaller component size (merge)
 * - removeEdge(n1, n2): O(n) for affected component
 * - hasNode(node): O(1)
 * - hasEdge(n1, n2): O(1)
 */

/**
 * Creates a canonical edge key for consistent lookups.
 * Sorts nodes alphabetically to ensure addEdge(a,b) and addEdge(b,a) produce same key.
 */
const makeEdgeKey = (node1: string, node2: string): string =>
  node1 < node2 ? `${node1}--${node2}` : `${node2}--${node1}`

/**
 * Graph maintains connected components with O(1) lookups.
 *
 * Internal structure:
 * - nodeToComponent: Maps each node to its component ID
 * - componentToNodes: Maps each component ID to the set of nodes in that component
 * - edges: Set of edge keys for edge existence checks and proper removal
 * - adjacency: Maps each node to its direct neighbors (for split detection on removal)
 * - nextComponentId: Counter for generating unique component IDs
 */
export interface Graph {
  nodeToComponent: Map<string, number>
  componentToNodes: Map<number, Set<string>>
  edges: Set<string>
  adjacency: Map<string, Set<string>>
  nextComponentId: number
}

/**
 * Creates a new empty Graph instance.
 */
export const createGraph = (): Graph => ({
  nodeToComponent: new Map(),
  componentToNodes: new Map(),
  edges: new Set(),
  adjacency: new Map(),
  nextComponentId: 0,
})

/**
 * BFS traversal from a starting node, collecting all reachable nodes.
 */
const bfsCollect = (
  adjacency: Map<string, Set<string>>,
  startNode: string,
): Set<string> => {
  const visited = new Set<string>([startNode])
  const queue = [startNode]

  while (queue.length > 0) {
    const current = queue.shift()!
    const neighbors = adjacency.get(current)
    if (!neighbors) continue

    for (const neighbor of neighbors) {
      if (!visited.has(neighbor)) {
        visited.add(neighbor)
        queue.push(neighbor)
      }
    }
  }

  return visited
}

/**
 * Removes an isolated node (one with no edges) from the data structure.
 */
const removeIsolatedNode = (graph: Graph, node: string): void => {
  const componentId = graph.nodeToComponent.get(node)
  if (componentId !== undefined) {
    graph.componentToNodes.get(componentId)?.delete(node)
    if (graph.componentToNodes.get(componentId)?.size === 0) {
      graph.componentToNodes.delete(componentId)
    }
  }
  graph.nodeToComponent.delete(node)
  graph.adjacency.delete(node)
}

/**
 * Handles component split after edge removal.
 * If node2 is no longer reachable from node1, creates a new component for node2's component.
 */
const handleComponentSplit = (
  graph: Graph,
  node1: string,
  node2: string,
): void => {
  // BFS from node1 to see if we can reach node2
  const reachableFromNode1 = bfsCollect(graph.adjacency, node1)

  // If node2 is reachable, no split occurred
  if (reachableFromNode1.has(node2)) return

  // Component split: create new component for node2's component
  const newComponentId = graph.nextComponentId++
  const oldComponentId = graph.nodeToComponent.get(node2)!
  const newComponent = bfsCollect(graph.adjacency, node2)

  // Move nodes to new component
  for (const node of newComponent) {
    graph.nodeToComponent.set(node, newComponentId)
    graph.componentToNodes.get(oldComponentId)?.delete(node)
  }
  graph.componentToNodes.set(newComponentId, newComponent)

  // Clean up empty old component
  if (graph.componentToNodes.get(oldComponentId)?.size === 0) {
    graph.componentToNodes.delete(oldComponentId)
  }
}

/**
 * Adds an edge between two nodes.
 * If nodes are in different components, merges them (smaller into larger).
 * If either node is new, adds it to the appropriate component.
 */
export const addEdge = (graph: Graph, node1: string, node2: string): void => {
  const edgeKey = makeEdgeKey(node1, node2)

  // Skip if edge already exists
  if (graph.edges.has(edgeKey)) return

  // Add edge
  graph.edges.add(edgeKey)

  // Update adjacency
  if (!graph.adjacency.has(node1)) graph.adjacency.set(node1, new Set())
  if (!graph.adjacency.has(node2)) graph.adjacency.set(node2, new Set())
  graph.adjacency.get(node1)!.add(node2)
  graph.adjacency.get(node2)!.add(node1)

  const c1 = graph.nodeToComponent.get(node1)
  const c2 = graph.nodeToComponent.get(node2)

  if (c1 === undefined && c2 === undefined) {
    // Both new - create new component
    const id = graph.nextComponentId++
    graph.nodeToComponent.set(node1, id)
    graph.nodeToComponent.set(node2, id)
    graph.componentToNodes.set(id, new Set([node1, node2]))
  } else if (c1 !== undefined && c2 === undefined) {
    // Add node2 to node1's component
    graph.nodeToComponent.set(node2, c1)
    graph.componentToNodes.get(c1)!.add(node2)
  } else if (c1 === undefined && c2 !== undefined) {
    // Add node1 to node2's component
    graph.nodeToComponent.set(node1, c2)
    graph.componentToNodes.get(c2)!.add(node1)
  } else if (c1 !== c2) {
    // Merge components - move smaller into larger
    const set1 = graph.componentToNodes.get(c1!)!
    const set2 = graph.componentToNodes.get(c2!)!
    const [smallerId, smallerSet, largerId, largerSet] =
      set1.size < set2.size ? [c1!, set1, c2!, set2] : [c2!, set2, c1!, set1]

    for (const node of smallerSet) {
      graph.nodeToComponent.set(node, largerId)
      largerSet.add(node)
    }
    graph.componentToNodes.delete(smallerId)
  } else {
    // c1 === c2 (both defined, same component) - nodes already connected, no-op
  }
}

/**
 * Removes an edge between two nodes.
 * If removal splits the component, creates new components via BFS.
 * If a node becomes isolated (no edges), removes it entirely.
 */
export const removeEdge = (
  graph: Graph,
  node1: string,
  node2: string,
): void => {
  const edgeKey = makeEdgeKey(node1, node2)

  // Skip if edge doesn't exist
  if (!graph.edges.has(edgeKey)) return

  // Remove edge
  graph.edges.delete(edgeKey)

  // Update adjacency
  graph.adjacency.get(node1)?.delete(node2)
  graph.adjacency.get(node2)?.delete(node1)

  // Check for isolated nodes
  const adj1 = graph.adjacency.get(node1)
  const adj2 = graph.adjacency.get(node2)
  const node1Isolated = !adj1 || adj1.size === 0
  const node2Isolated = !adj2 || adj2.size === 0

  if (node1Isolated) removeIsolatedNode(graph, node1)
  if (node2Isolated) removeIsolatedNode(graph, node2)

  // If both nodes still have edges, check if component split
  if (!node1Isolated && !node2Isolated) {
    handleComponentSplit(graph, node1, node2)
  }
}

/**
 * Returns all connected components as arrays of nodes.
 * O(C) where C is number of components.
 */
export const getAllComponents = (graph: Graph): string[][] =>
  [...graph.componentToNodes.values()].map((set) => [...set])

/**
 * Returns all nodes in the same connected component as the given node.
 * O(1) lookup.
 */
export const getComponent = (graph: Graph, node: string): string[] => {
  const componentId = graph.nodeToComponent.get(node)
  if (componentId === undefined) return []
  return [...graph.componentToNodes.get(componentId)!]
}

/**
 * Checks if a node exists in the structure.
 * O(1) lookup.
 */
export const hasNode = (graph: Graph, node: string): boolean =>
  graph.nodeToComponent.has(node)

/**
 * Checks if an edge exists between two nodes.
 * O(1) lookup.
 */
export const hasEdge = (graph: Graph, node1: string, node2: string): boolean =>
  graph.edges.has(makeEdgeKey(node1, node2))

/**
 * Returns the degree (number of edges) for a node.
 * O(1) lookup.
 */
export const getDegree = (graph: Graph, node: string): number =>
  graph.adjacency.get(node)?.size ?? 0
