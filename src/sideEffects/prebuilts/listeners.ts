import {
  addGroup,
  addGroups,
  addNode,
  removeGroup,
  removeNode,
} from '../../core/listenerGraph'
import type {
  ListenerRegistration__internal,
  StoreInstance,
} from '../../core/types'
import type { ListenerFn } from '../../pipeline/processors/listeners.types'
import type { GenericMeta } from '../../types'
import { generateListenerId, guard } from '../../utils/guards'

/**
 * Internal registration logic shared by both single and batch variants.
 * Works with ListenerRegistration__internal to avoid DeepKey resolution overhead.
 */
const prepareRegistration = (
  store: StoreInstance<object>,
  registration: ListenerRegistration__internal,
  graph: { fns: Map<string, ListenerFn> },
): {
  path: string
  id: string
  wrappedFn: ListenerFn
  scope: string | null
} => {
  guard.listenerScope(registration.path, registration.scope)

  const path = registration.path ?? ''
  const id = generateListenerId(path, registration.fn, graph.fns)

  const wrappedFn: ListenerFn = (changes, state) =>
    store._internal.timing.run(
      'listeners',
      () => registration.fn(changes, state),
      {
        path,
        name: 'listener',
      },
    )

  return { path, id, wrappedFn, scope: registration.scope ?? null }
}

/**
 * Batch version of registerListener. Adds all groups first (single sort + edge
 * recomputation), then registers all nodes. Returns a single combined cleanup.
 *
 * PERF: For N listeners sharing M unique groups, this does O(M log M) work once
 * instead of O(M × M log M) from N individual addGroup calls.
 */
export const registerListenersBatch = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  store: StoreInstance<DATA, META>,
  registrations: ListenerRegistration__internal<DATA, META>[],
): (() => void) => {
  const graph = store._internal.graphs.listenerGraph

  // Phase 1: Validate all registrations and collect group paths
  const groupPaths: string[] = []
  const prepared: ReturnType<typeof prepareRegistration>[] = []

  for (const registration of registrations) {
    const entry = prepareRegistration(store, registration, graph)
    groupPaths.push(entry.path)
    prepared.push(entry)
  }

  // Phase 2: Add all groups at once (single sort + edge recomputation)
  addGroups(graph, groupPaths)

  // Phase 3: Register all nodes and fns
  const cleanupEntries: { id: string; path: string }[] = []
  for (const { path, id, wrappedFn, scope } of prepared) {
    graph.fns.set(id, wrappedFn)
    addNode(graph, id, scope, path)
    cleanupEntries.push({ id, path })
  }

  // Return combined cleanup
  return () => {
    for (const { id, path } of cleanupEntries) {
      const isEmpty = removeNode(graph, id, path)
      if (isEmpty) removeGroup(graph, path)
      graph.fns.delete(id)
    }
  }
}

export const registerListener = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  store: StoreInstance<DATA, META>,
  registration: ListenerRegistration__internal<DATA, META>,
): (() => void) => {
  const graph = store._internal.graphs.listenerGraph
  const { path, id, wrappedFn, scope } = prepareRegistration(
    store,
    registration,
    graph,
  )

  // Ensure group exists for this path
  addGroup(graph, path)

  // Store wrapped fn and node metadata
  graph.fns.set(id, wrappedFn)
  addNode(graph, id, scope, path)

  // Return cleanup function
  return () => {
    const isEmpty = removeNode(graph, id, path)
    if (isEmpty) removeGroup(graph, path)
    graph.fns.delete(id)
  }
}
