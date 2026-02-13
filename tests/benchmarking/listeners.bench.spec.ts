/**
 * Listener Processing Strategy Benchmark
 *
 * Compares approaches for processListeners with change accumulation:
 * 1. Flat loop (current): pre-computed FlatListener[], single loop, push spillage
 * 2. Old nested loop: Map + sortedListenerPaths, nested loop, queue tracking
 * 3. Reduce (immutable): reduce with spread accumulator
 * 4. Reduce (mutable acc): reduce with push accumulator
 * 5. Graph-based: ListenerGraph with pre-computed edges and local pendingByGroup
 */

import { bench, describe } from 'vitest'

import type { ListenerRegistration } from '../../src/core/types'
import type {
  GroupMeta,
  ListenerFn,
  ListenerGraph,
  ListenerRoute,
} from '../../src/pipeline/processors/types'
import type { GenericMeta } from '../../src/types'
import { dot } from '../../src/utils/dot'
import { getPathDepth } from '../../src/utils/pathUtils'

// ============================================================================
// Types
// ============================================================================

type BenchState = Record<string, unknown>
type AnyChange = [string, unknown, GenericMeta]

/** Legacy flat listener structure for backward-compat benchmarks */
interface FlatListener {
  path: string
  prefix: string
  prefixLen: number
  isRoot: boolean
  scope: string | null
  fn: ListenerFn
  depth: number
}

// ============================================================================
// Shared helpers
// ============================================================================

const filterAndRelativize = (
  changes: AnyChange[],
  listenerPath: string,
): AnyChange[] => {
  const result: AnyChange[] = []
  if (listenerPath === '') {
    for (const change of changes) {
      if (!change[0].includes('.')) {
        result.push(change)
      }
    }
    return result
  }
  const prefix = listenerPath + '.'
  for (const change of changes) {
    if (change[0].startsWith(prefix)) {
      result.push([change[0].slice(prefix.length), change[1], change[2]])
    }
  }
  return result
}

const getRelevantChanges = (
  changes: AnyChange[],
  prefix: string,
  prefixLen: number,
  isRoot: boolean,
): AnyChange[] => {
  const result: AnyChange[] = []
  if (isRoot) {
    for (const change of changes) {
      if (!change[0].includes('.')) {
        result.push(change)
      }
    }
    return result
  }
  for (const change of changes) {
    if (change[0].startsWith(prefix)) {
      result.push([change[0].slice(prefixLen), change[1], change[2]])
    }
  }
  return result
}

const callListener = (
  registration: ListenerRegistration<BenchState>,
  relevantChanges: AnyChange[],
  currentState: BenchState,
): AnyChange[] => {
  if (relevantChanges.length === 0) return []
  const scope = (registration.scope as string) ?? ''
  const scopedState =
    scope === '' ? currentState : dot.get__unsafe(currentState, scope)
  const result = registration.fn(relevantChanges, scopedState) as
    | AnyChange[]
    | undefined
  if (!result || result.length === 0) return []
  return result.map(([path, value, meta]) => [
    path,
    value,
    { isListenerChange: true, ...meta },
  ])
}

const callFlatListener = (
  fn: ListenerFn,
  scope: string | null,
  relevantChanges: AnyChange[],
  currentState: BenchState,
): AnyChange[] => {
  if (relevantChanges.length === 0) return []
  const scopedState =
    scope === null ? currentState : dot.get__unsafe(currentState, scope)
  const result = fn(relevantChanges, scopedState)
  if (!result || result.length === 0) return []
  return result.map(([path, value, meta]) => [
    path,
    value,
    { isListenerChange: true, ...meta },
  ])
}

// ============================================================================
// Strategy A: Flat loop (current implementation)
// ============================================================================

const processListeners_flat = (
  changes: AnyChange[],
  flatListeners: FlatListener[],
  currentState: BenchState,
): AnyChange[] => {
  const allChanges = [...changes].sort(
    (a, b) => getPathDepth(b[0]) - getPathDepth(a[0]),
  )
  const produced: AnyChange[] = []

  for (const listener of flatListeners) {
    const relevant = getRelevantChanges(
      allChanges,
      listener.prefix,
      listener.prefixLen,
      listener.isRoot,
    )
    if (relevant.length === 0) continue

    const newChanges = callFlatListener(
      listener.fn,
      listener.scope,
      relevant,
      currentState,
    )
    if (newChanges.length > 0) {
      produced.push(...newChanges)
      allChanges.push(...newChanges)
    }
  }

  return produced
}

// ============================================================================
// Strategy B: Old nested loop (Map + sortedListenerPaths)
// ============================================================================

const processListeners_nested = (
  changes: AnyChange[],
  listeners: Map<string, ListenerRegistration<BenchState>[]>,
  sortedListenerPaths: string[],
  currentState: BenchState,
): AnyChange[] => {
  const sortedChanges = [...changes].sort(
    (a, b) => getPathDepth(b[0]) - getPathDepth(a[0]),
  )
  const produced: AnyChange[] = []

  for (const listenerPath of sortedListenerPaths) {
    const pathListeners = listeners.get(listenerPath)!

    let relevantChanges = filterAndRelativize(sortedChanges, listenerPath)

    for (const registration of pathListeners) {
      const newChanges = callListener(
        registration,
        relevantChanges,
        currentState,
      )
      if (newChanges.length > 0) {
        produced.push(...newChanges)
        sortedChanges.push(...newChanges)
        relevantChanges = filterAndRelativize(sortedChanges, listenerPath)
      }
    }
  }

  return produced
}

// ============================================================================
// Strategy C: reduce (immutable)
// ============================================================================

const processListeners_reduce = (
  changes: AnyChange[],
  flatListeners: FlatListener[],
  currentState: BenchState,
): AnyChange[] => {
  const initialSorted = [...changes].sort(
    (a, b) => getPathDepth(b[0]) - getPathDepth(a[0]),
  )

  const result = flatListeners.reduce<{
    allChanges: AnyChange[]
    produced: AnyChange[]
  }>(
    (acc, listener) => {
      const relevantChanges = getRelevantChanges(
        acc.allChanges,
        listener.prefix,
        listener.prefixLen,
        listener.isRoot,
      )
      const newChanges = callFlatListener(
        listener.fn,
        listener.scope,
        relevantChanges,
        currentState,
      )
      if (newChanges.length > 0) {
        return {
          allChanges: [...acc.allChanges, ...newChanges],
          produced: [...acc.produced, ...newChanges],
        }
      }
      return acc
    },
    { allChanges: initialSorted, produced: [] },
  )

  return result.produced
}

// ============================================================================
// Strategy D: reduce (mutable accumulator)
// ============================================================================

const processListeners_reduceMut = (
  changes: AnyChange[],
  flatListeners: FlatListener[],
  currentState: BenchState,
): AnyChange[] => {
  const initialSorted = [...changes].sort(
    (a, b) => getPathDepth(b[0]) - getPathDepth(a[0]),
  )

  const result = flatListeners.reduce<{
    allChanges: AnyChange[]
    produced: AnyChange[]
  }>(
    (acc, listener) => {
      const relevantChanges = getRelevantChanges(
        acc.allChanges,
        listener.prefix,
        listener.prefixLen,
        listener.isRoot,
      )
      const newChanges = callFlatListener(
        listener.fn,
        listener.scope,
        relevantChanges,
        currentState,
      )
      if (newChanges.length > 0) {
        acc.allChanges.push(...newChanges)
        acc.produced.push(...newChanges)
      }
      return acc
    },
    { allChanges: initialSorted, produced: [] },
  )

  return result.produced
}

// ============================================================================
// Strategy E: graph-based (ListenerGraph with pre-computed edges)
// ============================================================================

const getRelevantChangesForMeta = (
  changes: AnyChange[],
  meta: GroupMeta,
): AnyChange[] => {
  const result: AnyChange[] = []
  if (meta.isRoot) {
    for (const change of changes) {
      if (!change[0].includes('.')) {
        result.push(change)
      }
    }
    return result
  }
  for (const change of changes) {
    if (change[0].startsWith(meta.prefix)) {
      result.push([change[0].slice(meta.prefixLen), change[1], change[2]])
    }
  }
  return result
}

const processListeners_graph = (
  changes: AnyChange[],
  graph: ListenerGraph,
  listenerFns: Map<string, ListenerFn>,
  currentState: BenchState,
): AnyChange[] => {
  const allChanges = [...changes]
  const produced: AnyChange[] = []

  // Local accumulator replaces mutable group.pending
  const pendingByGroup = new Map<string, AnyChange[]>()
  for (const path of graph.order) pendingByGroup.set(path, [])

  for (const groupPath of graph.order) {
    const meta = graph.groupMeta.get(groupPath)!
    const memberIds = graph.groupMembers.get(groupPath)!
    const edges = graph.edges.get(groupPath)!
    const pending = pendingByGroup.get(groupPath)!

    const relevant = getRelevantChangesForMeta(allChanges, meta)
    const groupRelevant = [...relevant, ...pending]
    if (groupRelevant.length === 0) continue

    for (const id of memberIds) {
      const fn = listenerFns.get(id)
      if (!fn) continue
      const node = graph.nodes.get(id)!
      const scopedState =
        node.scope === null
          ? currentState
          : dot.get__unsafe(currentState, node.scope)
      const result = fn(groupRelevant, scopedState)
      if (!result || result.length === 0) continue

      const newChanges: AnyChange[] = result.map(([path, value, meta]) => [
        path,
        value,
        { isListenerChange: true, ...meta },
      ])

      produced.push(...newChanges)
      allChanges.push(...newChanges)

      // Route to downstream groups via pre-computed edges
      for (const change of newChanges) {
        for (const edge of edges) {
          if (edge.isRoot && !change[0].includes('.')) {
            pendingByGroup.get(edge.target)!.push(change)
          } else if (change[0].startsWith(edge.prefix)) {
            pendingByGroup
              .get(edge.target)!
              .push([change[0].slice(edge.prefixLen), change[1], change[2]])
          }
        }
      }
    }
  }

  return produced
}

// ============================================================================
// Test fixture factory
// ============================================================================

const createListenerFn = (
  producesChanges: boolean,
  isFirst: boolean,
): ListenerFn =>
  function listenerFn(_changes: AnyChange[]) {
    if (producesChanges && isFirst) {
      return [['_marker', Date.now(), {}] as AnyChange]
    }
    return undefined
  } as ListenerFn

/* eslint-disable sonarjs/cognitive-complexity */
const createFixture = (config: {
  listenerPaths: string[]
  listenersPerPath: number
  producesChanges: boolean
  changeCount: number
}) => {
  const state: BenchState = { root: {} }
  const listeners = new Map<string, ListenerRegistration<BenchState>[]>()
  const flatListeners: FlatListener[] = []
  const listenerFnMap = new Map<string, ListenerFn>()

  // Build the graph
  const graph: ListenerGraph = {
    order: [],
    groupMeta: new Map(),
    groupMembers: new Map(),
    nodes: new Map(),
    edges: new Map(),
  }

  for (const path of config.listenerPaths) {
    const regs: ListenerRegistration<BenchState>[] = []
    const isRoot = path === ''
    const prefix = isRoot ? '' : path + '.'
    const prefixLen = isRoot ? 0 : path.length + 1
    const depth = getPathDepth(path)

    // Set up group in graph
    graph.groupMeta.set(path, { prefix, prefixLen, isRoot, depth })
    graph.groupMembers.set(path, [])
    graph.order.push(path)

    for (let i = 0; i < config.listenersPerPath; i++) {
      const fn = createListenerFn(config.producesChanges, i === 0)
      const id = isRoot ? `__${fn.name}_${i}` : `${path}_${fn.name}_${i}`

      regs.push({
        path: (path === ''
          ? null
          : path) as ListenerRegistration<BenchState>['path'],
        scope: (path === ''
          ? null
          : path) as ListenerRegistration<BenchState>['scope'],
        fn: fn as ListenerRegistration<BenchState>['fn'],
      })

      flatListeners.push({
        path,
        prefix,
        prefixLen,
        isRoot,
        scope: isRoot ? null : path,
        fn,
        depth,
      })

      listenerFnMap.set(id, fn)

      // Add to graph
      graph.nodes.set(id, {
        scope: isRoot ? null : path,
        groupPath: path,
      })
      graph.groupMembers.get(path)!.push(id)
    }

    listeners.set(path, regs)
  }

  // Sort: deepest first (matching real behavior)
  const sortedListenerPaths = Array.from(listeners.keys()).sort(
    (a, b) => getPathDepth(b) - getPathDepth(a),
  )

  // Sort flatListeners the same way
  flatListeners.sort((a, b) => b.depth - a.depth)

  // Sort graph order and compute edges
  graph.order.sort((a, b) => {
    const metaA = graph.groupMeta.get(a)!
    const metaB = graph.groupMeta.get(b)!
    return metaB.depth - metaA.depth
  })

  // Compute edges
  for (let i = 0; i < graph.order.length; i++) {
    const routes: ListenerRoute[] = []
    for (let j = i + 1; j < graph.order.length; j++) {
      const targetPath = graph.order[j]!
      const targetMeta = graph.groupMeta.get(targetPath)!
      routes.push({
        target: targetPath,
        prefix: targetMeta.prefix,
        prefixLen: targetMeta.prefixLen,
        isRoot: targetMeta.isRoot,
      })
    }
    graph.edges.set(graph.order[i]!, routes)
  }

  const changes: AnyChange[] = Array.from(
    { length: config.changeCount },
    (_, i) => {
      const path = config.listenerPaths[i % config.listenerPaths.length]!
      const childPath = path === '' ? `field${i}` : `${path}.field${i}`
      return [childPath, `value${i}`, {}] as AnyChange
    },
  )

  return {
    state,
    listeners,
    sortedListenerPaths,
    flatListeners,
    graph,
    listenerFnMap,
    changes,
  }
}

// ============================================================================
// Benchmarks
// ============================================================================

describe('processListeners strategies', () => {
  // --- Small: 3 paths, 1 listener each, no produced changes ---
  describe('small (3 paths, 1 listener, no spillage)', () => {
    const fixture = createFixture({
      listenerPaths: ['a.b.c', 'a.b', ''],
      listenersPerPath: 1,
      producesChanges: false,
      changeCount: 3,
    })

    bench('flat loop (current)', () => {
      processListeners_flat(
        fixture.changes,
        fixture.flatListeners,
        fixture.state,
      )
    })

    bench('nested loop (old)', () => {
      processListeners_nested(
        fixture.changes,
        fixture.listeners,
        fixture.sortedListenerPaths,
        fixture.state,
      )
    })

    bench('reduce (immutable)', () => {
      processListeners_reduce(
        fixture.changes,
        fixture.flatListeners,
        fixture.state,
      )
    })

    bench('reduce (mutable acc)', () => {
      processListeners_reduceMut(
        fixture.changes,
        fixture.flatListeners,
        fixture.state,
      )
    })

    bench('graph (ListenerGraph)', () => {
      processListeners_graph(
        fixture.changes,
        fixture.graph,
        fixture.listenerFnMap,
        fixture.state,
      )
    })
  })

  // --- Medium: 5 paths, 3 listeners each, with spillage ---
  describe('medium (5 paths, 3 listeners, with spillage)', () => {
    const fixture = createFixture({
      listenerPaths: ['a.b.c.d', 'a.b.c', 'a.b', 'a', ''],
      listenersPerPath: 3,
      producesChanges: true,
      changeCount: 10,
    })

    bench('flat loop (current)', () => {
      processListeners_flat(
        fixture.changes,
        fixture.flatListeners,
        fixture.state,
      )
    })

    bench('nested loop (old)', () => {
      processListeners_nested(
        fixture.changes,
        fixture.listeners,
        fixture.sortedListenerPaths,
        fixture.state,
      )
    })

    bench('reduce (immutable)', () => {
      processListeners_reduce(
        fixture.changes,
        fixture.flatListeners,
        fixture.state,
      )
    })

    bench('reduce (mutable acc)', () => {
      processListeners_reduceMut(
        fixture.changes,
        fixture.flatListeners,
        fixture.state,
      )
    })

    bench('graph (ListenerGraph)', () => {
      processListeners_graph(
        fixture.changes,
        fixture.graph,
        fixture.listenerFnMap,
        fixture.state,
      )
    })
  })

  // --- Large: 10 paths, 5 listeners each, with spillage, 50 changes ---
  describe('large (10 paths, 5 listeners, 50 changes, spillage)', () => {
    const fixture = createFixture({
      listenerPaths: [
        'store.products.item.variant.option',
        'store.products.item.variant',
        'store.products.item',
        'store.products',
        'store.cart.items.detail',
        'store.cart.items',
        'store.cart',
        'store.user.profile',
        'store.user',
        '',
      ],
      listenersPerPath: 5,
      producesChanges: true,
      changeCount: 50,
    })

    bench('flat loop (current)', () => {
      processListeners_flat(
        fixture.changes,
        fixture.flatListeners,
        fixture.state,
      )
    })

    bench('nested loop (old)', () => {
      processListeners_nested(
        fixture.changes,
        fixture.listeners,
        fixture.sortedListenerPaths,
        fixture.state,
      )
    })

    bench('reduce (immutable)', () => {
      processListeners_reduce(
        fixture.changes,
        fixture.flatListeners,
        fixture.state,
      )
    })

    bench('reduce (mutable acc)', () => {
      processListeners_reduceMut(
        fixture.changes,
        fixture.flatListeners,
        fixture.state,
      )
    })

    bench('graph (ListenerGraph)', () => {
      processListeners_graph(
        fixture.changes,
        fixture.graph,
        fixture.listenerFnMap,
        fixture.state,
      )
    })
  })

  // --- Stress: many same-path listeners (accumulation-heavy) ---
  describe('stress: 20 same-path listeners with accumulation', () => {
    const fixture = createFixture({
      listenerPaths: ['data.items'],
      listenersPerPath: 20,
      producesChanges: true,
      changeCount: 5,
    })

    bench('flat loop (current)', () => {
      processListeners_flat(
        fixture.changes,
        fixture.flatListeners,
        fixture.state,
      )
    })

    bench('nested loop (old)', () => {
      processListeners_nested(
        fixture.changes,
        fixture.listeners,
        fixture.sortedListenerPaths,
        fixture.state,
      )
    })

    bench('reduce (immutable)', () => {
      processListeners_reduce(
        fixture.changes,
        fixture.flatListeners,
        fixture.state,
      )
    })

    bench('reduce (mutable acc)', () => {
      processListeners_reduceMut(
        fixture.changes,
        fixture.flatListeners,
        fixture.state,
      )
    })

    bench('graph (ListenerGraph)', () => {
      processListeners_graph(
        fixture.changes,
        fixture.graph,
        fixture.listenerFnMap,
        fixture.state,
      )
    })
  })
})
