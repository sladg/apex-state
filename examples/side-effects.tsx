/**
 * Side effects examples — sync, flip, aggregation, listeners.
 *
 * These examples type-check against the real @sladg/apex-state exports.
 */

import type { OnStateListener } from '@sladg/apex-state'
import { createGenericStore, listeners } from '@sladg/apex-state'

interface TradeState {
  'source': string
  'target': string
  'active': boolean
  'inactive': boolean
  'orders': { items: { price: number }[] }
  'audit': { lastEdit: number }
  'user': { profile: { name: string; age: number } }
  'summary.price': number
  'legs.0.price': number
  'legs.1.price': number
}

const store = createGenericStore<TradeState>()

// @llms-example: Standalone listener fns — typed independently of the store
// Use OnStateListener<DATA, SUB_STATE> to define standalone listener functions.
// SUB_STATE matches the scope type (what the listener receives as state).

// Scoped listener fn: receives user.profile as state, returns scoped changes
const profileListener: OnStateListener<
  TradeState,
  TradeState['user']['profile']
> = (_changes, _state) => {
  // _changes: ArrayOfChanges<TradeState['user']['profile']>  (paths relative to scope)
  // _state: TradeState['user']['profile']                     (scoped state)
  // return: ArrayOfChanges<TradeState['user']['profile']> | undefined  (scoped paths)
  return [['name', `updated-${_state.name}`, {}]]
}

// Full-state listener fn: receives the entire TradeState['orders'] scope
const ordersHandler: OnStateListener<TradeState, TradeState['orders']> = (
  _changes,
  _state,
) => undefined
// @llms-example-end

// @llms-example: Register sync paths, flip paths, aggregations, and listeners in one call
const SideEffectsDemo = () => {
  store.useSideEffects('demo', {
    syncPaths: [['source', 'target']],
    flipPaths: [['active', 'inactive']],
    // Aggregation: target reflects the common value when all sources agree, null otherwise.
    // Multiple pairs with the same target form a group.
    // Currently supports consensus (all-equal) mode only — SUM, AVG, COUNT planned (see Roadmap).
    aggregations: [
      ['summary.price', 'legs.0.price'],
      ['summary.price', 'legs.1.price'],
    ],
    listeners: [{ path: 'orders', scope: 'orders', fn: ordersHandler }],
  })

  return null
}
// @llms-example-end

// @llms-example: Inline listener with standalone fn (direct pattern)
const ListenerDemo = () => {
  store.useSideEffects('listeners', {
    listeners: [
      {
        path: 'user.profile', // watch changes under this path
        scope: 'user.profile', // receive scoped state
        fn: profileListener, // standalone fn — pre-typed
      },
    ],
  })

  return null
}
// @llms-example-end

// @llms-example: Branded listeners — validated with scope-prefix-of-path check
// Use listeners<State>()([...]) for type-safe scope/path validation at definition time.
// The branded result is accepted by useSideEffects without re-validation.
const brandedListeners = listeners<TradeState>()([
  {
    path: 'user.profile', // scope omitted → defaults to 'user.profile'
    fn: profileListener, // fn typed for scoped state
  },
  {
    path: 'orders', // scope omitted → defaults to 'orders'
    fn: ordersHandler, // standalone handler
  },
  {
    path: 'active', // watch a specific path
    scope: null, // null scope → fn gets full state
    fn: ((_changes, _state) => undefined) as OnStateListener<
      TradeState,
      TradeState
    >,
  },
])
// @llms-example-end

// @llms-example: Pre-warmed pair helpers — define pairs at module scope, reuse across components
//
// When to use each pattern:
// - Pre-warmed (store.syncPairs): Best for most cases. Type is already bound to your state,
//   pairs are validated once and reused. No need to repeat <State> generic.
// - Standalone (syncPairs<State>()): Useful when you need pairs outside a store context,
//   e.g. in shared config files or libraries.
// - Inline (as shown in SideEffectsDemo): Quick and simple for one-off usage.

const {
  useSideEffects,
  syncPairs: storeSyncPairs,
  flipPairs: storeFlipPairs,
  aggregationPairs: storeAggregationPairs,
  listeners: storeListeners,
} = store

// Define pairs at module scope — validated once, reused across components
const syncs = storeSyncPairs([['source', 'target']])
const flips = storeFlipPairs([['active', 'inactive']])
const aggs = storeAggregationPairs([
  ['summary.price', 'legs.0.price'],
  ['summary.price', 'legs.1.price'],
])
// Pre-warmed listeners — scope omitted, defaults to path
const list = storeListeners([{ path: 'orders', fn: ordersHandler }])

const PreWarmedDemo = () => {
  useSideEffects('pre-warmed', {
    syncPaths: syncs,
    flipPaths: flips,
    aggregations: aggs,
    listeners: list,
  })

  return null
}
// @llms-example-end

void SideEffectsDemo
void ListenerDemo
void PreWarmedDemo
void brandedListeners
