/**
 * Side effects examples — sync, flip, aggregation, listeners.
 *
 * These examples type-check against the real @sladg/apex-state exports.
 */

import type { ArrayOfChanges } from '@sladg/apex-state'
import { createGenericStore } from '@sladg/apex-state'

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
    listeners: [{ path: 'orders', scope: 'orders', fn: handler }],
  })

  return null
}
// @llms-example-end

// @llms-example: Scoped listener that watches user.profile changes and produces audit entries
const ListenerDemo = () => {
  store.useSideEffects('listeners', {
    listeners: [
      {
        path: 'user.profile', // watch changes under this path
        scope: 'user.profile', // receive scoped state
        fn: (_changes, _state) => {
          // changes: [['name', 'Alice', {}]]  -- paths relative to scope
          // state: user.profile sub-object
          return [['audit.lastEdit', Date.now(), {}]] // return FULL paths
        },
      },
    ],
  })

  return null
}
// @llms-example-end

const handler = (
  _changes: ArrayOfChanges<TradeState['orders']>,
  _state: TradeState['orders'],
): ArrayOfChanges<TradeState> | undefined => undefined

void SideEffectsDemo
void ListenerDemo
