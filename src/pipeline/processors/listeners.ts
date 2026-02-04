/**
 * Listener Processor
 *
 * Processes registered listeners with filtered/scoped changes.
 * Listeners only receive child changes (not exact path matches).
 */

import type { StoreInstance } from '../../core/types'
import type { ArrayOfChanges, GenericMeta } from '../../types'
import { dot } from '../../utils/dot'
import { getPathDepth } from '../../utils/pathUtils'
import { AnyChange } from '../normalizeChanges'
import { queueChange } from '../queue'
import type { ProcessListenerArgs } from './types'

/**
 * Process a single listener with relevant changes
 * Handles path (what to watch) vs scope (how to present data) distinction
 */
const processListener = <DATA extends object, META extends GenericMeta>(
  props: ProcessListenerArgs<DATA, META>,
): void => {
  if (props.relevantChanges.length === 0) return

  const scope = props.registration.scope ?? ''

  // Get scoped state for the listener
  const scopedState =
    scope === ''
      ? props.currentState
      : dot.get__unsafe(props.currentState, scope)

  // Call listener ONCE with changes and SCOPED state
  const result = props.registration.fn(props.relevantChanges, scopedState)
  if (!result || result.length === 0) return

  // Add listener metadata to returned changes
  for (const [path, value, changeMeta] of result) {
    const meta = { isListenerChange: true, ...changeMeta }
    queueChange({ queue: props.queue, path, value, meta })
  }
}

/**
 * Filter changes to only include children of the listener path
 * and convert to relative paths in a single pass
 */
const filterAndRelativize = (
  changes: AnyChange[],
  listenerPath: string,
): AnyChange[] => {
  const result: AnyChange[] = []

  // Root listener: include top-level paths only (no dots in path)
  if (listenerPath === '') {
    for (const change of changes) {
      if (!change[0].includes('.')) {
        result.push(change)
      }
    }
    return result
  }

  // Non-root listener: filter children and convert to relative paths
  const prefix = listenerPath + '.'
  for (const change of changes) {
    if (change[0].startsWith(prefix)) {
      result.push([change[0].slice(prefix.length), change[1], change[2]])
    }
  }

  return result
}

export const processListeners = <DATA extends object, META extends GenericMeta>(
  changes: ArrayOfChanges<DATA, META>,
  store: StoreInstance<DATA, META>,
  currentState: DATA,
): void => {
  const { listeners, sortedListenerPaths } = store._internal.graphs
  const { queue } = store._internal.processing

  // Sort changes by path depth (deepest first)
  const sortedChanges = ([...changes] as AnyChange[]).sort(
    (a, b) => getPathDepth(b[0]) - getPathDepth(a[0]),
  )

  // Process each listener path with filtered changes
  for (const listenerPath of sortedListenerPaths) {
    const pathListeners = listeners.get(listenerPath)!

    // Filter and relativize changes in single pass
    const relevantChanges = filterAndRelativize(sortedChanges, listenerPath)

    // Process each listener with the filtered changes
    for (const registration of pathListeners) {
      processListener({
        // Safe: ListenerRegistration and ListenerRegistrationInternal have same runtime shape
        // The function signature difference (ArrayOfChanges vs AnyChange[]) is a compile-time distinction only
        registration: registration as unknown as ProcessListenerArgs<
          DATA,
          META
        >['registration'],
        relevantChanges,
        currentState,
        queue,
      })
    }
  }
}
