/**
 * Batch Application
 *
 * Applies a batch of changes to state, checking for actual value differences.
 */

import _get from 'lodash/get'
import _set from 'lodash/set'

import type { ArrayOfChanges, GenericMeta } from '../types'

export const applyBatch = <DATA extends object, META extends GenericMeta>(
  changes: ArrayOfChanges<DATA, META>,
  state: DATA,
): void => {
  for (const [path, value] of changes) {
    const pathStr = path as string

    // Check if value actually changed before setting
    // For primitives: catches true no-ops (same value)
    // For objects: catches same-reference updates, but allows new references
    // This is the right trade-off: deep equality is too expensive
    const current = _get(state, pathStr)
    if (current !== value) {
      _set(state, pathStr, value)
    }
  }
}
