/**
 * Batch Application
 *
 * Applies a batch of changes to state, checking for actual value differences.
 */

import type { ArrayOfChanges, GenericMeta } from '../types'
import { dot } from '../utils/dot'

export const applyBatch = <DATA extends object, META extends GenericMeta>(
  changes: ArrayOfChanges<DATA, META>,
  state: DATA,
): void => {
  for (const [path, value] of changes) {
    const pathStr = path as string
    dot.set__unsafe(state, pathStr, value)
  }
}
