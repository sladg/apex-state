/**
 * Change Queue Utilities
 *
 * Shared queue manipulation for the pipeline.
 */

import type { ArrayOfChanges, GenericMeta } from '../types'

interface QueueChangeArgs<DATA extends object, META extends GenericMeta> {
  queue: ArrayOfChanges<DATA, META>
  path: string
  value: unknown
  meta: GenericMeta
}

/** Centralize type assertion for pushing runtime paths into typed queue */
export const queueChange = <DATA extends object, META extends GenericMeta>(
  props: QueueChangeArgs<DATA, META>,
): void => {
  // Cast is safe: paths in graphs were validated at registration
  props.queue.push([props.path, props.value, props.meta] as ArrayOfChanges<
    DATA,
    META
  >[number])
}
