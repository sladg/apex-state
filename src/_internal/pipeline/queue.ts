/**
 * Change Queue Utilities
 *
 * Shared queue manipulation for the pipeline.
 */

import type { GenericMeta } from '~/types'

import type { ChangeTuple } from '../types/changes'

interface QueueChangeArgs {
  queue: ChangeTuple
  path: string
  value: unknown
  meta: GenericMeta
}

/** Centralize type assertion for pushing runtime paths into typed queue */
export const queueChange = (props: QueueChangeArgs): void => {
  props.queue.push([props.path, props.value, props.meta])
}
