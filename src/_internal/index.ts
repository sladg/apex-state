/**
 * Internal API Surface
 *
 * Single entry point for all _internal exports consumed by public modules.
 * This file documents the exact coupling between _internal and the rest of the library.
 *
 * Allowed consumers: store/, sideEffects/, concerns/, core/
 * Blocked consumers: index.ts, hooks/, utils/, types/ (enforced by ESLint)
 *
 * NOTE: Graph, TopicRouter, and guards are public utilities in src/utils/.
 * Import those directly — they are NOT part of _internal.
 */

import { registerConcernEffects } from './concerns/registration'
import { processChanges } from './pipeline/processChanges'
import { deepMerge } from './utils/deepMerge'
import { createTiming } from './utils/timing'

// Types (can't live on a runtime object — exported separately)
export type {
  ConcernValues,
  InternalState,
  ListenerRegistrationInternal,
} from './core/types'
export type { ChangeTuple } from './types/changes'
export type { Timing, TimingConfig } from './utils/timing'

/**
 * Runtime namespace for all internal APIs.
 *
 * Usage: `import { _internal } from '../_internal'`
 * Then:  `_internal.processChanges(store, changes)`, etc.
 */
export const _internal = {
  // Pipeline — change processing
  processChanges,

  // Utils — timing, merging
  deepMerge,
  createTiming,

  // Concerns — effect registration
  registerConcernEffects,
} as const
