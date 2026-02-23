/**
 * Shared helpers for benchmark files.
 *
 * Extracted from baseline, legacy-vs-wasm, optimization, and profiling benchmarks
 * to reduce ~300 lines of duplication across 6 files.
 */

import { type Change, createWasmPipeline } from '../../src/wasm/bridge'

export type { Change }

export const BENCH_OPTIONS = { iterations: 50, warmupIterations: 5 }

// ---------------------------------------------------------------------------
// State builders
// ---------------------------------------------------------------------------

/** Build a flat state with N fields: `${prefix}_0`, `${prefix}_1`, … */
export const buildFields = (
  count: number,
  prefix = 'field',
  value: ((i: number) => unknown) | unknown = undefined,
): Record<string, unknown> => {
  const valueFn =
    typeof value === 'function'
      ? (value as (i: number) => unknown)
      : value !== undefined
        ? () => value
        : (i: number) => `value_${i}`
  const state: Record<string, unknown> = {}
  for (let i = 0; i < count; i++) {
    state[`${prefix}_${i}`] = valueFn(i)
  }
  return state
}

/** Shorthand: N string fields (field_0 … field_N-1). */
export const buildState = (count: number) => buildFields(count)

/** Build N changes for batch processing. */
export const buildBatch = (count: number): Change[] =>
  Array.from({ length: count }, (_, i) => ({
    path: `field_${i}`,
    value: `batch_${i}`,
  }))

/** Build a deeply nested state object (N levels deep). */
export const buildDeepState = (depth: number): Record<string, unknown> => {
  let obj: Record<string, unknown> = { value: 'x' }
  for (let i = depth; i >= 1; i--) {
    obj = { [`l${i}`]: obj }
  }
  return obj
}

// ---------------------------------------------------------------------------
// Pipeline factories
// ---------------------------------------------------------------------------

/** Create a bare pipeline with N string fields, no effects registered. */
export const createBarePipeline = (fieldCount: number) => {
  const pipeline = createWasmPipeline()
  pipeline.shadowInit(buildState(fieldCount))
  return pipeline
}

/** Create a pipeline with N sync pairs: field_0 → sync_target_0, etc. */
export const createSyncPipeline = (pairCount: number) => {
  const state = buildState(pairCount)
  for (let i = 0; i < pairCount; i++) {
    state[`sync_target_${i}`] = `value_${i}`
  }
  const pipeline = createWasmPipeline()
  pipeline.shadowInit(state)
  const syncPairs: [string, string][] = []
  for (let i = 0; i < pairCount; i++) {
    syncPairs.push([`field_${i}`, `sync_target_${i}`])
  }
  pipeline.registerSideEffects({
    registration_id: 'bench',
    sync_pairs: syncPairs,
  })
  return pipeline
}

/** Create a pipeline with N flip pairs: bool_0 → flip_target_0, etc. */
export const createFlipPipeline = (pairCount: number) => {
  const state = buildFields(pairCount, 'bool', false)
  for (let i = 0; i < pairCount; i++) {
    state[`flip_target_${i}`] = true
  }
  const pipeline = createWasmPipeline()
  pipeline.shadowInit(state)
  const flipPairs: [string, string][] = []
  for (let i = 0; i < pairCount; i++) {
    flipPairs.push([`bool_${i}`, `flip_target_${i}`])
  }
  pipeline.registerSideEffects({
    registration_id: 'bench',
    flip_pairs: flipPairs,
  })
  return pipeline
}

/** Create a pipeline with N listeners on field_0. */
export const createListenerPipeline = (listenerCount: number) => {
  const state = buildState(1)
  const pipeline = createWasmPipeline()
  pipeline.shadowInit(state)
  const listeners = []
  for (let i = 0; i < listenerCount; i++) {
    listeners.push({
      subscriber_id: i,
      topic_path: 'field_0',
      scope_path: '',
    })
  }
  pipeline.registerSideEffects({
    registration_id: 'bench',
    listeners,
  })
  return pipeline
}

/** Create a pipeline with N listeners, each on a different path (field_0 … field_N-1). */
export const createMultiPathListenerPipeline = (listenerCount: number) => {
  const state = buildState(listenerCount)
  const pipeline = createWasmPipeline()
  pipeline.shadowInit(state)
  const listeners = []
  for (let i = 0; i < listenerCount; i++) {
    listeners.push({
      subscriber_id: i,
      topic_path: `field_${i}`,
      scope_path: '',
    })
  }
  pipeline.registerSideEffects({
    registration_id: 'bench',
    listeners,
  })
  return pipeline
}

/** Create a pipeline with combined sync + flip + listeners. */
export const createCombinedPipeline = (
  syncCount: number,
  flipCount: number,
  listenerCount: number,
) => {
  const state: Record<string, unknown> = {
    ...buildFields(syncCount),
    ...buildFields(syncCount, 'sync_target'),
    ...buildFields(flipCount, 'bool', false),
    ...buildFields(flipCount, 'flip_target', true),
  }
  const pipeline = createWasmPipeline()
  pipeline.shadowInit(state)

  const syncPairs: [string, string][] = []
  for (let i = 0; i < syncCount; i++) {
    syncPairs.push([`field_${i}`, `sync_target_${i}`])
  }

  const flipPairs: [string, string][] = []
  for (let i = 0; i < flipCount; i++) {
    flipPairs.push([`bool_${i}`, `flip_target_${i}`])
  }

  const listeners = []
  for (let i = 0; i < listenerCount; i++) {
    listeners.push({
      subscriber_id: i,
      topic_path: `field_0`,
      scope_path: '',
    })
  }

  pipeline.registerSideEffects({
    registration_id: 'bench',
    sync_pairs: syncPairs,
    flip_pairs: flipPairs,
    listeners,
  })
  return pipeline
}

/** Create a pipeline with N BoolLogic concerns on field_0. */
export const createBoolLogicPipeline = (logicCount: number) => {
  const state: Record<string, unknown> = {
    ...buildState(2), // field_0 (trigger), field_1 (value to compare)
    ...buildFields(logicCount, 'bool', false),
  }
  const pipeline = createWasmPipeline()
  pipeline.shadowInit(state)
  const boolLogics = []
  for (let i = 0; i < logicCount; i++) {
    boolLogics.push({
      output_path: `bool_${i}`,
      tree_json: JSON.stringify({ IS_EQUAL: ['field_0', 'field_1'] }),
    })
  }
  pipeline.registerConcerns({
    registration_id: 'bench',
    bool_logics: boolLogics,
  })
  return pipeline
}
