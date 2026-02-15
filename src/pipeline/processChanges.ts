/**
 * Change Processing Pipeline
 *
 * Routes changes through WASM for aggregation, sync, flip, and BoolLogic.
 * Listener dispatch uses WASM-created dispatch plans with JS handler execution.
 *
 * Falls back to JS processors when WASM is not loaded.
 */

import { snapshot } from 'valtio'

import type { StoreInstance } from '../core/types'
import type { ArrayOfChanges, GenericMeta } from '../types'
import { dot } from '../utils/dot'
import type { Change, FullExecutionPlan } from '../wasm/bridge'
import { isWasmLoaded, validatorSchemas, wasm } from '../wasm/bridge'
import { applyBatch } from './applyBatch'
import {
  processAggregationWrites,
  processFlipPaths,
  processListeners,
  processSyncPaths,
} from './processors'
import { queueChange } from './queue'

// ---------------------------------------------------------------------------
// Conversion helpers: ArrayOfChanges <-> Change[]
// ---------------------------------------------------------------------------

/** Convert pipeline tuple format to bridge object format. */
const tuplesToBridgeChanges = <DATA extends object, META extends GenericMeta>(
  changes: ArrayOfChanges<DATA, META>,
): Change[] =>
  changes.map(([path, value]) => ({
    path: path as string,
    value,
  }))

/** Convert bridge object format to pipeline tuple format. */
const bridgeChangesToTuples = <DATA extends object, META extends GenericMeta>(
  changes: Change[],
  meta: GenericMeta = {},
): ArrayOfChanges<DATA, META> =>
  changes.map(
    (c) => [c.path, c.value, meta] as ArrayOfChanges<DATA, META>[number],
  ) as unknown as ArrayOfChanges<DATA, META>

/**
 * Execute a pre-computed execution plan with propagation map.
 * Trivial loop — no ancestor walking, no path remapping logic.
 * WASM pre-computes all routing; TS just iterates and calls handlers.
 */
const executeFullExecutionPlan = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  plan: FullExecutionPlan | null,
  stateChanges: Change[],
  store: StoreInstance<DATA, META>,
  currentState: DATA,
): Change[] => {
  if (!plan || plan.groups.length === 0) {
    return []
  }

  const { listenerHandlers } = store._internal.graphs
  const allProducedChanges: Change[] = []
  const extra = new Map<number, Change[]>()

  for (const group of plan.groups) {
    for (const d of group.dispatches) {
      const registration = listenerHandlers.get(d.subscriber_id)
      if (!registration) continue

      const scope = registration.scope ?? ''
      const scopedState =
        scope === '' ? currentState : dot.get__unsafe(currentState, scope)

      // Build input: changes referenced by index + propagated extras
      const input: [string, unknown, GenericMeta][] = d.input_change_ids.map(
        (id) =>
          [stateChanges[id].path, stateChanges[id].value, {}] as [
            string,
            unknown,
            GenericMeta,
          ],
      )

      const extraChanges = extra.get(d.dispatch_id)
      if (extraChanges) {
        for (const c of extraChanges) {
          input.push([c.path, c.value, {}] as [string, unknown, GenericMeta])
        }
      }

      const result = registration.fn(input, scopedState)
      if (!result || !(result as unknown[]).length) continue

      const producedChanges = (result as [string, unknown][]).map(
        ([path, value]) => ({ path, value }),
      )
      allProducedChanges.push(...producedChanges)

      // Propagate to parent dispatches via pre-computed propagation map
      const targets = plan.propagation_map[d.dispatch_id]
      if (targets) {
        for (const t of targets) {
          const remapped = producedChanges.map((c) => ({
            path: t.remap_prefix
              ? c.path === ''
                ? t.remap_prefix
                : `${t.remap_prefix}.${c.path}`
              : c.path,
            value: c.value,
          }))
          const existing = extra.get(t.target_dispatch_id) ?? []
          extra.set(t.target_dispatch_id, [...existing, ...remapped])
        }
      }
    }
  }

  return allProducedChanges
}

// ---------------------------------------------------------------------------
// JS fallback pipeline (used when WASM is not loaded)
// ---------------------------------------------------------------------------

const processChangesJS = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  store: StoreInstance<DATA, META>,
  initialChanges: ArrayOfChanges<DATA, META>,
): void => {
  const { processing } = store._internal

  // Use queue as the mutable batch - processors write to it directly
  processing.queue = [...initialChanges]

  // Get current state snapshot for processing
  const currentState = snapshot(store.state) as DATA

  // Single-pass sequential processing - order matters
  // 1. Aggregation writes: intercept writes to targets, distribute to sources
  processAggregationWrites(processing.queue, store)

  // 2. Sync: bidirectional path synchronization
  processSyncPaths(processing.queue, store)

  // 3. Flip: invert boolean values across paired paths
  processFlipPaths(processing.queue, store)

  // 4. Listeners: reactive side effects
  processListeners(processing.queue, store, currentState)

  // Apply all accumulated changes to state once
  applyBatch(processing.queue, store.state)
}

// ---------------------------------------------------------------------------
// WASM pipeline
// ---------------------------------------------------------------------------

const processChangesWASM = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  store: StoreInstance<DATA, META>,
  initialChanges: ArrayOfChanges<DATA, META>,
): void => {
  const { processing } = store._internal

  // Convert to bridge format
  const bridgeChanges = tuplesToBridgeChanges(initialChanges)

  // WASM handles: aggregation -> sync -> flip -> BoolLogic evaluation
  // Returns state changes, concern changes, validators to run, and pre-computed execution plan
  const {
    changes: stateChanges,
    concern_changes: concernChanges,
    validators_to_run: validatorsToRun,
    execution_plan: executionPlan,
  } = wasm.processChanges(bridgeChanges)

  // Early exit if all results are empty (diff filtered out all no-ops)
  if (
    stateChanges.length === 0 &&
    concernChanges.length === 0 &&
    validatorsToRun.length === 0
  ) {
    return
  }

  // Apply BoolLogic results to _concerns proxy
  for (const change of concernChanges) {
    const concernPath = change.path.slice('_concerns.'.length)
    dot.set__unsafe(store._concerns, concernPath, change.value)
  }

  // Execute validators if any are affected
  if (validatorsToRun?.length) {
    for (const validator of validatorsToRun) {
      const schema = validatorSchemas.get(validator.validator_id)
      if (!schema) continue

      // Parse dependency values from JSON strings
      const values = Object.fromEntries(
        Object.entries(validator.dependency_values).map(([k, v]) => [
          k,
          JSON.parse(v) as unknown,
        ]),
      )

      // For single-field validators, validate the primary value
      const primaryValue = Object.values(values)[0]
      const zodResult = schema.safeParse(primaryValue)

      // Write validation result to _concerns proxy
      const concernPath = validator.output_path.slice('_concerns.'.length)
      dot.set__unsafe(store._concerns, concernPath, {
        isError: !zodResult.success,
        errors: zodResult.success
          ? []
          : zodResult.error.errors.map((e) => ({
              field: e.path.length > 0 ? e.path.join('.') : '.',
              message: e.message,
            })),
      })
    }
  }

  // Get current state snapshot for listener execution
  const currentState = snapshot(store.state) as DATA

  // Convert state changes back to pipeline tuple format
  processing.queue = bridgeChangesToTuples(stateChanges)

  // Execute pre-computed execution plan (trivial loop, no WASM calls)
  if (executionPlan) {
    const producedChanges = executeFullExecutionPlan(
      executionPlan,
      stateChanges,
      store,
      currentState,
    )

    // Add listener-produced changes to the queue
    for (const change of producedChanges) {
      queueChange({
        queue: processing.queue,
        path: change.path,
        value: change.value,
        meta: { isListenerChange: true },
      })
    }
  }

  // Checkpoint 3 (output): Diff final queue against shadow state
  // Filters out any no-ops before applying to valtio (including listener-produced changes)
  const queueAsBridgeChanges = tuplesToBridgeChanges(processing.queue)
  const genuineChanges = wasm.diffChanges(queueAsBridgeChanges)

  // Early exit if diff filtered everything out
  if (genuineChanges.length === 0) {
    return
  }

  // Convert back to tuple format for applyBatch
  processing.queue = bridgeChangesToTuples(genuineChanges)

  // Apply all genuine changes to state once
  applyBatch(processing.queue, store.state)
}

// ---------------------------------------------------------------------------
// Main entry point
// ---------------------------------------------------------------------------

export const processChanges = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  store: StoreInstance<DATA, META>,
  initialChanges: ArrayOfChanges<DATA, META>,
): void => {
  try {
    if (isWasmLoaded()) {
      processChangesWASM(store, initialChanges)
    } else {
      processChangesJS(store, initialChanges)
    }
  } catch {
    //
  }
}
