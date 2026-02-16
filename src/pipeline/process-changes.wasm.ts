/**
 * WASM Pipeline - processChanges implementation
 *
 * Routes changes through WASM for aggregation, sync, flip, and BoolLogic.
 * Listener dispatch uses WASM-created dispatch plans with JS handler execution.
 */

import { snapshot } from 'valtio'

import type {
  ConcernValues,
  DebugTrackEntry,
  StoreInstance,
} from '../core/types'
import type { ArrayOfChanges, GenericMeta } from '../types'
import { dot } from '../utils/dot'
import type { Change, FullExecutionPlan } from '../wasm/bridge'
import { validatorSchemas, wasm } from '../wasm/bridge'
import { applyBatch } from './apply-batch'

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
/**
 * Build the input array for a single listener dispatch.
 * Combines changes referenced by index with propagated extras from parent dispatches.
 */
const buildDispatchInput = (
  d: FullExecutionPlan['groups'][number]['dispatches'][number],
  stateChanges: Change[],
  extra: Map<number, Change[]>,
): [string, unknown, GenericMeta][] => {
  // Single pass: filter + map combined
  const input: [string, unknown, GenericMeta][] = []
  for (const id of d.input_change_ids) {
    const change = stateChanges[id]
    if (change !== undefined) {
      input.push([change.path, change.value, {}])
    }
  }

  const extraChanges = extra.get(d.dispatch_id)
  if (extraChanges) {
    for (const c of extraChanges) {
      input.push([c.path, c.value, {}])
    }
  }

  return input
}

/**
 * Remap a path with prefix for propagation to parent dispatches.
 */
const remapPath = (path: string, remapPrefix: string | null): string => {
  if (!remapPrefix) return path
  return path === '' ? remapPrefix : `${remapPrefix}.${path}`
}

/**
 * Propagate produced changes to parent dispatches via pre-computed propagation map.
 */
const propagateChanges = (
  dispatchId: number,
  producedChanges: Change[],
  propagationMap: FullExecutionPlan['propagation_map'],
  extra: Map<number, Change[]>,
): void => {
  const targets = propagationMap[dispatchId]
  if (!targets) return

  for (const t of targets) {
    let existing = extra.get(t.target_dispatch_id)
    if (!existing) {
      existing = []
      extra.set(t.target_dispatch_id, existing)
    }
    for (const c of producedChanges) {
      existing.push({
        path: remapPath(c.path, t.remap_prefix),
        value: c.value,
      })
    }
  }
}

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

      const input = buildDispatchInput(d, stateChanges, extra)

      const result = registration.fn(input, scopedState)
      if (!result || !(result as unknown[]).length) continue

      const producedChanges = (result as [string, unknown][]).map(
        ([path, value]) => ({ path, value }),
      )
      allProducedChanges.push(...producedChanges)

      // Propagate to parent dispatches via pre-computed propagation map
      propagateChanges(
        d.dispatch_id,
        producedChanges,
        plan.propagation_map,
        extra,
      )
    }
  }

  return allProducedChanges
}

/**
 * Apply concern changes to store._concerns.
 *
 * STRUCTURE: _concerns uses a two-level map for efficient lookups:
 * ```typescript
 * _concerns["user.email"]["validationState"] = { isError: false, ... }
 * _concerns["user.email"]["disabledWhen"] = true
 * ```
 *
 * WHY TWO LEVELS:
 * - First level: full path to the field (e.g., "user.email")
 * - Second level: concern name (e.g., "validationState", "disabledWhen")
 * - This allows getting all concerns for a path: `_concerns["user.email"]`
 * - React hooks can efficiently subscribe to specific path+concern combinations
 *
 * INPUT FORMAT: Paths come from WASM as "basePath.concernName"
 * - Example: "user.email.validationState" → _concerns["user.email"]["validationState"]
 * - We split on the LAST dot to separate basePath from concernName
 *
 * @param changes - Changes with paths in "basePath.concernName" format (no _concerns. prefix)
 * @param concerns - The store._concerns proxy to apply changes to
 */
const applyConcernChanges = (
  changes: Change[],
  concerns: ConcernValues,
): void => {
  for (const c of changes) {
    const lastDot = c.path.lastIndexOf('.')
    const basePath = c.path.slice(0, lastDot)
    const concernName = c.path.slice(lastDot + 1)

    if (!concerns[basePath]) {
      concerns[basePath] = {}
    }
    concerns[basePath]![concernName] = c.value
  }
}

/**
 * Execute Zod validators and return concern changes.
 * Takes validators_to_run from WASM, returns concern changes with _concerns. prefix.
 */
const runValidators = (
  validatorsToRun: {
    validator_id: number
    output_path: string
    dependency_values: Record<string, string>
  }[],
): Change[] => {
  const validationResults: Change[] = []

  for (const validator of validatorsToRun) {
    const schema = validatorSchemas.get(validator.validator_id)
    if (!schema) continue

    // Parse dependency values from JSON strings
    // dependency_values can be either a Map or a plain object from WASM
    // @FIXME: This should not be neccessary
    const entries =
      validator.dependency_values instanceof Map
        ? Array.from(validator.dependency_values.entries())
        : Object.entries(validator.dependency_values)

    const values = Object.fromEntries(
      entries.map(([k, v]) => [k, JSON.parse(v as string) as unknown]),
    )

    // For single-field validators, validate the primary value
    const primaryValue = Object.values(values)[0]
    const zodResult = schema.safeParse(primaryValue)

    // Return as Change with _concerns. prefix (WASM will strip it in finalize)
    validationResults.push({
      path: validator.output_path, // Already has _concerns. prefix
      value: {
        isError: !zodResult.success,
        errors: zodResult.success
          ? []
          : zodResult.error.errors.map((e) => ({
              field: e.path.length > 0 ? e.path.join('.') : '.',
              message: e.message,
            })),
      },
    })
  }

  return validationResults
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

const CONCERNS_PREFIX = '_concerns.'
const CONCERNS_PREFIX_LEN = CONCERNS_PREFIX.length

/** Partition changes into state vs concern changes, stripping _concerns. prefix. */
const partitionChanges = (
  changes: Change[],
): { stateChanges: Change[]; concernChanges: Change[] } => {
  const stateChanges: Change[] = []
  const concernChanges: Change[] = []
  for (const change of changes) {
    if (change.path.startsWith(CONCERNS_PREFIX)) {
      concernChanges.push({
        path: change.path.slice(CONCERNS_PREFIX_LEN),
        value: change.value,
      })
    } else {
      stateChanges.push(change)
    }
  }
  return { stateChanges, concernChanges }
}

// ---------------------------------------------------------------------------
// Record applied changes for debug tracking
// ---------------------------------------------------------------------------

const pushDebugChanges = (
  target: { path: string; value: unknown }[],
  changes: Change[],
): void => {
  for (const c of changes) {
    target.push({ path: c.path, value: c.value })
  }
}

// ---------------------------------------------------------------------------
// Main WASM pipeline implementation
// ---------------------------------------------------------------------------

export const processChangesWasm: typeof import('./process-changes').processChanges =
  (store, initialChanges) => {
    // Convert to bridge format
    const bridgeChanges = tuplesToBridgeChanges(initialChanges)

    // Initialize debug entry if tracking is enabled
    const trackEntry: DebugTrackEntry | null = store._debug
      ? {
          input: initialChanges.map(([p, v, m]) => [p as string, v, m]),
          applied: [],
          appliedConcerns: [],
          timestamp: Date.now(),
        }
      : null

    // 1. WASM Phase 1: aggregation → sync → flip → BoolLogic
    const { state_changes, execution_plan, validators_to_run, has_work } =
      wasm.processChanges(bridgeChanges)

    // Early exit if WASM signals no work to do
    if (!has_work) {
      if (trackEntry) store._debug!.calls.push(trackEntry)
      return
    }

    // 2. Apply state changes FIRST (so listeners see updated state)
    // Partition early: separate state changes from concern changes
    const early = partitionChanges(state_changes)

    // Apply state changes to valtio (so listeners see updated state)
    if (early.stateChanges.length > 0) {
      applyBatch(bridgeChangesToTuples(early.stateChanges), store.state)
    }

    // 3. Execute listeners (JS-only: user functions) - now with updated state
    const currentState = snapshot(store.state) as typeof store.state
    const produced = executeFullExecutionPlan(
      execution_plan,
      state_changes,
      store,
      currentState,
    )

    // Apply concern changes from phase 1 (BoolLogic results)
    if (early.concernChanges.length > 0) {
      applyConcernChanges(early.concernChanges, store._concerns)
    }

    // 4. Execute validators (JS-only: Zod schemas)
    const validationResults = runValidators(validators_to_run)

    // 5. WASM Phase 2: merge, diff, update shadow
    // Single flat array: listener output + validator output (with _concerns. prefix)
    const jsChanges = produced.concat(validationResults)
    const final = wasm.pipelineFinalize(jsChanges)

    // 6. Apply NEW changes from listeners/validators to valtio
    // Phase 1 changes were already applied above, so filter them out
    const late = partitionChanges(final.state_changes)

    // Apply only NEW changes (listener/validator output)
    if (late.stateChanges.length > 0) {
      applyBatch(bridgeChangesToTuples(late.stateChanges), store.state)
    }
    if (late.concernChanges.length > 0) {
      applyConcernChanges(late.concernChanges, store._concerns)
    }

    // Record applied changes for debug tracking
    if (trackEntry) {
      pushDebugChanges(trackEntry.applied, early.stateChanges)
      pushDebugChanges(trackEntry.applied, late.stateChanges)
      pushDebugChanges(trackEntry.appliedConcerns, early.concernChanges)
      pushDebugChanges(trackEntry.appliedConcerns, late.concernChanges)
      store._debug!.calls.push(trackEntry)
    }
  }
