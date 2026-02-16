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
  const input: [string, unknown, GenericMeta][] = d.input_change_ids
    .filter((id) => stateChanges[id] !== undefined)
    .map(
      (id) =>
        [stateChanges[id]!.path, stateChanges[id]!.value, {}] as [
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
    const remapped = producedChanges.map((c) => ({
      path: remapPath(c.path, t.remap_prefix),
      value: c.value,
    }))
    const existing = extra.get(t.target_dispatch_id) ?? []
    extra.set(t.target_dispatch_id, [...existing, ...remapped])
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
    const parts = c.path.split('.')
    const concernName = parts.pop()!
    const basePath = parts.join('.')

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
      path: validator.output_path, // Already has _concerns. prefix from WASM
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
// Partition final changes by prefix and route to appropriate proxies
// ---------------------------------------------------------------------------

const partitionAndApply = <DATA extends object, META extends GenericMeta>(
  finalChanges: Change[],
  store: StoreInstance<DATA, META>,
): { stateChanges: Change[]; concernChanges: Change[] } => {
  const stateChanges: Change[] = []
  const concernChanges: Change[] = []

  for (const change of finalChanges) {
    if (change.path.startsWith('_concerns.')) {
      // Strip prefix and route to _concerns proxy
      concernChanges.push({
        path: change.path.slice('_concerns.'.length),
        value: change.value,
      })
    } else {
      // Route to state proxy
      stateChanges.push(change)
    }
  }

  if (stateChanges.length > 0) {
    applyBatch(bridgeChangesToTuples(stateChanges), store.state)
  }
  if (concernChanges.length > 0) {
    applyConcernChanges(concernChanges, store._concerns)
  }

  return { stateChanges, concernChanges }
}

// ---------------------------------------------------------------------------
// Record applied changes for debug tracking
// ---------------------------------------------------------------------------

const recordDebugTracking = <DATA extends object, META extends GenericMeta>(
  trackEntry: DebugTrackEntry,
  stateChanges: Change[],
  concernChanges: Change[],
  store: StoreInstance<DATA, META>,
): void => {
  for (const c of stateChanges) {
    trackEntry.applied.push({ path: c.path, value: c.value })
  }
  for (const c of concernChanges) {
    trackEntry.appliedConcerns.push({ path: c.path, value: c.value })
  }
  store._debug!.calls.push(trackEntry)
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

    // 2. Execute listeners (JS-only: user functions)
    const currentState = snapshot(store.state) as typeof store.state
    const produced = executeFullExecutionPlan(
      execution_plan,
      state_changes,
      store,
      currentState,
    )

    // 3. Execute validators (JS-only: Zod schemas)
    const validationResults = runValidators(validators_to_run)

    // 4. WASM Phase 2: merge, diff, update shadow
    // Single flat array: listener output + validator output (with _concerns. prefix)
    const jsChanges: Change[] = [...produced, ...validationResults]
    const final = wasm.pipelineFinalize(jsChanges)

    // 5. Apply to valtio - partition by prefix and route to appropriate proxies
    const { stateChanges, concernChanges } = partitionAndApply(
      final.state_changes,
      store,
    )

    // Record applied changes for debug tracking
    if (trackEntry) {
      recordDebugTracking(trackEntry, stateChanges, concernChanges, store)
    }
  }
