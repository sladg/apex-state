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
import type { PipelineObserver } from '../utils/debug-log'
import { dot } from '../utils/dot'
import type { Change, FullExecutionPlan, WasmPipeline } from '../wasm/bridge'
import { applyBatch } from './apply-batch'

// ---------------------------------------------------------------------------
// Origin → meta flag mapping
// ---------------------------------------------------------------------------

const ORIGIN_TO_META: Record<string, keyof GenericMeta> = {
  sync: 'isSyncPathChange',
  flip: 'isFlipPathChange',
  aggregation: 'isAggregationChange',
  computation: 'isComputationChange',
  clear: 'isClearPathChange',
  listener: 'isListenerChange',
}

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

/** Build a map of user-provided meta by path from the original input changes. */
const buildUserMetaByPath = <DATA extends object, META extends GenericMeta>(
  changes: ArrayOfChanges<DATA, META>,
): Map<string, GenericMeta> => {
  const map = new Map<string, GenericMeta>()
  for (const change of changes) {
    const meta = change[2]
    if (meta && Object.keys(meta as object).length > 0) {
      map.set(change[0] as string, meta as GenericMeta)
    }
  }
  return map
}

/** Convert bridge object format to pipeline tuple format, mapping origin to meta flags. */
const bridgeChangesToTuples = <DATA extends object, META extends GenericMeta>(
  changes: Change[],
  userMetaByPath?: Map<string, GenericMeta>,
): ArrayOfChanges<DATA, META> =>
  changes.map((c) => {
    const meta = buildMeta(c.path, c.origin, userMetaByPath)
    return [c.path, c.value, meta] as ArrayOfChanges<DATA, META>[number]
  }) as unknown as ArrayOfChanges<DATA, META>

// ---------------------------------------------------------------------------
// Shared micro-helpers
// ---------------------------------------------------------------------------

/** Build meta object from user-provided meta + origin flag. */
const buildMeta = (
  path: string,
  origin: string | undefined,
  userMetaByPath?: Map<string, GenericMeta>,
): GenericMeta => {
  const baseMeta = userMetaByPath?.get(path) ?? {}
  const originKey = origin ? ORIGIN_TO_META[origin] : undefined
  return originKey ? { ...baseMeta, [originKey]: true } : baseMeta
}

/** Append changes to the extra map for a given dispatch ID (get-or-create). */
const pushExtra = (
  extra: Map<number, Change[]>,
  dispatchId: number,
  changes: Change[],
): void => {
  let existing = extra.get(dispatchId)
  if (!existing) {
    existing = []
    extra.set(dispatchId, existing)
  }
  for (const c of changes) {
    existing.push(c)
  }
}

// ---------------------------------------------------------------------------
// Path helpers
// ---------------------------------------------------------------------------

/**
 * Relativize a change path against a topic path, matching legacy filterAndRelativize:
 * - Root topic (empty): path passes through as-is
 * - Exact match: keep full path (legacy line 68-70)
 * - Child match: strip topic prefix (legacy line 71-72)
 */
const relativizePath = (changePath: string, topicPath: string): string => {
  if (topicPath === '') return changePath
  if (changePath === topicPath) return changePath
  const prefix = topicPath + '.'
  if (changePath.startsWith(prefix)) return changePath.slice(prefix.length)
  return changePath
}

/** Build the input array for a single listener dispatch. */
const buildDispatchInput = (
  d: FullExecutionPlan['groups'][number]['dispatches'][number],
  stateChanges: Change[],
  extra: Map<number, Change[]>,
  userMetaByPath?: Map<string, GenericMeta>,
): [string, unknown, GenericMeta][] => {
  const topicPath = d.topic_path
  // Single pass: filter + map combined
  const input: [string, unknown, GenericMeta][] = []
  for (const id of d.input_change_ids) {
    const change = stateChanges[id]
    if (change !== undefined) {
      const meta = buildMeta(change.path, change.origin, userMetaByPath)
      input.push([relativizePath(change.path, topicPath), change.value, meta])
    }
  }

  const extraChanges = extra.get(d.dispatch_id)
  if (extraChanges) {
    for (const c of extraChanges) {
      const meta = buildMeta(c.path, c.origin, userMetaByPath)
      input.push([relativizePath(c.path, topicPath), c.value, meta])
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
      ...(c.origin ? { origin: c.origin } : {}),
    }))
    pushExtra(extra, t.target_dispatch_id, remapped)
  }
}

/**
 * Execute a pre-computed execution plan with propagation map.
 * WASM pre-computes all routing; TS just iterates and calls handlers.
 */
const executeFullExecutionPlan = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  plan: FullExecutionPlan | null,
  stateChanges: Change[],
  store: StoreInstance<DATA, META>,
  userMetaByPath?: Map<string, GenericMeta>,
  obs?: PipelineObserver,
): Change[] => {
  if (!plan || plan.groups.length === 0) {
    return []
  }

  const { listenerHandlers } = store._internal.graphs
  const allProducedChanges: Change[] = []
  const extra = new Map<number, Change[]>()

  // Single snapshot of the full state — then slice scoped subtrees from it.
  // snapshot() returns a plain frozen object, so dot.get__unsafe is just
  // regular property access with zero proxy overhead.
  const currentState = snapshot(store.state) as DATA

  // Flatten all dispatches in execution order for cascading injection
  const allDispatches = plan.groups.flatMap((g) => g.dispatches)

  for (const [i, d] of allDispatches.entries()) {
    const registration = listenerHandlers.get(d.subscriber_id)
    if (!registration) continue

    const scope = registration.scope ?? ''
    const scopedState =
      scope === '' ? currentState : dot.get__unsafe(currentState, scope)

    // buildDispatchInput reads from extra — which now includes cascaded changes
    const input = buildDispatchInput(d, stateChanges, extra, userMetaByPath)

    const result = registration.fn(input, scopedState)
    const durationMs = store._internal.timing.lastDuration
    obs?.listenerDispatch(
      d.subscriber_id,
      registration.name,
      scope || '(root)',
      input,
      result ?? [],
      i,
      durationMs,
    )
    if (!result || !(result as unknown[]).length) continue

    const producedChanges = (result as [string, unknown][]).map(
      ([path, value]) => ({ path, value, origin: 'listener' }),
    )
    allProducedChanges.push(...producedChanges)

    // Propagate to parent dispatches via pre-computed propagation map (cross-depth)
    propagateChanges(
      d.dispatch_id,
      producedChanges,
      plan.propagation_map,
      extra,
    )

    // Same-depth cascading: inject produced changes into extra for ALL subsequent dispatches
    // buildDispatchInput will pick them up naturally via extra map
    for (const subsequent of allDispatches.slice(i + 1)) {
      pushExtra(extra, subsequent.dispatch_id, producedChanges)
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
 * Execute schema validators and return concern changes.
 * Takes validators_to_run from WASM, returns concern changes with _concerns. prefix.
 */
const runValidators = (
  validatorsToRun: {
    validator_id: number
    output_path: string
    dependency_values: Record<string, string>
  }[],
  pipeline: WasmPipeline,
  obs?: PipelineObserver,
): Change[] => {
  const validationResults: Change[] = []

  for (const validator of validatorsToRun) {
    const schema = pipeline.validatorSchemas.get(validator.validator_id)
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
    const parseResult = schema.safeParse(primaryValue)

    const validationValue = {
      isError: !parseResult.success,
      errors: parseResult.success
        ? []
        : parseResult.error.errors.map((e) => ({
            field: e.path.length > 0 ? e.path.join('.') : '.',
            message: e.message,
          })),
    }

    obs?.validatorResult(validator.output_path, primaryValue, validationValue)

    // Return as Change with _concerns. prefix (WASM will strip it in finalize)
    validationResults.push({
      path: validator.output_path, // Already has _concerns. prefix
      value: validationValue,
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
        ...(change.origin ? { origin: change.origin } : {}),
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
    const pipeline = store._internal.pipeline!
    const { observer: obs } = store._internal

    // Capture user-provided meta before sending to WASM (WASM strips meta)
    const userMetaByPath = buildUserMetaByPath(initialChanges)

    // Convert to bridge format
    const bridgeChanges = tuplesToBridgeChanges(initialChanges)

    obs.pipelineStart('wasm', bridgeChanges)

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
      pipeline.processChanges(bridgeChanges)

    // Early exit if WASM signals no work to do
    if (!has_work) {
      if (trackEntry) store._debug!.calls.push(trackEntry)
      obs.pipelineEnd()
      return
    }

    // 2. Apply state changes FIRST (so listeners see updated state)
    // Partition early: separate state changes from concern changes
    const early = partitionChanges(state_changes)
    obs.phase1(early.stateChanges)

    // Apply state changes to valtio (so listeners see updated state)
    if (early.stateChanges.length > 0) {
      applyBatch(
        bridgeChangesToTuples(early.stateChanges, userMetaByPath),
        store.state,
      )
    }

    // Log sync/flip changes from phase 1
    const syncChanges = early.stateChanges.filter((c) => c.origin === 'sync')
    const flipChanges = early.stateChanges.filter((c) => c.origin === 'flip')
    if (syncChanges.length > 0) obs.syncExpand(syncChanges)
    if (flipChanges.length > 0) obs.flipExpand(flipChanges)

    // 3. Execute listeners (JS-only: user functions) - now with updated state
    const produced = executeFullExecutionPlan(
      execution_plan,
      state_changes,
      store,
      userMetaByPath,
      obs,
    )

    // Apply concern changes from phase 1 (BoolLogic results)
    if (early.concernChanges.length > 0) {
      applyConcernChanges(early.concernChanges, store._concerns)
    }

    // 4. Execute validators (JS-only: schema validation)
    const validationResults = runValidators(validators_to_run, pipeline, obs)

    // 5. WASM Phase 2: merge, diff, update shadow
    // Single flat array: listener output + validator output (with _concerns. prefix)
    const jsChanges = produced.concat(validationResults)
    const final = pipeline.pipelineFinalize(jsChanges)

    // 6. Apply NEW changes from listeners/validators to valtio
    // Phase 1 changes were already applied above, so filter them out
    const late = partitionChanges(final.state_changes)
    obs.phase2(late.stateChanges)

    // Apply only NEW changes (listener/validator output)
    if (late.stateChanges.length > 0) {
      applyBatch(
        bridgeChangesToTuples(late.stateChanges, userMetaByPath),
        store.state,
      )
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

    obs.pipelineEnd()
  }
