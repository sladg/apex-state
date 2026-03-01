/**
 * WASM Pipeline - processChanges implementation
 *
 * Routes changes through WASM for aggregation, sync, flip, and BoolLogic.
 * Listener dispatch uses WASM-created dispatch plans with JS handler execution.
 *
 * Data model: `Change` is normalized once at entry and carried through every stage.
 * Meta stays attached to each record. State vs concern partitioning uses the
 * `_concerns.` path prefix directly at apply time.
 */

import { snapshot } from 'valtio'

import type {
  ConcernValues,
  DebugTrackEntry,
  StoreInstance,
} from '../core/types'
import { type ArrayOfChanges, type GenericMeta } from '../types'
import { dot } from '../utils/dot'
import type { ListenerDispatchTrace, UnifiedPipelineTrace } from '../utils/log'
import type { Change, Wasm, WasmPipeline } from '../wasm/bridge'
import { applyBatch } from './apply-batch'

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

const CONCERNS_PREFIX = '_concerns.'
const CONCERNS_PREFIX_LEN = CONCERNS_PREFIX.length

// ---------------------------------------------------------------------------
// Normalization — single entry point, one shape for the entire pipeline
// ---------------------------------------------------------------------------

/** Normalize input tuples into Change[] once at entry. */
const normalizeInputChanges = <DATA extends object, META extends GenericMeta>(
  input: ArrayOfChanges<DATA, META>,
): Change[] =>
  input.map(([path, value, meta]) => ({
    path: path as string,
    value,
    meta: meta ?? {},
  }))

/** Normalize a WASM Change[] back to Change[], resolving meta. */
const normalizeWasmChanges = (wasmChanges: Change[]): Change[] =>
  wasmChanges.map(({ path, value, meta }) => ({ path, value, meta }))

/** Normalize raw listener output tuples `[path, value][]` to Change[]. */
const normalizeListenerOutput = (raw: [string, unknown][]): Change[] =>
  raw.map(([path, value]) => ({ path, value, meta: {} }))

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

/**
 * Remap a path with prefix for propagation to parent dispatches.
 */
const remapPath = (path: string, remapPrefix: string | null): string => {
  if (!remapPrefix) return path
  return path === '' ? remapPrefix : `${remapPrefix}.${path}`
}

// ---------------------------------------------------------------------------
// Dispatch helpers
// ---------------------------------------------------------------------------

/** Append Changes to the extra map for a given dispatch ID (get-or-create). */
/** Build the input tuple array for a single listener invocation. */
const buildDispatchInput = (
  d: Wasm.FullExecutionPlan['groups'][number]['dispatches'][number],
  listenerChanges: Change[],
  extra: Map<number, Change[]>,
): [string, unknown, GenericMeta][] => {
  const topicPath = d.topic_path
  const input: [string, unknown, GenericMeta][] = []

  for (const id of d.input_change_ids) {
    const change = listenerChanges[id]
    if (change !== undefined) {
      input.push([
        relativizePath(change.path, topicPath),
        structuredClone(change.value),
        change.meta,
      ])
    }
  }

  const extraChanges = extra.get(d.dispatch_id)
  if (extraChanges) {
    for (const c of extraChanges) {
      input.push([
        relativizePath(c.path, topicPath),
        structuredClone(c.value),
        c.meta,
      ])
    }
  }

  return input
}

/**
 * Propagate produced changes to parent dispatches via pre-computed propagation map.
 */
const propagateChanges = (
  dispatchId: number,
  producedChanges: Change[],
  propagationMap: Wasm.FullExecutionPlan['propagation_map'],
  extra: Map<number, Change[]>,
): void => {
  const targets = propagationMap[dispatchId]
  if (!targets) return

  for (const t of targets) {
    const remapped = producedChanges.map((c) => ({
      ...c,
      path: remapPath(c.path, t.remap_prefix),
    }))
    const existing = extra.get(t.target_dispatch_id) ?? []
    extra.set(t.target_dispatch_id, existing)
    existing.push(...remapped)
  }
}

/**
 * Execute a pre-computed execution plan with propagation map.
 * WASM pre-computes all routing; TS just iterates and calls handlers.
 * Returns produced changes and a log of each listener invocation for debug logging.
 */
const executeFullExecutionPlan = <DATA extends object>(
  plan: Wasm.FullExecutionPlan | null,
  listenerChanges: Change[],
  store: StoreInstance<DATA>,
): { produced: Change[]; listenerLog: ListenerDispatchTrace[] } => {
  if (!plan || plan.groups.length === 0) {
    return { produced: [], listenerLog: [] }
  }

  const { listenerHandlers } = store._internal.registrations
  const allProducedChanges: Change[] = []
  const listenerLog: ListenerDispatchTrace[] = []
  const extra = new Map<number, Change[]>()
  const timingEnabled = store._internal.config.debug.timing ?? false
  const timingThreshold = store._internal.config.debug.timingThreshold ?? 5

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
    const input = buildDispatchInput(d, listenerChanges, extra)

    // Inline timing — only pay cost of performance.now() when timing is enabled
    const t0 = timingEnabled ? performance.now() : 0
    const result = registration.fn(input, scopedState)
    const durationMs = timingEnabled ? performance.now() - t0 : 0
    const slow = timingEnabled && durationMs > timingThreshold

    if (slow) {
      console.warn(
        `[apex-state] Slow listener: ${registration.name || '(anonymous)'} took ${durationMs.toFixed(2)}ms (threshold: ${timingThreshold}ms)`,
      )
    }

    const producedChanges =
      result && (result as unknown[]).length > 0
        ? normalizeListenerOutput(result as [string, unknown][])
        : []

    listenerLog.push({
      dispatchId: d.dispatch_id,
      subscriberId: d.subscriber_id,
      fnName: registration.name,
      scope: scope || '(root)',
      topic: d.topic_path,
      registrationId: registration.registrationId,
      input: input as [string, unknown, unknown][],
      output: producedChanges,
      currentState: scopedState,
      durationMs,
      slow,
    })

    if (producedChanges.length === 0) continue

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
      const existing = extra.get(subsequent.dispatch_id) ?? []
      extra.set(subsequent.dispatch_id, existing)
      existing.push(...producedChanges)
    }
  }

  return { produced: allProducedChanges, listenerLog }
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
 * INPUT FORMAT: Paths come from WASM as "basePath.concernName" (no _concerns. prefix after partition)
 * - Example: "user.email.validationState" → _concerns["user.email"]["validationState"]
 * - We split on the LAST dot to separate basePath from concernName
 *
 * @param changes - Change[] with paths stripped of _concerns. prefix
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
 * Execute schema validators and return Change[].
 * Takes validators_to_run from WASM, returns concern changes with _concerns. prefix.
 */
const runValidators = (
  validatorsToRun: Wasm.ValidatorDispatch[],
  pipeline: WasmPipeline,
): Change[] => {
  const results: Change[] = []

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

    results.push({
      path: validator.output_path, // Already has _concerns. prefix
      value: validationValue,
      meta: {},
    })
  }

  return results
}

// ---------------------------------------------------------------------------
// Apply helpers
// ---------------------------------------------------------------------------

/** Strip _concerns. prefix from a concern path for applyConcernChanges. */
const stripConcernPrefix = (c: Change): Change => ({
  ...c,
  path: c.path.slice(CONCERNS_PREFIX_LEN),
})

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

export const processChangesWasm = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  store: StoreInstance<DATA>,
  initialChanges: ArrayOfChanges<DATA, META>,
): void => {
  const pipeline = store._internal.pipeline
  const { logger } = store._internal
  const timingEnabled = store._internal.config.debug.timing ?? false

  // Guard: pipeline is null during React StrictMode's simulated unmount/remount gap
  // (dev-only). The first mount executed correctly; the second invocation is idempotent
  // by design, so dropping writes here is correct. Also handles stale post-unmount calls.
  //
  // In production this path is unreachable — Guards 1 & 2 in provider.tsx ensure the
  // pipeline is always initialized before any setValue can fire. The dev warning below
  // catches regressions where pipeline is null for reasons other than StrictMode.
  if (!pipeline) {
    console.warn(
      '[apex-state] processChanges called with no active pipeline. ' +
        'Expected during React StrictMode effect re-mount — ' +
        'if you see this outside StrictMode, it indicates a bug.',
    )
    return
  }

  // Normalize once at entry — Change[] is the single shape for the entire pipeline
  const pipelineChanges = normalizeInputChanges(initialChanges)

  // Initialize debug entry if tracking is enabled
  const trackEntry: DebugTrackEntry | null = store._debug
    ? {
        input: initialChanges.map(([p, v, m]) => [p as string, v, m]),
        applied: [],
        appliedConcerns: [],
        timestamp: Date.now(),
      }
    : null

  const t0 = performance.now()

  const { listener_changes, execution_plan, validators_to_run, has_work } =
    pipeline.processChanges(pipelineChanges)

  // Early exit if WASM signals no work to do
  if (!has_work) {
    if (trackEntry) store._debug!.calls.push(trackEntry)
    return
  }

  // meta_json was serialized onto each change before crossing the WASM boundary —
  // normalizeWasmChanges parses it back so meta flows through unchanged.
  const listenerChanges = normalizeWasmChanges(listener_changes)

  const { produced, listenerLog } = executeFullExecutionPlan(
    execution_plan,
    listenerChanges,
    store,
  )

  // Execute validators (JS-only: schema validation)
  // @FIXME: These should run on-top of listener's provided changes. they need fresh state for validating.
  const validationResults = runValidators(validators_to_run, pipeline)

  // Single flat array: listener output + validator output
  const jsChanges = produced.concat(validationResults)
  const { state_changes: stateChangesFinal, trace: wasmTrace } =
    pipeline.pipelineFinalize(jsChanges)

  // Re-normalize finalized changes back to Change[] (WASM may have merged/reordered)
  const finalChanges = normalizeWasmChanges(stateChangesFinal)

  // Partition by path prefix and apply
  const stateChanges = finalChanges.filter(
    (c) => !c.path.startsWith(CONCERNS_PREFIX),
  )
  const concernChanges = finalChanges
    .filter((c) => c.path.startsWith(CONCERNS_PREFIX))
    .map(stripConcernPrefix)

  if (stateChanges.length > 0) {
    applyBatch(
      stateChanges.map(({ path, value, meta }) => [
        path,
        value,
        meta,
      ]) as ArrayOfChanges<DATA, META>,
      store.state,
    )
  }
  if (concernChanges.length > 0) {
    applyConcernChanges(concernChanges, store._concerns)
  }

  const durationMs = performance.now() - t0

  // Build unified trace (composes WASM trace + JS listener dispatches + wall-clock timing)
  const allDispatches =
    execution_plan?.groups.flatMap((g) => g.dispatches) ?? []
  const unifiedTrace: UnifiedPipelineTrace | null = wasmTrace
    ? {
        wasm: wasmTrace,
        listeners: listenerLog.map((entry, i) => ({
          dispatchId: allDispatches[i]?.dispatch_id ?? i,
          subscriberId: entry.subscriberId,
          fnName: entry.fnName,
          scope: entry.scope,
          topic: allDispatches[i]?.topic_path ?? '',
          registrationId:
            store._internal.registrations.listenerHandlers.get(
              entry.subscriberId,
            )?.registrationId ?? '',
          input: entry.input,
          output: entry.output,
          currentState: entry.currentState,
          durationMs: entry.durationMs,
          slow: entry.slow,
        })),
        totalDurationMs: durationMs,
        wasmDurationMs: wasmTrace.total_duration_us / 1000,
        listenerDurationMs: listenerLog.reduce(
          (sum, e) => sum + e.durationMs,
          0,
        ),
        listenerTimingEnabled: timingEnabled,
      }
    : null

  // Single log call with all pipeline data (no-op when log/devtools is disabled)
  const allApplied = [...stateChanges, ...concernChanges]
  const logData = {
    initialChanges: pipelineChanges,
    trace: unifiedTrace,
    appliedChanges: allApplied,
    stateSnapshot: snapshot(store.state),
  }
  logger.logPipeline(logData)
  store._internal.devtools?.notifyPipeline(logData)

  // Record applied changes for debug tracking
  if (trackEntry) {
    pushDebugChanges(trackEntry.applied, stateChanges)
    pushDebugChanges(trackEntry.appliedConcerns, concernChanges)
    store._debug!.calls.push(trackEntry)
  }
}
