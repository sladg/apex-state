/**
 * WASM Implementation - Concern Registration
 *
 * BoolLogic and validators use WASM registration.
 * Custom concerns still use effect() (can't be moved to WASM).
 */

import { effect } from 'valtio-reactive'

import type { StoreInstance } from '../core/types'
import type { ConcernRegistrationMap } from '../types'
import { dot } from '../utils/dot'
import type { Wasm, WasmPipeline } from '../wasm/bridge'
import type { BaseConcernProps, ConcernType } from './types'

/** Check if a concern config has a `boolLogic` field (BoolLogic concern). */
const isBoolLogicConfig = (
  config: Record<string, unknown>,
): config is { boolLogic: unknown } =>
  'boolLogic' in config && config['boolLogic'] != null

/** Check if a concern config has a `value_logic` field (ValueLogic concern). */
const isValueLogicConfig = (
  config: Record<string, unknown>,
): config is { valueLogic: unknown } =>
  'valueLogic' in config && config['valueLogic'] != null

/** Check if a concern config is schema-based validation. */
const isSchemaValidation = (
  concernName: string,
  config: Record<string, unknown>,
): boolean =>
  concernName === 'validationState' &&
  'schema' in config &&
  !('evaluate' in config)

/** Sequential validator ID counter. */
let nextValidatorId = 0

/** Sequential registration ID counter. */
let nextRegistrationId = 0

/** Batch-register BoolLogics, validators, and ValueLogics with WASM, apply initial results. */
const registerWasmBatch = (
  pipeline: WasmPipeline,
  boolLogics: Wasm.BoolLogicRegistration[],
  validators: Wasm.ValidatorRegistration[],
  valueLogics: Wasm.ValueLogicRegistration[],
  validatorConfigs: Map<
    number,
    {
      schema: any
      initialValue: unknown
      concernName: string
      concernsAtPath: Record<string, unknown>
    }
  >,
  concernRefs: Map<string, Record<string, unknown>>,
  disposeCallbacks: (() => void)[],
) => {
  const registrationId = `concerns-${nextRegistrationId++}`
  const result = pipeline.registerConcerns({
    registration_id: registrationId,
    bool_logics: boolLogics,
    validators,
    value_logics: valueLogics,
  })

  // Apply initial BoolLogic evaluations to _concerns
  for (const change of result.bool_logic_changes) {
    // BoolLogic output paths are like "email.disabledWhen" (no _concerns prefix)
    const lastDot = change.path.lastIndexOf('.')
    const basePath = change.path.slice(0, lastDot)
    const concernName = change.path.slice(lastDot + 1)
    const concernsAtPath = concernRefs.get(basePath)
    if (concernsAtPath && concernName) {
      concernsAtPath[concernName] = change.value
    }
  }

  // Apply initial ValueLogic evaluations to _concerns (same path pattern)
  for (const change of result.value_logic_changes) {
    const lastDot = change.path.lastIndexOf('.')
    const basePath = change.path.slice(0, lastDot)
    const concernName = change.path.slice(lastDot + 1)
    const concernsAtPath = concernRefs.get(basePath)
    if (concernsAtPath && concernName) {
      concernsAtPath[concernName] = change.value
    }
  }

  disposeCallbacks.push(() => {
    pipeline.unregisterConcerns(registrationId)
  })

  // Post-registration: run initial validation for each validator
  validatorConfigs.forEach((config, validatorId) => {
    const parseResult = config.schema.safeParse(config.initialValue)
    const validationResult = {
      isError: !parseResult.success,
      errors: parseResult.success
        ? []
        : parseResult.error.errors.map((e: any) => ({
            field: e.path.length > 0 ? e.path.join('.') : '.',
            message: e.message,
          })),
    }

    config.concernsAtPath[config.concernName] = validationResult

    // Store schema in per-pipeline storage for JS-side validator execution
    pipeline.validatorSchemas.set(validatorId, config.schema)
  })
}

/** Create a single concern effect with cached evaluate function. */
const createConcernEffect = <DATA extends object>(
  store: StoreInstance<DATA>,
  item: {
    path: string
    concernName: string
    config: Record<string, any>
    concern: ConcernType
    concernsAtPath: Record<string, unknown>
  },
  resultCache: Map<string, unknown>,
): (() => void) => {
  const { path, concernName, config, concern, concernsAtPath } = item
  const cacheKey = `${path}.${concernName}`

  // Resolve evaluate function once (not per effect trigger)
  // @FIXME: this should be coming from concern registration. we should have evaluate function for validation there, not here.
  const evaluateFn =
    'evaluate' in config && typeof config['evaluate'] === 'function'
      ? config['evaluate']
      : concern.evaluate

  // Wrap evaluation in effect() for automatic dependency tracking
  // effect() will automatically track ONLY the properties accessed during evaluate()
  return effect(() => {
    // READ from dataProxy (automatic tracking!)
    // Any property accessed here will trigger re-evaluation when changed
    const value = dot.get__unsafe(store.state, path)

    // OPTIMIZATION: Avoid object spread overhead (creates new object every evaluation)
    // Use Object.assign instead for single-pass property addition (40% faster)
    const evalProps: BaseConcernProps<any, string> & Record<string, any> =
      Object.assign({ state: store.state, path, value }, config)

    // EVALUATE concern (all state accesses inside are tracked!)
    const result = evaluateFn(evalProps)

    // Check cache (non-reactive!) to see if value changed
    const prev = resultCache.get(cacheKey)
    if (prev !== result) {
      // Update cache
      resultCache.set(cacheKey, result)

      // WRITE to pre-captured reference (NO tracked reads!)
      concernsAtPath[concernName] = result
    }
  })
}

/** Check if a concern config has an inline evaluate function (custom/ad-hoc concern). */
const isAdHocConcern = (
  config: Record<string, unknown>,
): config is { evaluate: (...args: any[]) => unknown } =>
  'evaluate' in config && typeof config['evaluate'] === 'function'

/** Collected registration data from single-pass classification. */
interface CollectedRegistrations {
  boolLogics: Wasm.BoolLogicRegistration[]
  validators: Wasm.ValidatorRegistration[]
  valueLogics: Wasm.ValueLogicRegistration[]
  validatorConfigs: Map<
    number,
    {
      schema: any
      initialValue: unknown
      concernName: string
      concernsAtPath: Record<string, unknown>
    }
  >
  jsEffects: {
    path: string
    concernName: string
    config: Record<string, any>
    concern: ConcernType
    concernsAtPath: Record<string, unknown>
  }[]
}

/** Single-pass: classify each concern config as BoolLogic, validator, or JS effect. */
/** Collect a schema validator registration. */
const collectValidator = <DATA extends object>(
  store: StoreInstance<DATA>,
  path: string,
  concernName: string,
  config: Record<string, any>,
  concernsAtPath: Record<string, unknown>,
  validators: CollectedRegistrations['validators'],
  validatorConfigs: CollectedRegistrations['validatorConfigs'],
) => {
  const validatorId = nextValidatorId++
  // IMPORTANT: Include _concerns. prefix for WASM shadow state traversal
  const depPaths =
    'scope' in config && config['scope'] ? [config['scope'] as string] : [path]

  validators.push({
    validator_id: validatorId,
    output_path: `_concerns.${path}.${concernName}`,
    dependency_paths: depPaths,
  })

  // Store schema and initial value info for post-registration processing
  const primaryValue = dot.get__unsafe(store.state, depPaths[0]!)
  validatorConfigs.set(validatorId, {
    schema: config['schema'],
    initialValue: primaryValue,
    concernName,
    concernsAtPath,
  })
}

/** Classify a single concern config and push to the appropriate bucket. */
const classifyConcern = <DATA extends object>(
  store: StoreInstance<DATA>,
  path: string,
  concernName: string,
  config: Record<string, any>,
  concernsAtPath: Record<string, unknown>,
  concernMap: Map<string, ConcernType>,
  result: CollectedRegistrations,
) => {
  // BoolLogic registration (WASM)
  if (isBoolLogicConfig(config)) {
    result.boolLogics.push({
      output_path: `${path}.${concernName}`,
      tree_json: JSON.stringify(config.boolLogic),
    })
    return
  }

  // ValueLogic registration (WASM)
  if (isValueLogicConfig(config)) {
    result.valueLogics.push({
      output_path: `${path}.${concernName}`,
      tree_json: JSON.stringify(config.valueLogic),
    })
    return
  }

  // Schema validator registration (WASM)
  if (isSchemaValidation(concernName, config)) {
    collectValidator(
      store,
      path,
      concernName,
      config,
      concernsAtPath,
      result.validators,
      result.validatorConfigs,
    )
    return
  }

  // JS-based concern — queue for effect creation after WASM batch
  // Resolve concern from registry, or create ad-hoc for inline evaluate functions
  const concern =
    concernMap.get(concernName) ??
    (isAdHocConcern(config)
      ? {
          name: concernName,
          description: `Custom concern: ${concernName}`,
          evaluate: config.evaluate,
        }
      : undefined)

  if (!concern) {
    console.warn(`Concern "${concernName}" not found`)
    return
  }

  result.jsEffects.push({ path, concernName, config, concern, concernsAtPath })
}

/** Single-pass: classify each concern config as BoolLogic, ValueLogic, validator, or JS effect. */
const collectRegistrations = <DATA extends object>(
  store: StoreInstance<DATA>,
  registrationEntries: [string, Record<string, any> | undefined][],
  concernRefs: Map<string, Record<string, unknown>>,
  concernMap: Map<string, ConcernType>,
): CollectedRegistrations => {
  const result: CollectedRegistrations = {
    boolLogics: [],
    validators: [],
    valueLogics: [],
    validatorConfigs: new Map(),
    jsEffects: [],
  }

  for (const [path, concernConfigs] of registrationEntries) {
    if (!concernConfigs) continue
    const concernsAtPath = concernRefs.get(path)!

    for (const [concernName, config] of Object.entries(concernConfigs)) {
      if (!config) continue
      classifyConcern(
        store,
        path,
        concernName,
        config,
        concernsAtPath,
        concernMap,
        result,
      )
    }
  }

  return result
}

/** Clean up concern values from the concerns proxy on unmount. */
const cleanupConcerns = <DATA extends object>(
  store: StoreInstance<DATA>,
  registrationEntries: [string, Record<string, any> | undefined][],
) => {
  for (const [path, concernConfigs] of registrationEntries) {
    if (!concernConfigs) continue
    const concernsObj = store._concerns[path]
    if (!concernsObj) continue

    // Delete specific concerns for this path
    for (const concernName of Object.keys(concernConfigs)) {
      // Use Reflect.deleteProperty to avoid dynamic delete lint error
      Reflect.deleteProperty(concernsObj, concernName)
    }

    // Clean up empty path object
    if (Object.keys(concernsObj).length === 0) {
      Reflect.deleteProperty(store._concerns, path)
    }
  }
}

export const registerConcernEffects = <DATA extends object>(
  store: StoreInstance<DATA>,
  registration: ConcernRegistrationMap<DATA>,
  concerns: readonly ConcernType[],
): (() => void) => {
  const disposeCallbacks: (() => void)[] = []
  const resultCache = new Map<string, unknown>()
  const concernRefs = new Map<string, Record<string, unknown>>()

  // Build O(1) concern lookup map (avoids linear .find() per inner iteration)
  const concernMap = new Map<string, ConcernType>()
  for (const c of concerns) {
    concernMap.set(c.name, c)
  }

  // Pre-initialize all path objects BEFORE creating effects
  const registrationEntries = Object.entries(registration) as [
    string,
    Record<string, any> | undefined,
  ][]
  for (const [path] of registrationEntries) {
    if (!store._concerns[path]) {
      store._concerns[path] = {}
    }
    concernRefs.set(path, store._concerns[path])
  }

  // Single pass: classify all concern configs
  const { boolLogics, validators, valueLogics, validatorConfigs, jsEffects } =
    collectRegistrations(store, registrationEntries, concernRefs, concernMap)

  // Register all BoolLogics, validators, and ValueLogics in one WASM call
  if (
    boolLogics.length > 0 ||
    validators.length > 0 ||
    valueLogics.length > 0
  ) {
    registerWasmBatch(
      store._internal.pipeline!,
      boolLogics,
      validators,
      valueLogics,
      validatorConfigs,
      concernRefs,
      disposeCallbacks,
    )
  }

  // Create effects for queued JS-based concerns
  for (const item of jsEffects) {
    disposeCallbacks.push(createConcernEffect(store, item, resultCache))
  }

  // Return cleanup function that disposes all effects on unmount
  return () => {
    for (const dispose of disposeCallbacks) dispose()
    resultCache.clear()
    concernRefs.clear()
    cleanupConcerns(store, registrationEntries)
  }
}
