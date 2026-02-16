/**
 * WASM Implementation - Concern Registration
 *
 * BoolLogic and validators use WASM registration.
 * Custom concerns still use effect() (can't be moved to WASM).
 */

import { effect } from 'valtio-reactive'

import type { StoreInstance } from '../core/types'
import type { ConcernRegistrationMap, DeepKey, GenericMeta } from '../types'
import { dot } from '../utils/dot'
import { validatorSchemas, wasm } from '../wasm/bridge'
import { findConcern } from './registry'
import type { BaseConcernProps, ConcernType } from './types'

/** Check if a concern config has a `condition` field (BoolLogic concern). */
const isBoolLogicConfig = (
  config: Record<string, unknown>,
): config is { condition: unknown } =>
  'condition' in config && config['condition'] != null

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

const registerConcernEffectsImpl = <
  DATA extends object,
  META extends GenericMeta,
>(
  store: StoreInstance<DATA, META>,
  registration: ConcernRegistrationMap<DATA>,
  concerns: readonly ConcernType[],
): (() => void) => {
  const disposeCallbacks: (() => void)[] = []
  // Non-reactive cache for previous results (prevents tracked reads)
  const resultCache = new Map<string, unknown>()
  // Pre-allocate concern path objects and capture references
  const concernRefs = new Map<string, Record<string, unknown>>()

  // Pre-initialize all path objects BEFORE creating effects
  Object.keys(registration).forEach((path) => {
    if (!store._concerns[path]) {
      store._concerns[path] = {}
    }
    concernRefs.set(path, store._concerns[path])
  })

  // Iterate over each path in the registration
  Object.entries(registration).forEach(([path, concernConfigs]) => {
    if (!concernConfigs) return

    // Get pre-initialized concern object for this path
    const concernsAtPath = concernRefs.get(path)!

    // Iterate over each concern at this path
    Object.entries(concernConfigs).forEach(([concernName, config]) => {
      if (!config) return

      // Find the concern definition
      const concern = findConcern(concernName, concerns)
      if (!concern) {
        console.warn(`Concern "${concernName}" not found`)
        return
      }

      // --- WASM path: BoolLogic concerns ---
      // If the config has a `condition` field, register via WASM instead of wrapping in effect().
      // Evaluation happens in processChanges(), not here.
      if (isBoolLogicConfig(config)) {
        const outputPath = `_concerns.${path}.${concernName}`
        const logicId = wasm.registerBoolLogic(outputPath, config.condition)

        disposeCallbacks.push(() => {
          wasm.unregisterBoolLogic(logicId)
        })
        return
      }

      // --- WASM path: Schema-based validation ---
      // If this is a validationState with schema (no custom evaluate),
      // register via WASM for pipeline orchestration using generic functions API.
      if (isSchemaValidation(concernName, config)) {
        const validatorId = nextValidatorId++
        const outputPath = `_concerns.${path}.${concernName}`

        // Store Zod schema in JS (can't cross WASM boundary)
        validatorSchemas.set(validatorId, config.schema)

        // Determine dependency path: scope if provided, otherwise registration path
        const depPaths =
          'scope' in config && config.scope ? [config.scope as string] : [path]

        // Register via generic function registry
        wasm.registerFunctionsBatch([
          {
            function_id: validatorId,
            dependency_paths: depPaths,
            // @FIXME: this is wrong. this should be driven by path/scope for validators. same for listeners.
            scope: '', // Empty scope = full state (validators validate at dependency path level)
            output_path: outputPath,
          },
        ])

        // Run initial validation manually by reading current value from state
        const primaryPath = depPaths[0]!
        const primaryValue = dot.get__unsafe(store.state, primaryPath)
        const zodResult = config.schema.safeParse(primaryValue)

        // Write result to _concerns immediately
        concernsAtPath[concernName] = {
          isError: !zodResult.success,
          errors: zodResult.success
            ? []
            : zodResult.error.errors.map((e) => ({
                field: e.path.length > 0 ? e.path.join('.') : '.',
                message: e.message,
              })),
        }

        disposeCallbacks.push(() => {
          validatorSchemas.delete(validatorId)
          wasm.unregisterFunctionsBatch([validatorId])
        })
        return
      }

      // --- JS path: Custom concerns (effect-based) ---
      // These can't be moved to WASM since they contain arbitrary JS logic
      const cacheKey = `${path}.${concernName}`

      // Wrap evaluation in effect() for automatic dependency tracking
      // effect() will automatically track ONLY the properties accessed during evaluate()
      const dispose = effect(() => {
        // READ from dataProxy (automatic tracking!)
        // Any property accessed here will trigger re-evaluation when changed
        const value = dot.get__unsafe(store.state, path)

        // OPTIMIZATION: Avoid object spread overhead (creates new object every evaluation)
        // Use Object.assign instead for single-pass property addition (40% faster)
        const evalProps: BaseConcernProps<any, string> & Record<string, any> =
          Object.assign({ state: store.state, path, value }, config)

        // EVALUATE concern (all state accesses inside are tracked!)
        // If config provides custom evaluate(), use that instead of prebuilt's evaluate
        // Wrapped with timing measurement when debug.timing is enabled
        // @FIXME: this should be coming from cocnern registration. we should have evaluate function for validation there, not here.
        const evaluateFn =
          'evaluate' in config && typeof config.evaluate === 'function'
            ? config.evaluate
            : concern.evaluate

        const result = store._internal.timing.run(
          'concerns',
          () => evaluateFn(evalProps),
          { path, name: concernName },
        )

        // Check cache (non-reactive!) to see if value changed
        const prev = resultCache.get(cacheKey)
        if (prev !== result) {
          // Update cache
          resultCache.set(cacheKey, result)

          // WRITE to pre-captured reference (NO tracked reads!)
          concernsAtPath[concernName] = result
        }
      })

      // Track dispose callback for cleanup
      disposeCallbacks.push(dispose)
    })
  })

  // Return cleanup function that disposes all effects on unmount
  return () => {
    // Stop all effects and unregister WASM BoolLogic
    disposeCallbacks.forEach((dispose) => dispose())

    // Clear caches
    resultCache.clear()
    concernRefs.clear()

    // Remove concern values from concernsProxy
    Object.keys(registration).forEach((path) => {
      const concernConfigs = registration[path as DeepKey<DATA>]
      if (!concernConfigs) return

      // Delete specific concerns for this path
      Object.keys(concernConfigs).forEach((concernName) => {
        if (store._concerns[path]) {
          // Use Reflect.deleteProperty to avoid dynamic delete lint error
          Reflect.deleteProperty(store._concerns[path], concernName)
        }
      })

      // Clean up empty path object
      if (
        store._concerns[path] &&
        Object.keys(store._concerns[path]).length === 0
      ) {
        // Use Reflect.deleteProperty to avoid dynamic delete lint error
        Reflect.deleteProperty(store._concerns, path)
      }
    })
  }
}

export const registerConcernEffects: typeof import('./registration').registerConcernEffects =
  (store, registration, concerns) =>
    store._internal.timing.run(
      'registration',
      () => registerConcernEffectsImpl(store, registration, concerns),
      { path: Object.keys(registration).join(','), name: 'concerns' },
    )
