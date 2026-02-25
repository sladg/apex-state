import { useCallback, useLayoutEffect } from 'react'

import { snapshot, useSnapshot } from 'valtio'

import type { ConcernType } from '../concerns'
import { defaultConcerns } from '../concerns'
import { registerConcernEffects as registerConcernEffectsLegacy } from '../concerns/registration'
import { registerConcernEffects as registerConcernEffectsWasm } from '../concerns/registration.wasm-impl'
import { useStoreContext } from '../core/context'
import type { ProviderProps, StoreConfig } from '../core/types'
import { processChangesLegacy } from '../pipeline/process-changes'
import { processChangesWasm } from '../pipeline/process-changes.wasm-impl'
import { registerSideEffects as registerSideEffectsLegacy } from '../sideEffects/registration'
import { registerSideEffects as registerSideEffectsWasm } from '../sideEffects/registration.wasm-impl'
import type {
  ArrayOfChanges,
  BoolLogic,
  CheckAggregationPairs,
  CheckComputationPairs,
  CheckListeners,
  CheckSyncPairs,
  ComputationOp,
  ConcernRegistrationMap,
  DeepKey,
  DeepValue,
  EvaluatedConcerns,
  GenericMeta,
  ResolvableDeepKey,
  ValidatedAggregationPairs,
  ValidatedComputationPairs,
  ValidatedFlipPairs,
  ValidatedListeners,
  ValidatedSyncPairs,
} from '../types'
import type { DeepKeyFiltered } from '../types/deep-key-filtered'
import type { SideEffects } from '../types/side-effects'
import { dot } from '../utils/dot'
import { createProvider } from './provider'
import { createWarmPairHelpers } from './warm-pair-helpers'

/**
 * Explicit return type of createGenericStore.
 *
 * Defined as an interface so TypeScript references type aliases by name
 * (e.g. `SideEffects<DATA, META>`) instead of expanding them inline.
 * Without this, large DATA types (2800+ DeepKey paths) cause
 * "inferred type exceeds the maximum length" (TS error) because the
 * compiler tries to inline-expand every O(N²) pair type.
 */
export interface GenericStoreApi<
  DATA extends object,
  META extends GenericMeta = GenericMeta,
  CONCERNS extends readonly ConcernType<string, any, any>[] =
    typeof defaultConcerns,
> {
  Provider: (props: ProviderProps<DATA>) => React.JSX.Element

  useFieldStore: <P extends DeepKey<DATA>>(
    path: P,
  ) => {
    value: DeepValue<DATA, P>
    setValue: (newValue: DeepValue<DATA, P>, meta?: META) => void
  } & Record<string, unknown>

  useStore: <P extends DeepKey<DATA>>(
    path: P,
  ) => [DeepValue<DATA, P>, (value: DeepValue<DATA, P>, meta?: META) => void]

  useJitStore: () => {
    proxyValue: DATA
    setChanges: (changes: ArrayOfChanges<DATA, META>) => void
    getState: () => DATA
  }

  useSideEffects: (id: string, effects: SideEffects<DATA, META>) => void

  useConcerns: <
    CUSTOM extends readonly ConcernType<string, any, any>[] = readonly [],
  >(
    id: string,
    registration: ConcernRegistrationMap<
      DATA,
      readonly [...CONCERNS, ...CUSTOM]
    >,
    customConcerns?: CUSTOM,
  ) => void

  withConcerns: <
    SELECTION extends Partial<
      Record<Extract<CONCERNS[number], { name: string }>['name'], boolean>
    >,
  >(
    selection: SELECTION,
  ) => {
    useFieldStore: <P extends DeepKey<DATA>>(
      path: P,
    ) => {
      value: DeepValue<DATA, P>
      setValue: (newValue: DeepValue<DATA, P>, meta?: META) => void
    } & {
      [K in keyof SELECTION as SELECTION[K] extends true
        ? K
        : never]?: K extends keyof EvaluatedConcerns<CONCERNS>
        ? EvaluatedConcerns<CONCERNS>[K]
        : never
    }
  }

  withMeta: (presetMeta: Partial<META>) => {
    useFieldStore: <P extends DeepKey<DATA>>(
      path: P,
    ) => {
      value: DeepValue<DATA, P>
      setValue: (newValue: DeepValue<DATA, P>, meta?: META) => void
    }
  }

  // Pre-warmed pair helpers from createWarmPairHelpers
  syncPairs: <T extends [ResolvableDeepKey<DATA>, ResolvableDeepKey<DATA>][]>(
    pairs: CheckSyncPairs<DATA, T>,
  ) => ValidatedSyncPairs<DATA>

  flipPairs: <T extends [ResolvableDeepKey<DATA>, ResolvableDeepKey<DATA>][]>(
    pairs: CheckSyncPairs<DATA, T>,
  ) => ValidatedFlipPairs<DATA>

  aggregationPairs: <
    T extends (
      | [ResolvableDeepKey<DATA>, ResolvableDeepKey<DATA>]
      | [ResolvableDeepKey<DATA>, ResolvableDeepKey<DATA>, BoolLogic<DATA>]
    )[],
  >(
    pairs: CheckAggregationPairs<DATA, T>,
  ) => ValidatedAggregationPairs<DATA>

  computationPairs: <
    T extends (
      | [
          ComputationOp,
          DeepKeyFiltered<DATA, number>,
          DeepKeyFiltered<DATA, number>,
        ]
      | [
          ComputationOp,
          DeepKeyFiltered<DATA, number>,
          DeepKeyFiltered<DATA, number>,
          BoolLogic<DATA>,
        ]
    )[],
  >(
    pairs: CheckComputationPairs<DATA, T>,
  ) => ValidatedComputationPairs<DATA>

  listeners: <
    T extends readonly {
      path: ResolvableDeepKey<DATA> | null
      scope?: ResolvableDeepKey<DATA> | null | undefined
      fn: (...args: any[]) => any
    }[],
  >(
    items: CheckListeners<DATA, META, T>,
  ) => ValidatedListeners<DATA, META>
}

export const createGenericStore = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
  CONCERNS extends readonly ConcernType<string, any, any>[] =
    typeof defaultConcerns,
>(
  config?: StoreConfig,
): GenericStoreApi<DATA, META, CONCERNS> => {
  const Provider = createProvider<DATA, META>(config)

  // Internal helper hook for field state access
  const _useFieldValue = <P extends DeepKey<DATA>>(path: P) => {
    const store = useStoreContext<DATA, META>()
    const snap = useSnapshot(store.state) as DATA
    const value = dot.get(snap, path)

    const setValue = useCallback(
      (newValue: DeepValue<DATA, P>, meta?: META) => {
        const changes: ArrayOfChanges<DATA, META> = [
          [path, newValue, (meta || {}) as META],
        ]

        const processChanges = store._internal.config.useLegacyImplementation
          ? processChangesLegacy
          : processChangesWasm

        processChanges(store, changes)
      },
      [store, path],
    )

    return { store, value, setValue }
  }

  const useFieldStore = <P extends DeepKey<DATA>>(
    path: P,
  ): {
    value: DeepValue<DATA, P>
    setValue: (newValue: DeepValue<DATA, P>, meta?: META) => void
  } & Record<string, unknown> => {
    const { store, value, setValue } = _useFieldValue(path)
    const concernsSnap = useSnapshot(store._concerns)
    const allConcerns = (concernsSnap[path] || {}) as Record<string, unknown>

    return { value, setValue, ...allConcerns }
  }

  const useStore = <P extends DeepKey<DATA>>(
    path: P,
  ): [DeepValue<DATA, P>, (value: DeepValue<DATA, P>, meta?: META) => void] => {
    const { value, setValue } = _useFieldValue(path)
    return [value, setValue]
  }

  const useJitStore = (): {
    proxyValue: DATA
    setChanges: (changes: ArrayOfChanges<DATA, META>) => void
    getState: () => DATA
  } => {
    const store = useStoreContext<DATA, META>()
    const proxyValue = useSnapshot(store.state) as DATA

    const setChanges = useCallback(
      (changes: ArrayOfChanges<DATA, META>) => {
        // WASM gateway: dispatch to WASM or legacy implementation
        const processChanges = store._internal.config.useLegacyImplementation
          ? processChangesLegacy
          : processChangesWasm

        processChanges(store, changes)
      },
      [store],
    )

    const getState = useCallback(() => {
      return snapshot(store.state) as DATA
    }, [store])

    return { proxyValue, setChanges, getState }
  }

  const useSideEffects = (
    id: string,
    effects: SideEffects<DATA, META>,
  ): void => {
    const store = useStoreContext<DATA, META>()
    useLayoutEffect(() => {
      // WASM gateway: dispatch to WASM or legacy implementation
      const registerSideEffects = store._internal.config.useLegacyImplementation
        ? registerSideEffectsLegacy
        : registerSideEffectsWasm

      return registerSideEffects(store, id, effects)
    }, [store, id, effects])
  }

  // Explicit type annotation from GenericStoreApi — TS can't structurally verify
  // generic function assignability for this method (contravariant generic params).
  // Typing the variable directly ensures the return type annotation works without `as`.
  const useConcerns: GenericStoreApi<DATA, META, CONCERNS>['useConcerns'] = ((
    id: string,
    registration: ConcernRegistrationMap<
      DATA,
      readonly [...CONCERNS, ...any[]]
    >,
    customConcerns?: readonly ConcernType<string, any, any>[],
  ): void => {
    const store = useStoreContext<DATA, META>()
    const concerns = (customConcerns ||
      defaultConcerns) as readonly ConcernType<any, any, any>[]

    useLayoutEffect(() => {
      // WASM gateway: dispatch to WASM or legacy implementation
      const registerConcernEffects = store._internal.config
        .useLegacyImplementation
        ? registerConcernEffectsLegacy
        : registerConcernEffectsWasm

      return registerConcernEffects(store, registration, concerns)
    }, [store, id, registration, customConcerns])
  }) as GenericStoreApi<DATA, META, CONCERNS>['useConcerns']

  // Explicit type annotation from GenericStoreApi — same reason as useConcerns.
  const withConcerns: GenericStoreApi<DATA, META, CONCERNS>['withConcerns'] = (<
    SELECTION extends Partial<
      Record<Extract<CONCERNS[number], { name: string }>['name'], boolean>
    >,
  >(
    selection: SELECTION,
  ) => {
    type SelectedConcerns = {
      [K in keyof SELECTION as SELECTION[K] extends true
        ? K
        : never]?: K extends keyof EvaluatedConcerns<CONCERNS>
        ? EvaluatedConcerns<CONCERNS>[K]
        : never
    }

    type WithConcernsFieldStore<P extends DeepKey<DATA>> = {
      value: DeepValue<DATA, P>
      setValue: (newValue: DeepValue<DATA, P>, meta?: META) => void
    } & SelectedConcerns

    return {
      useFieldStore: <P extends DeepKey<DATA>>(
        path: P,
      ): WithConcernsFieldStore<P> => {
        const { store, value, setValue } = _useFieldValue(path)
        const concernsSnap = useSnapshot(store._concerns)
        const allConcerns = concernsSnap[path] || {}

        const selectedConcerns = Object.keys(selection).reduce(
          (acc, key) => {
            if (
              (selection as Record<string, boolean | undefined>)[key] &&
              Object.prototype.hasOwnProperty.call(allConcerns, key)
            ) {
              acc[key] = allConcerns[key]
            }
            return acc
          },
          {} as Record<string, unknown>,
        ) as SelectedConcerns

        return { value, setValue, ...selectedConcerns }
      },
    }
  }) as GenericStoreApi<DATA, META, CONCERNS>['withConcerns']

  const withMeta = (presetMeta: Partial<META>) => ({
    useFieldStore: <P extends DeepKey<DATA>>(
      path: P,
    ): {
      value: DeepValue<DATA, P>
      setValue: (newValue: DeepValue<DATA, P>, meta?: META) => void
    } => {
      const { store, value, setValue: originalSetValue } = _useFieldValue(path)
      const concernsSnap = useSnapshot(store._concerns)
      const allConcerns = (concernsSnap[path] || {}) as Record<string, unknown>

      const setValue = useCallback(
        (newValue: DeepValue<DATA, P>, meta?: META) => {
          originalSetValue(newValue, { ...presetMeta, ...meta } as META)
        },
        [originalSetValue],
      )

      return { value, setValue, ...allConcerns }
    },
  })

  return {
    Provider,
    useFieldStore,
    useStore,
    useJitStore,
    useSideEffects,
    useConcerns,
    withConcerns,
    withMeta,
    ...createWarmPairHelpers<DATA, META>(),
  }
}
