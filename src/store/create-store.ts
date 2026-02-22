import { useCallback, useLayoutEffect } from 'react'

import { snapshot, useSnapshot } from 'valtio'

import type { ConcernType } from '../concerns'
import { defaultConcerns } from '../concerns'
import { registerConcernEffects as registerConcernEffectsLegacy } from '../concerns/registration'
import { registerConcernEffects as registerConcernEffectsWasm } from '../concerns/registration.wasm-impl'
import { useStoreContext } from '../core/context'
import type { StoreConfig } from '../core/types'
import { processChangesLegacy } from '../pipeline/process-changes'
import { processChangesWasm } from '../pipeline/process-changes.wasm-impl'
import { registerSideEffects as registerSideEffectsLegacy } from '../sideEffects/registration'
import { registerSideEffects as registerSideEffectsWasm } from '../sideEffects/registration.wasm-impl'
import type {
  ArrayOfChanges,
  ConcernRegistrationMap,
  DeepKey,
  DeepValue,
  EvaluatedConcerns,
  GenericMeta,
} from '../types'
import type { SideEffects } from '../types/side-effects'
import { dot } from '../utils/dot'
import { createProvider } from './provider'

export const createGenericStore = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
  CONCERNS extends readonly ConcernType<string, any, any>[] =
    typeof defaultConcerns,
>(
  config?: StoreConfig,
) => {
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

  const useConcerns = <
    CUSTOM extends readonly ConcernType<string, any, any>[] = readonly [],
  >(
    id: string,
    registration: ConcernRegistrationMap<
      DATA,
      readonly [...CONCERNS, ...CUSTOM]
    >,
    customConcerns?: CUSTOM,
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
  }

  const withConcerns = <
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
  }

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
  }
}

/** Return type of createGenericStore — used by testing mock for 1:1 type safety */
export type GenericStoreApi<
  DATA extends object,
  META extends GenericMeta = GenericMeta,
  CONCERNS extends readonly ConcernType<string, any, any>[] =
    typeof defaultConcerns,
> = ReturnType<typeof createGenericStore<DATA, META, CONCERNS>>
