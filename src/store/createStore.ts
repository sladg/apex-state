import { useCallback, useLayoutEffect } from 'react'

import { snapshot, useSnapshot } from 'valtio'

import type { ConcernType } from '../concerns'
import { defaultConcerns, registerConcernEffects } from '../concerns'
import { useStoreContext } from '../core/context'
import type { StoreConfig } from '../core/types'
import { processChanges } from '../pipeline/processChanges'
import { registerSideEffects } from '../sideEffects'
import type {
  ArrayOfChanges,
  DeepKey,
  DeepValue,
  EvaluatedConcerns,
  GenericMeta,
} from '../types'
import type { SideEffects } from '../types/sideEffects'
import { dot } from '../utils/dot'
import { createProvider } from './Provider'

export const createGenericStore = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
  CONCERNS extends readonly any[] = typeof defaultConcerns,
>(
  _config?: StoreConfig,
) => {
  const Provider = createProvider<DATA, META>()

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
  } => {
    const { value, setValue } = _useFieldValue(path)
    return { value, setValue }
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
      return registerSideEffects(store, id, effects)
    }, [store, id, effects])
  }

  const useConcerns = (
    id: string,
    registration: Partial<Record<DeepKey<DATA>, Partial<Record<string, any>>>>,
    customConcerns?: readonly ConcernType<any, any>[],
  ): void => {
    const store = useStoreContext<DATA, META>()
    const concerns = (customConcerns ||
      defaultConcerns) as readonly ConcernType<any, any>[]

    useLayoutEffect(() => {
      return registerConcernEffects(store, registration, concerns)
    }, [store, id, registration, customConcerns])
  }

  const withConcerns = <SELECTION extends Partial<Record<string, boolean>>>(
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
              selection[key] &&
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

  return {
    Provider,
    useFieldStore,
    useStore,
    useJitStore,
    useSideEffects,
    useConcerns,
    withConcerns,
  }
}
