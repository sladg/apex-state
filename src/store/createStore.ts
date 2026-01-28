/**
 * Core store factory
 *
 * Creates a store instance with valtio proxy and React Provider component.
 *
 * Architecture: Unified _internal pattern
 * - state: User data (tracked by valtio)
 * - _concerns: Computed concern values (tracked by valtio)
 * - _internal: Graphs, registrations, processing queue (NOT tracked - wrapped in ref())
 */

import { useCallback, useEffect, useLayoutEffect, useState } from 'react'

import { snapshot, useSnapshot } from 'valtio'

import { defaultConcerns, registerConcernEffects } from '../concerns'
import { useStoreContext } from '../hooks/useStoreContext'
import { registerSideEffects } from '../sideEffects/registration'
import type {
  ArrayOfChanges,
  DeepKey,
  DeepValue,
  EvaluatedConcerns,
  FieldTransformConfig,
  GenericMeta,
} from '../types'
import type { SideEffects } from '../types/sideEffects'
import { processChanges } from './executor'
import { createProvider } from './Provider'
import type { StoreConfig } from './types'
import { deepGet } from './utils/deepAccess'

/**
 * Creates a generic store with valtio proxy and React context
 *
 * This is a factory function that returns a configured store with:
 * - Valtio proxy for reactive state
 * - _concerns proxy for computed values
 * - _internal (non-tracked) for graphs and processing
 * - React Provider component for context
 *
 * @example
 * ```typescript
 * type AppState = {
 *   user: { name: string }
 *   count: number
 * }
 *
 * const store = createGenericStore<AppState>()
 *
 * function App() {
 *   return (
 *     <store.Provider initialState={{ user: { name: 'Alice' }, count: 0 }}>
 *       <YourApp />
 *     </store.Provider>
 *   )
 * }
 * ```
 *
 * @param config - Optional configuration (errorStorePath, maxIterations, etc.)
 * @returns Store object with Provider component and hooks
 */
export const createGenericStore = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
  CONCERNS extends readonly any[] = typeof defaultConcerns,
>(
  config?: StoreConfig,
) => {
  // Merge config with defaults (used in Provider creation)
  const _resolvedConfig = {
    errorStorePath: config?.errorStorePath ?? '_errors',
    maxIterations: config?.maxIterations ?? 100,
  }

  // Create the Provider component for this store
  const Provider = createProvider<DATA, META>()

  return {
    Provider,

    /**
     * useState-like hook for accessing and updating specific paths
     */
    useStore: <P extends DeepKey<DATA>>(
      path: P,
    ): [
      DeepValue<DATA, P>,
      (value: DeepValue<DATA, P>, meta?: META) => void,
    ] => {
      const store = useStoreContext<DATA, META>()
      const snap = useSnapshot(store.state) as DATA
      const value = deepGet(snap, path) as DeepValue<DATA, P>

      const setValue = useCallback(
        (newValue: DeepValue<DATA, P>, meta?: META) => {
          const changes: ArrayOfChanges<DATA, META> = [
            [path, newValue, (meta || {}) as META],
          ]
          processChanges(store, changes)
        },
        [store, path],
      )

      return [value, setValue]
    },

    /**
     * Just-In-Time hook for bulk operations and non-reactive reads
     */
    useJitStore: (): {
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

      return {
        proxyValue,
        setChanges,
        getState,
      }
    },

    /**
     * Hook for registering side effects
     * Registers directly into _internal.graphs
     */
    useSideEffects: (id: string, effects: SideEffects<DATA>): void => {
      const store = useStoreContext<DATA, META>()

      useLayoutEffect(() => {
        return registerSideEffects(store, id, effects)
      }, [store, id, effects])
    },

    /**
     * Form field hook with convenient object API {value, setValue}
     */
    useFieldStore: <P extends DeepKey<DATA>>(
      path: P,
    ): {
      value: DeepValue<DATA, P>
      setValue: (newValue: DeepValue<DATA, P>, meta?: META) => void
    } => {
      const store = useStoreContext<DATA, META>()
      const snap = useSnapshot(store.state) as DATA
      const value = deepGet(snap, path) as DeepValue<DATA, P>

      const setValue = useCallback(
        (newValue: DeepValue<DATA, P>, meta?: META) => {
          const changes: ArrayOfChanges<DATA, META> = [
            [path, newValue, (meta || {}) as META],
          ]
          processChanges(store, changes)
        },
        [store, path],
      )

      return {
        value,
        setValue,
      }
    },

    /**
     * Form field hook with bidirectional transformations
     */
    useFieldTransformedStore: <
      P extends DeepKey<DATA>,
      VAL extends DeepValue<DATA, P>,
      CTX,
    >(
      path: P,
      config: FieldTransformConfig<VAL, CTX>,
    ): {
      value: CTX
      setValue: (newContext: CTX) => void
    } => {
      const store = useStoreContext<DATA, META>()
      const { toTemporary, fromTemporary, context } = config

      const snap = useSnapshot(store.state) as DATA
      const storedValue = deepGet(snap, path) as DeepValue<DATA, P>
      const temporaryValue = toTemporary(storedValue as VAL, context)

      const [localValue, setLocalValue] = useState<CTX>(temporaryValue)

      useEffect(() => {
        setLocalValue(toTemporary(storedValue as VAL, context))
      }, [storedValue, toTemporary, context])

      const setValue = useCallback(
        (newContext: CTX) => {
          setLocalValue(newContext)
          const newStoredValue = fromTemporary(newContext, context)
          const changes: ArrayOfChanges<DATA, META> = [
            [path, newStoredValue as DeepValue<DATA, P>, {} as META],
          ]
          processChanges(store, changes)
        },
        [store, path, fromTemporary, context],
      )

      return {
        value: localValue,
        setValue,
      }
    },

    /**
     * Hook for registering concerns with automatic dependency tracking
     * Registers on mount, unregisters on unmount
     * Uses the store's CONCERNS array by default
     */
    useConcerns: (
      id: string,
      registration: Partial<
        Record<DeepKey<DATA>, Partial<Record<string, any>>>
      >,
      customConcerns?: readonly any[],
    ): void => {
      const store = useStoreContext<DATA, META>()
      const concerns = customConcerns || defaultConcerns

      useLayoutEffect(() => {
        // Cast store to base type for internal concern registration
        // Use unknown intermediate to satisfy TypeScript variance constraints
        return registerConcernEffects(store, registration, concerns)
        // Re-register if id, registration, or custom concerns change
      }, [store, id, registration, customConcerns])
    },

    /**
     * Hook for reading concern results for a specific path
     * Returns properly typed concerns based on the CONCERNS array
     */
    useFieldConcerns: <P extends DeepKey<DATA>>(
      path: P,
    ): EvaluatedConcerns<CONCERNS> => {
      const store = useStoreContext<DATA, META>()
      const snap = useSnapshot(store._concerns) as Record<string, unknown>

      // Return concerns at path, or empty object if none exist
      return (snap[path] || {}) as EvaluatedConcerns<CONCERNS>
    },
  }
}
