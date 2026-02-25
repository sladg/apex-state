/**
 * Valtio DevTools — Connects state and concerns proxies to Redux DevTools.
 *
 * Uses valtio/utils devtools() to expose both proxies as separate DevTools instances:
 * - 'apex-state:state' — application state proxy
 * - 'apex-state:concerns' — computed concern values proxy
 *
 * Separate from PipelineObserver (which tracks pipeline change flow).
 */

import { useLayoutEffect, useRef } from 'react'

import { devtools } from 'valtio/utils'

import type { StoreInstance } from '../core/types'
import type { GenericMeta } from '../types'

/**
 * Hook that connects store proxies to Redux DevTools when devtools flag is enabled.
 * Returns cleanup function. No-ops when disabled or extension not present.
 */
export const useStoreDevtools = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  store: StoreInstance<DATA, META>,
  enabled: boolean,
): void => {
  const unsubRef = useRef<(() => void)[]>([])

  useLayoutEffect(() => {
    const unsub1 = devtools(store.state, {
      name: 'apex-state:state',
      enabled,
    })
    const unsub2 = devtools(store._concerns, {
      name: 'apex-state:concerns',
      enabled,
    })
    unsubRef.current = [unsub1, unsub2].filter(
      (fn): fn is () => void => typeof fn === 'function',
    )

    return () => {
      for (const unsub of unsubRef.current) unsub()
      unsubRef.current = []
    }
  }, [store, enabled])
}
