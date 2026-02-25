/**
 * Valtio DevTools — Connects state and concerns proxies to Redux DevTools.
 *
 * Uses valtio/utils devtools() to expose both proxies as separate DevTools instances:
 * - '{prefix}:state' — application state proxy
 * - '{prefix}:concerns' — computed concern values proxy
 *
 * Singleton connections per proxy — survives StrictMode remounts and HMR reloads.
 * Separate from PipelineObserver (which tracks pipeline change flow).
 */

import { useLayoutEffect, useRef } from 'react'

import { devtools } from 'valtio/utils'

import type { StoreInstance } from '../core/types'
import type { GenericMeta } from '../types'
import type { DevToolsRef } from '../utils/debug-log'

// Track active devtools subscriptions per proxy identity.
// valtio's devtools() calls ext.connect() internally — calling it again for the
// same proxy creates a duplicate DevTools entry. We guard against that by
// tracking which proxies already have an active connection.
const connectedProxies = new WeakSet<object>()

const connectProxy = (
  proxyObj: object,
  name: string,
  enabled: boolean,
): (() => void) | undefined => {
  if (connectedProxies.has(proxyObj)) return undefined

  connectedProxies.add(proxyObj)
  const unsub = devtools(proxyObj, { name, enabled })

  if (typeof unsub === 'function') {
    return () => {
      unsub()
      connectedProxies.delete(proxyObj)
    }
  }

  return () => {
    connectedProxies.delete(proxyObj)
  }
}

/**
 * Hook that connects store proxies to Redux DevTools when devtools flag is enabled.
 * Uses DevToolsRef for unique store identification.
 */
export const useStoreDevtools = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  store: StoreInstance<DATA, META>,
  enabled: boolean,
  dt: DevToolsRef,
): void => {
  const unsubRef = useRef<(() => void)[]>([])

  useLayoutEffect(() => {
    const unsubs: (() => void)[] = []

    const unsub1 = connectProxy(store.state, `${dt.prefix}:state`, enabled)
    if (unsub1) unsubs.push(unsub1)

    const unsub2 = connectProxy(
      store._concerns,
      `${dt.prefix}:concerns`,
      enabled,
    )
    if (unsub2) unsubs.push(unsub2)

    unsubRef.current = unsubs

    return () => {
      for (const unsub of unsubRef.current) unsub()
      unsubRef.current = []
    }
  }, [store, enabled, dt])
}
