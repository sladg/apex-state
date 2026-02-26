/**
 * Valtio DevTools — Connects state and concerns proxies to Redux DevTools,
 * and provides a pipeline notifier for sending pipeline/registration events.
 *
 * Uses valtio/utils devtools() to expose both proxies as separate DevTools instances:
 * - '{prefix}:state' — application state proxy
 * - '{prefix}:concerns' — computed concern values proxy
 *
 * Singleton connections per proxy — survives StrictMode remounts and HMR reloads.
 */

import { devtools } from 'valtio/utils'

import pkg from '../../package.json'
import type { StoreConfig } from '../core/types'
import type { DeepRequired } from '../types'
import type { PipelineLogData } from '../utils/log'
import type { Wasm } from '../wasm/bridge'

// ---------------------------------------------------------------------------
// DevTools types
// ---------------------------------------------------------------------------

interface DevToolsInstance {
  init: (state: unknown) => void
  send: (action: { type: string }, state: unknown) => void
  unsubscribe: () => void
}

interface DevToolsExtension {
  connect: (options: { name: string; features?: object }) => DevToolsInstance
}

let devtoolsIdCounter = 0

const getDevToolsExtension = (): DevToolsExtension | undefined =>
  typeof window !== 'undefined'
    ? ((window as unknown as Record<string, unknown>)[
        '__REDUX_DEVTOOLS_EXTENSION__'
      ] as DevToolsExtension | undefined)
    : undefined

/** Connect to Redux DevTools, returning a new instance. */
const connectPipelineDevTools = (prefix: string): DevToolsInstance | null => {
  const ext = getDevToolsExtension()
  if (!ext) return null

  const instance = ext.connect({
    name: `${prefix}:pipeline`,
    features: { jump: false, skip: false, dispatch: false },
  })
  instance.init({})
  return instance
}

// ---------------------------------------------------------------------------
// Pipeline notifier — sends pipeline/registration events to DevTools
// ---------------------------------------------------------------------------

export interface DevToolsNotifier {
  notifyPipeline: (data: PipelineLogData) => void
  notifyRegistration: (
    type: 'register' | 'unregister',
    id: string,
    snapshot: Wasm.GraphSnapshot,
  ) => void
  destroy: () => void
}

/** Build a short label from input change paths. */
const buildPathLabel = (paths: string[]): string => {
  if (paths.length === 0) return '(empty)'
  if (paths.length <= 3) return paths.join(', ')
  return `${paths[0]} +${String(paths.length - 1)} more`
}

/** Build DevTools tree object for a pipeline run. */
const buildDevToolsTree = (data: PipelineLogData): Record<string, unknown> => {
  const tree: Record<string, unknown> = {
    input: data.input,
    duration: `${data.durationMs.toFixed(2)}ms`,
  }
  if (data.trace) tree['trace'] = data.trace
  for (const [i, entry] of data.listeners.entries()) {
    const n = entry.fnName || '(anonymous)'
    const key = `[${String(i).padStart(2, '0')}] listener:${String(entry.subscriberId)} ${n}`
    tree[key] = {
      fn: n,
      scope: entry.scope,
      input: entry.input,
      output: entry.output,
      ...(entry.durationMs > 0
        ? { duration: entry.durationMs.toFixed(2) + 'ms' }
        : {}),
    }
  }
  return tree
}

// ---------------------------------------------------------------------------
// Proxy DevTools — eager connection (no hooks needed)
// ---------------------------------------------------------------------------

// Track active devtools subscriptions per proxy identity.
// valtio's devtools() calls ext.connect() internally — calling it again for the
// same proxy creates a duplicate DevTools entry. We guard against that by
// tracking which proxies already have an active connection.
const connectedProxies = new WeakSet<object>()

const connectProxy = (
  proxyObj: object,
  name: string,
): (() => void) | undefined => {
  if (connectedProxies.has(proxyObj)) return undefined

  connectedProxies.add(proxyObj)
  const unsub = devtools(proxyObj, { name, enabled: true })

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
 * Initialize all DevTools connections: pipeline notifier + proxy inspection.
 * Returns a DevToolsNotifier, or null if devtools is disabled or unavailable.
 * destroy() tears down everything (pipeline + proxy connections).
 */
export const attachDevtools = (
  config: DeepRequired<StoreConfig>,
  stateProxy: object,
  concernsProxy: object,
): DevToolsNotifier | null => {
  if (!config.debug.devtools) return null

  devtoolsIdCounter++
  const prefix = `apex-state@${pkg.version}:${config.name}-${String(devtoolsIdCounter)}`

  // Pipeline DevTools connection
  const pipelineInstance = connectPipelineDevTools(prefix)
  if (!pipelineInstance) return null

  // Proxy DevTools connections (state + concerns inspection)
  const proxyUnsubs: (() => void)[] = []
  const unsub1 = connectProxy(stateProxy, `${prefix}:state`)
  if (unsub1) proxyUnsubs.push(unsub1)
  const unsub2 = connectProxy(concernsProxy, `${prefix}:concerns`)
  if (unsub2) proxyUnsubs.push(unsub2)

  return {
    notifyPipeline: (data) => {
      const paths = data.input.map((c) => c.path)
      const suffix = paths.length === 0 ? '' : ` ${buildPathLabel(paths)}`
      pipelineInstance.send(
        { type: `PIPELINE_RUN${suffix}` },
        buildDevToolsTree(data),
      )
    },

    notifyRegistration: (type, id, snapshot) => {
      const actionType =
        type === 'register'
          ? `REGISTER_SIDE_EFFECTS ${id}`
          : `UNREGISTER_SIDE_EFFECTS ${id}`
      pipelineInstance.send({ type: actionType }, { id, snapshot })
    },

    destroy: () => {
      for (const unsub of proxyUnsubs) unsub()
      pipelineInstance.unsubscribe()
    },
  }
}
