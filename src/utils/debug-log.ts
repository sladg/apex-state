/**
 * Pipeline Observer — Unified debug logging + Redux DevTools integration.
 *
 * Single interface that dispatches to:
 * - Console logging (controlled by logPipeline / logListeners / logConcerns flags)
 * - Redux DevTools (controlled by devtools flag) with nested tree-shaped state
 *
 * Call sites use one `observer.xyz()` call; the observer fans out internally.
 * Zero runtime cost when all flags are false (returns no-op object).
 */

import type { DebugConfig } from '../core/types'

// ---------------------------------------------------------------------------
// Public interface (exported, used by InternalState and call sites)
// ---------------------------------------------------------------------------

export interface PipelineObserver {
  pipelineStart: (label: string, input: unknown[]) => void
  phase1: (stateChanges: unknown[]) => void
  syncExpand: (changes: unknown[]) => void
  flipExpand: (changes: unknown[]) => void
  listenerDispatch: (
    subscriberId: number,
    fnName: string,
    scope: string,
    input: unknown[],
    output: unknown,
  ) => void
  validatorResult: (outputPath: string, input: unknown, result: unknown) => void
  phase2: (stateChanges: unknown[]) => void
  pipelineEnd: () => void
  concernEval: (
    path: string,
    name: string,
    value: unknown,
    result: unknown,
  ) => void
  destroy: () => void
}

// ---------------------------------------------------------------------------
// No-op singleton (zero overhead when all flags off)
// ---------------------------------------------------------------------------

const noop = () => {
  // no-op
}

const NOOP_OBSERVER: PipelineObserver = {
  pipelineStart: noop,
  phase1: noop,
  syncExpand: noop,
  flipExpand: noop,
  listenerDispatch: noop,
  validatorResult: noop,
  phase2: noop,
  pipelineEnd: noop,
  concernEval: noop,
  destroy: noop,
}

// ---------------------------------------------------------------------------
// Console logging layer
// ---------------------------------------------------------------------------

const PREFIX = 'apex-state'

const createConsoleLayer = (config: DebugConfig) => {
  const { logPipeline, logListeners, logConcerns } = config
  if (!logPipeline && !logListeners && !logConcerns) return NOOP_OBSERVER

  return {
    ...NOOP_OBSERVER,

    ...(logPipeline
      ? {
          pipelineStart: (label: string, input: unknown[]) => {
            console.group(`${PREFIX}:pipeline | ${label} `, input)
          },
          phase1: (stateChanges: unknown[]) => {
            console.log(
              `${PREFIX}:pipeline | phase1 state changes `,
              stateChanges,
            )
          },
          syncExpand: (changes: unknown[]) => {
            console.log(`${PREFIX}:pipeline | sync paths `, changes)
          },
          flipExpand: (changes: unknown[]) => {
            console.log(`${PREFIX}:pipeline | flip paths `, changes)
          },
          phase2: (stateChanges: unknown[]) => {
            console.log(
              `${PREFIX}:pipeline | phase2 state changes `,
              stateChanges,
            )
          },
          pipelineEnd: () => {
            console.groupEnd()
          },
        }
      : {}),

    ...(logListeners
      ? {
          listenerDispatch: (
            subscriberId: number,
            fnName: string,
            scope: string,
            input: unknown[],
            output: unknown,
          ) => {
            const name = fnName || '(anonymous)'
            console.group(
              `${PREFIX}:listener | id=${String(subscriberId)} ${name} scope=${scope} `,
              { input },
            )
            console.log(`${PREFIX}:listener | output `, output)
            console.groupEnd()
          },
        }
      : {}),

    ...(logConcerns
      ? {
          validatorResult: (
            outputPath: string,
            input: unknown,
            result: unknown,
          ) => {
            console.groupCollapsed(`${PREFIX}:validator | ${outputPath}`, {
              input,
              result,
            })
            console.groupEnd()
          },
          concernEval: (
            path: string,
            name: string,
            value: unknown,
            result: unknown,
          ) => {
            console.groupCollapsed(`${PREFIX}:concern | ${path}.${name}`, {
              value,
              result,
            })
            console.groupEnd()
          },
        }
      : {}),
  }
}

// ---------------------------------------------------------------------------
// Redux DevTools layer
// ---------------------------------------------------------------------------

export interface DevToolsInstance {
  init: (state: unknown) => void
  send: (action: { type: string }, state: unknown) => void
  unsubscribe: () => void
}

export interface DevToolsRef {
  prefix: string
  pipeline: DevToolsInstance | null
}

interface DevToolsExtension {
  connect: (options: { name: string; features?: object }) => DevToolsInstance
}

/** Format changes as a compact { path: value } map for DevTools readability. */
const changesMap = (changes: unknown[]): Record<string, unknown> => {
  const map: Record<string, unknown> = {}
  for (const c of changes) {
    if (Array.isArray(c)) {
      map[c[0] as string] = c[1]
    } else if (c && typeof c === 'object' && 'path' in c) {
      const obj = c as { path: string; value: unknown }
      map[obj.path] = obj.value
    } else {
      // Unknown format — skip
    }
  }
  return map
}

const getDevToolsExtension = (): DevToolsExtension | undefined =>
  typeof window !== 'undefined'
    ? ((window as unknown as Record<string, unknown>)[
        '__REDUX_DEVTOOLS_EXTENSION__'
      ] as DevToolsExtension | undefined)
    : undefined

/** Connect to Redux DevTools, reusing an existing instance from the ref. */
export const connectPipelineDevTools = (
  prefix: string,
): DevToolsInstance | null => {
  const ext = getDevToolsExtension()
  if (!ext) return null

  const instance = ext.connect({
    name: `${prefix}:pipeline`,
    features: { jump: false, skip: false, dispatch: false },
  })
  instance.init({})
  return instance
}

const createDevToolsLayer = (enabled: boolean, dt: DevToolsRef) => {
  if (!enabled) return NOOP_OBSERVER
  if (!dt.pipeline) return NOOP_OBSERVER

  const instance = dt.pipeline

  let runId = 0
  let tree: Record<string, unknown> = {}

  return {
    ...NOOP_OBSERVER,

    pipelineStart: (label: string, input: unknown[]) => {
      runId++
      tree = { runId, label, input: changesMap(input) }
    },

    phase1: (stateChanges: unknown[]) => {
      tree['phase1'] = changesMap(stateChanges)
    },

    syncExpand: (changes: unknown[]) => {
      tree['sync (+' + String(changes.length) + ')'] = changesMap(changes)
    },

    flipExpand: (changes: unknown[]) => {
      tree['flip (+' + String(changes.length) + ')'] = changesMap(changes)
    },

    listenerDispatch: (
      subscriberId: number,
      fnName: string,
      scope: string,
      input: unknown[],
      output: unknown,
    ) => {
      const name = fnName || '(anonymous)'
      const entry: Record<string, unknown> = {
        fn: name,
        scope,
        input: changesMap(input),
      }
      if (Array.isArray(output) && output.length > 0) {
        entry['output'] = changesMap(output)
      } else {
        entry['output'] = '(none)'
      }
      tree['listener:' + String(subscriberId) + ' ' + name] = entry
    },

    validatorResult: (outputPath: string, input: unknown, result: unknown) => {
      tree['validator ' + outputPath] = { input, result }
    },

    phase2: (stateChanges: unknown[]) => {
      if (stateChanges.length > 0) {
        tree['phase2 (+' + String(stateChanges.length) + ')'] =
          changesMap(stateChanges)
      }
    },

    pipelineEnd: () => {
      instance.send({ type: 'PIPELINE_RUN' }, tree)
    },

    destroy: () => {
      instance.unsubscribe()
    },
  }
}

// ---------------------------------------------------------------------------
// Factory: creates composite observer from both layers
// ---------------------------------------------------------------------------

/**
 * Create a unified pipeline observer for the store.
 * Fans out to console logging and Redux DevTools based on DebugConfig flags.
 * Returns no-op object when all flags are false (zero overhead).
 */
export const createPipelineObserver = (
  config: DebugConfig,
  dtRef: DevToolsRef,
): PipelineObserver => {
  const {
    logPipeline = false,
    logListeners = false,
    logConcerns = false,
    devtools = false,
  } = config

  if (!logPipeline && !logListeners && !logConcerns && !devtools) {
    return NOOP_OBSERVER
  }

  const con = createConsoleLayer(config)
  const dt = createDevToolsLayer(devtools, dtRef)

  return {
    pipelineStart: (label, input) => {
      con.pipelineStart(label, input)
      dt.pipelineStart(label, input)
    },
    phase1: (stateChanges) => {
      con.phase1(stateChanges)
      dt.phase1(stateChanges)
    },
    syncExpand: (changes) => {
      con.syncExpand(changes)
      dt.syncExpand(changes)
    },
    flipExpand: (changes) => {
      con.flipExpand(changes)
      dt.flipExpand(changes)
    },
    listenerDispatch: (subscriberId, fnName, scope, input, output) => {
      con.listenerDispatch(subscriberId, fnName, scope, input, output)
      dt.listenerDispatch(subscriberId, fnName, scope, input, output)
    },
    validatorResult: (outputPath, input, result) => {
      con.validatorResult(outputPath, input, result)
      dt.validatorResult(outputPath, input, result)
    },
    phase2: (stateChanges) => {
      con.phase2(stateChanges)
      dt.phase2(stateChanges)
    },
    pipelineEnd: () => {
      con.pipelineEnd()
      dt.pipelineEnd()
    },
    concernEval: (path, name, value, result) => {
      con.concernEval(path, name, value, result)
    },
    destroy: () => {
      dt.destroy()
    },
  }
}
