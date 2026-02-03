/**
 * Tests for flushQueue functionality
 *
 * flushQueue allows listeners to stop processing remaining listeners
 * and clear the pending queue by throwing internally.
 */

import Graph from 'graphology'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { proxy, ref } from 'valtio'

import { processChanges } from '../../src/store/executor'
import { registerListener } from '../../src/sideEffects/registration'
import type { StoreInstance } from '../../src/store/types'

interface TestState {
  value: number
  other: string
}

const createTestStore = (
  initialState: TestState,
): StoreInstance<TestState> => {
  return proxy({
    state: initialState,
    _concerns: {},
    _internal: ref({
      graphs: {
        sync: new Graph({ type: 'undirected' }),
        flip: new Graph({ type: 'undirected' }),
        aggregations: new Graph({ type: 'directed', allowSelfLoops: false }),
        listeners: new Map(),
      },
      registrations: {
        concerns: new Map(),
        effectCleanups: new Set(),
        sideEffectCleanups: new Map(),
      },
      processing: {
        queue: [],
        isProcessing: false,
      },
    }),
    config: {
      errorStorePath: '_errors',
      maxIterations: 100,
    },
  })
}

describe('flushQueue', () => {
  let store: StoreInstance<TestState>

  beforeEach(() => {
    store = createTestStore({ value: 0, other: 'initial' })
  })

  it('should stop processing remaining listeners when flushQueue is called', () => {
    const listenerA = vi.fn(() => undefined)
    const listenerB = vi.fn((_change, _state, flushQueue) => flushQueue())
    const listenerC = vi.fn(() => undefined)

    registerListener(store, 'value', listenerA)
    registerListener(store, 'value', listenerB)
    registerListener(store, 'value', listenerC)

    processChanges(store, [['value', 10, {}]])

    expect(listenerA).toHaveBeenCalledTimes(1)
    expect(listenerB).toHaveBeenCalledTimes(1)
    expect(listenerC).not.toHaveBeenCalled()
  })

  it('should clear queued changes preventing next iteration', () => {
    const listenerA = vi.fn((): [['other', string, object]] => [
      ['other', 'from-a', {}],
    ])
    const listenerB = vi.fn((_change, _state, flushQueue) => flushQueue())
    const otherListener = vi.fn(() => undefined)

    registerListener(store, 'value', listenerA)
    registerListener(store, 'value', listenerB)
    registerListener(store, 'other', otherListener)

    processChanges(store, [['value', 10, {}]])

    expect(store._internal.processing.queue).toEqual([])
    expect(otherListener).not.toHaveBeenCalled()
    expect(store.state.other).toBe('initial')
  })

  it('should still apply current batch changes before flush', () => {
    const listener = vi.fn((_change, _state, flushQueue) => flushQueue())

    registerListener(store, 'value', listener)
    processChanges(store, [['value', 42, {}]])

    expect(store.state.value).toBe(42)
  })

  it('should rethrow non-flush errors', () => {
    registerListener(store, 'value', () => {
      throw new Error('Test error')
    })

    expect(() => processChanges(store, [['value', 10, {}]])).toThrow('Test error')
  })
})
