/**
 * Pipeline executor tests
 *
 * Tests for the pipeline execution engine including:
 * - Synchronizer execution order
 * - Stabilization detection
 * - Infinite loop detection
 * - Multi-pass iteration
 */

import { describe, it, expect } from 'vitest'
import { executePipeline, applyChanges } from '../../src/pipeline/executor'
import type { ArrayOfChanges, GenericMeta } from '../../src/types'
import type { PipelineConfig } from '../../src/pipeline/types'
import { proxy } from 'valtio'

describe('executePipeline', () => {
  it('executes synchronizers in order', () => {
    const executionOrder: string[] = []

    const config: PipelineConfig<object, GenericMeta> = {
      synchronizers: [
        {
          name: 'first',
          fn: (changes) => {
            executionOrder.push('first')
            return changes
          },
        },
        {
          name: 'second',
          fn: (changes) => {
            executionOrder.push('second')
            return changes
          },
        },
        {
          name: 'third',
          fn: (changes) => {
            executionOrder.push('third')
            return changes
          },
        },
      ],
      maxIterations: 100,
    }

    executePipeline([], {}, config)
    expect(executionOrder).toEqual(['first', 'second', 'third'])
  })

  it('stabilizes when no new changes are added', () => {
    let runCount = 0

    const config: PipelineConfig<object, GenericMeta> = {
      synchronizers: [
        {
          name: 'test',
          fn: (changes) => {
            runCount++
            return changes // No new changes
          },
        },
      ],
      maxIterations: 100,
    }

    const initialChanges: ArrayOfChanges<any, GenericMeta> = [
      ['path', 'value', {}],
    ]

    executePipeline(initialChanges, {}, config)
    expect(runCount).toBe(1) // Should only run once
  })

  it('detects infinite loops and throws error', () => {
    const config: PipelineConfig<any, GenericMeta> = {
      synchronizers: [
        {
          name: 'infinite',
          fn: (changes) => {
            // Always add a new change - creates infinite loop
            return [...changes, ['new', 'value', {}]] as any
          },
        },
      ],
      maxIterations: 10,
    }

    expect(() => {
      executePipeline([], {}, config)
    }).toThrow(/exceeded max iterations/)
  })

  it('runs multiple iterations until stabilization', () => {
    let iteration = 0

    const config: PipelineConfig<{ count: number }, GenericMeta> = {
      synchronizers: [
        {
          name: 'counter',
          fn: (changes) => {
            iteration++
            // Add a new change for the first 3 iterations, then stabilize
            if (iteration < 3) {
              return [...changes, ['count', iteration, {}]] as ArrayOfChanges<{ count: number }, GenericMeta>
            }
            return changes
          },
        },
      ],
      maxIterations: 100,
    }

    const result = executePipeline([], { count: 0 }, config)

    // Should have run 3 times and added 3 changes
    expect(iteration).toBe(3)
    expect(result.length).toBe(2) // 2 changes added in iterations 1 and 2
  })

  it('processes changes through multiple synchronizers', () => {
    type State = { a: number; b: number; c: number }

    const config: PipelineConfig<State, GenericMeta> = {
      synchronizers: [
        {
          name: 'add-b',
          fn: (changes, state) => {
            // If 'a' changed, also change 'b' (only if b not already in changes)
            const hasA = changes.some(([path]) => path === 'a')
            const hasB = changes.some(([path]) => path === 'b')
            if (hasA && !hasB) {
              return [...changes, ['b', 20, { isProgramaticChange: true }]]
            }
            return changes
          },
        },
        {
          name: 'add-c',
          fn: (changes, state) => {
            // If 'b' changed, also change 'c' (only if c not already in changes)
            const hasB = changes.some(([path]) => path === 'b')
            const hasC = changes.some(([path]) => path === 'c')
            if (hasB && !hasC) {
              return [...changes, ['c', 30, { isProgramaticChange: true }]]
            }
            return changes
          },
        },
      ],
      maxIterations: 100,
    }

    const initialChanges: ArrayOfChanges<State, GenericMeta> = [
      ['a', 10, {}],
    ]

    const result = executePipeline(initialChanges, { a: 0, b: 0, c: 0 }, config)

    // Should have cascading changes: a → b → c
    // First iteration: a → adds b, then b → adds c
    // Second iteration: all exist, no new changes added, stabilizes
    expect(result.length).toBe(3)
    expect(result.map(([path]) => path)).toEqual(['a', 'b', 'c'])
  })

  it('preserves metadata through pipeline', () => {
    const config: PipelineConfig<any, GenericMeta> = {
      synchronizers: [
        {
          name: 'passthrough',
          fn: (changes) => changes,
        },
      ],
      maxIterations: 100,
    }

    const initialChanges: ArrayOfChanges<any, GenericMeta> = [
      ['path', 'value', { sender: 'test-user', isProgramaticChange: true }],
    ]

    const result = executePipeline(initialChanges, {}, config)

    expect(result[0][2]).toEqual({
      sender: 'test-user',
      isProgramaticChange: true,
    })
  })
})

describe('applyChanges', () => {
  it('applies single change to valtio proxy', () => {
    const state = proxy({ count: 0 })
    const changes: ArrayOfChanges<{ count: number }, GenericMeta> = [
      ['count', 42, {}],
    ]

    applyChanges(state, changes)
    expect(state.count).toBe(42)
  })

  it('applies multiple changes atomically', () => {
    const state = proxy({ a: 0, b: 0, c: 0 })
    const changes: ArrayOfChanges<{ a: number; b: number; c: number }, GenericMeta> = [
      ['a', 10, {}],
      ['b', 20, {}],
      ['c', 30, {}],
    ]

    applyChanges(state, changes)
    expect(state.a).toBe(10)
    expect(state.b).toBe(20)
    expect(state.c).toBe(30)
  })

  it('applies nested path changes', () => {
    const state = proxy({ user: { name: 'Initial', age: 0 } })
    const changes: ArrayOfChanges<{ user: { name: string; age: number } }, GenericMeta> = [
      ['user.name', 'Alice', {}],
      ['user.age', 30, {}],
    ]

    applyChanges(state, changes)
    expect(state.user.name).toBe('Alice')
    expect(state.user.age).toBe(30)
  })
})
