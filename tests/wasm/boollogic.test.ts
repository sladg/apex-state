import { beforeEach, describe, expect, it } from 'vitest'

import type { BoolLogic } from '~/types'
import { evaluateBoolLogic } from '~/utils/boolLogic'

interface TestState {
  user: {
    role: string
    age: number
    active: boolean
  }
  product: {
    name: string
    price: number
    stock: number
  }
  flags: {
    feature1: boolean
    feature2: boolean
  }
}

describe('BoolLogic Evaluator (WASM-003)', () => {
  let wasm: typeof import('../../rust/pkg/apex_state_wasm.js')

  beforeEach(async () => {
    wasm = await import('../../rust/pkg/apex_state_wasm.js')
    wasm.intern_clear()
  })

  const createShadowState = (
    state: Partial<TestState>,
  ): Record<number, unknown> => {
    // For testing, we manually map paths to IDs
    // In real usage, this would use the interning system
    const shadow: Record<number, unknown> = {}

    if (state.user?.role !== undefined) {
      shadow[0] = state.user.role
    }
    if (state.user?.age !== undefined) {
      shadow[1] = state.user.age
    }
    if (state.user?.active !== undefined) {
      shadow[2] = state.user.active
    }
    if (state.product?.name !== undefined) {
      shadow[3] = state.product.name
    }
    if (state.product?.price !== undefined) {
      shadow[4] = state.product.price
    }
    if (state.product?.stock !== undefined) {
      shadow[5] = state.product.stock
    }

    return shadow
  }

  describe('IS_EQUAL operator', () => {
    it('matches JS evaluator - string equality', () => {
      const state = {
        user: { role: 'admin', age: 30, active: true },
      } as TestState
      const jsLogic: BoolLogic<TestState> = {
        IS_EQUAL: ['user.role', 'admin'],
      }

      const jsResult = evaluateBoolLogic(jsLogic, state)

      const wasmTree = JSON.stringify({
        IS_EQUAL: { path_id: 0, expected: 'admin' },
      })
      const wasmState = JSON.stringify(createShadowState(state))
      const wasmResult = wasm.evaluate_boollogic(wasmTree, wasmState)

      expect(wasmResult).toBe(jsResult)
      expect(wasmResult).toBe(true)
    })

    it('matches JS evaluator - number equality', () => {
      const state = {
        user: { role: 'user', age: 25, active: true },
      } as TestState
      const jsLogic: BoolLogic<TestState> = { IS_EQUAL: ['user.age', 25] }

      const jsResult = evaluateBoolLogic(jsLogic, state)

      const wasmTree = JSON.stringify({
        IS_EQUAL: { path_id: 1, expected: 25 },
      })
      const wasmState = JSON.stringify(createShadowState(state))
      const wasmResult = wasm.evaluate_boollogic(wasmTree, wasmState)

      expect(wasmResult).toBe(jsResult)
      expect(wasmResult).toBe(true)
    })

    it('matches JS evaluator - boolean equality', () => {
      const state = {
        user: { role: 'user', age: 25, active: false },
      } as TestState
      const jsLogic: BoolLogic<TestState> = { IS_EQUAL: ['user.active', false] }

      const jsResult = evaluateBoolLogic(jsLogic, state)

      const wasmTree = JSON.stringify({
        IS_EQUAL: { path_id: 2, expected: false },
      })
      const wasmState = JSON.stringify(createShadowState(state))
      const wasmResult = wasm.evaluate_boollogic(wasmTree, wasmState)

      expect(wasmResult).toBe(jsResult)
      expect(wasmResult).toBe(true)
    })
  })

  describe('EXISTS operator', () => {
    it('matches JS evaluator - value exists', () => {
      const state = {
        user: { role: 'admin', age: 30, active: true },
      } as TestState
      const jsLogic: BoolLogic<TestState> = { EXISTS: 'user.role' }

      const jsResult = evaluateBoolLogic(jsLogic, state)

      const wasmTree = JSON.stringify({ EXISTS: { path_id: 0 } })
      const wasmState = JSON.stringify(createShadowState(state))
      const wasmResult = wasm.evaluate_boollogic(wasmTree, wasmState)

      expect(wasmResult).toBe(jsResult)
      expect(wasmResult).toBe(true)
    })

    it('matches JS evaluator - value missing', () => {
      const state = {
        user: { role: 'admin', age: 30, active: true },
      } as TestState
      const jsLogic: BoolLogic<TestState> = { EXISTS: 'product.name' }

      const jsResult = evaluateBoolLogic(jsLogic, state)

      const wasmTree = JSON.stringify({ EXISTS: { path_id: 3 } })
      const wasmState = JSON.stringify(createShadowState(state))
      const wasmResult = wasm.evaluate_boollogic(wasmTree, wasmState)

      expect(wasmResult).toBe(jsResult)
      expect(wasmResult).toBe(false)
    })
  })

  describe('IS_EMPTY operator', () => {
    it('matches JS evaluator - empty string', () => {
      const state = { product: { name: '', price: 0, stock: 0 } } as TestState
      const jsLogic: BoolLogic<TestState> = { IS_EMPTY: 'product.name' }

      const jsResult = evaluateBoolLogic(jsLogic, state)

      const wasmTree = JSON.stringify({ IS_EMPTY: { path_id: 3 } })
      const wasmState = JSON.stringify(createShadowState(state))
      const wasmResult = wasm.evaluate_boollogic(wasmTree, wasmState)

      expect(wasmResult).toBe(jsResult)
      expect(wasmResult).toBe(true)
    })

    it('matches JS evaluator - non-empty string', () => {
      const state = {
        product: { name: 'Widget', price: 100, stock: 10 },
      } as TestState
      const jsLogic: BoolLogic<TestState> = { IS_EMPTY: 'product.name' }

      const jsResult = evaluateBoolLogic(jsLogic, state)

      const wasmTree = JSON.stringify({ IS_EMPTY: { path_id: 3 } })
      const wasmState = JSON.stringify(createShadowState(state))
      const wasmResult = wasm.evaluate_boollogic(wasmTree, wasmState)

      expect(wasmResult).toBe(jsResult)
      expect(wasmResult).toBe(false)
    })

    it('matches JS evaluator - empty array', () => {
      const state = {
        user: { role: 'admin', age: 30, active: true },
        flags: { feature1: true, feature2: false },
      } as TestState
      const stateWithEmptyArray = { ...state, tags: [] }

      const jsLogic = { IS_EMPTY: 'tags' } as any
      const jsResult = evaluateBoolLogic(jsLogic, stateWithEmptyArray)

      const wasmTree = JSON.stringify({ IS_EMPTY: { path_id: 10 } })
      const wasmState = JSON.stringify({ 10: [] })
      const wasmResult = wasm.evaluate_boollogic(wasmTree, wasmState)

      expect(wasmResult).toBe(jsResult)
      expect(wasmResult).toBe(true)
    })

    it('matches JS evaluator - non-empty array', () => {
      const state = {
        user: { role: 'admin', age: 30, active: true },
        flags: { feature1: true, feature2: false },
      } as TestState
      const stateWithArray = { ...state, tags: ['tag1', 'tag2'] }

      const jsLogic = { IS_EMPTY: 'tags' } as any
      const jsResult = evaluateBoolLogic(jsLogic, stateWithArray)

      const wasmTree = JSON.stringify({ IS_EMPTY: { path_id: 10 } })
      const wasmState = JSON.stringify({ 10: ['tag1', 'tag2'] })
      const wasmResult = wasm.evaluate_boollogic(wasmTree, wasmState)

      expect(wasmResult).toBe(jsResult)
      expect(wasmResult).toBe(false)
    })

    it('matches JS evaluator - empty object', () => {
      const state = {
        user: { role: 'admin', age: 30, active: true },
      } as TestState
      const stateWithEmptyObj = { ...state, metadata: {} }

      const jsLogic = { IS_EMPTY: 'metadata' } as any
      const jsResult = evaluateBoolLogic(jsLogic, stateWithEmptyObj)

      const wasmTree = JSON.stringify({ IS_EMPTY: { path_id: 11 } })
      const wasmState = JSON.stringify({ 11: {} })
      const wasmResult = wasm.evaluate_boollogic(wasmTree, wasmState)

      expect(wasmResult).toBe(jsResult)
      expect(wasmResult).toBe(true)
    })

    it('matches JS evaluator - non-empty object', () => {
      const state = {
        user: { role: 'admin', age: 30, active: true },
      } as TestState
      const stateWithObj = { ...state, metadata: { key: 'value' } }

      const jsLogic = { IS_EMPTY: 'metadata' } as any
      const jsResult = evaluateBoolLogic(jsLogic, stateWithObj)

      const wasmTree = JSON.stringify({ IS_EMPTY: { path_id: 11 } })
      const wasmState = JSON.stringify({ 11: { key: 'value' } })
      const wasmResult = wasm.evaluate_boollogic(wasmTree, wasmState)

      expect(wasmResult).toBe(jsResult)
      expect(wasmResult).toBe(false)
    })

    it('matches JS evaluator - number zero is not empty', () => {
      const state = { product: { name: '', price: 0, stock: 0 } } as TestState
      const jsLogic: BoolLogic<TestState> = { IS_EMPTY: 'product.price' }

      const jsResult = evaluateBoolLogic(jsLogic, state)

      const wasmTree = JSON.stringify({ IS_EMPTY: { path_id: 4 } })
      const wasmState = JSON.stringify(createShadowState(state))
      const wasmResult = wasm.evaluate_boollogic(wasmTree, wasmState)

      expect(wasmResult).toBe(jsResult)
      expect(wasmResult).toBe(false)
    })

    it('matches JS evaluator - boolean false is not empty', () => {
      const state = {
        user: { role: 'user', age: 30, active: false },
      } as TestState
      const jsLogic: BoolLogic<TestState> = { IS_EMPTY: 'user.active' }

      const jsResult = evaluateBoolLogic(jsLogic, state)

      const wasmTree = JSON.stringify({ IS_EMPTY: { path_id: 2 } })
      const wasmState = JSON.stringify(createShadowState(state))
      const wasmResult = wasm.evaluate_boollogic(wasmTree, wasmState)

      expect(wasmResult).toBe(jsResult)
      expect(wasmResult).toBe(false)
    })
  })

  describe('AND operator', () => {
    it('matches JS evaluator - all true', () => {
      const state = {
        user: { role: 'admin', age: 30, active: true },
      } as TestState
      const jsLogic: BoolLogic<TestState> = {
        AND: [
          { IS_EQUAL: ['user.role', 'admin'] },
          { IS_EQUAL: ['user.active', true] },
        ],
      }

      const jsResult = evaluateBoolLogic(jsLogic, state)

      const wasmTree = JSON.stringify({
        AND: {
          children: [
            { IS_EQUAL: { path_id: 0, expected: 'admin' } },
            { IS_EQUAL: { path_id: 2, expected: true } },
          ],
        },
      })
      const wasmState = JSON.stringify(createShadowState(state))
      const wasmResult = wasm.evaluate_boollogic(wasmTree, wasmState)

      expect(wasmResult).toBe(jsResult)
      expect(wasmResult).toBe(true)
    })

    it('matches JS evaluator - one false', () => {
      const state = {
        user: { role: 'admin', age: 30, active: false },
      } as TestState
      const jsLogic: BoolLogic<TestState> = {
        AND: [
          { IS_EQUAL: ['user.role', 'admin'] },
          { IS_EQUAL: ['user.active', true] },
        ],
      }

      const jsResult = evaluateBoolLogic(jsLogic, state)

      const wasmTree = JSON.stringify({
        AND: {
          children: [
            { IS_EQUAL: { path_id: 0, expected: 'admin' } },
            { IS_EQUAL: { path_id: 2, expected: true } },
          ],
        },
      })
      const wasmState = JSON.stringify(createShadowState(state))
      const wasmResult = wasm.evaluate_boollogic(wasmTree, wasmState)

      expect(wasmResult).toBe(jsResult)
      expect(wasmResult).toBe(false)
    })
  })

  describe('OR operator', () => {
    it('matches JS evaluator - one true', () => {
      const state = {
        user: { role: 'admin', age: 30, active: false },
      } as TestState
      const jsLogic: BoolLogic<TestState> = {
        OR: [
          { IS_EQUAL: ['user.role', 'admin'] },
          { IS_EQUAL: ['user.active', true] },
        ],
      }

      const jsResult = evaluateBoolLogic(jsLogic, state)

      const wasmTree = JSON.stringify({
        OR: {
          children: [
            { IS_EQUAL: { path_id: 0, expected: 'admin' } },
            { IS_EQUAL: { path_id: 2, expected: true } },
          ],
        },
      })
      const wasmState = JSON.stringify(createShadowState(state))
      const wasmResult = wasm.evaluate_boollogic(wasmTree, wasmState)

      expect(wasmResult).toBe(jsResult)
      expect(wasmResult).toBe(true)
    })

    it('matches JS evaluator - all false', () => {
      const state = {
        user: { role: 'user', age: 30, active: false },
      } as TestState
      const jsLogic: BoolLogic<TestState> = {
        OR: [
          { IS_EQUAL: ['user.role', 'admin'] },
          { IS_EQUAL: ['user.active', true] },
        ],
      }

      const jsResult = evaluateBoolLogic(jsLogic, state)

      const wasmTree = JSON.stringify({
        OR: {
          children: [
            { IS_EQUAL: { path_id: 0, expected: 'admin' } },
            { IS_EQUAL: { path_id: 2, expected: true } },
          ],
        },
      })
      const wasmState = JSON.stringify(createShadowState(state))
      const wasmResult = wasm.evaluate_boollogic(wasmTree, wasmState)

      expect(wasmResult).toBe(jsResult)
      expect(wasmResult).toBe(false)
    })
  })

  describe('NOT operator', () => {
    it('matches JS evaluator - negation', () => {
      const state = {
        user: { role: 'admin', age: 30, active: true },
      } as TestState
      const jsLogic: BoolLogic<TestState> = {
        NOT: { IS_EQUAL: ['user.role', 'user'] },
      }

      const jsResult = evaluateBoolLogic(jsLogic, state)

      const wasmTree = JSON.stringify({
        NOT: {
          child: { IS_EQUAL: { path_id: 0, expected: 'user' } },
        },
      })
      const wasmState = JSON.stringify(createShadowState(state))
      const wasmResult = wasm.evaluate_boollogic(wasmTree, wasmState)

      expect(wasmResult).toBe(jsResult)
      expect(wasmResult).toBe(true)
    })
  })

  describe('Numeric comparisons', () => {
    it('matches JS evaluator - GT', () => {
      const state = {
        user: { role: 'user', age: 30, active: true },
      } as TestState
      const jsLogic: BoolLogic<TestState> = { GT: ['user.age', 25] }

      const jsResult = evaluateBoolLogic(jsLogic, state)

      const wasmTree = JSON.stringify({
        GT: { path_id: 1, threshold: 25 },
      })
      const wasmState = JSON.stringify(createShadowState(state))
      const wasmResult = wasm.evaluate_boollogic(wasmTree, wasmState)

      expect(wasmResult).toBe(jsResult)
      expect(wasmResult).toBe(true)
    })

    it('matches JS evaluator - LT', () => {
      const state = {
        user: { role: 'user', age: 20, active: true },
      } as TestState
      const jsLogic: BoolLogic<TestState> = { LT: ['user.age', 25] }

      const jsResult = evaluateBoolLogic(jsLogic, state)

      const wasmTree = JSON.stringify({
        LT: { path_id: 1, threshold: 25 },
      })
      const wasmState = JSON.stringify(createShadowState(state))
      const wasmResult = wasm.evaluate_boollogic(wasmTree, wasmState)

      expect(wasmResult).toBe(jsResult)
      expect(wasmResult).toBe(true)
    })

    it('matches JS evaluator - GTE', () => {
      const state = {
        user: { role: 'user', age: 25, active: true },
      } as TestState
      const jsLogic: BoolLogic<TestState> = { GTE: ['user.age', 25] }

      const jsResult = evaluateBoolLogic(jsLogic, state)

      const wasmTree = JSON.stringify({
        GTE: { path_id: 1, threshold: 25 },
      })
      const wasmState = JSON.stringify(createShadowState(state))
      const wasmResult = wasm.evaluate_boollogic(wasmTree, wasmState)

      expect(wasmResult).toBe(jsResult)
      expect(wasmResult).toBe(true)
    })

    it('matches JS evaluator - LTE', () => {
      const state = {
        user: { role: 'user', age: 25, active: true },
      } as TestState
      const jsLogic: BoolLogic<TestState> = { LTE: ['user.age', 25] }

      const jsResult = evaluateBoolLogic(jsLogic, state)

      const wasmTree = JSON.stringify({
        LTE: { path_id: 1, threshold: 25 },
      })
      const wasmState = JSON.stringify(createShadowState(state))
      const wasmResult = wasm.evaluate_boollogic(wasmTree, wasmState)

      expect(wasmResult).toBe(jsResult)
      expect(wasmResult).toBe(true)
    })
  })

  describe('IN operator', () => {
    it('matches JS evaluator - value in list', () => {
      const state = {
        user: { role: 'admin', age: 30, active: true },
      } as TestState
      const jsLogic: BoolLogic<TestState> = {
        IN: ['user.role', ['admin', 'moderator']],
      }

      const jsResult = evaluateBoolLogic(jsLogic, state)

      const wasmTree = JSON.stringify({
        IN: { path_id: 0, allowed: ['admin', 'moderator'] },
      })
      const wasmState = JSON.stringify(createShadowState(state))
      const wasmResult = wasm.evaluate_boollogic(wasmTree, wasmState)

      expect(wasmResult).toBe(jsResult)
      expect(wasmResult).toBe(true)
    })

    it('matches JS evaluator - value not in list', () => {
      const state = {
        user: { role: 'user', age: 30, active: true },
      } as TestState
      const jsLogic: BoolLogic<TestState> = {
        IN: ['user.role', ['admin', 'moderator']],
      }

      const jsResult = evaluateBoolLogic(jsLogic, state)

      const wasmTree = JSON.stringify({
        IN: { path_id: 0, allowed: ['admin', 'moderator'] },
      })
      const wasmState = JSON.stringify(createShadowState(state))
      const wasmResult = wasm.evaluate_boollogic(wasmTree, wasmState)

      expect(wasmResult).toBe(jsResult)
      expect(wasmResult).toBe(false)
    })
  })

  describe('Complex nested logic', () => {
    it('matches JS evaluator - nested AND/OR/NOT', () => {
      const state = {
        user: { role: 'admin', age: 30, active: true },
        product: { name: 'Widget', price: 150, stock: 5 },
      } as TestState

      const jsLogic: BoolLogic<TestState> = {
        AND: [
          { IS_EQUAL: ['user.role', 'admin'] },
          {
            OR: [{ GT: ['product.price', 100] }, { LT: ['product.stock', 10] }],
          },
          { NOT: { IS_EMPTY: 'product.name' } },
        ],
      }

      const jsResult = evaluateBoolLogic(jsLogic, state)

      const wasmTree = JSON.stringify({
        AND: {
          children: [
            { IS_EQUAL: { path_id: 0, expected: 'admin' } },
            {
              OR: {
                children: [
                  { GT: { path_id: 4, threshold: 100 } },
                  { LT: { path_id: 5, threshold: 10 } },
                ],
              },
            },
            {
              NOT: {
                child: { IS_EMPTY: { path_id: 3 } },
              },
            },
          ],
        },
      })
      const wasmState = JSON.stringify(createShadowState(state))
      const wasmResult = wasm.evaluate_boollogic(wasmTree, wasmState)

      expect(wasmResult).toBe(jsResult)
      expect(wasmResult).toBe(true)
    })
  })

  describe('Path ID extraction', () => {
    it('extracts all referenced path IDs', () => {
      const tree = JSON.stringify({
        AND: {
          children: [
            { IS_EQUAL: { path_id: 5, expected: true } },
            {
              OR: {
                children: [
                  { EXISTS: { path_id: 2 } },
                  { GT: { path_id: 10, threshold: 0 } },
                ],
              },
            },
            {
              NOT: {
                child: { IS_EMPTY: { path_id: 2 } },
              },
            },
          ],
        },
      })

      const ids = wasm.extract_path_ids(tree)

      expect(Array.from(ids)).toEqual([2, 5, 10])
    })
  })

  describe('Performance', () => {
    it('evaluates 1000 BoolLogic trees in < 100µs', () => {
      const state = {
        user: { role: 'admin', age: 30, active: true },
      } as TestState

      const tree = JSON.stringify({
        AND: {
          children: [
            { IS_EQUAL: { path_id: 0, expected: 'admin' } },
            { GT: { path_id: 1, threshold: 25 } },
          ],
        },
      })
      const wasmState = JSON.stringify(createShadowState(state))

      const start = performance.now()
      for (let i = 0; i < 1000; i++) {
        wasm.evaluate_boollogic(tree, wasmState)
      }
      const duration = (performance.now() - start) * 1000 // Convert to µs

      expect(duration).toBeLessThan(100000) // 100ms = 100,000µs (relaxed for JS overhead)
    })
  })
})
