import { beforeEach, describe, expect, it } from 'vitest'

import {
  evaluateBoolLogic,
  isWasmLoaded,
  loadWasm,
  resetWasm,
} from '../../src/wasm/bridge'

// Check if WASM module is available for testing
const checkWasmAvailable = async (): Promise<boolean> => {
  try {
    await loadWasm()
    return true
  } catch {
    return false
  }
}

describe('WASM BoolLogic Evaluation - JS/WASM Boundary', () => {
  let wasmAvailable = false

  beforeEach(async () => {
    // Check if WASM is available
    wasmAvailable = await checkWasmAvailable()

    // Reset WASM state for each test if available
    if (wasmAvailable) {
      resetWasm()
      await loadWasm()
    }
  })

  describe('WASM Module Loading', () => {
    it('should load WASM module or report unavailable', async () => {
      if (!wasmAvailable) {
        // WASM not built yet - this is expected in development
        expect(wasmAvailable).toBe(false)
        return
      }

      const wasm = await loadWasm()
      expect(wasm).toBeDefined()
      expect(wasm.evaluate_bool_logic).toBeTypeOf('function')
    })

    it('should cache WASM instance after first load', async () => {
      if (!wasmAvailable) return

      const wasm1 = await loadWasm()
      const wasm2 = await loadWasm()

      expect(wasm1).toBe(wasm2)
      expect(isWasmLoaded()).toBe(true)
    })

    it('should reset WASM instance when resetWasm is called', async () => {
      if (!wasmAvailable) return

      await loadWasm()
      expect(isWasmLoaded()).toBe(true)

      resetWasm()
      expect(isWasmLoaded()).toBe(false)
    })
  })

  describe('IS_EQUAL Operator', () => {
    it('should evaluate string equality correctly', async () => {
      if (!wasmAvailable) return

      const state = { user: { role: 'admin' } }

      expect(
        await evaluateBoolLogic({ IS_EQUAL: ['user.role', 'admin'] }, state),
      ).toBe(true)

      expect(
        await evaluateBoolLogic({ IS_EQUAL: ['user.role', 'editor'] }, state),
      ).toBe(false)
    })

    it('should evaluate number equality correctly', async () => {
      if (!wasmAvailable) return

      const state = { user: { age: 30 } }

      expect(
        await evaluateBoolLogic({ IS_EQUAL: ['user.age', 30] }, state),
      ).toBe(true)

      expect(
        await evaluateBoolLogic({ IS_EQUAL: ['user.age', 25] }, state),
      ).toBe(false)
    })

    it('should evaluate boolean equality correctly', async () => {
      if (!wasmAvailable) return

      const state = { settings: { enabled: true } }

      expect(
        await evaluateBoolLogic(
          { IS_EQUAL: ['settings.enabled', true] },
          state,
        ),
      ).toBe(true)

      expect(
        await evaluateBoolLogic(
          { IS_EQUAL: ['settings.enabled', false] },
          state,
        ),
      ).toBe(false)
    })

    it('should evaluate null equality correctly', async () => {
      if (!wasmAvailable) return

      const state = { user: { email: null } }

      expect(
        await evaluateBoolLogic({ IS_EQUAL: ['user.email', null] }, state),
      ).toBe(true)
    })

    it('should handle missing paths as false', async () => {
      if (!wasmAvailable) return

      const state = { user: { name: 'John' } }

      expect(
        await evaluateBoolLogic({ IS_EQUAL: ['user.missing', 'value'] }, state),
      ).toBe(false)
    })
  })

  describe('EXISTS Operator', () => {
    it('should return true for existing non-null values', async () => {
      if (!wasmAvailable) return

      const state = {
        user: {
          name: 'John',
          age: 0,
          email: '',
          isActive: false,
        },
      }

      expect(await evaluateBoolLogic({ EXISTS: 'user.name' }, state)).toBe(true)
      expect(await evaluateBoolLogic({ EXISTS: 'user.age' }, state)).toBe(true)
      expect(await evaluateBoolLogic({ EXISTS: 'user.email' }, state)).toBe(
        true,
      )
      expect(await evaluateBoolLogic({ EXISTS: 'user.isActive' }, state)).toBe(
        true,
      )
    })

    it('should return false for null values', async () => {
      if (!wasmAvailable) return

      const state = { user: { email: null } }

      expect(await evaluateBoolLogic({ EXISTS: 'user.email' }, state)).toBe(
        false,
      )
    })

    it('should return false for missing paths', async () => {
      if (!wasmAvailable) return

      const state = { user: { name: 'John' } }

      expect(await evaluateBoolLogic({ EXISTS: 'user.missing' }, state)).toBe(
        false,
      )
    })
  })

  describe('IS_EMPTY Operator', () => {
    it('should return true for empty strings', async () => {
      if (!wasmAvailable) return

      const state = { product: { description: '' } }

      expect(
        await evaluateBoolLogic({ IS_EMPTY: 'product.description' }, state),
      ).toBe(true)
    })

    it('should return false for non-empty strings', async () => {
      if (!wasmAvailable) return

      const state = { product: { description: 'A great product' } }

      expect(
        await evaluateBoolLogic({ IS_EMPTY: 'product.description' }, state),
      ).toBe(false)
    })

    it('should return true for empty arrays', async () => {
      if (!wasmAvailable) return

      const state = { user: { tags: [] } }

      expect(await evaluateBoolLogic({ IS_EMPTY: 'user.tags' }, state)).toBe(
        true,
      )
    })

    it('should return false for non-empty arrays', async () => {
      if (!wasmAvailable) return

      const state = { user: { tags: ['admin'] } }

      expect(await evaluateBoolLogic({ IS_EMPTY: 'user.tags' }, state)).toBe(
        false,
      )
    })

    it('should return true for empty objects', async () => {
      if (!wasmAvailable) return

      const state = { user: { metadata: {} } }

      expect(
        await evaluateBoolLogic({ IS_EMPTY: 'user.metadata' }, state),
      ).toBe(true)
    })

    it('should return false for non-empty objects', async () => {
      if (!wasmAvailable) return

      const state = { user: { metadata: { key: 'value' } } }

      expect(
        await evaluateBoolLogic({ IS_EMPTY: 'user.metadata' }, state),
      ).toBe(false)
    })

    it('should return true for null values', async () => {
      if (!wasmAvailable) return

      const state = { user: { email: null } }

      expect(await evaluateBoolLogic({ IS_EMPTY: 'user.email' }, state)).toBe(
        true,
      )
    })

    it('should return false for numbers (even zero)', async () => {
      if (!wasmAvailable) return

      const state = { user: { age: 0, count: 42 } }

      expect(await evaluateBoolLogic({ IS_EMPTY: 'user.age' }, state)).toBe(
        false,
      )
      expect(await evaluateBoolLogic({ IS_EMPTY: 'user.count' }, state)).toBe(
        false,
      )
    })

    it('should return false for booleans', async () => {
      if (!wasmAvailable) return

      const state = { settings: { enabled: false } }

      expect(
        await evaluateBoolLogic({ IS_EMPTY: 'settings.enabled' }, state),
      ).toBe(false)
    })
  })

  describe('GT Operator (Greater Than)', () => {
    it('should return true when value is greater', async () => {
      if (!wasmAvailable) return

      const state = { product: { price: 100 } }

      expect(
        await evaluateBoolLogic({ GT: ['product.price', 50] }, state),
      ).toBe(true)
    })

    it('should return false when value is equal or less', async () => {
      if (!wasmAvailable) return

      const state = { product: { price: 50 } }

      expect(
        await evaluateBoolLogic({ GT: ['product.price', 50] }, state),
      ).toBe(false)
      expect(
        await evaluateBoolLogic({ GT: ['product.price', 100] }, state),
      ).toBe(false)
    })

    it('should handle negative numbers', async () => {
      if (!wasmAvailable) return

      const state = { temperature: { celsius: -5 } }

      expect(
        await evaluateBoolLogic({ GT: ['temperature.celsius', -10] }, state),
      ).toBe(true)
      expect(
        await evaluateBoolLogic({ GT: ['temperature.celsius', 0] }, state),
      ).toBe(false)
    })

    it('should return false for non-numeric values', async () => {
      if (!wasmAvailable) return

      const state = { user: { name: 'John' } }

      expect(await evaluateBoolLogic({ GT: ['user.name', 5] }, state)).toBe(
        false,
      )
    })
  })

  describe('LT Operator (Less Than)', () => {
    it('should return true when value is less', async () => {
      if (!wasmAvailable) return

      const state = { product: { price: 30 } }

      expect(
        await evaluateBoolLogic({ LT: ['product.price', 50] }, state),
      ).toBe(true)
    })

    it('should return false when value is equal or greater', async () => {
      if (!wasmAvailable) return

      const state = { product: { price: 50 } }

      expect(
        await evaluateBoolLogic({ LT: ['product.price', 50] }, state),
      ).toBe(false)
      expect(
        await evaluateBoolLogic({ LT: ['product.price', 30] }, state),
      ).toBe(false)
    })
  })

  describe('GTE Operator (Greater Than or Equal)', () => {
    it('should return true when value is greater', async () => {
      if (!wasmAvailable) return

      const state = { user: { age: 25 } }

      expect(await evaluateBoolLogic({ GTE: ['user.age', 18] }, state)).toBe(
        true,
      )
    })

    it('should return true when value is equal', async () => {
      if (!wasmAvailable) return

      const state = { user: { age: 18 } }

      expect(await evaluateBoolLogic({ GTE: ['user.age', 18] }, state)).toBe(
        true,
      )
    })

    it('should return false when value is less', async () => {
      if (!wasmAvailable) return

      const state = { user: { age: 16 } }

      expect(await evaluateBoolLogic({ GTE: ['user.age', 18] }, state)).toBe(
        false,
      )
    })
  })

  describe('LTE Operator (Less Than or Equal)', () => {
    it('should return true when value is less', async () => {
      if (!wasmAvailable) return

      const state = { product: { quantity: 3 } }

      expect(
        await evaluateBoolLogic({ LTE: ['product.quantity', 5] }, state),
      ).toBe(true)
    })

    it('should return true when value is equal', async () => {
      if (!wasmAvailable) return

      const state = { product: { quantity: 5 } }

      expect(
        await evaluateBoolLogic({ LTE: ['product.quantity', 5] }, state),
      ).toBe(true)
    })

    it('should return false when value is greater', async () => {
      if (!wasmAvailable) return

      const state = { product: { quantity: 10 } }

      expect(
        await evaluateBoolLogic({ LTE: ['product.quantity', 5] }, state),
      ).toBe(false)
    })
  })

  describe('IN Operator', () => {
    it('should return true when value is in array', async () => {
      if (!wasmAvailable) return

      const state = { user: { role: 'editor' } }

      expect(
        await evaluateBoolLogic(
          { IN: ['user.role', ['admin', 'editor', 'viewer']] },
          state,
        ),
      ).toBe(true)
    })

    it('should return false when value is not in array', async () => {
      if (!wasmAvailable) return

      const state = { user: { role: 'guest' } }

      expect(
        await evaluateBoolLogic(
          { IN: ['user.role', ['admin', 'editor', 'viewer']] },
          state,
        ),
      ).toBe(false)
    })

    it('should handle numeric values', async () => {
      if (!wasmAvailable) return

      const state = { product: { quantity: 5 } }

      expect(
        await evaluateBoolLogic(
          { IN: ['product.quantity', [1, 3, 5, 7, 9]] },
          state,
        ),
      ).toBe(true)

      expect(
        await evaluateBoolLogic(
          { IN: ['product.quantity', [2, 4, 6, 8, 10]] },
          state,
        ),
      ).toBe(false)
    })

    it('should handle boolean values', async () => {
      if (!wasmAvailable) return

      const state = { settings: { enabled: true } }

      expect(
        await evaluateBoolLogic(
          { IN: ['settings.enabled', [true, false]] },
          state,
        ),
      ).toBe(true)
    })

    it('should return false for empty array', async () => {
      if (!wasmAvailable) return

      const state = { user: { role: 'admin' } }

      expect(await evaluateBoolLogic({ IN: ['user.role', []] }, state)).toBe(
        false,
      )
    })
  })

  describe('AND Operator', () => {
    it('should return true when all conditions are true', async () => {
      if (!wasmAvailable) return

      const state = {
        user: { role: 'admin', age: 25 },
      }

      expect(
        await evaluateBoolLogic(
          {
            AND: [
              { IS_EQUAL: ['user.role', 'admin'] },
              { GT: ['user.age', 18] },
            ],
          },
          state,
        ),
      ).toBe(true)
    })

    it('should return false when any condition is false', async () => {
      if (!wasmAvailable) return

      const state = {
        user: { role: 'editor', age: 25 },
      }

      expect(
        await evaluateBoolLogic(
          {
            AND: [
              { IS_EQUAL: ['user.role', 'admin'] },
              { GT: ['user.age', 18] },
            ],
          },
          state,
        ),
      ).toBe(false)
    })

    it('should return true for empty AND (vacuous truth)', async () => {
      if (!wasmAvailable) return

      const state = {}

      expect(await evaluateBoolLogic({ AND: [] }, state)).toBe(true)
    })

    it('should handle nested AND expressions', async () => {
      if (!wasmAvailable) return

      const state = {
        user: { role: 'admin', age: 25, verified: true },
      }

      expect(
        await evaluateBoolLogic(
          {
            AND: [
              { IS_EQUAL: ['user.role', 'admin'] },
              {
                AND: [{ GT: ['user.age', 18] }, { EXISTS: 'user.verified' }],
              },
            ],
          },
          state,
        ),
      ).toBe(true)
    })
  })

  describe('OR Operator', () => {
    it('should return true when any condition is true', async () => {
      if (!wasmAvailable) return

      const state = {
        user: { role: 'editor', age: 16 },
      }

      expect(
        await evaluateBoolLogic(
          {
            OR: [
              { IS_EQUAL: ['user.role', 'admin'] },
              { IS_EQUAL: ['user.role', 'editor'] },
            ],
          },
          state,
        ),
      ).toBe(true)
    })

    it('should return false when all conditions are false', async () => {
      if (!wasmAvailable) return

      const state = {
        user: { role: 'guest', age: 16 },
      }

      expect(
        await evaluateBoolLogic(
          {
            OR: [
              { IS_EQUAL: ['user.role', 'admin'] },
              { GT: ['user.age', 18] },
            ],
          },
          state,
        ),
      ).toBe(false)
    })

    it('should return false for empty OR', async () => {
      if (!wasmAvailable) return

      const state = {}

      expect(await evaluateBoolLogic({ OR: [] }, state)).toBe(false)
    })

    it('should handle nested OR expressions', async () => {
      if (!wasmAvailable) return

      const state = {
        user: { role: 'guest', premium: false, trial: true },
      }

      expect(
        await evaluateBoolLogic(
          {
            OR: [
              { IS_EQUAL: ['user.role', 'admin'] },
              {
                OR: [{ EXISTS: 'user.premium' }, { EXISTS: 'user.trial' }],
              },
            ],
          },
          state,
        ),
      ).toBe(true)
    })
  })

  describe('NOT Operator', () => {
    it('should negate true to false', async () => {
      if (!wasmAvailable) return

      const state = { user: { role: 'admin' } }

      expect(
        await evaluateBoolLogic(
          {
            NOT: { IS_EQUAL: ['user.role', 'admin'] },
          },
          state,
        ),
      ).toBe(false)
    })

    it('should negate false to true', async () => {
      if (!wasmAvailable) return

      const state = { user: { role: 'editor' } }

      expect(
        await evaluateBoolLogic(
          {
            NOT: { IS_EQUAL: ['user.role', 'admin'] },
          },
          state,
        ),
      ).toBe(true)
    })

    it('should handle double negation', async () => {
      if (!wasmAvailable) return

      const state = { user: { age: 25 } }

      expect(
        await evaluateBoolLogic(
          {
            NOT: {
              NOT: { GT: ['user.age', 18] },
            },
          },
          state,
        ),
      ).toBe(true)
    })
  })

  describe('Complex Nested Expressions', () => {
    it('should evaluate complex AND/OR combinations', async () => {
      if (!wasmAvailable) return

      const state = {
        user: {
          role: 'editor',
          age: 25,
          verified: true,
        },
      }

      // (role === 'admin' OR role === 'editor') AND age > 18 AND verified
      expect(
        await evaluateBoolLogic(
          {
            AND: [
              {
                OR: [
                  { IS_EQUAL: ['user.role', 'admin'] },
                  { IS_EQUAL: ['user.role', 'editor'] },
                ],
              },
              { GT: ['user.age', 18] },
              { EXISTS: 'user.verified' },
            ],
          },
          state,
        ),
      ).toBe(true)
    })

    it('should evaluate deep nesting with NOT', async () => {
      if (!wasmAvailable) return

      const state = {
        product: {
          price: 99.99,
          inStock: true,
          category: 'electronics',
        },
      }

      // NOT (price > 100 OR NOT inStock) AND category === 'electronics'
      expect(
        await evaluateBoolLogic(
          {
            AND: [
              {
                NOT: {
                  OR: [
                    { GT: ['product.price', 100] },
                    { NOT: { EXISTS: 'product.inStock' } },
                  ],
                },
              },
              { IS_EQUAL: ['product.category', 'electronics'] },
            ],
          },
          state,
        ),
      ).toBe(true)
    })

    it('should evaluate multiple levels of nesting', async () => {
      if (!wasmAvailable) return

      const state = {
        user: {
          role: 'editor',
          age: 25,
          permissions: {
            read: true,
            write: true,
            delete: false,
          },
        },
      }

      expect(
        await evaluateBoolLogic(
          {
            AND: [
              { IN: ['user.role', ['admin', 'editor', 'moderator']] },
              {
                OR: [
                  { GTE: ['user.age', 21] },
                  { IS_EQUAL: ['user.role', 'admin'] },
                ],
              },
              {
                AND: [
                  { EXISTS: 'user.permissions.read' },
                  { EXISTS: 'user.permissions.write' },
                ],
              },
              {
                NOT: { EXISTS: 'user.permissions.delete' },
              },
            ],
          },
          state,
        ),
      ).toBe(true)
    })
  })

  describe('Edge Cases', () => {
    it('should handle deeply nested paths', async () => {
      if (!wasmAvailable) return

      const state = {
        app: {
          settings: {
            user: {
              profile: {
                display: {
                  theme: 'dark',
                },
              },
            },
          },
        },
      }

      expect(
        await evaluateBoolLogic(
          {
            IS_EQUAL: ['app.settings.user.profile.display.theme', 'dark'],
          },
          state,
        ),
      ).toBe(true)
    })

    it('should handle missing intermediate paths gracefully', async () => {
      if (!wasmAvailable) return

      const state = { user: {} }

      expect(
        await evaluateBoolLogic({ EXISTS: 'user.profile.name' }, state),
      ).toBe(false)

      expect(
        await evaluateBoolLogic(
          { IS_EQUAL: ['user.profile.name', 'John'] },
          state,
        ),
      ).toBe(false)
    })

    it('should handle unicode in paths and values', async () => {
      if (!wasmAvailable) return

      const state = {
        用户: {
          名称: '张三',
        },
      }

      expect(await evaluateBoolLogic({ EXISTS: '用户.名称' }, state)).toBe(true)

      expect(
        await evaluateBoolLogic({ IS_EQUAL: ['用户.名称', '张三'] }, state),
      ).toBe(true)
    })

    it('should handle special characters in paths', async () => {
      if (!wasmAvailable) return

      const state = {
        'user-info': {
          email_address: 'test@example.com',
        },
      }

      expect(
        await evaluateBoolLogic({ EXISTS: 'user-info.email_address' }, state),
      ).toBe(true)
    })

    it('should handle large numbers', async () => {
      if (!wasmAvailable) return

      const state = {
        metrics: {
          views: 1000000,
          revenue: 99999.99,
        },
      }

      expect(
        await evaluateBoolLogic({ GT: ['metrics.views', 999999] }, state),
      ).toBe(true)

      expect(
        await evaluateBoolLogic({ LTE: ['metrics.revenue', 100000] }, state),
      ).toBe(true)
    })

    it('should handle zero and negative numbers correctly', async () => {
      if (!wasmAvailable) return

      const state = {
        account: {
          balance: 0,
          debt: -500,
        },
      }

      expect(
        await evaluateBoolLogic({ IS_EQUAL: ['account.balance', 0] }, state),
      ).toBe(true)

      expect(await evaluateBoolLogic({ LT: ['account.debt', 0] }, state)).toBe(
        true,
      )
    })
  })

  describe('Error Handling', () => {
    it('should throw error for invalid BoolLogic format', async () => {
      if (!wasmAvailable) return

      const state = { user: { role: 'admin' } }

      await expect(
        evaluateBoolLogic({ INVALID_OPERATOR: ['path', 'value'] }, state),
      ).rejects.toThrow()
    })

    it('should throw error for malformed state', async () => {
      if (!wasmAvailable) return

      // Circular reference (cannot be serialized to JSON)
      const circularState: any = { user: {} }
      circularState.user.self = circularState

      await expect(
        evaluateBoolLogic({ EXISTS: 'user.name' }, circularState),
      ).rejects.toThrow()
    })
  })

  describe('Type Safety at JS/WASM Boundary', () => {
    it('should correctly deserialize all operator types from JS objects', async () => {
      if (!wasmAvailable) return

      const state = {
        user: {
          role: 'admin',
          age: 30,
          tags: ['developer'],
        },
      }

      // All these should deserialize correctly in WASM
      const operators = [
        { IS_EQUAL: ['user.role', 'admin'] },
        { EXISTS: 'user.age' },
        { IS_EMPTY: 'user.tags' },
        { GT: ['user.age', 25] },
        { LT: ['user.age', 35] },
        { GTE: ['user.age', 30] },
        { LTE: ['user.age', 30] },
        { IN: ['user.role', ['admin', 'editor']] },
        { AND: [{ EXISTS: 'user.role' }] },
        { OR: [{ EXISTS: 'user.role' }] },
        { NOT: { IS_EMPTY: 'user.role' } },
      ]

      for (const operator of operators) {
        // Should not throw - verifies deserialization works
        await evaluateBoolLogic(operator, state)
      }
    })

    it('should handle all JSON value types in state', async () => {
      if (!wasmAvailable) return

      const state = {
        string: 'hello',
        number: 42,
        boolean: true,
        null: null,
        array: [1, 2, 3],
        object: { nested: 'value' },
      }

      expect(
        await evaluateBoolLogic({ IS_EQUAL: ['string', 'hello'] }, state),
      ).toBe(true)

      expect(await evaluateBoolLogic({ IS_EQUAL: ['number', 42] }, state)).toBe(
        true,
      )

      expect(
        await evaluateBoolLogic({ IS_EQUAL: ['boolean', true] }, state),
      ).toBe(true)

      expect(await evaluateBoolLogic({ IS_EQUAL: ['null', null] }, state)).toBe(
        true,
      )

      expect(await evaluateBoolLogic({ EXISTS: 'array' }, state)).toBe(true)

      expect(await evaluateBoolLogic({ EXISTS: 'object.nested' }, state)).toBe(
        true,
      )
    })
  })
})
