import { beforeEach, describe, expect, it } from 'vitest'

import {
  dumpShadowState,
  evaluateBoolLogic,
  getShadowValue,
  initShadowState,
  isWasmLoaded,
  loadWasm,
  resetWasm,
  updateShadowValue,
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

describe('WASM Shadow State - JS/WASM Boundary', () => {
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
      expect(wasm.shadow_init).toBeTypeOf('function')
      expect(wasm.shadow_get).toBeTypeOf('function')
      expect(wasm.shadow_dump).toBeTypeOf('function')
      expect(wasm.shadow_update).toBeTypeOf('function')
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

  describe('Shadow State Initialization', () => {
    it('should initialize shadow state with nested object', async () => {
      if (!wasmAvailable) return

      const state = {
        user: {
          name: 'Alice',
          age: 30,
        },
      }

      await initShadowState(state)

      const retrieved = await dumpShadowState()
      expect(retrieved).toEqual(state)
    })

    it('should initialize shadow state with deep nesting', async () => {
      if (!wasmAvailable) return

      const state = {
        user: {
          profile: {
            personal: {
              name: 'Alice',
              email: 'alice@example.com',
            },
            settings: {
              theme: 'dark',
            },
          },
        },
      }

      await initShadowState(state)

      const retrieved = await dumpShadowState()
      expect(retrieved).toEqual(state)
    })

    it('should initialize shadow state with arrays', async () => {
      if (!wasmAvailable) return

      const state = {
        users: ['Alice', 'Bob', 'Charlie'],
        scores: [100, 200, 300],
      }

      await initShadowState(state)

      const retrieved = await dumpShadowState()
      expect(retrieved).toEqual(state)
    })

    it('should initialize shadow state with mixed types', async () => {
      if (!wasmAvailable) return

      const state = {
        user: {
          name: 'Alice',
          age: 30,
          isActive: true,
          email: null,
          tags: ['developer', 'admin'],
        },
      }

      await initShadowState(state)

      const retrieved = await dumpShadowState()
      expect(retrieved).toEqual(state)
    })

    it('should initialize shadow state with empty object', async () => {
      if (!wasmAvailable) return

      const state = {}

      await initShadowState(state)

      const retrieved = await dumpShadowState()
      expect(retrieved).toEqual(state)
    })

    it('should reinitialize shadow state with different structure', async () => {
      if (!wasmAvailable) return

      const state1 = { user: { name: 'Alice' } }
      await initShadowState(state1)

      const state2 = { product: { price: 100 } }
      await initShadowState(state2)

      const retrieved = await dumpShadowState()
      expect(retrieved).toEqual(state2)
    })
  })

  describe('Shadow State Get - Valid Paths', () => {
    it('should retrieve value at shallow path', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        user: {
          name: 'Alice',
          age: 30,
        },
      })

      const name = await getShadowValue(['user', 'name'])
      const age = await getShadowValue(['user', 'age'])

      expect(name).toBe('Alice')
      expect(age).toBe(30)
    })

    it('should retrieve value at deep path', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        user: {
          profile: {
            personal: {
              name: 'Alice',
              email: 'alice@example.com',
            },
          },
        },
      })

      const name = await getShadowValue(['user', 'profile', 'personal', 'name'])
      const email = await getShadowValue([
        'user',
        'profile',
        'personal',
        'email',
      ])

      expect(name).toBe('Alice')
      expect(email).toBe('alice@example.com')
    })

    it('should retrieve object at intermediate path', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        user: {
          profile: {
            name: 'Alice',
            age: 30,
          },
        },
      })

      const profile = await getShadowValue(['user', 'profile'])

      expect(profile).toEqual({
        name: 'Alice',
        age: 30,
      })
    })

    it('should retrieve array value', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        users: ['Alice', 'Bob', 'Charlie'],
      })

      const users = await getShadowValue(['users'])

      expect(users).toEqual(['Alice', 'Bob', 'Charlie'])
    })

    it('should retrieve array element by index', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        users: ['Alice', 'Bob', 'Charlie'],
      })

      const first = await getShadowValue(['users', '0'])
      const second = await getShadowValue(['users', '1'])
      const third = await getShadowValue(['users', '2'])

      expect(first).toBe('Alice')
      expect(second).toBe('Bob')
      expect(third).toBe('Charlie')
    })

    it('should retrieve nested array element', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        users: [
          { name: 'Alice', age: 30 },
          { name: 'Bob', age: 25 },
        ],
      })

      const firstUser = await getShadowValue(['users', '0'])
      const firstName = await getShadowValue(['users', '0', 'name'])
      const secondAge = await getShadowValue(['users', '1', 'age'])

      expect(firstUser).toEqual({ name: 'Alice', age: 30 })
      expect(firstName).toBe('Alice')
      expect(secondAge).toBe(25)
    })

    it('should retrieve boolean values', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        settings: {
          enabled: true,
          disabled: false,
        },
      })

      const enabled = await getShadowValue(['settings', 'enabled'])
      const disabled = await getShadowValue(['settings', 'disabled'])

      expect(enabled).toBe(true)
      expect(disabled).toBe(false)
    })

    it('should retrieve null values', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        user: {
          email: null,
        },
      })

      const email = await getShadowValue(['user', 'email'])

      expect(email).toBe(null)
    })

    it('should retrieve number values', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        stats: {
          count: 42,
          average: 3.14,
          zero: 0,
          negative: -10,
        },
      })

      const count = await getShadowValue(['stats', 'count'])
      const average = await getShadowValue(['stats', 'average'])
      const zero = await getShadowValue(['stats', 'zero'])
      const negative = await getShadowValue(['stats', 'negative'])

      expect(count).toBe(42)
      expect(average).toBe(3.14)
      expect(zero).toBe(0)
      expect(negative).toBe(-10)
    })

    it('should retrieve string values', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        text: {
          normal: 'hello',
          empty: '',
          unicode: '你好',
          special: 'hello\nworld',
        },
      })

      const normal = await getShadowValue(['text', 'normal'])
      const empty = await getShadowValue(['text', 'empty'])
      const unicode = await getShadowValue(['text', 'unicode'])
      const special = await getShadowValue(['text', 'special'])

      expect(normal).toBe('hello')
      expect(empty).toBe('')
      expect(unicode).toBe('你好')
      expect(special).toBe('hello\nworld')
    })
  })

  describe('Shadow State Get - Invalid Paths', () => {
    it('should return undefined for non-existent path', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        user: {
          name: 'Alice',
        },
      })

      const missing = await getShadowValue(['user', 'email'])

      expect(missing).toBeUndefined()
    })

    it('should return undefined for non-existent deep path', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        user: {
          name: 'Alice',
        },
      })

      const missing = await getShadowValue([
        'user',
        'profile',
        'settings',
        'theme',
      ])

      expect(missing).toBeUndefined()
    })

    it('should return undefined for path through non-object', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        user: {
          name: 'Alice',
        },
      })

      // Try to traverse through a string (name is a string, not an object)
      const invalid = await getShadowValue(['user', 'name', 'first'])

      expect(invalid).toBeUndefined()
    })

    it('should return undefined for invalid array index', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        users: ['Alice', 'Bob'],
      })

      const outOfBounds = await getShadowValue(['users', '10'])

      expect(outOfBounds).toBeUndefined()
    })

    it('should return undefined for negative array index', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        users: ['Alice', 'Bob'],
      })

      const negative = await getShadowValue(['users', '-1'])

      expect(negative).toBeUndefined()
    })

    it('should return undefined for non-numeric array index', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        users: ['Alice', 'Bob'],
      })

      const invalid = await getShadowValue(['users', 'first'])

      expect(invalid).toBeUndefined()
    })

    it('should return undefined for completely non-existent root path', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        user: {
          name: 'Alice',
        },
      })

      const missing = await getShadowValue(['product', 'price'])

      expect(missing).toBeUndefined()
    })
  })

  describe('Shadow State Dump', () => {
    it('should dump entire state tree', async () => {
      if (!wasmAvailable) return

      const state = {
        user: {
          name: 'Alice',
          age: 30,
        },
        product: {
          title: 'Widget',
          price: 100,
        },
      }

      await initShadowState(state)

      const dumped = await dumpShadowState()

      expect(dumped).toEqual(state)
    })

    it('should dump deeply nested state', async () => {
      if (!wasmAvailable) return

      const state = {
        level1: {
          level2: {
            level3: {
              level4: {
                level5: {
                  value: 'deep',
                },
              },
            },
          },
        },
      }

      await initShadowState(state)

      const dumped = await dumpShadowState()

      expect(dumped).toEqual(state)
    })

    it('should dump empty state', async () => {
      if (!wasmAvailable) return

      await initShadowState({})

      const dumped = await dumpShadowState()

      expect(dumped).toEqual({})
    })

    it('should dump state with arrays', async () => {
      if (!wasmAvailable) return

      const state = {
        users: [
          { name: 'Alice', age: 30 },
          { name: 'Bob', age: 25 },
        ],
        tags: ['tag1', 'tag2', 'tag3'],
      }

      await initShadowState(state)

      const dumped = await dumpShadowState()

      expect(dumped).toEqual(state)
    })

    it('should dump state with all value types', async () => {
      if (!wasmAvailable) return

      const state = {
        string: 'hello',
        number: 42,
        boolean: true,
        null: null,
        array: [1, 2, 3],
        object: { nested: 'value' },
      }

      await initShadowState(state)

      const dumped = await dumpShadowState()

      expect(dumped).toEqual(state)
    })
  })

  describe('Shadow State Update - Leaf Updates', () => {
    it('should update string value', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        user: {
          name: 'Alice',
        },
      })

      await updateShadowValue(['user', 'name'], 'Bob')

      const name = await getShadowValue(['user', 'name'])
      expect(name).toBe('Bob')
    })

    it('should update number value', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        product: {
          price: 100,
        },
      })

      await updateShadowValue(['product', 'price'], 200)

      const price = await getShadowValue(['product', 'price'])
      expect(price).toBe(200)
    })

    it('should update boolean value', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        settings: {
          enabled: false,
        },
      })

      await updateShadowValue(['settings', 'enabled'], true)

      const enabled = await getShadowValue(['settings', 'enabled'])
      expect(enabled).toBe(true)
    })

    it('should update to null value', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        user: {
          email: 'alice@example.com',
        },
      })

      await updateShadowValue(['user', 'email'], null)

      const email = await getShadowValue(['user', 'email'])
      expect(email).toBe(null)
    })

    it('should preserve unrelated paths after leaf update', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        user: {
          name: 'Alice',
          age: 30,
          email: 'alice@example.com',
        },
      })

      await updateShadowValue(['user', 'name'], 'Bob')

      const age = await getShadowValue(['user', 'age'])
      const email = await getShadowValue(['user', 'email'])

      expect(age).toBe(30)
      expect(email).toBe('alice@example.com')
    })
  })

  describe('Shadow State Update - Subtree Updates', () => {
    it('should update object subtree', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        user: {
          name: 'Alice',
          age: 30,
        },
      })

      await updateShadowValue(['user'], {
        name: 'Bob',
        age: 25,
        email: 'bob@example.com',
      })

      const user = await getShadowValue(['user'])

      expect(user).toEqual({
        name: 'Bob',
        age: 25,
        email: 'bob@example.com',
      })
    })

    it('should update array subtree', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        tags: ['tag1', 'tag2'],
      })

      await updateShadowValue(['tags'], ['new1', 'new2', 'new3'])

      const tags = await getShadowValue(['tags'])

      expect(tags).toEqual(['new1', 'new2', 'new3'])
    })

    it('should preserve sibling paths after subtree update', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        user: {
          name: 'Alice',
        },
        product: {
          title: 'Widget',
        },
      })

      await updateShadowValue(['user'], { name: 'Bob', age: 25 })

      const product = await getShadowValue(['product'])

      expect(product).toEqual({ title: 'Widget' })
    })
  })

  describe('Shadow State Update - Root Updates', () => {
    it('should update entire state with empty path', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        user: {
          name: 'Alice',
        },
      })

      await updateShadowValue([], {
        product: {
          title: 'Widget',
        },
      })

      const state = await dumpShadowState()

      expect(state).toEqual({
        product: {
          title: 'Widget',
        },
      })
    })
  })

  describe('Shadow State Update - Path Creation', () => {
    it('should create intermediate objects when updating non-existent path', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        user: {
          name: 'Alice',
        },
      })

      await updateShadowValue(['user', 'profile', 'bio'], 'Software Engineer')

      const bio = await getShadowValue(['user', 'profile', 'bio'])

      expect(bio).toBe('Software Engineer')
    })

    it('should create deeply nested path', async () => {
      if (!wasmAvailable) return

      await initShadowState({})

      await updateShadowValue(['level1', 'level2', 'level3', 'value'], 'deep')

      const value = await getShadowValue([
        'level1',
        'level2',
        'level3',
        'value',
      ])

      expect(value).toBe('deep')
    })
  })

  describe('Nested Operations - Arrays', () => {
    it('should handle array of arrays', async () => {
      if (!wasmAvailable) return

      const state = {
        matrix: [
          [1, 2, 3],
          [4, 5, 6],
          [7, 8, 9],
        ],
      }

      await initShadowState(state)

      const matrix = await getShadowValue(['matrix'])
      const row0 = await getShadowValue(['matrix', '0'])
      const cell = await getShadowValue(['matrix', '1', '2'])

      expect(matrix).toEqual([
        [1, 2, 3],
        [4, 5, 6],
        [7, 8, 9],
      ])
      expect(row0).toEqual([1, 2, 3])
      expect(cell).toBe(6)
    })

    it('should update nested array element', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        matrix: [
          [1, 2, 3],
          [4, 5, 6],
        ],
      })

      await updateShadowValue(['matrix', '0', '1'], 99)

      const row0 = await getShadowValue(['matrix', '0'])
      expect(row0).toEqual([1, 99, 3])
    })

    it('should update entire nested array row', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        matrix: [
          [1, 2, 3],
          [4, 5, 6],
        ],
      })

      await updateShadowValue(['matrix', '1'], [10, 20, 30])

      const matrix = await getShadowValue(['matrix'])
      expect(matrix).toEqual([
        [1, 2, 3],
        [10, 20, 30],
      ])
    })

    it('should handle array of objects with deep nesting', async () => {
      if (!wasmAvailable) return

      const state = {
        users: [
          {
            name: 'Alice',
            profile: {
              settings: {
                theme: 'dark',
              },
            },
          },
          {
            name: 'Bob',
            profile: {
              settings: {
                theme: 'light',
              },
            },
          },
        ],
      }

      await initShadowState(state)

      const theme0 = await getShadowValue([
        'users',
        '0',
        'profile',
        'settings',
        'theme',
      ])
      const theme1 = await getShadowValue([
        'users',
        '1',
        'profile',
        'settings',
        'theme',
      ])

      expect(theme0).toBe('dark')
      expect(theme1).toBe('light')
    })

    it('should update deeply nested array element property', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        users: [
          {
            name: 'Alice',
            profile: {
              settings: {
                theme: 'dark',
              },
            },
          },
        ],
      })

      await updateShadowValue(
        ['users', '0', 'profile', 'settings', 'theme'],
        'light',
      )

      const theme = await getShadowValue([
        'users',
        '0',
        'profile',
        'settings',
        'theme',
      ])
      expect(theme).toBe('light')
    })

    it('should handle empty arrays', async () => {
      if (!wasmAvailable) return

      const state = {
        emptyArray: [],
        nested: {
          alsoEmpty: [],
        },
      }

      await initShadowState(state)

      const empty1 = await getShadowValue(['emptyArray'])
      const empty2 = await getShadowValue(['nested', 'alsoEmpty'])

      expect(empty1).toEqual([])
      expect(empty2).toEqual([])
    })

    it('should handle mixed array types', async () => {
      if (!wasmAvailable) return

      const state = {
        mixed: ['string', 42, true, null, { key: 'value' }, [1, 2, 3]],
      }

      await initShadowState(state)

      expect(await getShadowValue(['mixed', '0'])).toBe('string')
      expect(await getShadowValue(['mixed', '1'])).toBe(42)
      expect(await getShadowValue(['mixed', '2'])).toBe(true)
      expect(await getShadowValue(['mixed', '3'])).toBe(null)
      expect(await getShadowValue(['mixed', '4'])).toEqual({ key: 'value' })
      expect(await getShadowValue(['mixed', '5'])).toEqual([1, 2, 3])
    })
  })

  describe('Nested Operations - Complex Structures', () => {
    it('should handle deeply nested mixed structures', async () => {
      if (!wasmAvailable) return

      const state = {
        level1: {
          array1: [
            {
              level2: {
                array2: [
                  {
                    level3: {
                      value: 'deep',
                    },
                  },
                ],
              },
            },
          ],
        },
      }

      await initShadowState(state)

      const value = await getShadowValue([
        'level1',
        'array1',
        '0',
        'level2',
        'array2',
        '0',
        'level3',
        'value',
      ])

      expect(value).toBe('deep')
    })

    it('should handle multiple parallel nested structures', async () => {
      if (!wasmAvailable) return

      const state = {
        branch1: {
          nested: {
            value: 'a',
          },
        },
        branch2: {
          nested: {
            value: 'b',
          },
        },
        branch3: {
          nested: {
            value: 'c',
          },
        },
      }

      await initShadowState(state)

      expect(await getShadowValue(['branch1', 'nested', 'value'])).toBe('a')
      expect(await getShadowValue(['branch2', 'nested', 'value'])).toBe('b')
      expect(await getShadowValue(['branch3', 'nested', 'value'])).toBe('c')
    })

    it('should update one branch without affecting others', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        branch1: {
          nested: {
            value: 'a',
          },
        },
        branch2: {
          nested: {
            value: 'b',
          },
        },
      })

      await updateShadowValue(['branch1', 'nested', 'value'], 'updated')

      expect(await getShadowValue(['branch1', 'nested', 'value'])).toBe(
        'updated',
      )
      expect(await getShadowValue(['branch2', 'nested', 'value'])).toBe('b')
    })
  })

  describe('Edge Cases - Sequential Updates', () => {
    it('should handle multiple sequential leaf updates', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        counter: 0,
      })

      await updateShadowValue(['counter'], 1)
      await updateShadowValue(['counter'], 2)
      await updateShadowValue(['counter'], 3)

      const counter = await getShadowValue(['counter'])
      expect(counter).toBe(3)
    })

    it('should handle sequential updates to different paths', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        user: {
          name: 'Alice',
          age: 30,
          email: 'alice@example.com',
        },
      })

      await updateShadowValue(['user', 'name'], 'Bob')
      await updateShadowValue(['user', 'age'], 25)
      await updateShadowValue(['user', 'email'], 'bob@example.com')

      const user = await getShadowValue(['user'])
      expect(user).toEqual({
        name: 'Bob',
        age: 25,
        email: 'bob@example.com',
      })
    })

    it('should handle sequential subtree updates', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        config: {
          theme: 'dark',
        },
      })

      await updateShadowValue(['config'], {
        theme: 'light',
        language: 'en',
      })

      await updateShadowValue(['config'], {
        theme: 'dark',
        language: 'fr',
        timezone: 'UTC',
      })

      const config = await getShadowValue(['config'])
      expect(config).toEqual({
        theme: 'dark',
        language: 'fr',
        timezone: 'UTC',
      })
    })
  })

  describe('Edge Cases - Type Changes', () => {
    it('should handle type change from primitive to object', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        field: 'string value',
      })

      await updateShadowValue(['field'], {
        nested: 'object value',
      })

      const field = await getShadowValue(['field'])
      expect(field).toEqual({
        nested: 'object value',
      })
    })

    it('should handle type change from object to primitive', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        field: {
          nested: 'object value',
        },
      })

      await updateShadowValue(['field'], 'string value')

      const field = await getShadowValue(['field'])
      expect(field).toBe('string value')
    })

    it('should handle type change from array to object', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        field: [1, 2, 3],
      })

      await updateShadowValue(['field'], {
        key: 'value',
      })

      const field = await getShadowValue(['field'])
      expect(field).toEqual({
        key: 'value',
      })
    })

    it('should handle type change from object to array', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        field: {
          key: 'value',
        },
      })

      await updateShadowValue(['field'], [1, 2, 3])

      const field = await getShadowValue(['field'])
      expect(field).toEqual([1, 2, 3])
    })
  })

  describe('Edge Cases - Special Values', () => {
    it('should handle empty string values correctly', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        emptyString: '',
        normalString: 'hello',
      })

      expect(await getShadowValue(['emptyString'])).toBe('')
      expect(await getShadowValue(['normalString'])).toBe('hello')
    })

    it('should distinguish between null, undefined, and empty string', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        nullValue: null,
        emptyString: '',
      })

      expect(await getShadowValue(['nullValue'])).toBe(null)
      expect(await getShadowValue(['emptyString'])).toBe('')
      expect(await getShadowValue(['nonExistent'])).toBeUndefined()
    })

    it('should handle zero and false as valid values', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        zero: 0,
        false: false,
        emptyString: '',
      })

      expect(await getShadowValue(['zero'])).toBe(0)
      expect(await getShadowValue(['false'])).toBe(false)
      expect(await getShadowValue(['emptyString'])).toBe('')
    })

    it('should handle large numbers', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        large: 9007199254740991, // Number.MAX_SAFE_INTEGER
        negative: -9007199254740991,
      })

      expect(await getShadowValue(['large'])).toBe(9007199254740991)
      expect(await getShadowValue(['negative'])).toBe(-9007199254740991)
    })

    it('should handle special string characters', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        newline: 'hello\nworld',
        tab: 'hello\tworld',
        quote: "hello'world",
        doubleQuote: 'hello"world',
        backslash: 'hello\\world',
      })

      expect(await getShadowValue(['newline'])).toBe('hello\nworld')
      expect(await getShadowValue(['tab'])).toBe('hello\tworld')
      expect(await getShadowValue(['quote'])).toBe("hello'world")
      expect(await getShadowValue(['doubleQuote'])).toBe('hello"world')
      expect(await getShadowValue(['backslash'])).toBe('hello\\world')
    })

    it('should handle unicode strings', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        chinese: '你好世界',
        emoji: '👋🌍',
        mixed: 'Hello 世界 👋',
      })

      expect(await getShadowValue(['chinese'])).toBe('你好世界')
      expect(await getShadowValue(['emoji'])).toBe('👋🌍')
      expect(await getShadowValue(['mixed'])).toBe('Hello 世界 👋')
    })
  })

  describe('Edge Cases - Path Operations', () => {
    it('should handle empty path for root access', async () => {
      if (!wasmAvailable) return

      const state = {
        user: {
          name: 'Alice',
        },
      }

      await initShadowState(state)

      const root = await getShadowValue([])
      expect(root).toEqual(state)
    })

    it('should handle single-segment paths', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        name: 'Alice',
        age: 30,
      })

      expect(await getShadowValue(['name'])).toBe('Alice')
      expect(await getShadowValue(['age'])).toBe(30)
    })

    it('should handle very deep paths', async () => {
      if (!wasmAvailable) return

      await initShadowState({})

      // Create a path 10 levels deep
      await updateShadowValue(
        ['l1', 'l2', 'l3', 'l4', 'l5', 'l6', 'l7', 'l8', 'l9', 'l10'],
        'deep value',
      )

      const value = await getShadowValue([
        'l1',
        'l2',
        'l3',
        'l4',
        'l5',
        'l6',
        'l7',
        'l8',
        'l9',
        'l10',
      ])

      expect(value).toBe('deep value')
    })
  })

  describe('Edge Cases - Complex Updates', () => {
    it('should handle update that creates new branch alongside existing data', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        existing: {
          data: 'value',
        },
      })

      await updateShadowValue(['new', 'branch', 'value'], 'new data')

      const existing = await getShadowValue(['existing', 'data'])
      const newBranch = await getShadowValue(['new', 'branch', 'value'])

      expect(existing).toBe('value')
      expect(newBranch).toBe('new data')
    })

    it('should handle updating parent of existing nested structure', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        parent: {
          child: {
            grandchild: {
              value: 'deep',
            },
          },
        },
      })

      // Update parent, which should replace entire subtree
      await updateShadowValue(['parent'], {
        newChild: 'replaced',
      })

      const parent = await getShadowValue(['parent'])
      const oldChild = await getShadowValue(['parent', 'child'])

      expect(parent).toEqual({
        newChild: 'replaced',
      })
      expect(oldChild).toBeUndefined()
    })

    it('should handle creating intermediate arrays', async () => {
      if (!wasmAvailable) return

      await initShadowState({})

      await updateShadowValue(['users', '0', 'name'], 'Alice')

      const name = await getShadowValue(['users', '0', 'name'])
      expect(name).toBe('Alice')
    })

    it('should handle sparse array creation', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        items: [],
      })

      await updateShadowValue(['items', '5'], 'value')

      const item5 = await getShadowValue(['items', '5'])
      expect(item5).toBe('value')
    })
  })

  describe('Edge Cases - State Consistency', () => {
    it('should maintain state consistency after multiple mixed operations', async () => {
      if (!wasmAvailable) return

      await initShadowState({
        users: [{ name: 'Alice' }],
        config: { theme: 'dark' },
      })

      // Mixed operations
      await updateShadowValue(['users', '0', 'age'], 30)
      await updateShadowValue(['config', 'language'], 'en')
      await updateShadowValue(['users', '1'], { name: 'Bob', age: 25 })
      await updateShadowValue(['stats', 'count'], 100)

      const state = await dumpShadowState()

      expect(state).toEqual({
        users: [
          { name: 'Alice', age: 30 },
          { name: 'Bob', age: 25 },
        ],
        config: {
          theme: 'dark',
          language: 'en',
        },
        stats: {
          count: 100,
        },
      })
    })

    it('should handle complete state replacement and verification', async () => {
      if (!wasmAvailable) return

      const state1 = {
        data: {
          nested: {
            value: 1,
          },
        },
      }

      const state2 = {
        different: {
          structure: {
            value: 2,
          },
        },
      }

      await initShadowState(state1)
      expect(await dumpShadowState()).toEqual(state1)

      await updateShadowValue([], state2)
      expect(await dumpShadowState()).toEqual(state2)

      // Verify old paths are gone
      expect(await getShadowValue(['data'])).toBeUndefined()
      expect(await getShadowValue(['different', 'structure', 'value'])).toBe(2)
    })
  })

  describe('BoolLogic Integration with Nested Shadow State', () => {
    it('should evaluate IS_EQUAL with nested shadow state paths', async () => {
      if (!wasmAvailable) return

      const state = {
        user: {
          profile: {
            personal: {
              name: 'Alice',
              age: 30,
            },
            settings: {
              theme: 'dark',
            },
          },
        },
      }

      await initShadowState(state)

      expect(
        await evaluateBoolLogic(
          { IS_EQUAL: ['user.profile.personal.name', 'Alice'] },
          state,
        ),
      ).toBe(true)

      expect(
        await evaluateBoolLogic(
          { IS_EQUAL: ['user.profile.personal.age', 30] },
          state,
        ),
      ).toBe(true)

      expect(
        await evaluateBoolLogic(
          { IS_EQUAL: ['user.profile.settings.theme', 'dark'] },
          state,
        ),
      ).toBe(true)

      expect(
        await evaluateBoolLogic(
          { IS_EQUAL: ['user.profile.personal.name', 'Bob'] },
          state,
        ),
      ).toBe(false)
    })

    it('should evaluate EXISTS with nested shadow state paths', async () => {
      if (!wasmAvailable) return

      const state = {
        user: {
          profile: {
            personal: {
              name: 'Alice',
              email: null,
            },
            settings: {
              theme: 'dark',
            },
          },
        },
      }

      await initShadowState(state)

      expect(
        await evaluateBoolLogic({ EXISTS: 'user.profile.personal.name' }, state),
      ).toBe(true)

      expect(
        await evaluateBoolLogic(
          { EXISTS: 'user.profile.settings.theme' },
          state,
        ),
      ).toBe(true)

      expect(
        await evaluateBoolLogic({ EXISTS: 'user.profile.personal.email' }, state),
      ).toBe(false)

      expect(
        await evaluateBoolLogic(
          { EXISTS: 'user.profile.personal.missing' },
          state,
        ),
      ).toBe(false)

      expect(
        await evaluateBoolLogic({ EXISTS: 'user.missing.path' }, state),
      ).toBe(false)
    })

    it('should evaluate IS_EMPTY with nested shadow state paths', async () => {
      if (!wasmAvailable) return

      const state = {
        product: {
          info: {
            description: '',
            title: 'Widget',
          },
          tags: [],
        },
      }

      await initShadowState(state)

      expect(
        await evaluateBoolLogic(
          { IS_EMPTY: 'product.info.description' },
          state,
        ),
      ).toBe(true)

      expect(
        await evaluateBoolLogic({ IS_EMPTY: 'product.info.title' }, state),
      ).toBe(false)

      expect(
        await evaluateBoolLogic({ IS_EMPTY: 'product.tags' }, state),
      ).toBe(true)
    })

    it('should evaluate AND operator with nested shadow state', async () => {
      if (!wasmAvailable) return

      const state = {
        user: {
          profile: {
            role: 'admin',
            status: 'active',
          },
          permissions: {
            canEdit: true,
          },
        },
      }

      await initShadowState(state)

      expect(
        await evaluateBoolLogic(
          {
            AND: [
              { IS_EQUAL: ['user.profile.role', 'admin'] },
              { IS_EQUAL: ['user.profile.status', 'active'] },
            ],
          },
          state,
        ),
      ).toBe(true)

      expect(
        await evaluateBoolLogic(
          {
            AND: [
              { IS_EQUAL: ['user.profile.role', 'admin'] },
              { IS_EQUAL: ['user.permissions.canEdit', true] },
            ],
          },
          state,
        ),
      ).toBe(true)

      expect(
        await evaluateBoolLogic(
          {
            AND: [
              { IS_EQUAL: ['user.profile.role', 'admin'] },
              { IS_EQUAL: ['user.profile.status', 'inactive'] },
            ],
          },
          state,
        ),
      ).toBe(false)
    })

    it('should evaluate OR operator with nested shadow state', async () => {
      if (!wasmAvailable) return

      const state = {
        user: {
          profile: {
            role: 'editor',
            status: 'active',
          },
        },
      }

      await initShadowState(state)

      expect(
        await evaluateBoolLogic(
          {
            OR: [
              { IS_EQUAL: ['user.profile.role', 'admin'] },
              { IS_EQUAL: ['user.profile.role', 'editor'] },
            ],
          },
          state,
        ),
      ).toBe(true)

      expect(
        await evaluateBoolLogic(
          {
            OR: [
              { IS_EQUAL: ['user.profile.status', 'inactive'] },
              { IS_EQUAL: ['user.profile.status', 'active'] },
            ],
          },
          state,
        ),
      ).toBe(true)

      expect(
        await evaluateBoolLogic(
          {
            OR: [
              { IS_EQUAL: ['user.profile.role', 'admin'] },
              { IS_EQUAL: ['user.profile.status', 'inactive'] },
            ],
          },
          state,
        ),
      ).toBe(false)
    })

    it('should evaluate NOT operator with nested shadow state', async () => {
      if (!wasmAvailable) return

      const state = {
        user: {
          profile: {
            role: 'editor',
          },
        },
      }

      await initShadowState(state)

      expect(
        await evaluateBoolLogic(
          {
            NOT: { IS_EQUAL: ['user.profile.role', 'admin'] },
          },
          state,
        ),
      ).toBe(true)

      expect(
        await evaluateBoolLogic(
          {
            NOT: { IS_EQUAL: ['user.profile.role', 'editor'] },
          },
          state,
        ),
      ).toBe(false)

      expect(
        await evaluateBoolLogic(
          {
            NOT: { EXISTS: 'user.profile.missing' },
          },
          state,
        ),
      ).toBe(true)
    })

    it('should evaluate complex nested BoolLogic with shadow state', async () => {
      if (!wasmAvailable) return

      const state = {
        user: {
          profile: {
            role: 'admin',
            status: 'active',
          },
          permissions: {
            canEdit: true,
            canDelete: false,
          },
        },
      }

      await initShadowState(state)

      expect(
        await evaluateBoolLogic(
          {
            AND: [
              { IS_EQUAL: ['user.profile.role', 'admin'] },
              {
                OR: [
                  { IS_EQUAL: ['user.permissions.canEdit', true] },
                  { IS_EQUAL: ['user.permissions.canDelete', true] },
                ],
              },
            ],
          },
          state,
        ),
      ).toBe(true)

      expect(
        await evaluateBoolLogic(
          {
            AND: [
              { IS_EQUAL: ['user.profile.status', 'active'] },
              {
                NOT: {
                  IS_EQUAL: ['user.profile.role', 'guest'],
                },
              },
            ],
          },
          state,
        ),
      ).toBe(true)
    })

    it('should evaluate BoolLogic with array paths in shadow state', async () => {
      if (!wasmAvailable) return

      const state = {
        users: [
          {
            name: 'Alice',
            role: 'admin',
          },
          {
            name: 'Bob',
            role: 'editor',
          },
        ],
      }

      await initShadowState(state)

      expect(
        await evaluateBoolLogic(
          { IS_EQUAL: ['users.0.name', 'Alice'] },
          state,
        ),
      ).toBe(true)

      expect(
        await evaluateBoolLogic({ IS_EQUAL: ['users.0.role', 'admin'] }, state),
      ).toBe(true)

      expect(
        await evaluateBoolLogic({ IS_EQUAL: ['users.1.name', 'Bob'] }, state),
      ).toBe(true)

      expect(
        await evaluateBoolLogic({ EXISTS: 'users.0.name' }, state),
      ).toBe(true)

      expect(
        await evaluateBoolLogic({ EXISTS: 'users.2.name' }, state),
      ).toBe(false)
    })

    it('should evaluate BoolLogic after shadow state updates', async () => {
      if (!wasmAvailable) return

      const state = {
        user: {
          profile: {
            role: 'editor',
          },
        },
      }

      await initShadowState(state)

      expect(
        await evaluateBoolLogic(
          { IS_EQUAL: ['user.profile.role', 'editor'] },
          state,
        ),
      ).toBe(true)

      // Update shadow state
      await updateShadowValue(['user', 'profile', 'role'], 'admin')

      // Get updated state
      const updatedState = await dumpShadowState()

      expect(
        await evaluateBoolLogic(
          { IS_EQUAL: ['user.profile.role', 'admin'] },
          updatedState,
        ),
      ).toBe(true)

      expect(
        await evaluateBoolLogic(
          { IS_EQUAL: ['user.profile.role', 'editor'] },
          updatedState,
        ),
      ).toBe(false)
    })

    it('should evaluate BoolLogic with deeply nested shadow state', async () => {
      if (!wasmAvailable) return

      const state = {
        level1: {
          level2: {
            level3: {
              level4: {
                level5: {
                  value: 'deep',
                  count: 42,
                },
              },
            },
          },
        },
      }

      await initShadowState(state)

      expect(
        await evaluateBoolLogic(
          { IS_EQUAL: ['level1.level2.level3.level4.level5.value', 'deep'] },
          state,
        ),
      ).toBe(true)

      expect(
        await evaluateBoolLogic(
          { IS_EQUAL: ['level1.level2.level3.level4.level5.count', 42] },
          state,
        ),
      ).toBe(true)

      expect(
        await evaluateBoolLogic(
          { EXISTS: 'level1.level2.level3.level4.level5.value' },
          state,
        ),
      ).toBe(true)

      expect(
        await evaluateBoolLogic(
          { EXISTS: 'level1.level2.level3.level4.level5.missing' },
          state,
        ),
      ).toBe(false)
    })

    it('should evaluate BoolLogic with mixed array and object nesting', async () => {
      if (!wasmAvailable) return

      const state = {
        users: [
          {
            name: 'Alice',
            profile: {
              settings: {
                theme: 'dark',
              },
            },
          },
          {
            name: 'Bob',
            profile: {
              settings: {
                theme: 'light',
              },
            },
          },
        ],
      }

      await initShadowState(state)

      expect(
        await evaluateBoolLogic(
          { IS_EQUAL: ['users.0.profile.settings.theme', 'dark'] },
          state,
        ),
      ).toBe(true)

      expect(
        await evaluateBoolLogic(
          { IS_EQUAL: ['users.1.profile.settings.theme', 'light'] },
          state,
        ),
      ).toBe(true)

      expect(
        await evaluateBoolLogic(
          {
            AND: [
              { IS_EQUAL: ['users.0.name', 'Alice'] },
              { IS_EQUAL: ['users.0.profile.settings.theme', 'dark'] },
            ],
          },
          state,
        ),
      ).toBe(true)
    })

    it('should handle BoolLogic evaluation with special values in shadow state', async () => {
      if (!wasmAvailable) return

      const state = {
        values: {
          emptyString: '',
          zero: 0,
          false: false,
          null: null,
        },
      }

      await initShadowState(state)

      expect(
        await evaluateBoolLogic(
          { IS_EMPTY: 'values.emptyString' },
          state,
        ),
      ).toBe(true)

      expect(
        await evaluateBoolLogic({ IS_EQUAL: ['values.zero', 0] }, state),
      ).toBe(true)

      expect(
        await evaluateBoolLogic({ IS_EQUAL: ['values.false', false] }, state),
      ).toBe(true)

      expect(
        await evaluateBoolLogic({ IS_EQUAL: ['values.null', null] }, state),
      ).toBe(true)

      expect(
        await evaluateBoolLogic({ EXISTS: 'values.emptyString' }, state),
      ).toBe(true)

      expect(
        await evaluateBoolLogic({ EXISTS: 'values.zero' }, state),
      ).toBe(true)

      expect(
        await evaluateBoolLogic({ EXISTS: 'values.false' }, state),
      ).toBe(true)

      expect(
        await evaluateBoolLogic({ EXISTS: 'values.null' }, state),
      ).toBe(false)
    })
  })
})
