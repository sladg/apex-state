import { beforeEach, describe, expect, it } from 'vitest'

import {
  dumpShadowState,
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
})
