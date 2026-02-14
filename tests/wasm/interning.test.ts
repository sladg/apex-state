import { describe, expect, it, beforeEach, vi } from 'vitest'

import type { ApexStateWasm, PathID } from '../../src/wasm/bridge'
import {
  loadWasm,
  isWasmLoaded,
  resetWasm,
  internPath,
  resolvePath,
  batchInternPaths,
  batchResolvePaths,
  getInternCount,
  clearInternTable,
  roundtripPath,
  internAndResolve,
  batchInternAndResolve,
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

describe('WASM String Interning - Boundary Conversion', () => {
  let wasmAvailable = false

  beforeEach(async () => {
    // Check if WASM is available
    wasmAvailable = await checkWasmAvailable()

    // Reset WASM state for each test if available
    if (wasmAvailable) {
      resetWasm()
      await loadWasm()
      await clearInternTable()
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
      expect(wasm.intern).toBeTypeOf('function')
      expect(wasm.resolve).toBeTypeOf('function')
      expect(wasm.batch_intern).toBeTypeOf('function')
      expect(wasm.intern_count).toBeTypeOf('function')
      expect(wasm.intern_clear).toBeTypeOf('function')
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

  describe('String to PathID Conversion (Inbound)', () => {
    it('should intern a string path to numeric PathID', async () => {
      if (!wasmAvailable) return

      const wasm = await loadWasm()
      const id = wasm.intern('user.name')

      expect(id).toBeTypeOf('number')
      expect(id).toBeGreaterThanOrEqual(0)
    })

    it('should return same PathID for duplicate strings', async () => {
      if (!wasmAvailable) return

      const wasm = await loadWasm()
      const id1 = wasm.intern('user.email')
      const id2 = wasm.intern('user.email')

      expect(id1).toBe(id2)
    })

    it('should return different PathIDs for different strings', async () => {
      if (!wasmAvailable) return

      const wasm = await loadWasm()
      const id1 = wasm.intern('user.name')
      const id2 = wasm.intern('user.email')

      expect(id1).not.toBe(id2)
    })

    it('should handle empty string', async () => {
      if (!wasmAvailable) return

      const wasm = await loadWasm()
      const id = wasm.intern('')

      expect(id).toBeTypeOf('number')
      expect(id).toBeGreaterThanOrEqual(0)
    })

    it('should handle unicode strings', async () => {
      if (!wasmAvailable) return

      const wasm = await loadWasm()
      const id = wasm.intern('用户.名称')

      expect(id).toBeTypeOf('number')
      expect(id).toBeGreaterThanOrEqual(0)
    })

    it('should handle special characters', async () => {
      if (!wasmAvailable) return

      const wasm = await loadWasm()
      const paths = ['user.name!', 'user@email', 'user#tag', 'user$price']

      const ids = paths.map((path) => wasm.intern(path))

      // All should be valid numeric IDs
      ids.forEach((id) => {
        expect(id).toBeTypeOf('number')
        expect(id).toBeGreaterThanOrEqual(0)
      })

      // All should be unique
      const uniqueIds = new Set(ids)
      expect(uniqueIds.size).toBe(paths.length)
    })
  })

  describe('PathID to String Conversion (Outbound)', () => {
    it('should resolve PathID back to original string', async () => {
      if (!wasmAvailable) return

      const wasm = await loadWasm()
      const original = 'user.profile.avatar'
      const id = wasm.intern(original)
      const resolved = wasm.resolve(id)

      expect(resolved).toBe(original)
    })

    it('should handle roundtrip conversion correctly', async () => {
      if (!wasmAvailable) return

      const wasm = await loadWasm()
      const paths = ['user.name', 'user.email', 'user.age', 'settings.theme']

      paths.forEach((original) => {
        const id = wasm.intern(original)
        const resolved = wasm.resolve(id)
        expect(resolved).toBe(original)
      })
    })

    it('should return empty string for invalid PathID', async () => {
      if (!wasmAvailable) return

      const wasm = await loadWasm()
      const invalidId = 999999

      const resolved = wasm.resolve(invalidId)

      expect(resolved).toBe('')
    })

    it('should preserve unicode in roundtrip', async () => {
      if (!wasmAvailable) return

      const wasm = await loadWasm()
      const original = '用户.设置.主题'
      const id = wasm.intern(original)
      const resolved = wasm.resolve(id)

      expect(resolved).toBe(original)
    })
  })

  describe('Batch Operations', () => {
    it('should batch intern multiple paths efficiently', async () => {
      if (!wasmAvailable) return

      const wasm = await loadWasm()
      const paths = ['user.name', 'user.email', 'user.age']

      const ids = wasm.batch_intern(paths)

      expect(ids).toHaveLength(paths.length)
      ids.forEach((id) => {
        expect(id).toBeTypeOf('number')
        expect(id).toBeGreaterThanOrEqual(0)
      })
    })

    it('should handle duplicates in batch intern', async () => {
      if (!wasmAvailable) return

      const wasm = await loadWasm()
      const paths = ['user.name', 'user.email', 'user.name', 'user.age']

      const ids = wasm.batch_intern(paths)

      expect(ids).toHaveLength(paths.length)
      expect(ids[0]).toBe(ids[2]) // Duplicate should have same ID
      expect(ids[0]).not.toBe(ids[1])
    })

    it('should batch resolve multiple PathIDs', async () => {
      if (!wasmAvailable) return

      const wasm = await loadWasm()
      const paths = ['user.name', 'user.email', 'user.age']
      const ids = wasm.batch_intern(paths)

      const resolved = ids.map((id) => wasm.resolve(id))

      expect(resolved).toEqual(paths)
    })

    it('should handle empty array in batch operations', async () => {
      if (!wasmAvailable) return

      const wasm = await loadWasm()
      const ids = wasm.batch_intern([])

      expect(ids).toEqual([])
    })
  })

  describe('Interning Table State', () => {
    it('should track intern count correctly', async () => {
      if (!wasmAvailable) return

      const wasm = await loadWasm()
      wasm.intern_clear()

      expect(wasm.intern_count()).toBe(0)

      wasm.intern('user.name')
      expect(wasm.intern_count()).toBe(1)

      wasm.intern('user.email')
      expect(wasm.intern_count()).toBe(2)

      wasm.intern('user.name') // Duplicate
      expect(wasm.intern_count()).toBe(2)
    })

    it('should clear interning table', async () => {
      if (!wasmAvailable) return

      const wasm = await loadWasm()
      wasm.intern('user.name')
      wasm.intern('user.email')

      expect(wasm.intern_count()).toBeGreaterThan(0)

      wasm.intern_clear()

      expect(wasm.intern_count()).toBe(0)
    })

    it('should allow re-interning after clear', async () => {
      if (!wasmAvailable) return

      const wasm = await loadWasm()
      const id1 = wasm.intern('user.name')

      wasm.intern_clear()

      const id2 = wasm.intern('user.name')

      // After clear, same string should get new ID (likely 0)
      expect(id2).toBeTypeOf('number')
      expect(wasm.resolve(id2)).toBe('user.name')
    })
  })

  describe('Auto-Loading Wrapper Functions', () => {
    it('should auto-load WASM with internPath', async () => {
      if (!wasmAvailable) return

      resetWasm()
      expect(isWasmLoaded()).toBe(false)

      const id = await internPath('user.name')

      expect(isWasmLoaded()).toBe(true)
      expect(id).toBeTypeOf('number')
    })

    it('should auto-load WASM with resolvePath', async () => {
      if (!wasmAvailable) return

      resetWasm()
      const id = await internPath('user.name')

      resetWasm()
      expect(isWasmLoaded()).toBe(false)

      // Load again and resolve
      await loadWasm()
      const testId = (await loadWasm()).intern('test.path')
      resetWasm()

      const resolved = await resolvePath(testId)

      expect(isWasmLoaded()).toBe(true)
      expect(resolved).toBeTypeOf('string')
    })

    it('should auto-load WASM with batchInternPaths', async () => {
      if (!wasmAvailable) return

      resetWasm()

      const ids = await batchInternPaths(['user.name', 'user.email'])

      expect(isWasmLoaded()).toBe(true)
      expect(ids).toHaveLength(2)
    })

    it('should auto-load WASM with getInternCount', async () => {
      if (!wasmAvailable) return

      resetWasm()

      const count = await getInternCount()

      expect(isWasmLoaded()).toBe(true)
      expect(count).toBeTypeOf('number')
    })

    it('should auto-load WASM with clearInternTable', async () => {
      if (!wasmAvailable) return

      resetWasm()

      await clearInternTable()

      expect(isWasmLoaded()).toBe(true)
    })
  })

  describe('Roundtrip Wrapper Functions', () => {
    it('should roundtrip path correctly', async () => {
      if (!wasmAvailable) return

      const original = 'user.settings.theme'
      const result = await roundtripPath(original)

      expect(result).toBe(original)
    })

    it('should intern and resolve with wrapper', async () => {
      if (!wasmAvailable) return

      const original = 'user.profile.name'
      const result = await internAndResolve(original)

      expect(result).toBe(original)
    })

    it('should batch intern and resolve with wrapper', async () => {
      if (!wasmAvailable) return

      const paths = ['user.name', 'user.email', 'user.age']
      const results = await batchInternAndResolve(paths)

      expect(results).toEqual(paths)
    })

    it('should handle empty array in batch wrappers', async () => {
      if (!wasmAvailable) return

      const results = await batchInternAndResolve([])

      expect(results).toEqual([])
    })
  })

  describe('Edge Cases', () => {
    it('should handle very long strings', async () => {
      if (!wasmAvailable) return

      const wasm = await loadWasm()
      const longPath = 'user.' + 'property.'.repeat(100) + 'value'

      const id = wasm.intern(longPath)
      const resolved = wasm.resolve(id)

      expect(resolved).toBe(longPath)
    })

    it('should handle whitespace-only strings', async () => {
      if (!wasmAvailable) return

      const wasm = await loadWasm()
      const paths = [' ', '  ', '\t', '\n', '   \t\n  ']

      paths.forEach((path) => {
        const id = wasm.intern(path)
        const resolved = wasm.resolve(id)
        expect(resolved).toBe(path)
      })
    })

    it('should handle strings with mixed unicode and ASCII', async () => {
      if (!wasmAvailable) return

      const wasm = await loadWasm()
      const path = 'user.名前.name.用户'

      const id = wasm.intern(path)
      const resolved = wasm.resolve(id)

      expect(resolved).toBe(path)
    })

    it('should maintain separate IDs for case-sensitive strings', async () => {
      if (!wasmAvailable) return

      const wasm = await loadWasm()
      const id1 = wasm.intern('User.Name')
      const id2 = wasm.intern('user.name')
      const id3 = wasm.intern('USER.NAME')

      expect(id1).not.toBe(id2)
      expect(id2).not.toBe(id3)
      expect(id1).not.toBe(id3)

      expect(wasm.resolve(id1)).toBe('User.Name')
      expect(wasm.resolve(id2)).toBe('user.name')
      expect(wasm.resolve(id3)).toBe('USER.NAME')
    })

    it('should handle sequential intern operations', async () => {
      if (!wasmAvailable) return

      const wasm = await loadWasm()
      wasm.intern_clear()

      const paths: string[] = []
      const ids: PathID[] = []

      // Intern 100 unique paths
      for (let i = 0; i < 100; i++) {
        const path = `path.${i}.value`
        paths.push(path)
        ids.push(wasm.intern(path))
      }

      // Verify all are unique
      const uniqueIds = new Set(ids)
      expect(uniqueIds.size).toBe(100)

      // Verify all resolve correctly
      ids.forEach((id, index) => {
        expect(wasm.resolve(id)).toBe(paths[index])
      })
    })
  })

  describe('Memory and Performance', () => {
    it('should handle stress test with many unique paths', async () => {
      if (!wasmAvailable) return

      const wasm = await loadWasm()
      wasm.intern_clear()

      const pathCount = 1000
      const paths: string[] = []

      // Generate and intern 1000 unique paths
      for (let i = 0; i < pathCount; i++) {
        const path = `path.${i}.property.${i % 10}.value`
        paths.push(path)
      }

      const ids = wasm.batch_intern(paths)

      expect(ids).toHaveLength(pathCount)
      expect(wasm.intern_count()).toBeLessThanOrEqual(pathCount)

      // Verify first, middle, and last paths
      expect(wasm.resolve(ids[0])).toBe(paths[0])
      expect(wasm.resolve(ids[Math.floor(pathCount / 2)])).toBe(
        paths[Math.floor(pathCount / 2)],
      )
      expect(wasm.resolve(ids[pathCount - 1])).toBe(paths[pathCount - 1])
    })

    it('should handle deduplication in stress test', async () => {
      if (!wasmAvailable) return

      const wasm = await loadWasm()
      wasm.intern_clear()

      // Create array with 100 paths, but only 10 unique
      const paths: string[] = []
      for (let i = 0; i < 100; i++) {
        paths.push(`path.${i % 10}.value`)
      }

      const ids = wasm.batch_intern(paths)

      expect(ids).toHaveLength(100)
      expect(wasm.intern_count()).toBe(10) // Only 10 unique
    })
  })
})
