import { beforeEach, describe, expect, it } from 'vitest'

import {
  clearInternCache,
  getInternCacheSize,
  getPathId,
  internPath,
  internPaths,
} from '~/utils/intern'

describe('String Interning (WASM-002)', () => {
  let wasm: typeof import('../../rust/pkg/apex_state_wasm.js')

  beforeEach(async () => {
    wasm = await import('../../rust/pkg/apex_state_wasm.js')
    wasm.intern_clear()
    clearInternCache()
  })

  describe('WASM-side interning', () => {
    it('interns strings deterministically', async () => {
      const id1 = wasm.intern('user.name')
      const id2 = wasm.intern('user.email')
      const id3 = wasm.intern('user.name') // Same path

      expect(id1).toBe(id3)
      expect(id1).not.toBe(id2)
    })

    it('resolves IDs back to strings', async () => {
      const id = wasm.intern('product.price')
      const resolved = wasm.resolve(id)

      expect(resolved).toBe('product.price')
    })

    it('returns empty string for unknown IDs', async () => {
      const resolved = wasm.resolve(9999)
      expect(resolved).toBe('')
    })

    it('round-trips correctly', async () => {
      const paths = ['a.b.c', 'x.y.z', 'foo', 'bar.baz']

      for (const path of paths) {
        const id = wasm.intern(path)
        const resolved = wasm.resolve(id)
        expect(resolved).toBe(path)
      }
    })

    it('assigns sequential IDs', async () => {
      wasm.intern_clear()

      const id1 = wasm.intern('first')
      const id2 = wasm.intern('second')
      const id3 = wasm.intern('third')

      expect(id1).toBe(0)
      expect(id2).toBe(1)
      expect(id3).toBe(2)
    })

    it('clears the table', async () => {
      wasm.intern('test.path')
      expect(wasm.intern_count()).toBeGreaterThan(0)

      wasm.intern_clear()
      expect(wasm.intern_count()).toBe(0)

      const id = wasm.intern('test.path')
      expect(id).toBe(0) // IDs reset after clear
    })

    it('handles batch interning', async () => {
      const paths = ['path1', 'path2', 'path3']
      const ids = wasm.intern_batch(paths as any)

      expect(ids).toHaveLength(3)
      expect(ids[0]).toBe(wasm.intern('path1'))
      expect(ids[1]).toBe(wasm.intern('path2'))
      expect(ids[2]).toBe(wasm.intern('path3'))
    })
  })

  describe('JS-side caching', () => {
    it('caches interned paths', async () => {
      const id = await internPath('user.name')

      expect(getPathId('user.name')).toBe(id)
      expect(getInternCacheSize()).toBe(1)
    })

    it('returns cached ID on subsequent calls', async () => {
      const id1 = await internPath('user.email')
      const id2 = await internPath('user.email') // Should use cache

      expect(id1).toBe(id2)
      expect(getInternCacheSize()).toBe(1)
    })

    it('returns undefined for non-interned paths', () => {
      expect(getPathId('never.interned')).toBeUndefined()
    })

    it('clears the cache', async () => {
      await internPath('test.path')
      expect(getInternCacheSize()).toBeGreaterThan(0)

      clearInternCache()
      expect(getInternCacheSize()).toBe(0)
    })

    it('batch interns paths efficiently', async () => {
      const paths = ['a.b', 'c.d', 'e.f']
      const ids = await internPaths(paths)

      expect(ids).toHaveLength(3)
      expect(getInternCacheSize()).toBe(3)

      // Verify all paths cached
      for (let i = 0; i < paths.length; i++) {
        const pathId = getPathId(paths[i])
        expect(pathId).toBeDefined()
        expect(pathId).toBe(ids[i])
      }
    })

    it('uses cache in batch operations', async () => {
      // Pre-intern some paths
      await internPath('pre.cached.1')
      await internPath('pre.cached.2')

      const paths = ['pre.cached.1', 'new.path', 'pre.cached.2']
      const ids = await internPaths(paths)

      expect(ids).toHaveLength(3)
      expect(getInternCacheSize()).toBe(3)
    })
  })

  describe('JS-WASM synchronization', () => {
    it('keeps JS cache in sync with WASM table', async () => {
      const paths = ['sync.test.1', 'sync.test.2', 'sync.test.3']

      // Intern via JS utility
      for (const path of paths) {
        await internPath(path)
      }

      // Verify WASM has the same IDs
      for (const path of paths) {
        const jsId = getPathId(path)
        const wasmId = wasm.intern(path) // Should return existing ID

        expect(jsId).toBe(wasmId)
      }
    })

    it('handles mixed JS and direct WASM interning', async () => {
      // Direct WASM intern
      const wasmId1 = wasm.intern('wasm.direct.1')

      // JS intern (should recognize existing WASM ID)
      const jsId1 = await internPath('wasm.direct.1')

      expect(jsId1).toBe(wasmId1)
    })

    it('maintains consistency after clear', async () => {
      await internPath('before.clear')

      // Clear both JS and WASM
      wasm.intern_clear()
      clearInternCache()

      expect(getInternCacheSize()).toBe(0)
      expect(wasm.intern_count()).toBe(0)

      // New intern should start from ID 0
      const id = await internPath('after.clear')
      expect(id).toBe(0)
    })
  })

  describe('Performance', () => {
    it('interns 1000 paths in < 5ms', async () => {
      const paths = Array.from({ length: 1000 }, (_, i) => `path.${i}`)

      const start = performance.now()
      const ids = wasm.intern_batch(paths as any)
      const duration = performance.now() - start

      expect(ids).toHaveLength(1000)
      expect(duration).toBeLessThan(5)
    })

    it('JS cache lookup is faster than WASM call', async () => {
      const path = 'performance.test.path'
      await internPath(path) // Warm up cache

      // Time cache lookup
      const cacheStart = performance.now()
      for (let i = 0; i < 1000; i++) {
        getPathId(path)
      }
      const cacheDuration = performance.now() - cacheStart

      // Time WASM calls
      const wasmStart = performance.now()
      for (let i = 0; i < 1000; i++) {
        wasm.intern(path)
      }
      const wasmDuration = performance.now() - wasmStart

      expect(cacheDuration).toBeLessThan(wasmDuration)
    })
  })
})
