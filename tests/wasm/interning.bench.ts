import { bench, describe, beforeAll } from 'vitest'

import type { ApexStateWasm, PathID } from '../../src/wasm/bridge'
import { loadWasm } from '../../src/wasm/bridge'

// =============================================================================
// Setup
// =============================================================================

let wasm: ApexStateWasm | null = null
let wasmAvailable = false

// Pre-interned paths for benchmarking
let testPaths: string[] = []
let testPathIds: PathID[] = []

beforeAll(async () => {
  try {
    wasm = await loadWasm()
    wasmAvailable = true

    // Clear and prepare test data
    wasm.intern_clear()

    // Generate test paths
    testPaths = [
      'user.profile.name',
      'user.profile.email',
      'user.profile.avatar.url',
      'user.settings.theme',
      'user.settings.notifications.email',
      'user.settings.notifications.push',
      'app.state.loading',
      'app.state.error.message',
      'app.state.error.code',
      'app.data.items.0.id',
      'app.data.items.1.name',
      'app.data.items.2.value',
    ]

    // Pre-intern paths for PathID benchmarks
    testPathIds = testPaths.map((path) => wasm!.intern(path))
  } catch {
    wasmAvailable = false
  }
})

// =============================================================================
// String vs PathID Comparison Benchmarks
// =============================================================================

describe('String vs PathID Comparison', () => {
  describe('equality comparison', () => {
    bench(
      'string comparison (O(n))',
      () => {
        const path1 = 'user.profile.name'
        const path2 = 'user.profile.name'
        return path1 === path2
      },
      { skip: !wasmAvailable },
    )

    bench(
      'PathID comparison (O(1))',
      () => {
        // PathID comparison is just numeric equality - O(1)
        const id1 = testPathIds[0]
        const id2 = testPathIds[0]
        return id1 === id2
      },
      { skip: !wasmAvailable },
    )

    bench(
      'string comparison - different paths (O(n))',
      () => {
        const path1 = 'user.profile.name'
        const path2 = 'user.profile.email'
        return path1 === path2
      },
      { skip: !wasmAvailable },
    )

    bench(
      'PathID comparison - different paths (O(1))',
      () => {
        const id1 = testPathIds[0]
        const id2 = testPathIds[1]
        return id1 === id2
      },
      { skip: !wasmAvailable },
    )
  })

  describe('long path comparison', () => {
    const longPath =
      'app.state.user.profile.settings.notifications.email.preferences.frequency.daily.enabled'
    let longPathId: PathID

    beforeAll(() => {
      if (wasm) {
        longPathId = wasm.intern(longPath)
      }
    })

    bench(
      'long string comparison (O(n))',
      () => {
        const path1 = longPath
        const path2 = longPath
        return path1 === path2
      },
      { skip: !wasmAvailable },
    )

    bench(
      'long path PathID comparison (O(1))',
      () => {
        const id1 = longPathId
        const id2 = longPathId
        return id1 === id2
      },
      { skip: !wasmAvailable },
    )
  })
})

// =============================================================================
// Interning Overhead Benchmarks
// =============================================================================

describe('Interning Operations', () => {
  describe('single path interning', () => {
    bench(
      'intern new path',
      () => {
        // Note: This will deduplicate, so it's actually O(1) after first intern
        return wasm!.intern('temp.path.' + Math.random())
      },
      { skip: !wasmAvailable },
    )

    bench(
      'intern existing path (deduplication)',
      () => {
        // This should be very fast - just HashMap lookup
        return wasm!.intern('user.profile.name')
      },
      { skip: !wasmAvailable },
    )
  })

  describe('batch interning', () => {
    const batchPaths = Array.from(
      { length: 100 },
      (_, i) => `path.${i}.value`,
    )

    bench(
      'batch intern 100 unique paths',
      () => {
        return wasm!.batch_intern(batchPaths)
      },
      { skip: !wasmAvailable },
    )

    bench(
      'batch intern 100 duplicate paths',
      () => {
        const duplicates = Array(100).fill('user.profile.name')
        return wasm!.batch_intern(duplicates)
      },
      { skip: !wasmAvailable },
    )
  })
})

// =============================================================================
// Resolution Benchmarks
// =============================================================================

describe('PathID Resolution', () => {
  describe('single path resolution', () => {
    bench(
      'resolve single PathID',
      () => {
        return wasm!.resolve(testPathIds[0])
      },
      { skip: !wasmAvailable },
    )

    bench(
      'resolve invalid PathID',
      () => {
        return wasm!.resolve(999999)
      },
      { skip: !wasmAvailable },
    )
  })

  describe('batch resolution', () => {
    bench(
      'resolve 10 PathIDs',
      () => {
        return testPathIds.map((id) => wasm!.resolve(id))
      },
      { skip: !wasmAvailable },
    )

    bench(
      'resolve 100 PathIDs',
      () => {
        const ids = Array.from({ length: 100 }, (_, i) => i as PathID)
        return ids.map((id) => wasm!.resolve(id))
      },
      { skip: !wasmAvailable },
    )
  })
})

// =============================================================================
// Roundtrip Performance
// =============================================================================

describe('Roundtrip Operations', () => {
  bench(
    'string roundtrip (intern + resolve)',
    () => {
      const id = wasm!.intern('user.profile.name')
      return wasm!.resolve(id)
    },
    { skip: !wasmAvailable },
  )

  bench(
    'batch roundtrip (100 paths)',
    () => {
      const paths = Array.from({ length: 100 }, (_, i) => `path.${i}`)
      const ids = wasm!.batch_intern(paths)
      return ids.map((id) => wasm!.resolve(id))
    },
    { skip: !wasmAvailable },
  )
})

// =============================================================================
// Collection Operations
// =============================================================================

describe('Collection Operations with PathIDs', () => {
  describe('set operations', () => {
    const paths = Array.from({ length: 1000 }, (_, i) =>
      i < 10 ? 'duplicate.path' : `unique.path.${i}`,
    )
    let pathIds: PathID[]

    beforeAll(() => {
      if (wasm) {
        pathIds = wasm.batch_intern(paths)
      }
    })

    bench(
      'create Set from 1000 strings',
      () => {
        return new Set(paths)
      },
      { skip: !wasmAvailable },
    )

    bench(
      'create Set from 1000 PathIDs',
      () => {
        return new Set(pathIds)
      },
      { skip: !wasmAvailable },
    )

    bench(
      'find duplicates in 1000 strings',
      () => {
        const seen = new Set<string>()
        const duplicates: string[] = []
        for (const path of paths) {
          if (seen.has(path)) {
            duplicates.push(path)
          }
          seen.add(path)
        }
        return duplicates
      },
      { skip: !wasmAvailable },
    )

    bench(
      'find duplicates in 1000 PathIDs',
      () => {
        const seen = new Set<PathID>()
        const duplicates: PathID[] = []
        for (const id of pathIds) {
          if (seen.has(id)) {
            duplicates.push(id)
          }
          seen.add(id)
        }
        return duplicates
      },
      { skip: !wasmAvailable },
    )
  })

  describe('map operations', () => {
    const paths = Array.from({ length: 100 }, (_, i) => `path.${i}.value`)
    let pathIds: PathID[]

    beforeAll(() => {
      if (wasm) {
        pathIds = wasm.batch_intern(paths)
      }
    })

    bench(
      'create Map with string keys (100 entries)',
      () => {
        const map = new Map<string, number>()
        paths.forEach((path, i) => map.set(path, i))
        return map
      },
      { skip: !wasmAvailable },
    )

    bench(
      'create Map with PathID keys (100 entries)',
      () => {
        const map = new Map<PathID, number>()
        pathIds.forEach((id, i) => map.set(id, i))
        return map
      },
      { skip: !wasmAvailable },
    )

    bench(
      'lookup in Map with string keys (100 lookups)',
      () => {
        const map = new Map<string, number>()
        paths.forEach((path, i) => map.set(path, i))

        let sum = 0
        paths.forEach((path) => {
          sum += map.get(path) ?? 0
        })
        return sum
      },
      { skip: !wasmAvailable },
    )

    bench(
      'lookup in Map with PathID keys (100 lookups)',
      () => {
        const map = new Map<PathID, number>()
        pathIds.forEach((id, i) => map.set(id, i))

        let sum = 0
        pathIds.forEach((id) => {
          sum += map.get(id) ?? 0
        })
        return sum
      },
      { skip: !wasmAvailable },
    )
  })
})

// =============================================================================
// String Pattern Matching
// =============================================================================

describe('Pattern Matching Performance', () => {
  const testPath = 'user.profile.settings.notifications.email.enabled'

  describe('prefix checking', () => {
    bench('string startsWith', () => {
      return testPath.startsWith('user.profile')
    })

    bench('PathID-based prefix check (with resolution)', () => {
      if (!wasm) return false
      const pathId = wasm.intern(testPath)
      const prefixId = wasm.intern('user.profile')

      // Simulate PathID-based prefix checking
      // (In real WASM implementation, this would be done in Rust)
      const pathStr = wasm.resolve(pathId)
      const prefixStr = wasm.resolve(prefixId)
      return pathStr.startsWith(prefixStr)
    })
  })

  describe('suffix checking', () => {
    bench('string endsWith', () => {
      return testPath.endsWith('.enabled')
    })

    bench('PathID-based suffix check (with resolution)', () => {
      if (!wasm) return false
      const pathId = wasm.intern(testPath)
      const suffixId = wasm.intern('.enabled')

      const pathStr = wasm.resolve(pathId)
      const suffixStr = wasm.resolve(suffixId)
      return pathStr.endsWith(suffixStr)
    })
  })

  describe('substring checking', () => {
    bench('string includes', () => {
      return testPath.includes('notifications')
    })

    bench('PathID-based substring check (with resolution)', () => {
      if (!wasm) return false
      const pathId = wasm.intern(testPath)
      const substringId = wasm.intern('notifications')

      const pathStr = wasm.resolve(pathId)
      const substringStr = wasm.resolve(substringId)
      return pathStr.includes(substringStr)
    })
  })
})

// =============================================================================
// Memory Efficiency
// =============================================================================

describe('Memory Efficiency', () => {
  bench(
    'memory: store 1000 duplicate strings directly',
    () => {
      const duplicatePath = 'user.profile.name.with.many.segments'
      const arr: string[] = []
      for (let i = 0; i < 1000; i++) {
        arr.push(duplicatePath)
      }
      return arr
    },
    { skip: !wasmAvailable },
  )

  bench(
    'memory: store 1000 duplicate paths as PathIDs',
    () => {
      if (!wasm) return []
      const pathId = wasm.intern('user.profile.name.with.many.segments')
      const arr: PathID[] = []
      for (let i = 0; i < 1000; i++) {
        arr.push(pathId)
      }
      return arr
    },
    { skip: !wasmAvailable },
  )
})
