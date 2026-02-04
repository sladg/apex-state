/**
 * Performance Benchmark Tests
 *
 * Measures hot-path timings to validate performance assumptions
 * and establish baselines for optimization work.
 *
 * Each benchmark runs the operation in a tight loop to amortize
 * measurement overhead and detect microsecond-level differences.
 *
 * Convention: benchmarks use `performance.now()` and assert upper bounds.
 * These are NOT pass/fail thresholds for CI — they're canaries to
 * catch regressions. Adjust bounds per machine if needed.
 */

import { describe, expect, it } from 'vitest'
import { z } from 'zod'

import { createGenericStore } from '../../src'
import type { BoolLogic } from '../../src/types/boolLogic'
import { evaluateBoolLogic } from '../../src/utils/boolLogic'
import { dot } from '../../src/utils/dot'
import { interpolateTemplate } from '../../src/utils/interpolation'
import { typeHelpers } from '../mocks'
import { flushSync, renderWithStore } from '../utils/react'

// ---------------------------------------------------------------------------
// Shared state shape — deeply nested to stress-test path operations
// ---------------------------------------------------------------------------

interface BenchState {
  portfolio: {
    books: Record<
      string,
      {
        name: string
        products: Record<
          string,
          {
            status: string
            legs: Record<
              string,
              {
                strike: number
                notional: number
                greeks: { delta: number; gamma: number; vega: number }
                marketData: {
                  spot: number
                  volSurface: {
                    atmVol: number
                    smile: Record<string, { strike: number; vol: number }>
                  }
                }
              }
            >
          }
        >
      }
    >
  }
  isHedged: boolean
  needsRebalance: boolean
  aggregatedDelta: number
  _errors: Record<string, string[]>
}

const makeBenchState = (): BenchState => ({
  portfolio: {
    books: {
      b1: {
        name: 'G10 Options',
        products: {
          p1: {
            status: 'draft',
            legs: {
              l1: {
                strike: 1.1,
                notional: 1_000_000,
                greeks: { delta: 0.55, gamma: 0.03, vega: 0.12 },
                marketData: {
                  spot: 1.1,
                  volSurface: {
                    atmVol: 0.1,
                    smile: {
                      s25p: { strike: 1.08, vol: 0.12 },
                      atm: { strike: 1.1, vol: 0.1 },
                      s25c: { strike: 1.15, vol: 0.11 },
                    },
                  },
                },
              },
              l2: {
                strike: 1.1,
                notional: 1_000_000,
                greeks: { delta: -0.45, gamma: 0.02, vega: 0.11 },
                marketData: {
                  spot: 1.1,
                  volSurface: {
                    atmVol: 0.1,
                    smile: {
                      s25p: { strike: 1.08, vol: 0.12 },
                      atm: { strike: 1.1, vol: 0.1 },
                      s25c: { strike: 1.15, vol: 0.11 },
                    },
                  },
                },
              },
            },
          },
        },
      },
    },
  },
  isHedged: true,
  needsRebalance: false,
  aggregatedDelta: 0.1,
  _errors: {},
})

const DEEP_PATH =
  'portfolio.books.b1.products.p1.legs.l1.marketData.volSurface.smile.s25p.vol'
const MEDIUM_PATH = 'portfolio.books.b1.products.p1.legs.l1.strike'
const SHALLOW_PATH = 'isHedged'

// ---------------------------------------------------------------------------
// 1. deepGet / lodash _get — the most-called function in the library
// ---------------------------------------------------------------------------

describe('Benchmark: deepGet (lodash _get) throughput', () => {
  it('shallow path (1 level) — 10k iterations', () => {
    const state = makeBenchState()
    const iterations = 10_000

    const start = performance.now()
    for (let i = 0; i < iterations; i++) {
      dot.get__unsafe(state, SHALLOW_PATH)
    }
    const elapsed = performance.now() - start
    const perOp = (elapsed / iterations) * 1000 // µs

    expect(perOp).toBeLessThan(10) // < 10µs per get

    console.log(
      `  deepGet shallow: ${perOp.toFixed(3)}µs/op (${elapsed.toFixed(2)}ms total)`,
    )
  })

  it('medium path (7 levels) — 10k iterations', () => {
    const state = makeBenchState()
    const iterations = 10_000

    const start = performance.now()
    for (let i = 0; i < iterations; i++) {
      dot.get__unsafe(state, MEDIUM_PATH)
    }
    const elapsed = performance.now() - start
    const perOp = (elapsed / iterations) * 1000

    expect(perOp).toBeLessThan(10)

    console.log(
      `  deepGet medium:  ${perOp.toFixed(3)}µs/op (${elapsed.toFixed(2)}ms total)`,
    )
  })

  it('deep path (12 levels) — 10k iterations', () => {
    const state = makeBenchState()
    const iterations = 10_000

    const start = performance.now()
    for (let i = 0; i < iterations; i++) {
      dot.get__unsafe(state, DEEP_PATH)
    }
    const elapsed = performance.now() - start
    const perOp = (elapsed / iterations) * 1000

    expect(perOp).toBeLessThan(10)

    console.log(
      `  deepGet deep:    ${perOp.toFixed(3)}µs/op (${elapsed.toFixed(2)}ms total)`,
    )
  })

  it('native hand-rolled get vs lodash _get — 10k iterations each', () => {
    const state = makeBenchState()
    const iterations = 10_000

    // Native approach
    const nativeGet = (obj: object, path: string): unknown =>
      path
        .split('.')
        .reduce<unknown>(
          (acc, key) =>
            acc != null && typeof acc === 'object'
              ? Reflect.get(acc, key)
              : undefined,
          obj,
        )

    const startNative = performance.now()
    for (let i = 0; i < iterations; i++) {
      nativeGet(state, DEEP_PATH)
    }
    const nativeElapsed = performance.now() - startNative

    // Lodash approach
    const startLodash = performance.now()
    for (let i = 0; i < iterations; i++) {
      dot.get__unsafe(state, DEEP_PATH)
    }
    const lodashElapsed = performance.now() - startLodash

    const speedup = lodashElapsed / nativeElapsed

    console.log(
      `  native:  ${((nativeElapsed / iterations) * 1000).toFixed(3)}µs/op`,
    )

    console.log(
      `  lodash:  ${((lodashElapsed / iterations) * 1000).toFixed(3)}µs/op`,
    )

    console.log(`  ratio:   lodash is ${speedup.toFixed(2)}x vs native`)

    // Both should complete within reasonable time
    expect(nativeElapsed).toBeLessThan(100)
    expect(lodashElapsed).toBeLessThan(100)
  })
})

// ---------------------------------------------------------------------------
// 1b. deepSet — lodash _set vs native Reflect.set
// ---------------------------------------------------------------------------

describe('Benchmark: deepSet — lodash vs native', () => {
  it('lodash _set vs native Reflect.set — deep path — 100k iterations', () => {
    const iterations = 100_000

    // lodash set (current library implementation)

    // Native set using Reflect.set
    const nativeSet = (obj: object, path: string, value: unknown): void => {
      const keys = path.split('.')
      const last = keys.length - 1
      let current: object = obj
      for (let i = 0; i < last; i++) {
        current = Reflect.get(current, keys[i]!) as object
      }
      Reflect.set(current, keys[last]!, value)
    }

    // Deep path set
    const state1 = makeBenchState()
    const startLodash = performance.now()
    for (let i = 0; i < iterations; i++) {
      dot.set__unsafe(state1, DEEP_PATH, 0.12 + (i % 100) * 0.001)
    }
    const lodashElapsed = performance.now() - startLodash

    const state2 = makeBenchState()
    const startNative = performance.now()
    for (let i = 0; i < iterations; i++) {
      nativeSet(state2, DEEP_PATH, 0.12 + (i % 100) * 0.001)
    }
    const nativeElapsed = performance.now() - startNative

    const lodashPerOp = (lodashElapsed / iterations) * 1000
    const nativePerOp = (nativeElapsed / iterations) * 1000

    console.log(`  Deep set — lodash:  ${lodashPerOp.toFixed(3)}µs/op`)
    console.log(`  Deep set — native:  ${nativePerOp.toFixed(3)}µs/op`)
    console.log(
      `  Ratio: native is ${(lodashElapsed / nativeElapsed).toFixed(2)}x faster`,
    )

    // Verify both produce the same result
    dot.set__unsafe(state1, DEEP_PATH, 0.999)
    nativeSet(state2, DEEP_PATH, 0.999)
    expect(state1).toEqual(state2)

    expect(nativeElapsed).toBeLessThan(500)
  })

  it('lodash _set vs native Reflect.set — medium path — 100k iterations', () => {
    const iterations = 100_000

    const nativeSet = (obj: object, path: string, value: unknown): void => {
      const keys = path.split('.')
      const last = keys.length - 1
      let current: object = obj
      for (let i = 0; i < last; i++) {
        current = Reflect.get(current, keys[i]!) as object
      }
      Reflect.set(current, keys[last]!, value)
    }

    const state1 = makeBenchState()
    const startLodash = performance.now()
    for (let i = 0; i < iterations; i++) {
      dot.set__unsafe(state1, MEDIUM_PATH, 1.1 + (i % 100) * 0.01)
    }
    const lodashElapsed = performance.now() - startLodash

    const state2 = makeBenchState()
    const startNative = performance.now()
    for (let i = 0; i < iterations; i++) {
      nativeSet(state2, MEDIUM_PATH, 1.1 + (i % 100) * 0.01)
    }
    const nativeElapsed = performance.now() - startNative

    console.log(
      `  Medium set — lodash: ${((lodashElapsed / iterations) * 1000).toFixed(3)}µs/op`,
    )
    console.log(
      `  Medium set — native: ${((nativeElapsed / iterations) * 1000).toFixed(3)}µs/op`,
    )
    console.log(
      `  Ratio: native is ${(lodashElapsed / nativeElapsed).toFixed(2)}x faster`,
    )

    expect(nativeElapsed).toBeLessThan(500)
  })

  it('lodash _set vs native Reflect.set — shallow path — 100k iterations', () => {
    const iterations = 100_000

    const nativeSet = (obj: object, path: string, value: unknown): void => {
      const keys = path.split('.')
      const last = keys.length - 1
      let current: object = obj
      for (let i = 0; i < last; i++) {
        current = Reflect.get(current, keys[i]!) as object
      }
      Reflect.set(current, keys[last]!, value)
    }

    const state1 = makeBenchState()
    const startLodash = performance.now()
    for (let i = 0; i < iterations; i++) {
      dot.set__unsafe(state1, SHALLOW_PATH, i % 2 === 0)
    }
    const lodashElapsed = performance.now() - startLodash

    const state2 = makeBenchState()
    const startNative = performance.now()
    for (let i = 0; i < iterations; i++) {
      nativeSet(state2, SHALLOW_PATH, i % 2 === 0)
    }
    const nativeElapsed = performance.now() - startNative

    console.log(
      `  Shallow set — lodash: ${((lodashElapsed / iterations) * 1000).toFixed(3)}µs/op`,
    )
    console.log(
      `  Shallow set — native: ${((nativeElapsed / iterations) * 1000).toFixed(3)}µs/op`,
    )
    console.log(
      `  Ratio: native is ${(lodashElapsed / nativeElapsed).toFixed(2)}x faster`,
    )

    expect(nativeElapsed).toBeLessThan(500)
  })
})

// ---------------------------------------------------------------------------
// 2. evaluateBoolLogic — runs on every concern with conditions
// ---------------------------------------------------------------------------

describe('Benchmark: evaluateBoolLogic throughput', () => {
  it('simple IS_EQUAL — 10k iterations', () => {
    const state = makeBenchState()
    const iterations = 10_000

    const start = performance.now()
    for (let i = 0; i < iterations; i++) {
      evaluateBoolLogic({ IS_EQUAL: ['isHedged', true] }, state)
    }
    const elapsed = performance.now() - start
    const perOp = (elapsed / iterations) * 1000

    expect(perOp).toBeLessThan(10)

    console.log(`  IS_EQUAL simple: ${perOp.toFixed(3)}µs/op`)
  })

  it('deep path IS_EQUAL — 10k iterations', () => {
    const state = makeBenchState()
    const iterations = 10_000

    const start = performance.now()
    for (let i = 0; i < iterations; i++) {
      evaluateBoolLogic({ IS_EQUAL: [MEDIUM_PATH, 1.1] }, state)
    }
    const elapsed = performance.now() - start
    const perOp = (elapsed / iterations) * 1000

    expect(perOp).toBeLessThan(10)

    console.log(`  IS_EQUAL deep:   ${perOp.toFixed(3)}µs/op`)
  })

  it('complex AND/OR/NOT — 10k iterations', () => {
    const state = makeBenchState()
    const iterations = 10_000
    const condition: BoolLogic<BenchState> = {
      AND: [
        {
          NOT: { IS_EQUAL: ['portfolio.books.b1.products.p1.status', 'draft'] },
        },
        {
          OR: [
            { GT: ['portfolio.books.b1.products.p1.legs.l1.strike', 2] },
            { IS_EQUAL: ['isHedged', false] },
            { EXISTS: 'aggregatedDelta' },
          ],
        },
        { LTE: ['portfolio.books.b1.products.p1.legs.l1.notional', 5_000_000] },
      ],
    }

    const start = performance.now()
    for (let i = 0; i < iterations; i++) {
      evaluateBoolLogic(condition, state)
    }
    const elapsed = performance.now() - start
    const perOp = (elapsed / iterations) * 1000

    expect(perOp).toBeLessThan(50)

    console.log(`  complex AND/OR:  ${perOp.toFixed(3)}µs/op`)
  })

  it('numeric comparisons GT/LT/GTE/LTE — lodash vs native overhead', () => {
    const state = makeBenchState()
    const iterations = 10_000

    // Lodash path (via evaluateBoolLogic)
    const startLodash = performance.now()
    for (let i = 0; i < iterations; i++) {
      evaluateBoolLogic({ GT: [MEDIUM_PATH, 0.5] }, state)
    }
    const lodashElapsed = performance.now() - startLodash

    // Native equivalent: _get + direct >
    const nativeGet = (obj: object, path: string): unknown =>
      path
        .split('.')
        .reduce<unknown>(
          (acc, key) =>
            acc != null && typeof acc === 'object'
              ? Reflect.get(acc, key)
              : undefined,
          obj,
        )

    let _sink = false
    const startNative = performance.now()
    for (let i = 0; i < iterations; i++) {
      const val = nativeGet(state, MEDIUM_PATH)
      _sink = typeof val === 'number' && val > 0.5
    }
    void _sink
    const nativeElapsed = performance.now() - startNative

    console.log(
      `  GT lodash:  ${((lodashElapsed / iterations) * 1000).toFixed(3)}µs/op`,
    )

    console.log(
      `  GT native:  ${((nativeElapsed / iterations) * 1000).toFixed(3)}µs/op`,
    )

    console.log(
      `  ratio:      lodash is ${(lodashElapsed / nativeElapsed).toFixed(2)}x vs native`,
    )

    expect(lodashElapsed).toBeLessThan(100)
  })
})

// ---------------------------------------------------------------------------
// 2b. Type-safe vs any-based deep access — measures overhead of type safety
// ---------------------------------------------------------------------------

describe('Benchmark: type-safe vs any-based deep access', () => {
  it('Reflect.get vs optional chaining (any) for deep access — 100k iterations', () => {
    const state = makeBenchState()
    const iterations = 100_000

    // NEW: type-safe approach using Reflect.get (no any)
    const reflectGet = (obj: object, path: string): unknown =>
      path
        .split('.')
        .reduce<unknown>(
          (acc, key) =>
            acc != null && typeof acc === 'object'
              ? Reflect.get(acc, key)
              : undefined,
          obj,
        )

    // OLD: any-based approach using optional chaining

    const anyGet = (obj: any, path: string): unknown =>
      path.split('.').reduce((acc, key) => acc?.[key], obj)

    // Warm up
    reflectGet(state, DEEP_PATH)
    anyGet(state, DEEP_PATH)

    // Benchmark Reflect.get
    const startReflect = performance.now()
    for (let i = 0; i < iterations; i++) {
      reflectGet(state, DEEP_PATH)
    }
    const reflectElapsed = performance.now() - startReflect

    // Benchmark any optional chaining
    const startAny = performance.now()
    for (let i = 0; i < iterations; i++) {
      anyGet(state, DEEP_PATH)
    }
    const anyElapsed = performance.now() - startAny

    // Benchmark dot.get__unsafe (lodash, used in library)
    const startLodash = performance.now()
    for (let i = 0; i < iterations; i++) {
      dot.get__unsafe(state, DEEP_PATH)
    }
    const lodashElapsed = performance.now() - startLodash

    const reflectPerOp = (reflectElapsed / iterations) * 1000
    const anyPerOp = (anyElapsed / iterations) * 1000
    const lodashPerOp = (lodashElapsed / iterations) * 1000

    console.log(`  Reflect.get (type-safe): ${reflectPerOp.toFixed(3)}µs/op`)
    console.log(`  acc?.[key]  (any-based): ${anyPerOp.toFixed(3)}µs/op`)
    console.log(`  dot.get__unsafe  (lodash): ${lodashPerOp.toFixed(3)}µs/op`)
    console.log(
      `  Reflect vs any ratio:   ${(reflectElapsed / anyElapsed).toFixed(2)}x`,
    )
    console.log(
      `  Lodash vs any ratio:    ${(lodashElapsed / anyElapsed).toFixed(2)}x`,
    )

    // Verify all return same result
    expect(reflectGet(state, DEEP_PATH)).toBe(anyGet(state, DEEP_PATH))
    expect(reflectGet(state, DEEP_PATH)).toBe(dot.get__unsafe(state, DEEP_PATH))

    // All should complete in reasonable time
    expect(reflectElapsed).toBeLessThan(500)
    expect(anyElapsed).toBeLessThan(500)
  })

  it('shallow path (1 level) — Reflect vs any vs lodash — 100k iterations', () => {
    const state = makeBenchState()
    const iterations = 100_000

    const reflectGet = (obj: object, path: string): unknown =>
      path
        .split('.')
        .reduce<unknown>(
          (acc, key) =>
            acc != null && typeof acc === 'object'
              ? Reflect.get(acc, key)
              : undefined,
          obj,
        )

    const anyGet = (obj: any, path: string): unknown =>
      path.split('.').reduce((acc, key) => acc?.[key], obj)

    reflectGet(state, SHALLOW_PATH)
    anyGet(state, SHALLOW_PATH)

    const startReflect = performance.now()
    for (let i = 0; i < iterations; i++) {
      reflectGet(state, SHALLOW_PATH)
    }
    const reflectElapsed = performance.now() - startReflect

    const startAny = performance.now()
    for (let i = 0; i < iterations; i++) {
      anyGet(state, SHALLOW_PATH)
    }
    const anyElapsed = performance.now() - startAny

    const startLodash = performance.now()
    for (let i = 0; i < iterations; i++) {
      dot.get__unsafe(state, SHALLOW_PATH)
    }
    const lodashElapsed = performance.now() - startLodash

    console.log(
      `  Shallow — Reflect: ${((reflectElapsed / iterations) * 1000).toFixed(3)}µs/op`,
    )
    console.log(
      `  Shallow — any:     ${((anyElapsed / iterations) * 1000).toFixed(3)}µs/op`,
    )
    console.log(
      `  Shallow — lodash:  ${((lodashElapsed / iterations) * 1000).toFixed(3)}µs/op`,
    )
    console.log(
      `  Reflect vs any:    ${(reflectElapsed / anyElapsed).toFixed(2)}x`,
    )

    expect(reflectElapsed).toBeLessThan(500)
  })

  it('medium path (7 levels) — Reflect vs any vs lodash — 100k iterations', () => {
    const state = makeBenchState()
    const iterations = 100_000

    const reflectGet = (obj: object, path: string): unknown =>
      path
        .split('.')
        .reduce<unknown>(
          (acc, key) =>
            acc != null && typeof acc === 'object'
              ? Reflect.get(acc, key)
              : undefined,
          obj,
        )

    const anyGet = (obj: any, path: string): unknown =>
      path.split('.').reduce((acc, key) => acc?.[key], obj)

    reflectGet(state, MEDIUM_PATH)
    anyGet(state, MEDIUM_PATH)

    const startReflect = performance.now()
    for (let i = 0; i < iterations; i++) {
      reflectGet(state, MEDIUM_PATH)
    }
    const reflectElapsed = performance.now() - startReflect

    const startAny = performance.now()
    for (let i = 0; i < iterations; i++) {
      anyGet(state, MEDIUM_PATH)
    }
    const anyElapsed = performance.now() - startAny

    const startLodash = performance.now()
    for (let i = 0; i < iterations; i++) {
      dot.get__unsafe(state, MEDIUM_PATH)
    }
    const lodashElapsed = performance.now() - startLodash

    console.log(
      `  Medium — Reflect: ${((reflectElapsed / iterations) * 1000).toFixed(3)}µs/op`,
    )
    console.log(
      `  Medium — any:     ${((anyElapsed / iterations) * 1000).toFixed(3)}µs/op`,
    )
    console.log(
      `  Medium — lodash:  ${((lodashElapsed / iterations) * 1000).toFixed(3)}µs/op`,
    )
    console.log(
      `  Reflect vs any:   ${(reflectElapsed / anyElapsed).toFixed(2)}x`,
    )

    expect(reflectElapsed).toBeLessThan(500)
  })
})

// ---------------------------------------------------------------------------
// 3. interpolateTemplate — runs for dynamic labels/tooltips/placeholders
// ---------------------------------------------------------------------------

describe('Benchmark: interpolateTemplate throughput', () => {
  it('single placeholder — 10k iterations', () => {
    const state = makeBenchState()
    const iterations = 10_000

    const start = performance.now()
    for (let i = 0; i < iterations; i++) {
      interpolateTemplate(
        'Strike: {{portfolio.books.b1.products.p1.legs.l1.strike}}',
        state,
      )
    }
    const elapsed = performance.now() - start
    const perOp = (elapsed / iterations) * 1000

    expect(perOp).toBeLessThan(50)

    console.log(`  single placeholder: ${perOp.toFixed(3)}µs/op`)
  })

  it('multiple placeholders — 10k iterations', () => {
    const state = makeBenchState()
    const iterations = 10_000
    const template =
      'Strike: {{portfolio.books.b1.products.p1.legs.l1.strike}} | ' +
      'Spot: {{portfolio.books.b1.products.p1.legs.l1.marketData.spot}} | ' +
      'Delta: {{portfolio.books.b1.products.p1.legs.l1.greeks.delta}} | ' +
      'ATM: {{portfolio.books.b1.products.p1.legs.l1.marketData.volSurface.atmVol}}'

    const start = performance.now()
    for (let i = 0; i < iterations; i++) {
      interpolateTemplate(template, state)
    }
    const elapsed = performance.now() - start
    const perOp = (elapsed / iterations) * 1000

    expect(perOp).toBeLessThan(100)

    console.log(`  4 placeholders:     ${perOp.toFixed(3)}µs/op`)
  })

  it('regex recompilation cost — precompiled vs inline', () => {
    const state = makeBenchState()
    const iterations = 10_000
    const template = 'Val: {{portfolio.books.b1.products.p1.legs.l1.strike}}'

    // Current approach: regex in replace() (recompiled each call)
    const startInline = performance.now()
    for (let i = 0; i < iterations; i++) {
      template.replace(/\{\{([^}]+)\}\}/g, (_match, path) => {
        const val = dot.get__unsafe(state, path)
        return typeof val === 'number' ? String(val) : String(val)
      })
    }
    const inlineElapsed = performance.now() - startInline

    // Optimized: precompiled regex
    const REGEX = /\{\{([^}]+)\}\}/g
    const startPrecompiled = performance.now()
    for (let i = 0; i < iterations; i++) {
      REGEX.lastIndex = 0
      template.replace(REGEX, (_match, path) => {
        const val = dot.get__unsafe(state, path)
        return typeof val === 'number' ? String(val) : String(val)
      })
    }
    const precompiledElapsed = performance.now() - startPrecompiled

    console.log(
      `  inline regex:      ${((inlineElapsed / iterations) * 1000).toFixed(3)}µs/op`,
    )

    console.log(
      `  precompiled regex: ${((precompiledElapsed / iterations) * 1000).toFixed(3)}µs/op`,
    )

    console.log(
      `  ratio:             ${(inlineElapsed / precompiledElapsed).toFixed(2)}x`,
    )

    expect(inlineElapsed).toBeLessThan(200)
  })
})

// ---------------------------------------------------------------------------
// 4. processChanges pipeline — full pipeline with/without side effects
// ---------------------------------------------------------------------------

describe('Benchmark: processChanges pipeline', () => {
  it('single change, no side effects — measures snapshot() cost', async () => {
    const store = createGenericStore<BenchState>()

    // eslint-disable-next-line no-restricted-syntax
    const { storeInstance } = renderWithStore(store, makeBenchState())
    await flushSync()

    const iterations = 100
    const times: number[] = []

    for (let i = 0; i < iterations; i++) {
      const start = performance.now()
      storeInstance.state.portfolio.books['b1']!.products['p1']!.legs[
        'l1'
      ]!.strike = 1.1 + i * 0.01
      await flushSync()
      times.push(performance.now() - start)
    }

    const sorted = times.sort((a, b) => a - b)
    const avg = times.reduce((a, b) => a + b, 0) / times.length
    const p95 = sorted[Math.floor(times.length * 0.95)]!

    console.log(`  avg: ${avg.toFixed(3)}ms | p95: ${p95.toFixed(3)}ms`)

    expect(avg).toBeLessThan(5)
    expect(p95).toBeLessThan(10)
  })

  it('single change with sync + flip side effects', async () => {
    const store = createGenericStore<BenchState>()

    const SyncFlipBench = () => {
      store.useSideEffects('bench-effects', {
        syncPaths: [
          typeHelpers.syncPair<BenchState>(
            'portfolio.books.b1.products.p1.legs.l1.strike',
            'portfolio.books.b1.products.p1.legs.l2.strike',
          ),
        ],
        flipPaths: [
          typeHelpers.flipPair<BenchState>('isHedged', 'needsRebalance'),
        ],
      })
      return null
    }

    // eslint-disable-next-line no-restricted-syntax
    const { storeInstance } = renderWithStore(
      <SyncFlipBench />,
      store,
      makeBenchState(),
    )
    await flushSync()

    const iterations = 100
    const times: number[] = []

    for (let i = 0; i < iterations; i++) {
      const start = performance.now()
      storeInstance.state.portfolio.books['b1']!.products['p1']!.legs[
        'l1'
      ]!.strike = 1.1 + i * 0.01
      await flushSync()
      times.push(performance.now() - start)
    }

    const sorted = times.sort((a, b) => a - b)
    const avg = times.reduce((a, b) => a + b, 0) / times.length
    const p95 = sorted[Math.floor(times.length * 0.95)]!

    console.log(
      `  with sync+flip — avg: ${avg.toFixed(3)}ms | p95: ${p95.toFixed(3)}ms`,
    )

    expect(avg).toBeLessThan(5)
    expect(p95).toBeLessThan(10)
  })

  it('single change with 10 concerns registered', async () => {
    const store = createGenericStore<BenchState>()

    const L1 = 'portfolio.books.b1.products.p1.legs.l1'
    const L2 = 'portfolio.books.b1.products.p1.legs.l2'
    const disabledCondition: BoolLogic<BenchState> = {
      IS_EQUAL: ['portfolio.books.b1.products.p1.status', 'approved'],
    }

    // eslint-disable-next-line no-restricted-syntax
    const { storeInstance } = renderWithStore(store, makeBenchState(), {
      concerns: {
        [`${L1}.strike`]: {
          validationState: { schema: z.number().positive().max(10) },
          disabledWhen: { condition: disabledCondition },
          dynamicTooltip: {
            template: `Strike: {{${L1}.strike}} | Spot: {{${L1}.marketData.spot}}`,
          },
        },
        [`${L1}.notional`]: {
          validationState: { schema: z.number().positive() },
          disabledWhen: { condition: disabledCondition },
        },
        [`${L2}.strike`]: {
          validationState: { schema: z.number().positive().max(10) },
          disabledWhen: { condition: disabledCondition },
          dynamicTooltip: {
            template: `Strike: {{${L2}.strike}} | Spot: {{${L2}.marketData.spot}}`,
          },
        },
        [`${L2}.notional`]: {
          validationState: { schema: z.number().positive() },
          disabledWhen: { condition: disabledCondition },
        },
      },
    })
    await flushSync()

    const iterations = 100
    const times: number[] = []

    for (let i = 0; i < iterations; i++) {
      const start = performance.now()
      storeInstance.state.portfolio.books['b1']!.products['p1']!.legs[
        'l1'
      ]!.strike = 1.1 + i * 0.01
      await flushSync()
      times.push(performance.now() - start)
    }

    const sorted = times.sort((a, b) => a - b)
    const avg = times.reduce((a, b) => a + b, 0) / times.length
    const p95 = sorted[Math.floor(times.length * 0.95)]!

    console.log(
      `  with 10 concerns — avg: ${avg.toFixed(3)}ms | p95: ${p95.toFixed(3)}ms`,
    )

    // Only leg-1 concerns should re-evaluate (selective recalc)
    // But pipeline still runs for every change
    expect(avg).toBeLessThan(5)
    expect(p95).toBeLessThan(10)
  })

  it('batch of 20 rapid mutations with sync side effects', async () => {
    const store = createGenericStore<BenchState>()

    const BatchSyncBench = () => {
      store.useSideEffects('bench-batch', {
        syncPaths: [
          typeHelpers.syncPair<BenchState>(
            'portfolio.books.b1.products.p1.legs.l1.strike',
            'portfolio.books.b1.products.p1.legs.l2.strike',
          ),
        ],
      })
      return null
    }

    // eslint-disable-next-line no-restricted-syntax
    const { storeInstance } = renderWithStore(
      <BatchSyncBench />,
      store,
      makeBenchState(),
    )
    await flushSync()

    const iterations = 50
    const times: number[] = []

    for (let i = 0; i < iterations; i++) {
      // Rapid 20 mutations before flushing — valtio batches these
      const start = performance.now()
      for (let j = 0; j < 20; j++) {
        storeInstance.state.portfolio.books['b1']!.products['p1']!.legs[
          'l1'
        ]!.greeks.delta = 0.5 + j * 0.01 + i * 0.001
      }
      await flushSync()
      times.push(performance.now() - start)
    }

    const sorted = times.sort((a, b) => a - b)
    const avg = times.reduce((a, b) => a + b, 0) / times.length
    const p95 = sorted[Math.floor(times.length * 0.95)]!

    console.log(
      `  batch 20 mutations — avg: ${avg.toFixed(3)}ms | p95: ${p95.toFixed(3)}ms`,
    )

    expect(avg).toBeLessThan(5)
  })
})

// ---------------------------------------------------------------------------
// 5. snapshot() cost — measures valtio snapshot overhead on different sizes
// ---------------------------------------------------------------------------

describe('Benchmark: valtio snapshot() cost', () => {
  it('snapshot on deeply nested state — isolate the cost', async () => {
    const { snapshot } = await import('valtio')
    const { proxy } = await import('valtio/vanilla')

    const state = proxy(makeBenchState())
    const iterations = 1_000

    const start = performance.now()
    for (let i = 0; i < iterations; i++) {
      snapshot(state)
    }
    const elapsed = performance.now() - start
    const perOp = (elapsed / iterations) * 1000

    console.log(
      `  snapshot():   ${perOp.toFixed(3)}µs/op (${elapsed.toFixed(2)}ms total for ${iterations} ops)`,
    )

    // snapshot is called once per processChanges — if it's expensive, lazy-load it
    expect(perOp).toBeLessThan(500) // generous bound
  })
})

// ---------------------------------------------------------------------------
// 6. Concern evaluation end-to-end — full round-trip
// ---------------------------------------------------------------------------

describe('Benchmark: concern evaluation round-trip', () => {
  it('change → concern re-evaluated — measures full reactive chain', async () => {
    const store = createGenericStore<BenchState>()

    const STRIKE_PATH = 'portfolio.books.b1.products.p1.legs.l1.strike'

    // eslint-disable-next-line no-restricted-syntax
    const { storeInstance } = renderWithStore(store, makeBenchState(), {
      concerns: {
        [STRIKE_PATH]: {
          validationState: { schema: z.number().positive().max(10) },
          disabledWhen: {
            condition: {
              AND: [
                {
                  IS_EQUAL: [
                    'portfolio.books.b1.products.p1.status',
                    'approved',
                  ],
                },
                { GT: [STRIKE_PATH, 5] },
              ],
            },
          },
          dynamicTooltip: {
            template: `Strike {{${STRIKE_PATH}}} vs spot {{portfolio.books.b1.products.p1.legs.l1.marketData.spot}}`,
          },
        },
      },
    })
    await flushSync()

    // Warm up
    storeInstance.state.portfolio.books['b1']!.products['p1']!.legs[
      'l1'
    ]!.strike = 1.15
    await flushSync()

    // Measure
    const iterations = 50
    const times: number[] = []

    for (let i = 0; i < iterations; i++) {
      const start = performance.now()
      storeInstance.state.portfolio.books['b1']!.products['p1']!.legs[
        'l1'
      ]!.strike = 1.1 + i * 0.01
      await flushSync()
      times.push(performance.now() - start)

      // Verify concern updated
      const tooltip = storeInstance._concerns[STRIKE_PATH]?.['dynamicTooltip']
      expect(tooltip).toContain('Strike')
    }

    const sorted = times.sort((a, b) => a - b)
    const avg = times.reduce((a, b) => a + b, 0) / times.length
    const p95 = sorted[Math.floor(times.length * 0.95)]!
    const min = sorted[0]!
    const max = sorted[sorted.length - 1]!

    console.log(`  Full round-trip (3 concerns):`)

    console.log(
      `    avg: ${avg.toFixed(3)}ms | p95: ${p95.toFixed(3)}ms | min: ${min.toFixed(3)}ms | max: ${max.toFixed(3)}ms`,
    )

    expect(avg).toBeLessThan(5)
    expect(p95).toBeLessThan(10)
  })
})

// ---------------------------------------------------------------------------
// 7. String operations — path depth, startsWith, split
// ---------------------------------------------------------------------------

describe('Benchmark: string operations in hot paths', () => {
  it('getPathDepth: split vs manual count — 100k iterations', () => {
    const iterations = 100_000

    // Current: path.split('.').length
    let _depth = 0
    const startSplit = performance.now()
    for (let i = 0; i < iterations; i++) {
      _depth = DEEP_PATH.split('.').length
    }
    void _depth
    const splitElapsed = performance.now() - startSplit

    // Alternative: count dots
    const countDots = (path: string): number => {
      let count = 1
      for (let j = 0; j < path.length; j++) {
        if (path.charCodeAt(j) === 46) count++ // '.' === 46
      }
      return count
    }

    const startCount = performance.now()
    for (let i = 0; i < iterations; i++) {
      countDots(DEEP_PATH)
    }
    const countElapsed = performance.now() - startCount

    console.log(
      `  split('.').length: ${((splitElapsed / iterations) * 1000).toFixed(3)}µs/op`,
    )

    console.log(
      `  charCode count:   ${((countElapsed / iterations) * 1000).toFixed(3)}µs/op`,
    )

    console.log(
      `  ratio:            split is ${(splitElapsed / countElapsed).toFixed(2)}x vs charCode`,
    )

    // Both should be fast — but split allocates arrays
    expect(splitElapsed).toBeLessThan(200)
    expect(countElapsed).toBeLessThan(200)
  })

  it('startsWith vs indexOf for path matching — 100k iterations', () => {
    const iterations = 100_000
    const prefix = 'portfolio.books.b1.products.p1.legs.l1.'

    const startStartsWith = performance.now()
    for (let i = 0; i < iterations; i++) {
      DEEP_PATH.startsWith(prefix)
    }
    const startsWithElapsed = performance.now() - startStartsWith

    let _match = false
    const startIndexOf = performance.now()
    for (let i = 0; i < iterations; i++) {
      _match = DEEP_PATH.indexOf(prefix) === 0
    }
    void _match
    const indexOfElapsed = performance.now() - startIndexOf

    console.log(
      `  startsWith: ${((startsWithElapsed / iterations) * 1000).toFixed(3)}µs/op`,
    )

    console.log(
      `  indexOf===0: ${((indexOfElapsed / iterations) * 1000).toFixed(3)}µs/op`,
    )

    expect(startsWithElapsed).toBeLessThan(100)
  })
})
