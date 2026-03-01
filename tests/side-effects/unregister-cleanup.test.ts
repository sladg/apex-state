/**
 * TEST: unregisterSideEffects Cleanup
 *
 * Verifies that after unregistering a registration ID, all of its
 * side effects (sync pairs, directed sync pairs, flip pairs, aggregations,
 * computations, listeners, clear paths) are fully removed and no longer
 * processed by the pipeline.
 *
 * Tests also verify that other registrations are NOT affected — only the
 * unregistered registration's rules are removed.
 */

import { afterEach, beforeEach, describe, expect, it } from 'vitest'

import { createWasmPipeline, type WasmPipeline } from '~/wasm/bridge'

/** Helper: find change by path */
const findChange = (
  changes: { path: string; value: unknown }[],
  path: string,
) => changes.find((c) => c.path === path)

/** Helper: get all paths from changes */
const getPaths = (changes: { path: string; value: unknown }[]) =>
  changes.map((c) => c.path)

describe('unregisterSideEffects: bidirectional sync pairs removed', () => {
  let pipeline: WasmPipeline
  beforeEach(() => {
    pipeline = createWasmPipeline()
  })
  afterEach(() => {
    pipeline.destroy()
  })

  // Register sync pair [a ↔ b], verify it syncs, then unregister and verify it no longer syncs.
  it('should stop syncing after unregister', () => {
    // Register sync: a ↔ b
    pipeline.shadowInit({ a: 'hello', b: null })
    pipeline.registerSideEffects({
      registration_id: 'sync-reg',
      sync_pairs: [['a', 'b']],
    })

    // Verify sync is active: changing a should propagate to b
    const before = pipeline.processChanges([
      { path: 'a', value: 'world', meta: {} },
    ])
    const bChange = findChange(before.listener_changes, 'b')
    expect(bChange).toBeDefined()
    expect(bChange?.value).toBe('world')

    // Unregister the sync registration
    pipeline.unregisterSideEffects('sync-reg')

    // Verify sync is gone: changing a should NOT propagate to b
    const after = pipeline.processChanges([
      { path: 'a', value: 'changed-again', meta: {} },
    ])
    expect(findChange(after.listener_changes, 'b')).toBeUndefined()
    expect(getPaths(after.listener_changes)).not.toContain('b')
  })

  // Register sync pair [a ↔ b], unregister, then verify the reverse direction is also gone.
  it('should stop syncing in both directions after unregister', () => {
    // Register sync: a ↔ b
    pipeline.shadowInit({ a: 'x', b: 'x' })
    pipeline.registerSideEffects({
      registration_id: 'bidir-reg',
      sync_pairs: [['a', 'b']],
    })

    // Unregister
    pipeline.unregisterSideEffects('bidir-reg')

    // Neither direction should sync anymore
    const result1 = pipeline.processChanges([
      { path: 'a', value: 'new-a', meta: {} },
    ])
    expect(findChange(result1.listener_changes, 'b')).toBeUndefined()

    const result2 = pipeline.processChanges([
      { path: 'b', value: 'new-b', meta: {} },
    ])
    expect(findChange(result2.listener_changes, 'a')).toBeUndefined()
  })
})

describe('unregisterSideEffects: directed sync pairs removed', () => {
  let pipeline: WasmPipeline
  beforeEach(() => {
    pipeline = createWasmPipeline()
  })
  afterEach(() => {
    pipeline.destroy()
  })

  // Register directed sync [src → tgt], verify it syncs one-way, then unregister and verify gone.
  it('should stop one-way sync from source to target after unregister', () => {
    // Register directed sync: src → tgt
    pipeline.shadowInit({ src: 'hello', tgt: null })
    pipeline.registerSideEffects({
      registration_id: 'directed-reg',
      directed_sync_pairs: [['src', 'tgt']],
    })

    // Verify directed sync is active: changing src should propagate to tgt
    const before = pipeline.processChanges([
      { path: 'src', value: 'world', meta: {} },
    ])
    expect(findChange(before.listener_changes, 'tgt')?.value).toBe('world')

    // Unregister the directed sync registration
    pipeline.unregisterSideEffects('directed-reg')

    // Verify directed sync is gone
    const after = pipeline.processChanges([
      { path: 'src', value: 'changed', meta: {} },
    ])
    expect(findChange(after.listener_changes, 'tgt')).toBeUndefined()
  })

  // After unregistering directed sync, the snapshot should show it's gone.
  it('should remove directed pair from graph snapshot after unregister', () => {
    // Register directed sync: src → tgt
    pipeline.shadowInit({ src: 'A', tgt: '' })
    pipeline.registerSideEffects({
      registration_id: 'directed-snap',
      directed_sync_pairs: [['src', 'tgt']],
    })

    // Verify it appears in the snapshot
    const before = pipeline.getGraphSnapshot()
    expect(before.directed_sync_pairs).toHaveLength(1)

    // Unregister and verify snapshot is clean
    pipeline.unregisterSideEffects('directed-snap')
    const after = pipeline.getGraphSnapshot()
    expect(after.directed_sync_pairs).toHaveLength(0)
  })
})

describe('unregisterSideEffects: flip pairs removed', () => {
  let pipeline: WasmPipeline
  beforeEach(() => {
    pipeline = createWasmPipeline()
  })
  afterEach(() => {
    pipeline.destroy()
  })

  // Register flip pair [on ↔ off], verify it flips, then unregister and verify no longer flips.
  it('should stop flipping booleans after unregister', () => {
    // Register flip: on ↔ off (they stay inverted)
    pipeline.shadowInit({ on: true, off: false })
    pipeline.registerSideEffects({
      registration_id: 'flip-reg',
      flip_pairs: [['on', 'off']],
    })

    // Verify flip is active: setting on=false should produce off=true
    const before = pipeline.processChanges([
      { path: 'on', value: false, meta: {} },
    ])
    expect(findChange(before.listener_changes, 'off')?.value).toBe(true)

    // Unregister the flip registration
    pipeline.unregisterSideEffects('flip-reg')

    // Verify flip is gone: setting on=true should NOT produce off=false
    const after = pipeline.processChanges([
      { path: 'on', value: true, meta: {} },
    ])
    expect(findChange(after.listener_changes, 'off')).toBeUndefined()
  })
})

describe('unregisterSideEffects: aggregations removed', () => {
  let pipeline: WasmPipeline
  beforeEach(() => {
    pipeline = createWasmPipeline()
  })
  afterEach(() => {
    pipeline.destroy()
  })

  // Register aggregation [total ← price1, price2], verify it aggregates,
  // unregister and verify price changes no longer update total.
  it('should stop aggregating after unregister', () => {
    // Register aggregation: total = common value of [price1, price2]
    pipeline.shadowInit({ total: null, price1: 100, price2: 100 })
    pipeline.registerSideEffects({
      registration_id: 'agg-reg',
      aggregation_pairs: [
        ['total', 'price1'],
        ['total', 'price2'],
      ],
    })

    // Verify aggregation is active: changing price1 triggers re-aggregation
    const before = pipeline.processChanges([
      { path: 'price1', value: 200, meta: {} },
    ])
    // price1=200, price2=100 → not all equal → total=undefined (or some sentinel)
    // The key check: total IS recalculated (appears in changes)
    expect(getPaths(before.listener_changes)).toContain('total')

    // Unregister the aggregation
    pipeline.unregisterSideEffects('agg-reg')

    // Verify aggregation is gone: changing prices no longer affects total
    const after = pipeline.processChanges([
      { path: 'price1', value: 150, meta: {} },
    ])
    expect(findChange(after.listener_changes, 'total')).toBeUndefined()

    const after2 = pipeline.processChanges([
      { path: 'price2', value: 150, meta: {} },
    ])
    expect(findChange(after2.listener_changes, 'total')).toBeUndefined()
  })
})

describe('unregisterSideEffects: partial unregistration — other rules still active', () => {
  let pipeline: WasmPipeline
  beforeEach(() => {
    pipeline = createWasmPipeline()
  })
  afterEach(() => {
    pipeline.destroy()
  })

  // Register two separate registrations: one with sync, one with flip.
  // Unregister only the sync registration. Verify:
  //   - sync is gone
  //   - flip still works
  it('should only remove unregistered registration rules, leaving others intact', () => {
    // Registration A: sync a ↔ b
    // Registration B: flip x ↔ y
    pipeline.shadowInit({ a: 1, b: 1, x: true, y: false })

    pipeline.registerSideEffects({
      registration_id: 'reg-A',
      sync_pairs: [['a', 'b']],
    })
    pipeline.registerSideEffects({
      registration_id: 'reg-B',
      flip_pairs: [['x', 'y']],
    })

    // Verify both rules work before unregistration
    const syncBefore = pipeline.processChanges([
      { path: 'a', value: 99, meta: {} },
    ])
    expect(findChange(syncBefore.listener_changes, 'b')?.value).toBe(99)

    const flipBefore = pipeline.processChanges([
      { path: 'x', value: false, meta: {} },
    ])
    expect(findChange(flipBefore.listener_changes, 'y')?.value).toBe(true)

    // Unregister only reg-A (sync)
    pipeline.unregisterSideEffects('reg-A')

    // Sync should be gone
    const syncAfter = pipeline.processChanges([
      { path: 'a', value: 42, meta: {} },
    ])
    expect(findChange(syncAfter.listener_changes, 'b')).toBeUndefined()

    // Flip should still work
    const flipAfter = pipeline.processChanges([
      { path: 'x', value: true, meta: {} },
    ])
    expect(findChange(flipAfter.listener_changes, 'y')?.value).toBe(false)
  })

  // Register three sync pairs across two registrations.
  // Unregister one; verify only that pair is gone.
  it('should only remove the specific sync pair from the unregistered registration', () => {
    // Registration 1: sync [a ↔ b] and [c ↔ d]
    // Registration 2: sync [e ↔ f]
    pipeline.shadowInit({ a: 0, b: 0, c: 0, d: 0, e: 0, f: 0 })

    pipeline.registerSideEffects({
      registration_id: 'reg-multi',
      sync_pairs: [
        ['a', 'b'],
        ['c', 'd'],
      ],
    })
    pipeline.registerSideEffects({
      registration_id: 'reg-single',
      sync_pairs: [['e', 'f']],
    })

    // Unregister reg-multi
    pipeline.unregisterSideEffects('reg-multi')

    // a↔b and c↔d should be gone
    const r1 = pipeline.processChanges([{ path: 'a', value: 1, meta: {} }])
    expect(findChange(r1.listener_changes, 'b')).toBeUndefined()

    const r2 = pipeline.processChanges([{ path: 'c', value: 1, meta: {} }])
    expect(findChange(r2.listener_changes, 'd')).toBeUndefined()

    // e↔f should still work
    const r3 = pipeline.processChanges([{ path: 'e', value: 1, meta: {} }])
    expect(findChange(r3.listener_changes, 'f')?.value).toBe(1)
  })
})

describe('unregisterSideEffects: graph snapshot stays clean', () => {
  let pipeline: WasmPipeline
  beforeEach(() => {
    pipeline = createWasmPipeline()
  })
  afterEach(() => {
    pipeline.destroy()
  })

  // After unregistering everything, the graph snapshot should be fully empty.
  it('should produce an empty graph snapshot after all registrations are unregistered', () => {
    pipeline.shadowInit({ a: 0, b: 0, c: true, d: false, src: '', tgt: '' })

    pipeline.registerSideEffects({
      registration_id: 'all-types',
      sync_pairs: [['a', 'b']],
      directed_sync_pairs: [['src', 'tgt']],
      flip_pairs: [['c', 'd']],
    })

    // Verify they appear in snapshot
    const before = pipeline.getGraphSnapshot()
    expect(before.sync_pairs.length).toBeGreaterThan(0)
    expect(before.directed_sync_pairs.length).toBeGreaterThan(0)
    expect(before.flip_pairs.length).toBeGreaterThan(0)

    // Unregister everything
    pipeline.unregisterSideEffects('all-types')

    // Snapshot should be fully empty
    const after = pipeline.getGraphSnapshot()
    expect(after.sync_pairs).toHaveLength(0)
    expect(after.directed_sync_pairs).toHaveLength(0)
    expect(after.flip_pairs).toHaveLength(0)
  })
})
