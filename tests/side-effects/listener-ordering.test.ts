/**
 * TEST: Listener Dispatch Ordering
 *
 * Verifies that the WASM pipeline dispatches listeners in the correct order:
 *
 *   1. effective_depth DESC  — max(topic_depth, anchor_depth), deepest first
 *   2. topic_depth DESC      — actual topic path depth (tiebreaker within effective_depth)
 *   3. scope_depth DESC      — deeper scope path runs before shallower
 *   4. anchor_depth DESC     — deeper anchor path runs before shallower (when no scope)
 *   5. registration_index ASC — FIFO: earlier-registered wins when all else equal
 *
 * Multi-path listeners use the deepest topic path for their effective_depth.
 * Null-path (root) listeners with a deep anchor_path are elevated to that anchor's depth.
 */

import { afterEach, describe, expect, it } from 'vitest'

import { createWasmPipeline, type WasmPipeline } from '~/wasm/bridge'

/** Extract ordered subscriber IDs from an execution plan (flattened across all groups). */
const getOrderedIds = (
  plan: { groups: { dispatches: { subscriber_id: number }[] }[] } | null,
): number[] => {
  if (!plan) return []
  return plan.groups.flatMap((g) => g.dispatches.map((d) => d.subscriber_id))
}

describe('Listener Dispatch Ordering', () => {
  let pipeline: WasmPipeline

  afterEach(() => {
    pipeline?.destroy()
  })

  // Trigger a change that touches a top-level path so all topic listeners get seeded.
  const triggerChange = () =>
    pipeline.processChanges([{ path: 'a.b.c.d.e', value: 1, meta: {} }])

  // --- 1. topic depth ---

  it('should dispatch deeper topic paths before shallower ones', () => {
    // Register in reverse depth order — sort must correct it.
    // Sub 10: depth 1, Sub 20: depth 3, Sub 30: depth 2
    pipeline = createWasmPipeline()
    pipeline.shadowInit({ a: { b: { c: { d: { e: 0 } } } } })

    pipeline.registerSideEffects({
      registration_id: 'reg-1',
      listeners: [
        { subscriber_id: 10, topic_paths: ['a'], scope_path: '' },
        { subscriber_id: 20, topic_paths: ['a.b.c'], scope_path: '' },
        { subscriber_id: 30, topic_paths: ['a.b'], scope_path: '' },
      ],
    })

    const result = triggerChange()
    // Expected: 20 (depth 3) → 30 (depth 2) → 10 (depth 1)
    expect(getOrderedIds(result.execution_plan)).toEqual([20, 30, 10])
  })

  // --- 2. scope depth tiebreaker ---

  it('should dispatch deeper scope before shallower scope at same topic depth', () => {
    // All at topic depth 2 — scope depth is the tiebreaker.
    // Sub 1: scope depth 0, Sub 2: scope depth 1, Sub 3: scope depth 2
    pipeline = createWasmPipeline()
    pipeline.shadowInit({ a: { b: { c: { d: { e: 0 } } } } })

    pipeline.registerSideEffects({
      registration_id: 'reg-scope',
      listeners: [
        { subscriber_id: 1, topic_paths: ['a.b'], scope_path: '' },
        { subscriber_id: 2, topic_paths: ['a.b'], scope_path: 'a' },
        { subscriber_id: 3, topic_paths: ['a.b'], scope_path: 'a.b' },
      ],
    })

    const result = triggerChange()
    // Expected: 3 (scope 2) → 2 (scope 1) → 1 (scope 0)
    expect(getOrderedIds(result.execution_plan)).toEqual([3, 2, 1])
  })

  // --- 3. anchor depth tiebreaker ---

  it('should dispatch deeper anchor before shallower anchor when scope is absent', () => {
    // All at topic depth 2, no scope — anchor depth is tiebreaker.
    // Sub 1: no anchor (0), Sub 2: anchor depth 1, Sub 3: anchor depth 2
    pipeline = createWasmPipeline()
    pipeline.shadowInit({ a: { b: { c: { d: { e: 0 } } } } })

    pipeline.registerSideEffects({
      registration_id: 'reg-anchor-tiebreak',
      anchor_path: 'a',
      listeners: [{ subscriber_id: 2, topic_paths: ['a.b'], scope_path: '' }],
    })
    pipeline.registerSideEffects({
      registration_id: 'reg-anchor-tiebreak-2',
      anchor_path: 'a.b',
      listeners: [{ subscriber_id: 3, topic_paths: ['a.b'], scope_path: '' }],
    })
    pipeline.registerSideEffects({
      registration_id: 'reg-no-anchor',
      listeners: [{ subscriber_id: 1, topic_paths: ['a.b'], scope_path: '' }],
    })

    const result = triggerChange()
    // Expected: 3 (anchor depth 2) → 2 (anchor depth 1) → 1 (no anchor)
    expect(getOrderedIds(result.execution_plan)).toEqual([3, 2, 1])
  })

  // --- 4. anchor elevates root listener ---

  it('should elevate a root listener with deep anchor above a shallower topic listener', () => {
    // L1: topic=a.b.c (depth 3, effective=3)
    // L2: topic="" root, anchor=a.b.c (depth 0, effective=3 via anchor)
    // L3: topic=a.b (depth 2, effective=2)
    // Expected: L1 → L2 → L3
    //   L1 and L2 share effective_depth=3; topic_depth tiebreaker puts L1 (topic=3) before L2 (topic=0).
    pipeline = createWasmPipeline()
    pipeline.shadowInit({ a: { b: { c: { d: { e: 0 } } } } })

    pipeline.registerSideEffects({
      registration_id: 'reg-deep-topic',
      listeners: [{ subscriber_id: 1, topic_paths: ['a.b.c'], scope_path: '' }],
    })
    pipeline.registerSideEffects({
      registration_id: 'reg-anchored-root',
      anchor_path: 'a.b.c',
      listeners: [{ subscriber_id: 2, topic_paths: [''], scope_path: '' }],
    })
    pipeline.registerSideEffects({
      registration_id: 'reg-shallow-topic',
      listeners: [{ subscriber_id: 3, topic_paths: ['a.b'], scope_path: '' }],
    })

    const result = triggerChange()
    expect(getOrderedIds(result.execution_plan)).toEqual([1, 2, 3])
  })

  // --- 5. FIFO tiebreaker ---

  it('should preserve registration order when all sort keys are equal', () => {
    // All three listeners have identical keys — FIFO must be respected.
    pipeline = createWasmPipeline()
    pipeline.shadowInit({ a: { b: { c: { d: { e: 0 } } } } })

    pipeline.registerSideEffects({
      registration_id: 'reg-fifo-1',
      listeners: [{ subscriber_id: 10, topic_paths: ['a.b'], scope_path: 'a' }],
    })
    pipeline.registerSideEffects({
      registration_id: 'reg-fifo-2',
      listeners: [{ subscriber_id: 20, topic_paths: ['a.b'], scope_path: 'a' }],
    })
    pipeline.registerSideEffects({
      registration_id: 'reg-fifo-3',
      listeners: [{ subscriber_id: 30, topic_paths: ['a.b'], scope_path: 'a' }],
    })

    const result = triggerChange()
    expect(getOrderedIds(result.execution_plan)).toEqual([10, 20, 30])
  })

  // --- 6. multi-path uses deepest ---

  it('should use the deepest topic path to determine effective_depth for multi-path listeners', () => {
    // Sub 2 has paths ["a", "a.b.c"] — deepest is depth 3.
    // Sub 1 has path ["a.b"] — depth 2.
    // Expected: Sub 2 (eff=3) before Sub 1 (eff=2).
    pipeline = createWasmPipeline()
    pipeline.shadowInit({ a: { b: { c: { d: { e: 0 } } } } })

    pipeline.registerSideEffects({
      registration_id: 'reg-single',
      listeners: [{ subscriber_id: 1, topic_paths: ['a.b'], scope_path: '' }],
    })
    pipeline.registerSideEffects({
      registration_id: 'reg-multi',
      listeners: [
        { subscriber_id: 2, topic_paths: ['a', 'a.b.c'], scope_path: '' },
      ],
    })

    const result = triggerChange()
    const ids = getOrderedIds(result.execution_plan)
    const idx1 = ids.indexOf(1)
    const idx2 = ids.indexOf(2)
    expect(idx2).toBeLessThan(idx1) // Sub 2 runs before Sub 1
  })

  // --- 7. full mixed scenario ---

  it('should order a mixed set of listeners correctly across all sort criteria', () => {
    // Five listeners with varying depths:
    // Sub A: topic=a.b.c.d (depth 4)
    // Sub B: topic=a.b.c (depth 3), scope=a.b (scope depth 2)
    // Sub C: topic=a.b.c (depth 3), scope=a (scope depth 1)
    // Sub D: topic="" root, anchor=a.b.c (effective depth 3 via anchor, topic depth 0)
    // Sub E: topic=a.b (depth 2)
    //
    // Sort:
    //   A: eff=4, topic=4, scope=0, anchor=0 → runs 1st
    //   B: eff=3, topic=3, scope=2, anchor=0 → runs 2nd (same eff as C/D, deeper scope)
    //   C: eff=3, topic=3, scope=1, anchor=0 → runs 3rd
    //   D: eff=3, topic=0, scope=0, anchor=3 → runs 4th (eff=3, but topic=0 loses to C)
    //   E: eff=2, topic=2, scope=0, anchor=0 → runs 5th
    pipeline = createWasmPipeline()
    pipeline.shadowInit({ a: { b: { c: { d: 0 } } } })

    pipeline.registerSideEffects({
      registration_id: 'reg-A',
      listeners: [
        { subscriber_id: 100, topic_paths: ['a.b.c.d'], scope_path: '' },
      ],
    })
    pipeline.registerSideEffects({
      registration_id: 'reg-B',
      listeners: [
        { subscriber_id: 200, topic_paths: ['a.b.c'], scope_path: 'a.b' },
      ],
    })
    pipeline.registerSideEffects({
      registration_id: 'reg-C',
      listeners: [
        { subscriber_id: 300, topic_paths: ['a.b.c'], scope_path: 'a' },
      ],
    })
    pipeline.registerSideEffects({
      registration_id: 'reg-D',
      anchor_path: 'a.b.c',
      listeners: [{ subscriber_id: 400, topic_paths: [''], scope_path: '' }],
    })
    pipeline.registerSideEffects({
      registration_id: 'reg-E',
      listeners: [{ subscriber_id: 500, topic_paths: ['a.b'], scope_path: '' }],
    })

    const result = pipeline.processChanges([
      { path: 'a.b.c.d', value: 42, meta: {} },
    ])

    expect(getOrderedIds(result.execution_plan)).toEqual([
      100, 200, 300, 400, 500,
    ])
  })
})
