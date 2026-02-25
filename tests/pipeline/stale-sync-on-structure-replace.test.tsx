/**
 * Stale sync pairs during structure replacement
 *
 * Regression test: when a parent structure is replaced (e.g., `nested` gets a
 * new object without some keys), sync pairs registered on old child paths
 * should NOT write values into paths that no longer exist.
 *
 * The race condition (React lifecycle):
 * 1. Old component registers sync pairs: field1 ↔ nested.deep.value
 * 2. Parent structure replaced: nested = { other: 1 } (nested.deep gone)
 * 3. A change to field1 triggers processChanges
 * 4. Sync sees field1 has peer nested.deep.value (old pair still registered)
 * 5. BUG: process_sync_paths_into writes to nested.deep.value (ghost path)
 * 6. Old component finally unmounts and cleans up — too late
 *
 * Fix: process_sync_paths_into / process_flip_paths_into check
 * shadow.get(peer_path).is_none() before emitting sync/flip changes.
 */

import { describe, expect, it } from 'vitest'

import { registerSideEffects } from '~/sideEffects/registration.wasm-impl'
import type { ArrayOfChanges, GenericMeta } from '~/types'

import { createTestStore } from '../utils/react'

// ---------------------------------------------------------------------------
// Tests — WASM only (fix is in Rust pipeline)
// ---------------------------------------------------------------------------

describe('[WASM] Stale sync pairs on structure replacement', () => {
  it('should not sync to dead peer path after parent structure is replaced', () => {
    interface State {
      field1: number
      nested: { deep: { value: number } }
    }

    const { storeInstance, processChanges } = createTestStore<State>(
      { useLegacyImplementation: false },
      { field1: 10, nested: { deep: { value: 10 } } },
    )

    // Old component registers sync: field1 ↔ nested.deep.value
    const oldCleanup = registerSideEffects(storeInstance, 'old', {
      syncPaths: [['field1', 'nested.deep.value']],
    })

    // Verify initial sync
    expect(storeInstance.state.field1).toBe(10)
    expect(storeInstance.state.nested.deep.value).toBe(10)

    // Replace nested with object that has NO deep key
    processChanges(storeInstance, [
      ['nested', { other: 42 }, {} as GenericMeta],
    ] as unknown as ArrayOfChanges<State, GenericMeta>)

    // nested.deep no longer exists
    expect(
      (storeInstance.state.nested as Record<string, unknown>)['deep'],
    ).toBeUndefined()

    // Now change field1 — sync pair is still registered but peer path is dead
    // WITHOUT FIX: sync writes to nested.deep.value, creating ghost structure
    // WITH FIX: sync skips because shadow.get("nested.deep.value") is None
    processChanges(storeInstance, [
      ['field1', 999, {} as GenericMeta],
    ] as ArrayOfChanges<State, GenericMeta>)

    expect(storeInstance.state.field1).toBe(999)

    // CRITICAL: nested.deep should still NOT exist — no ghost write from stale sync
    expect(
      (storeInstance.state.nested as Record<string, unknown>)['deep'],
    ).toBeUndefined()

    oldCleanup()
  })

  it('should not sync when parent is replaced with fewer keys', () => {
    interface State {
      data: { a: number; b: number; c: number }
    }

    const { storeInstance, processChanges } = createTestStore<State>(
      { useLegacyImplementation: false },
      { data: { a: 1, b: 1, c: 1 } },
    )

    // Register sync: data.a ↔ data.b
    const oldCleanup = registerSideEffects(storeInstance, 'old', {
      syncPaths: [['data.a', 'data.b']],
    })

    // Replace data with object that only has 'c'
    processChanges(storeInstance, [
      ['data', { c: 42 }, {} as GenericMeta],
    ] as ArrayOfChanges<State, GenericMeta>)

    // data.a and data.b no longer exist
    expect(
      (storeInstance.state.data as Record<string, unknown>)['a'],
    ).toBeUndefined()
    expect(
      (storeInstance.state.data as Record<string, unknown>)['b'],
    ).toBeUndefined()

    // Re-create data.a — this should NOT trigger sync to dead data.b
    processChanges(storeInstance, [
      ['data.a', 77, {} as GenericMeta],
    ] as ArrayOfChanges<State, GenericMeta>)

    expect((storeInstance.state.data as Record<string, unknown>)['a']).toBe(77)
    // data.b must NOT have been ghost-created by stale sync
    expect(
      (storeInstance.state.data as Record<string, unknown>)['b'],
    ).toBeUndefined()

    oldCleanup()
  })

  it('should not flip to dead peer path after parent structure is replaced', () => {
    interface State {
      topBool: boolean
      form: { fields: { enabled: boolean } }
    }

    const { storeInstance, processChanges } = createTestStore<State>(
      { useLegacyImplementation: false },
      { topBool: true, form: { fields: { enabled: false } } },
    )

    // Register flip: topBool ↔ form.fields.enabled
    const oldCleanup = registerSideEffects(storeInstance, 'old-flip', {
      flipPaths: [['topBool', 'form.fields.enabled']],
    })

    expect(storeInstance.state.topBool).toBe(true)
    expect(storeInstance.state.form.fields.enabled).toBe(false)

    // Replace form — fields.enabled is gone
    processChanges(storeInstance, [
      ['form', {}, {} as GenericMeta],
    ] as ArrayOfChanges<State, GenericMeta>)

    expect(
      (storeInstance.state.form as Record<string, unknown>)['fields'],
    ).toBeUndefined()

    // Change topBool — should NOT flip to dead form.fields.enabled
    processChanges(storeInstance, [
      ['topBool', false, {} as GenericMeta],
    ] as ArrayOfChanges<State, GenericMeta>)

    expect(storeInstance.state.topBool).toBe(false)
    // No ghost form.fields created by stale flip
    expect(
      (storeInstance.state.form as Record<string, unknown>)['fields'],
    ).toBeUndefined()

    oldCleanup()
  })
})
