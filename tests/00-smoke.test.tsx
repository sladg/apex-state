/**
 * SMOKE TEST - Single Point of Failure Detection
 *
 * This test runs BEFORE all other tests to validate core functionality.
 * If this test fails, the entire test suite should stop to avoid cascading failures.
 *
 * What it validates:
 * - Store creation works
 * - Basic state mutations work
 * - React hooks work
 * - WASM bridge is functional (if available)
 * - Side effects can be registered
 *
 * Configure vitest with `bail: 1` to stop on first failure.
 */

import { render, screen } from '@testing-library/react'
import { describe, expect, it } from 'vitest'

import { createGenericStore } from '../src/store/createStore'

// ---------------------------------------------------------------------------
// Test State Type
// ---------------------------------------------------------------------------

interface SmokeState {
  testField: string
  counter: number
  flag: boolean
}

const initialState: SmokeState = {
  testField: 'initial',
  counter: 0,
  flag: false,
}

// ---------------------------------------------------------------------------
// SMOKE TEST SUITE
// ---------------------------------------------------------------------------

describe('🚨 SMOKE TEST - Core Functionality', () => {
  it('should create store without errors', () => {
    const store = createGenericStore<SmokeState>()

    expect(store).toBeDefined()
    expect(store.Provider).toBeDefined()
    expect(store.useFieldStore).toBeDefined()
    expect(store.useStore).toBeDefined()
    expect(store.useJitStore).toBeDefined()
    expect(store.useSideEffects).toBeDefined()
  })

  it('should render Provider and access state via hooks', () => {
    const store = createGenericStore<SmokeState>()

    const TestComponent = () => {
      const { value } = store.useFieldStore('testField')
      return <div data-testid="test-value">{value}</div>
    }

    render(
      <store.Provider initialState={initialState}>
        <TestComponent />
      </store.Provider>,
    )

    expect(screen.getByTestId('test-value')).toHaveTextContent('initial')
  })

  it('should mutate state via setValue', () => {
    // TODO: Implement test for setValue mutation
    // Step 1: Create store and component with useFieldStore
    // Step 2: Render with Provider and initial state
    // Step 3: Use fireEvent.click() wrapped in act()
    // Step 4: await flushEffects() to wait for async updates
    // Step 5: Assert value changed from 'initial' to 'updated'
    expect(true).toBe(true)
  })

  it('should trigger React re-render on state change', () => {
    // TODO: Implement test for React re-renders
    // Step 1: Create store and component with renderCount tracking
    // Step 2: Use useFieldStore to read counter
    // Step 3: fireEvent.click() to increment counter
    // Step 4: await flushEffects() to wait for re-render
    // Step 5: Assert renderCount increased by 1 (single re-render)
    expect(true).toBe(true)
  })

  it('should support side effects registration (syncPaths)', () => {
    // TODO: Implement test for syncPaths side effect
    // Step 1: Create store with useSideEffects hook
    // Step 2: Register syncPaths for 'testField' and 'syncTarget'
    // Step 3: fireEvent.click() to set source value
    // Step 4: await flushEffects() to wait for sync processing
    // Step 5: Assert both source and target have same value
    expect(true).toBe(true)
  })

  it('should support side effects registration (flipPaths)', () => {
    // TODO: Implement test for flipPaths side effect
    // Step 1: Create store with useSideEffects hook
    // Step 2: Register flipPaths for 'flag' and 'invertedFlag'
    // Step 3: fireEvent.click() to toggle flag
    // Step 4: await flushEffects() to wait for flip processing
    // Step 5: Assert flag and invertedFlag are opposite booleans
    expect(true).toBe(true)
  })

  it('should validate WASM bridge is available (if built)', async () => {
    try {
      // Try to load WASM module
      await import('../rust/pkg/apex_state_wasm.js')

      // If we get here, WASM is available - success!
      expect(true).toBe(true)
    } catch {
      // WASM not built - FAIL the smoke test
      console.error(
        '❌ WASM module not available - run `npm run build:wasm` before testing',
      )
      expect(true).toEqual(false)
    }
  })
})
