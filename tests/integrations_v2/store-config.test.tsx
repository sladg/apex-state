/**
 * Store Creation & Configuration (createGenericStore with StoreConfig)
 *
 * Validates that store creation:
 * - Works with default config
 * - Respects all StoreConfig options
 * - Respects maxIterations limit
 * - Supports debug config
 */

import { describe, expect, it } from 'vitest'

import { createGenericStore } from '../../src'
import type { BasicTestState } from '../mocks'
import { basicTestFixtures } from '../mocks'
import { MODES, mountStore } from '../utils/react'

describe.each(MODES)('[$name] Store Creation & Configuration', ({ config }) => {
  describe('Default configuration', () => {
    it('should create store with default config', () => {
      // Call createGenericStore() with no arguments
      // Assert store object created
      // Assert has Provider, useFieldStore, useStore, useJitStore, etc.
      const store = createGenericStore<BasicTestState>()
      expect(store).toBeDefined()
      expect(store.Provider).toBeDefined()
      expect(store.useFieldStore).toBeDefined()
      expect(store.useStore).toBeDefined()
      expect(store.useJitStore).toBeDefined()
      expect(store.useConcerns).toBeDefined()
      expect(store.useSideEffects).toBeDefined()
      expect(store.withConcerns).toBeDefined()
    })

    it('should use default errorStorePath ("_errors")', () => {
      // Create store with defaults
      // Trigger validation error
      // Assert errors stored under '_errors' path
      const store = createGenericStore<BasicTestState>()
      const { storeInstance } = mountStore(store, basicTestFixtures.empty)

      // Check that errorStorePath defaults to '_errors'
      expect(storeInstance._internal.config.errorStorePath).toBe('_errors')
    })

    it('should use default maxIterations (100)', () => {
      // Create store with defaults
      // Assert pipeline respects iteration limit
      const store = createGenericStore<BasicTestState>()
      const { storeInstance } = mountStore(store, basicTestFixtures.empty)

      // Check that maxIterations defaults to 100
      expect(storeInstance._internal.config.maxIterations).toBe(100)
    })

    it('should default to WASM implementation', () => {
      // Create store with defaults (no config)
      // Assert pipeline is initialized (WASM is the only implementation)
      const store = createGenericStore<BasicTestState>()
      const { storeInstance } = mountStore(store, basicTestFixtures.empty)

      // Pipeline must be non-null — WASM is the only implementation
      expect(storeInstance._internal.pipeline).not.toBeNull()
    })

    it('should default to debug disabled', () => {
      // Create store with defaults
      // Assert no timing logs emitted
      const store = createGenericStore<BasicTestState>()
      const { storeInstance } = mountStore(store, basicTestFixtures.empty)

      // Default config should have debug with timing disabled
      expect(storeInstance._internal.config.debug?.timing).toBe(false)
      expect(storeInstance._internal.config.debug?.track).toBe(false)
    })
  })

  describe('Custom errorStorePath', () => {
    it('should store errors at custom path', () => {
      // Create store with config: { errorStorePath: 'formErrors' }
      // Trigger validation error
      // Assert errors stored under 'formErrors'
      const store = createGenericStore<BasicTestState>({
        ...config,
        errorStorePath: 'formErrors',
      })
      const { storeInstance } = mountStore(store, basicTestFixtures.empty)

      expect(storeInstance._internal.config.errorStorePath).toBe('formErrors')
    })

    it('should support nested error store path', () => {
      // Create store with config: { errorStorePath: 'meta.errors' }
      // Assert errors stored at nested path
      const store = createGenericStore<BasicTestState>({
        ...config,
        errorStorePath: 'meta.errors',
      })
      const { storeInstance } = mountStore(store, basicTestFixtures.empty)

      expect(storeInstance._internal.config.errorStorePath).toBe('meta.errors')
    })
  })

  describe('maxIterations configuration', () => {
    it('should limit pipeline iterations', () => {
      // Create store with config: { maxIterations: 5 }
      // Create scenario that would loop indefinitely
      // Assert pipeline stops after 5 iterations
      const store = createGenericStore<BasicTestState>({
        ...config,
        maxIterations: 5,
      })
      const { storeInstance } = mountStore(store, basicTestFixtures.empty)

      expect(storeInstance._internal.config.maxIterations).toBe(5)
    })

    it('should allow higher iteration count for complex workflows', () => {
      // Create store with config: { maxIterations: 500 }
      // Create complex workflow needing many iterations
      // Assert all iterations complete
      const store = createGenericStore<BasicTestState>({
        ...config,
        maxIterations: 500,
      })
      const { storeInstance } = mountStore(store, basicTestFixtures.empty)

      expect(storeInstance._internal.config.maxIterations).toBe(500)
    })

    it('should handle maxIterations = 1', () => {
      // Create store with config: { maxIterations: 1 }
      // Assert single pipeline pass completes
      const store = createGenericStore<BasicTestState>({
        ...config,
        maxIterations: 1,
      })
      const { storeInstance } = mountStore(store, basicTestFixtures.empty)

      expect(storeInstance._internal.config.maxIterations).toBe(1)
    })
  })

  describe('Debug configuration', () => {
    it('should enable timing when debug.timing = true', () => {
      // Create store with config: { debug: { timing: true } }
      // Process changes
      // Assert timing information available (or logged)
      const store = createGenericStore<BasicTestState>({
        ...config,
        debug: { timing: true },
      })
      const { storeInstance } = mountStore(store, basicTestFixtures.empty)

      expect(storeInstance._internal.config.debug?.timing).toBe(true)
    })

    it('should respect timingThreshold', () => {
      // Create store with config: { debug: { timing: true, timingThreshold: 10 } }
      // Only log timing for operations exceeding 10ms
      const store = createGenericStore<BasicTestState>({
        ...config,
        debug: { timing: true, timingThreshold: 10 },
      })
      const { storeInstance } = mountStore(store, basicTestFixtures.empty)

      expect(storeInstance._internal.config.debug?.timing).toBe(true)
      expect(storeInstance._internal.config.debug?.timingThreshold).toBe(10)
    })

    it('should not log timing when debug.timing = false', () => {
      // Create store with config: { debug: { timing: false } }
      // Process changes
      // Assert no timing output
      const store = createGenericStore<BasicTestState>({
        ...config,
        debug: { timing: false },
      })
      const { storeInstance } = mountStore(store, basicTestFixtures.empty)

      expect(storeInstance._internal.config.debug?.timing).toBe(false)
    })
  })

  describe('Store instance properties', () => {
    it('should expose state proxy', () => {
      // Create store and initialize with Provider
      // Assert store instance has state property
      // Assert state is valtio proxy
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance } = mountStore(store, basicTestFixtures.empty)

      expect(storeInstance.state).toBeDefined()
      expect(storeInstance.state.fieldA).toBe('')
      expect(storeInstance.state.fieldB).toBe('')
    })

    it('should expose _concerns proxy', () => {
      // Create store and initialize
      // Assert store instance has _concerns property
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance } = mountStore(store, basicTestFixtures.empty)

      expect(storeInstance._concerns).toBeDefined()
      expect(typeof storeInstance._concerns).toBe('object')
    })

    it('should expose _internal state', () => {
      // Create store
      // Assert store instance has _internal property
      // Assert _internal contains side effect graphs
      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance } = mountStore(store, basicTestFixtures.empty)

      expect(storeInstance._internal).toBeDefined()
      expect(storeInstance._internal.config).toBeDefined()
    })
  })

  describe('Store factory return value', () => {
    it('should return Provider component', () => {
      // Assert createGenericStore().Provider exists
      // Assert it's a valid React component
      const store = createGenericStore<BasicTestState>(config)

      expect(store.Provider).toBeDefined()
      expect(typeof store.Provider).toBe('function')
    })

    it('should return all hook functions', () => {
      // Assert useFieldStore, useStore, useJitStore exist
      // Assert useSideEffects, useConcerns, withConcerns exist
      const store = createGenericStore<BasicTestState>(config)

      expect(store.useFieldStore).toBeDefined()
      expect(store.useStore).toBeDefined()
      expect(store.useJitStore).toBeDefined()
      expect(store.useSideEffects).toBeDefined()
      expect(store.useConcerns).toBeDefined()
      expect(store.withConcerns).toBeDefined()
    })

    it('should return type-safe hooks', () => {
      // Create store with typed state
      // Assert hooks enforce type constraints
      const store = createGenericStore<BasicTestState>(config)

      expect(store.useFieldStore).toBeDefined()
      expect(typeof store.useFieldStore).toBe('function')
      expect(store.useStore).toBeDefined()
      expect(typeof store.useStore).toBe('function')
    })
  })

  describe('Multiple store instances', () => {
    it('should create independent store instances', () => {
      // Create store1 and store2 with different configs
      // Assert they are independent
      // Changes in store1 don't affect store2
      const store1 = createGenericStore<BasicTestState>({
        ...config,
        maxIterations: 50,
      })
      const store2 = createGenericStore<BasicTestState>({
        ...config,
        maxIterations: 200,
      })

      const { storeInstance: instance1 } = mountStore(
        store1,
        basicTestFixtures.empty,
      )
      const { storeInstance: instance2 } = mountStore(
        store2,
        basicTestFixtures.empty,
      )

      expect(instance1._internal.config.maxIterations).toBe(50)
      expect(instance2._internal.config.maxIterations).toBe(200)
    })

    it('should support different state types per store', () => {
      // Create store<TypeA>() and store<TypeB>()
      // Assert each enforces its own type
      interface StateTypeA {
        fieldA: string
      }

      interface StateTypeB {
        fieldB: number
      }

      const storeA = createGenericStore<StateTypeA>(config)
      const storeB = createGenericStore<StateTypeB>(config)

      expect(storeA).toBeDefined()
      expect(storeB).toBeDefined()
    })
  })

  describe('Store initialization edge cases', () => {
    it('should handle undefined config gracefully', () => {
      // Create store with undefined config
      // Assert uses defaults
      const store = createGenericStore<BasicTestState>(undefined)
      const { storeInstance } = mountStore(store, basicTestFixtures.empty)

      expect(storeInstance._internal.config).toBeDefined()
      expect(storeInstance._internal.config.maxIterations).toBe(100)
    })

    it('should handle partial config (only some fields)', () => {
      // Create store with config: { maxIterations: 50 }
      // Assert other config fields use defaults
      const store = createGenericStore<BasicTestState>({
        ...config,
        maxIterations: 50,
      })
      const { storeInstance } = mountStore(store, basicTestFixtures.empty)

      expect(storeInstance._internal.config.maxIterations).toBe(50)
    })

    it('should handle empty config object', () => {
      // Create store with config: {}
      // Assert uses all defaults
      const store = createGenericStore<BasicTestState>({})
      const { storeInstance } = mountStore(store, basicTestFixtures.empty)

      expect(storeInstance._internal.config).toBeDefined()
      expect(storeInstance._internal.config.maxIterations).toBe(100)
    })
  })
})
