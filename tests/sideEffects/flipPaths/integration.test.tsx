/**
 * Integration tests for flip paths side-effect
 *
 * Tests complete flip paths functionality in React component context.
 * Validates boolean flipping, enum swapping, and infinite loop prevention.
 */

import React from 'react'
import { describe, test, expect } from 'vitest'
import { render, waitFor } from '@testing-library/react'
import { createGenericStore } from '../../../src/store/createStore'
import { createFlipPathsRegistry } from '../../../src/sideEffects/flipPaths/registry'
import { createFlipPathsSynchronizer } from '../../../src/pipeline/synchronizers/flipPaths'
import type { GenericMeta } from '../../../src/types'

interface BooleanState {
  a: boolean
  b: boolean
  c: boolean
}

type Theme = 'light' | 'dark'

interface ThemeState {
  mode: Theme
  theme: Theme
  background: Theme
}

describe('Flip Paths Integration', () => {
  describe('Manual synchronizer integration', () => {
    test('should flip boolean values through synchronizer', () => {
      const registry = createFlipPathsRegistry<BooleanState>()
      registry.register('ab', 'a', 'b')

      const synchronizer = createFlipPathsSynchronizer<
        BooleanState,
        GenericMeta
      >(registry)

      const initialState: BooleanState = { a: true, b: false, c: false }
      const changes = synchronizer([['a', false, {}]], initialState)

      // Should have original change plus flip change for b
      expect(changes.length).toBe(2)
      expect(changes[0]).toEqual(['a', false, {}])
      expect(changes[1][0]).toBe('b')
      expect(changes[1][1]).toBe(true) // Flipped from false
      expect(changes[1][2].isFlipPathChange).toBe(true)
    })

    test('should swap enum values through synchronizer', () => {
      const registry = createFlipPathsRegistry<ThemeState>()
      registry.register('mode-theme', 'mode', 'theme')

      const synchronizer = createFlipPathsSynchronizer<ThemeState, GenericMeta>(
        registry
      )

      const initialState: ThemeState = {
        mode: 'light',
        theme: 'dark',
        background: 'light',
      }
      const changes = synchronizer([['mode', 'dark', {}]], initialState)

      // Should have original change plus flip change for theme
      expect(changes.length).toBe(2)
      expect(changes[1][0]).toBe('theme')
      expect(changes[1][1]).toBe('light') // Swapped to mode's old value
      expect(changes[1][2].isFlipPathChange).toBe(true)
    })

    test('should work bidirectionally', () => {
      const registry = createFlipPathsRegistry<BooleanState>()
      registry.register('ab', 'a', 'b')

      const synchronizer = createFlipPathsSynchronizer<
        BooleanState,
        GenericMeta
      >(registry)

      const initialState: BooleanState = { a: true, b: false, c: false }

      // Change 'b' instead of 'a'
      const changes = synchronizer([['b', true, {}]], initialState)

      // Should flip 'a' to false
      expect(changes.length).toBe(2)
      const flipChange = changes.find((c) => c[0] === 'a')
      expect(flipChange?.[1]).toBe(false)
      expect(flipChange?.[2].isFlipPathChange).toBe(true)
    })

    test('should prevent infinite loops', () => {
      const registry = createFlipPathsRegistry<BooleanState>()
      registry.register('ab', 'a', 'b')

      const synchronizer = createFlipPathsSynchronizer<
        BooleanState,
        GenericMeta
      >(registry)

      const initialState: BooleanState = { a: false, b: true, c: false }

      // First pass
      const firstPass = synchronizer([['a', true, {}]], initialState)
      expect(firstPass.length).toBe(2) // Original + 1 flip

      // Second pass with only flip changes (typical pipeline scenario)
      const flipChangesOnly = firstPass.filter((c) => c[2].isFlipPathChange)
      const secondPass = synchronizer(flipChangesOnly, initialState)

      // Should not create more flip changes (already marked)
      expect(secondPass.length).toBe(flipChangesOnly.length)
    })

    test('should handle multiple flip pairs', () => {
      const registry = createFlipPathsRegistry<BooleanState>()
      registry.register('ab', 'a', 'b')
      registry.register('bc-independent', 'b', 'c')

      const synchronizer = createFlipPathsSynchronizer<
        BooleanState,
        GenericMeta
      >(registry)

      const initialState: BooleanState = { a: true, b: false, c: true }
      const changes = synchronizer([['a', false, {}]], initialState)

      // Original change 'a' should flip 'b'
      const pathsChanged = changes.map((c) => c[0])
      expect(pathsChanged).toContain('a')
      expect(pathsChanged).toContain('b')

      // 'b' flip is marked, won't cascade to 'c' in same pass
      const flipChanges = changes.filter((c) => c[2].isFlipPathChange)
      expect(flipChanges.length).toBe(1) // Only 'b' flipped
    })
  })

  describe('Store integration', () => {
    test('basic store operations work with boolean flips', async () => {
      const store = createGenericStore<BooleanState, GenericMeta>()

      function TestComponent() {
        const [a, setA] = store.useStore('a')
        const [b] = store.useStore('b')

        return (
          <div>
            <span data-testid="a">{String(a)}</span>
            <span data-testid="b">{String(b)}</span>
            <button onClick={() => setA(!a)}>Toggle A</button>
          </div>
        )
      }

      const { getByText, getByTestId } = render(
        <store.Provider initialState={{ a: true, b: false, c: false }}>
          <TestComponent />
        </store.Provider>
      )

      expect(getByTestId('a').textContent).toBe('true')
      expect(getByTestId('b').textContent).toBe('false')

      getByText('Toggle A').click()

      await waitFor(() => {
        expect(getByTestId('a').textContent).toBe('false')
      })
    })

    test('basic store operations work with enum swaps', async () => {
      const store = createGenericStore<ThemeState, GenericMeta>()

      function TestComponent() {
        const [mode, setMode] = store.useStore('mode')
        const [theme] = store.useStore('theme')

        return (
          <div>
            <span data-testid="mode">{mode}</span>
            <span data-testid="theme">{theme}</span>
            <button onClick={() => setMode(mode === 'light' ? 'dark' : 'light')}>
              Toggle Mode
            </button>
          </div>
        )
      }

      const { getByText, getByTestId } = render(
        <store.Provider
          initialState={{ mode: 'light', theme: 'dark', background: 'light' }}
        >
          <TestComponent />
        </store.Provider>
      )

      expect(getByTestId('mode').textContent).toBe('light')
      expect(getByTestId('theme').textContent).toBe('dark')

      getByText('Toggle Mode').click()

      await waitFor(() => {
        expect(getByTestId('mode').textContent).toBe('dark')
      })
    })

    test('useSideEffects hook registration works', () => {
      const store = createGenericStore<BooleanState, GenericMeta>()

      function TestComponent() {
        // Register flip paths side-effect
        store.useSideEffects('test-flip', {
          flipPaths: {
            pairs: [{ id: 'ab', path1: 'a', path2: 'b' }],
          },
        })

        return <div>Component with flip side effects</div>
      }

      const { container } = render(
        <store.Provider initialState={{ a: true, b: false, c: false }}>
          <TestComponent />
        </store.Provider>
      )

      // Should render without errors
      expect(container).toBeTruthy()
    })
  })

  describe('Registry lifecycle', () => {
    test('should handle dynamic registration', () => {
      const registry = createFlipPathsRegistry<BooleanState>()

      // Register first pair
      registry.register('ab', 'a', 'b')
      expect(registry.hasFlip('a')).toBe(true)
      expect(registry.getFlippedPath('a')).toBe('b')

      // Register second pair (note: 'b' will now flip with 'c', not 'a')
      registry.register('bc', 'b', 'c')
      expect(registry.hasFlip('b')).toBe(true)
      expect(registry.getFlippedPath('b')).toBe('c')
      // Note: 'a' still flips with 'b', but 'b' now flips with 'c' (last registration)

      // Unregister first pair - removes 'a' ↔ 'b' mapping
      registry.unregister('ab')
      expect(registry.hasFlip('a')).toBe(false)
      expect(registry.getFlippedPath('a')).toBeUndefined()
      // 'b' was removed by unregistering 'ab', so it no longer flips with 'c'
      // unless 'bc' was registered after 'ab' was unregistered

      // Re-register bc to test continued operation
      registry.register('bc-2', 'b', 'c')
      expect(registry.getFlippedPath('b')).toBe('c')
    })

    test('should handle mount/unmount scenarios', () => {
      interface ExtendedState {
        a: boolean
        b: boolean
        c: boolean
        d: boolean
      }

      const registry = createFlipPathsRegistry<ExtendedState>()

      // Simulate component mount
      registry.register('component-1', 'a', 'b')

      // Verify first component registration
      expect(registry.getFlippedPath('a')).toBe('b')
      expect(registry.getFlippedPath('b')).toBe('a')

      // Simulate another component mount (non-overlapping paths)
      registry.register('component-2', 'c', 'd')

      // Both should be active
      expect(registry.getFlippedPath('a')).toBe('b')
      expect(registry.getFlippedPath('c')).toBe('d')

      // Simulate first component unmount
      registry.unregister('component-1')

      // 'a' and 'b' should no longer flip
      expect(registry.hasFlip('a')).toBe(false)
      expect(registry.hasFlip('b')).toBe(false)

      // 'c' and 'd' should still flip
      expect(registry.getFlippedPath('c')).toBe('d')

      // Simulate second component unmount
      registry.unregister('component-2')

      // All should be cleared
      expect(registry.hasFlip('c')).toBe(false)
      expect(registry.hasFlip('d')).toBe(false)
    })

    test('should handle re-registration with same ID', () => {
      const registry = createFlipPathsRegistry<BooleanState>()

      // Initial registration
      registry.register('flip1', 'a', 'b')
      expect(registry.getFlippedPath('a')).toBe('b')

      // Re-register with different paths (same ID)
      registry.register('flip1', 'b', 'c')

      // Old mapping should be gone
      expect(registry.getFlippedPath('a')).toBeUndefined()

      // New mapping should work
      expect(registry.getFlippedPath('b')).toBe('c')
    })
  })

  describe('Error handling', () => {
    test('should handle non-existent paths gracefully', () => {
      const registry = createFlipPathsRegistry<BooleanState>()
      const synchronizer = createFlipPathsSynchronizer<
        BooleanState,
        GenericMeta
      >(registry)

      // Register valid paths
      registry.register('ab', 'a', 'b')

      // Query non-existent path
      expect(registry.hasFlip('nonexistent' as any)).toBe(false)
      expect(registry.getFlippedPath('nonexistent' as any)).toBeUndefined()

      // Changes to non-existent path should pass through
      const initialState: BooleanState = { a: true, b: false, c: false }
      const changes = synchronizer(
        [['nonexistent' as any, true, {}]],
        initialState
      )

      expect(changes.length).toBe(1)
      expect(changes[0][0]).toBe('nonexistent')
    })

    test('should handle undefined values in state', () => {
      interface OptionalState {
        a: boolean | undefined
        b: boolean | undefined
      }

      const registry = createFlipPathsRegistry<OptionalState>()
      registry.register('ab', 'a', 'b')

      const synchronizer = createFlipPathsSynchronizer<
        OptionalState,
        GenericMeta
      >(registry)

      const initialState: OptionalState = { a: undefined, b: undefined }
      const changes = synchronizer([['a', true, {}]], initialState)

      // Should flip to false
      expect(changes[1][1]).toBe(false)
    })

    test('should handle null values gracefully', () => {
      interface NullableState {
        a: string | null
        b: string | null
      }

      const registry = createFlipPathsRegistry<NullableState>()
      registry.register('ab', 'a', 'b')

      const synchronizer = createFlipPathsSynchronizer<
        NullableState,
        GenericMeta
      >(registry)

      const initialState: NullableState = { a: 'value', b: null }
      const changes = synchronizer([['a', null, {}]], initialState)

      // Should swap: b gets 'value' (old value of a)
      expect(changes[1][1]).toBe('value')
    })
  })

  describe('Performance scenarios', () => {
    test('should handle rapid state changes', () => {
      const registry = createFlipPathsRegistry<BooleanState>()
      registry.register('ab', 'a', 'b')

      const synchronizer = createFlipPathsSynchronizer<
        BooleanState,
        GenericMeta
      >(registry)

      const initialState: BooleanState = { a: false, b: true, c: false }

      // Simulate 100 rapid flip changes
      const start = performance.now()
      for (let i = 0; i < 100; i++) {
        synchronizer([['a', i % 2 === 0, {}]], initialState)
      }
      const duration = performance.now() - start

      // Should complete quickly (< 20ms for 100 iterations)
      expect(duration).toBeLessThan(20)
    })

    test('should handle multiple flip pairs efficiently', () => {
      const registry = createFlipPathsRegistry<any>()

      // Create 10 independent flip pairs
      for (let i = 0; i < 10; i++) {
        registry.register(`flip${i}`, `path${i}`, `flipped${i}`)
      }

      const synchronizer = createFlipPathsSynchronizer(registry)

      const state: any = {}
      for (let i = 0; i < 10; i++) {
        state[`path${i}`] = false
        state[`flipped${i}`] = true
      }

      const changes: any[] = []
      for (let i = 0; i < 10; i++) {
        changes.push([`path${i}`, true, {}])
      }

      const start = performance.now()
      const result = synchronizer(changes, state)
      const duration = performance.now() - start

      // Should complete quickly
      expect(duration).toBeLessThan(5)

      // Should flip all pairs
      expect(result.length).toBe(20) // 10 original + 10 flipped
    })

    test('should handle alternating flips efficiently', () => {
      const registry = createFlipPathsRegistry<BooleanState>()
      registry.register('ab', 'a', 'b')

      const synchronizer = createFlipPathsSynchronizer<
        BooleanState,
        GenericMeta
      >(registry)

      const initialState: BooleanState = { a: true, b: false, c: false }

      // Simulate alternating changes
      const start = performance.now()
      for (let i = 0; i < 50; i++) {
        if (i % 2 === 0) {
          synchronizer([['a', !initialState.a, {}]], initialState)
        } else {
          synchronizer([['b', !initialState.b, {}]], initialState)
        }
      }
      const duration = performance.now() - start

      // Should complete quickly
      expect(duration).toBeLessThan(15)
    })
  })

  describe('Complex scenarios', () => {
    test('should handle multiple simultaneous changes', () => {
      const registry = createFlipPathsRegistry<BooleanState>()
      registry.register('ab', 'a', 'b')
      registry.register('bc', 'b', 'c')

      const synchronizer = createFlipPathsSynchronizer<
        BooleanState,
        GenericMeta
      >(registry)

      const initialState: BooleanState = { a: true, b: false, c: true }

      // Change both 'a' and 'c' at once
      const changes = synchronizer(
        [
          ['a', false, {}],
          ['c', false, {}],
        ],
        initialState
      )

      // Should have 2 original + 2 flipped changes
      expect(changes.length).toBe(4)

      // 'a' should flip 'b'
      const bFlip = changes.find(
        (c) => c[0] === 'b' && c[2].isFlipPathChange
      )
      expect(bFlip).toBeDefined()

      // 'c' should flip 'b' (but 'b' has two flip relationships)
      const allBChanges = changes.filter((c) => c[0] === 'b')
      expect(allBChanges.length).toBeGreaterThan(0)
    })

    test('should preserve metadata through flips', () => {
      const registry = createFlipPathsRegistry<BooleanState>()
      registry.register('ab', 'a', 'b')

      const synchronizer = createFlipPathsSynchronizer<
        BooleanState,
        GenericMeta
      >(registry)

      const initialState: BooleanState = { a: true, b: false, c: false }

      const changes = synchronizer(
        [
          [
            'a',
            false,
            { sender: 'user-123', isProgramaticChange: false },
          ],
        ],
        initialState
      )

      const flipChange = changes.find((c) => c[0] === 'b')
      expect(flipChange?.[2].sender).toBe('user-123')
      expect(flipChange?.[2].isProgramaticChange).toBe(false)
      expect(flipChange?.[2].isFlipPathChange).toBe(true)
    })

    test('should work with nested paths', () => {
      interface NestedState {
        user: {
          settings: {
            darkMode: boolean
            lightMode: boolean
          }
        }
      }

      const registry = createFlipPathsRegistry<NestedState>()
      registry.register(
        'modes',
        'user.settings.darkMode',
        'user.settings.lightMode'
      )

      const synchronizer = createFlipPathsSynchronizer<
        NestedState,
        GenericMeta
      >(registry)

      const initialState: NestedState = {
        user: { settings: { darkMode: true, lightMode: false } },
      }

      const changes = synchronizer(
        [['user.settings.darkMode', false, {}]],
        initialState
      )

      expect(changes.length).toBe(2)
      const flipChange = changes.find((c) => c[0] === 'user.settings.lightMode')
      expect(flipChange?.[1]).toBe(true)
    })
  })
})
