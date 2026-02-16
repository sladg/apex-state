/**
 * Integration Tests: Aggregation Behavior
 *
 * Tests the core aggregation logic:
 * - Target value calculation based on source path equality
 * - Interaction with sync/flip paths
 * - Bidirectional updates (write to target distributes to sources)
 */

import { screen } from '@testing-library/react'
import { beforeEach, describe, expect, it } from 'vitest'

import { createGenericStore } from '../../src/store/createStore'
import { flushEffects, mountStore } from '../utils/react'

// Use symbol-based enum for clearer test semantics and type safety
const SelectionSymbol = {
  Selected: Symbol('Selected'),
  Unselected: Symbol('Unselected'),
} as const

type SelectionType = (typeof SelectionSymbol)[keyof typeof SelectionSymbol]

// Helper to convert symbol to display string
const selectionToString = (s: SelectionType | undefined): string => {
  if (s === undefined) return 'undefined'
  if (s === SelectionSymbol.Selected) return 'Selected'
  if (s === SelectionSymbol.Unselected) return 'Unselected'
  return 'unknown'
}

interface AggregationTestState {
  // Aggregation target and sources must have same type for type-safe paths
  allSelected: SelectionType
  masterValue: string

  // Source paths for aggregation (same type as target)
  item1: { selected: SelectionType; value: string }
  item2: { selected: SelectionType; value: string }
  item3: { selected: SelectionType; value: string }
  item4: { selected: SelectionType; value: string }
}

describe('Aggregation Behavior', () => {
  let store: ReturnType<typeof createGenericStore<AggregationTestState>>

  beforeEach(() => {
    store = createGenericStore<AggregationTestState>()
  })

  describe('Target calculation from source paths', () => {
    it('should keep target unchanged when 0 source paths exist', async () => {
      // This tests what happens with an empty aggregation
      // (edge case - aggregation with no sources)
      const TestComponent = () => {
        const [allSelected] = store.useStore('allSelected')

        // Register aggregation with no source paths (empty array scenario)
        // We simulate this by having target but no sources defined
        store.useSideEffects('empty-agg', {
          aggregations: [],
        })

        return (
          <div>
            <div data-testid="all-selected">
              {selectionToString(allSelected)}
            </div>
          </div>
        )
      }

      mountStore(<TestComponent />, store, {
        allSelected: SelectionSymbol.Unselected,
        masterValue: '',
        item1: { selected: SelectionSymbol.Unselected, value: 'a' },
        item2: { selected: SelectionSymbol.Unselected, value: 'b' },
        item3: { selected: SelectionSymbol.Unselected, value: 'c' },
        item4: { selected: SelectionSymbol.Unselected, value: 'd' },
      })

      await flushEffects()

      // With no aggregation sources, target keeps its initial value
      expect(screen.getByTestId('all-selected').textContent).toBe('Unselected')
    })

    it('should correctly sync from 1 source path', async () => {
      const TestComponent = () => {
        const [allSelected] = store.useStore('allSelected')
        const [, setItem1Selected] = store.useStore('item1.selected')

        // Single source aggregation
        store.useSideEffects('single-source-agg', {
          aggregations: [['allSelected', 'item1.selected']],
        })

        return (
          <div>
            <button
              onClick={() => setItem1Selected(SelectionSymbol.Selected)}
              data-testid="set-selected"
            >
              Set Selected
            </button>
            <button
              onClick={() => setItem1Selected(SelectionSymbol.Unselected)}
              data-testid="set-unselected"
            >
              Set Unselected
            </button>
            <div data-testid="all-selected">
              {selectionToString(allSelected)}
            </div>
          </div>
        )
      }

      mountStore(<TestComponent />, store, {
        allSelected: SelectionSymbol.Unselected,
        masterValue: '',
        item1: { selected: SelectionSymbol.Unselected, value: 'a' },
        item2: { selected: SelectionSymbol.Unselected, value: 'b' },
        item3: { selected: SelectionSymbol.Unselected, value: 'c' },
        item4: { selected: SelectionSymbol.Unselected, value: 'd' },
      })

      await flushEffects()

      // With 1 source, target should match that source
      expect(screen.getByTestId('all-selected').textContent).toBe('Unselected')

      // Change source to Selected
      screen.getByTestId('set-selected').click()
      await flushEffects()

      expect(screen.getByTestId('all-selected').textContent).toBe('Selected')

      // Change source back to Unselected
      screen.getByTestId('set-unselected').click()
      await flushEffects()

      expect(screen.getByTestId('all-selected').textContent).toBe('Unselected')
    })

    it('should set target to undefined when adding path with different value to synced paths', async () => {
      const TestComponent = () => {
        const [allSelected] = store.useStore('allSelected')
        const [, setItem4Selected] = store.useStore('item4.selected')

        // 4 sources - all start with same value
        store.useSideEffects('multi-source-agg', {
          aggregations: [
            ['allSelected', 'item1.selected'],
            ['allSelected', 'item2.selected'],
            ['allSelected', 'item3.selected'],
            ['allSelected', 'item4.selected'],
          ],
        })

        return (
          <div>
            <button
              onClick={() => setItem4Selected(SelectionSymbol.Unselected)}
              data-testid="set-different"
            >
              Set Different
            </button>
            <div data-testid="all-selected">
              {selectionToString(allSelected)}
            </div>
          </div>
        )
      }

      // Start with all items Selected
      mountStore(<TestComponent />, store, {
        allSelected: SelectionSymbol.Unselected,
        masterValue: '',
        item1: { selected: SelectionSymbol.Selected, value: 'a' },
        item2: { selected: SelectionSymbol.Selected, value: 'b' },
        item3: { selected: SelectionSymbol.Selected, value: 'c' },
        item4: { selected: SelectionSymbol.Selected, value: 'd' },
      })

      await flushEffects()

      // All 4 sources are Selected, so target should be Selected
      expect(screen.getByTestId('all-selected').textContent).toBe('Selected')

      // Change item4 to Unselected (different from the other 3)
      screen.getByTestId('set-different').click()
      await flushEffects()

      // Now sources have different values, target should be undefined
      expect(screen.getByTestId('all-selected').textContent).toBe('undefined')
    })

    it('should handle write to target distributing to all sources', async () => {
      const TestComponent = () => {
        const [allSelected, setAllSelected] = store.useStore('allSelected')
        const [item1] = store.useStore('item1.selected')
        const [item2] = store.useStore('item2.selected')
        const [item3] = store.useStore('item3.selected')

        store.useSideEffects('write-distribution-agg', {
          aggregations: [
            ['allSelected', 'item1.selected'],
            ['allSelected', 'item2.selected'],
            ['allSelected', 'item3.selected'],
          ],
        })

        return (
          <div>
            <button
              onClick={() => setAllSelected(SelectionSymbol.Selected)}
              data-testid="select-all"
            >
              Select All
            </button>
            <button
              onClick={() => setAllSelected(SelectionSymbol.Unselected)}
              data-testid="deselect-all"
            >
              Deselect All
            </button>
            <div data-testid="all-selected">
              {selectionToString(allSelected)}
            </div>
            <div data-testid="item1">{selectionToString(item1)}</div>
            <div data-testid="item2">{selectionToString(item2)}</div>
            <div data-testid="item3">{selectionToString(item3)}</div>
          </div>
        )
      }

      // Start with mixed values
      mountStore(<TestComponent />, store, {
        allSelected: SelectionSymbol.Unselected,
        masterValue: '',
        item1: { selected: SelectionSymbol.Selected, value: 'a' },
        item2: { selected: SelectionSymbol.Unselected, value: 'b' },
        item3: { selected: SelectionSymbol.Selected, value: 'c' },
        item4: { selected: SelectionSymbol.Unselected, value: 'd' },
      })

      await flushEffects()

      // Mixed values -> undefined
      expect(screen.getByTestId('all-selected').textContent).toBe('undefined')

      // Write to target should distribute to all sources
      screen.getByTestId('select-all').click()
      await flushEffects()

      expect(screen.getByTestId('all-selected').textContent).toBe('Selected')
      expect(screen.getByTestId('item1').textContent).toBe('Selected')
      expect(screen.getByTestId('item2').textContent).toBe('Selected')
      expect(screen.getByTestId('item3').textContent).toBe('Selected')

      // Deselect all
      screen.getByTestId('deselect-all').click()
      await flushEffects()

      expect(screen.getByTestId('all-selected').textContent).toBe('Unselected')
      expect(screen.getByTestId('item1').textContent).toBe('Unselected')
      expect(screen.getByTestId('item2').textContent).toBe('Unselected')
      expect(screen.getByTestId('item3').textContent).toBe('Unselected')
    })
  })

  describe('Aggregation with flip paths interaction', () => {
    // Note: Flip paths only work with boolean values, so these tests use boolean

    it('should result in undefined when flip paths prevent sources from being equal', async () => {
      // Scenario: 4 paths aggregated to target
      // Two of these paths have a flip relationship
      // When we set target, it distributes to all 4
      // But flip path immediately inverts one of them
      // Result: sources can never all be equal -> target becomes undefined

      interface FlipAggState {
        masterSwitch: boolean
        pathA: boolean // will be flipped with pathB
        pathB: boolean // will be flipped with pathA
        pathC: boolean
        pathD: boolean
      }

      const flipStore = createGenericStore<FlipAggState>()

      const TestComponent = () => {
        const [masterSwitch, setMasterSwitch] =
          flipStore.useStore('masterSwitch')
        const [pathA] = flipStore.useStore('pathA')
        const [pathB] = flipStore.useStore('pathB')
        const [pathC] = flipStore.useStore('pathC')
        const [pathD] = flipStore.useStore('pathD')

        // First register flip paths
        flipStore.useSideEffects('flip-effect', {
          flipPaths: [['pathA', 'pathB']],
        })

        // Then register aggregation across all 4 paths
        flipStore.useSideEffects('agg-effect', {
          aggregations: [
            ['masterSwitch', 'pathA'],
            ['masterSwitch', 'pathB'],
            ['masterSwitch', 'pathC'],
            ['masterSwitch', 'pathD'],
          ],
        })

        return (
          <div>
            <button
              onClick={() => setMasterSwitch(true)}
              data-testid="set-master"
            >
              Set Master True
            </button>
            <div data-testid="master">
              {masterSwitch === undefined ? 'undefined' : String(masterSwitch)}
            </div>
            <div data-testid="pathA">{String(pathA)}</div>
            <div data-testid="pathB">{String(pathB)}</div>
            <div data-testid="pathC">{String(pathC)}</div>
            <div data-testid="pathD">{String(pathD)}</div>
          </div>
        )
      }

      // Start with pathA and pathB already flipped (A=true, B=false)
      mountStore(<TestComponent />, flipStore, {
        masterSwitch: false,
        pathA: true,
        pathB: false, // Flipped from pathA
        pathC: true,
        pathD: true,
      })

      await flushEffects()

      // Initial state: A=true, B=false, C=true, D=true
      // Sources are not all equal -> undefined
      expect(screen.getByTestId('master').textContent).toBe('undefined')
      expect(screen.getByTestId('pathA').textContent).toBe('true')
      expect(screen.getByTestId('pathB').textContent).toBe('false')

      // Try to set master to true
      // This will distribute true to all 4 sources
      // But flip path will then set pathB = !pathA = false
      // So we end up with: A=true, B=false, C=true, D=true
      // Sources not equal -> master becomes undefined
      screen.getByTestId('set-master').click()
      await flushEffects()

      // The key assertion: master should be undefined
      // because the flip relationship interferes with the aggregation
      // When aggregation distributes true to all paths, flip processor
      // processes both pathA and pathB changes, flipping each other
      // This results in a state where not all sources are equal
      expect(screen.getByTestId('master').textContent).toBe('undefined')
    })

    it('should maintain aggregation value when sources remain equal without flip interference', async () => {
      interface NoFlipAggState {
        masterSwitch: boolean
        pathA: boolean
        pathB: boolean
        pathC: boolean
        pathD: boolean
      }

      const noFlipStore = createGenericStore<NoFlipAggState>()

      const TestComponent = () => {
        const [masterSwitch, setMasterSwitch] =
          noFlipStore.useStore('masterSwitch')
        const [pathA] = noFlipStore.useStore('pathA')
        const [pathB] = noFlipStore.useStore('pathB')
        const [pathC] = noFlipStore.useStore('pathC')
        const [pathD] = noFlipStore.useStore('pathD')

        // Only aggregation, no flip paths
        noFlipStore.useSideEffects('agg-only', {
          aggregations: [
            ['masterSwitch', 'pathA'],
            ['masterSwitch', 'pathB'],
            ['masterSwitch', 'pathC'],
            ['masterSwitch', 'pathD'],
          ],
        })

        return (
          <div>
            <button
              onClick={() => setMasterSwitch(true)}
              data-testid="set-master"
            >
              Set Master True
            </button>
            <div data-testid="master">
              {masterSwitch === undefined ? 'undefined' : String(masterSwitch)}
            </div>
            <div data-testid="pathA">{String(pathA)}</div>
            <div data-testid="pathB">{String(pathB)}</div>
            <div data-testid="pathC">{String(pathC)}</div>
            <div data-testid="pathD">{String(pathD)}</div>
          </div>
        )
      }

      // Start with all different values
      mountStore(<TestComponent />, noFlipStore, {
        masterSwitch: false,
        pathA: true,
        pathB: false,
        pathC: true,
        pathD: false,
      })

      await flushEffects()

      // Mixed values -> undefined
      expect(screen.getByTestId('master').textContent).toBe('undefined')

      // Set master to true (should distribute to all)
      screen.getByTestId('set-master').click()
      await flushEffects()

      // Without flip interference, all sources should be true
      expect(screen.getByTestId('master').textContent).toBe('true')
      expect(screen.getByTestId('pathA').textContent).toBe('true')
      expect(screen.getByTestId('pathB').textContent).toBe('true')
      expect(screen.getByTestId('pathC').textContent).toBe('true')
      expect(screen.getByTestId('pathD').textContent).toBe('true')
    })
  })

  describe('Edge cases', () => {
    it('should handle string value aggregation', async () => {
      const TestComponent = () => {
        const [masterValue] = store.useStore('masterValue')
        const [, setItem1Value] = store.useStore('item1.value')
        const [, setItem2Value] = store.useStore('item2.value')

        store.useSideEffects('string-agg', {
          aggregations: [
            ['masterValue', 'item1.value'],
            ['masterValue', 'item2.value'],
          ],
        })

        return (
          <div>
            <button
              onClick={() => {
                setItem1Value('same')
                setItem2Value('same')
              }}
              data-testid="set-same"
            >
              Set Same
            </button>
            <button
              onClick={() => {
                setItem1Value('different1')
                setItem2Value('different2')
              }}
              data-testid="set-different"
            >
              Set Different
            </button>
            <div data-testid="master">
              {masterValue === undefined ? 'undefined' : masterValue}
            </div>
          </div>
        )
      }

      mountStore(<TestComponent />, store, {
        allSelected: SelectionSymbol.Unselected,
        masterValue: '',
        item1: { selected: SelectionSymbol.Unselected, value: 'a' },
        item2: { selected: SelectionSymbol.Unselected, value: 'b' },
        item3: { selected: SelectionSymbol.Unselected, value: 'c' },
        item4: { selected: SelectionSymbol.Unselected, value: 'd' },
      })

      await flushEffects()

      // Different initial values -> undefined
      expect(screen.getByTestId('master').textContent).toBe('undefined')

      // Set both to same value
      screen.getByTestId('set-same').click()
      await flushEffects()

      expect(screen.getByTestId('master').textContent).toBe('same')

      // Set to different values
      screen.getByTestId('set-different').click()
      await flushEffects()

      expect(screen.getByTestId('master').textContent).toBe('undefined')
    })

    it('should update target when source changes to match others', async () => {
      const TestComponent = () => {
        const [allSelected] = store.useStore('allSelected')
        const [, setItem1] = store.useStore('item1.selected')
        const [, setItem2] = store.useStore('item2.selected')

        store.useSideEffects('convergence-agg', {
          aggregations: [
            ['allSelected', 'item1.selected'],
            ['allSelected', 'item2.selected'],
          ],
        })

        return (
          <div>
            <button
              onClick={() => setItem1(SelectionSymbol.Selected)}
              data-testid="set-item1-selected"
            >
              Item1 Selected
            </button>
            <button
              onClick={() => setItem2(SelectionSymbol.Selected)}
              data-testid="set-item2-selected"
            >
              Item2 Selected
            </button>
            <div data-testid="all-selected">
              {selectionToString(allSelected)}
            </div>
          </div>
        )
      }

      mountStore(<TestComponent />, store, {
        allSelected: SelectionSymbol.Unselected,
        masterValue: '',
        item1: { selected: SelectionSymbol.Selected, value: 'a' },
        item2: { selected: SelectionSymbol.Unselected, value: 'b' },
        item3: { selected: SelectionSymbol.Unselected, value: 'c' },
        item4: { selected: SelectionSymbol.Unselected, value: 'd' },
      })

      await flushEffects()

      // item1=Selected, item2=Unselected -> undefined
      expect(screen.getByTestId('all-selected').textContent).toBe('undefined')

      // Change item2 to match item1
      screen.getByTestId('set-item2-selected').click()
      await flushEffects()

      // Now both are Selected -> target should be Selected
      expect(screen.getByTestId('all-selected').textContent).toBe('Selected')
    })
  })
})
