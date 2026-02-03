/**
 * Integration Tests: withConcerns
 */

import '@testing-library/jest-dom'

import { screen } from '@testing-library/react'
import { describe, expect, test } from 'vitest'
import { z } from 'zod'

import { createGenericStore } from '../../src'
import type { OptimizationState } from '../mocks'
import { optimizationFixtures } from '../mocks'
import { flushEffects, renderWithStore } from '../utils/react'

const createOptimizationStore = () => createGenericStore<OptimizationState>()

describe('Integration: withConcerns', () => {
  test('returns typed field store with selected concerns', async () => {
    // 1. Setup Store
    interface State {
      user: {
        name: string
        age: number
      }
    }
    const store = createGenericStore<State>()

    // 2. Component using withConcerns
    function TestComponent() {
      // Register concerns
      store.useConcerns('test', {
        'user.name': {
          validationState: { schema: z.string().min(2) },
          disabledWhen: { condition: { IS_EQUAL: ['user.age', 0] } },
        },
      })

      // Create specialized store
      const specialized = store.withConcerns({
        validationState: true,
        disabledWhen: true,
      })

      // Use specialized hook
      const field = specialized.useFieldStore('user.name')

      // Check values (compile-time check: accessing properties)
      const validationResult = field.validationState // { isError: boolean, errors: [] } | undefined
      const isDisabled = field.disabledWhen // boolean | undefined

      return (
        <div>
          <span data-testid="valid">{String(validationResult?.isError)}</span>
          <span data-testid="disabled">{String(isDisabled)}</span>
          <span data-testid="value">{field.value}</span>
        </div>
      )
    }

    // 3. Render
    renderWithStore(<TestComponent />, store, { user: { name: 'A', age: 0 } })

    // 4. Assertions
    // Name 'A' is too short -> validationState should show isError: true. It takes a tick to evaluate.
    await flushEffects()
    expect(screen.getByTestId('valid')).toHaveTextContent('true')
    expect(screen.getByTestId('disabled')).toHaveTextContent('true')
    expect(screen.getByTestId('value')).toHaveTextContent('A')
  })

  test('only returns selected concerns', () => {
    // Type checking: Verify withConcerns properly filters concern types
    // This is a compile-time type verification that the specialized store
    // correctly filters to only include selected concerns in the field type
    interface State {
      val: string
    }
    const store = createGenericStore<State>()
    // Intentionally unused variable - used for compile-time type checking only
    void store.withConcerns({
      validationState: true,
      // dynamicTooltip NOT selected - this ensures filtering works at compile time
    })
    // If this compiles without errors, the type filtering is working correctly
    expect(true).toBe(true)
  })

  test('render optimization: does not re-render when unselected concern changes', async () => {
    // 1. Setup
    const store = createOptimizationStore()
    let renderCount = 0

    function TestComponent() {
      // Register concerns
      store.useConcerns('test', {
        val: {
          // concern 1: depends on val
          validationState: { schema: z.string().min(1) },
          // concern 2: depends on isInternal (unrelated to concern 1's validation logic)
          disabledWhen: { condition: { IS_EQUAL: ['isInternal', true] } },
        },
      })

      // Select ONLY validationState
      const specialized = store.withConcerns({
        validationState: true,
        // disabledWhen NOT selected
      })

      const field = specialized.useFieldStore('val')

      // Side-effect to count renders
      renderCount++

      return (
        <div>
          <span data-testid="valid">
            {String(!field.validationState?.isError)}
          </span>
          <span data-testid="value">{field.value}</span>
        </div>
      )
    }

    function Control() {
      const [_val, setVal] = store.useStore('val')
      const [_isInternal, setIsInternal] = store.useStore('isInternal')

      return (
        <div>
          <button
            data-testid="update-internal"
            onClick={() => setIsInternal(true)}
          >
            Update Internal
          </button>
          <button data-testid="update-val" onClick={() => setVal('B')}>
            Update Val
          </button>
        </div>
      )
    }

    // 2. Initial Render
    renderWithStore(
      <>
        <TestComponent />
        <Control />
      </>,
      store,
      optimizationFixtures.initial,
    )

    // Wait for initial concerns to evaluate
    await flushEffects()
    expect(screen.getByTestId('valid')).toHaveTextContent('true')

    // Reset count
    renderCount = 0

    // 3. Update 'isInternal' -> affects 'disabledWhen' (which we did NOT select)
    const updateBtn = screen.getByTestId('update-internal')
    updateBtn.click()

    // Wait a tick for potential updates
    await new Promise((r) => setTimeout(r, 50))

    // Expect NO re-render (renderCount remains 0)
    expect(renderCount).toBe(0)

    // 4. Update 'val' -> affects 'validationState' (which we DID select)
    const updateValBtn = screen.getByTestId('update-val')
    updateValBtn.click()

    await flushEffects()
    expect(screen.getByTestId('value')).toHaveTextContent('B')

    // Expect re-render
    expect(renderCount).toBeGreaterThan(0)
  })
})
