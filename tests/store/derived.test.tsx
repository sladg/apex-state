/**
 * Tests for derived values (getter auto-detection)
 *
 * Verifies that getter properties are automatically detected and work reactively
 */

import { useContext } from 'react'

import { screen } from '@testing-library/react'
import { useSnapshot } from 'valtio'
import { describe, expect, it } from 'vitest'

import { StoreContext } from '../../src/core/context'
import { createGenericStore } from '../../src/store/createStore'
import { detectGetters, extractGetters } from '../../src/utils/deriveValues'
import {
  fireEvent,
  flushEffects,
  renderWithStore,
} from '../../tests/utils/react'
import { typeHelpers } from '../mocks'

/**
 * Type helpers for derived state with getters.
 * TypeScript doesn't know about runtime getters added to proxy state,
 * so we define explicit types for test assertions.
 */
interface WithFullName {
  readonly fullName: string
}
interface WithDoubledTripled {
  readonly doubled: number
  readonly tripled: number
  readonly multiplied: number
}
interface WithTotals {
  readonly total: number
  readonly totalWithTax: number
}
interface WithSumProduct {
  readonly sum: number
  readonly product: number
}
interface WithComputed {
  readonly computed: string
}

describe('Derived Values', () => {
  describe('extractGetters', () => {
    it('should extract getter properties from object', () => {
      const obj = {
        firstName: 'John',
        lastName: 'Doe',
        get fullName() {
          return `${this.firstName} ${this.lastName}`
        },
      }

      const { base, computed } = extractGetters(obj)

      expect(base).toEqual({ firstName: 'John', lastName: 'Doe' })
      expect(Object.keys(computed)).toContain('fullName')
      expect(typeof computed['fullName']).toBe('function')
    })

    it('should handle objects without getters', () => {
      const obj = {
        a: 1,
        b: 2,
      }

      const { base, computed } = extractGetters(obj)

      expect(base).toEqual({ a: 1, b: 2 })
      expect(Object.keys(computed)).toHaveLength(0)
    })

    it('should handle multiple getters', () => {
      const obj = {
        count: 5,
        get doubled() {
          return this.count * 2
        },
        get tripled() {
          return this.count * 3
        },
      }

      const { base, computed } = extractGetters(obj)

      expect(base).toEqual({ count: 5 })
      expect(Object.keys(computed)).toHaveLength(2)
      expect(Object.keys(computed)).toContain('doubled')
      expect(Object.keys(computed)).toContain('tripled')
    })
  })

  describe('detectGetters', () => {
    it('should detect nested getters', () => {
      const obj = {
        user: {
          firstName: 'Jane',
          get greeting() {
            return `Hello, ${this.firstName}`
          },
        },
      }

      const getters = detectGetters(obj)

      expect(Object.keys(getters)).toContain('user.greeting')
    })

    it('should return empty object for no getters', () => {
      const obj = { a: 1, b: 2 }
      const getters = detectGetters(obj)

      expect(Object.keys(getters)).toHaveLength(0)
    })
  })

  describe('Store with derived values', () => {
    it('should initialize with getter properties', () => {
      interface TestState {
        firstName: string
        lastName: string
        get fullName(): string
      }

      const store = createGenericStore<TestState>()

      const TestComponent = () => {
        const storeInstance = useContext(StoreContext)
        const snap = useSnapshot(storeInstance!.state)

        // Type assertion needed because TypeScript doesn't know about getters in proxy
        const fullName = (snap as WithFullName).fullName

        return (
          <div>
            <span data-testid="fullName">{fullName}</span>
          </div>
        )
      }

      const initialState: TestState = {
        firstName: 'John',
        lastName: 'Doe',
        get fullName() {
          return `${this.firstName} ${this.lastName}`
        },
      }

      renderWithStore(<TestComponent />, store, initialState)

      expect(screen.getByTestId('fullName').textContent).toBe('John Doe')
    })

    it('should reactively update getter when dependency changes', async () => {
      interface TestState {
        firstName: string
        lastName: string
        get fullName(): string
      }

      const store = createGenericStore<TestState>()

      const TestComponent = () => {
        const storeInstance = useContext(StoreContext)
        const snap = useSnapshot(storeInstance!.state)
        const [_firstName, setFirstName] = store.useStore('firstName')

        const fullName = (snap as WithFullName).fullName

        return (
          <div>
            <span data-testid="fullName">{fullName}</span>
            <button
              data-testid="change-name"
              onClick={() => setFirstName('Jane')}
            >
              Change
            </button>
          </div>
        )
      }

      const initialState: TestState = {
        firstName: 'John',
        lastName: 'Doe',
        get fullName() {
          return `${this.firstName} ${this.lastName}`
        },
      }

      renderWithStore(<TestComponent />, store, initialState)

      expect(screen.getByTestId('fullName').textContent).toBe('John Doe')

      // Click button to change firstName
      fireEvent.click(screen.getByTestId('change-name'))

      // Wait for effects to flush
      await flushEffects()

      // Getter should recalculate
      expect(screen.getByTestId('fullName').textContent).toBe('Jane Doe')
    })

    it('should only re-render when accessed getter dependencies change', () => {
      interface TestState {
        firstName: string
        lastName: string
        age: number
        get fullName(): string
      }

      const store = createGenericStore<TestState>()

      let renderCount = 0

      const TestComponent = () => {
        renderCount++
        const storeInstance = useContext(StoreContext)
        const snap = useSnapshot(storeInstance!.state)
        const [age, setAge] = store.useStore('age')

        // Only access fullName getter (which depends on firstName and lastName)
        const fullName = (snap as WithFullName).fullName

        return (
          <div>
            <span data-testid="fullName">{fullName}</span>
            <span data-testid="render-count">{renderCount}</span>
            <button data-testid="change-age" onClick={() => setAge(age + 1)}>
              Change Age
            </button>
          </div>
        )
      }

      const initialState: TestState = {
        firstName: 'John',
        lastName: 'Doe',
        age: 30,
        get fullName() {
          return `${this.firstName} ${this.lastName}`
        },
      }

      renderWithStore(<TestComponent />, store, initialState)

      const initialRenderCount = renderCount

      // Change age - should NOT trigger re-render because
      // component doesn't access age in snapshot
      fireEvent.click(screen.getByTestId('change-age'))

      // Since age is not accessed via snap, changing it shouldn't re-render
      // (Note: This test verifies Valtio's selective re-rendering)
      expect(renderCount).toBe(initialRenderCount)
    })

    it('should support multiple getters with different dependencies', async () => {
      interface TestState {
        count: number
        multiplier: number
        get doubled(): number
        get tripled(): number
        get multiplied(): number
      }

      const store = createGenericStore<TestState>()

      const TestComponent = () => {
        const storeInstance = useContext(StoreContext)
        const snap = useSnapshot(storeInstance!.state)
        const [count, setCount] = store.useStore('count')
        const [multiplier, setMultiplier] = store.useStore('multiplier')

        return (
          <div>
            <span data-testid="doubled">
              {(snap as WithDoubledTripled).doubled}
            </span>
            <span data-testid="tripled">
              {(snap as WithDoubledTripled).tripled}
            </span>
            <span data-testid="multiplied">
              {(snap as WithDoubledTripled).multiplied}
            </span>
            <button data-testid="inc-count" onClick={() => setCount(count + 1)}>
              Inc Count
            </button>
            <button
              data-testid="inc-mult"
              onClick={() => setMultiplier(multiplier + 1)}
            >
              Inc Multiplier
            </button>
          </div>
        )
      }

      const initialState: TestState = {
        count: 5,
        multiplier: 2,
        get doubled() {
          return this.count * 2
        },
        get tripled() {
          return this.count * 3
        },
        get multiplied() {
          return this.count * this.multiplier
        },
      }

      renderWithStore(<TestComponent />, store, initialState)

      // Initial values
      expect(screen.getByTestId('doubled').textContent).toBe('10')
      expect(screen.getByTestId('tripled').textContent).toBe('15')
      expect(screen.getByTestId('multiplied').textContent).toBe('10')

      // Increment count - all getters should update
      fireEvent.click(screen.getByTestId('inc-count'))
      await flushEffects()

      expect(screen.getByTestId('doubled').textContent).toBe('12')
      expect(screen.getByTestId('tripled').textContent).toBe('18')
      expect(screen.getByTestId('multiplied').textContent).toBe('12')

      // Increment multiplier - only multiplied getter should change
      fireEvent.click(screen.getByTestId('inc-mult'))
      await flushEffects()

      expect(screen.getByTestId('doubled').textContent).toBe('12')
      expect(screen.getByTestId('tripled').textContent).toBe('18')
      expect(screen.getByTestId('multiplied').textContent).toBe('18')
    })

    it('should handle nested object getters', async () => {
      interface TestState {
        user: {
          firstName: string
          lastName: string
          get fullName(): string
        }
      }

      const store = createGenericStore<TestState>()

      const TestComponent = () => {
        const storeInstance = useContext(StoreContext)
        const snap = useSnapshot(storeInstance!.state)
        const [_firstName, setFirstName] = store.useStore('user.firstName')

        return (
          <div>
            <span data-testid="fullName">
              {(snap.user as WithFullName).fullName}
            </span>
            <button
              data-testid="change-name"
              onClick={() => setFirstName('Jane')}
            >
              Change
            </button>
          </div>
        )
      }

      const initialState: TestState = {
        user: {
          firstName: 'John',
          lastName: 'Doe',
          get fullName() {
            return `${this.firstName} ${this.lastName}`
          },
        },
      }

      renderWithStore(<TestComponent />, store, initialState)

      expect(screen.getByTestId('fullName').textContent).toBe('John Doe')

      fireEvent.click(screen.getByTestId('change-name'))
      await flushEffects()

      expect(screen.getByTestId('fullName').textContent).toBe('Jane Doe')
    })

    it('should handle getters with complex logic', async () => {
      interface TestState {
        items: { name: string; price: number }[]
        taxRate: number
        get total(): number
        get totalWithTax(): number
      }

      const store = createGenericStore<TestState>()

      const TestComponent = () => {
        const storeInstance = useContext(StoreContext)
        const snap = useSnapshot(storeInstance!.state)
        const { setChanges } = store.useJitStore()

        return (
          <div>
            <span data-testid="total">{(snap as WithTotals).total}</span>
            <span data-testid="totalWithTax">
              {(snap as WithTotals).totalWithTax}
            </span>
            <button
              data-testid="add-item"
              onClick={() => {
                setChanges([
                  typeHelpers.change<TestState>(
                    'items',
                    [...snap.items, { name: 'New Item', price: 10 }],
                    {},
                  ),
                ])
              }}
            >
              Add Item
            </button>
          </div>
        )
      }

      const initialState: TestState = {
        items: [
          { name: 'Item 1', price: 100 },
          { name: 'Item 2', price: 200 },
        ],
        taxRate: 0.1,
        get total() {
          return this.items.reduce((sum, item) => sum + item.price, 0)
        },
        get totalWithTax() {
          return this.total * (1 + this.taxRate)
        },
      }

      renderWithStore(<TestComponent />, store, initialState)

      // Initial: 100 + 200 = 300, with 10% tax = 330
      expect(screen.getByTestId('total').textContent).toBe('300')
      expect(screen.getByTestId('totalWithTax').textContent).toBe('330')

      // Add item
      fireEvent.click(screen.getByTestId('add-item'))
      await flushEffects()

      // New total: 300 + 10 = 310, with 10% tax = 341
      expect(screen.getByTestId('total').textContent).toBe('310')
      expect(screen.getByTestId('totalWithTax').textContent).toBe('341')
    })

    it('should handle computed values with numeric operations', () => {
      interface TestState {
        a: number
        b: number
        get sum(): number
        get product(): number
      }

      const store = createGenericStore<TestState>()

      const TestComponent = () => {
        const storeInstance = useContext(StoreContext)
        const snap = useSnapshot(storeInstance!.state)

        const sum = (snap as WithSumProduct).sum
        const product = (snap as WithSumProduct).product

        return (
          <div>
            <span data-testid="sum">{sum}</span>
            <span data-testid="product">{product}</span>
          </div>
        )
      }

      const initialState: TestState = {
        a: 3,
        b: 4,
        get sum() {
          return this.a + this.b
        },
        get product() {
          return this.a * this.b
        },
      }

      renderWithStore(<TestComponent />, store, initialState)

      expect(screen.getByTestId('sum').textContent).toBe('7')
      expect(screen.getByTestId('product').textContent).toBe('12')
    })

    it('should work without any getters', () => {
      interface TestState {
        value: string
      }

      const store = createGenericStore<TestState>()

      const TestComponent = () => {
        const storeInstance = useContext(StoreContext)
        const snap = useSnapshot(storeInstance!.state)

        return <div data-testid="value">{snap.value}</div>
      }

      renderWithStore(<TestComponent />, store, {
        value: 'test',
      })

      expect(screen.getByTestId('value').textContent).toBe('test')
    })

    it('should verify getter recalculation behavior (cache vs recompute)', async () => {
      interface TestState {
        value: number
        get computed(): string
      }

      const store = createGenericStore<TestState>()

      // Track how many times the getter is called
      let getterCallCount = 0

      const TestComponent = () => {
        const storeInstance = useContext(StoreContext)
        const snap = useSnapshot(storeInstance!.state)
        const [value, setValue] = store.useStore('value')

        // Access the getter multiple times
        const computed1 = (snap as WithComputed).computed
        const computed2 = (snap as WithComputed).computed
        const computed3 = (snap as WithComputed).computed

        return (
          <div>
            <span data-testid="computed1">{computed1}</span>
            <span data-testid="computed2">{computed2}</span>
            <span data-testid="computed3">{computed3}</span>
            <span data-testid="call-count">{getterCallCount}</span>
            <button data-testid="increment" onClick={() => setValue(value + 1)}>
              Increment
            </button>
          </div>
        )
      }

      const initialState: TestState = {
        value: 1,
        get computed() {
          getterCallCount++
          return `computed-${this.value}-${getterCallCount}`
        },
      }

      renderWithStore(<TestComponent />, store, initialState)

      // After first render, check how many times getter was called
      const initialCallCount = getterCallCount

      // All three accesses should return the same value
      const c1 = screen.getByTestId('computed1').textContent
      const c2 = screen.getByTestId('computed2').textContent
      const c3 = screen.getByTestId('computed3').textContent

      // If cached: all three should be identical (same counter value)
      // If recalculated: each would have different counter
      expect(c1).toBe(c2)
      expect(c2).toBe(c3)

      // Now change the dependency
      fireEvent.click(screen.getByTestId('increment'))
      await flushEffects()

      // After dependency change, getter should recalculate
      const newCallCount = getterCallCount
      expect(newCallCount).toBeGreaterThan(initialCallCount)

      // The computed value should reflect the new value
      const newComputed = screen.getByTestId('computed1').textContent
      expect(newComputed).toContain('computed-2')
    })
  })
})
