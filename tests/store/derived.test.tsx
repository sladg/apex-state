/**
 * Tests for derived values (getter auto-detection)
 *
 * Verifies that getter properties are automatically detected and work reactively
 */

import { describe, it, expect } from 'vitest'
import { render } from '@testing-library/react'
import { useSnapshot } from 'valtio'
import { createGenericStore } from '../../src/store/createStore'
import { extractGetters, detectGetters } from '../../src/store/utils/deriveValues'
import { useContext } from 'react'
import { StoreContext } from '../../src/store/StoreContext'

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
      expect(typeof computed.fullName).toBe('function')
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
      type TestState = {
        firstName: string
        lastName: string
        get fullName(): string
      }

      const store = createGenericStore<TestState>()

      const TestComponent = () => {
        const storeInstance = useContext(StoreContext)
        const snap = useSnapshot(storeInstance!.state)

        // Type assertion needed because TypeScript doesn't know about getters in proxy
        const fullName = (snap as any).fullName

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

      const { getByTestId } = render(
        <store.Provider initialState={initialState}>
          <TestComponent />
        </store.Provider>
      )

      expect(getByTestId('fullName').textContent).toBe('John Doe')
    })

    it('should handle computed values with numeric operations', () => {
      type TestState = {
        a: number
        b: number
        get sum(): number
        get product(): number
      }

      const store = createGenericStore<TestState>()

      const TestComponent = () => {
        const storeInstance = useContext(StoreContext)
        const snap = useSnapshot(storeInstance!.state)

        const sum = (snap as any).sum
        const product = (snap as any).product

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

      const { getByTestId } = render(
        <store.Provider initialState={initialState}>
          <TestComponent />
        </store.Provider>
      )

      expect(getByTestId('sum').textContent).toBe('7')
      expect(getByTestId('product').textContent).toBe('12')
    })

    it('should work without any getters', () => {
      type TestState = {
        value: string
      }

      const store = createGenericStore<TestState>()

      const TestComponent = () => {
        const storeInstance = useContext(StoreContext)
        const snap = useSnapshot(storeInstance!.state)

        return <div data-testid="value">{snap.value}</div>
      }

      const { getByTestId } = render(
        <store.Provider initialState={{ value: 'test' }}>
          <TestComponent />
        </store.Provider>
      )

      expect(getByTestId('value').textContent).toBe('test')
    })
  })
})
