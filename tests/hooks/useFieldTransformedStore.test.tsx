/**
 * Tests for useFieldTransformedStore hook
 *
 * Verifies bidirectional transformations for form fields with
 * different display and storage formats.
 */

import React from 'react'
import { describe, test, expect } from 'vitest'
import { render, fireEvent, waitFor } from '@testing-library/react'
import { createGenericStore } from '../../src/store/createStore'

describe('useFieldTransformedStore', () => {
  test('transforms value from storage to display format', () => {
    type State = { birthDate: string } // ISO format

    const store = createGenericStore<State>()

    function DateField() {
      const dateField = store.useFieldTransformedStore('birthDate', {
        toTemporary: (iso: string) => {
          const date = new Date(iso)
          return date.toLocaleDateString('en-US')
        },
        fromTemporary: (display: string) => {
          return new Date(display).toISOString()
        },
      })

      return (
        <div>
          <input
            data-testid="date-input"
            value={dateField.value}
            onChange={(e) => dateField.setValue(e.target.value)}
          />
          <span data-testid="date-display">{dateField.value}</span>
        </div>
      )
    }

    const isoDate = '2000-01-15T00:00:00.000Z'

    const { getByTestId } = render(
      <store.Provider initialState={{ birthDate: isoDate }}>
        <DateField />
      </store.Provider>
    )

    const input = getByTestId('date-input') as HTMLInputElement
    const display = getByTestId('date-display')

    // Should display formatted date, not ISO string
    expect(input.value).toContain('1/15/2000')
    expect(display.textContent).toContain('1/15/2000')
    expect(input.value).not.toContain('T00:00:00')
  })

  test('transforms value from display to storage format', async () => {
    type State = { count: number }

    const store = createGenericStore<State>()

    let storedValue: number | undefined

    function NumberField() {
      const numberField = store.useFieldTransformedStore('count', {
        toTemporary: (num: number) => {
          // Display with thousand separators
          return num.toLocaleString('en-US')
        },
        fromTemporary: (display: string) => {
          // Parse back to number
          return Number(display.replace(/,/g, ''))
        },
      })

      // Track stored value
      const [actualStored] = store.useStore('count')
      storedValue = actualStored

      return (
        <div>
          <input
            data-testid="number-input"
            value={numberField.value}
            onChange={(e) => numberField.setValue(e.target.value)}
          />
          <span data-testid="stored-value">{actualStored}</span>
        </div>
      )
    }

    const { getByTestId } = render(
      <store.Provider initialState={{ count: 1000 }}>
        <NumberField />
      </store.Provider>
    )

    const input = getByTestId('number-input') as HTMLInputElement
    const storedDisplay = getByTestId('stored-value')

    // Initial: displays formatted, stores as number
    expect(input.value).toBe('1,000')
    expect(storedDisplay.textContent).toBe('1000')

    // Update to new formatted value
    fireEvent.change(input, { target: { value: '5,000' } })

    await waitFor(() => {
      expect(storedDisplay.textContent).toBe('5000')
    })

    expect(storedValue).toBe(5000)
  })

  test('handles context parameter for transformations', () => {
    type State = { temperature: number } // Celsius

    const store = createGenericStore<State>()

    function TemperatureField({ unit }: { unit: 'C' | 'F' }) {
      const tempField = store.useFieldTransformedStore('temperature', {
        toTemporary: (celsius: number, context?: 'C' | 'F') => {
          if (context === 'F') {
            return `${(celsius * 9) / 5 + 32}°F`
          }
          return `${celsius}°C`
        },
        fromTemporary: (display: string, context?: 'C' | 'F') => {
          const value = parseFloat(display)
          if (context === 'F') {
            return ((value - 32) * 5) / 9
          }
          return value
        },
        context: unit,
      })

      return (
        <div>
          <input
            data-testid="temp-input"
            value={tempField.value}
            onChange={(e) => tempField.setValue(e.target.value)}
          />
        </div>
      )
    }

    // Test Celsius
    const { getByTestId, rerender } = render(
      <store.Provider initialState={{ temperature: 20 }}>
        <TemperatureField unit="C" />
      </store.Provider>
    )

    let input = getByTestId('temp-input') as HTMLInputElement
    expect(input.value).toBe('20°C')

    // Test Fahrenheit
    rerender(
      <store.Provider initialState={{ temperature: 20 }}>
        <TemperatureField unit="F" />
      </store.Provider>
    )

    input = getByTestId('temp-input') as HTMLInputElement
    expect(input.value).toBe('68°F')
  })

  test('updates when stored value changes externally', async () => {
    type State = { value: number }

    const store = createGenericStore<State>()

    function TransformedField() {
      const field = store.useFieldTransformedStore('value', {
        toTemporary: (num: number) => `Value: ${num}`,
        fromTemporary: (str: string) => {
          const match = str.match(/\d+/)
          return match ? Number(match[0]) : 0
        },
      })

      return (
        <div>
          <span data-testid="display">{field.value}</span>
        </div>
      )
    }

    function ExternalUpdater() {
      const [value, setValue] = store.useStore('value')

      return (
        <button
          data-testid="external-update"
          onClick={() => setValue(999)}
        >
          Update Externally
        </button>
      )
    }

    const { getByTestId } = render(
      <store.Provider initialState={{ value: 42 }}>
        <TransformedField />
        <ExternalUpdater />
      </store.Provider>
    )

    const display = getByTestId('display')

    expect(display.textContent).toBe('Value: 42')

    fireEvent.click(getByTestId('external-update'))

    await waitFor(() => {
      expect(display.textContent).toBe('Value: 999')
    })
  })

  test('provides responsive UI updates with local state', () => {
    type State = { text: string }

    const store = createGenericStore<State>()

    function UppercaseField() {
      const field = store.useFieldTransformedStore('text', {
        toTemporary: (str: string) => str.toUpperCase(),
        fromTemporary: (str: string) => str.toLowerCase(),
      })

      return (
        <input
          data-testid="input"
          value={field.value}
          onChange={(e) => field.setValue(e.target.value)}
        />
      )
    }

    const { getByTestId } = render(
      <store.Provider initialState={{ text: 'hello' }}>
        <UppercaseField />
      </store.Provider>
    )

    const input = getByTestId('input') as HTMLInputElement

    // Displays uppercase
    expect(input.value).toBe('HELLO')

    // User types uppercase, stored as lowercase
    fireEvent.change(input, { target: { value: 'WORLD' } })

    expect(input.value).toBe('WORLD')
  })

  test('works with complex transformations', () => {
    type State = { items: string[] }

    const store = createGenericStore<State>()

    function TagsField() {
      const tagsField = store.useFieldTransformedStore('items', {
        toTemporary: (items: string[]) => items.join(', '),
        fromTemporary: (str: string) => {
          return str.split(',').map((s) => s.trim()).filter(Boolean)
        },
      })

      return (
        <div>
          <input
            data-testid="tags-input"
            value={tagsField.value}
            onChange={(e) => tagsField.setValue(e.target.value)}
          />
        </div>
      )
    }

    const { getByTestId } = render(
      <store.Provider initialState={{ items: ['react', 'valtio', 'typescript'] }}>
        <TagsField />
      </store.Provider>
    )

    const input = getByTestId('tags-input') as HTMLInputElement

    // Display as comma-separated
    expect(input.value).toBe('react, valtio, typescript')

    // Update with new comma-separated list
    fireEvent.change(input, { target: { value: 'vue, redux, javascript' } })

    expect(input.value).toBe('vue, redux, javascript')
  })
})
