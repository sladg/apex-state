/**
 * Tests for useFieldStore hook
 *
 * Verifies the convenient object-based API for form field management.
 */

import React from 'react'
import { describe, test, expect } from 'vitest'
import { render, waitFor } from '@testing-library/react'
import { userEvent } from '@testing-library/user-event'
import { createGenericStore } from '../../src/store/createStore'

describe('useFieldStore', () => {
  test('provides object API with value and setValue', async () => {
    type State = { email: string }
    const store = createGenericStore<State>()

    function EmailField() {
      const emailField = store.useFieldStore('email')

      return (
        <div>
          <input
            data-testid="email-input"
            value={emailField.value}
            onChange={(e) => emailField.setValue(e.target.value)}
          />
          <span data-testid="email-display">{emailField.value}</span>
        </div>
      )
    }

    const { getByTestId } = render(
      <store.Provider initialState={{ email: 'test@example.com' }}>
        <EmailField />
      </store.Provider>
    )

    const input = getByTestId('email-input') as HTMLInputElement
    const display = getByTestId('email-display')

    // Initial value
    expect(input.value).toBe('test@example.com')
    expect(display.textContent).toBe('test@example.com')

    // Update value
    await userEvent.clear(input)
    await userEvent.type(input, 'new@example.com')

    await waitFor(() => {
      expect(display.textContent).toBe('new@example.com')
    })
  })

  test('works with nested paths', async () => {
    type State = { user: { profile: { name: string } } }
    const store = createGenericStore<State>()

    function NameField() {
      const nameField = store.useFieldStore('user.profile.name')

      return (
        <input
          data-testid="name-input"
          value={nameField.value}
          onChange={(e) => nameField.setValue(e.target.value)}
        />
      )
    }

    const { getByTestId } = render(
      <store.Provider
        initialState={{ user: { profile: { name: 'John Doe' } } }}
      >
        <NameField />
      </store.Provider>
    )

    const input = getByTestId('name-input') as HTMLInputElement

    expect(input.value).toBe('John Doe')

    await userEvent.clear(input)
    await userEvent.type(input, 'Jane Smith')

    await waitFor(() => {
      expect(input.value).toBe('Jane Smith')
    })
  })

  test('supports different value types', async () => {
    type State = {
      count: number
      isActive: boolean
      tags: string[]
    }
    const store = createGenericStore<State>()

    function Fields() {
      const countField = store.useFieldStore('count')
      const activeField = store.useFieldStore('isActive')

      return (
        <div>
          <button
            data-testid="count-button"
            onClick={() => countField.setValue(10)}
          >
            Set Count
          </button>
          <span data-testid="count">{countField.value}</span>
          <button
            data-testid="active-button"
            onClick={() => activeField.setValue(!activeField.value)}
          >
            Toggle Active
          </button>
          <span data-testid="active">{activeField.value.toString()}</span>
        </div>
      )
    }

    const { getByTestId } = render(
      <store.Provider
        initialState={{ count: 5, isActive: true, tags: [] }}
      >
        <Fields />
      </store.Provider>
    )

    expect(getByTestId('count').textContent).toBe('5')
    expect(getByTestId('active').textContent).toBe('true')

    await userEvent.click(getByTestId('count-button'))
    await userEvent.click(getByTestId('active-button'))

    await waitFor(() => {
      expect(getByTestId('count').textContent).toBe('10')
      expect(getByTestId('active').textContent).toBe('false')
    })
  })

  test('accepts optional meta parameter', async () => {
    type State = { value: string }
    type Meta = { source: string }
    const store = createGenericStore<State, Meta>()

    function Component() {
      const field = store.useFieldStore('value')

      return (
        <div>
          <span data-testid="value">{field.value}</span>
          <button
            data-testid="update"
            onClick={() => field.setValue('updated', { source: 'button' } as Meta)}
          >
            Update
          </button>
        </div>
      )
    }

    const { getByTestId } = render(
      <store.Provider initialState={{ value: 'initial' }}>
        <Component />
      </store.Provider>
    )

    expect(getByTestId('value').textContent).toBe('initial')

    await userEvent.click(getByTestId('update'))

    await waitFor(() => {
      expect(getByTestId('value').textContent).toBe('updated')
    })
  })
})
