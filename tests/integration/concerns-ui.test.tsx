/**
 * Integration Tests: Dynamic UI State from Concerns
 *
 * Scenario: Product form with conditional UI
 * Tests conditional visibility, labels, disabled state, and dynamic text
 */

import { screen } from '@testing-library/react'
import { beforeEach, describe, expect, it } from 'vitest'

import type { TestState } from '../mocks'
import { defaults, testStateFixtures } from '../mocks'
import { ProductComponent } from '../utils/components'
import { createStore, fireEvent, flush, renderWithStore } from '../utils/react'

describe('Integration: Dynamic UI State from Concerns', () => {
  let store: ReturnType<typeof createStore<TestState>>

  beforeEach(() => {
    store = createStore<TestState>(testStateFixtures.productEmpty)
  })

  it('TC4.1: shows weight field only for physical products', async () => {
    renderWithStore(
      <ProductComponent store={store} />,
      store,
      { ...testStateFixtures.productEmpty },
      {
        concerns: {
          weight: {
            visibleWhen: {
              condition: { IS_EQUAL: ['type', 'physical'] },
            },
          },
        },
      },
    )

    // Physical product - weight field should be visible
    expect(screen.getByTestId('weight-input')).toBeInTheDocument()

    // Switch to digital
    const typeSelect = screen.getByTestId('type-select')
    fireEvent.change(typeSelect, { target: { value: 'digital' } })

    await flush()

    // Weight field should now be hidden
    // We expect it to be gone from DOM because showWeight will be false
    expect(screen.queryByTestId('weight-input')).not.toBeInTheDocument()
  })

  it('TC4.2: shows download URL field only for digital products', async () => {
    renderWithStore(
      <ProductComponent store={store} />,
      store,
      { ...testStateFixtures.productEmpty },
      {
        concerns: {
          downloadUrl: {
            visibleWhen: {
              condition: { IS_EQUAL: ['type', 'digital'] },
            },
          },
        },
      },
    )

    // Switch to digital
    const typeSelect = screen.getByTestId('type-select')
    fireEvent.change(typeSelect, { target: { value: 'digital' } })

    await flush()

    // Digital product - download URL should be visible
    expect(screen.getByTestId('download-url-input')).toBeInTheDocument()
  })

  it('TC4.3: disables price field when product is published', async () => {
    renderWithStore(
      <ProductComponent store={store} />,
      store,
      {
        ...defaults,
        type: 'physical' as const,
        price: 10,
        requiresShipping: true,
        taxable: true,
      },
      {
        concerns: {
          price: {
            disabledWhen: {
              condition: { IS_EQUAL: ['isPublished', true] },
            },
          },
        },
      },
    )

    const priceInput = screen.getByTestId('price-input') as HTMLInputElement
    expect(priceInput.disabled).toBe(false)

    // Publish the product
    const publishedCheckbox = screen.getByTestId('published-checkbox')
    fireEvent.click(publishedCheckbox)

    // Price input should now be disabled
    await flush()
    expect(priceInput.disabled).toBe(true)
  })

  it('TC4.4: updates field label based on product type', async () => {
    renderWithStore(<ProductComponent store={store} />, store)

    expect(screen.getByTestId('name-label')).toHaveTextContent('Product Name')

    const typeSelect = screen.getByTestId('type-select')
    fireEvent.change(typeSelect, { target: { value: 'digital' } })

    await flush()
    expect(screen.getByTestId('name-label')).toHaveTextContent('Software Name')
  })

  it('TC4.5: updates field placeholder dynamically', async () => {
    renderWithStore(<ProductComponent store={store} />, store)

    const typeSelect = screen.getByTestId('type-select')
    fireEvent.change(typeSelect, { target: { value: 'digital' } })

    await flush()

    const input = screen.getByTestId('download-url-input') as HTMLInputElement
    expect(input.placeholder).toContain('https://download.example.com')
  })

  it('TC4.6: displays dynamic tooltip with field restrictions', async () => {
    renderWithStore(<ProductComponent store={store} />, store)

    const typeSelect = screen.getByTestId('type-select')
    fireEvent.change(typeSelect, { target: { value: 'digital' } })

    await flush()

    const tooltip = screen.getByTestId('price-tooltip')
    expect(tooltip).toHaveAttribute(
      'title',
      'Software products have flat pricing',
    )
  })

  it('TC4.7: makes fields read-only when product is published', async () => {
    renderWithStore(
      <ProductComponent store={store} />,
      store,
      {
        ...defaults,
        type: 'physical' as const,
        name: 'Test Product',
        requiresShipping: true,
        taxable: true,
      },
      {
        concerns: {
          name: {
            readonlyWhen: {
              condition: { IS_EQUAL: ['isPublished', true] },
            },
          },
        },
      },
    )

    const nameInput = screen.getByTestId('name-input') as HTMLInputElement
    expect(nameInput.readOnly).toBe(false)

    const publishedCheckbox = screen.getByTestId('published-checkbox')
    fireEvent.click(publishedCheckbox)

    await flush()
    expect(nameInput.readOnly).toBe(true)
  })

  it('TC4.8: re-evaluates concerns when state dependencies change', async () => {
    renderWithStore(
      <ProductComponent store={store} />,
      store,
      {
        ...defaults,
        type: 'physical' as const,
        price: 10,
        requiresShipping: true,
        taxable: true,
      },
      {
        concerns: {
          price: {
            disabledWhen: {
              condition: { IS_EQUAL: ['type', 'digital'] },
            },
          },
        },
      },
    )

    const priceInput = screen.getByTestId('price-input') as HTMLInputElement
    expect(priceInput.disabled).toBe(false)
    expect(screen.queryByTestId('price-disabled-msg')).not.toBeInTheDocument()

    // Switch to digital - price should be disabled
    const typeSelect = screen.getByTestId('type-select')
    fireEvent.change(typeSelect, { target: { value: 'digital' } })

    await flush()
    expect(priceInput.disabled).toBe(true)
    expect(screen.getByTestId('price-disabled-msg')).toBeInTheDocument()
  })
})
