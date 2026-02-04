/**
 * Integration Tests: Complex Workflows
 *
 * Scenario: Multi-step wizard form with validation and state aggregation
 * Tests form navigation, conditional validation, and progress tracking
 */

import { screen } from '@testing-library/react'
import { beforeEach, describe, expect, it } from 'vitest'

import { createGenericStore } from '../../src'
import type { WizardForm } from '../mocks'
import { wizardFormFixtures } from '../mocks'
import { WizardComponent } from '../utils/components'
import { fireEvent, flushEffects, renderWithStore } from '../utils/react'

const createWizardFormStore = () => createGenericStore<WizardForm>()

describe('Integration: Complex Workflows - Multi-Step Wizard', () => {
  let store: ReturnType<typeof createWizardFormStore>

  beforeEach(() => {
    store = createWizardFormStore()
  })

  it('TC6.1: validates step fields before navigation', async () => {
    renderWithStore(
      <WizardComponent store={store} />,
      store,
      structuredClone(wizardFormFixtures.step1Empty),
    )

    const nextBtn = screen.getByTestId('next-btn')

    // Click next without filling - should show error
    fireEvent.click(nextBtn)
    await flushEffects()

    expect(screen.getByTestId('firstName-error')).toBeInTheDocument()
    expect(screen.getByTestId('current-step')).toHaveTextContent('1')

    // Fill field and try again
    const input = screen.getByTestId('firstName-input')
    fireEvent.change(input, { target: { value: 'John' } })
    await flushEffects()

    // Still on step 1 because lastName is not filled
    expect(screen.getByTestId('current-step')).toHaveTextContent('1')
  })

  it('TC6.2: stores and displays validation errors', async () => {
    // Custom validation for this specific test
    const CustomWizard = ({ store }: { store: any }) => {
      const personalInfoField = store.useFieldStore('personalInfo')
      const errorsField = store.useFieldStore('_errors')

      const validateEmail = (value: string) => {
        const errors = { ...errorsField.value }
        if (value && !/^[^\s@]+@[^\s@]+$/.test(value)) {
          errors['email'] = ['Invalid email format']
        } else {
          delete errors['email']
        }
        errorsField.setValue(errors)
      }

      return (
        <div>
          <input
            data-testid="firstName-input"
            value={personalInfoField.value.firstName}
            onChange={(e) => {
              personalInfoField.setValue({
                ...personalInfoField.value,
                firstName: e.target.value,
              })
              validateEmail(e.target.value)
            }}
          />
          {errorsField.value['email'] && (
            <span data-testid="error-message">
              {errorsField.value['email'][0]}
            </span>
          )}
          <span data-testid="error-count">
            {Object.keys(errorsField.value).length}
          </span>
        </div>
      )
    }

    renderWithStore(
      <CustomWizard store={store} />,
      store,
      structuredClone(wizardFormFixtures.step1Empty),
    )

    const input = screen.getByTestId('firstName-input')
    fireEvent.change(input, { target: { value: 'invalid' } })

    await flushEffects()

    expect(screen.getByTestId('error-message')).toBeInTheDocument()
  })

  it('TC6.3: conditionally displays fields based on step', async () => {
    renderWithStore(
      <WizardComponent store={store} />,
      store,
      structuredClone(wizardFormFixtures.step1Empty),
    )

    // Step 1 should be visible
    expect(screen.getByTestId('step1-form')).toBeInTheDocument()
    expect(screen.queryByTestId('step2-form')).not.toBeInTheDocument()

    // Move to step 2 - need to fill fields first to pass validation in the shared component
    const firstNameInput = screen.getByTestId('firstName-input')
    const lastNameInput = screen.getByTestId('lastName-input')
    fireEvent.change(firstNameInput, { target: { value: 'John' } })
    fireEvent.change(lastNameInput, { target: { value: 'Doe' } })
    await flushEffects()

    const nextBtn = screen.getByTestId('next-btn')
    fireEvent.click(nextBtn)

    await flushEffects()

    expect(screen.queryByTestId('step1-form')).not.toBeInTheDocument()
    expect(screen.getByTestId('step2-form')).toBeInTheDocument()
  })

  it('TC6.4: disables navigation while validation in progress', async () => {
    renderWithStore(
      <WizardComponent store={store} delay={50} />,
      store,
      structuredClone(wizardFormFixtures.step1Empty),
    )

    // Fill fields to pass validation
    const firstNameInput = screen.getByTestId('firstName-input')
    const lastNameInput = screen.getByTestId('lastName-input')
    fireEvent.change(firstNameInput, { target: { value: 'John' } })
    fireEvent.change(lastNameInput, { target: { value: 'Doe' } })
    await flushEffects()

    const nextBtn = screen.getByTestId('next-btn') as HTMLButtonElement
    expect(nextBtn.disabled).toBe(false)

    fireEvent.click(nextBtn)

    await flushEffects()

    expect(nextBtn.disabled).toBe(true)
    expect(nextBtn).toHaveTextContent('Validating...')
  })

  it('TC6.5: displays progress through steps', () => {
    renderWithStore(
      <WizardComponent store={store} />,
      store,
      structuredClone(wizardFormFixtures.step1Empty),
    )

    expect(screen.getByTestId('progress-text')).toHaveTextContent('Step 1 of 3')
  })

  it('TC6.6: review step shows aggregated form data', () => {
    renderWithStore(
      <WizardComponent store={store} />,
      store,
      structuredClone(wizardFormFixtures.step3Review),
    )

    expect(screen.getByTestId('review-section')).toBeInTheDocument()
    expect(screen.getByTestId('personal-summary')).toHaveTextContent('John Doe')
    expect(screen.getByTestId('address-summary')).toHaveTextContent(
      '123 Main St, Springfield 12345',
    )
  })

  it('TC6.7: back button works and validation applies on forward', async () => {
    renderWithStore(
      <WizardComponent store={store} />,
      store,
      structuredClone(wizardFormFixtures.step1Empty),
    )

    const prevBtn = screen.getByTestId('prev-btn') as HTMLButtonElement
    const nextBtn = screen.getByTestId('next-btn') as HTMLButtonElement

    // On step 1, back should be disabled
    expect(prevBtn.disabled).toBe(true)
    expect(nextBtn.disabled).toBe(false)

    // Fill fields to pass validation
    const firstNameInput = screen.getByTestId('firstName-input')
    const lastNameInput = screen.getByTestId('lastName-input')
    fireEvent.change(firstNameInput, { target: { value: 'John' } })
    fireEvent.change(lastNameInput, { target: { value: 'Doe' } })
    await flushEffects()

    // Move to step 2
    fireEvent.click(nextBtn)

    await flushEffects()

    expect(screen.getByTestId('current-step')).toHaveTextContent('2')
    expect(prevBtn.disabled).toBe(false)

    // Go back
    fireEvent.click(prevBtn)

    await flushEffects()

    expect(screen.getByTestId('current-step')).toHaveTextContent('1')
  })

  it('TC6.8: concerns and side effects work together in workflow', async () => {
    // Shared WizardComponent already handles basic validation and has next-btn
    renderWithStore(
      <WizardComponent store={store} />,
      store,
      structuredClone(wizardFormFixtures.step1Empty),
    )

    const firstNameInput = screen.getByTestId('firstName-input')
    const lastNameInput = screen.getByTestId('lastName-input')
    const nextBtn = screen.getByTestId('next-btn')

    // Fill both fields
    fireEvent.change(firstNameInput, { target: { value: 'John' } })
    await flushEffects()

    fireEvent.change(lastNameInput, { target: { value: 'Doe' } })
    await flushEffects()

    // Click next
    fireEvent.click(nextBtn)

    await flushEffects()

    expect(screen.getByTestId('current-step')).toHaveTextContent('2')
  })
})
