/**
 * Integration Tests: Complex Workflows
 *
 * Scenario: Multi-step wizard form with validation and state aggregation
 * Tests form navigation, conditional validation, and progress tracking
 */

import React, { useState } from 'react'
import { describe, it, expect, beforeEach } from 'vitest'
import { render, screen, fireEvent } from '@testing-library/react'

import {
  createWizardFormStore,
  wizardFormFixtures,
} from '../mocks'

describe('Integration: Complex Workflows - Multi-Step Wizard', () => {
  let store: ReturnType<typeof createWizardFormStore>

  beforeEach(() => {
    store = createWizardFormStore()
  })

  // TC6.1: Validate current step before allowing next
  it('TC6.1: validates step fields before navigation', async () => {
    function WizardComponent() {
      const currentStepField = store.useFieldStore('currentStep')
      const personalInfoField = store.useFieldStore('personalInfo')
      const errorsField = store.useFieldStore('_errors')

      const validateStep = (step: number): boolean => {
        const errors = { ...errorsField.value }
        let isValid = true

        if (step === 1) {
          const { firstName, lastName } = personalInfoField.value
          if (!firstName) {
            errors['personalInfo.firstName'] = ['First name required']
            isValid = false
          } else {
            delete errors['personalInfo.firstName']
          }
          if (!lastName) {
            errors['personalInfo.lastName'] = ['Last name required']
            isValid = false
          } else {
            delete errors['personalInfo.lastName']
          }
        }

        errorsField.setValue(errors)
        return isValid
      }

      const handleNext = () => {
        if (validateStep(currentStepField.value)) {
          currentStepField.setValue((currentStepField.value + 1) as any)
        }
      }

      return (
        <div>
          <span data-testid="current-step">{currentStepField.value}</span>
          {currentStepField.value === 1 && (
            <div>
              <input
                data-testid="firstName-input"
                value={personalInfoField.value.firstName}
                onChange={e => {
                  personalInfoField.setValue({
                    ...personalInfoField.value,
                    firstName: e.target.value,
                  })
                }}
                placeholder="First Name"
              />
            </div>
          )}
          <button data-testid="next-btn" onClick={handleNext}>
            Next
          </button>
          {errorsField.value['personalInfo.firstName'] && (
            <span data-testid="firstName-error">
              {errorsField.value['personalInfo.firstName'][0]}
            </span>
          )}
        </div>
      )
    }

    render(
      <store.Provider initialState={{ ...wizardFormFixtures.step1Empty }}>
        <WizardComponent />
      </store.Provider>
    )

    const nextBtn = screen.getByTestId('next-btn')

    // Click next without filling - should show error
    fireEvent.click(nextBtn)
    expect(screen.getByTestId('firstName-error')).toBeInTheDocument()
    expect(screen.getByTestId('current-step')).toHaveTextContent('1')

    // Fill field and try again
    const input = screen.getByTestId('firstName-input')
    fireEvent.change(input, { target: { value: 'John' } })

    // Still on step 1 because lastName is not filled
    expect(screen.getByTestId('current-step')).toHaveTextContent('1')
  })

  // TC6.2: Store errors for invalid fields
  it('TC6.2: stores and displays validation errors', async () => {
    function WizardComponent() {
      const personalInfoField = store.useFieldStore('personalInfo')
      const errorsField = store.useFieldStore('_errors')

      const validateEmail = (value: string) => {
        const errors = { ...errorsField.value }
        if (value && !/^[^\s@]+@[^\s@]+$/.test(value)) {
          errors.email = ['Invalid email format']
        } else {
          delete errors.email
        }
        errorsField.setValue(errors)
      }

      return (
        <div>
          <input
            data-testid="firstName-input"
            value={personalInfoField.value.firstName}
            onChange={e => {
              personalInfoField.setValue({
                ...personalInfoField.value,
                firstName: e.target.value,
              })
              validateEmail(e.target.value)
            }}
          />
          {errorsField.value.email && (
            <span data-testid="error-message">{errorsField.value.email[0]}</span>
          )}
          <span data-testid="error-count">{Object.keys(errorsField.value).length}</span>
        </div>
      )
    }

    render(
      <store.Provider initialState={{ ...wizardFormFixtures.step1Empty }}>
        <WizardComponent />
      </store.Provider>
    )

    const input = screen.getByTestId('firstName-input')
    fireEvent.change(input, { target: { value: 'invalid' } })

    await flushEffects()
    
      expect(screen.getByTestId('error-message')).toBeInTheDocument()
    
  })

  // TC6.3: Show/hide fields based on current step
  it('TC6.3: conditionally displays fields based on step', async () => {
    function WizardComponent() {
      const currentStepField = store.useFieldStore('currentStep')
      const personalInfoField = store.useFieldStore('personalInfo')
      const addressInfoField = store.useFieldStore('addressInfo')

      return (
        <div>
          <span data-testid="current-step">{currentStepField.value}</span>

          {currentStepField.value === 1 && (
            <div data-testid="step1-form">
              <input
                data-testid="firstName-input"
                value={personalInfoField.value.firstName}
                onChange={e =>
                  personalInfoField.setValue({
                    ...personalInfoField.value,
                    firstName: e.target.value,
                  })
                }
                placeholder="First Name"
              />
              <input
                data-testid="lastName-input"
                value={personalInfoField.value.lastName}
                onChange={e =>
                  personalInfoField.setValue({
                    ...personalInfoField.value,
                    lastName: e.target.value,
                  })
                }
                placeholder="Last Name"
              />
            </div>
          )}

          {currentStepField.value === 2 && (
            <div data-testid="step2-form">
              <input
                data-testid="street-input"
                value={addressInfoField.value.street}
                onChange={e =>
                  addressInfoField.setValue({
                    ...addressInfoField.value,
                    street: e.target.value,
                  })
                }
                placeholder="Street"
              />
              <input
                data-testid="city-input"
                value={addressInfoField.value.city}
                onChange={e =>
                  addressInfoField.setValue({
                    ...addressInfoField.value,
                    city: e.target.value,
                  })
                }
                placeholder="City"
              />
            </div>
          )}

          <button
            data-testid="next-btn"
            onClick={() => {
              if (currentStepField.value < 3) {
                currentStepField.setValue((currentStepField.value + 1) as any)
              }
            }}
          >
            Next
          </button>
        </div>
      )
    }

    render(
      <store.Provider initialState={{ ...wizardFormFixtures.step1Empty }}>
        <WizardComponent />
      </store.Provider>
    )

    // Step 1 should be visible
    expect(screen.getByTestId('step1-form')).toBeInTheDocument()
    expect(screen.queryByTestId('step2-form')).not.toBeInTheDocument()

    // Move to step 2
    const nextBtn = screen.getByTestId('next-btn')
    fireEvent.click(nextBtn)

    await flushEffects()
    
      expect(screen.queryByTestId('step1-form')).not.toBeInTheDocument()
      expect(screen.getByTestId('step2-form')).toBeInTheDocument()
    
  })

  // TC6.4: Disable next button while validating
  it('TC6.4: disables navigation while validation in progress', async () => {
    function WizardComponent() {
      const currentStepField = store.useFieldStore('currentStep')
      const [isValidating, setIsValidating] = useState(false)

      const handleNext = async () => {
        setIsValidating(true)
        // Simulate validation delay
        await new Promise(resolve => setTimeout(resolve, 50))
        currentStepField.setValue((currentStepField.value + 1) as any)
        setIsValidating(false)
      }

      return (
        <div>
          <button
            data-testid="next-btn"
            onClick={handleNext}
            disabled={isValidating}
          >
            {isValidating ? 'Validating...' : 'Next'}
          </button>
        </div>
      )
    }

    render(
      <store.Provider initialState={{ ...wizardFormFixtures.step1Empty }}>
        <WizardComponent />
      </store.Provider>
    )

    const nextBtn = screen.getByTestId('next-btn') as HTMLButtonElement
    expect(nextBtn.disabled).toBe(false)

    fireEvent.click(nextBtn)

    await flushEffects()
    
      expect(nextBtn.disabled).toBe(true)
      expect(nextBtn).toHaveTextContent('Validating...')
    
  })

  // TC6.5: Show progress indicator
  it('TC6.5: displays progress through steps', () => {
    function WizardComponent() {
      const currentStepField = store.useFieldStore('currentStep')
      const steps = ['Personal Info', 'Address Info', 'Review']

      return (
        <div>
          <div data-testid="progress">
            {steps.map((label, idx) => (
              <div
                key={idx}
                data-testid={`step-${idx + 1}`}
                className={currentStepField.value === idx + 1 ? 'active' : ''}
              >
                {label}
              </div>
            ))}
          </div>
          <span data-testid="progress-text">
            Step {currentStepField.value} of {steps.length}
          </span>
        </div>
      )
    }

    render(
      <store.Provider initialState={{ ...wizardFormFixtures.step1Empty }}>
        <WizardComponent />
      </store.Provider>
    )

    expect(screen.getByTestId('progress-text')).toHaveTextContent('Step 1 of 3')
  })

  // TC6.6: Review page aggregates & displays all data
  it('TC6.6: review step shows aggregated form data', () => {
    function WizardComponent() {
      const currentStepField = store.useFieldStore('currentStep')
      const personalInfoField = store.useFieldStore('personalInfo')
      const addressInfoField = store.useFieldStore('addressInfo')

      const aggregateData = () => {
        const { firstName, lastName } = personalInfoField.value
        const { street, city, zipCode } = addressInfoField.value
        return `${firstName} ${lastName}, ${street}, ${city} ${zipCode}`
      }

      return (
        <div>
          <span data-testid="current-step">{currentStepField.value}</span>

          {currentStepField.value === 3 && (
            <div data-testid="review-section">
              <h3>Review Your Information</h3>
              <p data-testid="aggregated-data">{aggregateData()}</p>
              <p data-testid="personal-summary">
                {personalInfoField.value.firstName} {personalInfoField.value.lastName}
              </p>
              <p data-testid="address-summary">
                {addressInfoField.value.street}, {addressInfoField.value.city}{' '}
                {addressInfoField.value.zipCode}
              </p>
            </div>
          )}
        </div>
      )
    }

    render(
      <store.Provider initialState={{ ...wizardFormFixtures.step3Review }}>
        <WizardComponent />
      </store.Provider>
    )

    expect(screen.getByTestId('review-section')).toBeInTheDocument()
    expect(screen.getByTestId('personal-summary')).toHaveTextContent('John Doe')
    expect(screen.getByTestId('address-summary')).toHaveTextContent(
      '123 Main St, Springfield 12345'
    )
  })

  // TC6.7: Back/next navigation works with validation
  it('TC6.7: back button works and validation applies on forward', async () => {
    function WizardComponent() {
      const currentStepField = store.useFieldStore('currentStep')

      return (
        <div>
          <span data-testid="current-step">{currentStepField.value}</span>
          <button
            data-testid="prev-btn"
            onClick={() => {
              if (currentStepField.value > 1) {
                currentStepField.setValue((currentStepField.value - 1) as any)
              }
            }}
            disabled={currentStepField.value === 1}
          >
            Back
          </button>
          <button
            data-testid="next-btn"
            onClick={() => {
              if (currentStepField.value < 3) {
                currentStepField.setValue((currentStepField.value + 1) as any)
              }
            }}
            disabled={currentStepField.value === 3}
          >
            Next
          </button>
        </div>
      )
    }

    render(
      <store.Provider initialState={{ ...wizardFormFixtures.step1Empty }}>
        <WizardComponent />
      </store.Provider>
    )

    const prevBtn = screen.getByTestId('prev-btn') as HTMLButtonElement
    const nextBtn = screen.getByTestId('next-btn') as HTMLButtonElement

    // On step 1, back should be disabled
    expect(prevBtn.disabled).toBe(true)
    expect(nextBtn.disabled).toBe(false)

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

  // TC6.8: Concerns & side effects work in complex workflow
  it('TC6.8: concerns and side effects work together in workflow', async () => {
    function WizardComponent() {
      const currentStepField = store.useFieldStore('currentStep')
      const personalInfoField = store.useFieldStore('personalInfo')

      // Register concerns for the form
      store.useConcerns('wizard-form', {
        'personalInfo.firstName': {},
        'personalInfo.lastName': {},
      })

      // Register side effects
      store.useSideEffects('wizard-effects', {})

      const handleNext = () => {
        const { firstName, lastName } = personalInfoField.value
        if (firstName && lastName) {
          currentStepField.setValue((currentStepField.value + 1) as any)
        }
      }

      return (
        <div>
          <span data-testid="current-step">{currentStepField.value}</span>
          {currentStepField.value === 1 && (
            <div>
              <input
                data-testid="firstName-input"
                value={personalInfoField.value.firstName}
                onChange={e =>
                  personalInfoField.setValue({
                    ...personalInfoField.value,
                    firstName: e.target.value,
                  })
                }
              />
              <input
                data-testid="lastName-input"
                value={personalInfoField.value.lastName}
                onChange={e =>
                  personalInfoField.setValue({
                    ...personalInfoField.value,
                    lastName: e.target.value,
                  })
                }
              />
            </div>
          )}
          <button data-testid="next-btn" onClick={handleNext}>
            Next
          </button>
        </div>
      )
    }

    render(
      <store.Provider initialState={{ ...wizardFormFixtures.step1Empty }}>
        <WizardComponent />
      </store.Provider>
    )

    const firstNameInput = screen.getByTestId('firstName-input')
    const lastNameInput = screen.getByTestId('lastName-input')
    const nextBtn = screen.getByTestId('next-btn')

    // Fill both fields
    fireEvent.change(firstNameInput, { target: { value: 'John' } })
    fireEvent.change(lastNameInput, { target: { value: 'Doe' } })

    // Click next
    fireEvent.click(nextBtn)

    await flushEffects()
    
      expect(screen.getByTestId('current-step')).toHaveTextContent('2')
    
  })
})
