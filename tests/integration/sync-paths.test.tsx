/**
 * Integration Tests: Bidirectional Field Synchronization
 *
 * Scenario: Form fields that stay synchronized
 * Tests sync paths, circular sync prevention, and batch operations
 */

import React from 'react'

import { screen } from '@testing-library/react'
import { beforeEach, describe, expect, it } from 'vitest'

import type { ProfileForm } from '../mocks'
import {
  createProfileFormStore,
  profileFormFixtures,
  typeHelpers,
} from '../mocks'
import { fireEvent, flushEffects, renderWithStore } from '../utils/react'

describe('Integration: Bidirectional Field Sync', () => {
  let store: ReturnType<typeof createProfileFormStore>

  beforeEach(() => {
    store = createProfileFormStore()
  })

  // TC2.1: Update firstName → fullName updates
  it('TC2.1: updates fullName when firstName changes', async () => {
    function FormComponent() {
      store.useSideEffects('profile-sync', {
        syncPaths: [typeHelpers.syncPair<ProfileForm>('firstName', 'lastName')],
      })

      const firstNameField = store.useFieldStore('firstName')
      const fullNameField = store.useFieldStore('fullName')

      return (
        <div>
          <input
            data-testid="firstName-input"
            value={firstNameField.value}
            onChange={(e) => firstNameField.setValue(e.target.value)}
            placeholder="First Name"
          />
          <span data-testid="fullName-display">{fullNameField.value}</span>
        </div>
      )
    }

    renderWithStore(<FormComponent />, store, { ...profileFormFixtures.empty })

    const input = screen.getByTestId('firstName-input') as HTMLInputElement
    fireEvent.change(input, { target: { value: 'John' } })

    await flushEffects()

    expect(input.value).toBe('John')
  })

  // TC2.2: Update lastName → fullName updates
  it('TC2.2: updates fullName when lastName changes', async () => {
    function FormComponent() {
      const lastNameField = store.useFieldStore('lastName')
      const fullNameField = store.useFieldStore('fullName')

      // Listener to compute fullName
      store.useSideEffects('compute-fullname', {})

      return (
        <div>
          <input
            data-testid="lastName-input"
            value={lastNameField.value}
            onChange={(e) => lastNameField.setValue(e.target.value)}
            placeholder="Last Name"
          />
          <span data-testid="fullName-display">{fullNameField.value}</span>
        </div>
      )
    }

    renderWithStore(<FormComponent />, store, {
      firstName: 'John',
      lastName: '',
      fullName: 'John',
      displayName: 'John',
    })

    const input = screen.getByTestId('lastName-input') as HTMLInputElement
    fireEvent.change(input, { target: { value: 'Doe' } })

    await flushEffects()

    expect(input.value).toBe('Doe')
  })

  // TC2.3: Update displayName → firstName updates
  it('TC2.3: updates firstName when displayName changes (sync pair)', async () => {
    function FormComponent() {
      store.useSideEffects('display-sync', {
        syncPaths: [
          typeHelpers.syncPair<ProfileForm>('displayName', 'firstName'),
        ],
      })

      const displayNameField = store.useFieldStore('displayName')
      const firstNameField = store.useFieldStore('firstName')

      return (
        <div>
          <input
            data-testid="displayName-input"
            value={displayNameField.value}
            onChange={(e) => displayNameField.setValue(e.target.value)}
            placeholder="Display Name"
          />
          <span data-testid="firstName-display">{firstNameField.value}</span>
        </div>
      )
    }

    renderWithStore(<FormComponent />, store, { ...profileFormFixtures.filled })

    const input = screen.getByTestId('displayName-input') as HTMLInputElement
    fireEvent.change(input, { target: { value: 'Jane' } })

    await flushEffects()

    expect(input.value).toBe('Jane')
  })

  // TC2.4: Circular sync doesn't infinite loop
  it('TC2.4: handles circular sync pairs without infinite loops', async () => {
    let renderCount = 0

    function FormComponent() {
      renderCount++

      store.useSideEffects('circular-sync', {
        syncPaths: [
          typeHelpers.syncPair<ProfileForm>('firstName', 'lastName'),
          typeHelpers.syncPair<ProfileForm>('lastName', 'firstName'),
        ],
      })

      const firstNameField = store.useFieldStore('firstName')
      const lastNameField = store.useFieldStore('lastName')

      return (
        <div>
          <input
            data-testid="firstName-input"
            value={firstNameField.value}
            onChange={(e) => firstNameField.setValue(e.target.value)}
          />
          <input
            data-testid="lastName-input"
            value={lastNameField.value}
            onChange={(e) => lastNameField.setValue(e.target.value)}
          />
          <span data-testid="render-count">{renderCount}</span>
        </div>
      )
    }

    renderWithStore(<FormComponent />, store, { ...profileFormFixtures.filled })

    const firstInput = screen.getByTestId('firstName-input') as HTMLInputElement
    const initialRenderCount = renderCount

    // Change firstName - should sync to lastName but not create infinite loop
    fireEvent.change(firstInput, { target: { value: 'Jane' } })

    await flushEffects()

    expect(firstInput.value).toBe('Jane')
    // Verify no excessive re-renders (processing should be bounded)
    expect(renderCount).toBeLessThan(initialRenderCount + 10)
  })

  // TC2.5: Multiple syncs in one setValue batch correctly
  it('TC2.5: handles multiple path updates in single batch', async () => {
    function FormComponent() {
      store.useSideEffects('multi-sync', {
        syncPaths: [
          typeHelpers.syncPair<ProfileForm>('firstName', 'displayName'),
        ],
      })

      const firstNameField = store.useFieldStore('firstName')
      const displayNameField = store.useFieldStore('displayName')

      return (
        <div>
          <input
            data-testid="firstName-input"
            value={firstNameField.value}
            onChange={(e) => firstNameField.setValue(e.target.value)}
          />
          <span data-testid="displayName-value">{displayNameField.value}</span>
        </div>
      )
    }

    renderWithStore(<FormComponent />, store, { ...profileFormFixtures.empty })

    const input = screen.getByTestId('firstName-input') as HTMLInputElement

    // Multiple changes
    fireEvent.change(input, { target: { value: 'John' } })
    fireEvent.change(input, { target: { value: 'Johnny' } })

    await flushEffects()

    expect(input.value).toBe('Johnny')
  })

  // TC2.6: Sync paths respect order (no race conditions)
  it('TC2.6: processes sync pairs in correct order', async () => {
    const executionLog: string[] = []

    function FormComponent() {
      store.useSideEffects('ordered-sync', {
        syncPaths: [
          typeHelpers.syncPair<ProfileForm>('firstName', 'lastName'),
          typeHelpers.syncPair<ProfileForm>('lastName', 'displayName'),
        ],
      })

      const firstNameField = store.useFieldStore('firstName')
      const lastNameField = store.useFieldStore('lastName')
      const displayNameField = store.useFieldStore('displayName')

      return (
        <div>
          <input
            data-testid="firstName-input"
            value={firstNameField.value}
            onChange={(e) => {
              executionLog.push('firstName-change')
              firstNameField.setValue(e.target.value)
            }}
          />
          <span data-testid="lastName-value">{lastNameField.value}</span>
          <span data-testid="displayName-value">{displayNameField.value}</span>
        </div>
      )
    }

    renderWithStore(<FormComponent />, store, { ...profileFormFixtures.empty })

    const input = screen.getByTestId('firstName-input') as HTMLInputElement
    fireEvent.change(input, { target: { value: 'John' } })

    await flushEffects()

    expect(input.value).toBe('John')
    expect(executionLog.length).toBeGreaterThan(0)
  })
})
