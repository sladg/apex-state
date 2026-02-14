/**
 * Integration test for complete pipeline flow
 * Tests that the real user-facing API works correctly
 */

import { screen } from '@testing-library/react'
import { beforeEach, describe, expect, it } from 'vitest'

import { createGenericStore } from '~/store/createStore'

import { flush, renderWithStore } from '../utils/react'

interface TestForm {
  email: string
  backupEmail: string
  isEnabled: boolean
  isDisabled: boolean
  user: {
    name: string
    profile: {
      bio: string
    }
  }
}

describe('Pipeline Integration Tests', () => {
  let store: ReturnType<typeof createGenericStore<TestForm>>

  beforeEach(() => {
    store = createGenericStore<TestForm>()
  })

  it('should sync values across paired paths', async () => {
    const TestComponent = () => {
      const [email, setEmail] = store.useStore('email')
      const [backupEmail] = store.useStore('backupEmail')

      store.useSideEffects('sync-test', {
        syncPaths: [['email', 'backupEmail']],
      })

      return (
        <div>
          <button
            onClick={() => setEmail('test@example.com')}
            data-testid="set-email"
          >
            Set Email
          </button>
          <div data-testid="email">{email}</div>
          <div data-testid="backup">{backupEmail}</div>
        </div>
      )
    }

    renderWithStore(<TestComponent />, store, {
      email: '',
      backupEmail: '',
      isEnabled: true,
      isDisabled: false,
      user: { name: '', profile: { bio: '' } },
    })

    const button = screen.getByTestId('set-email')
    button.click()

    await flush()

    // Email and backup should be synced
    expect(screen.getByTestId('email').textContent).toBe('test@example.com')
    expect(screen.getByTestId('backup').textContent).toBe('test@example.com')
  })

  it('should flip boolean values across paired paths', async () => {
    const TestComponent = () => {
      const [isEnabled, setEnabled] = store.useStore('isEnabled')
      const [isDisabled] = store.useStore('isDisabled')

      store.useSideEffects('flip-test', {
        flipPaths: [['isEnabled', 'isDisabled']],
      })

      return (
        <div>
          <button onClick={() => setEnabled(false)} data-testid="toggle">
            Toggle
          </button>
          <div data-testid="enabled">{String(isEnabled)}</div>
          <div data-testid="disabled">{String(isDisabled)}</div>
        </div>
      )
    }

    renderWithStore(<TestComponent />, store, {
      email: '',
      backupEmail: '',
      isEnabled: true,
      isDisabled: false,
      user: { name: '', profile: { bio: '' } },
    })

    const button = screen.getByTestId('toggle')
    button.click()

    await flush()

    // Values should be flipped
    expect(screen.getByTestId('enabled').textContent).toBe('false')
    expect(screen.getByTestId('disabled').textContent).toBe('true')
  })

  it('should process all pipeline stages correctly', async () => {
    const TestComponent = () => {
      const [email, setEmail] = store.useStore('email')
      const [backupEmail] = store.useStore('backupEmail')
      const [isEnabled, setEnabled] = store.useStore('isEnabled')
      const [isDisabled] = store.useStore('isDisabled')

      store.useSideEffects('full-pipeline-test', {
        syncPaths: [['email', 'backupEmail']],
        flipPaths: [['isEnabled', 'isDisabled']],
      })

      return (
        <div>
          <button
            onClick={() => {
              setEmail('test@example.com')
              setEnabled(false)
            }}
            data-testid="update-all"
          >
            Update All
          </button>
          <div data-testid="email">{email}</div>
          <div data-testid="backup">{backupEmail}</div>
          <div data-testid="enabled">{String(isEnabled)}</div>
          <div data-testid="disabled">{String(isDisabled)}</div>
        </div>
      )
    }

    renderWithStore(<TestComponent />, store, {
      email: '',
      backupEmail: '',
      isEnabled: true,
      isDisabled: false,
      user: { name: '', profile: { bio: '' } },
    })

    const button = screen.getByTestId('update-all')
    button.click()

    await flush()

    // Sync should work
    expect(screen.getByTestId('email').textContent).toBe('test@example.com')
    expect(screen.getByTestId('backup').textContent).toBe('test@example.com')

    // Flip should work
    expect(screen.getByTestId('enabled').textContent).toBe('false')
    expect(screen.getByTestId('disabled').textContent).toBe('true')
  })
})
