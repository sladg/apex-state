/**
 * Testing mock examples — __mocked setup, seed state, track calls.
 *
 * These examples type-check against the real @sladg/apex-state/testing exports.
 */

import { __mocked, createGenericStore } from '@sladg/apex-state/testing'
import { renderHook } from '@testing-library/react'
import { beforeEach, describe, expect, it, vi } from 'vitest'

vi.mock('@sladg/apex-state')

interface FormState {
  user: {
    email: string
    name: string
  }
  count: number
}

// @llms-example: Seed mock state with type-safe chaining and assert snapshot values
describe('testing mock examples', () => {
  beforeEach(() => __mocked.reset())

  it('seeds state with type-safe chaining', () => {
    __mocked
      .set<FormState>({ user: { email: '', name: '' }, count: 0 })
      .set('user.email', 'alice@test.com')
      .set('count', 42)

    const snap = __mocked.getState()
    expect(snap).toEqual({
      user: { email: 'alice@test.com', name: '' },
      count: 42,
    })
  })
})
// @llms-example-end

// @llms-example: Track setValue calls from useStore hooks in consumer tests
describe('tracking setValue calls', () => {
  beforeEach(() => __mocked.reset())

  it('tracks setValue calls', () => {
    const store = createGenericStore<FormState>()
    const hook = renderHook(() => store.useStore('user.email'))

    // Simulate user interaction
    hook.result.current[1]('bob@test.com')

    // Assert the call was tracked
    expect(__mocked.state.calls).toContainEqual({
      path: 'user.email',
      value: 'bob@test.com',
      meta: undefined,
    })
  })
})
// @llms-example-end

// @llms-example: Assert useConcerns and useSideEffects registrations are tracked
describe('effect registration tracking', () => {
  beforeEach(() => __mocked.reset())

  it('tracks concern registrations', () => {
    const store = createGenericStore<FormState>()
    const registration = {
      'user.email': { disabledWhen: { boolLogic: { AND: [] } } },
    }

    store.useConcerns('form-concerns', registration)

    expect(__mocked.state.effects).toContainEqual({
      id: 'form-concerns',
      type: 'concerns',
      registration,
    })
  })

  it('tracks side effect registrations', () => {
    const store = createGenericStore<FormState>()
    store.useSideEffects('my-effects', { listeners: [] })

    expect(__mocked.state.effects).toHaveLength(1)
    expect(__mocked.state.effects[0]?.type).toBe('sideEffects')
  })
})
// @llms-example-end
