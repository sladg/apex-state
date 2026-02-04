/**
 * Integration Tests: Basic Feature Integration
 *
 * Simple tests demonstrating that core features work together.
 * No complex async patterns - just verify the basics work.
 */

import { screen } from '@testing-library/react'
import { describe, expect, test } from 'vitest'

import { createGenericStore } from '../../src'
import { typeHelpers } from '../mocks'
import { Component } from '../utils/components'
import { renderWithStore } from '../utils/react'

describe('Integration: Core Features Working Together', () => {
  test('store with sync paths working', () => {
    interface State {
      a: number
      b: number
      c: number
    }

    const store = createGenericStore<State>()

    const sideEffects = {
      syncPaths: [typeHelpers.syncPair<State>('a', 'b')],
    }

    renderWithStore(
      <Component store={store} sideEffects={sideEffects} />,
      store,
      { a: 1, b: 1, c: 5 },
    )

    // Sync: a and b should be synced (both 1)
    expect(screen.getByTestId('a').textContent).toBe('1')
    expect(screen.getByTestId('b').textContent).toBe('1')

    // c is independent
    expect(screen.getByTestId('c').textContent).toBe('5')
  })

  test('multiple side effects registered simultaneously', () => {
    interface State {
      field1: string
      field2: string
      flag1: boolean
      flag2: boolean
    }

    const store = createGenericStore<State>()

    const sideEffects = {
      syncPaths: [typeHelpers.syncPair<State>('field1', 'field2')],
      flipPaths: [typeHelpers.flipPair<State>('flag1', 'flag2')],
    }

    renderWithStore(
      <Component store={store} sideEffects={sideEffects} />,
      store,
      {
        field1: 'test',
        field2: 'test',
        flag1: true,
        flag2: false,
      },
    )

    // Sync: fields should match
    expect(screen.getByTestId('field1').textContent).toBe('test')
    expect(screen.getByTestId('field2').textContent).toBe('test')

    // Flip: flags should be opposite
    expect(screen.getByTestId('flag1').textContent).toBe('true')
    expect(screen.getByTestId('flag2').textContent).toBe('false')
  })

  test('hooks work correctly with Provider', () => {
    interface State {
      value: string
    }
    const store = createGenericStore<State>()

    renderWithStore(<Component store={store} />, store, { value: 'test-value' })

    expect(screen.getByTestId('field-value').textContent).toBe('test-value')
    expect(screen.getByTestId('proxy-value').textContent).toBe('test-value')
  })
})
