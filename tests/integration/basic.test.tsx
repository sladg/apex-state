/**
 * Integration Tests: Basic Feature Integration
 *
 * Simple tests demonstrating that core features work together.
 * No complex async patterns - just verify the basics work.
 */

import React from 'react'
import { describe, test, expect } from 'vitest'
import { render, screen } from '@testing-library/react'
import { createGenericStore } from '../../src'

describe('Integration: Core Features Working Together', () => {
  test('store with sync paths working', () => {
    type State = {
      a: number
      b: number
      c: number
    }

    const store = createGenericStore<State>()

    function Component() {
      store.useSideEffects('test', {
        syncPaths: {
          pairs: [{ id: 'sync-ab', path1: 'a' as any, path2: 'b' as any }],
        },
      })

      const [a] = store.useStore('a')
      const [b] = store.useStore('b')
      const [c] = store.useStore('c')

      return <div>
        <span data-testid="a">{a}</span>
        <span data-testid="b">{b}</span>
        <span data-testid="c">{c}</span>
      </div>
    }

    render(
      <store.Provider initialState={{ a: 1, b: 1, c: 5 }}>
        <Component />
      </store.Provider>
    )

    // Sync: a and b should be synced (both 1)
    expect(screen.getByTestId('a').textContent).toBe('1')
    expect(screen.getByTestId('b').textContent).toBe('1')

    // c is independent
    expect(screen.getByTestId('c').textContent).toBe('5')
  })

  test('multiple side effects registered simultaneously', () => {
    type State = {
      field1: string
      field2: string
      flag1: boolean
      flag2: boolean
    }

    const store = createGenericStore<State>()

    function Component() {
      store.useSideEffects('multi-effects', {
        syncPaths: {
          pairs: [{ id: 'sync', path1: 'field1' as any, path2: 'field2' as any }],
        },
        flipPaths: {
          pairs: [{ id: 'flip', path1: 'flag1' as any, path2: 'flag2' as any }],
        },
      })

      const [field1] = store.useStore('field1')
      const [field2] = store.useStore('field2')
      const [flag1] = store.useStore('flag1')
      const [flag2] = store.useStore('flag2')

      return <div>
        <span data-testid="field1">{field1}</span>
        <span data-testid="field2">{field2}</span>
        <span data-testid="flag1">{flag1.toString()}</span>
        <span data-testid="flag2">{flag2.toString()}</span>
      </div>
    }

    render(
      <store.Provider initialState={{
        field1: 'test',
        field2: 'test',
        flag1: true,
        flag2: false,
      }}>
        <Component />
      </store.Provider>
    )

    // Sync: fields should match
    expect(screen.getByTestId('field1').textContent).toBe('test')
    expect(screen.getByTestId('field2').textContent).toBe('test')

    // Flip: flags should be opposite
    expect(screen.getByTestId('flag1').textContent).toBe('true')
    expect(screen.getByTestId('flag2').textContent).toBe('false')
  })

  test('hooks work correctly with Provider', () => {
    type State = { value: string }
    const store = createGenericStore<State>()

    function Component() {
      const field = store.useFieldStore('value')
      const { proxyValue } = store.useJitStore()

      return <div>
        <span data-testid="field-value">{field.value}</span>
        <span data-testid="proxy-value">{proxyValue.value}</span>
      </div>
    }

    render(
      <store.Provider initialState={{ value: 'test-value' }}>
        <Component />
      </store.Provider>
    )

    expect(screen.getByTestId('field-value').textContent).toBe('test-value')
    expect(screen.getByTestId('proxy-value').textContent).toBe('test-value')
  })
})
