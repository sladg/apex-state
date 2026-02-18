/**
 * Type-level tests for createGenericStore
 *
 * Verifies compile-time type inference using expectTypeOf() and @ts-expect-error.
 * No state-change logic here — these tests exist to catch regressions where a type
 * is accidentally widened to any / unknown / string.
 *
 * Structure:
 *  - useFieldStore value types  — expectTypeOf inside components (hooks require React context)
 *  - path type safety           — @ts-expect-error, no component needed
 *  - narrowness guards          — @ts-expect-error with declare const / ReturnType<>
 *  - withConcerns type inference — expectTypeOf inside components
 */

import { screen } from '@testing-library/react'
import { describe, expect, expectTypeOf, it } from 'vitest'
// ---------------------------------------------------------------------------
// useConcerns — registration type safety
// ---------------------------------------------------------------------------
import { z } from 'zod'

import type { ValidationStateResult } from '../../src/concerns/prebuilts'
import { createGenericStore } from '../../src/store/create-store'
import { mountStore } from '../utils/react'

// ---------------------------------------------------------------------------
// Shared fixtures
// ---------------------------------------------------------------------------

interface FormState {
  user: { name: string; email: string }
  count: number
  active: boolean
}

// Legacy mode — keeps tests independent of WASM async loading timing
const store = createGenericStore<FormState>({ useLegacyImplementation: true })

const initialState: FormState = {
  user: { name: 'Alice', email: 'alice@example.com' },
  count: 0,
  active: true,
}

// ---------------------------------------------------------------------------
// Helper types used by narrowness guards.
// These extract field result types without any runtime hooks.
// ---------------------------------------------------------------------------

type NameField = ReturnType<typeof store.useFieldStore<'user.name'>>
type CountField = ReturnType<typeof store.useFieldStore<'count'>>
type ActiveField = ReturnType<typeof store.useFieldStore<'active'>>
type UserField = ReturnType<typeof store.useFieldStore<'user'>>

// withConcerns field types — declare const used only in typeof (type position), never at runtime.
// TypeScript 4.7 instantiation expressions let us fix the generic parameter P.
declare const _wcStore: ReturnType<
  typeof store.withConcerns<{ validationState: true }>
>
type WcNameField = ReturnType<typeof _wcStore.useFieldStore<'user.name'>>
type WcCountField = ReturnType<typeof _wcStore.useFieldStore<'count'>>

// ---------------------------------------------------------------------------
// useFieldStore — value type matches the path
// ---------------------------------------------------------------------------

describe('createGenericStore — useFieldStore value types', () => {
  it('infers string value type for string path', () => {
    // expectTypeOf() is a compile-time assertion — wrong types cause TS compilation failure
    function TypeComponent() {
      const nameField = store.useFieldStore('user.name')
      expectTypeOf(nameField.value).toBeString()
      expectTypeOf(nameField.setValue).parameter(0).toBeString()
      return <div data-testid="result">pass</div>
    }
    mountStore(<TypeComponent />, store, initialState)
    expect(screen.getByTestId('result')).toHaveTextContent('pass')
  })

  it('infers number value type for number path', () => {
    function TypeComponent() {
      const countField = store.useFieldStore('count')
      expectTypeOf(countField.value).toBeNumber()
      expectTypeOf(countField.setValue).parameter(0).toBeNumber()
      return <div data-testid="result">pass</div>
    }
    mountStore(<TypeComponent />, store, initialState)
    expect(screen.getByTestId('result')).toHaveTextContent('pass')
  })

  it('infers boolean value type for boolean path', () => {
    function TypeComponent() {
      const activeField = store.useFieldStore('active')
      expectTypeOf(activeField.value).toBeBoolean()
      expectTypeOf(activeField.setValue).parameter(0).toBeBoolean()
      return <div data-testid="result">pass</div>
    }
    mountStore(<TypeComponent />, store, initialState)
    expect(screen.getByTestId('result')).toHaveTextContent('pass')
  })

  it('infers nested object value type for object path', () => {
    function TypeComponent() {
      const userField = store.useFieldStore('user')
      // Full object type preserved — not collapsed to 'object' or 'unknown'
      expectTypeOf(userField.value).toEqualTypeOf<FormState['user']>()
      return <div data-testid="result">pass</div>
    }
    mountStore(<TypeComponent />, store, initialState)
    expect(screen.getByTestId('result')).toHaveTextContent('pass')
  })
})

// ---------------------------------------------------------------------------
// Path type safety — @ts-expect-error confirms invalid paths are rejected
// ---------------------------------------------------------------------------

describe('createGenericStore — path type safety', () => {
  it('useFieldStore accepts only valid DeepKey<DATA> paths', () => {
    type ValidPath = Parameters<typeof store.useFieldStore>[0]

    const p1: ValidPath = 'user.name'
    const p2: ValidPath = 'count'
    const p3: ValidPath = 'user.email'
    const p4: ValidPath = 'user'
    const p5: ValidPath = 'active'
    void [p1, p2, p3, p4, p5]

    // @ts-expect-error — 'nonExistent' is not in DeepKey<FormState>
    const bad: ValidPath = 'nonExistent'
    void bad
  })

  it('withConcerns useFieldStore also enforces path type safety', () => {
    type ValidPath = Parameters<
      ReturnType<
        typeof store.withConcerns<{ validationState: true }>
      >['useFieldStore']
    >[0]

    const p1: ValidPath = 'user.name'
    const p2: ValidPath = 'count'
    void [p1, p2]

    // @ts-expect-error — 'user.phone' does not exist in FormState
    const bad: ValidPath = 'user.phone'
    void bad
  })
})

// ---------------------------------------------------------------------------
// Narrowness guards — @ts-expect-error ensures types are NOT too wide.
// If a type is accidentally widened to any/unknown/string, the wrong-type
// assignment stops erroring → the @ts-expect-error directive itself becomes
// the error, surfacing the regression.
//
// Pattern: `null as unknown as T` gives a value of type T at compile time
// without requiring an actual value at runtime (null is safe with void).
// ---------------------------------------------------------------------------

describe('createGenericStore — narrowness guards', () => {
  it('useFieldStore string value is not assignable to number or boolean', () => {
    // null as unknown as NameField['value'] = null at runtime, typed as string at compile time
    // @ts-expect-error — string is not assignable to number (catches 'any' widening)
    const _n: number = null as unknown as NameField['value']
    // @ts-expect-error — string is not assignable to boolean
    const _b: boolean = null as unknown as NameField['value']
    void [_n, _b]
  })

  it('useFieldStore number value is not assignable to string or boolean', () => {
    // @ts-expect-error — number is not assignable to string
    const _s: string = null as unknown as CountField['value']
    // @ts-expect-error — number is not assignable to boolean
    const _b: boolean = null as unknown as CountField['value']
    void [_s, _b]
  })

  it('useFieldStore boolean value is not assignable to string or number', () => {
    // @ts-expect-error — boolean is not assignable to string
    const _s: string = null as unknown as ActiveField['value']
    // @ts-expect-error — boolean is not assignable to number
    const _n: number = null as unknown as ActiveField['value']
    void [_s, _n]
  })

  it('useFieldStore object value is not assignable to primitives', () => {
    // @ts-expect-error — FormState['user'] is not assignable to string
    const _s: string = null as unknown as UserField['value']
    // @ts-expect-error — FormState['user'] is not assignable to number
    const _n: number = null as unknown as UserField['value']
    void [_s, _n]
  })

  it('setValue rejects wrong value types', () => {
    type StringSetter = Parameters<NameField['setValue']>[0]
    type NumberSetter = Parameters<CountField['setValue']>[0]

    // @ts-expect-error — setter for 'user.name' requires string, not number
    const _1: StringSetter = 42 as number
    // @ts-expect-error — setter for 'count' requires number, not string
    const _2: NumberSetter = 'hello' as string
    void [_1, _2]
  })

  it('withConcerns value is not wider than the path type', () => {
    // @ts-expect-error — 'user.name' value is string, not number
    const _1: number = null as unknown as WcNameField['value']
    // @ts-expect-error — 'count' value is number, not string
    const _2: string = null as unknown as WcCountField['value']
    void [_1, _2]
  })

  it('validationState is not wider than ValidationStateResult', () => {
    // @ts-expect-error — ValidationStateResult | undefined is not string
    const _s: string = null as unknown as WcNameField['validationState']
    // @ts-expect-error — ValidationStateResult | undefined is not boolean
    const _b: boolean = null as unknown as WcNameField['validationState']
    // @ts-expect-error — ValidationStateResult | undefined is not number
    const _n: number = null as unknown as WcNameField['validationState']
    void [_s, _b, _n]
  })
})

// ---------------------------------------------------------------------------
// withConcerns — concern result types and value type preservation
// ---------------------------------------------------------------------------

describe('createGenericStore — withConcerns type inference', () => {
  it('rejects unknown concern names', () => {
    // @ts-expect-error — 'nonExistent' is not a registered concern name
    store.withConcerns({ nonExistent: true })
  })

  it('validationState is typed as ValidationStateResult | undefined when selected', () => {
    function TypeComponent() {
      const field = store
        .withConcerns({ validationState: true })
        .useFieldStore('user.name')

      // validationState must come through as the proper result type, not unknown
      expectTypeOf(field.validationState).toEqualTypeOf<
        ValidationStateResult | undefined
      >()
      expectTypeOf(field.value).toBeString()
      return <div data-testid="result">pass</div>
    }
    mountStore(<TypeComponent />, store, initialState)
    expect(screen.getByTestId('result')).toHaveTextContent('pass')
  })

  it('disabledWhen concern is accessible when selected (value type is correct)', () => {
    // disabledWhen concern type is imprecise (see note above), but value type must
    // still be correctly driven by the path
    function TypeComponent() {
      const field = store
        .withConcerns({ disabledWhen: true })
        .useFieldStore('user.name')

      expectTypeOf(field.value).toBeString()
      return <div data-testid="result">pass</div>
    }
    mountStore(<TypeComponent />, store, initialState)
    expect(screen.getByTestId('result')).toHaveTextContent('pass')
  })

  it('value type tracks the path even when concerns are selected', () => {
    function TypeComponent() {
      const nameField = store
        .withConcerns({ validationState: true })
        .useFieldStore('user.name')
      const countField = store
        .withConcerns({ validationState: true })
        .useFieldStore('count')
      const activeField = store
        .withConcerns({ disabledWhen: true })
        .useFieldStore('active')

      expectTypeOf(nameField.value).toBeString()
      expectTypeOf(countField.value).toBeNumber()
      expectTypeOf(activeField.value).toBeBoolean()
      return <div data-testid="result">pass</div>
    }
    mountStore(<TypeComponent />, store, initialState)
    expect(screen.getByTestId('result')).toHaveTextContent('pass')
  })

  it('validationState is precise even when combined with other selections', () => {
    function TypeComponent() {
      const field = store
        .withConcerns({ validationState: true, disabledWhen: true })
        .useFieldStore('count')

      expectTypeOf(field.validationState).toEqualTypeOf<
        ValidationStateResult | undefined
      >()
      expectTypeOf(field.value).toBeNumber()
      return <div data-testid="result">pass</div>
    }
    mountStore(<TypeComponent />, store, initialState)
    expect(screen.getByTestId('result')).toHaveTextContent('pass')
  })

  it('setValue parameter type tracks the path inside withConcerns', () => {
    function TypeComponent() {
      const nameField = store
        .withConcerns({ validationState: true })
        .useFieldStore('user.name')
      const countField = store
        .withConcerns({ disabledWhen: true })
        .useFieldStore('count')
      const activeField = store
        .withConcerns({ validationState: true })
        .useFieldStore('active')

      expectTypeOf(nameField.setValue).parameter(0).toBeString()
      expectTypeOf(countField.setValue).parameter(0).toBeNumber()
      expectTypeOf(activeField.setValue).parameter(0).toBeBoolean()
      return <div data-testid="result">pass</div>
    }
    mountStore(<TypeComponent />, store, initialState)
    expect(screen.getByTestId('result')).toHaveTextContent('pass')
  })
})

describe('createGenericStore — useConcerns registration type safety', () => {
  it('rejects schema with wrong type for path', () => {
    function TypeComponent() {
      store.useConcerns('my-concerns', {
        'user.name': {
          // @ts-expect-error — user.name is string, not number
          validationState: { schema: z.number() },
        },
      })
      return <div data-testid="result">pass</div>
    }
    mountStore(<TypeComponent />, store, initialState)
    expect(screen.getByTestId('result')).toHaveTextContent('pass')
  })

  it('accepts schema with correct type for path', () => {
    function TypeComponent() {
      store.useConcerns('my-concerns', {
        'user.name': {
          validationState: { schema: z.string().min(1) },
        },
        'count': {
          validationState: { schema: z.number().positive() },
        },
      })
      return <div data-testid="result">pass</div>
    }
    mountStore(<TypeComponent />, store, initialState)
    expect(screen.getByTestId('result')).toHaveTextContent('pass')
  })

  it('rejects unknown concern names in useConcerns registration', () => {
    function TypeComponent() {
      store.useConcerns('my-concerns', {
        'user.name': {
          // @ts-expect-error — 'nonExistent' is not a registered concern name
          nonExistent: { schema: z.string() },
        },
      })
      return <div data-testid="result">pass</div>
    }
    mountStore(<TypeComponent />, store, initialState)
    expect(screen.getByTestId('result')).toHaveTextContent('pass')
  })
})
