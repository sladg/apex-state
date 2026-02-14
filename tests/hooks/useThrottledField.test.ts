/**
 * Tests for useThrottledField hook
 *
 * Verifies throttle behavior:
 * - First setValue executes immediately
 * - Subsequent rapid calls are buffered
 * - Last value wins when multiple calls occur within throttle window
 * - Timeout cleanup on unmount
 */

/* eslint-disable no-restricted-syntax */
import { act, renderHook } from '@testing-library/react'
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'

import { useThrottledField } from '~/hooks/useThrottledField'

describe('useThrottledField', () => {
  beforeEach(() => {
    vi.useFakeTimers()
  })

  afterEach(() => {
    vi.useRealTimers()
  })

  it('should execute first setValue immediately', () => {
    const field = { value: 0, setValue: vi.fn() }
    const { result } = renderHook(() => useThrottledField(field, { ms: 100 }))

    act(() => {
      result.current.setValue(42)
    })

    expect(field.setValue).toHaveBeenCalledWith(42)
    expect(field.setValue).toHaveBeenCalledTimes(1)
  })

  it('should throttle rapid setValue calls', () => {
    const field = { value: 0, setValue: vi.fn() }
    const { result } = renderHook(() => useThrottledField(field, { ms: 50 }))

    act(() => {
      result.current.setValue(1) // Immediate
      result.current.setValue(2) // Buffered
      result.current.setValue(3) // Buffered (overwrites 2)
    })

    expect(field.setValue).toHaveBeenCalledTimes(1)
    expect(field.setValue).toHaveBeenCalledWith(1)

    // Advance timer to trigger flush
    act(() => {
      vi.advanceTimersByTime(60)
    })

    expect(field.setValue).toHaveBeenCalledTimes(2)
    expect(field.setValue).toHaveBeenLastCalledWith(3)
  })

  it('should pass through other field properties', () => {
    const field = {
      value: 0,
      setValue: vi.fn(),
      extra: 'data',
      nested: { a: 1 },
    }
    const { result } = renderHook(() => useThrottledField(field, { ms: 100 }))

    expect(result.current.extra).toBe('data')
    expect(result.current.nested).toEqual({ a: 1 })
    expect(result.current.value).toBe(0)
  })

  it('should cleanup timeout on unmount', () => {
    const field = { value: 0, setValue: vi.fn() }
    const { result, unmount } = renderHook(() =>
      useThrottledField(field, { ms: 100 }),
    )

    act(() => {
      result.current.setValue(1) // Immediate
      result.current.setValue(2) // Buffered
    })

    expect(field.setValue).toHaveBeenCalledTimes(1)

    unmount()

    // Advance timer - buffered value should NOT be applied
    act(() => {
      vi.advanceTimersByTime(150)
    })

    expect(field.setValue).toHaveBeenCalledTimes(1)
  })

  it('should allow immediate call after throttle period', () => {
    const field = { value: 0, setValue: vi.fn() }
    const { result } = renderHook(() => useThrottledField(field, { ms: 50 }))

    act(() => {
      result.current.setValue(1) // Immediate
    })

    expect(field.setValue).toHaveBeenCalledTimes(1)

    // Wait for throttle period to pass
    act(() => {
      vi.advanceTimersByTime(60)
    })

    act(() => {
      result.current.setValue(2) // Should be immediate again
    })

    expect(field.setValue).toHaveBeenCalledTimes(2)
    expect(field.setValue).toHaveBeenLastCalledWith(2)
  })

  it('should handle value type correctly', () => {
    const field = { value: 'initial', setValue: vi.fn() }
    const { result } = renderHook(() => useThrottledField(field, { ms: 100 }))

    act(() => {
      result.current.setValue('updated')
    })

    expect(field.setValue).toHaveBeenCalledWith('updated')
  })

  it('should handle object values', () => {
    const field = {
      value: { name: 'test' },
      setValue: vi.fn(),
    }
    const { result } = renderHook(() => useThrottledField(field, { ms: 50 }))

    const newValue = { name: 'updated' }

    act(() => {
      result.current.setValue(newValue)
    })

    expect(field.setValue).toHaveBeenCalledWith(newValue)
  })

  it('should not schedule multiple timeouts for rapid calls', () => {
    const field = { value: 0, setValue: vi.fn() }
    const { result } = renderHook(() => useThrottledField(field, { ms: 100 }))

    act(() => {
      result.current.setValue(1) // Immediate
      result.current.setValue(2) // Buffered, schedules timeout
      result.current.setValue(3) // Buffered, should NOT schedule another timeout
      result.current.setValue(4) // Buffered
    })

    expect(field.setValue).toHaveBeenCalledTimes(1)

    // Advance timer
    act(() => {
      vi.advanceTimersByTime(110)
    })

    // Should only have been called once more (not multiple times)
    expect(field.setValue).toHaveBeenCalledTimes(2)
    expect(field.setValue).toHaveBeenLastCalledWith(4)
  })

  it('should work with zero throttle time', () => {
    const field = { value: 0, setValue: vi.fn() }
    const { result } = renderHook(() => useThrottledField(field, { ms: 0 }))

    act(() => {
      result.current.setValue(1)
      result.current.setValue(2)
      result.current.setValue(3)
    })

    // With ms: 0, all calls should be immediate
    expect(field.setValue).toHaveBeenCalledTimes(3)
  })

  it('should flush pending value when enough time passes between calls', () => {
    const field = { value: 0, setValue: vi.fn() }
    const { result } = renderHook(() => useThrottledField(field, { ms: 50 }))

    act(() => {
      result.current.setValue(1) // Immediate
    })

    expect(field.setValue).toHaveBeenCalledTimes(1)

    // Advance time but not enough
    act(() => {
      vi.advanceTimersByTime(30)
    })

    act(() => {
      result.current.setValue(2) // Buffered
    })

    expect(field.setValue).toHaveBeenCalledTimes(1)

    // Advance remaining time
    act(() => {
      vi.advanceTimersByTime(25)
    })

    expect(field.setValue).toHaveBeenCalledTimes(2)
    expect(field.setValue).toHaveBeenLastCalledWith(2)
  })

  describe('meta argument support', () => {
    it('should pass meta argument on immediate call', () => {
      const field = {
        value: 0,
        setValue: vi.fn(),
      }
      const { result } = renderHook(() => useThrottledField(field, { ms: 100 }))

      act(() => {
        result.current.setValue(42, { source: 'websocket' })
      })

      expect(field.setValue).toHaveBeenCalledWith(42, { source: 'websocket' })
    })

    it('should preserve last meta argument when throttling', () => {
      const field = {
        value: 0,
        setValue: vi.fn(),
      }
      const { result } = renderHook(() => useThrottledField(field, { ms: 50 }))

      act(() => {
        result.current.setValue(1, { source: 'user' }) // Immediate
        result.current.setValue(2, { source: 'api' }) // Buffered
        result.current.setValue(3, { source: 'websocket' }) // Buffered (overwrites)
      })

      expect(field.setValue).toHaveBeenCalledTimes(1)
      expect(field.setValue).toHaveBeenCalledWith(1, { source: 'user' })

      // Advance timer to trigger flush
      act(() => {
        vi.advanceTimersByTime(60)
      })

      expect(field.setValue).toHaveBeenCalledTimes(2)
      expect(field.setValue).toHaveBeenLastCalledWith(3, {
        source: 'websocket',
      })
    })

    it('should work without meta argument', () => {
      const field = {
        value: 0,
        setValue: vi.fn(),
      }
      const { result } = renderHook(() => useThrottledField(field, { ms: 100 }))

      act(() => {
        result.current.setValue(42)
      })

      expect(field.setValue).toHaveBeenCalledWith(42)
    })
  })

  describe('composability', () => {
    it('should work with extended field interfaces', () => {
      interface ExtendedField {
        value: number
        setValue: (v: number) => void
        commit: () => void
        cancel: () => void
        isDirty: boolean
      }

      const field: ExtendedField = {
        value: 0,
        setValue: vi.fn(),
        commit: vi.fn(),
        cancel: vi.fn(),
        isDirty: false,
      }

      const { result } = renderHook(() =>
        useThrottledField<number, [], ExtendedField>(field, { ms: 100 }),
      )

      // Check all passthrough props exist
      expect(result.current.value).toBe(0)
      expect(result.current.commit).toBe(field.commit)
      expect(result.current.cancel).toBe(field.cancel)
      expect(result.current.isDirty).toBe(false)

      // setValue should be throttled
      act(() => {
        result.current.setValue(42)
      })

      expect(field.setValue).toHaveBeenCalledWith(42)
    })
  })
})
