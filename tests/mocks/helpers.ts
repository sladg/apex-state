/**
 * Test helper utilities
 *
 * Common utilities and patterns used across integration tests.
 *
 * ## Purpose
 * Provides reusable validation functions, error messages, DOM helpers,
 * and test patterns that eliminate duplication across integration tests.
 *
 * ## Categories
 *
 * - **validators** - Business logic validation functions (email, password, url)
 * - **errorMessages** - Standardized error strings matching validation rules
 * - **assertions** - DOM state checks (disabled, visible, readonly)
 * - **testPatterns** - Common user interaction patterns (fillForm, submitForm)
 * - **domHelpers** - Query utilities for finding elements and errors
 * - **generators** - Random data generators for test cases
 * - **typeHelpers** - Type-safe wrappers for runtime/dynamic paths
 *
 * ## Complementary Tools
 * For concern-specific testing (performance, evaluation tracking), see
 * `tests/concerns/test-utils.ts` instead.
 */

import type { ArrayOfChanges, GenericMeta } from '../../src/types'
import type { FlipPair, SyncPair } from '../../src/types/pathsOfSameValue'

/**
 * Common validators used in tests
 */
export const validators = {
  /**
   * Email validator
   */
  email: (email: string): boolean => {
    return /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(email)
  },

  /**
   * Simple password validator (8+ chars, 1 uppercase, 1 number)
   */
  password: (password: string): boolean => {
    return (
      /^.{8,}$/.test(password) && // 8+ chars
      /[A-Z]/.test(password) && // 1 uppercase
      /[0-9]/.test(password)
    ) // 1 number
  },

  /**
   * Username validator (3+ chars, alphanumeric + underscore)
   */
  username: (username: string): boolean => {
    return /^[a-zA-Z0-9_]{3,}$/.test(username)
  },

  /**
   * URL validator
   */
  url: (url: string): boolean => {
    try {
      new URL(url)
      return true
    } catch {
      return false
    }
  },
}

/**
 * Common error messages
 */
export const errorMessages = {
  emailInvalid: 'Please enter a valid email address',
  emailRequired: 'Email is required',
  passwordTooShort: 'Password must be at least 8 characters',
  passwordWeak: 'Password must contain uppercase letter and number',
  passwordRequired: 'Password required',
  passwordsMismatch: 'Passwords do not match',
  usernameTaken: 'Username is already taken',
  usernameRequired: 'Username is required',
  firstNameRequired: 'First name is required',
  lastNameRequired: 'Last name is required',
  termsRequired: 'You must agree to the terms',
}

/**
 * Common assertions
 */
export const assertions = {
  /**
   * Assert field value matches expected
   */
  fieldValue: (
    element: HTMLInputElement | HTMLSelectElement,
    expected: string,
  ) => {
    return element.value === expected
  },

  /**
   * Assert checkbox state
   */
  checkboxState: (element: HTMLInputElement, expected: boolean) => {
    return element.checked === expected
  },

  /**
   * Assert element is visible
   */
  isVisible: (element: HTMLElement | null) => {
    return element !== null
  },

  /**
   * Assert element is hidden
   */
  isHidden: (element: HTMLElement | null) => {
    return element === null
  },

  /**
   * Assert element is disabled
   */
  isDisabled: (element: HTMLButtonElement | HTMLInputElement) => {
    return element.disabled
  },

  /**
   * Assert element is enabled
   */
  isEnabled: (element: HTMLButtonElement | HTMLInputElement) => {
    return !element.disabled
  },

  /**
   * Assert element is readonly
   */
  isReadOnly: (element: HTMLInputElement) => {
    return element.readOnly
  },
}

/**
 * Common test setup patterns
 */
export const testPatterns = {
  /**
   * Simulate user filling out a form
   */
  fillForm: (
    fields: Record<string, { element: HTMLInputElement; value: string }>,
  ) => {
    Object.values(fields).forEach(({ element, value }) => {
      element.value = value
      element.dispatchEvent(new Event('change', { bubbles: true }))
    })
  },

  /**
   * Simulate user clicking a button and waiting for response
   */
  clickAndWait: async (element: HTMLElement, delay = 50) => {
    element.dispatchEvent(new MouseEvent('click', { bubbles: true }))
    await new Promise((resolve) => setTimeout(resolve, delay))
  },

  /**
   * Simulate form submission
   */
  submitForm: (form: HTMLFormElement) => {
    form.dispatchEvent(new Event('submit', { bubbles: true }))
  },
}

/**
 * DOM query helpers
 */
export const domHelpers = {
  /**
   * Get all error messages in the document
   */
  getAllErrors: (root: HTMLElement | Document = document): string[] => {
    const errorElements = root.querySelectorAll('[data-testid*="error"]')
    return Array.from(errorElements).map((el) => el.textContent || '')
  },

  /**
   * Check if form has any validation errors
   */
  hasErrors: (root: HTMLElement | Document = document): boolean => {
    const errorCount = root.querySelectorAll('[data-testid*="error"]').length
    return errorCount > 0
  },

  /**
   * Get error count
   */
  getErrorCount: (root: HTMLElement | Document = document): number => {
    return root.querySelectorAll('[data-testid*="error"]').length
  },

  /**
   * Get field by testid
   */
  getField: (
    testId: string,
    root: HTMLElement | Document = document,
  ): HTMLInputElement => {
    return root.querySelector(`[data-testid="${testId}"]`) as HTMLInputElement
  },

  /**
   * Get button by testid
   */
  getButton: (
    testId: string,
    root: HTMLElement | Document = document,
  ): HTMLButtonElement => {
    return root.querySelector(`[data-testid="${testId}"]`) as HTMLButtonElement
  },
}

/**
 * Mock data generators
 */
export const generators = {
  /**
   * Generate random email
   */
  email: (): string => {
    const randomId = Math.random().toString(36).substring(7)
    return `test-${randomId}@example.com`
  },

  /**
   * Generate random username
   */
  username: (): string => {
    const randomId = Math.random().toString(36).substring(7)
    return `user_${randomId}`
  },

  /**
   * Generate random password
   */
  password: (): string => {
    return `Pass${Math.random().toString(36).substring(2, 8)}123`
  },

  /**
   * Generate random product name
   */
  productName: (): string => {
    const adjectives = ['Premium', 'Deluxe', 'Standard', 'Basic']
    const nouns = ['Widget', 'Gadget', 'Device', 'Product']
    const adj = adjectives[Math.floor(Math.random() * adjectives.length)]
    const noun = nouns[Math.floor(Math.random() * nouns.length)]
    return `${adj} ${noun}`
  },

  /**
   * Generate random price
   */
  price: (min = 10, max = 999): number => {
    return Math.floor(Math.random() * (max - min + 1)) + min
  },
}

/**
 * Type-safe test helpers for dynamic paths
 *
 * These helpers centralize type assertions needed when working with
 * dynamic/runtime paths in tests. The casts are safe because tests
 * verify runtime behavior, not compile-time type safety.
 */
export const typeHelpers = {
  /**
   * Create a change tuple with runtime path.
   * Use when path is dynamic (e.g., template literal) or when TypeScript
   * can't infer the tuple type correctly.
   *
   * @example
   * ```typescript
   * // Instead of: [`items.${id}.qty`, 10, {}] as any
   * typeHelpers.change(`items.${id}.qty`, 10, {})
   * ```
   */
  change: <DATA extends object, META extends GenericMeta = GenericMeta>(
    path: string,
    value: unknown,
    meta: META = {} as META,
  ): ArrayOfChanges<DATA, META>[number] =>
    [path, value, meta] as ArrayOfChanges<DATA, META>[number],

  /**
   * Create a sync pair tuple.
   * Use when TypeScript infers string[] instead of [string, string].
   *
   * @example
   * ```typescript
   * // Instead of: ['firstName', 'lastName'] as any
   * typeHelpers.syncPair('firstName', 'lastName')
   * ```
   */
  syncPair: <DATA extends object>(
    path1: string,
    path2: string,
  ): SyncPair<DATA> => [path1, path2] as unknown as SyncPair<DATA>,

  /**
   * Create a flip pair tuple.
   * Use when TypeScript infers string[] instead of [string, string].
   *
   * @example
   * ```typescript
   * // Instead of: ['isActive', 'isInactive'] as any
   * typeHelpers.flipPair('isActive', 'isInactive')
   * ```
   */
  flipPair: <DATA extends object>(
    path1: string,
    path2: string,
  ): FlipPair<DATA> => [path1, path2] as unknown as FlipPair<DATA>,

  /**
   * Create an array of changes with runtime paths.
   * Use when creating multiple changes with dynamic paths.
   *
   * @example
   * ```typescript
   * typeHelpers.changes([
   *   [`items.${id}.qty`, 10],
   *   [`items.${id}.price`, 25],
   * ])
   * ```
   */
  changes: <DATA extends object, META extends GenericMeta = GenericMeta>(
    items: [string, unknown, META?][],
  ): ArrayOfChanges<DATA, META> =>
    items.map(([path, value, meta]) => [
      path,
      value,
      meta ?? ({} as META),
    ]) as ArrayOfChanges<DATA, META>,
}
