/**
 * Test helper utilities
 *
 * Common utilities and patterns used across integration tests.
 */

import { ReactNode } from 'react'

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
    return /^.{8,}$/.test(password) && // 8+ chars
           /[A-Z]/.test(password) &&  // 1 uppercase
           /[0-9]/.test(password)      // 1 number
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
  fieldValue: (element: HTMLInputElement | HTMLSelectElement, expected: string) => {
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
  fillForm: (fields: Record<string, { element: HTMLInputElement; value: string }>) => {
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
    await new Promise(resolve => setTimeout(resolve, delay))
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
    return Array.from(errorElements).map(el => el.textContent || '')
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
  getField: (testId: string, root: HTMLElement | Document = document): HTMLInputElement => {
    return root.querySelector(`[data-testid="${testId}"]`) as HTMLInputElement
  },

  /**
   * Get button by testid
   */
  getButton: (testId: string, root: HTMLElement | Document = document): HTMLButtonElement => {
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
