/**
 * Test helpers and utilities
 *
 * Reusable components and functions for integration tests
 */

import React from 'react'
import type { ReactNode } from 'react'

/**
 * Count renders of a component
 */
export function useRenderCount() {
  const renderCount = React.useRef(0)
  React.useEffect(() => {
    renderCount.current++
  })
  return renderCount.current
}

/**
 * Wrapper component that tracks renders
 */
export function RenderCounter({ children, onRender }: { children: ReactNode, onRender: () => void }) {
  React.useEffect(() => {
    onRender()
  })
  return <>{children}</>
}

/**
 * Wait for condition with timeout
 */
export async function waitForCondition(
  condition: () => boolean,
  timeout = 1000,
  interval = 50
): Promise<void> {
  const start = Date.now()

  while (!condition()) {
    if (Date.now() - start > timeout) {
      throw new Error('Timeout waiting for condition')
    }
    await new Promise(resolve => setTimeout(resolve, interval))
  }
}
