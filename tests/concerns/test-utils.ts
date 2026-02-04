/**
 * Test utilities for concerns benchmarking and tracking
 *
 * Provides performance measurement and evaluation tracking for P0 test suite
 */

/**
 * Performance benchmarking harness with round-trip tracking
 */
export class PerformanceBenchmark {
  private marks = new Map<string, number>()
  private measurements: { name: string; duration: number }[] = []

  start(label: string) {
    this.marks.set(label, performance.now())
  }

  end(label: string): number {
    const startTime = this.marks.get(label)
    if (!startTime) throw new Error(`No start mark for ${label}`)

    const duration = performance.now() - startTime
    this.measurements.push({ name: label, duration })
    this.marks.delete(label)

    return duration
  }

  report() {
    const summary = {
      count: this.measurements.length,
      total: this.measurements.reduce((sum, m) => sum + m.duration, 0),
      average:
        this.measurements.reduce((sum, m) => sum + m.duration, 0) /
        this.measurements.length,
      max: Math.max(...this.measurements.map((m) => m.duration)),
      min: Math.min(...this.measurements.map((m) => m.duration)),
    }

    return { measurements: this.measurements, summary }
  }

  clear() {
    this.marks.clear()
    this.measurements = []
  }
}

/**
 * Wait for effects to settle
 */
export async function waitForEffects(ms = 10): Promise<void> {
  await new Promise((resolve) => setTimeout(resolve, ms))
}

/**
 * Render tracking for React tests
 */
export function createRenderTracker() {
  const renderLog: { timestamp: number; [key: string]: any }[] = []

  return {
    log: renderLog,
    track: (data: Record<string, any>) => {
      renderLog.push({
        timestamp: performance.now(),
        ...data,
      })
    },
    clear: () => {
      renderLog.length = 0
    },
  }
}
