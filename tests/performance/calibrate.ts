/**
 * Calibration helpers for hardware-independent performance regression tests.
 *
 * A fixed "unit of work" (JSON parse/stringify cycles) establishes a reference
 * that scales with machine speed. Assertions use ratios against this reference,
 * so the same threshold passes on fast laptops and slow CI machines.
 */

/** A fixed workload that scales with machine speed */
export const calibrate = (): number => {
  const OBJ = { a: { b: { c: { d: 1 } } } }
  const ITERS = 10_000
  const start = performance.now()
  for (let i = 0; i < ITERS; i++) JSON.parse(JSON.stringify(OBJ))
  return performance.now() - start
}

/** Measure N sync iterations, return elapsed ms */
export const measure = (fn: () => void, iterations: number): number => {
  const start = performance.now()
  for (let i = 0; i < iterations; i++) fn()
  return performance.now() - start
}

/** Measure N async iterations, return elapsed ms */
export const measureAsync = async (
  fn: () => Promise<void>,
  iterations: number,
): Promise<number> => {
  const start = performance.now()
  for (let i = 0; i < iterations; i++) await fn()
  return performance.now() - start
}
