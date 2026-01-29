import react from '@vitejs/plugin-react'
import { defineConfig } from 'vitest/config'

export default defineConfig({
  plugins: [react()],
  test: {
    globals: true,
    environment: 'jsdom',
    setupFiles: ['./tests/setup.ts'],
    include: ['tests/**/*.test.ts', 'tests/**/*.test.tsx'],
    exclude: ['**/node_modules/**', '**/dist/**', '**/out-of-git/**'],
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
    },
  },
  benchmark: {
    include: ['tests/benchmarking/**/*.bench.spec.ts'],
    exclude: ['**/node_modules/**', '**/dist/**', '**/out-of-git/**'],
    // Compare against baseline file to detect performance regressions
    compare: './tests/benchmarking/baseline.json',
  },
})
