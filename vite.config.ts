import path from 'node:path'

import react from '@vitejs/plugin-react'
import { defineConfig } from 'vitest/config'

const isPerf = process.env.VITEST_PERF === 'true'

export default defineConfig({
  plugins: [react()],
  resolve: {
    alias: {
      '~': path.resolve(__dirname, 'src'),
    },
  },
  test: {
    globals: true,
    environment: 'jsdom',
    setupFiles: ['./tests/setup.ts'],
    include: isPerf
      ? ['tests/performance/**/*.test.ts', 'tests/performance/**/*.test.tsx']
      : ['tests/**/*.test.ts', 'tests/**/*.test.tsx'],
    exclude: isPerf
      ? ['**/node_modules/**']
      : ['**/node_modules/**', 'tests/performance/**', 'tests/benchmarking/**'],
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      include: ['src/**/*.ts', 'src/**/*.tsx'],
      exclude: ['src/**/*.d.ts'],
    },
    benchmark: {
      include: ['tests/**/*.bench.spec.ts'],
      exclude: ['**/node_modules/**', '**/dist/**', '**/out-of-git/**'],
      compare: './tests/benchmarking/baseline.json',
    },
  },
})
