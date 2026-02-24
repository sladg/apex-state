import path from 'node:path'

import react from '@vitejs/plugin-react'
import wasm from 'vite-plugin-wasm'
import { defineConfig } from 'vitest/config'

export default defineConfig({
  plugins: [wasm(), react()],
  resolve: {
    alias: {
      '~': path.resolve(__dirname, 'src'),
    },
  },
  test: {
    globals: true,
    environment: 'happy-dom',
    setupFiles: ['./tests/setup.ts'],
    include: ['tests/**/*.test.{ts,tsx}', 'tests/**/*.test-d.{ts,tsx}'],
    exclude: [
      '**/node_modules/**',
      'tests/performance/**',
      'tests/benchmarking/**',
      'tests/reference/**',
    ],
    // Stop on first failure
    // bail: 1,
    // Ensure deterministic test ordering
    sequence: {
      shuffle: false,
      hooks: 'stack',
    },
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      include: ['src/**/*.ts', 'src/**/*.tsx'],
      exclude: ['src/**/*.d.ts'],
    },
    benchmark: {
      include: ['tests/**/*.bench.spec.ts'],
      // Note: Vitest doesn't support tolerance-based comparison yet
      // Results are tracked manually for performance regression detection
    },
  },
})
