import nodeConfig from '@sladg/eslint-config-base/node'
import jsonConfig from '@sladg/eslint-config-base/optional/json'
import { defineConfig, globalIgnores } from 'eslint/config'

export default defineConfig([
  nodeConfig,
  jsonConfig,
  globalIgnores(['out-of-git', 'node_modules']),
  {
    settings: {
      'import/resolver': {
        typescript: true,
      },
    },
    rules: {
      'sonarjs/no-duplicate-string': 'off',
      // Enforce architectural import boundaries between src/ modules
      'import/no-restricted-paths': [
        'error',
        {
          zones: [
            // 1. Public API (index.ts) must not import from _internal
            {
              target: './src/index.ts',
              from: './src/_internal',
              message:
                'Public API must not leak internals. Re-export from public modules instead.',
            },
            // 2-4. _internal must not depend on high-level modules (store, hooks, sideEffects)
            {
              target: './src/_internal',
              from: './src/store',
              message:
                'Internal code must not depend on store (higher-level module).',
            },
            {
              target: './src/_internal',
              from: './src/hooks',
              message:
                'Internal code must not depend on hooks (higher-level module).',
            },
            {
              target: './src/_internal',
              from: './src/sideEffects',
              message:
                'Internal code must not depend on sideEffects (higher-level module).',
            },
            // 5. Hooks use public APIs only, not _internal
            {
              target: './src/hooks',
              from: './src/_internal',
              message: 'Hooks must use public APIs, not _internal.',
            },
            // 6-7. Concerns are lower-level than store and hooks
            {
              target: './src/concerns',
              from: './src/store',
              message:
                'Concerns are lower-level than store — no upward dependencies.',
            },
            {
              target: './src/concerns',
              from: './src/hooks',
              message:
                'Concerns are lower-level than hooks — no upward dependencies.',
            },
            // 8. Utils are foundational — no upward dependencies
            {
              target: './src/utils',
              from: './src/store',
              message: 'Utils are foundational — no upward dependencies.',
            },
            {
              target: './src/utils',
              from: './src/hooks',
              message: 'Utils are foundational — no upward dependencies.',
            },
            {
              target: './src/utils',
              from: './src/concerns',
              message: 'Utils are foundational — no upward dependencies.',
            },
            {
              target: './src/utils',
              from: './src/sideEffects',
              message: 'Utils are foundational — no upward dependencies.',
            },
            {
              target: './src/utils',
              from: './src/_internal',
              message: 'Utils are foundational — no upward dependencies.',
            },
            // 9. Types are pure definitions — no runtime dependencies
            {
              target: './src/types',
              from: './src/store',
              message: 'Types are pure definitions — no runtime dependencies.',
            },
            {
              target: './src/types',
              from: './src/hooks',
              message: 'Types are pure definitions — no runtime dependencies.',
            },
            {
              target: './src/types',
              from: './src/concerns',
              message: 'Types are pure definitions — no runtime dependencies.',
            },
            {
              target: './src/types',
              from: './src/sideEffects',
              message: 'Types are pure definitions — no runtime dependencies.',
            },
            {
              target: './src/types',
              from: './src/_internal',
              message: 'Types are pure definitions — no runtime dependencies.',
            },
            {
              target: './src/types',
              from: './src/utils',
              message: 'Types are pure definitions — no runtime dependencies.',
            },
          ],
        },
      ],
      // Ban waitFor from @testing-library/react in favor of flushEffects
      // flushEffects is more deterministic for valtio proxy updates and React rendering
      'no-restricted-imports': [
        'error',
        {
          paths: [
            {
              name: '@testing-library/react',
              importNames: ['waitFor'],
              message:
                'Use flushEffects() from tests/utils/react instead of waitFor(). flushEffects is more deterministic for valtio proxy updates.',
            },
          ],
        },
      ],
      // Ban destructuring query methods from render() return value
      // Use screen.* instead to avoid stale references and improve test consistency
      'no-restricted-syntax': [
        'error',
        {
          selector:
            'VariableDeclarator[init.callee.name=/^render/] ObjectPattern[properties.length>0]',
          message:
            'Use screen.getByTestId() instead of destructuring from render(). Import screen from @testing-library/react.',
        },
      ],
    },
  },
])
