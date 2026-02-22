import nodeConfig from '@sladg/eslint-config-base/node'
import jsonConfig from '@sladg/eslint-config-base/optional/json'
import { defineConfig, globalIgnores } from 'eslint/config'
import checkFile from 'eslint-plugin-check-file'

export default defineConfig([
  nodeConfig,
  jsonConfig,
  globalIgnores([
    'out-of-git',
    'node_modules',
    'dist',
    'demo',
    'rust/target',
    'rust/pkg',
    '.auto-claude',
  ]),
  {
    rules: {
      'sonarjs/no-duplicate-string': 'off',
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

  // ---------------------------------------------------------------------------
  // File naming: kebab-case for all source and test files
  // ---------------------------------------------------------------------------
  {
    plugins: {
      'check-file': checkFile,
    },
    rules: {
      // All files must use kebab-case (e.g., process-changes.ts, not processChanges.ts)
      'check-file/filename-naming-convention': [
        'error',
        { '**/*.{ts,tsx,js,jsx}': 'KEBAB_CASE' },
        { ignoreMiddleExtensions: true },
      ],
    },
  },

  // ---------------------------------------------------------------------------
  // Test files: no exports (scaffolding belongs in tests/mocks/ or tests/utils/)
  // ---------------------------------------------------------------------------
  {
    files: [
      'tests/**/*.test.ts',
      'tests/**/*.test.tsx',
      'tests/**/*.bench.ts',
      'tests/**/*.bench.spec.ts',
    ],
    rules: {
      'no-restricted-syntax': [
        'error',
        // Keep existing render destructuring ban
        {
          selector:
            'VariableDeclarator[init.callee.name=/^render/] ObjectPattern[properties.length>0]',
          message:
            'Use screen.getByTestId() instead of destructuring from render(). Import screen from @testing-library/react.',
        },
        // Ban exports from test files — scaffolding belongs in tests/mocks/ or tests/utils/
        {
          selector: 'ExportNamedDeclaration',
          message:
            'Do not export from test files. Move interfaces, constants, and helpers to tests/mocks/ or tests/utils/.',
        },
        {
          selector: 'ExportDefaultDeclaration',
          message:
            'Do not export from test files. Move interfaces, constants, and helpers to tests/mocks/ or tests/utils/.',
        },
      ],
    },
  },

  // ---------------------------------------------------------------------------
  // Test support files: only allowed in tests/mocks/ and tests/utils/
  // Non-test .ts files in other test directories are rejected
  // ---------------------------------------------------------------------------
  {
    files: ['tests/**/*.ts', 'tests/**/*.tsx'],
    ignores: [
      'tests/**/*.test.ts',
      'tests/**/*.test.tsx',
      'tests/**/*.test-d.ts',
      'tests/**/*.test-d.tsx',
      'tests/**/*.bench.ts',
      'tests/**/*.bench.spec.ts',
      'tests/mocks/**',
      'tests/utils/**',
      'tests/setup.ts',
      'tests/vitest.d.ts',
      'tests/concerns/test-utils.ts',
      'tests/integrations_v2/setup.ts',
    ],
    plugins: {
      'check-file': checkFile,
    },
    rules: {
      'check-file/folder-match-with-fex': [
        'error',
        { '*.ts': 'tests/{mocks,utils}/**', '*.tsx': 'tests/{mocks,utils}/**' },
      ],
    },
  },
])
