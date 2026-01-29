import nodeConfig from '@sladg/eslint-config-base/node'
import jsonConfig from '@sladg/eslint-config-base/optional/json'
import { defineConfig, globalIgnores } from 'eslint/config'

export default defineConfig([
  nodeConfig,
  jsonConfig,
  globalIgnores(['out-of-git', 'node_modules']),
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
])
