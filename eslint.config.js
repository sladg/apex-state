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
    },
  },
])
