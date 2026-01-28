import reactConfig from '@sladg/eslint-config-base/next'
import jsonConfig from '@sladg/eslint-config-base/optional/json'
import { defineConfig, globalIgnores } from 'eslint/config'

export default defineConfig([
  reactConfig,
  jsonConfig,
  globalIgnores(['out-of-git', 'node_modules']),
  {
    rules: {
      'sonarjs/no-duplicate-string': 'off',
    },
  },
])
