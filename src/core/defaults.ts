import type { StoreConfig } from '~/core/types'
import type { DeepRequired } from '~/types'

export const DEFAULT_STORE_CONFIG: DeepRequired<StoreConfig> = {
  errorStorePath: '_errors',
  maxIterations: 100,
  debug: {
    timing: false,
    timingThreshold: 5,
  },
}
