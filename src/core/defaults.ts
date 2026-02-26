import type { DeepRequired } from '../types'
import type { StoreConfig } from './types'

export const DEFAULT_STORE_CONFIG: DeepRequired<StoreConfig> = {
  name: 'store',
  errorStorePath: '_errors',
  maxIterations: 100,
  debug: {
    log: false,
    timing: false,
    timingThreshold: 5,
    track: false,
    devtools: false,
  },
}
