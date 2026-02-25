import type { DeepRequired } from '../types'
import type { StoreConfig } from './types'

export const DEFAULT_STORE_CONFIG: DeepRequired<StoreConfig> = {
  errorStorePath: '_errors',
  maxIterations: 100,
  debug: {
    timing: false,
    timingThreshold: 5,
    track: false,
    logPipeline: false,
    logListeners: false,
    logConcerns: false,
    devtools: false,
  },
  useLegacyImplementation: false,
}
