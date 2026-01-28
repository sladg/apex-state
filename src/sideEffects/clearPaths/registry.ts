/**
 * ClearPathsRegistry for managing clear path registrations
 *
 * Allows registering rules that clear specified paths when trigger paths change.
 * Supports nested change detection via the clearOnNested flag.
 */

import type { DeepKey } from '../../types'
import type { ClearPathConfig } from './types'

/**
 * Registry state for clear path management
 */
interface ClearPathsRegistryState<DATA extends object> {
  rules: Map<string, ClearPathConfig<DATA>>
  triggerIndex: Map<string, Set<string>> // triggerPath → Set<ruleId>
}

/**
 * Registry for clear path registrations
 */
export interface ClearPathsRegistry<DATA extends object> {
  /**
   * Register a clear path rule
   */
  register(config: ClearPathConfig<DATA>): void

  /**
   * Unregister a clear path rule by ID
   */
  unregister(id: string): void

  /**
   * Get all clear rules triggered by a path change
   * Includes rules with exact trigger match and rules with clearOnNested=true
   */
  getClearRulesTriggeredBy(path: DeepKey<DATA>): ClearPathConfig<DATA>[]
}

/**
 * Create a new ClearPathsRegistry instance
 */
export function createClearPathsRegistry<
  DATA extends object
>(): ClearPathsRegistry<DATA> {
  const state: ClearPathsRegistryState<DATA> = {
    rules: new Map(),
    triggerIndex: new Map(),
  }

  return {
    register(config: ClearPathConfig<DATA>): void {
      state.rules.set(config.id, config)

      const triggerKey = config.triggerPath as string
      if (!state.triggerIndex.has(triggerKey)) {
        state.triggerIndex.set(triggerKey, new Set())
      }
      state.triggerIndex.get(triggerKey)!.add(config.id)
    },

    unregister(id: string): void {
      const config = state.rules.get(id)
      if (!config) return

      const triggerKey = config.triggerPath as string
      state.triggerIndex.get(triggerKey)?.delete(id)

      state.rules.delete(id)
    },

    getClearRulesTriggeredBy(path: DeepKey<DATA>): ClearPathConfig<DATA>[] {
      const pathStr = path as string
      const triggeredRules: ClearPathConfig<DATA>[] = []
      const seenIds = new Set<string>()

      // Check exact match
      const exactIds = state.triggerIndex.get(pathStr)
      if (exactIds) {
        for (const id of exactIds) {
          const rule = state.rules.get(id)
          if (rule) {
            triggeredRules.push(rule)
            seenIds.add(id)
          }
        }
      }

      // Check if path is nested under a trigger with clearOnNested=true
      for (const [triggerPath, ids] of state.triggerIndex.entries()) {
        if (pathStr.startsWith(triggerPath + '.')) {
          for (const id of ids) {
            if (seenIds.has(id)) continue
            const rule = state.rules.get(id)
            if (rule && rule.clearOnNested) {
              triggeredRules.push(rule)
              seenIds.add(id)
            }
          }
        }
      }

      return triggeredRules
    },
  }
}
