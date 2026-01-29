export type { BoolLogic } from '../types'

export interface BaseConcernProps<STATE, PATH extends string> {
  state: STATE
  path: PATH
  value: unknown
}

export interface ConcernType<
  EXTRA_PROPS = Record<string, unknown>,
  RETURN_TYPE = unknown,
> {
  name: string
  description: string
  /** Evaluated inside effect() - all state accesses are tracked */
  evaluate: (
    props: BaseConcernProps<Record<string, unknown>, string> & EXTRA_PROPS,
  ) => RETURN_TYPE
}

export interface ConcernRegistration {
  id: string
  path: string
  concernName: string
  concern: ConcernType
  config: Record<string, unknown>
  dispose: () => void
}
