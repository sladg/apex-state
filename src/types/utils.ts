/** Recursively makes all properties required, stripping undefined */
export type DeepRequired<T> = {
  [K in keyof T]-?: NonNullable<T[K]> extends object
    ? DeepRequired<NonNullable<T[K]>>
    : NonNullable<T[K]>
}

/**
 * Recursively makes all properties optional (allows undefined values).
 *
 * Written as a conditional type so TypeScript distributes it over union members
 * automatically — null/undefined fall through to `: T` and are preserved as-is.
 * Array check comes before object so arrays are not mapped over their keys.
 */
export type DeepPartial<T> = T extends (infer U)[]
  ? DeepPartial<U>[]
  : T extends readonly (infer U)[]
    ? readonly DeepPartial<U>[]
    : T extends object
      ? { [K in keyof T]?: DeepPartial<T[K]> }
      : T
