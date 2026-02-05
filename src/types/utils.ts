/** Recursively makes all properties required, stripping undefined */
export type DeepRequired<T> = {
  [K in keyof T]-?: NonNullable<T[K]> extends object
    ? DeepRequired<NonNullable<T[K]>>
    : NonNullable<T[K]>
}

/** Recursively makes all properties optional (allows undefined values) */
export type DeepPartial<T> = {
  [K in keyof T]?: NonNullable<T[K]> extends object
    ? DeepPartial<NonNullable<T[K]>> | undefined
    : T[K] | undefined
}
