/**
 * Hash key type for Record-based paths
 *
 * Use in template strings to construct paths through Record/HashMap properties.
 * This type represents the `[${string}]` pattern used for indexing into Records.
 * Both `Record<string, V>` and `Record<Enum, V>` collapse to this pattern in DeepKey.
 *
 * @example
 * ```typescript
 * type Path = `users.${HASH_KEY}.name` // "users.[${string}].name"
 * ```
 */
export type HASH_KEY = `[${string}]`
