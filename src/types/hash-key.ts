/**
 * Hash key type for Record-based paths
 *
 * Use in template strings to construct paths through Record/HashMap properties.
 * This type represents the literal string '[*]' used for indexing into Records.
 *
 * @example
 * ```typescript
 * type Path = `users.${HASH_KEY}.name` // "users.[*].name"
 * ```
 */
export type HASH_KEY = '[*]'
