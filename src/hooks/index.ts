/**
 * React Hooks for store access
 *
 * @internal Package-internal hooks used to implement the store instance API.
 * End users should import from the store object returned by createGenericStore instead.
 *
 * Example:
 * ```typescript
 * const store = createGenericStore<AppState>()
 * function MyComponent() {
 *   const [value] = store.useStore('path')  // ✓ Use this
 *   // NOT: useStoreContext()                 // ✗ Don't use this
 * }
 * ```
 */

export { useStoreContext } from './useStoreContext'
