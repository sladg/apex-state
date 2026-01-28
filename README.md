# @sladg/apex-state

Advanced state management wrapper around Valtio with sync paths, aggregations, and side effects.

## Installation

```bash
npm install @sladg/apex-state
```

## Peer Dependencies

```bash
npm install react zod valtio
```

## Features

- **Sync Paths**: Bidirectional synchronization between state paths
- **Aggregations**: Computed state with dependency tracking
- **Side Effects**: Listeners, validators, flip paths, clear paths
- **Type-Safe**: Full TypeScript support with deep path inference
- **Functional**: Factory functions with closures, no classes
- **Performance**: Built on graphology for efficient graph operations

## Development

```bash
# Install dependencies
npm install

# Type check
npm run type-check

# Build
npm run build

# Test
npm test
npm run test:watch
```

## Architecture

This library uses:
- **Valtio** for reactive state management
- **Graphology** for dependency graph operations
- **Remeda** for functional utilities and pipe operations
- **Lodash/Deepdash** for deep object operations

## Status

🚧 Under active development - Phase 1 (Project Setup) complete

## License

MIT
