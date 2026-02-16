# Test Cases Inventory

Complete listing of all test cases in the apex-state test suite, organized by directory and describe blocks.

---

## Package Setup Tests

### tests/dummy.test.ts
- **Path**: tests/dummy.test.ts

#### describe('Package Setup')
- [ ] should export VERSION constant
- [ ] should have proper test infrastructure

---

## Store Tests

### tests/store/deepAccess.test.ts
- **Path**: tests/store/deepAccess.test.ts

#### describe('Deep Access Utilities')

##### describe('deepGet')
- [ ] should get top-level property
- [ ] should get nested property
- [ ] should return undefined for missing property
- [ ] should handle deeply nested objects
- [ ] should handle objects with null values

##### describe('deepSet')
- [ ] should set top-level property
- [ ] should set nested property
- [ ] should create intermediate objects if missing
- [ ] should handle deeply nested paths
- [ ] should overwrite existing values
- [ ] should set numeric values
- [ ] should set object values

##### describe('deepHas')
- [ ] should return true for existing top-level property
- [ ] should return false for missing top-level property
- [ ] should return true for existing nested property
- [ ] should return false for missing nested property
- [ ] should handle null and undefined values
- [ ] should work with deeply nested paths

##### describe('Integration with valtio proxy')
- [ ] should work with proxy objects

### tests/store/createStore.test.tsx
- **Path**: tests/store/createStore.test.tsx

#### describe('createGenericStore')
- [ ] should create a store with Provider
- [ ] should render Provider without errors
- [ ] should accept errorStorePath prop
- [ ] should support multiple independent Provider instances
- [ ] should work with nested state objects
- [ ] should handle empty initial state
- [ ] should support custom meta type

### tests/store/provider.test.tsx
- **Path**: tests/store/provider.test.tsx

#### describe('Provider Component')
- [ ] should provide store instance via context
- [ ] should initialize with provided initial state
- [ ] should use default errorStorePath
- [ ] should use custom errorStorePath when provided
- [ ] should render children correctly
- [ ] should support nested providers with different stores
- [ ] should maintain store instance across re-renders
- [ ] should handle complex nested initial state

### tests/store/derived.test.tsx
- **Path**: tests/store/derived.test.tsx

#### describe('Derived Values')

##### describe('extractGetters')
- [ ] should extract getter properties from object
- [ ] should handle objects without getters
- [ ] should handle multiple getters

##### describe('detectGetters')
- [ ] should detect nested getters
- [ ] should return empty object for no getters

##### describe('Store with derived values')
- [ ] should initialize with getter properties
- [ ] should reactively update getter when dependency changes
- [ ] should only re-render when accessed getter dependencies change
- [ ] should support multiple getters with different dependencies
- [ ] should handle nested object getters
- [ ] should handle getters with complex logic
- [ ] should handle computed values with numeric operations
- [ ] should work without any getters
- [ ] should verify getter recalculation behavior (cache vs recompute)

---

## Type Tests

### tests/types/deepKey.test.ts
- **Path**: tests/types/deepKey.test.ts

#### describe('DeepKey')
- [ ] handles simple object
- [ ] handles nested object
- [ ] handles deep nested (5 levels)
- [ ] handles with arrays
- [ ] handles with optional properties
- [ ] handles complex nested structure
- [ ] should not accept invalid paths

### tests/types/deepValue.test.ts
- **Path**: tests/types/deepValue.test.ts

#### describe('DeepValue')
- [ ] handles top level properties
- [ ] handles nested object properties
- [ ] handles deep nested properties
- [ ] handles array types
- [ ] handles array element properties
- [ ] handles optional properties
- [ ] handles complex nested structure
- [ ] should return unknown for invalid paths
- [ ] handles real world example

### tests/types/changes.test.ts
- **Path**: tests/types/changes.test.ts

#### describe('GenericMeta')
- [ ] has all required properties
- [ ] all properties are optional
- [ ] can be extended

#### describe('ArrayOfChanges')
- [ ] handles simple object
- [ ] handles nested object
- [ ] has type safety for values
- [ ] works with custom metadata
- [ ] handles real world scenario
- [ ] accepts empty array
- [ ] preserves array methods

### tests/types/interpolation.test.ts
- **Path**: tests/types/interpolation.test.ts

#### describe('ExtractPlaceholders type')
- [ ] extracts single placeholder
- [ ] extracts multiple placeholders
- [ ] extracts nested paths
- [ ] returns never for no placeholders

#### describe('ValidatedTemplate type')
- [ ] accepts valid single path
- [ ] accepts valid multiple paths
- [ ] accepts templates without placeholders
- [ ] accepts nested paths
- [ ] accepts number paths
- [ ] accepts boolean paths
- [ ] rejects invalid paths
- [ ] rejects when any path is invalid

#### describe('extractPlaceholders')
- [ ] extracts single placeholder
- [ ] extracts multiple placeholders
- [ ] extracts nested paths
- [ ] returns empty array for no placeholders
- [ ] handles adjacent placeholders

#### describe('interpolateTemplate')
- [ ] interpolates string values
- [ ] interpolates number values
- [ ] interpolates boolean values
- [ ] interpolates multiple placeholders
- [ ] leaves invalid paths unchanged for debugging
- [ ] leaves null/undefined values unchanged
- [ ] leaves object values unchanged
- [ ] handles templates without placeholders
- [ ] handles empty template
- [ ] handles nested paths

#### describe('Multiple placeholders (up to 5)')

##### type tests
- [ ] type: validates 2 placeholders
- [ ] type: validates 3 placeholders
- [ ] type: validates 4 placeholders
- [ ] type: validates 5 placeholders
- [ ] type: validates 5 nested placeholders
- [ ] type: rejects if any of 5 is invalid

##### runtime tests
- [ ] runtime: interpolates 2 placeholders
- [ ] runtime: interpolates 3 placeholders
- [ ] runtime: interpolates 4 placeholders
- [ ] runtime: interpolates 5 placeholders
- [ ] runtime: interpolates 5 with mixed types
- [ ] runtime: extracts 5 placeholders
- [ ] runtime: partial failure leaves invalid paths visible

#### describe('Real-world scenarios')
- [ ] dynamic label with price
- [ ] validation error message
- [ ] partial interpolation shows missing paths

### tests/types/pathConfigs.test.ts
- **Path**: tests/types/pathConfigs.test.ts

#### describe('SyncPair')
- [ ] accepts two string paths as tuple
- [ ] accepts two number paths as tuple
- [ ] accepts two boolean paths as tuple
- [ ] accepts nested paths with matching string types
- [ ] accepts nested paths with matching boolean types

#### describe('FlipPair')
- [ ] accepts two boolean paths as tuple
- [ ] accepts different boolean paths
- [ ] accepts nested boolean paths

#### describe('AggregationPair')
- [ ] accepts matching number paths as tuple [target, source]
- [ ] accepts string aggregation paths

#### describe('SideEffects')
- [ ] accepts syncPaths as array of tuples
- [ ] accepts multiple sync pairs
- [ ] accepts flipPaths as array of tuples
- [ ] accepts aggregations as array of [target, source] tuples
- [ ] accepts combined side effects
- [ ] accepts empty arrays

#### describe('Union membership - explicit checks')

##### String paths union
- [ ] string union includes "name"
- [ ] string union includes "email"
- [ ] string union includes "title"
- [ ] string union excludes "age" (number)
- [ ] string union excludes "isActive" (boolean)

##### Number paths union
- [ ] number union includes "age"
- [ ] number union includes "count"
- [ ] number union excludes "name" (string)
- [ ] number union excludes "isActive" (boolean)

##### Boolean paths union
- [ ] boolean union includes "isActive"
- [ ] boolean union includes "isVisible"
- [ ] boolean union excludes "name" (string)
- [ ] boolean union excludes "age" (number)

##### Exact union equality
- [ ] string paths union equals exactly name | email | title
- [ ] number paths union equals exactly age | count
- [ ] boolean paths union equals exactly isActive | isVisible

#### describe('Union membership - nested paths')

##### Nested string paths
- [ ] nested string union includes user.profile.firstName
- [ ] nested string union includes user.profile.lastName
- [ ] nested string union includes user.settings.theme
- [ ] nested string union includes meta.title
- [ ] nested string union excludes user.age (number)
- [ ] nested string union excludes meta.count (number)

##### Nested number paths
- [ ] nested number union includes user.age
- [ ] nested number union includes meta.count
- [ ] nested number union excludes string paths

##### Nested path equality
- [ ] nested string paths equals exact union
- [ ] nested number paths equals exact union

#### describe('PathsWithSameValueAs type equality')
- [ ] resolves string paths to union of all string paths
- [ ] resolves number paths to union of all number paths
- [ ] resolves boolean paths to union of all boolean paths
- [ ] resolves nested string paths correctly
- [ ] resolves nested number paths correctly
- [ ] handles single matching path
- [ ] handles deeply nested paths with multiple levels

#### describe('Real-world scenarios')
- [ ] form with synced email fields
- [ ] toggle states with flip
- [ ] complex nested state sync
- [ ] shopping cart with aggregations

---

## Utility Tests

### tests/utils/concerns.test.ts
- **Path**: tests/utils/concerns.test.ts

#### describe('createTestStore')
- [ ] creates a working test store with concern registration
- [ ] handles multiple concerns on the same path
- [ ] properly cleans up on dispose
- [ ] handles multiple registrations with different ids

### tests/utils/react.test.tsx
- **Path**: tests/utils/react.test.tsx

#### describe('React Test Utilities')

##### describe('createTestStore')
- [ ] creates a minimal test store with proxy
- [ ] provides useConcerns function
- [ ] provides getFieldConcerns function

##### describe('mountStore')
- [ ] renders component with store Provider

##### describe('fireEvent')
- [ ] wraps change events in act()
- [ ] wraps click events in act()

##### describe('flushEffects')
- [ ] flushes async valtio updates

##### describe('assertions')
- [ ] fieldValue checks input value
- [ ] checkboxState checks checkbox
- [ ] isVisible checks element exists
- [ ] isHidden checks element is null
- [ ] isDisabled checks disabled state
- [ ] isEnabled checks enabled state
- [ ] isReadOnly checks readonly state

##### describe('domHelpers')
- [ ] getAllErrors finds error elements
- [ ] hasErrors checks if errors exist
- [ ] getErrorCount returns error count
- [ ] getField retrieves input by testid
- [ ] getButton retrieves button by testid

---

## Concerns Tests

### tests/concerns/batch-updates.test.ts
- **Path**: tests/concerns/batch-updates.test.ts

#### describe('TEST-003: Batch Updates')
- [ ] AC1: All concerns evaluate (correctness)
- [ ] AC2: Each concern evaluates at most once per update cycle
- [ ] AC4: Final state is correct after bulk update
- [ ] Performance target: < 30ms end-to-end
- [ ] Round-trip: state change → concern value propagated < 15ms

### tests/concerns/cross-field-deps.test.ts
- **Path**: tests/concerns/cross-field-deps.test.ts

#### describe('TEST-002: Cross-Field Dependency Tracking')
- [ ] AC1: Only leg-1 disabled concern recalculates when status changes
- [ ] AC2: Leg-1 validationState does NOT recalculate
- [ ] AC3: Leg-2 concerns do NOT recalculate
- [ ] AC4: Correct disabled state after change
- [ ] Performance target: < 2ms for single concern evaluation
- [ ] Round-trip: cross-field dependency change → concern value available < 15ms

### tests/concerns/selective-recalc.test.ts
- **Path**: tests/concerns/selective-recalc.test.ts

#### describe('TEST-001: Selective Re-calculation')
- [ ] AC1: Only leg-1 concerns recalculate when leg-1 strike changes
- [ ] AC2: All leg-1 concerns recalculate
- [ ] AC3: Correct values after recalculation
- [ ] Performance target: < 5ms for re-evaluation
- [ ] Round-trip: strike change → concern value available < 15ms

### tests/concerns/react-integration.test.tsx
- **Path**: tests/concerns/react-integration.test.tsx

#### describe('TEST-007: React Integration')
- [ ] AC1: Single re-render with React 18 batching
- [ ] AC2: No intermediate states visible
- [ ] AC3: Concerns reflect final state
- [ ] Performance target: < 16ms render time (60fps)
- [ ] No flashing or visual glitches during updates

---

## Integration Tests

### tests/integration/basic.test.tsx
- **Path**: tests/integration/basic.test.tsx

#### describe('Integration: Core Features Working Together')
- [ ] store with sync paths working
- [ ] multiple side effects registered simultaneously

### tests/integration/form-validation.test.tsx
- **Path**: tests/integration/form-validation.test.tsx

#### describe('Integration: Form Validation with Concerns')
- [ ] TC1.1: validates email format with Zod schema
- [ ] TC1.2: password complexity validation (partial read)

### tests/integration/error-handling.test.tsx
- **Path**: tests/integration/error-handling.test.tsx

#### describe('Integration: Error Handling & Recovery')
- [ ] TC7.1: stores validation errors in _errors field
- [ ] TC7.2: displays errors from concerns (partial read)

### tests/integration/concerns-ui.test.tsx
- **Path**: tests/integration/concerns-ui.test.tsx

#### describe('Integration: Dynamic UI State from Concerns')
- [ ] TC4.1: shows weight field only for physical products (partial read)

### tests/integration/complex-workflows.test.tsx
- **Path**: tests/integration/complex-workflows.test.tsx

#### describe('Integration: Complex Workflows - Multi-Step Wizard')
- [ ] TC6.1: validates step fields before navigation (partial read)

### tests/integration/withConcerns.test.tsx
- **Path**: tests/integration/withConcerns.test.tsx

#### describe('Integration: withConcerns')
- [ ] returns typed field store with selected concerns
- [ ] only returns selected concerns (partial read)

### tests/integration/aggregations.test.tsx
- **Path**: tests/integration/aggregations.test.tsx

#### describe('Integration: Computed Values & Aggregations')
- [ ] TC3.1: recalculates cart subtotal when item added (partial read)
- [ ] TC3.2: Change quantity → item subtotal updates (partial read)

### tests/integration/side-effects.test.tsx
- **Path**: tests/integration/side-effects.test.tsx

#### describe('Integration: Side Effects - Listeners & Validators')
- [ ] TC5.1: listener updates lastUpdated on field change (partial read)
- [ ] TC5.2: validates email format and stores errors (partial read)

### tests/integration/sync-paths.test.tsx
- **Path**: tests/integration/sync-paths.test.tsx

#### describe('Integration: Bidirectional Field Sync')
- [ ] TC2.1: updates fullName when firstName changes (partial read)
- [ ] TC2.2: updates fullName when lastName changes (partial read)

### tests/integration/pipeline-sync-flip-listeners.test.tsx
- **Path**: tests/integration/pipeline-sync-flip-listeners.test.tsx

#### describe('Pipeline integration with side-effect graphs')
- [ ] Sync paths propagating changes
- [ ] Flip paths propagating inverse boolean values
- [ ] Listeners firing with correct metadata
- [ ] Proper ordering (deepest listeners first)
- [ ] Metadata tagging (partial read)

---

## Pipeline Tests

### tests/pipeline/integration.test.tsx
- **Path**: tests/pipeline/integration.test.tsx

#### describe('Pipeline integration with useJitStore')
- [ ] applies changes through pipeline
- [ ] getState returns non-reactive snapshot (partial read)

---

## Test Statistics

### Directory Breakdown
- **Store Tests**: 4 test files, 36+ test cases
- **Type Tests**: 4 test files, 100+ test cases
- **Utility Tests**: 2 test files, 30+ test cases
- **Concerns Tests**: 4 test files, 24+ test cases
- **Integration Tests**: 9 test files, 30+ test cases
- **Pipeline Tests**: 2 test files, 10+ test cases
- **Package Setup**: 1 test file, 2 test cases

### Total Test Files: 27
### Total Test Cases: 200+

---

## Key Testing Areas

### Core Functionality
- Store creation and initialization
- Provider component and context provision
- State management and proxies
- Derived values and getters

### Type Safety
- DeepKey path generation
- DeepValue type extraction
- Path configuration validation
- Type-safe side effects

### Performance
- Batch update performance (< 30ms)
- Cross-field dependency tracking (< 2ms)
- Selective recalculation (< 5ms)
- React integration rendering (< 16ms)

### Features
- Concerns system (validation, visibility, etc.)
- Side effects (sync paths, flip paths, aggregations)
- Listeners and validators
- Error handling and recovery
- Form validation
- Dynamic UI state from concerns

### Advanced Scenarios
- Multi-step wizard forms
- Bidirectional field synchronization
- Complex nested state structures
- Cascading updates and aggregations
- React 18 batching integration
