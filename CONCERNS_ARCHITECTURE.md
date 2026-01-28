# Concerns Architecture - Complete Specification

## Overview

A unified, config-driven state management system for complex trading applications built on valtio. Everything is declarative - concerns, actions, shortcuts, and side-effects are defined in configuration.

---

## Core Concepts

### 1. Data Store
Your application state using valtio proxy:

```typescript
const data = {
  products: {
    'leg-uuid-1': {
      type: 'call',
      strike: 100,
      expiry: '2024-12-31',
      premium: null,
      status: 'active'
    },
    'leg-uuid-2': {
      type: 'put',
      strike: 105,
      expiry: '2024-12-31',
      premium: null,
      status: 'locked'
    }
  },

  market: {
    spot: 1.10,
    volatility: 0.15
  },

  order: {
    quantity: 1000,
    side: 'buy'
  },

  // External data from GraphQL/APIs
  external: {
    marketData: null,    // synced from Apollo
    userLimits: null     // synced from Apollo
  },

  // Error storage
  _errors: {}
}
```

### 2. Path Templates with Variables

Use `$VARIABLES` for product-specific paths that get resolved at runtime:

```typescript
// Path variable registry (one per product/deal)
const pathVariables = {
  $LEG_ONE: 'leg-uuid-1',
  $LEG_TWO: 'leg-uuid-2'
}

// Paths in config use variables
'products.$LEG_ONE.strike'  // → resolves to 'products.leg-uuid-1.strike'
'products.$LEG_TWO.strike'  // → resolves to 'products.leg-uuid-2.strike'
```

### 3. Self-Reference with $

Within a concern definition, `$` refers to the current path:

```typescript
concerns: {
  'products.$LEG_ONE.strike': {
    disabled: {
      IS_EQUAL: ['products.$LEG_ONE.status', 'locked']
    }
  }
}

// $ resolves to: 'products.$LEG_ONE.strike'
// $.parent resolves to: 'products.$LEG_ONE'
```

---

## Concern Types

Concerns are derived values that depend on state. All concerns are defined in static configuration.

### **disabled** (boolean)
When field cannot be edited (affects both UI and data operations like aggregations).

```typescript
concerns: {
  'products.$LEG_ONE.strike': {
    disabled: {
      OR: [
        { IS_EQUAL: ['products.$LEG_ONE.status', 'locked'] },
        { IS_EQUAL: ['session.locked', true] }
      ]
    }
  }
}
```

**BoolLogic Operators:**
- `IS_EQUAL: [path, value]` - path value equals value
- `EXISTS: path` - path has non-null/undefined value
- `IS_EMPTY: path` - object is {}, array is [], or null/undefined
- `AND: [logic, logic, ...]` - all conditions true
- `OR: [logic, logic, ...]` - any condition true
- `NOT: logic` - negation
- `GT/LT/GTE/LTE: [path, value]` - numeric comparisons
- `IN: [path, [values]]` - value in array
- `HAS_CONCERN: [path, concernType]` - path has errors/warnings/etc

### **errors** (string[])
Validation errors stored in `_errors.*` paths.

```typescript
concerns: {
  'products.$LEG_ONE.strike': {
    errors: [
      {
        id: 'range',
        schema: z.number().min(0).max(10000),
        errorPath: '_errors.products.$LEG_ONE.strike'
      },
      {
        id: 'cross-leg',
        schema: z.number().refine((strikeOne) => {
          const strikeTwo = store.get('products.$LEG_TWO.strike')
          return strikeOne < strikeTwo
        }, { message: "Leg 1 strike must be less than Leg 2" }),
        errorPath: '_errors.products.$LEG_ONE.strike'
      }
    ]
  }
}
```

Errors are:
- Evaluated on every change to the path
- Stored in state at `errorPath`
- Multiple validators can register errors for same path (keyed by `id`)
- Affect operations: aggregations skip paths with errors

### **warnings** (string[])
Advisory messages that don't block operations.

```typescript
concerns: {
  'order.quantity': {
    warnings: [
      {
        id: 'risk-limit',
        condition: {
          GT: ['order.quantity', 'external.userLimits.maxQuantity']
        },
        message: "Quantity exceeds recommended limit"
      },
      {
        id: 'market-closed',
        condition: {
          IS_EQUAL: ['market.status', 'closed']
        },
        message: "Market is closed"
      }
    ]
  }
}
```

Warnings:
- Don't block data operations
- Can depend on external data
- Shown in UI as advisory badges

### **tooltip** (string)
Help text with string interpolation.

```typescript
concerns: {
  'products.$LEG_ONE.strike': {
    tooltip: "Strike: {products.$LEG_ONE.strike}, Status: {products.$LEG_ONE.status}"
  },

  'products.$LEG_ONE.premium': {
    tooltip: "Premium calculated at spot {market.spot} with vol {market.volatility}"
  }
}
```

Interpolation:
- `{path.to.value}` - replaced with value at path
- All paths resolved from current state
- Type-safe if using helper functions

### **options** (any[])
Dropdown/selector choices.

```typescript
concerns: {
  'order.side': {
    options: {
      static: ['buy', 'sell']
    }
  },

  'order.counterparty': {
    options: {
      fromPath: 'external.counterparties',  // array in state
      filter: {
        // Optional filtering with BoolLogic
        IN: ['_current.region', ['EMEA', 'APAC']]
      }
    }
  }
}
```

Options can be:
- `static: [...]` - hardcoded array
- `fromPath: 'path.to.array'` - read from state
- `filter: BoolLogic` - filter with `_current` as item being evaluated

### **computed** (any)
Async computed values (like premium from pricing engine).

```typescript
concerns: {
  'products.$LEG_ONE.premium': {
    computed: {
      evaluate: async (context) => {
        const { strike, expiry } = context.value
        const spot = context.state.market.spot
        const vol = context.state.market.volatility

        return await pricingEngine.calculate({
          strike,
          expiry,
          spot,
          volatility: vol
        })
      },
      dependencies: ['market.spot', 'market.volatility'],
      debounce: 500,
      staleWhileRevalidate: true
    }
  }
}
```

Computed concerns:
- Can be async
- Declare dependencies (invalidate when those change)
- Debouncing for performance
- Stale-while-revalidate pattern

### **Custom Concerns**

Users can register custom concern types:

```typescript
registerConcernType('highlight', {
  evaluate: (definition, context) => {
    if (context.value > definition.threshold) {
      return definition.highColor
    }
    return definition.lowColor
  }
})

// Use in config
concerns: {
  'products.$LEG_ONE.profit': {
    highlight: {
      threshold: 1000,
      highColor: 'green',
      lowColor: 'red'
    }
  }
}
```

---

## Concern Evaluation Context

All concern evaluators receive a context object:

```typescript
type EvaluationContext<Data> = {
  // Current state
  state: Data
  value: any                      // value at current path
  path: string                    // current path being evaluated

  // For async selectors with user input
  temporaryValue?: any            // uncommitted value (from UI)

  // External data (from GraphQL/APIs)
  external: Map<string, any>

  // Previous result (for memoization)
  previousResult?: any
}
```

**Key principle:** Concerns get everything from `context`, not from closures or outside variables.

---

## Actions

Actions are things users can do. Separated from how they're triggered (shortcuts, buttons, API).

```typescript
actions: {
  incrementStrike: {
    id: 'incrementStrike',
    scope: 'products.$LEG_ONE.strike',  // specific path

    execute: (context) => {
      const { path, value, store } = context
      store.set(path, value + 1)
    },

    enabled: {
      NOT: { IS_EQUAL: ['products.$LEG_ONE.status', 'locked'] }
    },

    label: 'Increment Strike',
    description: 'Increase strike by 1'
  },

  clearProduct: {
    id: 'clearProduct',
    scope: 'products.$LEG_ONE',  // parent path

    execute: (context) => {
      const { path, store } = context
      const defaultValue = store.getDefaultValue(path)
      store.set(path, defaultValue)
    },

    label: 'Clear Product'
  },

  submitOrder: {
    id: 'submitOrder',
    scope: 'store',  // global action

    execute: async (context) => {
      const { state, store } = context

      // Check for errors
      const hasErrors = Object.keys(state._errors).some(
        key => state._errors[key].length > 0
      )

      if (hasErrors) {
        store.set('ui.notification', 'Cannot submit: validation errors')
        return
      }

      await api.submitOrder(state.order)
      store.set('ui.notification', 'Order submitted')
    },

    enabled: {
      NOT: { HAS_CONCERN: ['order', 'errors'] }
    }
  }
}
```

**Action Context:**
```typescript
type ActionContext = {
  path: string              // path where action triggered
  value: any                // current value at path
  state: Data               // full state snapshot
  store: StoreAPI           // store methods (set, get, etc.)
  event?: Event             // triggering event (keyboard, mouse)
}
```

---

## Shortcuts

Keyboard bindings that trigger actions.

```typescript
shortcuts: {
  // Field-specific
  'products.$LEG_ONE.strike': [
    { key: 'ctrl+up', actionId: 'incrementStrike' },
    { key: 'ctrl+down', actionId: 'decrementStrike' }
  ],

  // Product-level (any field in this product)
  'products.$LEG_ONE': [
    { key: 'ctrl+d', actionId: 'clearProduct' }
  ],

  // Global
  'store': [
    {
      key: 'ctrl+s',
      actionId: 'submitOrder',
      preventDefault: true
    },
    {
      key: 'escape',
      actionId: 'cancelEdit',
      when: { IS_EQUAL: ['ui.editMode', true] }  // conditional
    }
  ]
}
```

**Shortcut precedence:** field-specific > product-level > global

**Key format:** `'ctrl+s'`, `'alt+shift+p'`, `'escape'`

---

## Side-Effects

Automatic state synchronization behaviors.

### **syncPaths**
Keep two paths in sync (bidirectional).

```typescript
sideEffects: {
  syncPaths: [
    ['order.price', 'display.currentPrice']
  ]
}
```

When `order.price` changes → `display.currentPrice` updates
When `display.currentPrice` changes → `order.price` updates

### **flipPaths**
Keep two boolean/enum paths opposite.

```typescript
sideEffects: {
  flipPaths: [
    ['ui.showBuyPanel', 'ui.showSellPanel']
  ]
}
```

When `ui.showBuyPanel` = true → `ui.showSellPanel` = false

### **aggregations**
Multi-field sync with conflict resolution.

```typescript
sideEffects: {
  aggregations: [
    {
      target: 'aggregated.strike',
      sources: [
        'products.$LEG_ONE.strike',
        'products.$LEG_TWO.strike',
        'products.$LEG_THREE.strike'
      ]
    }
  ]
}
```

**Behavior:**
- If all sources have same value → target = that value
- If sources differ → target = undefined
- When target changes → all **unlocked, error-free** sources update
- Respects `disabled` concern and `errors` concern

### **clearPaths**
Clear fields when trigger changes.

```typescript
sideEffects: {
  clearPaths: [
    {
      trigger: 'order.side',
      clear: ['order.limitPrice', 'order.stopPrice']
    }
  ]
}
```

When `order.side` changes → clear the specified paths

---

## External Data Integration (GraphQL/Apollo)

External data lives in `external.*` slice, synced by React hooks.

```typescript
// Store config
const storeConfig = {
  data: {
    // ... your state
    external: {
      marketData: null,
      userLimits: null,
      counterparties: []
    }
  },

  concerns: {
    'products.$LEG_ONE.premium': {
      computed: {
        evaluate: async (context) => {
          const marketData = context.state.external.marketData
          if (!marketData) return null

          return calculatePremium(context.value, marketData)
        },
        dependencies: ['external.marketData']  // re-eval when this changes
      }
    },

    'order.quantity': {
      warnings: [
        {
          condition: {
            GT: ['order.quantity', 'external.userLimits.maxQuantity']
          },
          message: "Exceeds limit"
        }
      ]
    }
  }
}

// React component syncs Apollo → Store
function ApolloSync() {
  const { data: marketData } = useQuery(GET_MARKET_DATA, {
    pollInterval: 5000
  })

  useEffect(() => {
    if (marketData) {
      store.set('external.marketData', marketData)
    }
  }, [marketData])

  return null
}

// In your app
<ApolloProvider client={apolloClient}>
  <StoreProvider>
    <ApolloSync />
    <TradingUI />
  </StoreProvider>
</ApolloProvider>
```

**Key principle:** Apollo manages fetching/caching, store just reads the data from `external.*` paths.

---

## Lazy Loading with Temporary Values

For selectors that search based on user input:

```typescript
concerns: {
  'order.counterparty': {
    options: {
      evaluate: async (context) => {
        // Use temporary value (what user is typing)
        const searchTerm = context.temporaryValue || context.value || ''

        if (searchTerm.length < 2) return []

        // Fetch from API based on search
        return await api.searchCounterparties(searchTerm)
      },
      dependsOnTemporary: true,  // re-evaluate when temp value changes
      debounce: 300
    }
  }
}

// React hook
const useFieldWithLazyOptions = (path) => {
  const [value, setValue] = useStore(path)
  const [tempValue, setTempValue] = useState(value)

  // Get options based on temporary value
  const options = useConcern(path, 'options', { temporaryValue: tempValue })

  return {
    value: tempValue,
    onChange: setTempValue,       // update temp (doesn't commit)
    onCommit: () => setValue(tempValue),  // commit to store
    options
  }
}
```

---

## Type Safety

### Path Type Safety with Helpers

```typescript
// Product-specific typed path builder
const createLegPaths = (variables: PathVariables) => ({
  strike: (leg: '$LEG_ONE' | '$LEG_TWO') =>
    `products.${variables[leg]}.strike` as const,

  expiry: (leg: '$LEG_ONE' | '$LEG_TWO') =>
    `products.${variables[leg]}.expiry` as const,

  status: (leg: '$LEG_ONE' | '$LEG_TWO') =>
    `products.${variables[leg]}.status` as const
})

// Usage
const paths = createLegPaths({
  $LEG_ONE: 'leg-uuid-1',
  $LEG_TWO: 'leg-uuid-2'
})

const config = {
  concerns: {
    [paths.strike('$LEG_ONE')]: {  // TypeScript validates this exists
      disabled: {
        IS_EQUAL: [paths.status('$LEG_ONE'), 'locked']
      }
    }
  }
}
```

### Runtime Validation

```typescript
// Store validates paths on registration
const store = createGenericStore(config, {
  validatePaths: true  // throws error if path doesn't exist in schema
})

// Error at initialization:
// "Invalid path: products.leg-uuid-1.typo does not exist in schema"
```

---

## Store API

```typescript
const store = createGenericStore(config)

// State access
store.get(path)                           // get value
store.set(path, value)                    // set value
store.getSnapshot()                       // non-reactive snapshot

// Concerns
store.getConcern(path, 'disabled')        // get concern value
store.isDisabled(path)                    // shorthand
store.getErrors(path)                     // shorthand
store.getWarnings(path)                   // shorthand
store.getTooltip(path)                    // shorthand
store.getOptions(path)                    // shorthand

// Actions
store.getActions(path)                    // list available actions
store.executeAction(actionId, path)       // trigger action

// Shortcuts
store.setFocusedPath(path)                // for keyboard shortcuts

// Path resolution
store.resolvePath('products.$LEG_ONE.strike')  // → 'products.leg-uuid-1.strike'

// Debugging
store.debug.events                        // event stream
store.debug.dumpState()                   // log state
store.debug.tracePath(path)               // all events for path
```

---

## React Hooks

```typescript
// Basic state hook
const [value, setValue] = useStore('order.quantity')

// All concerns for a path
const field = useFieldConcerns('products.$LEG_ONE.strike')
// Returns: { value, setValue, disabled, errors, warnings, tooltip, options }

// Single concern
const isDisabled = useConcern('products.$LEG_ONE.strike', 'disabled')

// Async concern with loading state
const { value: premium, loading, error } = useConcernAsync(
  'products.$LEG_ONE.premium',
  'computed'
)

// Available actions
const actions = useFieldActions('products.$LEG_ONE.strike')

// Temporary value for lazy loading
const { value, onChange, onCommit, options, optionsLoading } =
  useFieldWithLazyOptions('order.counterparty')
```

---

## Debug Tooling

### Redux DevTools (Built-in)

```typescript
import { devtools } from 'valtio/utils'

const store = createGenericStore(config)

// Enable devtools
devtools(store.state, { name: 'Trading Store', enabled: true })
```

Provides:
- Time-travel debugging
- State inspector
- Action log
- State diff viewer

### Event Stream (Custom)

```typescript
// Subscribe to store events
store.debug.events.subscribe((event) => {
  console.log(event)
  // { type: 'SET', path: '...', oldValue: ..., newValue: ..., timestamp: ... }
  // { type: 'SYNC_PATH', from: '...', to: '...', value: ..., timestamp: ... }
  // { type: 'CONCERN_EVALUATED', path: '...', concernType: '...', result: ..., duration: ... }
})

// Query events
store.debug.events.getByPath('order.quantity')
store.debug.events.getByType('SYNC_PATH')
store.debug.events.getInRange(startTime, endTime)
```

### Debug UI (Optional)

Simple drawer with event filtering:
- Press `Ctrl+Shift+D` to open
- Filter events by path, type, value
- View state snapshots
- Inspect concerns for any path

---

## Complete Example

```typescript
// Path variables for this strategy
const pathVars = {
  $LEG_ONE: 'uuid-call-leg',
  $LEG_TWO: 'uuid-put-leg'
}

// Helper for type-safe paths
const paths = createStrategyPaths(pathVars)

// Complete store configuration
const storeConfig = {
  data: {
    products: {
      [pathVars.$LEG_ONE]: {
        type: 'call',
        strike: 100,
        premium: null,
        status: 'active'
      },
      [pathVars.$LEG_TWO]: {
        type: 'put',
        strike: 105,
        premium: null,
        status: 'active'
      }
    },

    aggregated: {
      strike: 100
    },

    market: { spot: 102, volatility: 0.15 },

    external: {
      marketData: null,
      userLimits: null
    },

    _errors: {}
  },

  concerns: {
    [paths.strike('$LEG_ONE')]: {
      disabled: {
        IS_EQUAL: [paths.status('$LEG_ONE'), 'locked']
      },

      errors: [
        {
          id: 'range',
          schema: z.number().min(0).max(10000),
          errorPath: `_errors.${paths.strike('$LEG_ONE')}`
        }
      ],

      tooltip: `Strike for leg 1: {${paths.strike('$LEG_ONE')}}`
    },

    [paths.strike('$LEG_TWO')]: {
      disabled: {
        IS_EQUAL: [paths.status('$LEG_TWO'), 'locked']
      },

      errors: [
        {
          id: 'must-be-higher',
          schema: z.number().refine((val) => {
            const leg1Strike = store.get(paths.strike('$LEG_ONE'))
            return val > leg1Strike
          }, { message: "Must be higher than leg 1" }),
          errorPath: `_errors.${paths.strike('$LEG_TWO')}`
        }
      ]
    },

    [paths.premium('$LEG_ONE')]: {
      computed: {
        evaluate: async (context) => {
          const marketData = context.state.external.marketData
          if (!marketData) return null

          const { strike } = context.state.products[pathVars.$LEG_ONE]
          return await pricingEngine.calculate({
            strike,
            spot: marketData.spot,
            volatility: marketData.volatility
          })
        },
        dependencies: ['external.marketData', paths.strike('$LEG_ONE')],
        debounce: 500,
        staleWhileRevalidate: true
      }
    }
  },

  actions: {
    incrementStrike: {
      id: 'incrementStrike',
      scope: paths.strike('$LEG_ONE'),
      execute: ({ path, value, store }) => {
        store.set(path, value + 1)
      },
      enabled: {
        NOT: { IS_EQUAL: [paths.status('$LEG_ONE'), 'locked'] }
      }
    },

    submitStrategy: {
      id: 'submitStrategy',
      scope: 'store',
      execute: async ({ state, store }) => {
        const hasErrors = Object.keys(state._errors).length > 0
        if (hasErrors) {
          alert('Cannot submit: validation errors')
          return
        }

        await api.submitStrategy(state.products)
      }
    }
  },

  shortcuts: {
    [paths.strike('$LEG_ONE')]: [
      { key: 'ctrl+up', actionId: 'incrementStrike' }
    ],

    'store': [
      { key: 'ctrl+s', actionId: 'submitStrategy', preventDefault: true }
    ]
  },

  sideEffects: {
    syncPaths: [
      [paths.strike('$LEG_ONE'), 'display.leg1Strike']
    ],

    aggregations: [
      {
        target: 'aggregated.strike',
        sources: [
          paths.strike('$LEG_ONE'),
          paths.strike('$LEG_TWO')
        ]
      }
    ]
  }
}

// Create store
const store = createGenericStore(storeConfig, {
  pathVariables: pathVars,
  validatePaths: true
})

// Enable devtools
devtools(store.state, { name: 'Strategy Store' })

// Usage in React
function StrikeInput({ leg }) {
  const path = paths.strike(leg)
  const field = useFieldConcerns(path)

  return (
    <Input
      value={field.value}
      onChange={field.setValue}
      disabled={field.disabled}
      error={field.errors[0]}
      tooltip={field.tooltip}
    />
  )
}
```

---

## Summary

**Core Principles:**
1. ✅ Everything is declarative configuration
2. ✅ No asterisk paths - specific typed paths only
3. ✅ Concerns get data from `context`, not closures
4. ✅ Path variables (`$LEG_ONE`) for product-specific paths
5. ✅ Self-reference (`$`) in BoolLogic
6. ✅ External data in `external.*` slice
7. ✅ Functional approach (no classes)
8. ✅ Type-safe with helper functions
9. ✅ Built-in devtools (valtio + Redux DevTools)

**What This Gives You:**
- Unified system for disabled, errors, warnings, tooltips, options
- Actions separated from triggers (shortcuts, buttons, API)
- Side-effects (sync, flip, aggregate) respect concerns
- Async computed values with debouncing
- GraphQL/Apollo integration via `external.*`
- Full observability with event stream
- Type safety with typed path helpers
