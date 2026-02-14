---
created: unstaged
updated: unstaged
status: active
---

# Listener Groups with Pre-computed Relations

## Problem Statement

The current flat listener processor has two performance bottlenecks:

1. **Redundant filtering** — When multiple listeners share the same path,
   `getRelevantChanges` is called once per listener instead of once per path.
   With 20 same-path listeners, this is 20x wasted work.

2. **Blind spillage scanning** — When a listener produces changes, those changes
   are appended to `allChanges` and every subsequent listener re-scans the entire
   array. There is no routing — we don't leverage the fact that we already know
   which listeners watch which prefixes.

3. **Runtime sort** — Incoming changes are sorted by depth every time
   `processListeners` runs, even though the sort order doesn't affect prefix
   matching correctness.

```
Current: O(listeners x changes) for EVERY call

  changes ──► sort ──► for EACH listener:
                         filter ALL changes (prefix match)
                         call fn
                         push produced → allChanges
                         (next listener re-scans everything)
```

## Listener Identity

Every listener gets a unique ID derived from its path and function name.
Anonymous functions are **rejected at registration time** — listeners must be
named so they can be referenced, debugged, and traced.

### ID Format

```
  FORMAT:  {path}_{fn.name}
  ROOT:    __{fn.name}              (double underscore prefix for root)
  MINIFIED:{path}_{fn.name}_{counter}  (counter appended on collision)

  EXAMPLES:
  ─────────────────────────────────────────────────────
  path: 'a.b.c',   fn: function validateAge() {}
  ID:   'a.b.c_validateAge'

  path: 'a.b.c',   fn: function syncPrice() {}
  ID:   'a.b.c_syncPrice'

  path: null (root), fn: function rootAudit() {}
  ID:   '__rootAudit'

  path: 'a.b',     fn: () => {}
  ERROR: "Listener fn must be a named function. Got anonymous fn at path 'a.b'."
```

### Uniqueness: Dev Mode vs Production

In **development mode** (`process.env.NODE_ENV !== 'production'`):
- Anonymous functions → **throw error**
- Duplicate `path + fn.name` → **throw error**
- Forces developers to name functions and keep IDs unique

In **production mode** (minified bundles):
- Anonymous functions → **throw error** (still required)
- Duplicate `path + fn.name` → **append counter** to disambiguate
- Minification may shorten `validateAge` to `a` — counter prevents collision

```
  DEVELOPMENT MODE                      PRODUCTION MODE
  ════════════════                      ═══════════════

  register('a.b', validateAge)          register('a.b', a)
  ID: 'a.b_validateAge'  ✅            ID: 'a.b_a'      ✅

  register('a.b', validateAge)          register('a.b', a)      ← minified collision
  THROWS: duplicate ID    ❌            ID: 'a.b_a_1'    ✅     ← counter appended

  register('a.b', () => {})             register('a.b', () => {})
  THROWS: anonymous fn    ❌            THROWS: anonymous fn  ❌

  register('a.b', syncPrice)            register('a.b', a)      ← another minified 'a'
  ID: 'a.b_syncPrice'    ✅            ID: 'a.b_a_2'    ✅     ← counter increments
```

### ID Generation Algorithm

```
  generateListenerId(path, fn, existingIds):
  ───────────────────────────────────────────

  1. if (!fn.name || fn.name === 'anonymous')
       → THROW "Listener fn must be named"

  2. baseId = (path === '' ? '__' : path + '_') + fn.name

  3. if (!existingIds.has(baseId))
       → return baseId                       // unique, done

  4. if (process.env.NODE_ENV !== 'production')
       → THROW "Duplicate listener ID: {baseId}"

  5. counter = 1
     while (existingIds.has(baseId + '_' + counter))
       counter++
     → return baseId + '_' + counter         // production fallback
```

### Benefits

```
  1. DEBUGGING  — logs show 'a.b.c_validateAge produced 2 changes'
  2. TIMING    — debug.timing reports per-listener-ID performance
  3. TRACING   — spillage routes show source → target IDs
  4. CLEANUP   — unregister by ID (no object identity needed)
  5. TESTING   — assert specific listener was called / produced changes
  6. DEV GUARD — catches accidental duplicates early in development
  7. PROD SAFE — counter fallback handles minification gracefully
```

---

## Centralized Guards (`src/utils/guards.ts`)

All dev-mode validation checks should live in a single file: `src/utils/guards.ts`.
This follows the same pattern as `src/utils/hashKey.ts` which has `rejectDynamic`,
but centralizes ALL runtime guards in one place.

```
  src/utils/guards.ts
  ════════════════════

  /** Reject anonymous listener functions */
  rejectAnonymousFn(fn, path)
    → throws if !fn.name or fn.name === 'anonymous'
    → "Listener fn must be a named function. Got anonymous fn at path '{path}'."

  /** Reject duplicate listener IDs (dev mode only) */
  rejectDuplicateListenerId(id, existingIds)
    → throws if existingIds.has(id) AND process.env.NODE_ENV !== 'production'
    → "Duplicate listener ID: '{id}'. Each path+fn.name must be unique."

  /** Reject paths with dynamic hash key notation [*] (dev mode only) */
  rejectDynamic(path)
    → (moved from hashKey.ts or re-exported)
    → throws if path contains [*]

  /** Generate listener ID from path + fn.name */
  generateListenerId(path, fn, existingIds)
    → calls rejectAnonymousFn
    → calls rejectDuplicateListenerId (dev mode)
    → returns baseId or baseId + counter (production fallback)
```

### Dev-mode guard pattern

All guards that are only relevant during development should check
`process.env.NODE_ENV !== 'production'` so bundlers can tree-shake them:

```typescript
  const rejectDuplicateListenerId = (id: string, existingIds: Set<string>): void => {
    if (process.env.NODE_ENV !== 'production' && existingIds.has(id)) {
      throw new Error(`Duplicate listener ID: '${id}'`)
    }
  }
```

This applies to:
- `rejectDuplicateListenerId` — dev only (prod uses counter fallback)
- `rejectDynamic` — dev only (should be tree-shaken in prod)
- `rejectAnonymousFn` — always (even prod needs named functions for IDs)

### Migration note

`hashKey.rejectDynamic` should be updated to use the dev-mode guard pattern
and either moved to or re-exported from `src/utils/guards.ts`.

---

## Proposed Architecture

### Core Data Structures

```
ListenerGroup {
  path:       string          // 'a.b.c' or '' for root
  prefix:     string          // 'a.b.c.' or '' for root
  prefixLen:  number          // pre-computed length
  isRoot:     boolean
  depth:      number          // pre-computed getPathDepth(path)
  entries:    ListenerEntry[] // all listeners at this exact path
  ancestors:  ListenerGroup[] // groups whose path is a prefix of ours
}

ListenerEntry {
  id:     string          // 'a.b.c_validateAge'
  scope:  string | null
  fn:     ListenerFn
}
```

### Registration-Time Construction

When listeners are registered, they are validated (named function, unique ID),
grouped by path, sorted deepest-first, and ancestor relations are pre-computed.

```
                    registerListener() calls
                    ════════════════════════

  register('a.b.c.d', validateAge)    register('a.b', computeTotal)
  register('a.b.c.d', syncPrice)      register('a.b', formatDisplay)
  register('a.b.c',   enrichData)     register('',    rootAudit)
                                       register('',    rootSync)

                         │
                         ▼
              ┌──────────────────────┐
              │  1. Validate fn.name │
              │  2. Generate ID      │
              │  3. Check uniqueness │
              │  4. Group by path    │
              │  5. Sort groups      │
              │  6. Compute ancestors│
              └──────────┬───────────┘
                         │
                         ▼

  ┌──────────────────────────────────────────────────────────────────┐
  │              ListenerGroup[] (sorted by depth DESC)              │
  │                                                                  │
  │  ┌────────────────────────────────────────────────────────────┐  │
  │  │ GROUP 0                                                    │  │
  │  │ path: 'a.b.c.d'   depth: 4   prefix: 'a.b.c.d.'         │  │
  │  │ entries:                                                   │  │
  │  │   { id: 'a.b.c.d_validateAge', fn: validateAge }          │  │
  │  │   { id: 'a.b.c.d_syncPrice',   fn: syncPrice   }          │  │
  │  │ ancestors: ──────────────────────► [1, 2, 3]               │  │
  │  └────────────────────────────────────────────────────────────┘  │
  │                                                                  │
  │  ┌────────────────────────────────────────────────────────────┐  │
  │  │ GROUP 1                                                    │  │
  │  │ path: 'a.b.c'     depth: 3   prefix: 'a.b.c.'            │  │
  │  │ entries:                                                   │  │
  │  │   { id: 'a.b.c_enrichData', fn: enrichData }              │  │
  │  │ ancestors: ──────────────────────► [2, 3]                  │  │
  │  └────────────────────────────────────────────────────────────┘  │
  │                                                                  │
  │  ┌────────────────────────────────────────────────────────────┐  │
  │  │ GROUP 2                                                    │  │
  │  │ path: 'a.b'       depth: 2   prefix: 'a.b.'              │  │
  │  │ entries:                                                   │  │
  │  │   { id: 'a.b_computeTotal',  fn: computeTotal  }          │  │
  │  │   { id: 'a.b_formatDisplay', fn: formatDisplay }          │  │
  │  │ ancestors: ──────────────────────► [3]                     │  │
  │  └────────────────────────────────────────────────────────────┘  │
  │                                                                  │
  │  ┌────────────────────────────────────────────────────────────┐  │
  │  │ GROUP 3                                                    │  │
  │  │ path: ''           depth: 0   isRoot: true                │  │
  │  │ entries:                                                   │  │
  │  │   { id: '__rootAudit', fn: rootAudit }                    │  │
  │  │   { id: '__rootSync',  fn: rootSync  }                    │  │
  │  │ ancestors: ──────────────────────► []                      │  │
  │  └────────────────────────────────────────────────────────────┘  │
  └──────────────────────────────────────────────────────────────────┘

  ID REGISTRY (Map<string, ListenerEntry>):
  ──────────────────────────────────────────
  'a.b.c.d_validateAge' → entry ref
  'a.b.c.d_syncPrice'   → entry ref
  'a.b.c_enrichData'    → entry ref
  'a.b_computeTotal'    → entry ref
  'a.b_formatDisplay'   → entry ref
  '__rootAudit'         → entry ref
  '__rootSync'          → entry ref
```

### Ancestor Relation Computation

At registration time, when groups are rebuilt, ancestors are computed:

```
For each group G:
  G.ancestors = []
  for each other group A where A.depth < G.depth:
    if G.path starts with A.prefix:   // A is a path-ancestor of G
      G.ancestors.push(A)
    if A.isRoot:                       // root is ancestor of everything
      G.ancestors.push(A)

  sort G.ancestors by depth DESC       // deepest ancestor first
```

Example with paths `['a.b.c.d', 'a.b.c', 'a.b', 'x.y', '']`:

```
  RELATION MAP (pre-computed)
  ═══════════════════════════

  'a.b.c.d'  ───ancestors───►  'a.b.c'  ──►  'a.b'  ──►  ''
                                  │              │
  'a.b.c'    ───ancestors───────-┘              │
                                                │
  'a.b'      ───ancestors──────────────────────-┘──►  ''
                                                       │
  'x.y'      ───ancestors─────────────────────────────-┘──►  ''

  Note: 'a.b.c.d' is NOT an ancestor of 'x.y' (different subtree)
```

---

## Runtime Processing

### Overview

```
  processListeners(changes, store, currentState)
  ═══════════════════════════════════════════════

  incoming changes                  groups (pre-sorted, pre-related)
       │                                      │
       ▼                                      ▼
  ┌──────────┐                    ┌───────────────────────┐
  │ no sort  │                    │ iterate deepest-first │
  │ needed   │                    │ group[0] → group[N]   │
  └────┬─────┘                    └───────────┬───────────┘
       │                                      │
       └──────────────┬───────────────────────┘
                      │
                      ▼
         ┌─────────────────────────┐
         │  for each group:        │
         │                         │
         │  1. filter ONCE         │
         │  2. merge pending queue │
         │  3. call ALL listeners  │
         │  4. route produced      │
         └─────────────────────────┘
```

### Detailed Processing Flow

```
  incoming changes: [['a.b.c.d.name', 'Alice', {}],
                     ['a.b.c.d.age',  30,      {}],
                     ['x.y.value',    true,    {}]]


  ┌═══════════════════════════════════════════════════════════════════┐
  │ GROUP 0:  path:'a.b.c.d'   prefix:'a.b.c.d.'                    │
  │                                                                   │
  │  STEP 1 — Filter once for all listeners in group                 │
  │  ┌─────────────────────────────────────────────────────────┐     │
  │  │ relevant = filter(allChanges, prefix:'a.b.c.d.')        │     │
  │  │         = [['name', 'Alice', {}], ['age', 30, {}]]     │     │
  │  │                                                         │     │
  │  │ + merge group.pending (empty — first group, no routing) │     │
  │  └─────────────────────────────────────────────────────────┘     │
  │                                                                   │
  │  STEP 2 — Call each entry with SHARED relevant changes            │
  │  ┌─────────────────────────────────────────────────────────┐     │
  │  │ validateAge(relevant, state@scope)                      │     │
  │  │   ID: 'a.b.c.d_validateAge'                             │     │
  │  │   → [['a.b.c.d.flag', true, {}]]                       │     │
  │  │                                 │                        │     │
  │  │   produced change!              │                        │     │
  │  │   push to queue ◄──────────────┤                        │     │
  │  │   push to allChanges ◄─────────┤                        │     │
  │  │                                 │                        │     │
  │  │   ROUTE to downstream groups: ◄┘                        │     │
  │  │     'a.b.c.d.flag' starts with 'a.b.c.'? YES           │     │
  │  │       → push ['d.flag', true, meta] to group[1].pending │     │
  │  │     'a.b.c.d.flag' starts with 'a.b.'? YES             │     │
  │  │       → push ['c.d.flag', true, meta] to group[2].pending    │
  │  │     root: 'a.b.c.d.flag' has dots → skip               │     │
  │  │                                                         │     │
  │  │ syncPrice(relevant, state@scope) → undefined (no-op)   │     │
  │  │   ID: 'a.b.c.d_syncPrice'                              │     │
  │  │   NOTE: sees SAME relevant as validateAge               │     │
  │  │         (shared filter = no redundant work)             │     │
  │  └─────────────────────────────────────────────────────────┘     │
  │                                                                   │
  │  STEP 3 — Clear group pending for next cycle                     │
  └═══════════════════════════════════════════════════════════════════┘
                                  │
                                  ▼
  ┌═══════════════════════════════════════════════════════════════════┐
  │ GROUP 1:  path:'a.b.c'   prefix:'a.b.c.'                        │
  │                                                                   │
  │  STEP 1 — Filter + merge pending                                 │
  │  ┌─────────────────────────────────────────────────────────┐     │
  │  │ relevant = filter(allChanges, prefix:'a.b.c.')          │     │
  │  │         = [['d.name', 'Alice', {}],                     │     │
  │  │            ['d.age', 30, {}],                           │     │
  │  │            ['d.flag', true, meta]]  ← from allChanges   │     │
  │  │                                                         │     │
  │  │ + merge group.pending:                                  │     │
  │  │   [['d.flag', true, meta]]  ← pre-routed from group[0] │     │
  │  │                                                         │     │
  │  │ deduplicate = filter(allChanges) already includes it    │     │
  │  │ → pending items already in allChanges, no double-add    │     │
  │  └─────────────────────────────────────────────────────────┘     │
  │                                                                   │
  │  STEP 2 — Call entry                                              │
  │  ┌─────────────────────────────────────────────────────────┐     │
  │  │ enrichData(relevant, state@scope)                       │     │
  │  │   ID: 'a.b.c_enrichData'                                │     │
  │  │   → [['a.b.x', 5, {}]]                                 │     │
  │  │                                 │                        │     │
  │  │   ROUTE to downstream groups:   │                        │     │
  │  │     'a.b.x' starts with 'a.b.'? YES                    │     │
  │  │       → push ['x', 5, meta] to group[2].pending         │     │
  │  │     root: 'a.b.x' has dots → skip                      │     │
  │  └─────────────────────────────────────────────────────────┘     │
  └═══════════════════════════════════════════════════════════════════┘
                                  │
                                  ▼
  ┌═══════════════════════════════════════════════════════════════════┐
  │ GROUP 2:  path:'a.b'   prefix:'a.b.'                             │
  │                                                                   │
  │  STEP 1 — Filter + merge pending                                 │
  │  ┌─────────────────────────────────────────────────────────┐     │
  │  │ relevant = filter(allChanges, prefix:'a.b.')            │     │
  │  │         = [['c.d.name', …], ['c.d.age', …],            │     │
  │  │            ['c.d.flag', …], ['x', 5, meta]]            │     │
  │  │                                                         │     │
  │  │ pending from routing:                                   │     │
  │  │   ['c.d.flag', …] from group[0]                        │     │
  │  │   ['x', 5, meta]  from group[1]                        │     │
  │  │ (already in allChanges — no duplicates)                 │     │
  │  └─────────────────────────────────────────────────────────┘     │
  │                                                                   │
  │  STEP 2 — Call entries (shared relevant)                          │
  │  ┌─────────────────────────────────────────────────────────┐     │
  │  │ computeTotal(relevant, state@scope)                     │     │
  │  │   ID: 'a.b_computeTotal'                                │     │
  │  │   → ...                                                 │     │
  │  │                                                         │     │
  │  │ formatDisplay(relevant + computeTotal's produced)       │     │
  │  │   ID: 'a.b_formatDisplay'                               │     │
  │  │   → ...  (sees accumulated changes — within-group)      │     │
  │  └─────────────────────────────────────────────────────────┘     │
  └═══════════════════════════════════════════════════════════════════┘
                                  │
                                  ▼
  ┌═══════════════════════════════════════════════════════════════════┐
  │ GROUP 3:  root (path:'')                                         │
  │                                                                   │
  │  STEP 1 — Filter (top-level only: no dots in path)               │
  │  ┌─────────────────────────────────────────────────────────┐     │
  │  │ relevant = changes where !path.includes('.')            │     │
  │  │         = (any top-level changes from allChanges)       │     │
  │  └─────────────────────────────────────────────────────────┘     │
  │                                                                   │
  │  STEP 2 — Call entries (shared relevant)                         │
  │  ┌─────────────────────────────────────────────────────────┐     │
  │  │ rootAudit(relevant, state)                              │     │
  │  │   ID: '__rootAudit'                                     │     │
  │  │   → ...                                                 │     │
  │  │                                                         │     │
  │  │ rootSync(relevant + rootAudit's produced, state)        │     │
  │  │   ID: '__rootSync'                                      │     │
  │  │   → ...  (sees accumulated changes — within-group)      │     │
  │  └─────────────────────────────────────────────────────────┘     │
  └═══════════════════════════════════════════════════════════════════┘
```

### Within-Group Accumulation

Inside a group, listeners still execute sequentially. When a listener produces
changes, subsequent listeners in the SAME group see those changes:

```
  GROUP at 'a.b':  entries = [computeTotal, formatDisplay, logChanges]
  ══════════════════════════════════════════════════════════════════

  relevant = filter(allChanges)    ← computed ONCE for group

  computeTotal(relevant)
    ID: 'a.b_computeTotal'
    → [['a.b.new', 1, {}]]
    │
    ├──► push to allChanges
    ├──► push to queue
    ├──► route to downstream groups (if any match)
    └──► append relativized to groupRelevant
         groupRelevant = [...relevant, ['new', 1, meta]]

  formatDisplay(groupRelevant)
    ID: 'a.b_formatDisplay'
    → [['a.b.other', 2, {}]]
    │
    ├──► push to allChanges
    ├──► push to queue
    ├──► route to downstream groups
    └──► append to groupRelevant
         groupRelevant = [...relevant, ['new', 1, meta], ['other', 2, meta]]

  logChanges(groupRelevant)
    ID: 'a.b_logChanges'
    → undefined
    (sees accumulated changes from computeTotal + formatDisplay)
```

---

## Routing: How Produced Changes Find Their Targets

When a listener produces a change with an absolute path, we route it:

```
  produced change: ['x.y.z.w', value, meta]

  ┌──────────────────────────────────────────────────────────────┐
  │ ROUTING ALGORITHM                                            │
  │                                                              │
  │ 1. Push to queue (for applyBatch)                            │
  │ 2. Push to allChanges (catch-all for prefix scanning)        │
  │ 3. For each DOWNSTREAM group (not yet processed):            │
  │                                                              │
  │    for group in remainingGroups:                              │
  │      if group.isRoot && !changePath.includes('.'):            │
  │        group.pending.push([changePath, value, meta])          │
  │      elif changePath.startsWith(group.prefix):               │
  │        relativePath = changePath.slice(group.prefixLen)      │
  │        group.pending.push([relativePath, value, meta])        │
  │                                                              │
  │ NOTE: We only check groups that haven't been processed yet.  │
  │ Groups already processed (deeper) won't see this change      │
  │ in the current cycle — which is correct (deeper-first order).│
  └──────────────────────────────────────────────────────────────┘
```

Wait — do we actually need the ancestor pre-computation? Let me reconsider.

### Routing Strategy: Ancestors vs Remaining Groups

**Option A: Pre-computed ancestors (from producing group)**

```
  group[0] produces change → route to group[0].ancestors = [1, 2, 3]
  group[1] produces change → route to group[1].ancestors = [2, 3]
```

This works when the produced change path is UNDER the producing group's path.
But listeners can produce changes at ANY absolute path — a listener at `a.b.c`
could produce `['x.y', value, meta]`.

**Option B: Remaining-groups scan (from change path)**

```
  group[0] produces change → check remaining groups [1, 2, 3] via prefix match
  group[1] produces change → check remaining groups [2, 3] via prefix match
```

This handles arbitrary produced paths correctly. The scan is over remaining
groups (not all listeners), which is still much smaller.

**Decision: Option B** — Remaining-groups scan.

Ancestors are a useful optimization hint, but since produced changes can have
arbitrary absolute paths, we must check the actual path against each remaining
group's prefix. The win is that we scan `remaining_groups` (not
`remaining_listeners`), and each group only needs one prefix check.

```
  COST COMPARISON
  ═══════════════

  Current flat:
    produced change → appended to allChanges
    next listener → scans ALL of allChanges (including new)
    = O(all_changes x remaining_listeners)

  Grouped with routing:
    produced change → check remaining_groups via prefix
    = O(remaining_groups) per produced change
    next group → only reads its pending queue + initial filter
    = O(initial_changes) per group (computed once)
```

---

## Complexity Analysis

```
  REGISTRATION TIME
  ═════════════════

  grouping:     O(L)           L = number of listeners
  sorting:      O(G log G)     G = number of unique groups
  ancestors:    O(G^2)         pairwise prefix check (small in practice)
  total:        O(L + G^2)     one-time cost, G << L typically


  RUNTIME (per processListeners call)
  ════════════════════════════════════

  Current flat loop:
    filter:     O(C x L)       C = changes, L = listeners
    routing:    O(P x L)       P = produced changes (scan all remaining)
    total:      O((C + P) x L)

  Grouped with routing:
    filter:     O(C x G)       one filter per group, not per listener
    routing:    O(P x G)       one check per remaining group
    within-group: O(P_g x E_g)  P_g = produced in group, E_g = entries
    total:      O((C + P) x G + sum(P_g x E_g))

  With 20 same-path listeners (G=1 vs L=20):
    Current:    O(C x 20)
    Grouped:    O(C x 1)       20x improvement
```

---

## Data Flow: Absolute → Relative → Absolute

The path transformation pipeline is clearly separated:

```
  ┌────────────────────────────────────────────────────────────────┐
  │                    PATH FLOW PER GROUP                         │
  │                                                                │
  │  INCOMING (absolute)          LISTENER (relative)              │
  │  'a.b.c.name'                 'name'                           │
  │  'a.b.c.age'                  'age'                            │
  │       │                           │                            │
  │       │  slice(prefixLen)         │  listener.fn()             │
  │       │  ──────────────►          │  ──────────────►           │
  │       │                           │                            │
  │                              PRODUCED (absolute)               │
  │                              'a.b.c.flag'                      │
  │                              'x.y.other'                       │
  │                                   │                            │
  │                                   │  route to downstream       │
  │                                   │  groups (relativize per    │
  │                                   │  target group prefix)      │
  │                                   │  ──────────────►           │
  │                                   │                            │
  │                              TARGET GROUP 'a.b':               │
  │                              'c.flag' (sliced from 'a.b.c.flag')
  │                                                                │
  │                              TARGET GROUP 'x':                 │
  │                              'y.other' (sliced from 'x.y.other')
  └────────────────────────────────────────────────────────────────┘
```

---

## Files to Change

| File | Change |
|------|--------|
| `src/pipeline/processors/types.ts` | Add `ListenerGroup`, `ListenerEntry`, remove `FlatListener` |
| `src/core/types.ts` | Replace `flatListeners: FlatListener[]` with `listenerGroups: ListenerGroup[]` |
| `src/store/Provider.tsx` | Update initialization: `listenerGroups: []` |
| `src/sideEffects/prebuilts/listeners.ts` | Rewrite: ID generation, group management, ancestor computation |
| `src/pipeline/processors/listeners.ts` | Rewrite: group iteration, routing, shared filtering |
| `tests/benchmarking/listeners.bench.spec.ts` | Add grouped strategy, update fixtures |
| `tests/benchmarking/pipeline.bench.spec.ts` | Update mock store |
| `tests/pipeline/listener-*.test.tsx` | Update for named functions (no anonymous listeners) |

## Verification Criteria

1. All 722+ tests pass
2. Benchmark: grouped strategy matches or beats nested loop (old) in all scenarios
3. No `let` in processor (all `const` with mutation via push)
4. ESLint clean on all modified files
5. Stress test (20 same-path listeners): within 1.2x of old nested loop
6. Dev mode: throws on anonymous fn and duplicate ID
7. Prod mode: counter fallback on ID collision
