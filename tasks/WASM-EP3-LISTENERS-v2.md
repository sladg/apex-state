# WASM-EP3: Listener Orchestration (REVISED)

**Type**: Epic
**Priority**: P1
**Depends on**: WASM-EP2
**Goal**: Move listener routing and dispatch planning to WASM. WASM orchestrates which handlers to call and in what order. Handler functions stay in JS; WASM creates dispatch plans and routes results.

**Key Architectural Decisions**:
- WASM orchestrates, JS executes handlers
- Batch listener registration (single call)
- Dispatch protocol: WASM plan → JS execute → WASM route → repeat
- Listeners integrated into processChanges() return value
- 2-4 WASM↔JS round trips per pipeline run (one per depth level)

---

## WASM-018: TopicRouter in Rust

**Type**: Story | **Points**: 5 | **Priority**: P1
**Depends on**: WASM-002

### Description

Port the TopicRouter data structure from `src/utils/topicRouter.ts` to Rust. This includes topic registration, subscriber management, depth-first sorting, and pre-computed route tables. Handler functions stay in JS.

### Current implementation

- `src/utils/topicRouter.ts` — topics array (depth-sorted), topicMeta, subscribers, routes, subscriberMeta
- Handlers Map stays in JS (functions can't cross WASM boundary)

### Rust data model

```rust
struct TopicRouter {
    topics: Vec<u32>,                          // topic path IDs, deepest-first
    topic_meta: HashMap<u32, TopicMeta>,       // prefix, depth
    subscribers: HashMap<u32, Vec<u32>>,       // topic_id → [subscriber_ids]
    subscriber_meta: HashMap<u32, SubscriberMeta>,
    routes: HashMap<u32, Vec<Route>>,          // topic_id → downstream routes
}

struct TopicMeta {
    prefix: String,  // "user.profile"
    depth: u32,      // 2
}

struct SubscriberMeta {
    scope_path: String,  // "user.profile" (for scoped state resolution in JS)
    topic_path: String,  // "user.profile" (for topic matching)
}

struct Route {
    from_topic_id: u32,
    to_topic_id: u32,
}
```

### Interface

```rust
fn register_listeners_batch(listeners_json: &str) -> Result<(), JsValue>
// Input: [
//   {
//     "subscriber_id": 1,
//     "topic_path": "user.profile",
//     "scope_path": "user.profile"
//   },
//   {
//     "subscriber_id": 2,
//     "topic_path": "user",
//     "scope_path": "user"
//   }
// ]

fn unregister_listeners_batch(subscriber_ids_json: &str) -> Result<(), JsValue>
// Input: [1, 2, 3]
```

### Acceptance criteria

- [ ] Batch registration of listeners
- [ ] Topic add/remove with automatic depth-first re-sort
- [ ] Subscriber add/remove per topic
- [ ] Route table pre-computation on topology change
- [ ] SubscriberMeta (scope path, topic path) stored correctly
- [ ] Parity with JS TopicRouter for structural operations

---

## WASM-019: Dispatch plan generation

**Type**: Story | **Points**: 4 | **Priority**: P1
**Depends on**: WASM-018

### Description

Implement dispatch plan generation in Rust. Given a set of changes, WASM produces a plan describing which listeners to call, in what order, with what changes — grouped by depth level. JS executes the plan without needing to understand routing.

### Output format

```rust
struct DispatchPlan {
    levels: Vec<DispatchLevel>,  // One per depth, deepest first
}

struct DispatchLevel {
    depth: u32,
    dispatches: Vec<ListenerDispatch>,
}

struct ListenerDispatch {
    subscriber_id: u32,
    scope_path: String,           // for resolving scoped state in JS
    changes: Vec<Change>,         // relativized changes for this subscriber
}

struct Change {
    path: String,
    value_json: String,
}
```

### Algorithm

1. **Seed**: For each change, walk path upward to find matching topics
2. **Group by depth**: Organize matched topics by depth level (deepest first)
3. **Per depth level**:
   - Find all subscribers for matched topics
   - Relativize changes for each subscriber's topic prefix
   - Build ListenerDispatch entries
4. **Return plan**: JS executes handlers and sends produced changes back

### Acceptance criteria

- [ ] Dispatch plan groups listeners by depth level
- [ ] Changes are relativized per subscriber's topic prefix
- [ ] Plan ordering matches current deepest-first behavior
- [ ] Empty levels (no matching subscribers) are omitted
- [ ] Seed algorithm matches JS implementation

---

## WASM-020: Listener seeding and routing

**Type**: Story | **Points**: 5 | **Priority**: P1
**Depends on**: WASM-019

### Description

Port the seed and route algorithm from `src/_internal/pipeline/processors/listeners.ts` to Rust. This is the hot path: walking changed paths upward to find matching topics, then routing produced changes to downstream topics.

### Algorithm details

**Seeding**:
```rust
fn seed_topics(changes: &[Change]) -> HashSet<u32> {
    let mut matched_topics = HashSet::new();

    for change in changes {
        // Walk path upward: "user.profile.email" → "user.profile" → "user" → ""
        let mut path = change.path.clone();
        loop {
            if let Some(topic_id) = path_to_topic.get(&path) {
                matched_topics.insert(*topic_id);
            }

            // Move to parent
            match path.rfind('.') {
                Some(idx) => path.truncate(idx),
                None => {
                    // Check root topic ""
                    if let Some(root_id) = path_to_topic.get("") {
                        matched_topics.insert(*root_id);
                    }
                    break;
                }
            }
        }
    }

    matched_topics
}
```

**Routing**:
```rust
fn route_produced_changes(
    changes: &[Change],
    from_topic_id: u32
) -> HashMap<u32, Vec<Change>> {
    let routes = routes.get(&from_topic_id).unwrap_or(&vec![]);
    let mut routed = HashMap::new();

    for route in routes {
        routed.entry(route.to_topic_id)
            .or_insert_with(Vec::new)
            .extend(changes.clone());
    }

    routed
}
```

### WASM↔JS protocol

```
Round 1 (depth 3):
  WASM: create_dispatch_plan(changes) → DispatchPlan { level: 3, dispatches: [...] }
  JS: execute handlers for depth 3, collect produced changes
  JS: send_produced_changes(depth: 3, changes: [...])
  WASM: route to downstream topics, update accumulator

Round 2 (depth 2):
  WASM: create_dispatch_plan(accumulated_changes) → DispatchPlan { level: 2, ... }
  JS: execute handlers for depth 2
  ...

Final:
  WASM: return all accumulated changes (input + produced)
```

### Acceptance criteria

- [ ] Seed algorithm matches JS (ancestor walk, root topic)
- [ ] Within-topic accumulation works correctly
- [ ] Route dispatching routes produced changes to downstream topics
- [ ] 2-4 JS↔WASM round trips per pipeline run
- [ ] Parity with JS listener processor on all test scenarios

---

## WASM-021: processChanges() - Phase 3

**Type**: Story | **Points**: 5 | **Priority**: P1
**Depends on**: WASM-020

### Description

Extend processChanges() to orchestrate listener dispatch. This requires a multi-stage protocol where WASM creates dispatch plans, JS executes handlers, and WASM routes results to the next depth level.

### Enhanced interface

```rust
// Main entry point - same signature as Phase 2
fn process_changes(changes_json: &str) -> Result<String, JsValue>

// NEW: Listener dispatch protocol
fn create_dispatch_plan(changes_json: &str) -> Result<String, JsValue>
// Returns: DispatchPlan (depth level + subscribers + changes)

fn route_produced_changes(
    depth: u32,
    produced_changes_json: &str
) -> Result<String, JsValue>
// Routes produced changes to downstream topics
// Returns: next DispatchPlan or Done signal
```

### Algorithm

```rust
fn process_changes(changes_json: &str) -> Result<String, JsValue> {
    let mut all_changes = Vec::new();

    // 1-7: Same as Phase 2 (aggregation → sync → flip → BoolLogic)
    let mut changes = run_pipeline_phase_2(changes_json)?;
    all_changes.extend(changes.clone());

    // 8: Listener dispatch loop
    let mut current_changes = changes;
    let mut depth = get_max_depth();

    while depth > 0 {
        // Create dispatch plan for current depth
        let plan = create_dispatch_plan_internal(&current_changes, depth);

        if plan.dispatches.is_empty() {
            depth -= 1;
            continue;
        }

        // Signal JS to execute handlers (happens outside this function)
        // JS will call route_produced_changes() with results

        // For single-call version: collect all listener changes
        let listener_changes = execute_listeners_in_wasm(&plan);
        all_changes.extend(listener_changes.clone());
        current_changes = listener_changes;

        depth -= 1;
    }

    // 9: Return all changes (input + pipeline + listeners + BoolLogic)
    Ok(serialize_changes(all_changes))
}
```

### JS-side integration

```typescript
// Modified processChanges to handle listener dispatch
export const processChanges = (changes: Change[]): Change[] => {
  const changesJson = JSON.stringify(
    changes.map(c => ({ path: c.path, value_json: JSON.stringify(c.value) }))
  )

  // WASM runs pipeline and returns initial plan
  const resultJson = wasm.process_changes(changesJson)
  const result = JSON.parse(resultJson)

  // If result includes dispatch_plan, execute listener loop
  if (result.dispatch_plan) {
    let currentPlan = result.dispatch_plan

    while (currentPlan && currentPlan.levels.length > 0) {
      const producedChanges: Change[] = []

      for (const level of currentPlan.levels) {
        for (const dispatch of level.dispatches) {
          // Get handler from JS registry
          const handler = listenerHandlers.get(dispatch.subscriber_id)
          if (!handler) continue

          // Resolve scoped state
          const scopedState = resolveScopedState(dispatch.scope_path)

          // Execute handler
          const produced = handler(dispatch.changes, scopedState)
          if (produced) {
            producedChanges.push(...produced)
          }
        }
      }

      // Route produced changes to next depth level
      if (producedChanges.length > 0) {
        const routedJson = wasm.route_produced_changes(
          level.depth,
          JSON.stringify(producedChanges.map(c => ({
            path: c.path,
            value_json: JSON.stringify(c.value)
          })))
        )
        currentPlan = JSON.parse(routedJson).dispatch_plan
      } else {
        break
      }
    }
  }

  return result.changes.map((c: any) => ({
    path: c.path,
    value: JSON.parse(c.value_json)
  }))
}
```

### Acceptance criteria

- [ ] Listener dispatch integrated into processChanges()
- [ ] Multi-depth listener cascade works correctly
- [ ] Within-topic accumulation preserved
- [ ] Produced changes routed to downstream topics
- [ ] All changes returned in flat array
- [ ] Performance: dispatch protocol overhead < 10µs per level

---

## WASM-022: JS-side listener dispatch integration

**Type**: Story | **Points**: 3 | **Priority**: P1
**Depends on**: WASM-021

### Description

Update the JS-side pipeline integration to handle the listener dispatch protocol. Modify `src/_internal/pipeline/processChanges.ts` to execute handlers and feed results back to WASM.

### Key changes

**Handler registry** (stays in JS):
```typescript
// Map: subscriber_id → handler function
const listenerHandlers = new Map<number, ListenerHandler>()

export const registerListenerHandler = (
  subscriberId: number,
  handler: ListenerHandler
): void => {
  listenerHandlers.set(subscriberId, handler)
}

export const unregisterListenerHandler = (subscriberId: number): void => {
  listenerHandlers.delete(subscriberId)
}
```

**Scoped state resolution**:
```typescript
const resolveScopedState = (scopePath: string): any => {
  return scopePath === ''
    ? store.state
    : dot.get(store.state, scopePath)
}
```

**Error handling**:
```typescript
// Handler exceptions don't break the dispatch loop
try {
  const produced = handler(dispatch.changes, scopedState)
  if (produced) producedChanges.push(...produced)
} catch (error) {
  console.error(`Listener ${dispatch.subscriber_id} failed:`, error)
  // Continue to next handler
}
```

### Acceptance criteria

- [ ] Handler registry populated at listener registration time
- [ ] Handlers called with correct relativized changes and scoped state
- [ ] Produced changes routed to downstream topics via WASM
- [ ] Produced changes added to final output
- [ ] Handler exceptions don't break the dispatch loop
- [ ] Existing listener tests pass

---

## WASM-023: Phase 3 integration tests

**Type**: Story | **Points**: 3 | **Priority**: P1
**Depends on**: WASM-022

### Description

End-to-end tests for the full pipeline including WASM-driven listener dispatch.

### Test scenarios

1. **Single-depth listener**: Change triggers handler, produced changes applied
2. **Multi-depth cascade**: depth-3 listener produces change → triggers depth-1 listener
3. **Within-topic accumulation**: Subscriber B sees changes produced by subscriber A
4. **Route dispatching**: Produced changes reach downstream topics
5. **Mixed pipeline**: sync + flip + listeners + BoolLogic in one batch
6. **Listener spillage**: Verify isolation between flush cycles
7. **Parity test**: WASM dispatch vs JS dispatch on identical inputs
8. **Handler exceptions**: One handler fails, others continue
9. **Empty topics**: Topics with no subscribers are skipped
10. **Root topic**: Listeners on root topic ("") receive all changes

### Performance targets

- Dispatch plan generation: < 5µs for 10 listeners
- Full listener cascade (3 depth levels): < 50µs
- 100 changes with 20 listeners: < 200µs

### Acceptance criteria

- [ ] All scenarios pass
- [ ] Listener ordering matches current behavior (registration order within topic)
- [ ] No listener spillage between depth levels or flush cycles
- [ ] Performance targets met
- [ ] WASM dispatch matches JS dispatch behavior
- [ ] Tests run in Node (vitest)

---

## Summary

**Phase 3 Stories**:
- WASM-018: TopicRouter in Rust (batch registration)
- WASM-019: Dispatch plan generation
- WASM-020: Listener seeding and routing
- WASM-021: processChanges() - Phase 3 (listener orchestration)
- WASM-022: JS-side listener dispatch integration
- WASM-023: Integration tests

**Total Points**: 25

**Key Simplifications from Original**:
- Batch listener registration (single call)
- WASM orchestrates, JS executes (clear separation)
- Listeners integrated into processChanges() return value
- Dispatch protocol minimizes WASM↔JS round trips (2-4 per pipeline run)
- Handler functions stay in JS (can't cross WASM boundary)

**Dependencies**:
- WASM-018 builds on string interning (WASM-002)
- WASM-021 extends processChanges from WASM-015 (EP2)
- All listener work assumes pipeline is complete (EP2)

**Protocol Summary**:
```
processChanges(input_changes) {
  1. Run pipeline (aggregation → sync → flip → BoolLogic)
  2. Create dispatch plan for deepest level
  3. JS executes handlers
  4. WASM routes produced changes to next level
  5. Repeat steps 2-4 for each depth level
  6. Return all changes (input + pipeline + listeners + BoolLogic)
}
```
