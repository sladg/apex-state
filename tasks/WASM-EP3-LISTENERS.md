# WASM-EP3: Listener Routing

**Type**: Epic
**Priority**: P1
**Depends on**: WASM-EP2
**Goal**: Move TopicRouter and listener dispatch planning to WASM. Listener handler functions stay in JS; Rust orchestrates which handlers to call and in what order.

---

## WASM-016: TopicRouter in Rust

**Type**: Story | **Points**: 5 | **Priority**: P1
**Depends on**: WASM-002

### Description

Port the TopicRouter data structure from `src/utils/topicRouter.ts` to Rust. This includes topic registration, subscriber management, depth-first sorting, and pre-computed route tables.

### Current implementation

- `src/utils/topicRouter.ts` — topics array (depth-sorted), topicMeta, subscribers, routes, subscriberMeta, handlers

### Rust data model

```rust
struct TopicRouter {
    topics: Vec<u32>,                          // topic path IDs, deepest-first
    topic_meta: HashMap<u32, TopicMeta>,       // prefix, depth
    subscribers: HashMap<u32, Vec<u32>>,       // topic_id → [subscriber_ids]
    subscriber_meta: HashMap<u32, SubscriberMeta>,
    routes: HashMap<u32, Vec<Route>>,          // topic_id → downstream routes
}
// handlers Map stays in JS (handler functions can't cross to WASM)
```

### Acceptance criteria

- [ ] Topic add/remove with automatic depth-first re-sort
- [ ] Subscriber add/remove per topic
- [ ] Route table pre-computation on topology change
- [ ] SubscriberMeta (scope path, topic path) stored correctly
- [ ] Parity with JS TopicRouter for structural operations

---

## WASM-017: Dispatch plan generation

**Type**: Story | **Points**: 3 | **Priority**: P1
**Depends on**: WASM-016

### Description

Implement dispatch plan generation in Rust. Given a set of changes, WASM produces a plan describing which listeners to call, in what order, with what changes — grouped by depth level. JS executes the plan without needing to understand routing.

### Output format

```rust
struct DispatchPlan {
    // One entry per depth level, deepest first
    levels: Vec<DispatchLevel>,
}

struct DispatchLevel {
    depth: u32,
    dispatches: Vec<ListenerDispatch>,
}

struct ListenerDispatch {
    subscriber_id: u32,
    scope_path_id: u32,           // for resolving scoped state in JS
    changes: Vec<Change>,         // relativized changes for this subscriber
}
```

### Acceptance criteria

- [ ] Dispatch plan groups listeners by depth level
- [ ] Changes are relativized per subscriber's topic prefix
- [ ] Plan ordering matches current deepest-first behavior
- [ ] Empty levels (no matching subscribers) are omitted

---

## WASM-018: Listener seeding and routing in Rust

**Type**: Story | **Points**: 5 | **Priority**: P1
**Depends on**: WASM-017

### Description

Port the seed and route algorithm from `src/_internal/pipeline/processors/listeners.ts` to Rust. This is the hot path: walking changed paths upward to find matching topics, then routing produced changes to downstream topics.

### Current algorithm

1. **Seed**: For each change, walk path upward via `lastIndexOf('.')` → match to topics
2. **Iterate**: Process topics deepest-first, call subscribers, collect produced changes
3. **Within-topic accumulation**: Produced changes visible to subsequent subscribers in same topic
4. **Route dispatch**: Produced changes routed to downstream topics via pre-computed routes

### WASM boundary

- Seed + routing logic runs entirely in Rust
- Subscriber invocation requires crossing to JS (handlers are JS functions)
- Per depth level: WASM produces dispatch plan → JS executes handlers → JS sends produced changes back → WASM routes to next level

### Acceptance criteria

- [ ] Seed algorithm matches JS implementation (ancestor walk, root topic handling)
- [ ] Within-topic accumulation works correctly
- [ ] Route dispatching routes produced changes to downstream topics
- [ ] 2-4 JS↔WASM round trips per pipeline run (one per depth level)
- [ ] Parity with JS listener processor on all existing test scenarios

---

## WASM-019: JS-side listener dispatch loop

**Type**: Story | **Points**: 3 | **Priority**: P1
**Depends on**: WASM-018

### Description

Implement the JS-side dispatch loop that executes listener handlers based on WASM's dispatch plan. For each depth level, JS calls the handlers, collects produced changes, and sends them back to WASM for routing.

### Flow

```
for each depth level in dispatch plan:
  for each (subscriberId, changes) in level:
    scopedState = resolveScopedState(subscriberId)
    produced = handlerMap[subscriberId](changes, scopedState)
    if produced:
      feed back to WASM for downstream routing + queue for applyBatch
```

### Key files

- `src/_internal/pipeline/processChanges.ts` — integrate dispatch loop after WASM pipeline
- Handler map stays in JS (populated at listener registration time)

### Acceptance criteria

- [ ] Handlers called with correct relativized changes and scoped state
- [ ] Produced changes routed to downstream topics via WASM
- [ ] Produced changes added to applyBatch queue
- [ ] Handler exceptions don't break the dispatch loop
- [ ] Existing listener tests pass

---

## WASM-020: Phase 3 integration tests

**Type**: Story | **Points**: 3 | **Priority**: P1
**Depends on**: WASM-019

### Description

End-to-end tests for the full pipeline including WASM-driven listener dispatch.

### Test scenarios

1. Single-depth listener: change triggers handler, produced changes applied
2. Multi-depth cascade: depth-3 listener produces change → triggers depth-1 listener
3. Within-topic accumulation: subscriber B sees changes produced by subscriber A
4. Route dispatching: produced changes reach downstream topics
5. Mixed pipeline: sync + flip + listeners in one batch
6. Listener spillage: verify isolation between flush cycles
7. Parity test: WASM dispatch vs JS dispatch on identical inputs

### Acceptance criteria

- [ ] All scenarios pass
- [ ] Listener ordering matches current behavior (registration order within topic)
- [ ] No listener spillage between depth levels or flush cycles
- [ ] Performance: dispatch plan overhead < 5µs for 10 listeners
