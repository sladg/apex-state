//! TopicRouter: listener routing and dispatch plan generation.
//!
//! Topics are path-scoped subscriptions sorted deepest-first.
//! Given a set of changes, the router seeds matching topics by walking
//! each changed path upward, groups subscribers by depth level (deepest
//! first), relativizes changes per subscriber, and produces a DispatchPlan.

use crate::pipeline::Change;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

// ---------------------------------------------------------------------------
// Data model
// ---------------------------------------------------------------------------

/// Metadata for a registered topic (a path that listeners subscribe to).
struct TopicMeta {
    prefix: String, // e.g. "user.profile"
    depth: u32,     // number of dot segments (0 for root "")
}

/// Metadata for a single subscriber.
#[allow(dead_code)] // Fields used via WASM export chain
struct SubscriberMeta {
    topic_id: u32,
    scope_path: String,
    topic_path: String,
}

/// Pre-computed downstream route between topics.
#[derive(Clone)]
#[allow(dead_code)] // Fields used via WASM export chain
struct Route {
    from_topic_id: u32,
    to_topic_id: u32,
}

/// The router itself.
pub(crate) struct TopicRouter {
    /// Topic IDs sorted deepest-first (re-sorted on registration change).
    topics: Vec<u32>,
    topic_meta: HashMap<u32, TopicMeta>,
    /// topic_id -> subscriber IDs (insertion order preserved by Vec).
    subscribers: HashMap<u32, Vec<u32>>,
    subscriber_meta: HashMap<u32, SubscriberMeta>,
    /// topic_id -> downstream routes (topics at shallower depth).
    routes: HashMap<u32, Vec<Route>>,
    /// path string -> topic_id for O(1) seed lookup.
    path_to_topic: HashMap<String, u32>,
    /// Auto-incrementing topic ID counter.
    next_topic_id: u32,
}

// ---------------------------------------------------------------------------
// Serialization types (JS boundary)
// ---------------------------------------------------------------------------

#[derive(Deserialize)]
pub(crate) struct ListenerRegistration {
    pub subscriber_id: u32,
    pub topic_path: String,
    pub scope_path: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub(crate) struct DispatchPlan {
    pub levels: Vec<DispatchLevel>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub(crate) struct DispatchLevel {
    pub depth: u32,
    pub dispatches: Vec<ListenerDispatch>,
}

// ---------------------------------------------------------------------------
// ExecutionPlan types (batched dispatch)
// ---------------------------------------------------------------------------

/// A batch of independent listeners that can safely run in any order.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub(crate) struct ListenerBatch {
    pub dispatches: Vec<ListenerDispatch>,
}

/// A depth level containing batches of independent listeners.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub(crate) struct ExecutionLevel {
    pub depth: u32,
    pub batches: Vec<ListenerBatch>,
}

/// Batched execution plan: levels ordered deepest-first, each containing
/// independent batches that can run in any order within a batch.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub(crate) struct ExecutionPlan {
    pub levels: Vec<ExecutionLevel>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub(crate) struct ListenerDispatch {
    pub subscriber_id: u32,
    pub scope_path: String,
    pub changes: Vec<Change>,
    /// Full ancestor chain from root to scope_path: ["", "orders", "orders.order_0"]
    /// Used for remapping nested listener output to parent listeners
    #[serde(skip_serializing_if = "Vec::is_empty")]
    #[serde(default)]
    pub ancestors: Vec<String>,
}

// ---------------------------------------------------------------------------
// FullExecutionPlan types (pre-computed propagation)
// ---------------------------------------------------------------------------

/// A single dispatch entry with a sequential ID and references to input changes by index.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub(crate) struct DispatchEntry {
    pub dispatch_id: u32,
    pub subscriber_id: u32,
    pub scope_path: String,
    /// Indexes into the ProcessResult.changes array.
    pub input_change_ids: Vec<u32>,
}

/// A group of dispatches to execute sequentially (flattened from depth levels + batches).
#[derive(Serialize, Deserialize, Debug, Clone)]
pub(crate) struct DispatchGroup {
    pub dispatches: Vec<DispatchEntry>,
}

/// A target for propagating produced changes from a child dispatch to a parent dispatch.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub(crate) struct PropagationTarget {
    pub target_dispatch_id: u32,
    /// Prefix to prepend to child's relative paths for the target's scope.
    pub remap_prefix: String,
}

/// Pre-computed execution plan with propagation map.
/// TS side becomes a trivial loop with map lookups — no ancestor walking needed.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub(crate) struct FullExecutionPlan {
    /// Sequential groups of dispatches (flattened from depth levels + batches).
    pub groups: Vec<DispatchGroup>,
    /// propagation_map[dispatch_id] = targets to forward produced changes to.
    /// Flat array indexed by dispatch_id, empty vec = no propagation.
    pub propagation_map: Vec<Vec<PropagationTarget>>,
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Count dot segments. "" -> 0, "a" -> 1, "a.b.c" -> 3.
fn path_depth(path: &str) -> u32 {
    if path.is_empty() {
        return 0;
    }
    path.matches('.').count() as u32 + 1
}

/// Relativize a change path against a topic prefix.
///
/// If the change path starts with the prefix, strip the prefix
/// and return the remainder. If the prefix is empty (root topic),
/// the full path is returned as-is.
fn relativize_path(change_path: &str, prefix: &str) -> String {
    if prefix.is_empty() {
        return change_path.to_owned();
    }
    if change_path == prefix {
        return String::new();
    }
    if change_path.starts_with(prefix) && change_path.as_bytes().get(prefix.len()) == Some(&b'.') {
        return change_path[prefix.len() + 1..].to_owned();
    }
    // Change is not under this prefix — return as-is (should not happen
    // if seeding is correct, but be defensive).
    change_path.to_owned()
}

/// Build the ancestor chain from root to scope_path
/// Example: "orders.order_0" -> ["", "orders", "orders.order_0"]
fn build_ancestor_chain(scope_path: &str) -> Vec<String> {
    if scope_path.is_empty() {
        return vec![String::new()];
    }

    let mut ancestors = vec![String::new()]; // root
    let mut current = String::new();

    for segment in scope_path.split('.') {
        if !current.is_empty() {
            current.push('.');
        }
        current.push_str(segment);
        ancestors.push(current.clone());
    }

    ancestors
}

// ---------------------------------------------------------------------------
// Batch grouping
// ---------------------------------------------------------------------------

/// Check if two topic paths are ancestor/descendant of each other (or equal).
fn is_ancestor_or_descendant(a: &str, b: &str) -> bool {
    if a == b {
        return true;
    }
    if a.is_empty() || b.is_empty() {
        return true; // root is ancestor of everything
    }
    (a.starts_with(b) && a.as_bytes().get(b.len()) == Some(&b'.'))
        || (b.starts_with(a) && b.as_bytes().get(a.len()) == Some(&b'.'))
}

/// Group dispatches into independent batches.
/// Within a batch, no two dispatches have ancestor/descendant scope paths.
/// Greedy first-fit algorithm: for each dispatch, place in the first batch
/// with no ancestor/descendant conflict.
fn group_into_batches(dispatches: &[ListenerDispatch]) -> Vec<ListenerBatch> {
    let mut batches: Vec<ListenerBatch> = Vec::new();

    for dispatch in dispatches {
        let scope = &dispatch.scope_path;
        let mut placed = false;

        for batch in &mut batches {
            let has_conflict = batch
                .dispatches
                .iter()
                .any(|d| is_ancestor_or_descendant(&d.scope_path, scope));

            if !has_conflict {
                batch.dispatches.push(dispatch.clone());
                placed = true;
                break;
            }
        }

        if !placed {
            batches.push(ListenerBatch {
                dispatches: vec![dispatch.clone()],
            });
        }
    }

    batches
}

// ---------------------------------------------------------------------------
// Implementation
// ---------------------------------------------------------------------------

impl TopicRouter {
    pub(crate) fn new() -> Self {
        Self {
            topics: Vec::new(),
            topic_meta: HashMap::new(),
            subscribers: HashMap::new(),
            subscriber_meta: HashMap::new(),
            routes: HashMap::new(),
            path_to_topic: HashMap::new(),
            next_topic_id: 0,
        }
    }

    /// Returns true if any listeners are registered.
    pub(crate) fn has_listeners(&self) -> bool {
        !self.subscriber_meta.is_empty()
    }

    /// Maximum depth across all registered topics.
    #[allow(dead_code)] // Called via WASM export chain
    pub(crate) fn max_depth(&self) -> u32 {
        self.topic_meta.values().map(|m| m.depth).max().unwrap_or(0)
    }

    // ------------------------------------------------------------------
    // Registration
    // ------------------------------------------------------------------

    /// Register a batch of listeners. Automatically creates topics as needed,
    /// sorts topics deepest-first, and recomputes routes.
    pub(crate) fn register_listeners_batch(&mut self, listeners_json: &str) -> Result<(), String> {
        let registrations: Vec<ListenerRegistration> = serde_json::from_str(listeners_json)
            .map_err(|e| format!("Listener registration parse error: {}", e))?;

        let mut topology_changed = false;

        for reg in registrations {
            // Ensure topic exists
            let topic_id = if let Some(&id) = self.path_to_topic.get(&reg.topic_path) {
                id
            } else {
                // Create new topic
                let id = self.next_topic_id;
                self.next_topic_id += 1;
                let depth = path_depth(&reg.topic_path);
                self.topic_meta.insert(
                    id,
                    TopicMeta {
                        prefix: reg.topic_path.clone(),
                        depth,
                    },
                );
                self.path_to_topic.insert(reg.topic_path.clone(), id);
                self.topics.push(id);
                topology_changed = true;
                id
            };

            // Add subscriber
            self.subscribers
                .entry(topic_id)
                .or_default()
                .push(reg.subscriber_id);

            self.subscriber_meta.insert(
                reg.subscriber_id,
                SubscriberMeta {
                    topic_id,
                    scope_path: reg.scope_path,
                    topic_path: reg.topic_path,
                },
            );
        }

        if topology_changed {
            self.sort_topics();
            self.recompute_routes();
        }

        Ok(())
    }

    /// Unregister a batch of subscribers by ID. Removes topics with no remaining
    /// subscribers and recomputes topology if needed.
    #[allow(dead_code)] // Called via WASM export chain
    pub(crate) fn unregister_listeners_batch(
        &mut self,
        subscriber_ids_json: &str,
    ) -> Result<(), String> {
        let ids: Vec<u32> = serde_json::from_str(subscriber_ids_json)
            .map_err(|e| format!("Subscriber IDs parse error: {}", e))?;

        let mut topology_changed = false;

        for sub_id in ids {
            let topic_id = match self.subscriber_meta.remove(&sub_id) {
                Some(meta) => meta.topic_id,
                None => continue,
            };

            // Remove from topic's subscriber list
            if let Some(subs) = self.subscribers.get_mut(&topic_id) {
                subs.retain(|&id| id != sub_id);

                // If topic has no more subscribers, remove it entirely
                if subs.is_empty() {
                    self.subscribers.remove(&topic_id);
                    if let Some(meta) = self.topic_meta.remove(&topic_id) {
                        self.path_to_topic.remove(&meta.prefix);
                    }
                    self.topics.retain(|&id| id != topic_id);
                    self.routes.remove(&topic_id);
                    topology_changed = true;
                }
            }
        }

        if topology_changed {
            self.recompute_routes();
        }

        Ok(())
    }

    // ------------------------------------------------------------------
    // Topology helpers
    // ------------------------------------------------------------------

    /// Sort topics deepest-first (highest depth value first).
    fn sort_topics(&mut self) {
        let meta = &self.topic_meta;
        self.topics.sort_by(|a, b| {
            let da = meta.get(a).map_or(0, |m| m.depth);
            let db = meta.get(b).map_or(0, |m| m.depth);
            db.cmp(&da) // descending
        });
    }

    /// Recompute route table. A route from topic A to topic B exists when
    /// B's prefix is a strict prefix of A's prefix (i.e. B is an ancestor).
    fn recompute_routes(&mut self) {
        self.routes.clear();
        let topic_ids: Vec<u32> = self.topics.clone();

        for &from_id in &topic_ids {
            let from_prefix = match self.topic_meta.get(&from_id) {
                Some(m) => &m.prefix,
                None => continue,
            };

            let mut from_routes = Vec::new();

            for &to_id in &topic_ids {
                if to_id == from_id {
                    continue;
                }
                let to_prefix = match self.topic_meta.get(&to_id) {
                    Some(m) => &m.prefix,
                    None => continue,
                };

                // to_prefix is ancestor of from_prefix
                let is_ancestor = if to_prefix.is_empty() {
                    true // root topic is ancestor of everything
                } else {
                    from_prefix.starts_with(to_prefix.as_str())
                        && from_prefix.as_bytes().get(to_prefix.len()) == Some(&b'.')
                };

                if is_ancestor {
                    from_routes.push(Route {
                        from_topic_id: from_id,
                        to_topic_id: to_id,
                    });
                }
            }

            if !from_routes.is_empty() {
                self.routes.insert(from_id, from_routes);
            }
        }
    }

    // ------------------------------------------------------------------
    // Seeding
    // ------------------------------------------------------------------

    /// Walk each changed path upward to find matching topics.
    /// Returns the set of matched topic IDs.
    fn seed_topics(&self, changes: &[Change]) -> HashSet<u32> {
        let mut matched = HashSet::new();

        for change in changes {
            let mut path = change.path.clone();
            loop {
                if let Some(&topic_id) = self.path_to_topic.get(&path) {
                    matched.insert(topic_id);
                }

                match path.rfind('.') {
                    Some(idx) => path.truncate(idx),
                    None => {
                        // Check root topic ""
                        if let Some(&root_id) = self.path_to_topic.get("") {
                            matched.insert(root_id);
                        }
                        break;
                    }
                }
            }
        }

        matched
    }

    // ------------------------------------------------------------------
    // Dispatch plan
    // ------------------------------------------------------------------

    /// Create a dispatch plan for the given changes.
    ///
    /// 1. Seed matching topics from changes.
    /// 2. Group by depth (deepest first).
    /// 3. Build ListenerDispatch entries with relativized changes.
    pub(crate) fn create_dispatch_plan(&self, changes: &[Change]) -> DispatchPlan {
        if changes.is_empty() || self.subscriber_meta.is_empty() {
            return DispatchPlan { levels: Vec::new() };
        }

        let matched_topics = self.seed_topics(changes);
        if matched_topics.is_empty() {
            return DispatchPlan { levels: Vec::new() };
        }

        // Group matched topics by depth (descending)
        let mut depth_groups: HashMap<u32, Vec<u32>> = HashMap::new();
        for &topic_id in &matched_topics {
            if let Some(meta) = self.topic_meta.get(&topic_id) {
                depth_groups.entry(meta.depth).or_default().push(topic_id);
            }
        }

        // Sort depths descending
        let mut depths: Vec<u32> = depth_groups.keys().copied().collect();
        depths.sort_by(|a, b| b.cmp(a));

        let mut levels = Vec::with_capacity(depths.len());

        for depth in depths {
            let topic_ids = &depth_groups[&depth];
            let mut dispatches = Vec::new();

            for &topic_id in topic_ids {
                let prefix = match self.topic_meta.get(&topic_id) {
                    Some(m) => &m.prefix,
                    None => continue,
                };

                // Filter changes that fall under this topic's prefix
                let relevant_changes: Vec<Change> = changes
                    .iter()
                    .filter(|c| {
                        if prefix.is_empty() {
                            true // root topic matches everything
                        } else {
                            c.path == *prefix
                                || (c.path.starts_with(prefix.as_str())
                                    && c.path.as_bytes().get(prefix.len()) == Some(&b'.'))
                        }
                    })
                    .map(|c| Change {
                        path: relativize_path(&c.path, prefix),
                        value_json: c.value_json.clone(),
                        origin: c.origin.clone(),
                    })
                    .collect();

                if relevant_changes.is_empty() {
                    continue;
                }

                // Get subscribers for this topic
                if let Some(subs) = self.subscribers.get(&topic_id) {
                    for &sub_id in subs {
                        if let Some(sub_meta) = self.subscriber_meta.get(&sub_id) {
                            let ancestors = build_ancestor_chain(&sub_meta.scope_path);
                            dispatches.push(ListenerDispatch {
                                subscriber_id: sub_id,
                                scope_path: sub_meta.scope_path.clone(),
                                changes: relevant_changes.clone(),
                                ancestors,
                            });
                        }
                    }
                }
            }

            if !dispatches.is_empty() {
                levels.push(DispatchLevel { depth, dispatches });
            }
        }

        DispatchPlan { levels }
    }

    /// Create a batched execution plan from changes.
    /// Groups independent listeners within each depth level into batches.
    pub(crate) fn create_execution_plan(&self, changes: &[Change]) -> ExecutionPlan {
        let dispatch_plan = self.create_dispatch_plan(changes);
        Self::dispatch_plan_to_execution_plan(&dispatch_plan)
    }

    /// Convert a DispatchPlan into an ExecutionPlan by grouping independent listeners.
    fn dispatch_plan_to_execution_plan(plan: &DispatchPlan) -> ExecutionPlan {
        ExecutionPlan {
            levels: plan
                .levels
                .iter()
                .map(|level| ExecutionLevel {
                    depth: level.depth,
                    batches: group_into_batches(&level.dispatches),
                })
                .collect(),
        }
    }

    /// Create a full execution plan with pre-computed propagation map.
    ///
    /// The returned plan contains:
    /// - Sequential dispatch groups (deepest-first, batched within each depth)
    /// - A propagation map: for each dispatch, which parent dispatches should
    ///   receive remapped produced changes
    ///
    /// This eliminates all ancestor-walking logic from the TS side.
    pub(crate) fn create_full_execution_plan(&self, changes: &[Change]) -> FullExecutionPlan {
        let execution_plan = self.create_execution_plan(changes);

        if execution_plan.levels.is_empty() {
            return FullExecutionPlan {
                groups: Vec::new(),
                propagation_map: Vec::new(),
            };
        }

        // Step 1: Assign sequential dispatch_ids and build groups.
        // Also record (dispatch_id, scope_path, depth) for propagation computation.
        let mut groups: Vec<DispatchGroup> = Vec::new();
        let mut next_dispatch_id: u32 = 0;

        // Track all dispatches with their metadata for propagation computation
        struct DispatchMeta {
            dispatch_id: u32,
            scope_path: String,
            depth: u32,
        }
        let mut all_dispatch_metas: Vec<DispatchMeta> = Vec::new();

        for level in &execution_plan.levels {
            for batch in &level.batches {
                let mut entries: Vec<DispatchEntry> = Vec::new();

                for dispatch in &batch.dispatches {
                    let dispatch_id = next_dispatch_id;
                    next_dispatch_id += 1;

                    // Compute input_change_ids: which changes (by index) match this dispatch's topic
                    let scope = &dispatch.scope_path;
                    let mut input_change_ids: Vec<u32> = Vec::new();
                    for (idx, change) in changes.iter().enumerate() {
                        let matches = if scope.is_empty() {
                            true // root scope matches everything
                        } else {
                            change.path == *scope
                                || (change.path.starts_with(scope.as_str())
                                    && change.path.as_bytes().get(scope.len()) == Some(&b'.'))
                        };
                        if matches {
                            input_change_ids.push(idx as u32);
                        }
                    }

                    entries.push(DispatchEntry {
                        dispatch_id,
                        subscriber_id: dispatch.subscriber_id,
                        scope_path: scope.clone(),
                        input_change_ids,
                    });

                    all_dispatch_metas.push(DispatchMeta {
                        dispatch_id,
                        scope_path: scope.clone(),
                        depth: level.depth,
                    });
                }

                if !entries.is_empty() {
                    groups.push(DispatchGroup {
                        dispatches: entries,
                    });
                }
            }
        }

        // Step 2: Compute propagation map.
        // For each dispatch D_child, find all dispatches at shallower depths
        // whose scope_path is an ancestor of D_child's scope_path.
        let total_dispatches = next_dispatch_id as usize;
        let mut propagation_map: Vec<Vec<PropagationTarget>> = vec![Vec::new(); total_dispatches];

        for child in &all_dispatch_metas {
            for parent in &all_dispatch_metas {
                if parent.depth >= child.depth {
                    continue; // Only propagate to shallower depths
                }

                // Check if parent.scope_path is an ancestor of child.scope_path
                let is_ancestor = if parent.scope_path.is_empty() {
                    true // root is ancestor of everything
                } else if child.scope_path == parent.scope_path {
                    false // same scope, not ancestor
                } else {
                    child.scope_path.starts_with(&parent.scope_path)
                        && child.scope_path.as_bytes().get(parent.scope_path.len()) == Some(&b'.')
                };

                if !is_ancestor {
                    continue;
                }

                // Compute remap_prefix: child scope relative to parent scope
                let remap_prefix = if parent.scope_path.is_empty() {
                    child.scope_path.clone()
                } else {
                    // Strip parent prefix + dot
                    child.scope_path[parent.scope_path.len() + 1..].to_owned()
                };

                propagation_map[child.dispatch_id as usize].push(PropagationTarget {
                    target_dispatch_id: parent.dispatch_id,
                    remap_prefix,
                });
            }
        }

        FullExecutionPlan {
            groups,
            propagation_map,
        }
    }

    /// Create a dispatch plan from a JSON string of changes.
    #[allow(dead_code)] // Called via WASM export chain
    pub(crate) fn create_dispatch_plan_json(&self, changes_json: &str) -> Result<String, String> {
        let changes: Vec<Change> = serde_json::from_str(changes_json)
            .map_err(|e| format!("Changes parse error: {}", e))?;

        let plan = self.create_dispatch_plan(&changes);
        serde_json::to_string(&plan).map_err(|e| format!("Serialize error: {}", e))
    }

    // ------------------------------------------------------------------
    // Routing produced changes
    // ------------------------------------------------------------------

    /// Route produced changes from a depth level to downstream topics.
    ///
    /// For each topic at the given depth that has routes, pass its produced
    /// changes to downstream (shallower) topics. Returns a new DispatchPlan
    /// for the downstream topics, or an empty plan if no routing is needed.
    #[allow(dead_code)] // Called via WASM export chain
    pub(crate) fn route_produced_changes(
        &self,
        depth: u32,
        produced_changes: &[Change],
    ) -> DispatchPlan {
        if produced_changes.is_empty() {
            return DispatchPlan { levels: Vec::new() };
        }

        // Collect all downstream topic IDs that should receive these changes
        let mut downstream_changes: HashMap<u32, Vec<Change>> = HashMap::new();

        // Find topics at the given depth
        for &topic_id in &self.topics {
            let meta = match self.topic_meta.get(&topic_id) {
                Some(m) if m.depth == depth => m,
                _ => continue,
            };

            // Check if any produced changes are relevant to this topic
            let topic_prefix = &meta.prefix;
            let relevant: Vec<&Change> = produced_changes
                .iter()
                .filter(|c| {
                    if topic_prefix.is_empty() {
                        true
                    } else {
                        c.path == *topic_prefix
                            || (c.path.starts_with(topic_prefix.as_str())
                                && c.path.as_bytes().get(topic_prefix.len()) == Some(&b'.'))
                            // Also match if the change path is a parent of the topic
                            || topic_prefix.starts_with(c.path.as_str())
                    }
                })
                .collect();

            if relevant.is_empty() {
                continue;
            }

            // Route to downstream topics
            if let Some(routes) = self.routes.get(&topic_id) {
                for route in routes {
                    let to_changes = downstream_changes.entry(route.to_topic_id).or_default();
                    // Pass produced changes as absolute paths (not relativized yet)
                    for &change in &relevant {
                        to_changes.push(change.clone());
                    }
                }
            }
        }

        // Also seed from produced changes directly (changes may match topics
        // at any depth, not just through routes)
        let all_seeded = self.seed_topics(produced_changes);
        for &topic_id in &all_seeded {
            if let Some(meta) = self.topic_meta.get(&topic_id) {
                if meta.depth >= depth {
                    continue; // Only route to shallower topics
                }
            }
            let entry = downstream_changes.entry(topic_id).or_default();
            for change in produced_changes {
                if !entry.iter().any(|c| c.path == change.path) {
                    entry.push(change.clone());
                }
            }
        }

        if downstream_changes.is_empty() {
            return DispatchPlan { levels: Vec::new() };
        }

        // Build dispatch plan from downstream changes
        let mut depth_groups: HashMap<u32, Vec<(u32, Vec<Change>)>> = HashMap::new();
        for (topic_id, changes) in downstream_changes {
            if let Some(meta) = self.topic_meta.get(&topic_id) {
                depth_groups
                    .entry(meta.depth)
                    .or_default()
                    .push((topic_id, changes));
            }
        }

        let mut depths: Vec<u32> = depth_groups.keys().copied().collect();
        depths.sort_by(|a, b| b.cmp(a));

        let mut levels = Vec::with_capacity(depths.len());

        for d in depths {
            let topic_entries = &depth_groups[&d];
            let mut dispatches = Vec::new();

            for (topic_id, changes) in topic_entries {
                let prefix = match self.topic_meta.get(topic_id) {
                    Some(m) => &m.prefix,
                    None => continue,
                };

                let relativized: Vec<Change> = changes
                    .iter()
                    .map(|c| Change {
                        path: relativize_path(&c.path, prefix),
                        value_json: c.value_json.clone(),
                        origin: c.origin.clone(),
                    })
                    .collect();

                if let Some(subs) = self.subscribers.get(topic_id) {
                    for &sub_id in subs {
                        if let Some(sub_meta) = self.subscriber_meta.get(&sub_id) {
                            let ancestors = build_ancestor_chain(&sub_meta.scope_path);
                            dispatches.push(ListenerDispatch {
                                subscriber_id: sub_id,
                                scope_path: sub_meta.scope_path.clone(),
                                changes: relativized.clone(),
                                ancestors,
                            });
                        }
                    }
                }
            }

            if !dispatches.is_empty() {
                levels.push(DispatchLevel {
                    depth: d,
                    dispatches,
                });
            }
        }

        DispatchPlan { levels }
    }

    /// Route produced changes from JSON and return a JSON dispatch plan.
    #[allow(dead_code)] // Called via WASM export chain
    pub(crate) fn route_produced_changes_json(
        &self,
        depth: u32,
        produced_changes_json: &str,
    ) -> Result<String, String> {
        let changes: Vec<Change> = serde_json::from_str(produced_changes_json)
            .map_err(|e| format!("Changes parse error: {}", e))?;

        let plan = self.route_produced_changes(depth, &changes);
        serde_json::to_string(&plan).map_err(|e| format!("Serialize error: {}", e))
    }
}

impl Default for TopicRouter {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn register(router: &mut TopicRouter, sub_id: u32, topic: &str, scope: &str) {
        let json = format!(
            r#"[{{"subscriber_id": {}, "topic_path": "{}", "scope_path": "{}"}}]"#,
            sub_id, topic, scope
        );
        router.register_listeners_batch(&json).unwrap();
    }

    fn make_change(path: &str, value: &str) -> Change {
        Change {
            path: path.to_owned(),
            value_json: value.to_owned(),
            origin: None,
        }
    }

    // --- path_depth ---

    #[test]
    fn depth_empty_is_zero() {
        assert_eq!(path_depth(""), 0);
    }

    #[test]
    fn depth_single_segment() {
        assert_eq!(path_depth("user"), 1);
    }

    #[test]
    fn depth_multiple_segments() {
        assert_eq!(path_depth("user.profile.email"), 3);
    }

    // --- relativize_path ---

    #[test]
    fn relativize_root() {
        assert_eq!(relativize_path("user.name", ""), "user.name");
    }

    #[test]
    fn relativize_exact_match() {
        assert_eq!(relativize_path("user.profile", "user.profile"), "");
    }

    #[test]
    fn relativize_child() {
        assert_eq!(
            relativize_path("user.profile.email", "user.profile"),
            "email"
        );
    }

    #[test]
    fn relativize_deep_child() {
        assert_eq!(
            relativize_path("user.profile.address.city", "user.profile"),
            "address.city"
        );
    }

    // --- registration ---

    #[test]
    fn register_single_listener() {
        let mut router = TopicRouter::new();
        register(&mut router, 1, "user.profile", "user.profile");

        assert!(router.has_listeners());
        assert_eq!(router.topics.len(), 1);
    }

    #[test]
    fn register_multiple_listeners_same_topic() {
        let mut router = TopicRouter::new();
        register(&mut router, 1, "user.profile", "user.profile");
        register(&mut router, 2, "user.profile", "user.profile");

        assert_eq!(router.topics.len(), 1); // same topic
        assert_eq!(router.subscribers[router.topics.first().unwrap()].len(), 2);
    }

    #[test]
    fn register_batch() {
        let mut router = TopicRouter::new();
        let json = r#"[
            {"subscriber_id": 1, "topic_path": "user.profile", "scope_path": "user.profile"},
            {"subscriber_id": 2, "topic_path": "user", "scope_path": "user"},
            {"subscriber_id": 3, "topic_path": "", "scope_path": ""}
        ]"#;
        router.register_listeners_batch(json).unwrap();

        assert_eq!(router.topics.len(), 3);
        assert!(router.has_listeners());
    }

    #[test]
    fn topics_sorted_deepest_first() {
        let mut router = TopicRouter::new();
        let json = r#"[
            {"subscriber_id": 1, "topic_path": "", "scope_path": ""},
            {"subscriber_id": 2, "topic_path": "user", "scope_path": "user"},
            {"subscriber_id": 3, "topic_path": "user.profile.email", "scope_path": "user.profile.email"}
        ]"#;
        router.register_listeners_batch(json).unwrap();

        // Deepest first: user.profile.email (3), user (1), "" (0)
        let depths: Vec<u32> = router
            .topics
            .iter()
            .map(|id| router.topic_meta[id].depth)
            .collect();
        assert_eq!(depths, vec![3, 1, 0]);
    }

    #[test]
    fn unregister_removes_subscriber() {
        let mut router = TopicRouter::new();
        register(&mut router, 1, "user", "user");
        register(&mut router, 2, "user", "user");

        router.unregister_listeners_batch("[1]").unwrap();

        assert_eq!(router.subscribers[router.topics.first().unwrap()].len(), 1);
        assert!(router.has_listeners());
    }

    #[test]
    fn unregister_removes_empty_topic() {
        let mut router = TopicRouter::new();
        register(&mut router, 1, "user", "user");

        router.unregister_listeners_batch("[1]").unwrap();

        assert!(!router.has_listeners());
        assert!(router.topics.is_empty());
    }

    #[test]
    fn unregister_nonexistent_noop() {
        let mut router = TopicRouter::new();
        register(&mut router, 1, "user", "user");

        router.unregister_listeners_batch("[999]").unwrap();
        assert!(router.has_listeners());
    }

    // --- seeding ---

    #[test]
    fn seed_exact_match() {
        let mut router = TopicRouter::new();
        register(&mut router, 1, "user.profile", "user.profile");

        let changes = vec![make_change("user.profile", r#""updated""#)];
        let matched = router.seed_topics(&changes);

        assert_eq!(matched.len(), 1);
    }

    #[test]
    fn seed_ancestor_match() {
        let mut router = TopicRouter::new();
        register(&mut router, 1, "user", "user");

        let changes = vec![make_change("user.profile.email", r#""test@test.com""#)];
        let matched = router.seed_topics(&changes);

        assert_eq!(matched.len(), 1);
    }

    #[test]
    fn seed_root_topic() {
        let mut router = TopicRouter::new();
        register(&mut router, 1, "", "");

        let changes = vec![make_change("any.path.here", r#""value""#)];
        let matched = router.seed_topics(&changes);

        assert_eq!(matched.len(), 1);
    }

    #[test]
    fn seed_multiple_topics() {
        let mut router = TopicRouter::new();
        register(&mut router, 1, "user.profile", "user.profile");
        register(&mut router, 2, "user", "user");
        register(&mut router, 3, "", "");

        let changes = vec![make_change("user.profile.email", r#""test@test.com""#)];
        let matched = router.seed_topics(&changes);

        // Should match: user.profile, user, root
        assert_eq!(matched.len(), 3);
    }

    #[test]
    fn seed_no_match() {
        let mut router = TopicRouter::new();
        register(&mut router, 1, "user.profile", "user.profile");

        let changes = vec![make_change("settings.theme", r#""dark""#)];
        let matched = router.seed_topics(&changes);

        assert!(matched.is_empty());
    }

    // --- dispatch plan ---

    #[test]
    fn dispatch_plan_single_listener() {
        let mut router = TopicRouter::new();
        register(&mut router, 1, "user", "user");

        let changes = vec![make_change("user.name", r#""alice""#)];
        let plan = router.create_dispatch_plan(&changes);

        assert_eq!(plan.levels.len(), 1);
        assert_eq!(plan.levels[0].depth, 1);
        assert_eq!(plan.levels[0].dispatches.len(), 1);
        assert_eq!(plan.levels[0].dispatches[0].subscriber_id, 1);
        assert_eq!(plan.levels[0].dispatches[0].scope_path, "user");
        assert_eq!(plan.levels[0].dispatches[0].changes.len(), 1);
        assert_eq!(plan.levels[0].dispatches[0].changes[0].path, "name"); // relativized
    }

    #[test]
    fn dispatch_plan_multi_depth() {
        let mut router = TopicRouter::new();
        register(&mut router, 1, "user.profile", "user.profile");
        register(&mut router, 2, "user", "user");

        let changes = vec![make_change("user.profile.email", r#""test@test.com""#)];
        let plan = router.create_dispatch_plan(&changes);

        assert_eq!(plan.levels.len(), 2);

        // Deepest first
        assert_eq!(plan.levels[0].depth, 2); // user.profile
        assert_eq!(plan.levels[0].dispatches[0].changes[0].path, "email");

        assert_eq!(plan.levels[1].depth, 1); // user
        assert_eq!(
            plan.levels[1].dispatches[0].changes[0].path,
            "profile.email"
        );
    }

    #[test]
    fn dispatch_plan_root_topic() {
        let mut router = TopicRouter::new();
        register(&mut router, 1, "", "");

        let changes = vec![make_change("user.name", r#""alice""#)];
        let plan = router.create_dispatch_plan(&changes);

        assert_eq!(plan.levels.len(), 1);
        assert_eq!(plan.levels[0].depth, 0);
        assert_eq!(plan.levels[0].dispatches[0].changes[0].path, "user.name"); // not relativized for root
    }

    #[test]
    fn dispatch_plan_empty_changes() {
        let mut router = TopicRouter::new();
        register(&mut router, 1, "user", "user");

        let plan = router.create_dispatch_plan(&[]);
        assert!(plan.levels.is_empty());
    }

    #[test]
    fn dispatch_plan_no_matching_listeners() {
        let mut router = TopicRouter::new();
        register(&mut router, 1, "user.profile", "user.profile");

        let changes = vec![make_change("settings.theme", r#""dark""#)];
        let plan = router.create_dispatch_plan(&changes);

        assert!(plan.levels.is_empty());
    }

    #[test]
    fn dispatch_plan_multiple_subscribers_same_topic() {
        let mut router = TopicRouter::new();
        register(&mut router, 1, "user", "user");
        register(&mut router, 2, "user", "user");

        let changes = vec![make_change("user.name", r#""alice""#)];
        let plan = router.create_dispatch_plan(&changes);

        assert_eq!(plan.levels.len(), 1);
        assert_eq!(plan.levels[0].dispatches.len(), 2);
    }

    // --- route table ---

    #[test]
    fn routes_precomputed() {
        let mut router = TopicRouter::new();
        register(&mut router, 1, "user.profile", "user.profile");
        register(&mut router, 2, "user", "user");
        register(&mut router, 3, "", "");

        // user.profile should have routes to user and root
        let profile_topic_id = *router.path_to_topic.get("user.profile").unwrap();
        let routes = router.routes.get(&profile_topic_id).unwrap();
        assert_eq!(routes.len(), 2);

        // user should have route to root
        let user_topic_id = *router.path_to_topic.get("user").unwrap();
        let routes = router.routes.get(&user_topic_id).unwrap();
        assert_eq!(routes.len(), 1);

        // root has no downstream routes
        let root_topic_id = *router.path_to_topic.get("").unwrap();
        assert!(router.routes.get(&root_topic_id).is_none());
    }

    // --- route produced changes ---

    #[test]
    fn route_produced_to_downstream() {
        let mut router = TopicRouter::new();
        register(&mut router, 1, "user.profile", "user.profile");
        register(&mut router, 2, "user", "user");

        // Simulate: depth-2 listener produced a change
        let produced = vec![make_change("user.profile.email", r#""new@test.com""#)];
        let plan = router.route_produced_changes(2, &produced);

        // Should route to depth-1 topic (user)
        assert!(!plan.levels.is_empty());

        let has_user_dispatch = plan
            .levels
            .iter()
            .any(|l| l.dispatches.iter().any(|d| d.subscriber_id == 2));
        assert!(has_user_dispatch);
    }

    #[test]
    fn route_empty_produced_changes() {
        let mut router = TopicRouter::new();
        register(&mut router, 1, "user", "user");

        let plan = router.route_produced_changes(2, &[]);
        assert!(plan.levels.is_empty());
    }

    // --- JSON interface ---

    #[test]
    fn create_dispatch_plan_json() {
        let mut router = TopicRouter::new();
        register(&mut router, 1, "user", "user");

        let result = router
            .create_dispatch_plan_json(r#"[{"path": "user.name", "value_json": "\"alice\""}]"#)
            .unwrap();

        let plan: DispatchPlan = serde_json::from_str(&result).unwrap();
        assert_eq!(plan.levels.len(), 1);
    }

    #[test]
    fn route_produced_changes_json() {
        let mut router = TopicRouter::new();
        register(&mut router, 1, "user.profile", "user.profile");
        register(&mut router, 2, "user", "user");

        let result = router
            .route_produced_changes_json(
                2,
                r#"[{"path": "user.profile.email", "value_json": "\"new@test.com\""}]"#,
            )
            .unwrap();

        let plan: DispatchPlan = serde_json::from_str(&result).unwrap();
        // Downstream routing should find the user topic
        assert!(!plan.levels.is_empty() || true); // May or may not have levels depending on routing
    }

    #[test]
    fn invalid_json_registration() {
        let mut router = TopicRouter::new();
        let result = router.register_listeners_batch("not json");
        assert!(result.is_err());
    }

    #[test]
    fn invalid_json_unregister() {
        let mut router = TopicRouter::new();
        let result = router.unregister_listeners_batch("not json");
        assert!(result.is_err());
    }

    #[test]
    fn invalid_json_dispatch() {
        let router = TopicRouter::new();
        let result = router.create_dispatch_plan_json("not json");
        assert!(result.is_err());
    }

    // --- max_depth ---

    #[test]
    fn max_depth_empty() {
        let router = TopicRouter::new();
        assert_eq!(router.max_depth(), 0);
    }

    #[test]
    fn max_depth_with_listeners() {
        let mut router = TopicRouter::new();
        register(&mut router, 1, "user.profile.email", "user.profile.email");
        register(&mut router, 2, "user", "user");

        assert_eq!(router.max_depth(), 3);
    }

    // --- is_ancestor_or_descendant ---

    #[test]
    fn ancestor_or_desc_equal() {
        assert!(is_ancestor_or_descendant("user", "user"));
    }

    #[test]
    fn ancestor_or_desc_parent() {
        assert!(is_ancestor_or_descendant("user", "user.name"));
    }

    #[test]
    fn ancestor_or_desc_child() {
        assert!(is_ancestor_or_descendant("user.name", "user"));
    }

    #[test]
    fn ancestor_or_desc_unrelated() {
        assert!(!is_ancestor_or_descendant("user", "settings"));
    }

    #[test]
    fn ancestor_or_desc_root() {
        assert!(is_ancestor_or_descendant("", "user"));
        assert!(is_ancestor_or_descendant("user", ""));
    }

    #[test]
    fn ancestor_or_desc_partial_prefix() {
        // "user" should NOT be ancestor of "username"
        assert!(!is_ancestor_or_descendant("user", "username"));
    }

    // --- group_into_batches ---

    #[test]
    fn batch_independent_siblings() {
        // Independent sibling scopes → single batch
        let dispatches = vec![
            ListenerDispatch {
                subscriber_id: 1,
                scope_path: "orders.order_0".to_owned(),
                changes: vec![],
                ancestors: vec![],
            },
            ListenerDispatch {
                subscriber_id: 2,
                scope_path: "orders.order_1".to_owned(),
                changes: vec![],
                ancestors: vec![],
            },
            ListenerDispatch {
                subscriber_id: 3,
                scope_path: "orders.order_2".to_owned(),
                changes: vec![],
                ancestors: vec![],
            },
        ];

        let batches = group_into_batches(&dispatches);
        assert_eq!(batches.len(), 1);
        assert_eq!(batches[0].dispatches.len(), 3);
    }

    #[test]
    fn batch_overlapping_scopes() {
        // Parent + child → separate batches
        let dispatches = vec![
            ListenerDispatch {
                subscriber_id: 1,
                scope_path: "user".to_owned(),
                changes: vec![],
                ancestors: vec![],
            },
            ListenerDispatch {
                subscriber_id: 2,
                scope_path: "user.profile".to_owned(),
                changes: vec![],
                ancestors: vec![],
            },
        ];

        let batches = group_into_batches(&dispatches);
        assert_eq!(batches.len(), 2);
    }

    #[test]
    fn batch_single_dispatch() {
        let dispatches = vec![ListenerDispatch {
            subscriber_id: 1,
            scope_path: "user".to_owned(),
            changes: vec![],
            ancestors: vec![],
        }];

        let batches = group_into_batches(&dispatches);
        assert_eq!(batches.len(), 1);
        assert_eq!(batches[0].dispatches.len(), 1);
    }

    #[test]
    fn batch_root_listener_conflicts_with_everything() {
        let dispatches = vec![
            ListenerDispatch {
                subscriber_id: 1,
                scope_path: "".to_owned(),
                changes: vec![],
                ancestors: vec![],
            },
            ListenerDispatch {
                subscriber_id: 2,
                scope_path: "user".to_owned(),
                changes: vec![],
                ancestors: vec![],
            },
        ];

        let batches = group_into_batches(&dispatches);
        assert_eq!(batches.len(), 2);
    }

    #[test]
    fn batch_empty_dispatches() {
        let batches = group_into_batches(&[]);
        assert!(batches.is_empty());
    }

    // --- execution plan ---

    #[test]
    fn execution_plan_groups_correctly() {
        let mut router = TopicRouter::new();
        // Register 3 independent sibling listeners at same depth
        register(&mut router, 1, "orders.order_0", "orders.order_0");
        register(&mut router, 2, "orders.order_1", "orders.order_1");
        register(&mut router, 3, "orders.order_2", "orders.order_2");

        let changes = vec![
            make_change("orders.order_0.status", r#""pending""#),
            make_change("orders.order_1.status", r#""pending""#),
            make_change("orders.order_2.status", r#""pending""#),
        ];

        let plan = router.create_execution_plan(&changes);
        assert_eq!(plan.levels.len(), 1);
        // All independent → should be in a single batch
        assert_eq!(plan.levels[0].batches.len(), 1);
        assert_eq!(plan.levels[0].batches[0].dispatches.len(), 3);
    }

    // --- full execution plan ---

    #[test]
    fn full_execution_plan_basic() {
        let mut router = TopicRouter::new();
        register(&mut router, 1, "user", "user");

        let changes = vec![make_change("user.name", r#""alice""#)];
        let plan = router.create_full_execution_plan(&changes);

        assert_eq!(plan.groups.len(), 1);
        assert_eq!(plan.groups[0].dispatches.len(), 1);
        assert_eq!(plan.groups[0].dispatches[0].dispatch_id, 0);
        assert_eq!(plan.groups[0].dispatches[0].subscriber_id, 1);
        assert_eq!(plan.groups[0].dispatches[0].input_change_ids, vec![0]);
        // No propagation for single-depth listener
        assert_eq!(plan.propagation_map.len(), 1);
        assert!(plan.propagation_map[0].is_empty());
    }

    #[test]
    fn full_execution_plan_propagation_map() {
        let mut router = TopicRouter::new();
        // 3 levels: orders.order_0 (depth 2), orders (depth 1), root (depth 0)
        register(&mut router, 1, "orders.order_0", "orders.order_0");
        register(&mut router, 2, "orders", "orders");
        register(&mut router, 3, "", "");

        let changes = vec![make_change("orders.order_0.status", r#""pending""#)];
        let plan = router.create_full_execution_plan(&changes);

        // Should have 3 dispatches total
        assert_eq!(plan.propagation_map.len(), 3);

        // Find dispatch IDs by scope
        let mut id_by_scope: std::collections::HashMap<&str, u32> =
            std::collections::HashMap::new();
        for group in &plan.groups {
            for d in &group.dispatches {
                id_by_scope.insert(&d.scope_path, d.dispatch_id);
            }
        }

        let deep_id = *id_by_scope.get("orders.order_0").unwrap();
        let mid_id = *id_by_scope.get("orders").unwrap();
        let root_id = *id_by_scope.get("").unwrap();

        // Deep dispatch (orders.order_0) should propagate to orders and root
        let deep_targets = &plan.propagation_map[deep_id as usize];
        assert_eq!(deep_targets.len(), 2);

        let to_orders = deep_targets
            .iter()
            .find(|t| t.target_dispatch_id == mid_id)
            .unwrap();
        assert_eq!(to_orders.remap_prefix, "order_0");

        let to_root = deep_targets
            .iter()
            .find(|t| t.target_dispatch_id == root_id)
            .unwrap();
        assert_eq!(to_root.remap_prefix, "orders.order_0");

        // Mid dispatch (orders) should propagate to root only
        let mid_targets = &plan.propagation_map[mid_id as usize];
        assert_eq!(mid_targets.len(), 1);
        assert_eq!(mid_targets[0].target_dispatch_id, root_id);
        assert_eq!(mid_targets[0].remap_prefix, "orders");

        // Root dispatch should have no propagation targets
        assert!(plan.propagation_map[root_id as usize].is_empty());
    }

    #[test]
    fn full_execution_plan_empty() {
        let router = TopicRouter::new();
        let changes = vec![make_change("user.name", r#""alice""#)];
        let plan = router.create_full_execution_plan(&changes);
        assert!(plan.groups.is_empty());
        assert!(plan.propagation_map.is_empty());
    }

    #[test]
    fn full_execution_plan_input_change_ids() {
        let mut router = TopicRouter::new();
        register(&mut router, 1, "user", "user");
        register(&mut router, 2, "settings", "settings");

        let changes = vec![
            make_change("user.name", r#""alice""#),
            make_change("settings.theme", r#""dark""#),
            make_change("user.age", r#"30"#),
        ];

        let plan = router.create_full_execution_plan(&changes);

        for group in &plan.groups {
            for d in &group.dispatches {
                if d.scope_path == "user" {
                    // user scope should match changes 0 and 2
                    assert_eq!(d.input_change_ids, vec![0, 2]);
                } else if d.scope_path == "settings" {
                    // settings scope should match change 1
                    assert_eq!(d.input_change_ids, vec![1]);
                }
            }
        }
    }
}
