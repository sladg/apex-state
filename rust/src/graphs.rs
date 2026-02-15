//! Graph structures for sync and flip operations.
//!
//! Uses connected components for O(1) group lookup.
//! When a path in a group changes, all peers receive the same (or inverted) value.

use std::collections::{HashMap, HashSet};

/// A graph using connected components for O(1) group lookups.
///
/// Maintains bidirectional mapping:
/// - path_id → component_id (fast lookup)
/// - component_id → [path_ids] (fast iteration over peers)
/// - adjacency list (for edge tracking and split detection)
pub(crate) struct Graph {
    node_to_component: HashMap<u32, u32>,
    component_to_nodes: HashMap<u32, HashSet<u32>>,
    adjacency: HashMap<u32, HashSet<u32>>,
    edges: HashSet<(u32, u32)>,
    next_component_id: u32,
}

impl Graph {
    pub(crate) fn new() -> Self {
        Self {
            node_to_component: HashMap::new(),
            component_to_nodes: HashMap::new(),
            adjacency: HashMap::new(),
            edges: HashSet::new(),
            next_component_id: 0,
        }
    }

    /// Add an edge between two path IDs (public wrapper).
    /// If paths are in different components, merges them (smaller into larger).
    pub(crate) fn add_edge_public(&mut self, path1_id: u32, path2_id: u32) {
        self.add_edge(path1_id, path2_id);
    }

    /// Add an edge between two path IDs.
    /// If paths are in different components, merges them (smaller into larger).
    fn add_edge(&mut self, path1_id: u32, path2_id: u32) {
        // Create canonical edge representation
        let edge = if path1_id < path2_id {
            (path1_id, path2_id)
        } else {
            (path2_id, path1_id)
        };

        // Skip if edge already exists
        if self.edges.contains(&edge) {
            return;
        }

        // Record edge
        self.edges.insert(edge);

        // Update adjacency
        self.adjacency
            .entry(path1_id)
            .or_insert_with(HashSet::new)
            .insert(path2_id);
        self.adjacency
            .entry(path2_id)
            .or_insert_with(HashSet::new)
            .insert(path1_id);

        let g1 = self.node_to_component.get(&path1_id).copied();
        let g2 = self.node_to_component.get(&path2_id).copied();

        match (g1, g2) {
            (None, None) => {
                // Both paths are new - create new component
                let comp_id = self.next_component_id;
                self.next_component_id += 1;

                self.node_to_component.insert(path1_id, comp_id);
                self.node_to_component.insert(path2_id, comp_id);

                let mut nodes = HashSet::new();
                nodes.insert(path1_id);
                nodes.insert(path2_id);
                self.component_to_nodes.insert(comp_id, nodes);
            }
            (Some(comp1), None) => {
                // Add path2 to path1's component
                self.node_to_component.insert(path2_id, comp1);
                self.component_to_nodes.get_mut(&comp1).unwrap().insert(path2_id);
            }
            (None, Some(comp2)) => {
                // Add path1 to path2's component
                self.node_to_component.insert(path1_id, comp2);
                self.component_to_nodes.get_mut(&comp2).unwrap().insert(path1_id);
            }
            (Some(comp1), Some(comp2)) if comp1 != comp2 => {
                // Merge components - move smaller into larger
                let set1_size = self.component_to_nodes[&comp1].len();
                let set2_size = self.component_to_nodes[&comp2].len();

                let (smaller_comp, larger_comp) = if set1_size <= set2_size {
                    (comp1, comp2)
                } else {
                    (comp2, comp1)
                };

                let smaller_nodes: Vec<u32> = self.component_to_nodes[&smaller_comp]
                    .iter()
                    .copied()
                    .collect();

                for node in smaller_nodes {
                    self.node_to_component.insert(node, larger_comp);
                    self.component_to_nodes.get_mut(&larger_comp).unwrap().insert(node);
                }

                self.component_to_nodes.remove(&smaller_comp);
            }
            (Some(_), Some(_)) => {
                // Same component - already connected, no-op
            }
        }
    }

    /// Remove an edge between two path IDs (public wrapper).
    /// If removal splits a component, creates new components via BFS.
    pub(crate) fn remove_edge_public(&mut self, path1_id: u32, path2_id: u32) {
        self.remove_edge(path1_id, path2_id);
    }

    /// Remove an edge between two path IDs.
    /// If removal splits a component, creates new components via BFS.
    fn remove_edge(&mut self, path1_id: u32, path2_id: u32) {
        // Create canonical edge representation
        let edge = if path1_id < path2_id {
            (path1_id, path2_id)
        } else {
            (path2_id, path1_id)
        };

        // Skip if edge doesn't exist
        if !self.edges.contains(&edge) {
            return;
        }

        // Remove edge
        self.edges.remove(&edge);

        // Update adjacency
        self.adjacency.get_mut(&path1_id).map(|adj| adj.remove(&path2_id));
        self.adjacency.get_mut(&path2_id).map(|adj| adj.remove(&path1_id));

        // Check for isolated nodes
        let path1_isolated = self
            .adjacency
            .get(&path1_id)
            .map_or(true, |adj| adj.is_empty());
        let path2_isolated = self
            .adjacency
            .get(&path2_id)
            .map_or(true, |adj| adj.is_empty());

        if path1_isolated {
            self.remove_isolated_node(path1_id);
        }
        if path2_isolated {
            self.remove_isolated_node(path2_id);
        }

        // If both still have edges, check for component split
        if !path1_isolated && !path2_isolated {
            self.handle_component_split(path1_id, path2_id);
        }
    }

    /// Remove an isolated node from the graph.
    fn remove_isolated_node(&mut self, node: u32) {
        if let Some(comp_id) = self.node_to_component.get(&node).copied() {
            self.component_to_nodes.get_mut(&comp_id).map(|nodes| nodes.remove(&node));
            if self.component_to_nodes[&comp_id].is_empty() {
                self.component_to_nodes.remove(&comp_id);
            }
        }
        self.node_to_component.remove(&node);
        self.adjacency.remove(&node);
    }

    /// BFS traversal to collect all reachable nodes from a starting node.
    fn bfs_collect(&self, start: u32) -> HashSet<u32> {
        let mut visited = HashSet::new();
        let mut queue = vec![start];

        visited.insert(start);

        while let Some(current) = queue.pop() {
            if let Some(neighbors) = self.adjacency.get(&current) {
                for &neighbor in neighbors {
                    if !visited.contains(&neighbor) {
                        visited.insert(neighbor);
                        queue.push(neighbor);
                    }
                }
            }
        }

        visited
    }

    /// Handle component split after edge removal.
    /// If node2 is no longer reachable from node1, creates new component.
    fn handle_component_split(&mut self, node1: u32, node2: u32) {
        let reachable = self.bfs_collect(node1);

        // If node2 is reachable, no split occurred
        if reachable.contains(&node2) {
            return;
        }

        // Component split - create new component for node2's reachable set
        let old_comp = self.node_to_component[&node2];
        let new_comp = self.next_component_id;
        self.next_component_id += 1;

        let new_component = self.bfs_collect(node2);

        for node in &new_component {
            self.node_to_component.insert(*node, new_comp);
            self.component_to_nodes
                .get_mut(&old_comp)
                .map(|nodes| nodes.remove(node));
        }

        self.component_to_nodes.insert(new_comp, new_component);

        // Clean up empty old component
        if self.component_to_nodes[&old_comp].is_empty() {
            self.component_to_nodes.remove(&old_comp);
        }
    }

    /// Get all paths in the same component as the given path ID (public wrapper).
    /// Returns empty vec if path ID not found.
    pub(crate) fn get_component_paths_public(&self, node: u32) -> Vec<u32> {
        self.get_component_paths(node)
    }

    /// Get all paths in the same component as the given path ID.
    /// Returns empty vec if path ID not found.
    fn get_component_paths(&self, node: u32) -> Vec<u32> {
        self.node_to_component
            .get(&node)
            .and_then(|comp_id| self.component_to_nodes.get(comp_id))
            .map(|nodes| nodes.iter().copied().collect())
            .unwrap_or_default()
    }

    /// Check if a path ID exists in the graph.
    fn has_node(&self, node: u32) -> bool {
        self.node_to_component.contains_key(&node)
    }

    /// Clear all edges and components.
    pub(crate) fn clear(&mut self) {
        *self = Self::new();
    }
}

impl Default for Graph {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_graph() {
        let g = Graph::new();
        assert!(!g.has_node(0));
        assert!(g.get_component_paths(0).is_empty());
    }

    #[test]
    fn add_single_edge() {
        let mut g = Graph::new();
        g.add_edge(0, 1);

        assert!(g.has_node(0));
        assert!(g.has_node(1));

        let paths = g.get_component_paths(0);
        assert_eq!(paths.len(), 2);
        assert!(paths.contains(&0));
        assert!(paths.contains(&1));
    }

    #[test]
    fn add_duplicate_edge() {
        let mut g = Graph::new();
        g.add_edge(0, 1);
        let paths1 = g.get_component_paths(0);

        g.add_edge(0, 1); // Duplicate
        let paths2 = g.get_component_paths(0);

        assert_eq!(paths1.len(), paths2.len());
        assert_eq!(paths1.len(), 2);
    }

    #[test]
    fn merge_components() {
        let mut g = Graph::new();
        g.add_edge(0, 1);
        g.add_edge(2, 3);
        assert_eq!(g.get_component_paths(0).len(), 2);
        assert_eq!(g.get_component_paths(2).len(), 2);

        // Connect the two components
        g.add_edge(1, 2);

        // All should now be in same component
        let paths = g.get_component_paths(0);
        assert_eq!(paths.len(), 4);
        assert!(paths.contains(&0));
        assert!(paths.contains(&1));
        assert!(paths.contains(&2));
        assert!(paths.contains(&3));
    }

    #[test]
    fn remove_edge_no_split() {
        let mut g = Graph::new();
        g.add_edge(0, 1);
        g.add_edge(1, 2);

        // Remove edge between 0 and 1
        g.remove_edge(0, 1);

        // 0 should be isolated, 1 and 2 still connected
        assert!(!g.has_node(0));
        let paths = g.get_component_paths(1);
        assert_eq!(paths.len(), 2);
        assert!(paths.contains(&1));
        assert!(paths.contains(&2));
    }

    #[test]
    fn remove_edge_with_split() {
        let mut g = Graph::new();
        g.add_edge(0, 1);
        g.add_edge(1, 2);
        g.add_edge(2, 3);

        // Remove edge between 1 and 2
        // This splits [0, 1, 2, 3] into [0, 1] and [2, 3]
        g.remove_edge(1, 2);

        let paths_0 = g.get_component_paths(0);
        let paths_2 = g.get_component_paths(2);

        // [0, 1] in one component
        assert_eq!(paths_0.len(), 2);
        // [2, 3] in another component
        assert_eq!(paths_2.len(), 2);
        // No overlap
        assert!(!paths_0.iter().any(|p| *p == 2));
        assert!(!paths_0.iter().any(|p| *p == 3));
    }

    #[test]
    fn triangle_removal() {
        let mut g = Graph::new();
        g.add_edge(0, 1);
        g.add_edge(1, 2);
        g.add_edge(2, 0);

        assert_eq!(g.get_component_paths(0).len(), 3);

        // Remove one edge - triangle should stay connected
        g.remove_edge(0, 1);
        let paths = g.get_component_paths(0);
        assert_eq!(paths.len(), 3);
    }

    #[test]
    fn clear_graph() {
        let mut g = Graph::new();
        g.add_edge(0, 1);
        g.add_edge(2, 3);
        assert!(g.has_node(0));

        g.clear();
        assert!(!g.has_node(0));
        assert!(!g.has_node(2));
        assert!(g.get_component_paths(0).is_empty());
    }

    #[test]
    fn stress_many_components() {
        let mut g = Graph::new();
        for i in 0..100 {
            g.add_edge(i * 2, i * 2 + 1);
        }

        // Should have 100 components
        for i in 0..100 {
            let paths = g.get_component_paths(i * 2);
            assert_eq!(paths.len(), 2);
        }
    }

    #[test]
    fn stress_large_component() {
        let mut g = Graph::new();
        for i in 0..99 {
            g.add_edge(i, i + 1);
        }

        // Should all be in same component
        let paths = g.get_component_paths(0);
        assert_eq!(paths.len(), 100);
    }
}
