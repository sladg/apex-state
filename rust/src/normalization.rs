use serde::{Deserialize, Serialize};

/// A change to a path with a JSON value
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Change {
    pub path: String,
    pub value_json: String,
}

/// Result of normalizing a change for a path group
#[derive(Debug, Clone)]
pub struct NormalizedChange {
    pub matched_path: String,
    pub relative_path: Option<String>,
    pub value_json: String,
    pub connected_paths: Vec<String>,
}

/// Normalize a single change for a single registered path.
/// Returns None if the change is not relevant to the registered path.
///
/// Three match modes:
/// 1. Exact match: changePath == registeredPath → value applies directly
/// 2. Parent change: registeredPath starts with changePath + '.' → extract nested value
/// 3. Child change: changePath starts with registeredPath + '.' → preserve relative path
fn normalize_change(
    change_path: &str,
    registered_path: &str,
) -> Option<(Option<String>, String)> {
    // Case 1: Exact match
    // e.g., changePath='a.b.c', registeredPath='a.b.c'
    if change_path == registered_path {
        return Some((None, "EXACT".to_string()));
    }

    // Case 2: Parent change
    // e.g., changePath='a.b', registeredPath='a.b.c.d'
    // Extract nested value: 'a.b.c.d' - 'a.b.' = 'c.d'
    if registered_path.starts_with(&format!("{}.", change_path)) {
        let nested_path = &registered_path[change_path.len() + 1..];
        return Some((None, format!("PARENT:{}", nested_path)));
    }

    // Case 3: Child change
    // e.g., changePath='a.b.c.d.e', registeredPath='a.b.c'
    // Preserve relative path: 'a.b.c.d.e' - 'a.b.c.' = 'd.e'
    if change_path.starts_with(&format!("{}.", registered_path)) {
        let relative_path = &change_path[registered_path.len() + 1..];
        return Some((Some(relative_path.to_string()), "CHILD".to_string()));
    }

    // No match
    None
}

/// Normalize changes for grouped/connected paths (e.g., sync path components)
///
/// When a change matches ANY path in a group, it's recorded once for the whole group.
/// The relative_path can be used to apply the change to all neighbor paths.
///
/// # Arguments
/// - `changes`: List of changes with path and value
/// - `path_groups`: Groups of connected paths (e.g., from sync/flip components)
///
/// # Returns
/// Vector of normalized changes with matched path, relative path (if any), and connected paths
///
/// # Example
/// ```ignore
/// // Exact match on one path
/// changes: [Change { path: "a.b.c", value_json: "\"newValue\"" }]
/// path_groups: [["a.b.c", "path.synced", "wow.path"]]
/// result: [NormalizedChange {
///   matched_path: "a.b.c",
///   relative_path: None,
///   value_json: "\"newValue\"",
///   connected_paths: ["a.b.c", "path.synced", "wow.path"]
/// }]
/// ```
///
/// # Child change example
/// ```ignore
/// changes: [Change { path: "a.b.c.deep.nested", value_json: "42" }]
/// path_groups: [["a.b.c", "path.synced", "wow.path"]]
/// result: [NormalizedChange {
///   matched_path: "a.b.c",
///   relative_path: Some("deep.nested"),
///   value_json: "42",
///   connected_paths: ["a.b.c", "path.synced", "wow.path"]
/// }]
/// ```
pub fn normalize_changes_for_groups(
    changes: &[Change],
    path_groups: &[Vec<String>],
) -> Vec<NormalizedChange> {
    let mut result = Vec::new();

    for change in changes {
        for group in path_groups {
            // Find first matching path in this group
            let mut match_found: Option<(String, Option<String>)> = None;

            for registered_path in group {
                if let Some((relative_path, _match_type)) = normalize_change(&change.path, registered_path) {
                    match_found = Some((registered_path.clone(), relative_path));
                    break; // Stop at first match in group
                }
            }

            // If any path in group matched, record it once for the whole group
            if let Some((matched_path, relative_path)) = match_found {
                result.push(NormalizedChange {
                    matched_path,
                    relative_path,
                    value_json: change.value_json.clone(),
                    connected_paths: group.clone(),
                });
            }
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn exact_match() {
        let changes = vec![Change {
            path: "a.b.c".to_string(),
            value_json: "\"newValue\"".to_string(),
        }];

        let path_groups = vec![vec![
            "a.b.c".to_string(),
            "path.synced".to_string(),
            "wow.path".to_string(),
        ]];

        let result = normalize_changes_for_groups(&changes, &path_groups);

        assert_eq!(result.len(), 1);
        assert_eq!(result[0].matched_path, "a.b.c");
        assert_eq!(result[0].relative_path, None);
        assert_eq!(result[0].value_json, "\"newValue\"");
        assert_eq!(result[0].connected_paths.len(), 3);
    }

    #[test]
    fn parent_change() {
        let changes = vec![Change {
            path: "a.b".to_string(),
            value_json: "{\"c\": {\"d\": 42}}".to_string(),
        }];

        let path_groups = vec![vec![
            "a.b.c.d".to_string(),
            "path.synced".to_string(),
        ]];

        let result = normalize_changes_for_groups(&changes, &path_groups);

        assert_eq!(result.len(), 1);
        assert_eq!(result[0].matched_path, "a.b.c.d");
        assert_eq!(result[0].relative_path, None); // Parent change, no relative path
        assert_eq!(result[0].value_json, "{\"c\": {\"d\": 42}}");
    }

    #[test]
    fn child_change() {
        let changes = vec![Change {
            path: "a.b.c.deep.nested".to_string(),
            value_json: "42".to_string(),
        }];

        let path_groups = vec![vec![
            "a.b.c".to_string(),
            "path.synced".to_string(),
            "wow.path".to_string(),
        ]];

        let result = normalize_changes_for_groups(&changes, &path_groups);

        assert_eq!(result.len(), 1);
        assert_eq!(result[0].matched_path, "a.b.c");
        assert_eq!(result[0].relative_path, Some("deep.nested".to_string()));
        assert_eq!(result[0].value_json, "42");
        assert_eq!(result[0].connected_paths.len(), 3);
    }

    #[test]
    fn no_match() {
        let changes = vec![Change {
            path: "x.y.z".to_string(),
            value_json: "\"value\"".to_string(),
        }];

        let path_groups = vec![vec![
            "a.b.c".to_string(),
            "path.synced".to_string(),
        ]];

        let result = normalize_changes_for_groups(&changes, &path_groups);

        assert_eq!(result.len(), 0);
    }

    #[test]
    fn multiple_groups() {
        let changes = vec![
            Change {
                path: "a.b.c".to_string(),
                value_json: "\"val1\"".to_string(),
            },
            Change {
                path: "x.y.z".to_string(),
                value_json: "\"val2\"".to_string(),
            },
        ];

        let path_groups = vec![
            vec!["a.b.c".to_string(), "d.e.f".to_string()],
            vec!["x.y.z".to_string(), "m.n.o".to_string()],
        ];

        let result = normalize_changes_for_groups(&changes, &path_groups);

        assert_eq!(result.len(), 2);
        assert_eq!(result[0].matched_path, "a.b.c");
        assert_eq!(result[1].matched_path, "x.y.z");
    }

    #[test]
    fn deeply_nested_child_change() {
        let changes = vec![Change {
            path: "user.profile.address.street.name".to_string(),
            value_json: "\"Main St\"".to_string(),
        }];

        let path_groups = vec![vec![
            "user.profile".to_string(),
            "account.profile".to_string(),
        ]];

        let result = normalize_changes_for_groups(&changes, &path_groups);

        assert_eq!(result.len(), 1);
        assert_eq!(result[0].matched_path, "user.profile");
        assert_eq!(
            result[0].relative_path,
            Some("address.street.name".to_string())
        );
    }

    #[test]
    fn root_path_edge_case() {
        let changes = vec![Change {
            path: "".to_string(),
            value_json: "{}".to_string(),
        }];

        let path_groups = vec![vec![
            "".to_string(),
            "other".to_string(),
        ]];

        let result = normalize_changes_for_groups(&changes, &path_groups);

        assert_eq!(result.len(), 1);
        assert_eq!(result[0].matched_path, "");
        assert_eq!(result[0].relative_path, None);
    }

    #[test]
    fn root_change_with_child_path() {
        let changes = vec![Change {
            path: "".to_string(),
            value_json: "{\"a\": {\"b\": 42}}".to_string(),
        }];

        let path_groups = vec![vec![
            "a.b".to_string(),
            "x.y".to_string(),
        ]];

        let result = normalize_changes_for_groups(&changes, &path_groups);

        assert_eq!(result.len(), 1);
        assert_eq!(result[0].matched_path, "a.b");
        assert_eq!(result[0].relative_path, None); // Parent change from root
    }

    #[test]
    fn single_segment_paths() {
        let changes = vec![Change {
            path: "user".to_string(),
            value_json: "{\"name\": \"Alice\"}".to_string(),
        }];

        let path_groups = vec![vec![
            "user".to_string(),
            "profile".to_string(),
        ]];

        let result = normalize_changes_for_groups(&changes, &path_groups);

        assert_eq!(result.len(), 1);
        assert_eq!(result[0].matched_path, "user");
        assert_eq!(result[0].relative_path, None);
    }

    #[test]
    fn first_match_wins() {
        // When multiple paths in a group match, first match should win
        let changes = vec![Change {
            path: "a.b.c".to_string(),
            value_json: "\"value\"".to_string(),
        }];

        let path_groups = vec![vec![
            "a.b.c".to_string(),
            "a.b".to_string(), // This is a parent match, but should not be selected
        ]];

        let result = normalize_changes_for_groups(&changes, &path_groups);

        assert_eq!(result.len(), 1);
        assert_eq!(result[0].matched_path, "a.b.c"); // Exact match comes first
    }

    #[test]
    fn empty_changes() {
        let changes = vec![];
        let path_groups = vec![vec!["a.b.c".to_string()]];

        let result = normalize_changes_for_groups(&changes, &path_groups);

        assert_eq!(result.len(), 0);
    }

    #[test]
    fn empty_groups() {
        let changes = vec![Change {
            path: "a.b.c".to_string(),
            value_json: "\"value\"".to_string(),
        }];
        let path_groups = vec![];

        let result = normalize_changes_for_groups(&changes, &path_groups);

        assert_eq!(result.len(), 0);
    }
}
