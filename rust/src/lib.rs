use wasm_bindgen::prelude::*;

// Module declarations
pub mod bool_logic;
pub mod intern;

// Optional: Better panic messages in browser console
#[cfg(feature = "console_error_panic_hook")]
pub fn set_panic_hook() {
    console_error_panic_hook::set_once();
}

// Auto-execute on module load
#[wasm_bindgen(start)]
pub fn main() {
    #[cfg(feature = "console_error_panic_hook")]
    set_panic_hook();
}

// Export function to JavaScript
#[wasm_bindgen]
pub fn greet(name: &str) -> String {
    format!("Hello, {}!", name)
}

// Export intern function for path interning
#[wasm_bindgen]
pub fn intern(path: String) -> u32 {
    intern::intern_global(path)
}

// Export resolve function for path resolution
#[wasm_bindgen]
pub fn resolve(id: u32) -> String {
    intern::resolve_global(id).unwrap_or_else(|| String::new())
}

// Export batch_intern for efficient bulk interning
#[wasm_bindgen]
pub fn batch_intern(paths: Vec<String>) -> Vec<u32> {
    intern::batch_intern_global(paths)
}

// Export utility function to get interning table count (for debugging)
#[wasm_bindgen]
pub fn intern_count() -> usize {
    intern::global_count()
}

// Export utility function to clear interning table (for debugging/testing)
#[wasm_bindgen]
pub fn intern_clear() {
    intern::global_clear()
}

// Export evaluate_bool_logic function for WASM
#[wasm_bindgen]
pub fn evaluate_bool_logic(logic: JsValue, state: JsValue) -> Result<bool, JsValue> {
    // Convert JsValue to BoolLogic using serde-wasm-bindgen
    let logic: bool_logic::BoolLogic = serde_wasm_bindgen::from_value(logic)
        .map_err(|e| JsValue::from_str(&format!("Failed to parse BoolLogic: {:?}", e)))?;

    // Convert JsValue to serde_json::Value for state
    let state: serde_json::Value = serde_wasm_bindgen::from_value(state)
        .map_err(|e| JsValue::from_str(&format!("Failed to parse state: {:?}", e)))?;

    // Evaluate and return the result
    Ok(bool_logic::evaluate(&logic, &state))
}

// ============================================================================
// Internal Path Lookup Functions (PathID-based)
// ============================================================================
//
// These internal functions demonstrate efficient path operations using PathID
// instead of strings. They are NOT exported to JavaScript - they are purely
// for internal WASM operations.

/// Internal function to check if a path matches a given pattern using PathID
///
/// This demonstrates how internal WASM functions can use PathID for efficient
/// path matching without needing to work with strings directly.
///
/// # Arguments
/// * `path_id` - The PathID of the path to check
/// * `prefix_id` - The PathID of the required prefix (e.g., "user")
/// * `suffix_id` - The PathID of the required suffix (e.g., "name")
///
/// # Returns
/// true if the path starts with prefix AND ends with suffix, false otherwise
///
/// # Example
/// ```
/// use apex_state_wasm::intern::intern_global;
///
/// let path_id = intern_global("user.profile.name".to_string());
/// let prefix_id = intern_global("user".to_string());
/// let suffix_id = intern_global("name".to_string());
///
/// // This would be called internally, not from JavaScript
/// // assert!(internal_path_matches(path_id, prefix_id, suffix_id));
/// ```
fn internal_path_matches(
    path_id: intern::PathID,
    prefix_id: intern::PathID,
    suffix_id: intern::PathID,
) -> bool {
    // Use PathID-based operations for efficiency
    // These are O(1) for equality checks, only resolving strings when needed
    let starts_with = intern::path_starts_with(path_id, prefix_id).unwrap_or(false);
    let ends_with = intern::path_ends_with(path_id, suffix_id).unwrap_or(false);

    starts_with && ends_with
}

/// Internal function to find paths matching a pattern from a list of PathIDs
///
/// Demonstrates batch PathID-based operations - much more efficient than
/// processing strings when working with many paths.
///
/// # Arguments
/// * `path_ids` - Vector of PathIDs to search
/// * `pattern_id` - PathID to match against (using substring matching)
///
/// # Returns
/// Vector of PathIDs that contain the pattern
///
/// # Example
/// This is an internal function used by WASM operations, not called from JS.
fn internal_filter_paths(
    path_ids: Vec<intern::PathID>,
    pattern_id: intern::PathID,
) -> Vec<intern::PathID> {
    path_ids
        .into_iter()
        .filter(|&path_id| {
            // Use PathID-based contains check - efficient numeric comparison
            // when path_id == pattern_id, otherwise resolve strings only once
            intern::path_contains(path_id, pattern_id).unwrap_or(false)
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_greet() {
        assert_eq!(greet("World"), "Hello, World!");
    }

    #[test]
    fn test_internal_lookup() {
        // Clear any previous state
        intern::global_clear();

        // Intern test paths
        let path1 = intern::intern_global("user.profile.name".to_string());
        let path2 = intern::intern_global("user.settings.theme".to_string());
        let path3 = intern::intern_global("admin.profile.name".to_string());

        // Intern pattern components
        let user_prefix = intern::intern_global("user".to_string());
        let admin_prefix = intern::intern_global("admin".to_string());
        let name_suffix = intern::intern_global("name".to_string());
        let theme_suffix = intern::intern_global("theme".to_string());

        // Test internal_path_matches with PathID operations
        // Should match: starts with "user" AND ends with "name"
        assert!(internal_path_matches(path1, user_prefix, name_suffix));

        // Should NOT match: starts with "user" but ends with "theme"
        assert!(!internal_path_matches(path1, user_prefix, theme_suffix));

        // Should NOT match: ends with "name" but starts with "admin"
        assert!(!internal_path_matches(path1, admin_prefix, name_suffix));

        // Should match: starts with "admin" AND ends with "name"
        assert!(internal_path_matches(path3, admin_prefix, name_suffix));

        // Test internal_filter_paths with PathID operations
        let all_paths = vec![path1, path2, path3];
        let profile_pattern = intern::intern_global("profile".to_string());
        let settings_pattern = intern::intern_global("settings".to_string());

        // Filter paths containing "profile"
        let profile_paths = internal_filter_paths(all_paths.clone(), profile_pattern);
        assert_eq!(profile_paths.len(), 2); // path1 and path3
        assert!(profile_paths.contains(&path1));
        assert!(profile_paths.contains(&path3));

        // Filter paths containing "settings"
        let settings_paths = internal_filter_paths(all_paths.clone(), settings_pattern);
        assert_eq!(settings_paths.len(), 1); // only path2
        assert!(settings_paths.contains(&path2));

        // Clean up
        intern::global_clear();
    }

    #[test]
    fn test_internal_lookup_efficiency() {
        // This test demonstrates the efficiency benefits of PathID-based operations
        intern::global_clear();

        // Intern a path once
        let long_path = "user.profile.settings.preferences.theme.colors.primary".to_string();
        let path_id = intern::intern_global(long_path.clone());

        // Intern it again - should get same ID (O(1) deduplication)
        let path_id_again = intern::intern_global(long_path);
        assert_eq!(path_id, path_id_again);

        // Comparison using PathID is O(1) - just numeric comparison
        assert!(intern::paths_equal(path_id, path_id_again));

        // Create a batch of paths to filter
        let paths = vec![
            intern::intern_global("user.profile.name".to_string()),
            intern::intern_global("user.settings.theme".to_string()),
            intern::intern_global("admin.permissions.read".to_string()),
            intern::intern_global("user.profile.avatar".to_string()),
        ];

        // Filter using PathID operations (efficient - minimal string operations)
        let user_prefix = intern::intern_global("user".to_string());
        let user_paths: Vec<_> = paths
            .iter()
            .filter(|&&path_id| intern::path_starts_with(path_id, user_prefix).unwrap_or(false))
            .collect();

        // Should find 3 paths starting with "user"
        assert_eq!(user_paths.len(), 3);

        intern::global_clear();
    }
}
