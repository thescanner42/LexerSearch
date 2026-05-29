use std::collections::{HashMap, hash_map::Entry};

/// in the graph, when there is a full match, it then checks the transform
/// (which contains regular expressions)
///
/// there was some issues with serializing / deserializing regex_list::Regex, so
/// this is used instead
///
/// this solution is a bit awkward but works. todo
pub struct RegexCache {
    map: HashMap<String, regex_lite::Regex>,
}

impl RegexCache {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    /// pattern is already known to be a valid expr - checked before serializing
    /// when the graph is being built
    pub fn get(&mut self, pattern: &str) -> &regex_lite::Regex {
        match self.map.entry(pattern.to_owned()) {
            Entry::Occupied(e) => e.into_mut(),
            Entry::Vacant(e) => {
                let compiled = regex_lite::Regex::new(pattern).unwrap();
                e.insert(compiled)
            }
        }
    }
}

impl Default for RegexCache {
    fn default() -> Self {
        Self::new()
    }
}
