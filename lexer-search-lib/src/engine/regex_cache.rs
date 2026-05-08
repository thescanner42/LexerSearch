use std::collections::{HashMap, hash_map::Entry};

pub struct RegexCache {
    map: HashMap<String, regex_lite::Regex>,
}

impl RegexCache {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

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
