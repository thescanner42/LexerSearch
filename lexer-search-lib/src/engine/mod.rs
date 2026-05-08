mod canonicalizer; // prefilter
pub mod graph;
#[cfg(test)]
pub mod graph_test;
mod grouper; // postfilter
pub mod matcher;
pub mod matchers;
pub mod regex_cache;
pub mod token;
