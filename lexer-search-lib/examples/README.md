# Examples

Each example folder contains both the patterns and the scan content. The output
can be viewed like so:

```
cargo run -- ./lexer-search-lib/examples/grouping/ ./lexer-search-lib/examples/grouping/
```

```
find ./lexer-search-lib/examples/ -maxdepth 1 -mindepth 1 -type d -print0 | while IFS= read -r -d '' dir; do cargo run -- "$dir" "$dir" > "$dir"/results.ndjson; done
```