# LexerSearch

LexerSearch is a tool for identifying patterns in source code. Here's an
example:

```yaml

patterns: # example pattern
  - >
    &_VAR = $_STR;
    ...
    my_function(&_VAR)
  - my_function($_STR)
name: transform_regex
languages:
  - rust
transform:
  _STR: ^(?<ALG>[^-]+)-(?<KEY_SIZE>\d+)$
out:
  literal_key: literal_value
```

```rust
let x = "AES-128"; // example code
my_function(x);
```

```json5
{   // example output
    "start": {
        "offset": 4,
        "column": 5,
        "line": 1
    },
    "end": {
        "offset": 49,
        "column": 15,
        "line": 2
    },
    "file": "test_file.rs",
    "name": "transform_regex",
    "captures": {
        "ALG": "AES",
        "KEY_SIZE": "128",
        "literal_key": "literal_value"
    }
}
```

For syntax details and general guidelines see the [pattern
guide](./lexer-search-lib/PATTERN-GUIDE.md).

# Killer Feature

LexerSearch uses linear time and constant space with respect to the scan input.

Bounded memory provides a tradeoff: any workload can be handled gracefully
without OOM, but configurably many simultaneous independent overlapping matches
will be forgotten, and otherwise valid matches will fail at configurably
overlong tokens. 

# How?

Patterns are combined with a trie-like data structure. It supports transitions
which are reflexive (... operator), captures, or back references; partial
matches store capture information to facilitate this. There is also logic to
handle bracket and lexical level so that ... does not extend more or less than
it should.

Partial matches follow transitions as an NFA; a maximum number of partial
matches are allowed, with priority given to later starting position.
 
# Embed Patterns

LexerSearch has a feature flag to embed your scan patterns into the output binary.
By default this is disabled and the patterns are passed via cli.

```bash
$ cargo run -- <PATTERNS_PATH> <SCAN_PATH> # DEFAULT
$ LEXERSEARCH_EMBED_PATTERNS=<PATTERNS_PATH> cargo run --features=embed-patterns -- <SCAN_PATH>
```
