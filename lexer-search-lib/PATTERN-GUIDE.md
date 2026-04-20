# Pattern Guide

Patterns are contained in yaml files. The top level keys are:

- [patterns](#patterns)
- [languages](#languages)
- [transform](#transform)
- [out](#out)
- [name](#name)
- [group](#group)

# Patterns

LexerSearch patterns resemble the language being scanned. For example, `=` stated in a pattern will
match `=` stated in the source code.

Basic canonicalization is performed before pattern matching:

- comments are removed
- unnecessary whitespace is removed
- quotation types (`''` vs `""`) are treated as the same
- adjacent string literals are concatenated

Aside from those exceptions, a different pattern is required for each equivalent
way that code can be written.

```rust
let x = 0;
something(x); // DIFFERENT
something(0); // DIFFERENT
```

## Captures

Captures come in one of three forms, only matching a single lexer token and only
the appropriate type of lexer token:

- `#NUMBER_LITERAL`, matching `[0-9]+`
- `&IDENTIFIER`,     e.g. `_my_variable_123`
- `$STRING_LITERAL`, e.g. `"hello world"`

The first time a capture is stated in a pattern is the creation of the capture.
Subsequent mentions of that same capture act as a back-reference to the initial
capture.

> [!WARNING]  
> yaml interprets some characters in a special way, which may lead to unexpected
> results. For example, yaml anchors use the characters `&` and `*`. This
> usually is highlighted in your editor of choice, but when in doubt, consider
> containing the value in `""` quotes or use `|` or `>` multi-line scalar
> values.

> [!IMPORTANT]  
> Although `#ABC`, `&ABC`, and `$ABC`, are unrelated captures, only the
> capture's name ("ABC") populates the output. In these clashing cases, the last
> capture will be the one that appears in the output. Try to avoid this case.

> [!TIP]  
> - A capture name prefixed by `_` is suppressed from the output but will still
>   be used by the [transform](#transform). e.g. `$_ABC`.
> - The prefix character can be escaped via whitespace. e.g. `& ABC` matches the
>   literals `&` then `ABC`.
> - Capture names should be short and UPPERCASE.

## Ellipsis Operators

The ellipsis operators match zero or more lexer tokens. For example: `test(...)`
matches `test(1, 2, 3)`.

Ellipsis operators can be declared inside brackets or outside brackets. Here's
the ways they can be declared inside brackets: `(...)`, `[...]`, `{...}`, or `<
..> >`. LexerSearch doesn't parse. This leads to ambiguity regarding corner
brackets. `>` could represent a "greater than" comparison, or it could represent
the closing of a template argument list in languages like Rust or C++, e.g.
`vector<int>`. LexerSearch's pattern language has the author resolve the
ambiguity, like: `vector< ..> >`. The `..>` is the corner bracket ellipsis. This
indicates that the ellipsis operator is contained inside corner brackets. It is
deduced for other types of brackets.

### Not Too Short

When declared in brackets, the ellipsis operators match from an open bracket to
the corresponding close bracket. This better handles nested brackets. For
example rule `test(...)`:

```c
// don't stop early at first ')'
//        V
   test( (), y, () )
```

### Not Too Long

When declared in brackets they can't escape out of the bracket scope. For the
example rule `test(...)`:

```c
// first match ends where it should
//             V
   test(1, 2, 3); test(4, 5, 6);
//                            ^
// and doesn't continue to here
```

#### Scope Blocking

The scope blocking ellipsis ensures that variable declaration aren't accessible
in the parent lexical scope. It allows the "not too long" rule to apply to
lexical curly brackets:

```
let &VAR = #NUM;
..}
test(&VAR);
```

```rust
let x = 123; // yes!
{
  let x = 456; // no!
}
test(x);
```

The scope blocking ellipsis stores the change in depth from the previous scope
blocking ellipsis. In this example, both `b` and `c` have to be in scope of `a`:

```
a ..} b ..} c
```

```rust
a
{ // +1 depth. stored for next ..}
  b
} // -1 depth. sum of zero; ..} allows it
c
```

However this flow of information is blocked by ellipsis not contained in
brackets, e.g. `a ..} b ... c ..} d`.

## Repetitions

Looping is provided via the `..*` operator. For example:

```
#[test]
..* #[...] ..*
fn &NAME(...) {...}
```

The section surround by `..*` is matched zero or more times. If a repitition
section contains the creation of captures then those captures are forgotten
outside of the section.

## Set Start

The `..^` operator overwrites the start of the match's span. For example
pattern:

```
import abc
... ..^ abc.something(...)
```

```py
import abc
# match span starts at 'abc' and not the 'import' above
abc.something(123)
```

Since LexerSearch operates in bounded memory there is a configurable number of
simultaneous overlapping matches which are followed. Priority is given to
partial matches which start at a later position in the code. `..^` influences
the starting position of a partial match; it increases its priority and causes
other partial matches to be dropped first. `..^` applies to the next
non-reflexive transition in the pattern (e.g. `abc`).

## Embed Patterns

LexerSearch has a feature flag to embed your scan patterns into the output binary.
By default this is disabled and the patterns are passed via cli.

```bash
$ cargo run -- <PATTERNS_PATH> <SCAN_PATH> # DEFAULT
$ LEXERSEARCH_EMBED_PATTERNS=<PATTERNS_PATH> cargo run --features=embed-patterns -- <SCAN_PATH>
```

## Pattern Conflict

The LexerSearch matcher aims to be as simple and fast as possible while still
being featureful. This led to a design decision where partial matches do not
store traversal information. This means that some patterns when combined
together could give ambiguity as to which pattern was matched.

Cases where this ambiguity could arise give an error on load. Detection occurs
when patterns arrive at the same node (via a shared pattern prefix) and then at
the same node they would complete an operation which could lose information on
which pattern to choose when a full match occurs.

LexerSearch is designed under the assumption that these ambiguous cases only
occur when one of the patterns is incorrect. There is typically only one correct
way of creating a pattern which matches code.

### Ellipsis Operators

Pattern `vector< ..> >` searches for a vector declared with a template argument
list. `vector< ... >` searches for "vector" literal, less than, some tokens,
then greater than. Both patterns can't be used together since this gives
ambiguity on which reflexive transition a partial match followed. In this case,
only the former is likely correct. But if you truly need to, then as a
workaround "vector" can be replaced with a capture paired with the transform
checking for equality with "vector".

### Set Start

These two patterns can't be combined: `abc ..^ 123`, `abc 123` since this would
require the partial match to store information related to if its start position
was overriden or not, then only be accepted by the appropriate pattern. Patterns
should instead follow a common flow and set the start in a consistent place.

### Repitition

When a partial match follows a repitition back to a previous node, since it does
not store traversal information, it could still match with a pattern that did
not have the repitition or had a different repitition. For example these two
patterns can't be combined: `..* a ..*` and `..* b ..*`; on accepting `a` or
`b`, the partial match arrives back at the start node, losing information on
which repitition was followed - both patterns could still match.

# Languages

This field indicates what languages the patterns should apply to. For the
language support list, please refer to [here](./src/io/mod.rs).

## Python-like

Most python-like rules should be written like this:

```yaml
patterns: # EITHER OR
  - >
    &VAR=&ABC
    ...
    something(... kwar=&VAR ... )
  - "&VAR=&ABC...something(... kwar=&VAR ... )"
languages:
  - py
name: name
```

instead of this:

```yaml
patterns: # BAD
  - |
    &VAR=&ABC
    ...
    something(... kwar=&VAR ... )
languages:
  - py
name: name
```

Python-like languages are sensitive to line breaks. In the latter example, at
least one line of space is required between the two statements for a match. This
is likely not what was intended.

# Transform

Transform applies after a pattern is completely matched and associate the
capture's name (e.g. name "ABC" for capture "$ABC") with a
[regex](https://docs.rs/regex-lite/latest/regex_lite/#syntax). If the capture's
content does not match the expression then the match is discarded.

If the regex matches "named capture groups" (like `(?<ALG>.+)`) then they will
populate the output. If the named capture group's name matches the capture's
name then the matched values must be equal to accept a match. In this example
the match is accepted because both contain "x":

```rust
let x = "hi";
println!("{x}");
```

```yaml
patterns:
  - >
    &_VAR = $_STR;
    ..}
    println!($_FMT)
languages:
  - rust
name: fmt_test_example
out:
  literal_key: literal_value
transform:
  _FMT: ^\{(?<_VAR>[^}]+)}$

```

> [!IMPORTANT]  
> `transform` has the second highest priority, behind `out`, and will overwrite
> captures if the keys clash.

> [!TIP]  
> Named capture groups starting with `_` are suppressed from the output.

# Out

A pattern match provides output fields like the position of the match and
captures. `out` provides a mechanism which allows the author to state literal
output values which are otherwise not producible from the matched snippet.

> [!IMPORTANT]  
> `out` has the highest priority, and will overwrite captures if the keys clash.

> [!TIP]  
> By convention, if the captures contains "ignore": "true", then the entire
> result should be discarded. When paired with groups (below), this subtly
> allows for patterns which are not contained in other patterns.

# Name

The name field is used to identify which pattern a match is from. The
combination of the name and [group](#group) should be unique for each pattern.

```yaml
patterns:
  - $_STR # match any string
name: my_reasonably_unique_name_here
languages:
  - py
```

```yaml
{..., "name":"my_reasonably_unique_name_here", ...} # output
```

# Group

A pattern may opt in to grouping by stating a non empty group. Suppose the
following code is being matched:

```java
String v = "RSA";
KeyPairGenerator kpg = KeyPairGenerator.getInstance(v);
kpg.initialize(1024);
```

A single pattern can match this whole sequence and extract the algorithm name
and key size. However, any part of the sequence may be unavailable or not identified.

```java
String v = "RSA";
KeyPairGenerator kpg = KeyPairGenerator.getInstance(v);
kpg.initialize(Integer.parseInt("1024")); // not handled
```

One workaround would be to state several different rules; one matches all the
available parts, and the others match fewer parts, etc. This would create
duplicate results.

Another workaround would be to create different rules which only match against a
single part of interest. But this produces unrelated matches which are less
useful than when they are known to apply together. The solution to this is to
use a group:

```yaml
patterns: # "anchor" pattern
  - >
    &_VAR = KeyPairGenerator.getInstance(&_ARG ...)
languages:
  - java
group: java_key
name: anchor
```
```yaml
patterns: # key size from anchor
  - >
    &_VAR = KeyPairGenerator.getInstance(&_ARG ...)
    ..}
    &_VAR.initialize(#KEY_SIZE)
languages:
  - java
group: java_key
name: unique_key_size
```
```yaml
patterns: # alg name from anchor
  - >
    String &_VAR = $ALG;
    ..}
    KeyPairGenerator.getInstance(&_VAR ...)
languages:
  - java
name: alg
group: java_key
```

The above rules together produce a single match:

```yaml
{
    ...
    "name": "java_key",
    "captures": {
        "ALG": "RSA",
        "KEY_SIZE": "1024"
    }
}
```

The matches are merged into the same finding when:
- the user stated group is the same
- between the incoming match and the intersection of the matches already in the
  finding, the span of one must cover the other

This on its own is not enough to prevent merging of unrelated matches, as there
could be overlapping matches with patterns of unrelated variables. If the name
of a capture starts with "SAME" or "_SAME" then the capture name and value must
match in the finding for them to be merged.

By default, if a match being merged has the same name as an existing match, then
the new match replaces the old match, and the old match is discarded. This
better models cases like variable redeclaration / shadowing. However, if the
rule starts with "unique", then this instead forks the finding - both are sent
to the output.

# Examples

See the [examples](./examples/) folder for more details!
