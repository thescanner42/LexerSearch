# Pattern Guide

Patterns are contained in yaml files. The top level keys are:

- [patterns](#patterns)
- [languages](#languages)
- [transform](#transform)
- [out](#out)
- [name](#name)
- [group](#group)

## LexerSearchUI

This guide's schema is for running LexerSearch directly. If running from the
[web UI](https://thescanner42.github.io/LexerSearchUI/) it is slightly
different:

- the "languages" field is instead selected through a dropdown
- the top level is a list of the schema described here instead of a single
  element; this gives the same functionality as running LexerSearch with
  multiple pattern files

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

A capture is written with a dollar prefix (e.g. `$VAR`). It matches one
capture-able lexer token, which is a:

- number
- identifier/keyword
- or string literal

The type of token that was captured can be determined from the first character
of the captured token.

- number -> `[0-9]+`
- identifier/keyword -> `[a-zA-Z_][a-zA-Z0-9_]*`
- string literal -> `".*"`

The first time a capture with a new name is written in a pattern is the creation
of that capture; the captured token is remebered and can be referred to later.
Subsequent mentions of that same capture only allow the pattern to match if the
capture content also matches. For pattern `$ABC $ABC`:

```c
SAME SAME      // YES
SAME DIFFERENT // NO
```

Each capture populates information in the result. However, if a capture's name
starts with "_" (e.g. `$_ABC`) then is it suppressed and does not populate the
output.

If the capture's name is exactly `$_` then this is a non-capturing capture - it
accepts a capture-able token but does not remember it. Mentions of `$_` do not
need their contents to match. For `$_ $_`:

```c
SAME DIFFERENT // YES
```

> [!WARNING]  
> yaml interprets some characters in a special way, which may lead to unexpected
> results. For example, yaml anchors use the characters `&` and `*`. This
> usually is highlighted in your editor of choice, but when in doubt, consider
> containing the value in `""` quotes or use `|` or `>` multi-line scalar
> values.

> [!TIP]  
> - The prefix character can be escaped via whitespace. e.g. `$ ABC` matches the
>   literals `$` then `ABC`.
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
let $VAR = $NUM;
..}
test($VAR);
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

#### Statement Blocking

This is intended for emulating the source-sink functionality seen in other SAST
scanners. The following example detects when a variable is assigned to something
which involves the number 5.

```
$NUM = ..! 5 ..! ;
```

The `..!` operator is very similar to the scope blocking ellipsis: it will not escape the current lexical scope.

However, it also will not exit the current statement: it cannot go past a ';' not in a nested lexical scope.

Its state is reset when reaching `...` or `..}`, (which are both intended to be
used between statements).

## Repetitions

Looping is provided via the `..*` operator. For example:

```
#[test] // annotation
..* #[...] ..* // other annotations like "#[should_panic]"
..* pub ..*
..* pub(...) ..*
..* $_ ..* // various qualifiers like async, const, etc
fn $_F(...) {...} // fn
```

The section surround by `..*` is matched zero or more times. Only non-capturing
captures are allowed inside of a repitition section.

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

## Embed Patterns

LexerSearch has a feature flag to embed your scan patterns into the output binary.
By default this is disabled and the patterns are passed via cli.

```bash
$ cargo run -- <PATTERNS_PATH> <SCAN_PATH> # DEFAULT
$ LEXERSEARCH_EMBED_PATTERNS=<PATTERNS_PATH> cargo run --features=embed-patterns -- <SCAN_PATH>
```

# Languages

This field indicates what languages the patterns should apply to. For the
language support list, please refer to [here](./src/io/mod.rs).

## Python-like

Most python-like rules should be written like this:

```yaml
patterns: # EITHER OR
  - >
    $VAR=$ABC
    ...
    something(... kwar=$VAR ... )
  - "$VAR=$ABC...something(... kwar=$VAR ... )"
languages:
  - py
name: name
```

instead of this:

```yaml
patterns: # BAD
  - |
    $VAR=$ABC
    ...
    something(... kwar=$VAR ... )
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

If the regex matches "named capture groups" (like `(?<MY_NAME>.+)`) then they will
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
    $_VAR = $_STR;
    ..}
    println!($_EXPR)
languages:
  - rust
name: fmt_test_example
out:
  literal_key: literal_value
transform:
  _EXPR: ^\"\{(?<_VAR>[^}]+)}\"$
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

# Name

The name field is used to identify which pattern a match is from. The
combination of the name and [group](#group) should be unique for each pattern.

```yaml
patterns:
  - $_ABC # my expression here
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
String transportation_method = "bicycle";
Tranport transport = TransportGenerator.create(transportation_method);
transport.travel(100);
```

A single pattern can match this whole sequence and extract the transportation
method and travel distance. However, any part of the sequence may be unavailable
or not identified.

```java
String transportation_method = "bicycle";
Tranport transport = TransportGenerator.create(transportation_method);
transport.travel(Integer.parseInt("1024")); // maybe not handled
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
    TransportGenerator.create(...)
languages:
  - java
group:
  name: travel_group
name: anchor
```
```yaml
patterns: # distance from anchor
  - >
    $_VAR = TransportGenerator.create(...)
    ..}
    $_VAR.travel($DISTANCE)
languages:
  - java
transform:
  DISTANCE: ^[0-9]
group:
  name: travel_group
  unique: true
name: distance
```
```yaml
patterns: # travel method from anchor
  - >
    String $_VAR = $_EXP;
    ..}
    TransportGenerator.create($_VAR ...)
languages:
  - java
name: method
transform:
  _EXP: "^\"(?<METHOD>.*)\"$"
group:
  name: travel_group
```

The above rules together produce a single match:

```yaml
{
    ...
    "name": "travel_group",
    "captures": {
        "DISTANCE": "100",
        "METHOD": "bicycle"
    }
}
```

The matches are merged into the same finding when:
- the user stated group name is the same
- between the incoming match and the intersection of the matches already in the
  finding, the span of one must cover the other

This on its own is not enough to prevent merging of unrelated matches, as there
could be overlapping matches with patterns of unrelated variables. `group.match`
is a list of variables names whose captured content must match for the findings
to be merged.

By default, if a match being merged has the same name as an existing match, then
the new match replaces the old match, and the old match is discarded. This
better models cases like variable redeclaration / shadowing. However, if
`group.unique` is true, then this instead forks the finding - both are sent to
the output.

Lastly, a match can be cancelled by setting `group.cancel` to true; when merging
with other matches it causes the result to be discarded. This allows, for
example, detection of a pattern not contained in a different pattern.

# Examples

See the [examples](./examples/README.md) folder for more details!
