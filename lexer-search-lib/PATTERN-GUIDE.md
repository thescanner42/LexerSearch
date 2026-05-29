# Pattern Guide

Patterns are contained in yaml files. The top level keys are:

- [patterns](#patterns)
- [templates](#templates)
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
- string literal, or
- identifier/keyword

The type of token that was captured can be determined from the first character
of the captured token.

- number -> `[0-9]+`
- string literal -> `".*"`
- identifier/keyword -> everything else

The first time a capture with a new name is written in a pattern is the creation
of that capture; the captured token is remembered and can be referred to later.
Subsequent mentions of that same capture only allow the pattern to match if the
capture content also matches. For pattern `$ABC $ABC` the first is the capture
creation and the second is a backreference to the created capture `ABC`:

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


> [!WARNING]  
> avoid adjacent reflexive transitions like `... ...` as this can create an
> excessive number of partial matches.

### Not Too Short

When declared in brackets, the ellipsis operators will keep track of bracket
depth and will not exit the bracket scope early. For example rule `test(...)`:

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

This flow of information is deliberately blocked by ellipsis not contained in
brackets (as this could represent any change in lexical level), e.g. `a ..} b
... c ..} d`.

#### Statement Blocking

This is intended for emulating the source-sink functionality seen in other SAST
scanners. The following example detects when a variable is assigned to something
which involves the number 5.

```
$NUM = ..! 5 ..! ;
```

The `..!` operator is very similar to the scope blocking ellipsis: it will not escape the current lexical scope.

However, it also will not exit the current statement; it cannot go past a ';' not in a nested lexical scope.

Its state is reset when reaching `...` or `..}`, (which are both intended to be
used between statements).

## Repetitions

Looping is provided via the `..*` operator; the section surround by `..*` is
matched zero or more times. For example this detects test functions in rust:

```
#[test] // annotation
..* #[...] ..* // other annotations like "#[should_panic]"
..* pub ..*
..* pub(...) ..*
..* $_ ..* // various qualifiers like async, const, etc
fn $_F(...) {...} // fn
```

The total number of captures created in a repitition section must equal zero.

For example, the non-capture `$_` can be stated any number of times since it
does not create a capture. Similarly backreferencing to an existing capture also
does not create captures.

### Multi-repetition

The pattern `start ..* a ..* ..* b ..* end` matches the following example inputs:

- `start a a a a end`
- `start b b b b end`
- `start a a b b end`

The two repitition sections are executed "in series" - the `a`'s must proceed
the `b`'s. However, there are cases where the repititions should instead be
executed in parallel.

A repitition section can be separated with `..|`, indicating that it contains
multiple patterns.

In addition to the above examples, `start ..* a ..| b ..* end` will match:

- `start b a b a end`

### Replace

Inside a repitition section a capture replace can be used. It is written with a
percent prefix like `%ABC`. It overwrites an existing capture.

It's intended for use in propagating a pattern from a source to a sink, with
some number of assignment patterns in between. Recall that the number of
captures created in a repitition is only allowed to sum to zero, otherwise a
pattern's memory usage could increase with each time the repitition is used!
However it is ok to simply replace an existing capture.

There are two variants:

#### Pop Replace

For example:

```
$X = ..! source ..! ;
..}
..* $NEWX = ..! %X ..! ; ..} ..*
sink($X)
```

Matches:

```rust
x = source;
x2 = x;
sink(x);
```

In the example, `%X`:

- checks that the token it receives is equal to that which is captured earlier in
  the pattern by `$X`, then,
- since a different capture (`$NEWX`) was created in the same repitition
  section, `%X` takes the last created capture (`$NEWX`)'s content and puts it
  into `$X`. Since `$NEWX` was taken, after `%X` it's as if it was never created
  and can't be referenced

#### Create Replace

For example:

```
$X = ..! source ..! ;
..}
..* rev_replace($X, &mut %X) ..* ..}
sink($X)
```

In this example, no new capture was created in the repitition (note that `$X` in
the repitition is a backreference since `$X` was created previously). `%X` sets
its content to the capture-able token it receives, replacing `$X`.

> [!TIP]  
> Create replace can also be stated outside of repitition sections to replace an
> existing capture.

## Set Span

The `..^` and `..$` operators overwrite the start and end of the match's span,
respectively. For example pattern:

```
import abc
... ..^ abc.something(...)
```

```py
import abc
# match span starts at 'abc' and not the 'import' above
abc.something(123)
```

`..^` applies to the next token in the pattern, and `..$` applies to the
previous. For example, `abc ..^ def ..$ ghi` will set the span to include only
`def`.

These operators must not be applied in repitition sections nor around
reflexive transitions, e.g. `a ... ..^ b` instead of `a ..^ ... b`.

> [!WARNING]  
> LexerSearch uses constant memory with respect to the scan input and this
> yields a tradeoff - it maintains a configurable maximum number of independent
> partial ("candidate") matches. if many independent partial matches are created
> then the ones with an earlier start position are forgotten.
>
> try to avoid patterns which have a very large match span, and try to write
> patterns which share common prefixes where possible.

## Embed Patterns

LexerSearch has a feature flag to embed your scan patterns into the output binary.
By default this is disabled and the patterns are passed via cli.

```bash
$ cargo run -- <PATTERNS_PATH> <SCAN_PATH> # DEFAULT
$ LEXERSEARCH_EMBED_PATTERNS=<PATTERNS_PATH> cargo run --features=embed-patterns -- <SCAN_PATH>
```

# Templates

To prevent duplication when writing patterns, basic pre-processing list template
substitution is exposed via the `templates` key. For example:

```yaml
patterns:
- |-
  free({{FREE_ARG}})
name: pattern template example
languages:
  - c
templates:
  FREE_ARG:
    - $VAR
    - $_->$VAR
    - $_.$VAR
```

A template starts with `{{` and ends with `}}`. It contains the key lookup to
use in `templates` (e.g. `FREE_ARG` in the above example). Under the key is a
list of values which will be used to substitute the template in the pattern. The
above example expands into the following patterns:

- `free($VAR)`
- `free($_->$VAR)`
- `free($_.$VAR)`

If the same template is stated in multiple position in the pattern then they
will only take on the same value. For example, `{{A}}-{{A}}`, `A`s input of `[red,
blue]` will yield: `red-red`, `blue-blue`.

> [!TIP]  
> As with all other control tokens in LexerSearch, they can be escaped with
> whitespace (e.g. `{ {`)

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
combination of the name and [group name](#group) should be unique for each
pattern.

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
patterns: # "anchor" pattern - small intersection match span
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

The matches are merged into the same finding when the user stated group name is
the same and the match span of the anchor pattern is completely covered by the
span of the other matches.

This on its own is not enough to prevent merging of unrelated matches, as there
could be overlapping matches with patterns of unrelated variables. `group.match`
is a list of variable names whose captured content must match for the findings
to be merged.

By default, if a match being merged has the same name (not to be confused with
the group name) as an existing match, then the new match replaces the old match,
and the old match is discarded. This better models cases like variable
redeclaration / shadowing. However, if `group.unique` is true, then this instead
forks the finding - both are sent to the output.

A match can be cancelled by setting `group.cancel` to true; when merging with
other matches it causes the result to be discarded. This allows, for example,
detection of a pattern not contained in a different pattern.

The span of the output match is the union of the match spans within that group.
However if the span is overriden by the anchor pattern via `..^` or `..$` then
it will take priority.

> [!WARNING]  
> the current implementation for grouping has a small but configurable memory
> limit per group name. this means if a group produces many matches it could
> cause matches to incorrectly not be associated (since earlier matches will be
> forgotten). to avoid this, where possible ensure that the end position of
> matches in the same group occur in a similar position.

# Examples

See the [examples](./examples/README.md) folder for more details!
