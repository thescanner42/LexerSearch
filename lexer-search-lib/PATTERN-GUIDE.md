# Pattern Guide

Patterns are contained in yaml files. The top level keys are:

- [patterns](#patterns)
- [languages](#languages)
- [transform](#transform) (optional)
- [out](#out) (optional)
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

Other than matching token literals, the pattern syntax also provide
[captures](#captures) and the [ellipsis operators](#ellipsis-operators).

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

As mentioned above, the "not too long" rule applies inside brackets. However,
there is a useful extra place to make this rule apply:

- ellipsis not declared in brackets
- lexical bracket reached
  - curly `{}`
  - non python-like language scopes

The scope blocking ellipsis ensures that variable declaration aren't accessible
in the parent lexical scope:

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

Matches merge when the groups are equal, and between the match and the
intersection of the matches already in the finding group, the span of one must
cover the other.

When a match merges into a finding group, the captures are merged together in
the output and the span is the union of all the matches.

If a match with the same name has already been added to a group, the latter
match evicts the previous match. However, if the name is empty or unstated, this
instead duplicates the group and only evicts for one of them.

# Examples

See the [examples](./examples/) folder for more details!
