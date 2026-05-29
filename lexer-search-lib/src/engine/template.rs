use std::collections::BTreeMap;
use std::sync::LazyLock;

use regex_lite::Regex;

static RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\{\{(?<CONTENT>[^}]*)\}\}").unwrap());

#[derive(Clone)]
struct State {
    output: Vec<u8>,
    bindings: BTreeMap<Box<[u8]>, Box<[u8]>>,
}

// ai assisted
pub fn expand(
    pattern: &[u8],
    templates: &BTreeMap<Box<[u8]>, Vec<Box<[u8]>>>,
    max_expansions: std::num::NonZeroUsize,
) -> Result<Vec<Vec<u8>>, String> {
    let input =
        std::str::from_utf8(pattern).map_err(|_| "pattern is not valid utf8".to_string())?;

    let mut states = vec![State {
        output: Vec::new(),
        bindings: BTreeMap::new(),
    }];

    let mut last = 0;

    for caps in RE.captures_iter(input) {
        let m = caps.get(0).unwrap();
        let content = caps.name("CONTENT").unwrap();

        let literal = &input.as_bytes()[last..m.start()];

        for state in &mut states {
            state.output.extend_from_slice(literal);
        }

        let key: Box<[u8]> = content.as_str().as_bytes().into();

        let mut new_states = Vec::new();

        for mut state in states {
            // Already bound: reuse existing value
            if let Some(existing) = state.bindings.get(&key) {
                state.output.extend_from_slice(existing);
                new_states.push(state);
            } else {
                // First occurrence: expand choices
                let replacements = templates
                    .get(&key)
                    .ok_or_else(|| format!("missing template {}", content.as_str()))?;

                for replacement in replacements {
                    if new_states.len() >= max_expansions.get() {
                        return Err(format!(
                            "exceeded cfg limit of {} template expansions",
                            max_expansions
                        ));
                    }

                    let mut next = state.clone();

                    next.output.extend_from_slice(replacement);

                    next.bindings.insert(key.clone(), replacement.clone());

                    new_states.push(next);
                }
            }
        }

        states = new_states;

        last = m.end();
    }

    let suffix = &input.as_bytes()[last..];

    for state in &mut states {
        state.output.extend_from_slice(suffix);
    }

    Ok(states.into_iter().map(|s| s.output).collect())
}

#[test]
fn test_expansion() {
    let pattern = b"{{A}}-{{B}}".to_vec().into_boxed_slice();

    let mut templates: BTreeMap<Box<[u8]>, Vec<Box<[u8]>>> = BTreeMap::new();

    templates.insert(
        b"A".to_vec().into_boxed_slice(),
        vec![
            b"red".to_vec().into_boxed_slice(),
            b"blue".to_vec().into_boxed_slice(),
        ],
    );

    templates.insert(
        b"B".to_vec().into_boxed_slice(),
        vec![
            b"1".to_vec().into_boxed_slice(),
            b"2".to_vec().into_boxed_slice(),
        ],
    );

    let out = expand(&pattern, &templates, 1000.try_into().unwrap()).unwrap();

    let expected: Vec<Vec<u8>> = vec![
        b"red-1".to_vec(),
        b"red-2".to_vec(),
        b"blue-1".to_vec(),
        b"blue-2".to_vec(),
    ];

    assert_eq!(out, expected);
}

#[test]
fn test_limit() {
    let pattern = b"{{A}}-{{B}}".to_vec().into_boxed_slice();

    let mut templates: BTreeMap<Box<[u8]>, Vec<Box<[u8]>>> = BTreeMap::new();

    templates.insert(
        b"A".to_vec().into_boxed_slice(),
        vec![
            b"red".to_vec().into_boxed_slice(),
            b"blue".to_vec().into_boxed_slice(),
        ],
    );

    templates.insert(
        b"B".to_vec().into_boxed_slice(),
        vec![
            b"1".to_vec().into_boxed_slice(),
            b"2".to_vec().into_boxed_slice(),
        ],
    );

    assert!(expand(&pattern, &templates, 1.try_into().unwrap()).is_err());
}

#[test]
fn test_expansion2() {
    let pattern = b"{{A}}-{{A}}".to_vec().into_boxed_slice();

    let mut templates: BTreeMap<Box<[u8]>, Vec<Box<[u8]>>> = BTreeMap::new();

    templates.insert(
        b"A".to_vec().into_boxed_slice(),
        vec![
            b"red".to_vec().into_boxed_slice(),
            b"blue".to_vec().into_boxed_slice(),
        ],
    );

    let out = expand(&pattern, &templates, 1000.try_into().unwrap()).unwrap();

    let expected: Vec<Vec<u8>> = vec![b"red-red".to_vec(), b"blue-blue".to_vec()];

    assert_eq!(out, expected);
}

#[test]
fn test_escape() {
    let pattern = b"{ {A}}-{{A}}".to_vec().into_boxed_slice();

    let mut templates: BTreeMap<Box<[u8]>, Vec<Box<[u8]>>> = BTreeMap::new();

    templates.insert(
        b"A".to_vec().into_boxed_slice(),
        vec![
            b"red".to_vec().into_boxed_slice(),
            b"blue".to_vec().into_boxed_slice(),
        ],
    );

    let out = expand(&pattern, &templates, 1000.try_into().unwrap()).unwrap();

    let expected: Vec<Vec<u8>> = vec![b"{ {A}}-red".to_vec(), b"{ {A}}-blue".to_vec()];

    assert_eq!(out, expected);
}
