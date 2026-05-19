use std::{
    cmp::Ordering,
    collections::{BTreeMap, BinaryHeap},
    num::NonZero,
    sync::Arc,
};

use crate::{
    engine::{
        canonicalizer::Canonicalizer,
        graph::{Graph, GraphNodeEllipsisEnum, GraphTokenVariant, GroupInfo},
        grouper::Grouper,
        regex_cache::RegexCache,
        token::{Token, TokenVariant},
    },
    lexer::{Lexer, Position},
};

#[derive(Clone, Copy, Default, Debug)]
pub struct PartialMatchBracketState {
    /// depth: enforce too-long and too-short rules. bracket level while
    /// traversing (...)
    depth: u32,
    /// depth for ..}
    scope_blocking: u32,
    /// depth for ..!
    statement_blocking: u32,
}

impl PartialMatchBracketState {
    pub fn pass_through_scope_blocking(&self) -> Self {
        Self {
            depth: 0,
            scope_blocking: self.scope_blocking,
            statement_blocking: self.statement_blocking,
        }
    }
}

#[derive(Clone, Default, Debug)]
pub struct PartialMatch {
    /// byte offset in input
    ///
    /// original start offset in file, nor overriden by ..^
    pub priority_position: usize,
    pub start_position: Position,
    pub trie_position: usize,

    pub bracket_state: PartialMatchBracketState,

    // rc since it's likely to be passed unchanged between transitions
    //
    // Arc so Matcher can be sent between threads
    pub capture_stack: Arc<Vec<Arc<Box<[u8]>>>>,
}

impl PartialMatch {
    fn new(start_position: Position) -> Self {
        let mut ret: PartialMatch = Default::default();
        ret.start_position = start_position;
        ret.priority_position = start_position.offset;
        ret
    }
}

impl PartialEq for PartialMatch {
    fn eq(&self, other: &Self) -> bool {
        self.priority_position == other.priority_position
    }
}

impl Eq for PartialMatch {}

impl PartialOrd for PartialMatch {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for PartialMatch {
    fn cmp(&self, other: &Self) -> Ordering {
        // Reverse ordering so BinaryHeap pops smallest start_position
        other.priority_position.cmp(&self.priority_position)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Default, serde::Serialize)]
pub struct FullMatch {
    pub start: Position,
    pub end: Position,
    #[serde(skip_serializing_if = "String::is_empty")]
    pub name: String,
    #[serde(skip_serializing_if = "GroupInfo::is_default")]
    pub group: GroupInfo,
    #[serde(
        skip_serializing_if = "BTreeMap::is_empty",
        serialize_with = "serialize_captures_as_string_map"
    )]
    pub captures: BTreeMap<Box<[u8]>, Box<[u8]>>,
}

pub fn serialize_captures_as_string_map<S>(
    map: &BTreeMap<Box<[u8]>, Box<[u8]>>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    let mapped: BTreeMap<String, String> = map
        .iter()
        .map(|(k, v)| {
            (
                String::from_utf8_lossy(k).into_owned(),
                String::from_utf8_lossy(v).into_owned(),
            )
        })
        .collect();

    serde::Serialize::serialize(&mapped, serializer)
}

pub struct Matcher<'g> {
    graph: &'g Graph,
    canonicalizer: Canonicalizer,
    matches: BinaryHeap<PartialMatch>,
    max_concurrent_matches: usize,
    grouper: Grouper,
    exprs: RegexCache,
}

impl<'g> Matcher<'g> {
    /// configurable param. how many different partial matches are allowed to be
    /// followed at the same time
    ///
    /// the max_token_length should match the max_token_length used by the
    /// lexer. it's used by the canonicalizer, and sets a bound on the max size
    /// of concatenated tokens (which should be the same but doesn't have to be)
    ///
    /// max_group_memory indicates the max number of full matches to hold onto
    /// while checking for overlapping group matches
    pub fn new(
        graph: &'g Graph,
        max_concurrent_matches: usize,
        max_token_length: NonZero<usize>,
        max_distinct_groups: NonZero<usize>,
        max_group_full_matches: NonZero<usize>,
        max_group_unique_expansions: NonZero<usize>,
    ) -> Self {
        Self {
            graph: graph,
            matches: Default::default(),
            max_concurrent_matches,
            canonicalizer: Canonicalizer::new(max_token_length),
            grouper: Grouper::new(max_distinct_groups, max_group_full_matches, max_group_unique_expansions),
            exprs: Default::default(),
        }
    }

    /// process the complete content provided by r
    pub fn process_and_drain<L: Lexer, R: std::io::Read>(
        &mut self,
        r: &mut R,
        mut lexer: L,
        mut out: impl FnMut(FullMatch),
    ) -> Result<(), String> {
        debug_assert!(!lexer.configured_for_pattern());

        loop {
            let token = match self.canonicalizer.next_and_drain(|| {
                let token = match lexer.next_and_drain(r)? {
                    Some(v) => v,
                    None => return Ok(None),
                };
                Ok(Some(Token::from_lexer_token(token)))
            })? {
                Some(mut v) => {
                    v.variant.add_double_quotes();
                    v
                }
                None => {
                    self.grouper.drain(&mut out);
                    return Ok(());
                }
            };

            debug_assert!(!matches!(token.variant, TokenVariant::Ellipsis(_)));
            debug_assert!(!matches!(token.variant, TokenVariant::Capture(_)));
            debug_assert!(!matches!(token.variant, TokenVariant::OverlongCapture));

            // easiest way of satisfying borrow checker
            let mut grouper = std::mem::replace(&mut self.grouper, Grouper::dummy());
            self.step_token(token, |full_match| {
                grouper.process(full_match, &mut out);
            });
            std::mem::swap(&mut grouper, &mut self.grouper); // put it back
        }
    }

    /// use after process(). drains internal state; signals eof
    pub fn drain<L: Lexer>(&mut self, lexer: &mut L, mut out: impl FnMut(FullMatch)) {
        debug_assert!(!lexer.configured_for_pattern());

        loop {
            let token = match self.canonicalizer.next_and_drain(|| {
                let token = match lexer.drain() {
                    Some(v) => v,
                    None => return Ok(None),
                };
                Ok(Some(Token::from_lexer_token(token)))
            }).unwrap() // unwrap ok since above source closure does not produce read err
             {
                Some(mut v) => {
                    v.variant.add_double_quotes();
                    v
                },
                None => {
                    self.grouper.drain(&mut out);
                    return;
                },
            };

            debug_assert!(!matches!(token.variant, TokenVariant::Ellipsis(_)));
            debug_assert!(!matches!(token.variant, TokenVariant::Capture(_)));
            debug_assert!(!matches!(token.variant, TokenVariant::OverlongCapture));

            // easiest way of satisfying borrow checker
            let mut grouper = std::mem::replace(&mut self.grouper, Grouper::dummy());
            self.step_token(token, |full_match| {
                grouper.process(full_match, &mut out);
            });
            std::mem::swap(&mut grouper, &mut self.grouper); // put it back
        }
    }

    /// same lexer instance should be used between calls to process
    ///
    /// process each segment provided by r
    pub fn process<L: Lexer, R: std::io::Read>(
        &mut self,
        r: &mut R,
        lexer: &mut L,
        mut out: impl FnMut(FullMatch),
    ) -> Result<(), String> {
        debug_assert!(!lexer.configured_for_pattern());

        loop {
            let token = match self.canonicalizer.next(|| {
                let token = match lexer.next(r)? {
                    Some(v) => v,
                    None => return Ok(None),
                };
                Ok(Some(Token::from_lexer_token(token)))
            })? {
                Some(mut v) => {
                    v.variant.add_double_quotes();
                    v
                }
                None => return Ok(()),
            };

            debug_assert!(!matches!(token.variant, TokenVariant::Ellipsis(_)));
            debug_assert!(!matches!(token.variant, TokenVariant::Capture(_)));
            debug_assert!(!matches!(token.variant, TokenVariant::OverlongCapture));

            // easiest way of satisfying borrow checker
            let mut grouper = std::mem::replace(&mut self.grouper, Grouper::dummy());
            self.step_token(token, |full_match| {
                grouper.process(full_match, &mut out);
            });
            std::mem::swap(&mut grouper, &mut self.grouper); // put it back
        }
    }

    /// step forward the pattern matching algorithm, returning any complete
    /// matches
    fn step_token(&mut self, token: Token, mut out: impl FnMut(FullMatch)) {
        let mut next_matches: BinaryHeap<PartialMatch> =
            BinaryHeap::with_capacity(self.max_concurrent_matches);

        fn explore_transitions(
            exprs: &mut RegexCache,
            current: PartialMatch,
            input: &Token,
            graph: &Graph,
            next_matches: &mut BinaryHeap<PartialMatch>,
            max_concurrent_matches: usize,
            out: &mut impl FnMut(FullMatch),
        ) {
            fn handle_partial_match(
                next_matches: &mut BinaryHeap<PartialMatch>,
                max_concurrent_matches: usize,
                element: PartialMatch,
            ) {
                if next_matches.len() >= max_concurrent_matches {
                    next_matches.pop();
                }
                next_matches.push(element);
            }

            fn handle_maybe_full_match(
                expr: &mut RegexCache,
                graph: &Graph,
                element: &PartialMatch,
                last_token: &Token,
                out: &mut impl FnMut(FullMatch),
            ) {
                'outer: for full_match in graph.nodes[element.trie_position].full_match.iter() {
                    out(FullMatch {
                        start: element.start_position,
                        end: last_token.end,
                        name: full_match.name.clone(),
                        group: full_match.group.clone(),
                        captures: {
                            let mut map: BTreeMap<Box<[u8]>, Box<[u8]>> = BTreeMap::new();
                            // populate the captures from the capture stack
                            for (i, key) in full_match.backref_names.iter().enumerate() {
                                map.insert(key.name.clone(), (*element.capture_stack[i]).clone());
                            }

                            // transforms applied second last
                            for (i, key) in full_match.backref_names.iter().enumerate() {
                                if let Some(transform) = &key.transform {
                                    let transform: &regex_lite::Regex = expr.get(transform);
                                    let haystack: &[u8] = &element.capture_stack[i];
                                    // https://github.com/rust-lang/regex/issues/1196
                                    let haystack: &str = match std::str::from_utf8(haystack) {
                                        Ok(v) => v,
                                        Err(_) => continue 'outer,
                                    };
                                    match transform.captures(haystack) {
                                        Some(captures) => {
                                            for (i, m) in captures.iter().enumerate().skip(1) {
                                                let Some(m) = m else {
                                                    continue;
                                                };

                                                let Some(name) =
                                                    transform.capture_names().nth(i).flatten()
                                                else {
                                                    continue; // skip un-named group
                                                };

                                                let bytes: Box<[u8]> = m
                                                    .as_str()
                                                    .as_bytes()
                                                    .to_vec()
                                                    .into_boxed_slice();

                                                if let Some(e) = map.get(name.as_bytes()) {
                                                    if *e != bytes {
                                                        // named capture group matching capture must match
                                                        continue 'outer;
                                                    }
                                                }

                                                map.insert(
                                                    name.as_bytes().to_vec().into_boxed_slice(),
                                                    bytes,
                                                );
                                            }
                                        }
                                        None => continue 'outer, // no match at all skip finding
                                    }
                                }
                            }
                            // out applied last
                            for (k, v) in full_match.out.iter() {
                                map.insert(k.clone(), v.clone());
                            }
                            map
                        },
                    });
                }
            }

            match &input.variant {
                TokenVariant::String(items) | TokenVariant::Identifier(items) => {
                    // non-capture transition
                    let bracket_state = current.bracket_state.pass_through_scope_blocking();
                    for v in graph.nodes[current.trie_position].non_capture.iter() {
                        v.handle(|index, set_start| {
                            let new_start_position = if set_start {
                                input.start
                            } else {
                                current.start_position
                            };

                            let m = PartialMatch {
                                start_position: new_start_position,
                                priority_position: current.priority_position,
                                trie_position: index,
                                capture_stack: current.capture_stack.clone(),
                                bracket_state,
                            };
                            handle_maybe_full_match(exprs, graph, &m, &input, out);
                            handle_partial_match(next_matches, max_concurrent_matches, m);
                        });
                    }

                    // capture transition
                    for v in graph.nodes[current.trie_position].create_capture.iter() {
                        let mut new_stack = (*current.capture_stack).clone();
                        new_stack.push(Arc::new(items.to_vec().into_boxed_slice()));
                        let new_stack = Arc::new(new_stack);
                        v.handle(|index, set_start| {
                            let new_start_position = if set_start {
                                input.start
                            } else {
                                current.start_position
                            };

                            let m = PartialMatch {
                                start_position: new_start_position,
                                priority_position: current.priority_position,
                                trie_position: index,
                                capture_stack: new_stack.clone(), // rc
                                bracket_state,
                            };
                            handle_maybe_full_match(exprs, graph, &m, &input, out);
                            handle_partial_match(next_matches, max_concurrent_matches, m);
                        });
                    }

                    // backref
                    for (capture_index, dst) in graph.nodes[current.trie_position].backref.iter() {
                        if *items == *current.capture_stack[*capture_index] {
                            for dst in dst.iter() {
                                dst.handle(|v, set_start| {
                                    let m = PartialMatch {
                                        start_position: if set_start {
                                            input.start
                                        } else {
                                            current.start_position
                                        },
                                        priority_position: current.priority_position,
                                        trie_position: v,
                                        capture_stack: current.capture_stack.clone(),
                                        bracket_state: current
                                            .bracket_state
                                            .pass_through_scope_blocking(),
                                    };
                                    handle_maybe_full_match(exprs, graph, &m, &input, out);
                                    handle_partial_match(next_matches, max_concurrent_matches, m);
                                });
                            }
                        }
                    }

                    // backref replace
                    for (capture_index, dst) in
                        graph.nodes[current.trie_position].backref_replace.iter()
                    {
                        if *items == *current.capture_stack[*capture_index] {
                            for dst in dst.iter() {
                                dst.handle(|v, set_start| {
                                    // replace content at position
                                    let mut new_stack = (*current.capture_stack).clone();
                                    let last = new_stack.pop().unwrap();
                                    new_stack[*capture_index] = last;
                                    let new_stack = Arc::new(new_stack);
                                    let m = PartialMatch {
                                        start_position: if set_start {
                                            input.start
                                        } else {
                                            current.start_position
                                        },
                                        priority_position: current.priority_position,
                                        trie_position: v,
                                        capture_stack: new_stack,
                                        bracket_state: current
                                            .bracket_state
                                            .pass_through_scope_blocking(),
                                    };
                                    handle_maybe_full_match(exprs, graph, &m, &input, out);
                                    handle_partial_match(next_matches, max_concurrent_matches, m);
                                });
                            }
                        }
                    }

                    // create replace
                    for (capture_index, dst) in
                        graph.nodes[current.trie_position].create_replace.iter()
                    {
                        for dst in dst.iter() {
                            dst.handle(|v, set_start| {
                                let mut new_stack = (*current.capture_stack).clone();
                                new_stack[*capture_index] =
                                    Arc::new(items.to_vec().into_boxed_slice());
                                let new_stack = Arc::new(new_stack);
                                let m = PartialMatch {
                                    start_position: if set_start {
                                        input.start
                                    } else {
                                        current.start_position
                                    },
                                    priority_position: current.priority_position,
                                    trie_position: v,
                                    capture_stack: new_stack,
                                    bracket_state: current
                                        .bracket_state
                                        .pass_through_scope_blocking(),
                                };
                                handle_maybe_full_match(exprs, graph, &m, &input, out);
                                handle_partial_match(next_matches, max_concurrent_matches, m);
                            });
                        }
                    }
                }

                _ => {}
            }

            if current.bracket_state.depth != 0 {
                // blocked by NOT TOO SHORT rule
            } else {
                // attempt literal transition
                let literal: Option<GraphTokenVariant> = match &input.variant {
                    TokenVariant::Byte(b) => Some(GraphTokenVariant::Byte(*b)),
                    TokenVariant::String(items) => {
                        Some(GraphTokenVariant::Captureable(items.clone()))
                    }
                    TokenVariant::Identifier(items) => {
                        Some(GraphTokenVariant::Captureable(items.clone()))
                    }
                    TokenVariant::LexicalLevelChange(v) => {
                        Some(GraphTokenVariant::LexicalLevelChange(*v))
                    }
                    _ => None,
                };

                if let Some(literal) = literal {
                    if let Some(v) = graph.nodes[current.trie_position].edge.get(&literal) {
                        for v in v {
                            v.handle(|v, set_start| {
                                let m = PartialMatch {
                                    start_position: if set_start {
                                        input.start
                                    } else {
                                        current.start_position
                                    },
                                    priority_position: current.priority_position,
                                    trie_position: v,
                                    capture_stack: current.capture_stack.clone(), // rc
                                    bracket_state: current
                                        .bracket_state
                                        .pass_through_scope_blocking(),
                                };
                                handle_maybe_full_match(exprs, graph, &m, &input, out);
                                handle_partial_match(next_matches, max_concurrent_matches, m);
                            });
                        }
                    }
                }
            }

            for v in graph.nodes[current.trie_position].scope_blocking.iter() {
                let scope_blocking_delta: i32 = match input.variant {
                    TokenVariant::Byte(b'{') => 1,
                    TokenVariant::Byte(b'}') => -1,
                    TokenVariant::LexicalLevelChange(v) => v,
                    _ => 0,
                };

                let new_scope_blocking = match current
                    .bracket_state
                    .scope_blocking
                    .checked_add_signed(scope_blocking_delta)
                {
                    None => {
                        // too long
                        continue;
                    }
                    Some(v) => v,
                };

                let mut next = current.clone();
                next.bracket_state.scope_blocking = new_scope_blocking;
                // ..} is used between statements so reset the per statement state
                next.bracket_state.statement_blocking = 0;
                next.trie_position = *v;
                handle_partial_match(next_matches, max_concurrent_matches, next);
            }

            for v in graph.nodes[current.trie_position].statement_blocking.iter() {
                let statement_blocking_delta: i32 = match input.variant {
                    TokenVariant::Byte(b'{') => 1,
                    TokenVariant::Byte(b'}') => -1,
                    TokenVariant::LexicalLevelChange(v) => v,
                    _ => 0,
                };

                let new_statement_blocking = match current
                    .bracket_state
                    .statement_blocking
                    .checked_add_signed(statement_blocking_delta)
                {
                    None => {
                        // too long
                        continue;
                    }
                    Some(v) => v,
                };

                if new_statement_blocking == 0
                    && (input.variant == TokenVariant::Byte(b';')
                        || input.variant == TokenVariant::LexicalLevelChange(0))
                {
                    continue;
                }

                let mut next = current.clone();
                next.bracket_state.statement_blocking = new_statement_blocking;
                next.trie_position = *v;
                handle_partial_match(next_matches, max_concurrent_matches, next);
            }

            match &graph.nodes[current.trie_position].ellipsis {
                GraphNodeEllipsisEnum::None => {}
                GraphNodeEllipsisEnum::UncontainedAndCorner(uncontained_items, corner_items) => {
                    // uncontained ellipsis doesn't care about not too long and not too short rules
                    for v in uncontained_items {
                        let mut next = current.clone();
                        next.bracket_state = Default::default();
                        next.trie_position = *v;
                        handle_partial_match(next_matches, max_concurrent_matches, next);
                    }

                    let depth_delta: i32 = match input.variant {
                        TokenVariant::Byte(b'<') => 1,
                        TokenVariant::Byte(b'>') => -1,
                        _ => 0,
                    };
                    if let Some(new_depth) =
                        current.bracket_state.depth.checked_add_signed(depth_delta)
                    {
                        for v in corner_items {
                            let mut next = current.clone();
                            next.bracket_state.depth = new_depth;
                            next.trie_position = *v;
                            handle_partial_match(next_matches, max_concurrent_matches, next);
                        }
                    }
                }
                GraphNodeEllipsisEnum::Round(items)
                | GraphNodeEllipsisEnum::Square(items)
                | GraphNodeEllipsisEnum::Curly(items) => {
                    let depth_delta: i32 = match &graph.nodes[current.trie_position].ellipsis {
                        GraphNodeEllipsisEnum::Round(_) => match input.variant {
                            TokenVariant::Byte(b'(') => 1,
                            TokenVariant::Byte(b')') => -1,
                            _ => 0,
                        },
                        GraphNodeEllipsisEnum::Square(_) => match input.variant {
                            TokenVariant::Byte(b'[') => 1,
                            TokenVariant::Byte(b']') => -1,
                            _ => 0,
                        },
                        GraphNodeEllipsisEnum::Curly(_) => match input.variant {
                            TokenVariant::Byte(b'{') => 1,
                            TokenVariant::Byte(b'}') => -1,
                            _ => 0,
                        },
                        _ => unreachable!(),
                    };
                    if let Some(new_depth) =
                        current.bracket_state.depth.checked_add_signed(depth_delta)
                    {
                        for v in items {
                            let mut next = current.clone();
                            next.bracket_state.depth = new_depth;
                            next.trie_position = *v;
                            handle_partial_match(next_matches, max_concurrent_matches, next);
                        }
                    }
                }
            }
        }

        for m in std::mem::take(&mut self.matches).into_iter() {
            explore_transitions(
                &mut self.exprs,
                m,
                &token,
                &self.graph,
                &mut next_matches,
                self.max_concurrent_matches,
                &mut out,
            );
        }

        explore_transitions(
            &mut self.exprs,
            PartialMatch::new(token.start),
            &token,
            &self.graph,
            &mut next_matches,
            self.max_concurrent_matches,
            &mut out,
        );

        self.matches = next_matches;
    }
}
