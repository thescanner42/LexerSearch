use std::{
    cmp::Ordering,
    collections::{BTreeMap, BinaryHeap, HashMap},
    num::NonZero,
    sync::Arc,
    usize,
};

use serde::de::Error as SerdeError;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

use crate::{
    engine::{canonicalizer::Canonicalizer, grouper::Grouper},
    lexer::{EllipsisEnum, Lexer, LexerToken, LexerTokenVariant, MaybeSliceRef, Position},
};

#[derive(Debug, Copy, Clone, Default, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum BracketType {
    #[default]
    Round,
    Square,
    Corner,
    Curly,
}

/// bounded length owned token
#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum TokenVariant {
    /// stops non-reflexive patterns
    OverlongIdentifier,
    /// stops non-reflexive patterns
    OverlongNumber,
    /// stops non-reflexive patterns
    OverlongString,

    /// some character. like "*". this has lowest precedence. for example in
    /// c-like languages the '{' character instead is represented with an
    /// increment in lexical level
    Byte(u8),
    /// none indicates token was too long
    Number(Box<[u8]>),
    String(Box<[u8]>),
    Identifier(Box<[u8]>),
    /// see LexerTokenVariant for more details
    LexicalLevelChange(i32),

    /// only used by pattern creation logic, not matching logic
    Ellipsis(EllipsisEnum),
    /// only used by pattern creation logic, not matching logic
    Capture(Box<[u8]>),
    /// only used by pattern creation logic, not matching logic
    OverlongCapture,
}

#[derive(Clone)]
pub struct Token {
    pub variant: TokenVariant,
    pub start: Position,
    pub end: Position,
}

impl Token {
    fn from_lexer_token(token: LexerToken) -> Self {
        Token {
            variant: match token.variant {
                LexerTokenVariant::Byte(b) => TokenVariant::Byte(b),
                LexerTokenVariant::Number(MaybeSliceRef::Len(_)) => TokenVariant::OverlongNumber,
                LexerTokenVariant::String(MaybeSliceRef::Len(_), _) => TokenVariant::OverlongString,
                LexerTokenVariant::Identifier(MaybeSliceRef::Len(_)) => {
                    TokenVariant::OverlongIdentifier
                }
                LexerTokenVariant::Number(MaybeSliceRef::Some(items)) => {
                    TokenVariant::Number(items.to_vec().into_boxed_slice())
                }
                LexerTokenVariant::String(MaybeSliceRef::Some(items), quote_len) => {
                    TokenVariant::String(
                        items[quote_len..items.len() - quote_len]
                            .to_vec()
                            .into_boxed_slice(),
                    )
                }
                LexerTokenVariant::Identifier(MaybeSliceRef::Some(items)) => {
                    TokenVariant::Identifier(items.to_vec().into_boxed_slice())
                }
                LexerTokenVariant::LexicalLevelChange(delta, _maybe_items) => {
                    TokenVariant::LexicalLevelChange(delta)
                }
                // not produced by lexer not configured for pattern
                LexerTokenVariant::Ellipsis(b) => TokenVariant::Ellipsis(b),
                LexerTokenVariant::Capture(MaybeSliceRef::Len(_)) => TokenVariant::OverlongCapture,
                LexerTokenVariant::Capture(MaybeSliceRef::Some(items)) => {
                    TokenVariant::Capture(items.to_vec().into_boxed_slice())
                }
            },
            start: token.start,
            end: token.end,
        }
    }
}

/// a non-reflexive transition in the trie-like structure
#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum Transition {
    /// token literal was matched
    Literal(TokenVariant),
    CaptureString,
    CaptureNumber,
    CaptureIdentifier,
    /// index N on capture stack was matched
    Backref(usize),
}

#[derive(Default, Debug, Clone, Serialize, Deserialize)]
pub struct BackrefInfo {
    name: Box<[u8]>,
    #[serde(
        serialize_with = "serialize_regex_opt",
        deserialize_with = "deserialize_regex_opt"
    )]
    transform: Option<regex_lite::Regex>,
}

fn serialize_regex_opt<S>(r: &Option<regex_lite::Regex>, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    match r {
        Some(regex) => serializer.serialize_some(regex.as_str()),
        None => serializer.serialize_none(),
    }
}

fn deserialize_regex_opt<'de, D>(deserializer: D) -> Result<Option<regex_lite::Regex>, D::Error>
where
    D: Deserializer<'de>,
{
    let opt: Option<&str> = Option::deserialize(deserializer)?;
    if let Some(expr) = opt {
        regex_lite::Regex::new(expr)
            .map(Some)
            .map_err(D::Error::custom)
    } else {
        Ok(None)
    }
}

/// information used after complete match found
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct PatternInfo {
    /// associates the backref index with the name and transform stated in the
    /// pattern
    backref_names: Box<[BackrefInfo]>,
    /// as stated in the pattern, arbitrary user defined output for the finding
    out: BTreeMap<Box<[u8]>, Box<[u8]>>,

    /// as stated by pattern
    name: String,
    group: String,
}

/// indicates the type of reflexive transition (one that leads back to the same
/// node). e.g. caused by ...
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum ReflexiveTransition {
    /// ellipsis operator not contained in any brackets
    NormalEllipsis,
    /// ellipsis operator contained in specified type of brackets
    ContainedEllipsis(BracketType),
    /// similar to normal, but scope blocking enabled
    SBE,
}

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct JumpInfo {
    return_trie_position: usize,
    new_captures_since_loop_entry: usize,
}

#[derive(Debug, serde::Serialize, serde::Deserialize)]
struct TransitionTo {
    /// dst trie node
    transition: usize,

    loops: Option<JumpInfo>,

    /// from ..^
    set_start: bool,
}

#[derive(Default, Debug, serde::Serialize, serde::Deserialize)]
struct TrieNode {
    /// empty if no complete matches
    full_match: Vec<PatternInfo>,
    next: HashMap<Transition, TransitionTo>,
    reflexive: Option<ReflexiveTransition>,
}

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct Trie {
    nodes: Vec<TrieNode>,
}

impl Default for Trie {
    fn default() -> Self {
        Self {
            // default has index 0 valid but doesn't lead anywhere. required since
            // index 0 is the start index and assumed to be ok
            nodes: vec![Default::default()],
        }
    }
}

impl Trie {
    /// add the pattern to self. if an error is returned, then self was reverted
    /// to default (start again or exit)
    ///
    /// rule_name should be some unique name
    ///
    /// out is the arbitrary outputs defined in the pattern
    pub fn add_pattern<L: Lexer, R: std::io::Read>(
        &mut self,
        r: &mut R,
        out: &BTreeMap<Box<[u8]>, Box<[u8]>>,
        name: String,
        group: String,
        transform: &BTreeMap<Box<[u8]>, String>,
        mut lexer: L,
        max_token_length: NonZero<usize>,
    ) -> Result<(), String> {
        debug_assert!(lexer.configured_for_pattern());
        let mut trie = std::mem::take(self);

        let mut canonicalizer = Canonicalizer::new(max_token_length);

        // always points to a valid index (trie has min len of 1)
        let mut trie_position: usize = 0;

        // None if not currently inside ..+, Some otherwise
        let mut trie_jump_position: Option<JumpInfo> = None;
        // the previous TransitionTo that was followed. helpful for overall
        // construction, when something needs to apply to the previous thing.
        // contains the trie node and transition from that node
        let mut previous_transition: Option<(usize, Transition)> = None;

        // implement ..^ applies to next non reflexived transition
        let mut set_start: bool = false;

        // add a check for ..+
        let mut node_is_new: bool = false;
        // add a check for ..+
        let mut requires_close_repitition_next = false;

        let mut captures: HashMap<Box<[u8]>, usize> = Default::default();
        let mut capture_count_increment = 0;

        // used to determine if ... is contained in what type of brackets
        let mut bracket_stack: Vec<BracketType> = Default::default();

        loop {
            let token = match canonicalizer.next_and_drain(|| {
                let token = match lexer.next_and_drain(r)? {
                    Some(s) => s,
                    None => return Ok(None),
                };

                let token = Token::from_lexer_token(token);
                match token.variant {
                    TokenVariant::OverlongIdentifier
                    | TokenVariant::OverlongNumber
                    | TokenVariant::OverlongString
                    | TokenVariant::OverlongCapture => {
                        // do not emit TokenVariant::OverlongABC here. write a sane pattern!
                        return Err(format!(
                            "{} line {} col {}: token too long",
                            name, token.start.line, token.start.column
                        ));
                    }
                    _ => {}
                };
                return Ok(Some(token));
            })? {
                Some(v) => v,
                None => {
                    fn reverse_map(
                        captures: HashMap<Box<[u8]>, usize>,
                        transform: &BTreeMap<Box<[u8]>, String>,
                    ) -> Result<Box<[BackrefInfo]>, String> {
                        let len = captures.len();
                        let mut reversed: Vec<BackrefInfo> = Vec::with_capacity(len);
                        reversed.resize_with(len, || Default::default());
                        for (key, index) in captures {
                            reversed[index].name = key[1..].to_vec().into_boxed_slice();

                            let r: &Box<[u8]> = &reversed[index].name;
                            if let Some(expr) = transform.get(r) {
                                // if the transform accepts this backref then
                                // set it in the pattern info
                                let compiled = regex_lite::Regex::new(&expr).map_err(|e| {
                                    format!("regex compilation error of '{expr}': {e}")
                                })?;
                                reversed[index].transform = Some(compiled);
                            }
                        }
                        Ok(reversed.into_boxed_slice())
                    }

                    let mut new_out: BTreeMap<Box<[u8]>, Box<[u8]>> = Default::default();
                    for (k, v) in out.iter() {
                        new_out.insert(k.clone(), v.clone());
                    }

                    trie.nodes[trie_position].full_match.push(PatternInfo {
                        backref_names: reverse_map(captures, transform)?,
                        out: new_out,
                        name: name.clone(),
                        group: group.clone(),
                    });
                    let _ = std::mem::replace(self, trie);
                    return Ok(());
                }
            };

            enum PatternProduct {
                Transition(Transition),
                ReflexiveTransition(ReflexiveTransition),
                Jump,
                SetStart,
            }

            let product = match token.variant {
                TokenVariant::OverlongIdentifier
                | TokenVariant::OverlongNumber
                | TokenVariant::OverlongString
                | TokenVariant::OverlongCapture => unreachable!(), // discarded above
                TokenVariant::Byte(b) => {
                    let maybe_bracket_open = match b {
                        b'{' => Some(BracketType::Curly),
                        b'[' => Some(BracketType::Square),
                        b'(' => Some(BracketType::Round),
                        // corner brackets not used here. see cbee
                        _ => None, // not a bracket byte from the pattern
                    };

                    if let Some(bracket_type) = maybe_bracket_open {
                        bracket_stack.push(bracket_type);
                    }

                    let maybe_bracket_close = match b {
                        b'}' => Some(BracketType::Curly),
                        b']' => Some(BracketType::Square),
                        b')' => Some(BracketType::Round),
                        _ => None,
                    };

                    if let Some(bracket_type) = maybe_bracket_close {
                        let mut eq = false;
                        if let Some(pushed_type) = bracket_stack.last() {
                            if bracket_type == *pushed_type {
                                eq = true;
                            }
                        }

                        if eq {
                            bracket_stack.pop();
                        }
                    }
                    PatternProduct::Transition(Transition::Literal(token.variant))
                }
                TokenVariant::Ellipsis(ellipsis_type) => match ellipsis_type {
                    EllipsisEnum::Normal => match bracket_stack.last() {
                        Some(bracket_type) => PatternProduct::ReflexiveTransition(
                            ReflexiveTransition::ContainedEllipsis(*bracket_type),
                        ),
                        None => {
                            PatternProduct::ReflexiveTransition(ReflexiveTransition::NormalEllipsis)
                        }
                    },
                    EllipsisEnum::CBE => PatternProduct::ReflexiveTransition(
                        ReflexiveTransition::ContainedEllipsis(BracketType::Corner),
                    ),
                    EllipsisEnum::SBE => {
                        PatternProduct::ReflexiveTransition(ReflexiveTransition::SBE)
                    }
                    EllipsisEnum::Jump => PatternProduct::Jump,
                    EllipsisEnum::SetStart => PatternProduct::SetStart,
                },
                TokenVariant::Capture(items) => {
                    PatternProduct::Transition(match captures.get(&items) {
                        Some(ref_num) => {
                            // capture was stated previously, this is a
                            // backreference
                            Transition::Backref(*ref_num)
                        }
                        None => {
                            if let Some(v) = trie_jump_position.as_mut() {
                                v.new_captures_since_loop_entry += 1;
                            }
                            // this capture hasn't been seen yet
                            let ret = match items[0] {
                                b'&' => Transition::CaptureIdentifier,
                                b'#' => Transition::CaptureNumber,
                                b'$' => Transition::CaptureString,
                                _ => unreachable!(),
                            };
                            captures.insert(items.into(), capture_count_increment);
                            capture_count_increment += 1;
                            ret
                        }
                    })
                }
                _ => PatternProduct::Transition(Transition::Literal(token.variant)),
            };

            if requires_close_repitition_next {
                requires_close_repitition_next = false;
                if matches!(product, PatternProduct::Jump) && trie_jump_position.is_some() {
                    // ok - properly closed
                } else {
                    return Err(
                        "pattern conflict: repitition must match, already exists".to_string()
                    );
                }
            }

            match product {
                PatternProduct::SetStart => {
                    set_start = true;
                }
                PatternProduct::Jump => {
                    match trie_jump_position.take() {
                        None => {
                            // mark the current position for later reference
                            // (e.g. the token before the first ..+)
                            trie_jump_position = Some(JumpInfo {
                                return_trie_position: trie_position,
                                new_captures_since_loop_entry: 0,
                            });
                        }
                        Some(v) => {
                            // set the previous transition (before the second
                            // ..+) loop to the position marked by the first ..+
                            if let Some(previous_transition) = &previous_transition {
                                let previous_transition = trie.nodes[previous_transition.0]
                                    .next
                                    .get_mut(&previous_transition.1)
                                    .unwrap(); // safe since it was added previously

                                match &previous_transition.loops {
                                    Some(loops) => {
                                        if loops.return_trie_position == v.return_trie_position {
                                            // ok: same loop e.g.
                                            // ..+ a ..+ 123
                                            // ..+ a ..+ 456
                                        } else {
                                            // not ok. same source but different destination e.g.
                                            // 123 a a ..+ a ..+ 456
                                            // 123 a ..+ a a ..+ 456
                                            return Err("pattern conflict: repitition has different destinations".to_string());
                                        }
                                    }
                                    None => {
                                        if node_is_new {
                                            // ok: the last transition was newly
                                            // layed down by this same pattern
                                        } else if previous_transition.loops.is_none() {
                                            // 123 a 456
                                            // 123 ..+ a ..+ 456
                                            return Err("pattern conflict: repitition must match, does not already exist".to_string());
                                        }
                                        previous_transition.loops = Some(v);
                                    }
                                }
                            } else {
                                // e.g. ..+ ..+
                                return Err("..+ loop lacks a source or destination".to_string());
                            }
                        }
                    }
                    previous_transition = None;
                }
                PatternProduct::Transition(transition) => {
                    previous_transition = Some((trie_position, transition.clone()));
                    let len = trie.nodes.len();
                    let set_start = std::mem::take(&mut set_start);
                    match trie.nodes[trie_position].next.entry(transition) {
                        std::collections::hash_map::Entry::Occupied(occupied_entry) => {
                            node_is_new = false;
                            trie_position = occupied_entry.get().transition;
                            if occupied_entry.get().set_start != set_start {
                                if set_start {
                                    return Err("pattern conflict: set start must match, does not already exist".to_string());
                                } else {
                                    return Err(
                                        "pattern conflict: set start must match, already exists"
                                            .to_string(),
                                    );
                                }
                            }
                            if occupied_entry.get().loops.is_some() {
                                // a loop is seen at this transition so it is
                                // expected that this pattern also closes the
                                // loop at this same position (next token)
                                requires_close_repitition_next = true;
                            }
                        }
                        std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                            node_is_new = true;
                            vacant_entry.insert(TransitionTo {
                                // create transition
                                transition: len,
                                loops: Default::default(),
                                set_start,
                            });
                            trie_position = len;
                            trie.nodes.push(TrieNode::default());
                        }
                    }
                }
                PatternProduct::ReflexiveTransition(reflexive_transition) => {
                    previous_transition = None;
                    match trie.nodes[trie_position].reflexive {
                        Some(v) => {
                            if v != reflexive_transition {
                                return Err("pattern conflict: only one reflexive transition allowed"
                                    .to_string());
                            }
                        }
                        None => {
                            trie.nodes[trie_position].reflexive = Some(reflexive_transition);
                        }
                    }
                }
            }
        }
    }
}

#[derive(Clone, Default, Debug)]
pub struct PartialMatchBracketState {
    /// depth: enforce too-long and too-short rules. bracket level while
    /// traversing (...)
    depth: u32,
    /// indicates currently inside ..} and non zero bracket count
    ///
    /// allows for lexical scope bypass in too-long and too-short rules
    sbe_storage: u32,
}

impl PartialMatchBracketState {
    pub fn pass_through_sbe_info(&self) -> Self {
        Self {
            depth: 0,
            sbe_storage: self.sbe_storage,
        }
    }
}

#[derive(Clone, Default, Debug)]
pub struct PartialMatch {
    /// byte offset in input
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
        ret
    }
}

impl PartialEq for PartialMatch {
    fn eq(&self, other: &Self) -> bool {
        self.start_position == other.start_position
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
        other.start_position.offset.cmp(&self.start_position.offset)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct FullMatch {
    pub start: Position,
    pub end: Position,
    pub name: String,
    pub group: String,
    pub captures: BTreeMap<Box<[u8]>, Box<[u8]>>,
}

pub struct Matcher<'trie> {
    trie: &'trie Trie,
    canonicalizer: Canonicalizer,
    matches: BinaryHeap<PartialMatch>,
    max_concurrent_matches: usize,
    grouper: Grouper,
}

impl<'trie> Matcher<'trie> {
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
        trie: &'trie Trie,
        max_concurrent_matches: usize,
        max_token_length: NonZero<usize>,
        max_group_memory: NonZero<usize>,
    ) -> Self {
        Self {
            trie: trie,
            matches: Default::default(),
            max_concurrent_matches,
            canonicalizer: Canonicalizer::new(max_token_length),
            grouper: Grouper::new(max_group_memory),
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
                Some(v) => v,
                None => {
                    self.grouper.drain(&mut out);
                    return Ok(());
                }
            };

            debug_assert!(!matches!(token.variant, TokenVariant::Ellipsis(_)));
            debug_assert!(!matches!(token.variant, TokenVariant::Capture(_)));
            debug_assert!(!matches!(token.variant, TokenVariant::OverlongCapture));

            // easiest way of satisfying borrow checker
            let mut grouper = std::mem::take(&mut self.grouper);
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
                Some(v) => v,
                None => {
                    self.grouper.drain(&mut out);
                    return;
                },
            };

            debug_assert!(!matches!(token.variant, TokenVariant::Ellipsis(_)));
            debug_assert!(!matches!(token.variant, TokenVariant::Capture(_)));
            debug_assert!(!matches!(token.variant, TokenVariant::OverlongCapture));

            // easiest way of satisfying borrow checker
            let mut grouper = std::mem::take(&mut self.grouper);
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
                Some(v) => v,
                None => return Ok(()),
            };

            debug_assert!(!matches!(token.variant, TokenVariant::Ellipsis(_)));
            debug_assert!(!matches!(token.variant, TokenVariant::Capture(_)));
            debug_assert!(!matches!(token.variant, TokenVariant::OverlongCapture));

            // easiest way of satisfying borrow checker
            let mut grouper = std::mem::take(&mut self.grouper);
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
            current: PartialMatch,
            input: &Token,
            trie: &Trie,
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
                trie: &Trie,
                element: &PartialMatch,
                last_token: &Token,
                out: &mut impl FnMut(FullMatch),
            ) {
                'outer: for full_match in trie.nodes[element.trie_position].full_match.iter() {
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
                                    let transform: &regex_lite::Regex = transform; // rust-analyzer???
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

            fn handle_capture_and_backrefs(
                items: &[u8],
                capture_unit: Transition,
                current: &PartialMatch,
                input: &Token,
                trie: &Trie,
                next_matches: &mut BinaryHeap<PartialMatch>,
                max_concurrent_matches: usize,
                out: &mut impl FnMut(FullMatch),
            ) {
                // capture transition
                if let Some(v) = trie.nodes[current.trie_position].next.get(&capture_unit) {
                    // ok - typically a small number of captures
                    let mut new_stack = (*current.capture_stack).clone();
                    new_stack.push(Arc::new(items.to_vec().into_boxed_slice()));
                    let new_stack = Arc::new(new_stack);

                    let m = PartialMatch {
                        start_position: if v.set_start {
                            input.start
                        } else {
                            current.start_position
                        },
                        trie_position: v.transition,
                        capture_stack: new_stack, // rc
                        bracket_state: current.bracket_state.pass_through_sbe_info(),
                    };
                    handle_maybe_full_match(trie, &m, &input, out);
                    handle_partial_match(next_matches, max_concurrent_matches, m);
                }

                // backref transitions
                for (i, cap) in current.capture_stack.iter().enumerate() {
                    if *items == ***cap {
                        if let Some(v) = trie.nodes[current.trie_position]
                            .next
                            .get(&Transition::Backref(i))
                        {
                            let m = PartialMatch {
                                start_position: if v.set_start {
                                    input.start
                                } else {
                                    current.start_position
                                },
                                trie_position: v.transition,
                                capture_stack: current.capture_stack.clone(),
                                bracket_state: current.bracket_state.pass_through_sbe_info(),
                            };
                            handle_maybe_full_match(trie, &m, &input, out);
                            handle_partial_match(next_matches, max_concurrent_matches, m);
                        }
                    }
                }
            }

            if current.bracket_state.depth != 0 {
                // blocked by NOT TOO SHORT rule
            } else {
                // attempt literal transition
                match trie.nodes[current.trie_position]
                    .next
                    .get(&Transition::Literal(input.variant.clone()))
                {
                    Some(v) => {
                        let m = PartialMatch {
                            start_position: if v.set_start {
                                input.start
                            } else {
                                current.start_position
                            },
                            trie_position: v.transition,
                            capture_stack: current.capture_stack.clone(), // rc
                            bracket_state: current.bracket_state.pass_through_sbe_info(),
                        };
                        handle_maybe_full_match(trie, &m, &input, out);
                        handle_partial_match(next_matches, max_concurrent_matches, m);

                        if let Some(loops) = &v.loops {
                            let mut new_capture_stack: Vec<Arc<Box<[u8]>>> =
                                (*current.capture_stack).clone();
                            new_capture_stack.truncate(
                                new_capture_stack.len() - loops.new_captures_since_loop_entry,
                            );
                            let m = PartialMatch {
                                start_position: if v.set_start {
                                    input.start
                                } else {
                                    current.start_position
                                },
                                trie_position: loops.return_trie_position,
                                bracket_state: current.bracket_state.pass_through_sbe_info(),
                                capture_stack: Arc::new(new_capture_stack),
                            };
                            // handle_maybe_full_match(trie, &m, &input, out); // not possible
                            handle_partial_match(next_matches, max_concurrent_matches, m);
                        }
                    }
                    None => {}
                }

                match &input.variant {
                    TokenVariant::Number(items) => {
                        handle_capture_and_backrefs(
                            items,
                            Transition::CaptureNumber,
                            &current,
                            &input,
                            trie,
                            next_matches,
                            max_concurrent_matches,
                            out,
                        );
                    }

                    TokenVariant::Identifier(items) => {
                        handle_capture_and_backrefs(
                            items,
                            Transition::CaptureIdentifier,
                            &current,
                            &input,
                            trie,
                            next_matches,
                            max_concurrent_matches,
                            out,
                        );
                    }

                    TokenVariant::String(items) => {
                        handle_capture_and_backrefs(
                            items,
                            Transition::CaptureString,
                            &current,
                            &input,
                            trie,
                            next_matches,
                            max_concurrent_matches,
                            out,
                        );
                    }

                    _ => {}
                }
            }

            let mut next = current;
            let maybe_bracket = match trie.nodes[next.trie_position].reflexive {
                None => return, // no reflexive transition
                Some(reflexive) => {
                    match reflexive {
                        ReflexiveTransition::ContainedEllipsis(bracket_type) => Some(bracket_type),
                        ReflexiveTransition::SBE => Some(BracketType::Curly),
                        ReflexiveTransition::NormalEllipsis => {
                            // not influence by too long or too short rule
                            next.bracket_state = Default::default(); // clear sbe state
                            handle_partial_match(next_matches, max_concurrent_matches, next);
                            return;
                        }
                    }
                }
            };

            let mut continuation_valid = true; // check for NOT TOO LONG rule
            match input.variant {
                TokenVariant::Byte(b) => {
                    let vals = match maybe_bracket {
                        None => None,
                        Some(bracket) => match bracket {
                            BracketType::Round => match b {
                                b'(' => Some(1),
                                b')' => Some(-1),
                                _ => None,
                            },
                            BracketType::Square => match b {
                                b'[' => Some(1),
                                b']' => Some(-1),
                                _ => None,
                            },
                            BracketType::Corner => match b {
                                b'<' => Some(1),
                                b'>' => Some(-1),
                                _ => None,
                            },
                            BracketType::Curly => match b {
                                b'{' => Some(1),
                                b'}' => Some(-1),
                                _ => None,
                            },
                        },
                    };

                    if let Some(delta) = vals {
                        match trie.nodes[next.trie_position].reflexive.unwrap() {
                            ReflexiveTransition::ContainedEllipsis(_) => {
                                match next.bracket_state.depth.checked_add_signed(delta) {
                                    Some(v) => {
                                        next.bracket_state.depth = v;
                                    }
                                    None => {
                                        continuation_valid = false;
                                    }
                                }
                            }
                            ReflexiveTransition::SBE => {
                                match next.bracket_state.sbe_storage.checked_add_signed(delta) {
                                    Some(v) => {
                                        next.bracket_state.sbe_storage = v;
                                    }
                                    None => {
                                        continuation_valid = false;
                                    }
                                }
                            }
                            _ => unreachable!(),
                        }
                    }
                }
                _ => {}
            }

            if continuation_valid {
                handle_partial_match(next_matches, max_concurrent_matches, next);
            } else {
                // too long rule failed. the ... operator is not allowed to
                // expand into the parent bracket scope
            }
        }

        for m in std::mem::take(&mut self.matches).into_iter() {
            explore_transitions(
                m,
                &token,
                &self.trie,
                &mut next_matches,
                self.max_concurrent_matches,
                &mut out,
            );
        }

        explore_transitions(
            PartialMatch::new(token.start),
            &token,
            &self.trie,
            &mut next_matches,
            self.max_concurrent_matches,
            &mut out,
        );

        self.matches = next_matches;
    }
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use crate::{
        engine::matchers::{make_c_like_lexer, make_python_like_lexer, make_rust_like_lexer},
        lexer::EnumLexer,
    };

    use super::*;

    enum Language {
        CLike,
        PythonLike,
        #[allow(dead_code)]
        RustLike,
    }

    fn run_matcher_test(
        lang: Language,
        patterns: &'static [(&'static str, &'static [u8])],
        subject_bytes: &'static [u8],
        expected_matches: Vec<FullMatch>,
    ) {
        let mut trie = Trie::default();
        let enum_lexer_token_size = 99.try_into().unwrap();
        for pattern in patterns.iter() {
            let mut cursor: std::io::Cursor<&[u8]> = Cursor::new(pattern.1);
            let lexer: EnumLexer = match lang {
                Language::CLike => {
                    EnumLexer::CLike(make_c_like_lexer(false, true, enum_lexer_token_size))
                }
                Language::PythonLike => {
                    EnumLexer::PythonLike(make_python_like_lexer(true, enum_lexer_token_size))
                }
                Language::RustLike => {
                    EnumLexer::RustLike(make_rust_like_lexer(true, enum_lexer_token_size))
                }
            };

            trie.add_pattern(
                &mut cursor,
                &Default::default(),
                pattern.0.to_owned(),
                "".to_owned(),
                &Default::default(),
                lexer,
                enum_lexer_token_size,
            )
            .unwrap();
        }

        let mut cursor: std::io::Cursor<&[u8]> = Cursor::new(subject_bytes);
        let subject_lexer: EnumLexer = match lang {
            Language::CLike => {
                EnumLexer::CLike(make_c_like_lexer(false, false, enum_lexer_token_size))
            }
            Language::PythonLike => {
                EnumLexer::PythonLike(make_python_like_lexer(false, enum_lexer_token_size))
            }
            Language::RustLike => {
                EnumLexer::RustLike(make_rust_like_lexer(false, enum_lexer_token_size))
            }
        };
        let mut out: Vec<FullMatch> = Vec::new();
        let mut matcher = Matcher::new(&trie, 100, enum_lexer_token_size, 1.try_into().unwrap());
        matcher
            .process_and_drain(&mut cursor, subject_lexer, |m| {
                out.push(m);
            })
            .unwrap();

        assert_eq!(out, expected_matches);
    }

    #[test]
    fn basic_literal_test_c() {
        run_matcher_test(
            Language::CLike,
            &[("basic_literal_test_c", b"literal")],
            b"literal",
            vec![FullMatch {
                start: Position {
                    offset: 0,
                    column: 1,
                    line: 1,
                },
                end: Position {
                    offset: 7,
                    column: 8,
                    line: 1,
                },
                name: "basic_literal_test_c".to_string(),
                group: "".to_string(),
                captures: Default::default(),
            }],
        );
    }

    #[test]
    fn basic_capture_test_c() {
        run_matcher_test(
            Language::CLike,
            &[("basic_capture_test_c", b"$CAPTURE")],
            b"\"something\"",
            vec![FullMatch {
                start: Position {
                    offset: 0,
                    column: 1,
                    line: 1,
                },
                end: Position {
                    offset: 11,
                    column: 12,
                    line: 1,
                },
                name: "basic_capture_test_c".to_string(),
                group: "".to_string(),
                captures: BTreeMap::from([(
                    Box::from(b"CAPTURE" as &[u8]),
                    Box::from(b"something" as &[u8]),
                )]),
            }],
        );
    }

    #[test]
    fn backref_capture_test_c() {
        run_matcher_test(
            Language::CLike,
            &[("backref_capture_test_c", b"int &X = #VALUE; ... &X+=1;")],
            b"int my_variable = 123; char ignore = 0; my_variable += 1;",
            vec![FullMatch {
                start: Position {
                    offset: 0,
                    column: 1,
                    line: 1,
                },
                end: Position {
                    offset: 57,
                    column: 58,
                    line: 1,
                },
                name: "backref_capture_test_c".to_string(),
                group: "".to_string(),
                captures: BTreeMap::from([
                    (Box::from(b"X" as &[u8]), Box::from(b"my_variable" as &[u8])),
                    (Box::from(b"VALUE" as &[u8]), Box::from(b"123" as &[u8])),
                ]),
            }],
        );
    }

    #[test]
    fn basic_literal_test_py() {
        run_matcher_test(
            Language::PythonLike,
            &[("basic_literal_test_py", b"literal")],
            b" literal",
            vec![FullMatch {
                start: Position {
                    offset: 1,
                    column: 2,
                    line: 1,
                },
                end: Position {
                    offset: 8,
                    column: 9,
                    line: 1,
                },
                name: "basic_literal_test_py".to_string(),
                group: "".to_string(),
                captures: Default::default(),
            }],
        );
    }

    #[test]
    fn sbe_flow_state() {
        run_matcher_test(
            Language::CLike,
            &[("sbe_flow_state", b"a ..} b ..} c")],
            b"a { b } c",
            vec![FullMatch {
                start: Position {
                    offset: 0,
                    column: 1,
                    line: 1,
                },
                end: Position {
                    offset: 9,
                    column: 10,
                    line: 1,
                },
                name: "sbe_flow_state".to_string(),
                group: "".to_string(),
                captures: Default::default(),
            }],
        );
    }

    #[test]
    fn sbe_flow_state_break() {
        run_matcher_test(
            Language::CLike,
            &[("sbe_flow_state_break", b"a ..} b ... c ..} d")],
            b"a {{ b } c } d",
            vec![],
        );
    }

    #[test]
    fn sbe_no_break() {
        run_matcher_test(
            Language::CLike,
            &[("sbe_no_break", b"a ..} b(...) ..} c")],
            b"a { b(test) } c",
            vec![FullMatch {
                start: Position {
                    offset: 0,
                    column: 1,
                    line: 1,
                },
                end: Position {
                    offset: 15,
                    column: 16,
                    line: 1,
                },
                name: "sbe_no_break".to_string(),
                group: "".to_string(),
                captures: Default::default(),
            }],
        );
    }

    #[test]
    fn string_concatenation_canonicalization() {
        run_matcher_test(
            Language::CLike,
            &[("string_literal_norm", b"$STR")],
            b"\"abc\" \"def\"",
            vec![FullMatch {
                start: Position {
                    offset: 0,
                    column: 1,
                    line: 1,
                },
                end: Position {
                    offset: 11,
                    column: 12,
                    line: 1,
                },
                name: "string_literal_norm".to_string(),
                group: "".to_string(),
                captures: BTreeMap::from([(
                    Box::from(b"STR" as &[u8]),
                    Box::from(b"abcdef" as &[u8]),
                )]),
            }],
        );
    }

    #[test]
    fn string_concatenation_canonicalization2() {
        run_matcher_test(
            Language::CLike,
            &[("string_literal_norm2", b"$STR")],
            b"\"abc\"+\"def\"",
            vec![FullMatch {
                start: Position {
                    offset: 0,
                    column: 1,
                    line: 1,
                },
                end: Position {
                    offset: 11,
                    column: 12,
                    line: 1,
                },
                name: "string_literal_norm2".to_string(),
                group: "".to_string(),
                captures: BTreeMap::from([(
                    Box::from(b"STR" as &[u8]),
                    Box::from(b"abcdef" as &[u8]),
                )]),
            }],
        );
    }

    #[test]
    fn string_concatenation_canonicalization_pattern() {
        run_matcher_test(
            Language::CLike,
            &[("string_literal_norm_pattern", b"\"a\" \"b\" \"c\" hi")],
            b"\"ab\" \"c\" hi",
            vec![FullMatch {
                start: Position {
                    offset: 0,
                    column: 1,
                    line: 1,
                },
                end: Position {
                    offset: 11,
                    column: 12,
                    line: 1,
                },
                name: "string_literal_norm_pattern".to_string(),
                group: "".to_string(),
                captures: Default::default(),
            }],
        );
    }

    #[test]
    fn java_function_call_test() {
        run_matcher_test(
            Language::CLike,
            &[("java_function_call_test", b"getInstance($ALG ...)")],
            b"Cipher c = Cipher.getInstance(\"DES\");",
            vec![FullMatch {
                start: Position {
                    offset: 18,
                    column: 19,
                    line: 1,
                },
                end: Position {
                    offset: 36,
                    column: 37,
                    line: 1,
                },
                name: "java_function_call_test".to_string(),
                group: "".to_string(),
                captures: BTreeMap::from([(
                    Box::from(b"ALG" as &[u8]),
                    Box::from(b"DES" as &[u8]),
                )]),
            }],
        );
    }

    #[test]
    fn python_function_call_test() {
        run_matcher_test(
            Language::PythonLike,
            &[(
                "python_function_call_test",
                br#"&VAR = #NUM
...
abc(... kwar=&VAR ...)"#,
            )],
            br#"
x = 20
# COMMENTS AND STUFF
abc(kwar=x)
"#,
            vec![FullMatch {
                start: Position {
                    offset: 1,
                    column: 1,
                    line: 2,
                },
                end: Position {
                    offset: 40,
                    column: 12,
                    line: 4,
                },
                name: "python_function_call_test".to_string(),
                group: "".to_string(),
                captures: BTreeMap::from([
                    (Box::from(b"NUM" as &[u8]), Box::from(b"20" as &[u8])),
                    (Box::from(b"VAR" as &[u8]), Box::from(b"x" as &[u8])),
                ]),
            }],
        );
    }

    #[test]
    fn ellipsis_brackets() {
        // without proper bracket scope handling, 3 matches would appear, when
        // only 1 is correct
        run_matcher_test(
            Language::CLike,
            &[(
                "ellipsis_brackets",
                br#"
let &VAR = 123;
...
test(...&VAR...)
                "#,
            )],
            br#"
let y = 123;
let x = 123;
test( (), y, () );
// stuff
// here
other_function( (), x, () );
"#,
            vec![FullMatch {
                start: Position {
                    offset: 1,
                    column: 1,
                    line: 2,
                },
                end: Position {
                    offset: 44,
                    column: 18,
                    line: 4,
                },
                name: "ellipsis_brackets".to_string(),
                group: "".to_string(),
                captures: BTreeMap::from([(Box::from(b"VAR" as &[u8]), Box::from(b"y" as &[u8]))]),
            }],
        );
    }

    #[test]
    fn corner_brackets_ignored() {
        // without proper bracket scope handling, 3 matches would appear, when
        // only 1 is correct
        run_matcher_test(
            Language::RustLike,
            &[(
                "corner_brackets_ignored",
                br#"
match ... { ... }
"#,
            )],
            br#"
match value {
    1 => println!("yes"),
    _ => println!("no"),
}
"#,
            vec![FullMatch {
                start: Position {
                    offset: 1,
                    column: 1,
                    line: 2,
                },
                end: Position {
                    offset: 67,
                    column: 2,
                    line: 5,
                },
                name: "corner_brackets_ignored".to_string(),
                group: "".to_string(),
                captures: Default::default(),
            }],
        );
    }

    #[test]
    fn corner_bracket_aware() {
        run_matcher_test(
            Language::CLike,
            &[(
                "corner_brackets_aware",
                br#"
vector<..>>
"#,
            )],
            br#"
vector<int> a; vector<int> b;
"#,
            // two matches, not three
            vec![
                FullMatch {
                    start: Position {
                        offset: 1,
                        column: 1,
                        line: 2,
                    },
                    end: Position {
                        offset: 12,
                        column: 12,
                        line: 2,
                    },
                    name: "corner_brackets_aware".to_string(),
                    group: "".to_string(),
                    captures: Default::default(),
                },
                FullMatch {
                    start: Position {
                        offset: 16,
                        column: 16,
                        line: 2,
                    },
                    end: Position {
                        offset: 27,
                        column: 27,
                        line: 2,
                    },
                    name: "corner_brackets_aware".to_string(),
                    group: "".to_string(),
                    captures: Default::default(),
                },
            ],
        );
    }

    #[test]
    fn ellipsis_lexical_scope_brackets() {
        run_matcher_test(
            Language::CLike,
            &[(
                "lexical_brackets",
                br#"
let &VAR = #NUM;
..}
test(&VAR);
                "#,
            )],
            br#"
let x = 123;
{
    let x = 456;
}
// only one!
test(x);
"#,
            vec![FullMatch {
                start: Position {
                    offset: 1,
                    column: 1,
                    line: 2,
                },
                end: Position {
                    offset: 56,
                    column: 9,
                    line: 7,
                },
                name: "lexical_brackets".to_string(),
                group: "".to_string(),
                captures: BTreeMap::from([
                    (Box::from(b"VAR" as &[u8]), Box::from(b"x" as &[u8])),
                    (Box::from(b"NUM" as &[u8]), Box::from(b"123" as &[u8])),
                ]),
            }],
        );
    }

    #[test]
    fn python_scope() {
        run_matcher_test(
            Language::PythonLike, //
            &[("python scopes", br#"&VAR = #NUM...&VAR"#)],
            br#"if True:
    x = 42
x
"#,
            vec![FullMatch {
                start: Position {
                    offset: 13,
                    column: 5,
                    line: 2,
                },
                end: Position {
                    offset: 21,
                    column: 2,
                    line: 3,
                },
                name: "python scopes".to_string(),
                group: "".to_string(),
                captures: BTreeMap::from([
                    (Box::from(b"VAR" as &[u8]), Box::from(b"x" as &[u8])),
                    (Box::from(b"NUM" as &[u8]), Box::from(b"42" as &[u8])),
                ]),
            }],
        );
    }

    #[test]
    fn lexical_increase() {
        run_matcher_test(
            Language::CLike,
            &[("lexical_increase", br#"&VAR...something(&VAR)"#)],
            br#"
int x = 100;
{
    something(x);
}"#,
            vec![FullMatch {
                start: Position {
                    offset: 5,
                    column: 5,
                    line: 2,
                },
                end: Position {
                    offset: 32,
                    column: 17,
                    line: 4,
                },
                name: "lexical_increase".to_string(),
                group: "".to_string(),
                captures: BTreeMap::from([(Box::from(b"VAR" as &[u8]), Box::from(b"x" as &[u8]))]),
            }],
        );
    }

    fn test_pattern_combination(in_one: &'static str, in_two: &'static str) -> Result<(), String> {
        let mut trie = Trie::default();
        trie.add_pattern(
            &mut Cursor::new(in_one),
            &Default::default(),
            "".to_owned(),
            "".to_owned(),
            &Default::default(),
            make_c_like_lexer(false, true, 100.try_into().unwrap()),
            100.try_into().unwrap(),
        )?;
        trie.add_pattern(
            &mut Cursor::new(in_two),
            &Default::default(),
            "".to_owned(),
            "".to_owned(),
            &Default::default(),
            make_c_like_lexer(false, true, 100.try_into().unwrap()),
            100.try_into().unwrap(),
        )?;

        Ok(())
    }

    #[test]
    fn ambiguous_start() {
        assert!(test_pattern_combination("123 ..^ 456", "123 456").is_err());
        assert!(test_pattern_combination("123 456", "123 ..^ 456").is_err());

        assert!(test_pattern_combination("123 ..+ a ..+ 456", "123 a 456").is_err());
        assert!(test_pattern_combination("123 a 456", "123 ..+ a ..+ 456").is_err());

        assert!(test_pattern_combination("123 ..+ a ..+ 456", "..+ 123 a ..+ 456").is_err());

        assert!(test_pattern_combination("vec< ... >", "vec< ..> >").is_err());
        assert!(test_pattern_combination("vec< ..> >", "vec< ... >").is_err());

        assert!(test_pattern_combination("123 ..+ a ..+ 456", "123 ..+ a ..+ 456").is_ok());
        assert!(test_pattern_combination("123 ..^ 456", "123 ..^ 456").is_ok());
        assert!(test_pattern_combination("vec< ... >", "vec< ... >").is_ok());
    }
}
