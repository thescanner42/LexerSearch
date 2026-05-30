use std::{
    collections::{BTreeMap, HashSet},
    fmt,
    num::NonZero,
    vec,
};

use ahash::AHashMap;
use bincode::Decode;
use serde::{Serialize, Serializer};
use smallvec::SmallVec;

use crate::{
    engine::{
        canonicalizer::Canonicalizer,
        span::{
            GraphBuilderNodeEllipsisInfo, GraphNodeEllipsisInfo, GraphNodeEllipsisInfoEnum,
            InputSpanPreprocess, InputSpanPreprocessOutput, SetSpan,
        },
        token::{
            BracketStack, GraphBuilderBracketType, MultiRepitition, RepititionTokenVariant, Token,
            TokenVariant,
        },
    },
    lexer::{EllipsisEnum, Lexer},
};

#[derive(Default, Debug, Clone, Serialize, PartialEq, Eq, bincode::Encode, bincode::Decode)]
pub struct BackrefInfo {
    #[serde(serialize_with = "serialize_lossy_bytes")]
    pub name: Box<[u8]>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub transform: Option<String>,
}

pub fn serialize_lossy_bytes<S>(bytes: &Box<[u8]>, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    let s = String::from_utf8_lossy(bytes);
    serializer.serialize_str(&s)
}

/// information used after complete match found
#[derive(Debug, Clone, serde::Serialize, PartialEq, Eq, bincode::Encode, bincode::Decode)]
pub struct PatternInfo {
    /// associates the backref index with the name and transform stated in the
    /// pattern
    #[serde(skip_serializing_if = "<[_]>::is_empty")]
    pub backref_names: Box<[BackrefInfo]>,
    /// as stated in the pattern, arbitrary user defined output for the finding
    #[serde(
        skip_serializing_if = "BTreeMap::is_empty",
        serialize_with = "serialize_lossy_btreemap"
    )]
    pub out: BTreeMap<Box<[u8]>, Box<[u8]>>,

    /// as stated by pattern
    #[serde(skip_serializing_if = "String::is_empty")]
    pub name: String,
    #[serde(skip_serializing_if = "GroupInfo::is_default")]
    pub group: GroupInfo,
}

pub fn serialize_lossy_btreemap<S>(
    map: &BTreeMap<Box<[u8]>, Box<[u8]>>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: Serializer,
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

    mapped.serialize(serializer)
}

#[derive(
    Default,
    Debug,
    Clone,
    serde::Serialize,
    serde::Deserialize,
    PartialEq,
    Eq,
    bincode::Encode,
    bincode::Decode,
)]
pub struct GroupInfo {
    #[serde(skip_serializing_if = "String::is_empty")]
    pub name: String,
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub unique: bool,
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub cancel: bool,
    #[serde(
        default,
        skip_serializing_if = "HashSet::is_empty",
        with = "serde_out_set"
    )]
    pub r#match: HashSet<Box<[u8]>>,
}

impl GroupInfo {
    pub fn is_default(v: &Self) -> bool {
        v == &Self::default()
    }
}

mod serde_out_set {
    use std::collections::HashSet;

    use serde::{
        Deserializer, Serialize, Serializer,
        de::{self, Visitor},
    };

    fn encode(bytes: &[u8]) -> String {
        String::from_utf8_lossy(bytes).to_string()
    }

    fn decode(s: &str) -> Result<Box<[u8]>, ()> {
        Ok(s.as_bytes().to_vec().into_boxed_slice())
    }

    pub fn serialize<S>(set: &HashSet<Box<[u8]>>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let encoded: Vec<String> = set.iter().map(|v| encode(v)).collect();

        encoded.serialize(serializer)
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<HashSet<Box<[u8]>>, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct V;

        impl<'de> Visitor<'de> for V {
            type Value = HashSet<Box<[u8]>>;

            fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "a list of encoded byte strings")
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: de::SeqAccess<'de>,
            {
                let mut out = HashSet::new();

                while let Some(v) = seq.next_element::<String>()? {
                    out.insert(decode(&v).map_err(|_| de::Error::custom("invalid value"))?);
                }

                Ok(out)
            }
        }

        deserializer.deserialize_seq(V)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SmallVecBincodeWrapper(pub SmallVec<[u8; 0x40]>);

impl bincode::Encode for SmallVecBincodeWrapper {
    fn encode<E: bincode::enc::Encoder>(
        &self,
        encoder: &mut E,
    ) -> Result<(), bincode::error::EncodeError> {
        self.0.as_slice().encode(encoder)
    }
}

impl<Context> bincode::Decode<Context> for SmallVecBincodeWrapper {
    fn decode<D: bincode::de::Decoder<Context = Context>>(
        decoder: &mut D,
    ) -> Result<Self, bincode::error::DecodeError> {
        Ok(Self(SmallVec::from_vec(Vec::<u8>::decode(decoder)?)))
    }
}

impl<'de, Context> bincode::BorrowDecode<'de, Context> for SmallVecBincodeWrapper {
    fn borrow_decode<D>(decoder: &mut D) -> Result<Self, bincode::error::DecodeError>
    where
        D: bincode::de::BorrowDecoder<'de, Context = Context>,
    {
        Ok(Self(SmallVec::from_vec(Vec::<u8>::borrow_decode(decoder)?)))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, bincode::Encode, bincode::Decode)]
pub enum GraphTokenVariant {
    Byte(u8),
    Captureable(SmallVecBincodeWrapper),
    /// see LexerTokenVariant for more details
    LexicalLevelChange(i32),
}

// lossy display for --debug-graph
impl fmt::Display for GraphTokenVariant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GraphTokenVariant::Byte(b) => {
                if b.is_ascii_graphic() {
                    write!(f, "{}", *b as char)
                } else {
                    write!(f, "{:#x}", b)
                }
            }
            GraphTokenVariant::Captureable(bytes) => {
                // if every byte is printable ascii, render as a string
                if bytes.0.iter().all(|b| b.is_ascii_graphic() || *b == b' ') {
                    for b in bytes.0.iter() {
                        write!(f, "{}", *b as char)?;
                    }
                    Ok(())
                } else {
                    // mixed or non-ascii: fall back to comma-separated decimals
                    for (i, b) in bytes.0.iter().enumerate() {
                        if i > 0 {
                            write!(f, ",")?;
                        }
                        write!(f, "{:#x}", b)?;
                    }
                    Ok(())
                }
            }
            GraphTokenVariant::LexicalLevelChange(v) => write!(f, "l:{v}"),
        }
    }
}

impl Serialize for GraphTokenVariant {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        // always serialize as a string — works both as value and as map key
        serializer.serialize_str(&self.to_string())
    }
}

#[derive(Default, Debug, PartialEq, Eq)]
pub struct GraphBuilderNode {
    pub(crate) edge: AHashMap<GraphTokenVariant, SetSpan>,
    pub(crate) repitition: AHashMap<MultiRepitition, usize>,
    pub(crate) ellipsis: GraphBuilderNodeEllipsisInfo,
    pub(crate) scope_blocking: Option<usize>,
    pub(crate) statement_blocking: Option<usize>,
    pub(crate) create_capture: Option<SetSpan>,
    pub(crate) non_capture: Option<SetSpan>,

    /// associates capture index being replaced with where to go    
    pub(crate) backref: AHashMap<usize, SetSpan>,
    pub(crate) create_replace: AHashMap<usize, SetSpan>,
    pub(crate) backref_replace: AHashMap<usize, SetSpan>,

    pub(crate) full_match: Vec<PatternInfo>,
}

#[derive(Debug)]
pub struct GraphBuilder {
    pub(crate) nodes: Vec<GraphBuilderNode>,
}

impl Default for GraphBuilder {
    fn default() -> Self {
        Self {
            // default has index 0 valid but doesn't lead anywhere. required since
            // index 0 is the start index and assumed to be ok
            nodes: vec![Default::default()],
        }
    }
}

impl GraphBuilder {
    pub fn add_pattern<L: Lexer, R: std::io::Read>(
        &mut self,
        r: &mut R,
        out: &BTreeMap<Box<[u8]>, Box<[u8]>>,
        name: String,
        group: GroupInfo,
        transform: &BTreeMap<Box<[u8]>, String>,
        mut lexer: L,
        max_token_length: NonZero<usize>,
    ) -> Result<(), String> {
        debug_assert!(lexer.configured_for_pattern());

        let mut bracket_stack: BracketStack = Default::default();
        let mut canonicalizer = Canonicalizer::new(max_token_length);
        let mut span_preprocessor = InputSpanPreprocess::default();
        let mut graph_position = 0usize;
        // the current multi reptitition and the captures that were created in
        // the last repitition
        let mut current_repitition: Option<(MultiRepitition, Vec<Box<[u8]>>)> = None;

        // keep track of first time seeing capture vs backreference
        let mut captures: AHashMap<Box<[u8]>, usize> = Default::default();
        let mut capture_count_increment = 0;

        loop {
            let t: InputSpanPreprocessOutput = match span_preprocessor.next_and_drain(|| {
                match canonicalizer.next_and_drain(|| {
                    let token = match lexer.next_and_drain(r)? {
                        Some(s) => s,
                        None => return Ok(None),
                    };
                    return Ok(Some(Token::from_lexer_token(token)));
                })? {
                    Some(v) => Ok(Some(v)),
                    None => return Ok(None),
                }
            })? {
                None => {
                    if current_repitition.is_some() {
                        return Err("unclosed repitition".to_string());
                    }

                    // no more input
                    fn reverse_map(
                        captures: AHashMap<Box<[u8]>, usize>,
                        transform: &BTreeMap<Box<[u8]>, String>,
                    ) -> Result<Box<[BackrefInfo]>, String> {
                        let len = captures.len();
                        let mut reversed: Vec<BackrefInfo> = Vec::with_capacity(len);
                        reversed.resize_with(len, || Default::default());
                        for (key, index) in captures {
                            reversed[index].name = key.to_vec().into_boxed_slice();

                            let r: &Box<[u8]> = &reversed[index].name;
                            if let Some(expr) = transform.get(r) {
                                let _ = regex_lite::Regex::new(&expr).map_err(|e| {
                                    format!("regex compilation error of '{expr}': {e}")
                                })?;
                                reversed[index].transform = Some(expr.clone());
                            }
                        }
                        Ok(reversed.into_boxed_slice())
                    }

                    let mut new_out: BTreeMap<Box<[u8]>, Box<[u8]>> = Default::default();
                    for (k, v) in out.iter() {
                        new_out.insert(k.clone(), v.clone());
                    }

                    self.nodes[graph_position].full_match.push(PatternInfo {
                        backref_names: reverse_map(captures, transform)?,
                        out: new_out,
                        name: name.clone(),
                        group: group.clone(),
                    });
                    break;
                }
                Some(mut v) => {
                    v.token.variant.add_double_quotes();
                    v
                }
            };

            match t.token.variant {
                TokenVariant::OverlongIdentifier
                | TokenVariant::OverlongString
                | TokenVariant::OverlongCapture => {
                    return Err(format!(
                        "{} line {} col {}: token too long",
                        name, t.token.start.line, t.token.start.column
                    ));
                }
                _ => {}
            }

            if let TokenVariant::Byte(b) = t.token.variant {
                bracket_stack.handle(b);
            }

            let nodes_len = self.nodes.len();

            if matches!(
                t.token.variant,
                TokenVariant::Ellipsis(EllipsisEnum::JumpSep)
            ) {
                match current_repitition.as_mut() {
                    None => {
                        return Err("..| only allowed in repitition".to_string());
                    }
                    Some((v, new_captures)) => {
                        if !new_captures.is_empty() {
                            return Err(
                                "a repitition must have a sum of zero captures created".to_string()
                            );
                        }
                        v.push_repitition();
                    }
                }
                continue;
            }

            if matches!(t.token.variant, TokenVariant::Ellipsis(EllipsisEnum::Jump)) {
                // start or stop of a repitition
                match current_repitition.take() {
                    Some((v, new_captures)) => {
                        // end of ..*, the loop is fully known at this point
                        if !new_captures.is_empty() {
                            return Err(
                                "a repitition must have a sum of zero captures created".to_string()
                            );
                        }

                        match self.nodes[graph_position].repitition.entry(v) {
                            std::collections::hash_map::Entry::Occupied(occupied_entry) => {
                                graph_position = *occupied_entry.get(); // follow existing
                            }
                            std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                                vacant_entry.insert(nodes_len);
                                self.nodes.push(Default::default());
                                graph_position = nodes_len;
                            }
                        }
                    }
                    None => {
                        // begin ..*
                        current_repitition = Some(Default::default());
                    }
                }
                continue;
            }

            if let Some((repitition, new_captures)) = current_repitition.as_mut() {
                repitition.push(match t.token.variant {
                    // convert token variant to reptition token variant
                    TokenVariant::OverlongIdentifier
                    | TokenVariant::OverlongString
                    | TokenVariant::OverlongCapture => unreachable!(), // above
                    TokenVariant::Byte(b) => RepititionTokenVariant::Byte(b),
                    TokenVariant::String(items) | TokenVariant::Identifier(items) => {
                        RepititionTokenVariant::Captureable(items)
                    }
                    TokenVariant::LexicalLevelChange(v) => {
                        RepititionTokenVariant::LexicalLevelChange(v)
                    }
                    TokenVariant::Ellipsis(ellipsis_enum) => match ellipsis_enum {
                        EllipsisEnum::Jump | EllipsisEnum::JumpSep => unreachable!(), // above
                        EllipsisEnum::Normal => match bracket_stack.last() {
                            None => RepititionTokenVariant::Uncontained,
                            Some(v) => match v {
                                GraphBuilderBracketType::Round => RepititionTokenVariant::Round,
                                GraphBuilderBracketType::Square => RepititionTokenVariant::Square,
                                GraphBuilderBracketType::Curly => RepititionTokenVariant::Curly,
                            },
                        },
                        EllipsisEnum::CBE => RepititionTokenVariant::Corner,
                        EllipsisEnum::SBE(v) => {
                            if v {
                                RepititionTokenVariant::StatementBlocking
                            } else {
                                RepititionTokenVariant::ScopeBlocking
                            }
                        }
                        EllipsisEnum::SetStart | EllipsisEnum::SetEnd => unreachable!(), // handled by span preprocess
                    },
                    TokenVariant::Capture(items) => {
                        match items[0] {
                            b'$' => {
                                // normal capture
                                if *items == *b"$_" {
                                    // non-capture
                                    RepititionTokenVariant::NonCapturingCapture
                                } else {
                                    if let Some(v) = captures.get(&items[1..]) {
                                        // backref normal capture
                                        RepititionTokenVariant::Backref(*v)
                                    } else {
                                        // create capture
                                        new_captures.push(Box::<[_]>::from(&items[1..]));
                                        captures.insert(items[1..].into(), capture_count_increment);
                                        capture_count_increment += 1;
                                        RepititionTokenVariant::CreateCapture
                                    }
                                }
                            }
                            b'%' => {
                                // replace capture
                                if let Some(&v) = captures.get(&items[1..]) {
                                    match new_captures.pop() {
                                        Some(new_capture) => {
                                            captures.remove(&new_capture).unwrap();
                                            capture_count_increment -= 1;
                                            RepititionTokenVariant::PopReplace(v)
                                        }
                                        None => RepititionTokenVariant::CreateReplace(v),
                                    }
                                } else {
                                    return Err(
                                        "replace must reference an existing capture".to_string()
                                    );
                                }
                            }
                            _ => unreachable!(),
                        }
                    }
                });
                continue;
            }

            match t.token.variant {
                TokenVariant::Ellipsis(e) => {
                    match e {
                        EllipsisEnum::Jump | EllipsisEnum::JumpSep => unreachable!(), // handled above
                        EllipsisEnum::CBE => {
                            let old_graph_pos = graph_position;
                            let mut taken = std::mem::take(&mut self.nodes[old_graph_pos].ellipsis);
                            graph_position = taken.follow_or_add_corner(|| {
                                self.nodes.push(Default::default());
                                nodes_len
                            });
                            self.nodes[old_graph_pos].ellipsis = taken; // sidestep borrow checker
                        }
                        EllipsisEnum::Normal => {
                            let old_graph_pos = graph_position;
                            let mut taken = std::mem::take(&mut self.nodes[old_graph_pos].ellipsis);
                            graph_position = taken.follow_or_add(bracket_stack.last(), || {
                                self.nodes.push(Default::default());
                                nodes_len
                            });
                            self.nodes[old_graph_pos].ellipsis = taken; // sidestep borrow checker
                        }
                        EllipsisEnum::SBE(v) => {
                            if v {
                                // statement blocking
                                match self.nodes[graph_position].statement_blocking {
                                    Some(v) => {
                                        // follow existing path
                                        graph_position = v;
                                    }
                                    None => {
                                        self.nodes[graph_position].statement_blocking =
                                            Some(nodes_len);
                                        self.nodes.push(Default::default());
                                        graph_position = nodes_len;
                                    }
                                }
                            } else {
                                match self.nodes[graph_position].scope_blocking {
                                    Some(v) => {
                                        // follow existing path
                                        graph_position = v;
                                    }
                                    None => {
                                        self.nodes[graph_position].scope_blocking = Some(nodes_len);
                                        self.nodes.push(Default::default());
                                        graph_position = nodes_len;
                                    }
                                }
                            }
                        }
                        EllipsisEnum::SetStart | EllipsisEnum::SetEnd => unreachable!(), // handled by span preprocess above
                    }
                }

                TokenVariant::Capture(items) => {
                    if *items == *b"$_" {
                        let non_capture = self.nodes[graph_position].non_capture.take();
                        graph_position = match non_capture {
                            Some(mut v) => {
                                let next = v.follow_or_update(t.set_start, t.set_end, || {
                                    self.nodes.push(Default::default());
                                    nodes_len
                                });
                                self.nodes[graph_position].non_capture = Some(v); // put it back
                                next
                            }
                            None => {
                                self.nodes[graph_position].non_capture =
                                    Some(SetSpan::from(nodes_len, t.set_start, t.set_end));
                                self.nodes.push(Default::default());
                                nodes_len
                            }
                        };
                    } else {
                        if items[0] == b'%' {
                            // replace capture, not within repitition
                            if let Some(ref_num) = captures.get(&items[1..]) {
                                let mut add_node = false; // borrow checker annoyance
                                match self.nodes[graph_position].create_replace.entry(*ref_num) {
                                    std::collections::hash_map::Entry::Occupied(
                                        mut occupied_entry,
                                    ) => {
                                        graph_position = occupied_entry.get_mut().follow_or_update(
                                            t.set_start,
                                            t.set_end,
                                            || {
                                                add_node = true;
                                                nodes_len
                                            },
                                        );
                                    }
                                    std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                                        vacant_entry.insert(SetSpan::from(
                                            nodes_len,
                                            t.set_start,
                                            t.set_end,
                                        ));
                                        add_node = true;
                                        graph_position = nodes_len;
                                    }
                                }
                                if add_node {
                                    self.nodes.push(Default::default());
                                }
                            } else {
                                return Err("replace must reference existing capture".to_string());
                            }
                        } else {
                            // normal capture
                            match captures.get(&items[1..]) {
                                Some(ref_num) => {
                                    // capture was stated previously, this is a
                                    // backreference
                                    let mut add_node = false; // borrow checker annoyance
                                    match self.nodes[graph_position].backref.entry(*ref_num) {
                                        std::collections::hash_map::Entry::Occupied(
                                            mut occupied_entry,
                                        ) => {
                                            graph_position = occupied_entry
                                                .get_mut()
                                                .follow_or_update(t.set_start, t.set_end, || {
                                                    add_node = true;
                                                    nodes_len
                                                });
                                        }
                                        std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                                            vacant_entry.insert(SetSpan::from(
                                                nodes_len,
                                                t.set_start,
                                                t.set_end,
                                            ));
                                            add_node = true;
                                            graph_position = nodes_len;
                                        }
                                    }
                                    if add_node {
                                        self.nodes.push(Default::default());
                                    }
                                }
                                None => {
                                    captures.insert(items[1..].into(), capture_count_increment);
                                    capture_count_increment += 1;
                                    let mut add_node = false; // borrow checker annoyance
                                    match self.nodes[graph_position].create_capture.as_mut() {
                                        Some(v) => {
                                            graph_position =
                                                v.follow_or_update(t.set_start, t.set_end, || {
                                                    add_node = true;
                                                    nodes_len
                                                });
                                        }
                                        None => {
                                            self.nodes[graph_position].create_capture = Some(
                                                SetSpan::from(nodes_len, t.set_start, t.set_end),
                                            );
                                            add_node = true;
                                            graph_position = nodes_len;
                                        }
                                    }
                                    if add_node {
                                        self.nodes.push(Default::default());
                                    }
                                }
                            }
                        }
                    }
                }

                _ => {
                    // convert the token variant to graph token variant
                    let variant = match t.token.variant {
                        TokenVariant::OverlongIdentifier
                        | TokenVariant::OverlongString
                        | TokenVariant::OverlongCapture
                        | TokenVariant::Ellipsis(_)
                        | TokenVariant::Capture(_) => unreachable!(), // handled by various paths above
                        TokenVariant::Byte(b) => GraphTokenVariant::Byte(b),
                        TokenVariant::String(items) | TokenVariant::Identifier(items) => {
                            GraphTokenVariant::Captureable(SmallVecBincodeWrapper(items))
                        }
                        TokenVariant::LexicalLevelChange(v) => {
                            GraphTokenVariant::LexicalLevelChange(v)
                        }
                    };

                    let mut add_node = false;
                    match self.nodes[graph_position].edge.entry(variant) {
                        std::collections::hash_map::Entry::Occupied(mut occupied_entry) => {
                            graph_position = occupied_entry.get_mut().follow_or_update(
                                t.set_start,
                                t.set_end,
                                || {
                                    add_node = true;
                                    nodes_len
                                },
                            );
                        }
                        std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                            vacant_entry.insert(SetSpan::from(nodes_len, t.set_start, t.set_end));
                            add_node = true;
                            graph_position = nodes_len;
                        }
                    }
                    if add_node {
                        self.nodes.push(Default::default());
                    }
                }
            };
        }

        Ok(())
    }

    pub fn build(mut self) -> Result<Graph, String> {
        let mut ret = vec![Default::default()];
        // depth first traversal through the trie
        Self::build_subroutine(&mut self, 0, &mut ret, None)?;
        Ok(Graph { nodes: ret })
    }

    fn insert_edge(node: &mut GraphNode, k: GraphTokenVariant, v: SetSpan) {
        match node.edge.0.entry(k) {
            std::collections::hash_map::Entry::Occupied(mut occupied_entry) => {
                occupied_entry.get_mut().push(v);
            }
            std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(vec![v]);
            }
        }
    }

    fn copy_transitions(src: &GraphNode, dst: &mut GraphNode) -> Result<(), String> {
        for (k, v) in src.edge.0.iter() {
            match dst.edge.0.entry(k.clone()) {
                std::collections::hash_map::Entry::Occupied(mut occupied_entry) => {
                    occupied_entry.get_mut().extend_from_slice(v);
                }
                std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                    vacant_entry.insert(v.to_vec());
                }
            }
        }

        match (&src.ellipsis.other, &mut dst.ellipsis.other) {
            (GraphNodeEllipsisInfoEnum::None, _) => {}
            (_, GraphNodeEllipsisInfoEnum::None) => {
                dst.ellipsis.other = src.ellipsis.other.clone();
            }
            (
                GraphNodeEllipsisInfoEnum::Uncontained(src),
                GraphNodeEllipsisInfoEnum::Uncontained(dst),
            )
            | (GraphNodeEllipsisInfoEnum::Round(src), GraphNodeEllipsisInfoEnum::Round(dst))
            | (GraphNodeEllipsisInfoEnum::Square(src), GraphNodeEllipsisInfoEnum::Square(dst))
            | (GraphNodeEllipsisInfoEnum::Curly(src), GraphNodeEllipsisInfoEnum::Curly(dst)) => {
                dst.extend_from_slice(src);
            }

            _ => {
                // never
                return Err("incompatible ellipsis combination from loop".to_string());
            }
        }

        dst.scope_blocking.extend_from_slice(&src.scope_blocking);
        dst.statement_blocking
            .extend_from_slice(&src.statement_blocking);
        dst.non_capture.extend_from_slice(&src.non_capture);
        dst.create_capture.extend_from_slice(&src.create_capture);

        dst.full_match.extend_from_slice(&src.full_match);

        for (k, v) in src.backref.0.iter() {
            match dst.backref.0.entry(k.clone()) {
                std::collections::hash_map::Entry::Occupied(mut occupied_entry) => {
                    occupied_entry.get_mut().extend_from_slice(v);
                }
                std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                    vacant_entry.insert(v.to_vec());
                }
            }
        }

        for (k, v) in src.pop_replace.0.iter() {
            match dst.pop_replace.0.entry(k.clone()) {
                std::collections::hash_map::Entry::Occupied(mut occupied_entry) => {
                    occupied_entry.get_mut().extend_from_slice(v);
                }
                std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                    vacant_entry.insert(v.to_vec());
                }
            }
        }

        for (k, v) in src.create_replace.0.iter() {
            match dst.create_replace.0.entry(k.clone()) {
                std::collections::hash_map::Entry::Occupied(mut occupied_entry) => {
                    occupied_entry.get_mut().extend_from_slice(v);
                }
                std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                    vacant_entry.insert(v.to_vec());
                }
            }
        }

        Ok(())
    }

    fn redirect_transitions(g: &mut GraphNode, src: usize, dst: usize) {
        fn rewrite_vec(v: &mut [usize], src: usize, dst: usize) {
            for item in v {
                if *item == src {
                    *item = dst;
                }
            }
        }

        fn rewrite_setstart_enum_vec(v: &mut [SetSpan], src: usize, dst: usize) {
            for item in v {
                item.rewrite(src, dst);
            }
        }

        for targets in g.edge.0.values_mut() {
            rewrite_setstart_enum_vec(targets, src, dst);
        }

        match &mut g.ellipsis.other {
            GraphNodeEllipsisInfoEnum::None => {}
            GraphNodeEllipsisInfoEnum::Uncontained(v)
            | GraphNodeEllipsisInfoEnum::Round(v)
            | GraphNodeEllipsisInfoEnum::Square(v)
            | GraphNodeEllipsisInfoEnum::Curly(v) => {
                rewrite_vec(v, src, dst);
            }
        }

        rewrite_setstart_enum_vec(&mut g.create_capture, src, dst);
        rewrite_setstart_enum_vec(&mut g.non_capture, src, dst);

        for v in g.backref.0.values_mut() {
            rewrite_setstart_enum_vec(v, src, dst);
        }

        for v in g.pop_replace.0.values_mut() {
            rewrite_setstart_enum_vec(v, src, dst);
        }

        for v in g.create_replace.0.values_mut() {
            rewrite_setstart_enum_vec(v, src, dst);
        }
    }

    fn build_subroutine(
        &mut self,
        in_position: usize,
        ret: &mut Vec<GraphNode>,
        ret_position: Option<usize>,
    ) -> Result<(), String> {
        let ret_current_position = match ret_position {
            Some(v) => v,
            None => ret.len() - 1,
        };

        let mut in_literals = std::mem::take(&mut self.nodes[in_position].edge);
        for (k, v) in in_literals.drain() {
            v.handle(|in_dst, set_start, set_end| {
                let out_dst = ret.len();
                ret.push(Default::default());
                Self::build_subroutine(self, in_dst, ret, None)?;
                Self::insert_edge(
                    &mut ret[ret_current_position],
                    k.clone(),
                    SetSpan::from(out_dst, set_start, set_end),
                );
                Ok(())
            })?;
        }

        let ellipsis = std::mem::take(&mut self.nodes[in_position].ellipsis);

        ellipsis.handle(|in_dst, bracket_type| {
            let before_modification = std::mem::take(&mut ret[ret_current_position]);
            // first build the zero case
            Self::build_subroutine(self, in_dst, ret, Some(ret_current_position))?;
            let new_transitions_only = std::mem::take(&mut ret[ret_current_position]);
            ret[ret_current_position] = before_modification;

            // next the 1 or more case
            let loop_dst = ret.len();
            ret.push(Default::default());
            // loop at the loop node, then transition to the loop node
            match bracket_type {
                crate::engine::span::EllipsisInfoHandleEnum::Corner => {
                    ret[loop_dst].ellipsis = GraphNodeEllipsisInfo {
                        corner: vec![loop_dst],
                        other: GraphNodeEllipsisInfoEnum::None,
                    };
                    ret[ret_current_position].ellipsis = GraphNodeEllipsisInfo {
                        corner: vec![loop_dst],
                        other: Default::default(),
                    };
                }
                crate::engine::span::EllipsisInfoHandleEnum::Uncontained => {
                    ret[loop_dst].ellipsis = GraphNodeEllipsisInfo {
                        corner: Default::default(),
                        other: GraphNodeEllipsisInfoEnum::Uncontained(vec![loop_dst]),
                    };
                    ret[ret_current_position].ellipsis = GraphNodeEllipsisInfo {
                        corner: vec![],
                        other: GraphNodeEllipsisInfoEnum::Uncontained(vec![loop_dst]),
                    };
                }
                crate::engine::span::EllipsisInfoHandleEnum::Round => {
                    ret[loop_dst].ellipsis = GraphNodeEllipsisInfo {
                        corner: Default::default(),
                        other: GraphNodeEllipsisInfoEnum::Round(vec![loop_dst]),
                    };
                    ret[ret_current_position].ellipsis = GraphNodeEllipsisInfo {
                        corner: vec![],
                        other: GraphNodeEllipsisInfoEnum::Round(vec![loop_dst]),
                    };
                }
                crate::engine::span::EllipsisInfoHandleEnum::Square => {
                    ret[loop_dst].ellipsis = GraphNodeEllipsisInfo {
                        corner: Default::default(),
                        other: GraphNodeEllipsisInfoEnum::Square(vec![loop_dst]),
                    };
                    ret[ret_current_position].ellipsis = GraphNodeEllipsisInfo {
                        corner: vec![],
                        other: GraphNodeEllipsisInfoEnum::Square(vec![loop_dst]),
                    };
                }
                crate::engine::span::EllipsisInfoHandleEnum::Curly => {
                    ret[loop_dst].ellipsis = GraphNodeEllipsisInfo {
                        corner: Default::default(),
                        other: GraphNodeEllipsisInfoEnum::Curly(vec![loop_dst]),
                    };
                    ret[ret_current_position].ellipsis = GraphNodeEllipsisInfo {
                        corner: vec![],
                        other: GraphNodeEllipsisInfoEnum::Curly(vec![loop_dst]),
                    };
                }
            };
            // transition from the loop node
            Self::copy_transitions(&new_transitions_only, &mut ret[loop_dst])?;
            // put the transitions back that were taken before
            Self::copy_transitions(&new_transitions_only, &mut ret[ret_current_position])?;
            Ok(())
        })?;

        if let Some(scope_blocking) = std::mem::take(&mut self.nodes[in_position].scope_blocking) {
            // first build the zero case
            Self::build_subroutine(self, scope_blocking, ret, Some(ret_current_position))?;
            // next the 1 or more case
            let loop_dst = ret.len();
            ret.push(Default::default());
            // transition to the loop node
            ret[ret_current_position].scope_blocking.push(loop_dst);
            // transition from the loop node (includes self loop)
            let [src, dst] = ret
                .get_disjoint_mut([ret_current_position, loop_dst])
                .unwrap();
            Self::copy_transitions(src, dst)?;
        }

        if let Some(statement_blocking) =
            std::mem::take(&mut self.nodes[in_position].statement_blocking)
        {
            // first build the zero case
            Self::build_subroutine(self, statement_blocking, ret, Some(ret_current_position))?;
            // next the 1 or more case
            let loop_dst = ret.len();
            ret.push(Default::default());
            // transition to the loop node
            ret[ret_current_position].statement_blocking.push(loop_dst);
            // transition from the loop node (includes self loop)
            let [src, dst] = ret
                .get_disjoint_mut([ret_current_position, loop_dst])
                .unwrap();
            Self::copy_transitions(src, dst)?;
        }

        if let Some(create_capture) = std::mem::take(&mut self.nodes[in_position].create_capture) {
            create_capture.handle(|in_dst, set_start, set_end| {
                let out_dst = ret.len();
                ret.push(Default::default());
                Self::build_subroutine(self, in_dst, ret, None)?;
                ret[ret_current_position]
                    .create_capture
                    .push(SetSpan::from(out_dst, set_start, set_end));
                Ok(())
            })?;
        }

        if let Some(non_capture) = std::mem::take(&mut self.nodes[in_position].non_capture) {
            non_capture.handle(|in_dst, set_start, set_end| {
                let out_dst = ret.len();
                ret.push(Default::default());
                Self::build_subroutine(self, in_dst, ret, None)?;
                ret[ret_current_position]
                    .non_capture
                    .push(SetSpan::from(out_dst, set_start, set_end));
                Ok(())
            })?;
        }

        for (capture_index, sse) in std::mem::take(&mut self.nodes[in_position].backref) {
            sse.handle(|in_dst, set_start, set_end| {
                let out_dst = ret.len();
                ret.push(Default::default());
                Self::build_subroutine(self, in_dst, ret, None)?;
                ret[ret_current_position]
                    .backref.0
                    .entry(capture_index)
                    .or_default()
                    .push(SetSpan::from(out_dst, set_start, set_end));
                Ok(())
            })?;
        }

        for (capture_index, sse) in std::mem::take(&mut self.nodes[in_position].create_replace) {
            sse.handle(|in_dst, set_start, set_end| {
                let out_dst = ret.len();
                ret.push(Default::default());
                Self::build_subroutine(self, in_dst, ret, None)?;
                ret[ret_current_position]
                    .create_replace.0
                    .entry(capture_index)
                    .or_default()
                    .push(SetSpan::from(out_dst, set_start, set_end));
                Ok(())
            })?
        }

        for (capture_index, sse) in std::mem::take(&mut self.nodes[in_position].backref_replace) {
            sse.handle(|in_dst, set_start, set_end| {
                let out_dst = ret.len();
                ret.push(Default::default());
                Self::build_subroutine(self, in_dst, ret, None)?;
                ret[ret_current_position]
                    .pop_replace.0
                    .entry(capture_index)
                    .or_default()
                    .push(SetSpan::from(out_dst, set_start, set_end));
                Ok(())
            })?;
        }

        ret[ret_current_position]
            .full_match
            .extend(std::mem::take(&mut self.nodes[in_position].full_match));

        // handle reptitions
        // similar process to above but for many tokens in a chain
        let before_modification = std::mem::take(&mut ret[ret_current_position]);
        let mut new_transitions_only: Vec<GraphNode> = Default::default();
        // build the zero case
        let mut repititions = std::mem::take(&mut self.nodes[in_position].repitition);
        for (_, v) in repititions.iter() {
            Self::build_subroutine(self, *v, ret, Some(ret_current_position))?;
            new_transitions_only.push(std::mem::take(&mut ret[ret_current_position]));
        }
        new_transitions_only.reverse(); // take the last element for each, below
        ret[ret_current_position] = before_modification;

        {
            for (multi_repitition, _) in repititions.drain() {
                let multi_repitition = multi_repitition.get_vec();
                let mut loop_back_index: Option<usize> = None;

                for repitition in multi_repitition {
                    let mut repitition_current_position = ret_current_position;
                    for i in 0..2 {
                        // first it creates the nodes for the 1 case, up to the
                        // loopback node. then, it creates the loop which wraps
                        // back to the loopback node (1 or more case)
                        let is_loop = i == 1;
                        if loop_back_index.is_none() && is_loop {
                            loop_back_index = Some(repitition_current_position);
                        }
                        for (i, node) in repitition.iter().enumerate() {
                            let last_in_cycle = i == repitition.len() - 1;

                            if repitition_current_position == ret_current_position {
                                match node {
                                    RepititionTokenVariant::Uncontained
                                    | RepititionTokenVariant::Corner
                                    | RepititionTokenVariant::Round
                                    | RepititionTokenVariant::Square
                                    | RepititionTokenVariant::Curly
                                    | RepititionTokenVariant::ScopeBlocking
                                    | RepititionTokenVariant::StatementBlocking => {
                                        // TODO there isn't a strong reason for this
                                        // - simplifies implementation and there's
                                        // always a different way of writing the
                                        // pattern (reflexive at end instead of at
                                        // beginning)
                                        return Err("repitition requires content before reflexive transition (e.g. ...)".to_string());
                                    }
                                    _ => {}
                                }
                            }

                            match node {
                                RepititionTokenVariant::Captureable(_)
                                | RepititionTokenVariant::Byte(_)
                                | RepititionTokenVariant::LexicalLevelChange(_) => {
                                    let out_dst = if last_in_cycle && loop_back_index.is_some() {
                                        loop_back_index.unwrap()
                                    } else {
                                        let r = ret.len();
                                        ret.push(Default::default());
                                        r
                                    };

                                    let transition_to_insert = match node.clone() {
                                        RepititionTokenVariant::Byte(b) => {
                                            GraphTokenVariant::Byte(b)
                                        }
                                        RepititionTokenVariant::Captureable(items) => {
                                            GraphTokenVariant::Captureable(SmallVecBincodeWrapper(
                                                items,
                                            ))
                                        }
                                        RepititionTokenVariant::LexicalLevelChange(lv) => {
                                            GraphTokenVariant::LexicalLevelChange(lv)
                                        }
                                        _ => unreachable!(),
                                    };

                                    GraphBuilder::insert_edge(
                                        &mut ret[repitition_current_position],
                                        transition_to_insert,
                                        SetSpan::from(out_dst, false, false),
                                    );
                                    repitition_current_position = out_dst;
                                }
                                RepititionTokenVariant::Uncontained => {
                                    if last_in_cycle && loop_back_index.is_some() {
                                        // delete the current node
                                        ret.pop();
                                        // redirect the transitions that were going
                                        // to the current node back to the loopback
                                        Self::redirect_transitions(
                                            ret.last_mut().unwrap(),
                                            repitition_current_position,
                                            loop_back_index.unwrap(),
                                        );
                                        repitition_current_position = loop_back_index.unwrap();
                                    } else {
                                        // since each unique repitition is independent there is
                                        // no need to separate the 0 and 1 or more cases
                                        //
                                        // simply add the loop at the current node
                                        match &mut ret[repitition_current_position].ellipsis.other {
                                            GraphNodeEllipsisInfoEnum::Uncontained(un_items) => {
                                                un_items.push(repitition_current_position);
                                            }
                                            GraphNodeEllipsisInfoEnum::None => {
                                                ret[repitition_current_position].ellipsis.other =
                                                    GraphNodeEllipsisInfoEnum::Uncontained(vec![
                                                        repitition_current_position,
                                                    ]);
                                            }
                                            _ => {
                                                // never
                                                return Err("... clashing in builder".to_string());
                                            }
                                        }
                                    }
                                }
                                RepititionTokenVariant::Corner => {
                                    if last_in_cycle && loop_back_index.is_some() {
                                        // delete the current node
                                        ret.pop();
                                        // redirect the transitions that were going
                                        // to the current node back to the loopback
                                        Self::redirect_transitions(
                                            ret.last_mut().unwrap(),
                                            repitition_current_position,
                                            loop_back_index.unwrap(),
                                        );
                                        repitition_current_position = loop_back_index.unwrap();
                                    } else {
                                        ret[repitition_current_position]
                                            .ellipsis
                                            .corner
                                            .push(repitition_current_position);
                                    }
                                }
                                RepititionTokenVariant::Round => {
                                    if last_in_cycle && loop_back_index.is_some() {
                                        // delete the current node
                                        ret.pop();
                                        // redirect the transitions that were going
                                        // to the current node back to the loopback
                                        Self::redirect_transitions(
                                            ret.last_mut().unwrap(),
                                            repitition_current_position,
                                            loop_back_index.unwrap(),
                                        );
                                        repitition_current_position = loop_back_index.unwrap();
                                    } else {
                                        match &mut ret[repitition_current_position].ellipsis.other {
                                            GraphNodeEllipsisInfoEnum::Round(un_items) => {
                                                un_items.push(repitition_current_position);
                                            }
                                            GraphNodeEllipsisInfoEnum::None => {
                                                ret[repitition_current_position].ellipsis.other =
                                                    GraphNodeEllipsisInfoEnum::Round(vec![
                                                        repitition_current_position,
                                                    ]);
                                            }
                                            _ => {
                                                // never
                                                return Err("(...) clashing in builder".to_string());
                                            }
                                        }
                                    }
                                }
                                RepititionTokenVariant::Square => {
                                    if last_in_cycle && loop_back_index.is_some() {
                                        // delete the current node
                                        ret.pop();
                                        // redirect the transitions that were going
                                        // to the current node back to the loopback
                                        Self::redirect_transitions(
                                            ret.last_mut().unwrap(),
                                            repitition_current_position,
                                            loop_back_index.unwrap(),
                                        );
                                        repitition_current_position = loop_back_index.unwrap();
                                    } else {
                                        match &mut ret[repitition_current_position].ellipsis.other {
                                            GraphNodeEllipsisInfoEnum::Square(un_items) => {
                                                un_items.push(repitition_current_position);
                                            }
                                            GraphNodeEllipsisInfoEnum::None => {
                                                ret[repitition_current_position].ellipsis.other =
                                                    GraphNodeEllipsisInfoEnum::Square(vec![
                                                        repitition_current_position,
                                                    ]);
                                            }
                                            _ => {
                                                // never
                                                return Err("[...] clashing in builder".to_string());
                                            }
                                        }
                                    }
                                }
                                RepititionTokenVariant::Curly => {
                                    if last_in_cycle && loop_back_index.is_some() {
                                        // delete the current node
                                        ret.pop();
                                        // redirect the transitions that were going
                                        // to the current node back to the loopback
                                        Self::redirect_transitions(
                                            ret.last_mut().unwrap(),
                                            repitition_current_position,
                                            loop_back_index.unwrap(),
                                        );
                                        repitition_current_position = loop_back_index.unwrap();
                                    } else {
                                        match &mut ret[repitition_current_position].ellipsis.other {
                                            GraphNodeEllipsisInfoEnum::Curly(un_items) => {
                                                un_items.push(repitition_current_position);
                                            }
                                            GraphNodeEllipsisInfoEnum::None => {
                                                ret[repitition_current_position].ellipsis.other =
                                                    GraphNodeEllipsisInfoEnum::Curly(vec![
                                                        repitition_current_position,
                                                    ]);
                                            }
                                            _ => {
                                                // never
                                                return Err("{...} clashing in builder".to_string());
                                            }
                                        }
                                    }
                                }
                                RepititionTokenVariant::ScopeBlocking => {
                                    if last_in_cycle && loop_back_index.is_some() {
                                        // delete the current node
                                        ret.pop();
                                        // redirect the transitions that were going
                                        // to the current node back to the loopback
                                        Self::redirect_transitions(
                                            ret.last_mut().unwrap(),
                                            repitition_current_position,
                                            loop_back_index.unwrap(),
                                        );
                                        repitition_current_position = loop_back_index.unwrap();
                                    } else {
                                        ret[repitition_current_position]
                                            .scope_blocking
                                            .push(repitition_current_position);
                                    }
                                }
                                RepititionTokenVariant::StatementBlocking => {
                                    if last_in_cycle && loop_back_index.is_some() {
                                        // delete the current node
                                        ret.pop();
                                        // redirect the transitions that were going
                                        // to the current node back to the loopback
                                        Self::redirect_transitions(
                                            ret.last_mut().unwrap(),
                                            repitition_current_position,
                                            loop_back_index.unwrap(),
                                        );
                                        repitition_current_position = loop_back_index.unwrap();
                                    } else {
                                        ret[repitition_current_position]
                                            .statement_blocking
                                            .push(repitition_current_position);
                                    }
                                }
                                RepititionTokenVariant::NonCapturingCapture => {
                                    let out_dst = if last_in_cycle && loop_back_index.is_some() {
                                        loop_back_index.unwrap()
                                    } else {
                                        let r = ret.len();
                                        ret.push(Default::default());
                                        r
                                    };

                                    ret[repitition_current_position]
                                        .non_capture
                                        .push(SetSpan::from(out_dst, false, false));
                                    repitition_current_position = out_dst;
                                }
                                RepititionTokenVariant::CreateCapture => {
                                    let out_dst = if last_in_cycle && loop_back_index.is_some() {
                                        loop_back_index.unwrap()
                                    } else {
                                        let r = ret.len();
                                        ret.push(Default::default());
                                        r
                                    };

                                    ret[repitition_current_position]
                                        .create_capture
                                        .push(SetSpan::from(out_dst, false, false));
                                    repitition_current_position = out_dst;
                                }
                                RepititionTokenVariant::CreateReplace(n) => {
                                    let out_dst = if last_in_cycle && loop_back_index.is_some() {
                                        loop_back_index.unwrap()
                                    } else {
                                        let r = ret.len();
                                        ret.push(Default::default());
                                        r
                                    };
                                    ret[repitition_current_position]
                                        .create_replace.0
                                        .entry(*n)
                                        .or_default()
                                        .push(SetSpan::from(out_dst, false, false));
                                    repitition_current_position = out_dst;
                                }
                                RepititionTokenVariant::Backref(n) => {
                                    let out_dst = if last_in_cycle && loop_back_index.is_some() {
                                        loop_back_index.unwrap()
                                    } else {
                                        let r = ret.len();
                                        ret.push(Default::default());
                                        r
                                    };
                                    ret[repitition_current_position]
                                        .backref.0
                                        .entry(*n)
                                        .or_default()
                                        .push(SetSpan::from(out_dst, false, false));
                                    repitition_current_position = out_dst;
                                }
                                RepititionTokenVariant::PopReplace(n) => {
                                    let out_dst = if last_in_cycle && loop_back_index.is_some() {
                                        loop_back_index.unwrap()
                                    } else {
                                        let r = ret.len();
                                        ret.push(Default::default());
                                        r
                                    };
                                    ret[repitition_current_position]
                                        .pop_replace.0
                                        .entry(*n)
                                        .or_default()
                                        .push(SetSpan::from(out_dst, false, false));
                                    repitition_current_position = out_dst;
                                }
                            }
                        }
                    }
                }
                // transition from the loop node
                let new_transitions = new_transitions_only.pop().unwrap();
                // transition from the loop node
                Self::copy_transitions(&new_transitions, &mut ret[loop_back_index.unwrap()])?;
                // put the transitions back that were taken before
                Self::copy_transitions(&new_transitions, &mut ret[ret_current_position])?;
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize)]
pub struct AHashMapBincode<K: Eq + std::hash::Hash, V: PartialEq>(pub AHashMap<K, V>);

impl<K: Eq + std::hash::Hash, V: PartialEq> Default for AHashMapBincode<K, V> {
    fn default() -> Self {
        Self(AHashMap::new())
    }
}

impl<K: Eq + std::hash::Hash, V: PartialEq> AHashMapBincode<K, V> {
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl<K, V> bincode::Encode for AHashMapBincode<K, V>
where
    K: bincode::Encode + Eq + std::hash::Hash,
    V: bincode::Encode + PartialEq,
{
    fn encode<E: bincode::enc::Encoder>(
        &self,
        encoder: &mut E,
    ) -> Result<(), bincode::error::EncodeError> {
        let len = self.0.len() as u64;
        len.encode(encoder)?;

        for (k, v) in &self.0 {
            k.encode(encoder)?;
            v.encode(encoder)?;
        }

        Ok(())
    }
}

impl<Context, K, V> Decode<Context> for AHashMapBincode<K, V>
where
    K: Decode<Context> + Eq + std::hash::Hash,
    V: Decode<Context> + PartialEq,
{
    fn decode<D: bincode::de::Decoder<Context = Context>>(
        decoder: &mut D,
    ) -> Result<Self, bincode::error::DecodeError> {
        let len = u64::decode(decoder)? as usize;

        let mut map = AHashMap::with_capacity(len);

        for _ in 0..len {
            let k = K::decode(decoder)?;
            let v = V::decode(decoder)?;
            map.insert(k, v);
        }

        Ok(Self(map))
    }
}

impl<'de, Context, K, V> bincode::BorrowDecode<'de, Context> for AHashMapBincode<K, V>
where
    K: Decode<Context> + Eq + std::hash::Hash,
    V: Decode<Context> + PartialEq,
{
    fn borrow_decode<D>(decoder: &mut D) -> Result<Self, bincode::error::DecodeError>
    where
        D: bincode::de::BorrowDecoder<'de, Context = Context>,
    {
        // We ignore borrowing entirely and just decode owned values
        let len = u64::decode(decoder)? as usize;

        let mut map = ahash::AHashMap::with_capacity(len);

        for _ in 0..len {
            let k = K::decode(decoder)?;
            let v = V::decode(decoder)?;
            map.insert(k, v);
        }

        Ok(Self(map))
    }
}

#[derive(
    Debug, Default, Clone, serde::Serialize, bincode::Encode, bincode::Decode, PartialEq, Eq,
)]
pub struct GraphNode {
    #[serde(skip_serializing_if = "AHashMapBincode::is_empty")]
    pub edge: AHashMapBincode<GraphTokenVariant, Vec<SetSpan>>,
    #[serde(skip_serializing_if = "GraphNodeEllipsisInfo::is_default")]
    pub ellipsis: GraphNodeEllipsisInfo,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub scope_blocking: Vec<usize>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub statement_blocking: Vec<usize>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub create_capture: Vec<SetSpan>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub non_capture: Vec<SetSpan>,

    /// associates capture index with where to go
    #[serde(skip_serializing_if = "AHashMapBincode::is_empty")]
    pub(crate) backref: AHashMapBincode<usize, Vec<SetSpan>>,
    #[serde(skip_serializing_if = "AHashMapBincode::is_empty")]
    pub(crate) create_replace: AHashMapBincode<usize, Vec<SetSpan>>,
    #[serde(skip_serializing_if = "AHashMapBincode::is_empty")]
    pub(crate) pop_replace: AHashMapBincode<usize, Vec<SetSpan>>,

    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub full_match: Vec<PatternInfo>,
}

#[derive(Debug, serde::Serialize, bincode::Encode, bincode::Decode, PartialEq, Eq)]
pub struct Graph {
    pub nodes: Vec<GraphNode>,
}

impl Graph {
    pub fn is_default(&self) -> bool {
        *self == GraphBuilder::default().build().unwrap()
    }
}
