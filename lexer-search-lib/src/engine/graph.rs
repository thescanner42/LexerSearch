use std::{
    collections::{BTreeMap, HashMap, HashSet},
    fmt,
    num::NonZero,
    vec,
};

use serde::{Serialize, Serializer};

use crate::{
    engine::{
        canonicalizer::Canonicalizer,
        token::{
            BracketStack, GraphBuilderBracketType, Repitition, RepititionTokenVariant, Token,
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

/// where to go, and if the start should be set when accepting this token
#[derive(
    Debug,
    Clone,
    serde::Serialize,
    serde::Deserialize,
    PartialEq,
    Eq,
    bincode::Encode,
    bincode::Decode,
)]
pub enum SetStartEnum {
    No(usize),
    Yes(usize),
    /// no and yes, respectively
    Both(usize, usize),
}

impl SetStartEnum {
    pub fn from(i: usize, set_start: bool) -> Self {
        if set_start {
            SetStartEnum::Yes(i)
        } else {
            SetStartEnum::No(i)
        }
    }

    pub fn handle(&self, mut f: impl FnMut(usize, bool)) {
        match self {
            SetStartEnum::No(no_index) => f(*no_index, false),
            SetStartEnum::Yes(yes_index) => f(*yes_index, true),
            SetStartEnum::Both(no_index, yes_index) => {
                f(*no_index, false);
                f(*yes_index, true);
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, bincode::Encode, bincode::Decode)]
pub enum GraphTokenVariant {
    Byte(u8),
    Captureable(Box<[u8]>),
    /// see LexerTokenVariant for more details
    LexicalLevelChange(i32),
    /// index N on capture stack was matched
    Backref(usize),
    BackrefReplace(usize),
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
                if bytes.iter().all(|b| b.is_ascii_graphic() || *b == b' ') {
                    for b in bytes.iter() {
                        write!(f, "{}", *b as char)?;
                    }
                    Ok(())
                } else {
                    // mixed or non-ascii: fall back to comma-separated decimals
                    for (i, b) in bytes.iter().enumerate() {
                        if i > 0 {
                            write!(f, ",")?;
                        }
                        write!(f, "{:#x}", b)?;
                    }
                    Ok(())
                }
            }
            GraphTokenVariant::LexicalLevelChange(v) => write!(f, "l:{v}"),
            GraphTokenVariant::Backref(n) => write!(f, "r:{n}"),
            GraphTokenVariant::BackrefReplace(n) => write!(f, "rr:{n}"),
        }
    }
}

impl Serialize for GraphTokenVariant {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        // always serialize as a string — works both as value and as map key
        serializer.serialize_str(&self.to_string())
    }
}

/// it's not possible to have ... be contained and not contained in brackets yet
/// reaching the same node, inside of a pattern. this enum represents that
#[derive(Debug, PartialEq, Eq)]
pub enum GraphBuilderNodeEllipsisEnum {
    Uncontained(usize),
    Corner(usize),
    /// uncontained then corner, respectively
    ///
    /// since both are writeable:
    /// - vector< ... >
    /// - vector< ..> >
    UncontainedAndCorner(usize, usize),
    Round(usize),
    Square(usize),
    Curly(usize),
}

#[derive(Default, Debug, PartialEq, Eq)]
pub struct GraphBuilderNode {
    pub(crate) edge: HashMap<GraphTokenVariant, SetStartEnum>,
    pub(crate) repitition: HashMap<Repitition, usize>,
    pub(crate) ellipsis: Option<GraphBuilderNodeEllipsisEnum>,
    pub(crate) scope_blocking: Option<usize>,
    pub(crate) statement_blocking: Option<usize>,
    pub(crate) create_capture: Option<SetStartEnum>,
    pub(crate) non_capture: Option<SetStartEnum>,
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
        let mut graph_position = 0usize;
        // the current reptitition and the captures that were created in the
        // repitition
        let mut current_repitition: Option<(Repitition, Vec<Box<[u8]>>)> = None;
        let mut set_start = false;

        // keep track of first time seeing capture vs backreference
        let mut captures: HashMap<Box<[u8]>, usize> = Default::default();
        let mut capture_count_increment = 0;

        loop {
            let token: Token = match canonicalizer.next_and_drain(|| {
                let token = match lexer.next_and_drain(r)? {
                    Some(s) => s,
                    None => return Ok(None),
                };
                return Ok(Some(Token::from_lexer_token(token)));
            })? {
                None => {
                    // no more input
                    fn reverse_map(
                        captures: HashMap<Box<[u8]>, usize>,
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
                    v.variant.add_double_quotes();
                    v
                }
            };

            match token.variant {
                TokenVariant::OverlongIdentifier
                | TokenVariant::OverlongString
                | TokenVariant::OverlongCapture => {
                    return Err(format!(
                        "{} line {} col {}: token too long",
                        name, token.start.line, token.start.column
                    ));
                }
                _ => {}
            }

            if let TokenVariant::Byte(b) = token.variant {
                bracket_stack.handle(b);
            }

            let nodes_len = self.nodes.len();
            if matches!(token.variant, TokenVariant::Ellipsis(EllipsisEnum::Jump)) {
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
                        let mut r: Repitition = Default::default();
                        if std::mem::take(&mut set_start) {
                            r.push(RepititionTokenVariant::SetStart);
                        }
                        current_repitition = Some((Default::default(), Default::default()));
                    }
                }
                continue;
            } else {
                if let Some((repitition, new_captures)) = current_repitition.as_mut() {
                    repitition.push(match token.variant {
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
                            EllipsisEnum::Jump => unreachable!(), // above
                            EllipsisEnum::Normal => match bracket_stack.last() {
                                None => RepititionTokenVariant::Uncontained,
                                Some(v) => match v {
                                    GraphBuilderBracketType::Round => RepititionTokenVariant::Round,
                                    GraphBuilderBracketType::Square => {
                                        RepititionTokenVariant::Square
                                    }
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
                            EllipsisEnum::SetStart => RepititionTokenVariant::SetStart,
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
                                    match new_captures.pop() {
                                        Some(new_capture) => {
                                            captures.remove(&new_capture).unwrap();
                                            capture_count_increment -= 1;
                                            if let Some(v) = captures.get(&items[1..]) {
                                                RepititionTokenVariant::BackrefReplace(*v)
                                            } else {
                                                return Err("backref replace must reference an existing capture".to_string());
                                            }
                                        }
                                        None => return Err("expecting prior capture in repitition".to_string()),
                                    }
                                }
                                _ => unreachable!(),
                            }
                        }
                    });
                    continue;
                }
            }

            match token.variant {
                TokenVariant::Ellipsis(e) => {
                    match e {
                        EllipsisEnum::Jump => unreachable!(), // handled above
                        EllipsisEnum::CBE => {
                            // corner bracket ellipsis
                            match &self.nodes[graph_position].ellipsis {
                                None => {
                                    self.nodes[graph_position].ellipsis =
                                        Some(GraphBuilderNodeEllipsisEnum::Corner(nodes_len));
                                    self.nodes.push(Default::default());
                                    graph_position = nodes_len;
                                }
                                Some(v) => match v {
                                    GraphBuilderNodeEllipsisEnum::Uncontained(un) => {
                                        self.nodes[graph_position].ellipsis = Some(
                                            GraphBuilderNodeEllipsisEnum::UncontainedAndCorner(
                                                *un, nodes_len,
                                            ),
                                        );
                                        self.nodes.push(Default::default());
                                        graph_position = nodes_len;
                                    }
                                    GraphBuilderNodeEllipsisEnum::Corner(v) => {
                                        graph_position = *v;
                                    }
                                    GraphBuilderNodeEllipsisEnum::UncontainedAndCorner(
                                        _,
                                        corner,
                                    ) => {
                                        graph_position = *corner;
                                    }
                                    _ => {
                                        // never occcurs
                                        return Err(
                                            "internal pattern conflict, attempted to add ..>"
                                                .to_string(),
                                        );
                                    }
                                },
                            }
                        }
                        EllipsisEnum::Normal => {
                            match bracket_stack.last() {
                                None => {
                                    // uncontained ellipsis
                                    match &self.nodes[graph_position].ellipsis {
                                        None => {
                                            self.nodes[graph_position].ellipsis =
                                                Some(GraphBuilderNodeEllipsisEnum::Uncontained(
                                                    nodes_len,
                                                ));
                                            self.nodes.push(Default::default());
                                            graph_position = nodes_len;
                                        }
                                        Some(v) => match v {
                                            GraphBuilderNodeEllipsisEnum::Uncontained(v) => {
                                                graph_position = *v;
                                            }
                                            GraphBuilderNodeEllipsisEnum::Corner(corner) => {
                                                self.nodes[graph_position].ellipsis = Some(
                                                    GraphBuilderNodeEllipsisEnum::UncontainedAndCorner(
                                                        nodes_len, *corner,
                                                    ),
                                                );
                                                self.nodes.push(Default::default());
                                                graph_position = nodes_len;
                                            }
                                            GraphBuilderNodeEllipsisEnum::UncontainedAndCorner(
                                                un,
                                                _,
                                            ) => {
                                                graph_position = *un;
                                            }
                                            _ => {
                                                // never occcurs
                                                return Err(
                                                        "internal pattern conflict, attempted to add ..."
                                                            .to_string(),
                                                    );
                                            }
                                        },
                                    }
                                }
                                Some(v) => {
                                    // contained ellipsis
                                    match v {
                                        GraphBuilderBracketType::Round => {
                                            match &self.nodes[graph_position].ellipsis {
                                                None => {
                                                    self.nodes[graph_position].ellipsis =
                                                        Some(GraphBuilderNodeEllipsisEnum::Round(
                                                            nodes_len,
                                                        ));
                                                    self.nodes.push(Default::default());
                                                    graph_position = nodes_len;
                                                }
                                                Some(v) => {
                                                    match v {
                                                        GraphBuilderNodeEllipsisEnum::Round(v) => {
                                                            graph_position = *v;
                                                        }
                                                        _ => {
                                                            // never occcurs
                                                            return Err(
                                                                    "internal pattern conflict, attempted to add (...)"
                                                                        .to_string(),
                                                                );
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                        GraphBuilderBracketType::Square => {
                                            match &self.nodes[graph_position].ellipsis {
                                                None => {
                                                    self.nodes[graph_position].ellipsis =
                                                        Some(GraphBuilderNodeEllipsisEnum::Square(
                                                            nodes_len,
                                                        ));
                                                    self.nodes.push(Default::default());
                                                    graph_position = nodes_len;
                                                }
                                                Some(v) => {
                                                    match v {
                                                        GraphBuilderNodeEllipsisEnum::Square(v) => {
                                                            graph_position = *v;
                                                        }
                                                        _ => {
                                                            // never occcurs
                                                            return Err(
                                                                    "internal pattern conflict, attempted to add [...]"
                                                                        .to_string(),
                                                                );
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                        GraphBuilderBracketType::Curly => {
                                            match &self.nodes[graph_position].ellipsis {
                                                None => {
                                                    self.nodes[graph_position].ellipsis =
                                                        Some(GraphBuilderNodeEllipsisEnum::Curly(
                                                            nodes_len,
                                                        ));
                                                    self.nodes.push(Default::default());
                                                    graph_position = nodes_len;
                                                }
                                                Some(v) => {
                                                    match v {
                                                        GraphBuilderNodeEllipsisEnum::Curly(v) => {
                                                            graph_position = *v;
                                                        }
                                                        _ => {
                                                            // never occcurs
                                                            return Err(
                                                                    "internal pattern conflict, attempted to add {...}"
                                                                        .to_string(),
                                                                );
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
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
                        EllipsisEnum::SetStart => {
                            set_start = true;
                        }
                    }
                }

                _ => {
                    let set_start = std::mem::take(&mut set_start);

                    // convert the token variant to graph token variant
                    let variant = match token.variant {
                        TokenVariant::OverlongIdentifier
                        | TokenVariant::OverlongString
                        | TokenVariant::OverlongCapture
                        | TokenVariant::Ellipsis(_) => unreachable!(), // handled by various paths above
                        TokenVariant::Byte(b) => GraphTokenVariant::Byte(b),
                        TokenVariant::String(items) | TokenVariant::Identifier(items) => {
                            GraphTokenVariant::Captureable(items)
                        }
                        TokenVariant::LexicalLevelChange(v) => {
                            GraphTokenVariant::LexicalLevelChange(v)
                        }
                        TokenVariant::Capture(items) => {
                            if *items == *b"$_" {
                                match self.nodes[graph_position].non_capture.as_ref() {
                                    Some(v) => match v {
                                        SetStartEnum::No(no_index) => {
                                            if !set_start {
                                                graph_position = *no_index;
                                            } else {
                                                self.nodes[graph_position].non_capture =
                                                    Some(SetStartEnum::Both(*no_index, nodes_len));
                                                self.nodes.push(Default::default());
                                                graph_position = nodes_len;
                                            }
                                        }
                                        SetStartEnum::Yes(yes_index) => {
                                            if set_start {
                                                graph_position = *yes_index;
                                            } else {
                                                self.nodes[graph_position].non_capture =
                                                    Some(SetStartEnum::Both(nodes_len, *yes_index));
                                                self.nodes.push(Default::default());
                                                graph_position = nodes_len;
                                            }
                                        }
                                        SetStartEnum::Both(no_index, yes_index) => {
                                            if set_start {
                                                graph_position = *yes_index;
                                            } else {
                                                graph_position = *no_index;
                                            }
                                        }
                                    },
                                    None => {
                                        self.nodes[graph_position].non_capture =
                                            Some(SetStartEnum::from(nodes_len, set_start));
                                        self.nodes.push(Default::default());
                                        graph_position = nodes_len;
                                    }
                                }
                                continue;
                            } else {
                                if items[0] != b'$' {
                                    return Err(
                                        "backref replace only allowed in repitition".to_string()
                                    );
                                }
                                match captures.get(&items[1..]) {
                                    Some(ref_num) => {
                                        // capture was stated previously, this is a
                                        // backreference
                                        GraphTokenVariant::Backref(*ref_num)
                                    }
                                    None => {
                                        captures.insert(items[1..].into(), capture_count_increment);
                                        capture_count_increment += 1;
                                        match self.nodes[graph_position].create_capture.as_ref() {
                                            Some(v) => match v {
                                                SetStartEnum::No(no_index) => {
                                                    if !set_start {
                                                        graph_position = *no_index;
                                                    } else {
                                                        self.nodes[graph_position].create_capture =
                                                            Some(SetStartEnum::Both(
                                                                *no_index, nodes_len,
                                                            ));
                                                        self.nodes.push(Default::default());
                                                        graph_position = nodes_len;
                                                    }
                                                }
                                                SetStartEnum::Yes(yes_index) => {
                                                    if set_start {
                                                        graph_position = *yes_index;
                                                    } else {
                                                        self.nodes[graph_position].create_capture =
                                                            Some(SetStartEnum::Both(
                                                                nodes_len, *yes_index,
                                                            ));
                                                        self.nodes.push(Default::default());
                                                        graph_position = nodes_len;
                                                    }
                                                }
                                                SetStartEnum::Both(no_index, yes_index) => {
                                                    if set_start {
                                                        graph_position = *yes_index;
                                                    } else {
                                                        graph_position = *no_index;
                                                    }
                                                }
                                            },
                                            None => {
                                                self.nodes[graph_position].create_capture =
                                                    Some(SetStartEnum::from(nodes_len, set_start));
                                                self.nodes.push(Default::default());
                                                graph_position = nodes_len;
                                            }
                                        }
                                        continue;
                                    }
                                }
                            }
                        }
                    };

                    match self.nodes[graph_position].edge.entry(variant) {
                        std::collections::hash_map::Entry::Occupied(mut occupied_entry) => {
                            match occupied_entry.get() {
                                SetStartEnum::No(no_index) => {
                                    if !set_start {
                                        graph_position = *no_index; // follow existing path
                                    } else {
                                        *occupied_entry.get_mut() =
                                            SetStartEnum::Both(*no_index, nodes_len);
                                        self.nodes.push(Default::default());
                                        graph_position = nodes_len;
                                    }
                                }
                                SetStartEnum::Yes(yes_index) => {
                                    if set_start {
                                        graph_position = *yes_index; // follow existing path
                                    } else {
                                        *occupied_entry.get_mut() =
                                            SetStartEnum::Both(nodes_len, *yes_index);
                                        self.nodes.push(Default::default());
                                        graph_position = nodes_len;
                                    }
                                }
                                SetStartEnum::Both(no_index, yes_index) => {
                                    if set_start {
                                        graph_position = *yes_index;
                                    } else {
                                        graph_position = *no_index;
                                    }
                                }
                            }
                        }
                        std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                            if set_start {
                                vacant_entry.insert(SetStartEnum::Yes(nodes_len));
                            } else {
                                vacant_entry.insert(SetStartEnum::No(nodes_len));
                            }
                            self.nodes.push(Default::default());
                            graph_position = nodes_len;
                        }
                    }
                }
            };
        }

        if set_start {
            return Err("..^ isn't effective in this position".to_string());
        }

        Ok(())
    }

    pub fn build(mut self) -> Result<Graph, String> {
        let mut ret = vec![Default::default()];
        // depth first traversal through the trie
        Self::build_subroutine(&mut self, 0, &mut ret, None)?;
        Ok(Graph { nodes: ret })
    }

    fn insert_edge(node: &mut GraphNode, k: GraphTokenVariant, v: SetStartEnum) {
        match node.edge.entry(k) {
            std::collections::hash_map::Entry::Occupied(mut occupied_entry) => {
                occupied_entry.get_mut().push(v);
            }
            std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(vec![v]);
            }
        }
    }

    fn copy_transitions(src: &GraphNode, dst: &mut GraphNode) -> Result<(), String> {
        for (k, v) in src.edge.iter() {
            match dst.edge.entry(k.clone()) {
                std::collections::hash_map::Entry::Occupied(mut occupied_entry) => {
                    occupied_entry.get_mut().extend_from_slice(v);
                }
                std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                    vacant_entry.insert(v.to_vec());
                }
            }
        }

        match (&src.ellipsis, &mut dst.ellipsis) {
            (GraphNodeEllipsisEnum::None, _) => {}
            (_, GraphNodeEllipsisEnum::None) => {
                dst.ellipsis = src.ellipsis.clone();
            }
            (GraphNodeEllipsisEnum::Round(src), GraphNodeEllipsisEnum::Round(dst))
            | (GraphNodeEllipsisEnum::Square(src), GraphNodeEllipsisEnum::Square(dst))
            | (GraphNodeEllipsisEnum::Curly(src), GraphNodeEllipsisEnum::Curly(dst)) => {
                dst.extend_from_slice(src);
            }
            (
                GraphNodeEllipsisEnum::UncontainedAndCorner(src1, src2),
                GraphNodeEllipsisEnum::UncontainedAndCorner(dst1, dst2),
            ) => {
                dst1.extend_from_slice(src1);
                dst2.extend_from_slice(src2);
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

        fn rewrite_setstart_enum_vec(v: &mut [SetStartEnum], src: usize, dst: usize) {
            for item in v {
                rewrite_setstart(item, src, dst);
            }
        }

        fn rewrite_setstart(t: &mut SetStartEnum, src: usize, dst: usize) {
            match t {
                SetStartEnum::No(v) => {
                    if *v == src {
                        *v = dst;
                    }
                }
                SetStartEnum::Yes(v) => {
                    if *v == src {
                        *v = dst;
                    }
                }
                SetStartEnum::Both(no, yes) => {
                    if *no == src {
                        *no = dst;
                    }

                    if *yes == src {
                        *yes = dst;
                    }
                }
            }
        }

        for targets in g.edge.values_mut() {
            rewrite_setstart_enum_vec(targets, src, dst);
        }

        match &mut g.ellipsis {
            GraphNodeEllipsisEnum::None => {}

            GraphNodeEllipsisEnum::Round(v)
            | GraphNodeEllipsisEnum::Square(v)
            | GraphNodeEllipsisEnum::Curly(v) => {
                rewrite_vec(v, src, dst);
            }

            GraphNodeEllipsisEnum::UncontainedAndCorner(v1, v2) => {
                rewrite_vec(v1, src, dst);
                rewrite_vec(v2, src, dst);
            }
        }

        rewrite_setstart_enum_vec(&mut g.create_capture, src, dst);
        rewrite_setstart_enum_vec(&mut g.non_capture, src, dst);
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
            match v {
                SetStartEnum::No(in_no) => {
                    let out_dst = ret.len();
                    ret.push(Default::default());
                    Self::build_subroutine(self, in_no, ret, None)?;
                    Self::insert_edge(&mut ret[ret_current_position], k, SetStartEnum::No(out_dst));
                }
                SetStartEnum::Yes(in_yes) => {
                    let out_dst = ret.len();
                    ret.push(Default::default());
                    Self::build_subroutine(self, in_yes, ret, None)?;
                    Self::insert_edge(
                        &mut ret[ret_current_position],
                        k,
                        SetStartEnum::Yes(out_dst),
                    );
                }
                SetStartEnum::Both(in_no, in_yes) => {
                    let out_dst_no = ret.len();
                    ret.push(Default::default());
                    Self::build_subroutine(self, in_no, ret, None)?;
                    let out_dst_yes = ret.len();
                    ret.push(Default::default());
                    Self::build_subroutine(self, in_yes, ret, None)?;
                    Self::insert_edge(
                        &mut ret[ret_current_position],
                        k,
                        SetStartEnum::Both(out_dst_no, out_dst_yes),
                    );
                }
            }
        }

        if let Some(ellipsis) = std::mem::take(&mut self.nodes[in_position].ellipsis) {
            match ellipsis {
                GraphBuilderNodeEllipsisEnum::Uncontained(un) => {
                    let before_modification = std::mem::take(&mut ret[ret_current_position]);
                    // first build the zero case
                    Self::build_subroutine(self, un, ret, Some(ret_current_position))?;
                    let new_transitions_only = std::mem::take(&mut ret[ret_current_position]);
                    ret[ret_current_position] = before_modification;

                    // next the 1 or more case
                    let loop_dst = ret.len();
                    ret.push(Default::default());
                    // loop at the loop node
                    ret[loop_dst].ellipsis =
                        GraphNodeEllipsisEnum::UncontainedAndCorner(vec![loop_dst], vec![]);
                    // transition to the loop node
                    match &mut ret[ret_current_position].ellipsis {
                        GraphNodeEllipsisEnum::None => {
                            ret[ret_current_position].ellipsis =
                                GraphNodeEllipsisEnum::UncontainedAndCorner(vec![loop_dst], vec![]);
                        }
                        GraphNodeEllipsisEnum::UncontainedAndCorner(un_items, _items1) => {
                            un_items.push(loop_dst);
                        }
                        _ => {
                            // never
                            return Err("... clashing in builder".to_string());
                        }
                    }
                    // transition from the loop node
                    Self::copy_transitions(&new_transitions_only, &mut ret[loop_dst])?;
                    // put the transitions back that were taken before
                    Self::copy_transitions(&new_transitions_only, &mut ret[ret_current_position])?;
                }
                GraphBuilderNodeEllipsisEnum::Corner(corner) => {
                    let before_modification = std::mem::take(&mut ret[ret_current_position]);
                    // first build the zero case
                    Self::build_subroutine(self, corner, ret, Some(ret_current_position))?;
                    let new_transitions_only = std::mem::take(&mut ret[ret_current_position]);
                    ret[ret_current_position] = before_modification;

                    // next the 1 or more case
                    let loop_dst = ret.len();
                    ret.push(Default::default());
                    // loop at the loop node
                    ret[loop_dst].ellipsis =
                        GraphNodeEllipsisEnum::UncontainedAndCorner(vec![], vec![loop_dst]);
                    // transition to the loop node
                    match &mut ret[ret_current_position].ellipsis {
                        GraphNodeEllipsisEnum::None => {
                            ret[ret_current_position].ellipsis =
                                GraphNodeEllipsisEnum::UncontainedAndCorner(vec![], vec![loop_dst]);
                        }
                        GraphNodeEllipsisEnum::UncontainedAndCorner(_, corner_items) => {
                            corner_items.push(loop_dst);
                        }
                        _ => {
                            // never
                            return Err("..> clashing in builder".to_string());
                        }
                    }
                    // transition from the loop node
                    Self::copy_transitions(&new_transitions_only, &mut ret[loop_dst])?;
                    // put the transitions back that were taken before
                    Self::copy_transitions(&new_transitions_only, &mut ret[ret_current_position])?;
                }
                GraphBuilderNodeEllipsisEnum::UncontainedAndCorner(un, corner) => {
                    let before_modification = std::mem::take(&mut ret[ret_current_position]);
                    // first build the zero case
                    Self::build_subroutine(self, un, ret, Some(ret_current_position))?;
                    let un_transitions_only = std::mem::take(&mut ret[ret_current_position]);
                    Self::build_subroutine(self, corner, ret, Some(ret_current_position))?;
                    let corner_transitions_only = std::mem::take(&mut ret[ret_current_position]);

                    ret[ret_current_position] = before_modification;

                    // 1 or more case
                    let out_dst_un = ret.len();
                    ret.push(Default::default());
                    let out_dst_corner = ret.len();
                    ret.push(Default::default());
                    // loop at the loop node
                    ret[out_dst_un].ellipsis =
                        GraphNodeEllipsisEnum::UncontainedAndCorner(vec![out_dst_un], vec![]);
                    ret[out_dst_corner].ellipsis =
                        GraphNodeEllipsisEnum::UncontainedAndCorner(vec![], vec![out_dst_corner]);

                    // transition to the loop node
                    match &mut ret[ret_current_position].ellipsis {
                        GraphNodeEllipsisEnum::None => {
                            ret[ret_current_position].ellipsis =
                                GraphNodeEllipsisEnum::UncontainedAndCorner(
                                    vec![out_dst_un],
                                    vec![out_dst_corner],
                                );
                        }
                        GraphNodeEllipsisEnum::UncontainedAndCorner(un_items, corner_items) => {
                            un_items.push(out_dst_un);
                            corner_items.push(out_dst_corner);
                        }
                        _ => {
                            // never
                            return Err("... or ..> clashing in builder".to_string());
                        }
                    }

                    // transition from the loop node
                    Self::copy_transitions(&un_transitions_only, &mut ret[out_dst_un])?;
                    Self::copy_transitions(&corner_transitions_only, &mut ret[out_dst_corner])?;

                    // put the transitions back that were taken before
                    Self::copy_transitions(&un_transitions_only, &mut ret[ret_current_position])?;
                    Self::copy_transitions(
                        &corner_transitions_only,
                        &mut ret[ret_current_position],
                    )?;
                }
                GraphBuilderNodeEllipsisEnum::Round(round) => {
                    let before_modification = std::mem::take(&mut ret[ret_current_position]);
                    // first build the zero case
                    Self::build_subroutine(self, round, ret, Some(ret_current_position))?;
                    let new_transitions_only = std::mem::take(&mut ret[ret_current_position]);
                    ret[ret_current_position] = before_modification;

                    // next the 1 or more case
                    let loop_dst = ret.len();
                    ret.push(Default::default());
                    // loop at the loop node
                    ret[loop_dst].ellipsis = GraphNodeEllipsisEnum::Round(vec![loop_dst]);

                    match &mut ret[ret_current_position].ellipsis {
                        GraphNodeEllipsisEnum::None => {
                            ret[ret_current_position].ellipsis =
                                GraphNodeEllipsisEnum::Round(vec![loop_dst]);
                        }
                        GraphNodeEllipsisEnum::Round(round_items) => {
                            round_items.push(loop_dst);
                        }
                        _ => {
                            // never
                            return Err("(...) clashing in builder".to_string());
                        }
                    }
                    // transition from the loop node
                    Self::copy_transitions(&new_transitions_only, &mut ret[loop_dst])?;
                    // put the transitions back that were taken before
                    Self::copy_transitions(&new_transitions_only, &mut ret[ret_current_position])?;
                }
                GraphBuilderNodeEllipsisEnum::Square(square) => {
                    let before_modification = std::mem::take(&mut ret[ret_current_position]);
                    // first build the zero case
                    Self::build_subroutine(self, square, ret, Some(ret_current_position))?;
                    let new_transitions_only = std::mem::take(&mut ret[ret_current_position]);
                    ret[ret_current_position] = before_modification;

                    // next the 1 or more case
                    let loop_dst = ret.len();
                    ret.push(Default::default());
                    // loop at the loop node
                    ret[loop_dst].ellipsis = GraphNodeEllipsisEnum::Square(vec![loop_dst]);
                    // transition to the loop node
                    match &mut ret[ret_current_position].ellipsis {
                        GraphNodeEllipsisEnum::None => {
                            ret[ret_current_position].ellipsis =
                                GraphNodeEllipsisEnum::Square(vec![loop_dst]);
                        }
                        GraphNodeEllipsisEnum::Square(round_items) => {
                            round_items.push(loop_dst);
                        }
                        _ => {
                            // never
                            return Err("[...] clashing in builder".to_string());
                        }
                    }
                    // transition from the loop node
                    Self::copy_transitions(&new_transitions_only, &mut ret[loop_dst])?;
                    // put the transitions back that were taken before
                    Self::copy_transitions(&new_transitions_only, &mut ret[ret_current_position])?;
                }
                GraphBuilderNodeEllipsisEnum::Curly(curly) => {
                    let before_modification = std::mem::take(&mut ret[ret_current_position]);
                    // first build the zero case
                    Self::build_subroutine(self, curly, ret, Some(ret_current_position))?;
                    let new_transitions_only = std::mem::take(&mut ret[ret_current_position]);
                    ret[ret_current_position] = before_modification;

                    // next the 1 or more case
                    let loop_dst = ret.len();
                    ret.push(Default::default());
                    ret[loop_dst].ellipsis = GraphNodeEllipsisEnum::Curly(vec![loop_dst]);

                    match &mut ret[ret_current_position].ellipsis {
                        GraphNodeEllipsisEnum::None => {
                            ret[ret_current_position].ellipsis =
                                GraphNodeEllipsisEnum::Curly(vec![loop_dst]);
                        }
                        GraphNodeEllipsisEnum::Curly(round_items) => {
                            round_items.push(loop_dst);
                        }
                        _ => {
                            // never
                            return Err("{...} clashing in builder".to_string());
                        }
                    }
                    // transition from the loop node
                    Self::copy_transitions(&new_transitions_only, &mut ret[loop_dst])?;
                    // put the transitions back that were taken before
                    Self::copy_transitions(&new_transitions_only, &mut ret[ret_current_position])?;
                }
            }
        }

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
            match create_capture {
                SetStartEnum::No(no_index) => {
                    let out_dst = ret.len();
                    ret.push(Default::default());
                    Self::build_subroutine(self, no_index, ret, None)?;
                    ret[ret_current_position]
                        .create_capture
                        .push(SetStartEnum::No(out_dst));
                }
                SetStartEnum::Yes(yes_index) => {
                    let out_dst = ret.len();
                    ret.push(Default::default());
                    Self::build_subroutine(self, yes_index, ret, None)?;
                    ret[ret_current_position]
                        .create_capture
                        .push(SetStartEnum::Yes(out_dst));
                }
                SetStartEnum::Both(no_index, yes_index) => {
                    let out_dst_no = ret.len();
                    ret.push(Default::default());
                    Self::build_subroutine(self, no_index, ret, None)?;
                    let out_dst_yes = ret.len();
                    ret.push(Default::default());
                    Self::build_subroutine(self, yes_index, ret, None)?;
                    ret[ret_current_position]
                        .create_capture
                        .push(SetStartEnum::No(out_dst_no));
                    ret[ret_current_position]
                        .create_capture
                        .push(SetStartEnum::Yes(out_dst_yes));
                }
            }
        }

        if let Some(non_capture) = std::mem::take(&mut self.nodes[in_position].non_capture) {
            match non_capture {
                SetStartEnum::No(no_index) => {
                    let out_dst = ret.len();
                    ret.push(Default::default());
                    Self::build_subroutine(self, no_index, ret, None)?;
                    ret[ret_current_position]
                        .non_capture
                        .push(SetStartEnum::No(out_dst));
                }
                SetStartEnum::Yes(yes_index) => {
                    let out_dst = ret.len();
                    ret.push(Default::default());
                    Self::build_subroutine(self, yes_index, ret, None)?;
                    ret[ret_current_position]
                        .non_capture
                        .push(SetStartEnum::Yes(out_dst));
                }
                SetStartEnum::Both(no_index, yes_index) => {
                    let out_dst_no = ret.len();
                    ret.push(Default::default());
                    Self::build_subroutine(self, no_index, ret, None)?;
                    let out_dst_yes = ret.len();
                    ret.push(Default::default());
                    Self::build_subroutine(self, yes_index, ret, None)?;
                    ret[ret_current_position]
                        .non_capture
                        .push(SetStartEnum::No(out_dst_no));
                    ret[ret_current_position]
                        .non_capture
                        .push(SetStartEnum::Yes(out_dst_yes));
                }
            }
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
            let mut repitition_current_position = ret_current_position;
            for (repitition, _) in repititions.drain() {
                let repitition = repitition.get_vec();
                for i in 0..2 {
                    let is_loop = i == 1;
                    let mut set_start = false;
                    let loop_back_index = if is_loop {
                        Some(repitition_current_position)
                    } else {
                        None
                    };
                    for (i, node) in repitition.iter().enumerate() {
                        let last_node_in_loop = is_loop && i == repitition.len() - 1;

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
                            | RepititionTokenVariant::LexicalLevelChange(_)
                            | RepititionTokenVariant::Backref(_)
                            | RepititionTokenVariant::BackrefReplace(_) => {
                                let out_dst = if last_node_in_loop {
                                    loop_back_index.unwrap()
                                } else {
                                    let r = ret.len();
                                    ret.push(Default::default());
                                    r
                                };

                                let transition_to_insert = match node.clone() {
                                    RepititionTokenVariant::Byte(b) => GraphTokenVariant::Byte(b),
                                    RepititionTokenVariant::Captureable(items) => {
                                        GraphTokenVariant::Captureable(items)
                                    }
                                    RepititionTokenVariant::LexicalLevelChange(lv) => {
                                        GraphTokenVariant::LexicalLevelChange(lv)
                                    }
                                    RepititionTokenVariant::Backref(n) => {
                                        GraphTokenVariant::Backref(n)
                                    }
                                    RepititionTokenVariant::BackrefReplace(n) => {
                                        GraphTokenVariant::BackrefReplace(n)
                                    }
                                    _ => unreachable!(),
                                };

                                GraphBuilder::insert_edge(
                                    &mut ret[repitition_current_position],
                                    transition_to_insert,
                                    SetStartEnum::from(out_dst, std::mem::take(&mut set_start)),
                                );
                                repitition_current_position = out_dst;
                            }
                            RepititionTokenVariant::Uncontained => {
                                if last_node_in_loop {
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
                                    match &mut ret[repitition_current_position].ellipsis {
                                        GraphNodeEllipsisEnum::None => {
                                            ret[repitition_current_position].ellipsis =
                                                GraphNodeEllipsisEnum::UncontainedAndCorner(
                                                    vec![repitition_current_position],
                                                    vec![],
                                                );
                                        }
                                        GraphNodeEllipsisEnum::UncontainedAndCorner(
                                            un_items,
                                            _,
                                        ) => {
                                            un_items.push(repitition_current_position);
                                        }
                                        _ => {
                                            // never
                                            return Err("... clashing in builder".to_string());
                                        }
                                    }
                                }
                            }
                            RepititionTokenVariant::Corner => {
                                if last_node_in_loop {
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
                                    match &mut ret[repitition_current_position].ellipsis {
                                        GraphNodeEllipsisEnum::None => {
                                            ret[repitition_current_position].ellipsis =
                                                GraphNodeEllipsisEnum::UncontainedAndCorner(
                                                    vec![],
                                                    vec![repitition_current_position],
                                                );
                                        }
                                        GraphNodeEllipsisEnum::UncontainedAndCorner(
                                            _,
                                            corner_items,
                                        ) => {
                                            corner_items.push(repitition_current_position);
                                        }
                                        _ => {
                                            // never
                                            return Err("..> clashing in builder".to_string());
                                        }
                                    }
                                }
                            }
                            RepititionTokenVariant::Round => {
                                if last_node_in_loop {
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
                                    match &mut ret[repitition_current_position].ellipsis {
                                        GraphNodeEllipsisEnum::None => {
                                            ret[repitition_current_position].ellipsis =
                                                GraphNodeEllipsisEnum::Round(vec![
                                                    repitition_current_position,
                                                ]);
                                        }
                                        GraphNodeEllipsisEnum::Round(items) => {
                                            items.push(repitition_current_position);
                                        }
                                        _ => {
                                            // never
                                            return Err("(...) clashing in builder".to_string());
                                        }
                                    }
                                }
                            }
                            RepititionTokenVariant::Square => {
                                if last_node_in_loop {
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
                                    match &mut ret[repitition_current_position].ellipsis {
                                        GraphNodeEllipsisEnum::None => {
                                            ret[repitition_current_position].ellipsis =
                                                GraphNodeEllipsisEnum::Square(vec![
                                                    repitition_current_position,
                                                ]);
                                        }
                                        GraphNodeEllipsisEnum::Square(items) => {
                                            items.push(repitition_current_position);
                                        }
                                        _ => {
                                            // never
                                            return Err("[...] clashing in builder".to_string());
                                        }
                                    }
                                }
                            }
                            RepititionTokenVariant::Curly => {
                                if last_node_in_loop {
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
                                    match &mut ret[repitition_current_position].ellipsis {
                                        GraphNodeEllipsisEnum::None => {
                                            ret[repitition_current_position].ellipsis =
                                                GraphNodeEllipsisEnum::Curly(vec![
                                                    repitition_current_position,
                                                ]);
                                        }
                                        GraphNodeEllipsisEnum::Curly(items) => {
                                            items.push(repitition_current_position);
                                        }
                                        _ => {
                                            // never
                                            return Err("{...} clashing in builder".to_string());
                                        }
                                    }
                                }
                            }
                            RepititionTokenVariant::ScopeBlocking => {
                                if last_node_in_loop {
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
                                if last_node_in_loop {
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
                            RepititionTokenVariant::SetStart => {
                                set_start = true;
                            }
                            RepititionTokenVariant::NonCapturingCapture => {
                                let out_dst = if last_node_in_loop {
                                    loop_back_index.unwrap()
                                } else {
                                    let r = ret.len();
                                    ret.push(Default::default());
                                    r
                                };

                                ret[repitition_current_position].non_capture.push(
                                    SetStartEnum::from(out_dst, std::mem::take(&mut set_start)),
                                );
                                repitition_current_position = out_dst;
                            }
                            RepititionTokenVariant::CreateCapture => {
                                let out_dst = if last_node_in_loop {
                                    loop_back_index.unwrap()
                                } else {
                                    let r = ret.len();
                                    ret.push(Default::default());
                                    r
                                };

                                ret[repitition_current_position].create_capture.push(
                                    SetStartEnum::from(out_dst, std::mem::take(&mut set_start)),
                                );
                                repitition_current_position = out_dst;
                            }
                        }
                    }

                    if set_start {
                        return Err("..^ isn't effective in this position".to_string());
                    }

                    if is_loop {
                        // transition from the loop node
                        let new_transitions = new_transitions_only.pop().unwrap();
                        // transition from the loop node
                        Self::copy_transitions(
                            &new_transitions,
                            &mut ret[repitition_current_position],
                        )?;
                        // put the transitions back that were taken before
                        Self::copy_transitions(&new_transitions, &mut ret[ret_current_position])?;
                    }
                }
            }
        }

        Ok(())
    }
}

#[derive(
    Debug,
    Clone,
    Default,
    serde::Serialize,
    serde::Deserialize,
    bincode::Encode,
    bincode::Decode,
    PartialEq,
    Eq,
)]
pub enum GraphNodeEllipsisEnum {
    #[default]
    None,
    UncontainedAndCorner(Vec<usize>, Vec<usize>),
    Round(Vec<usize>),
    Square(Vec<usize>),
    Curly(Vec<usize>),
}

impl GraphNodeEllipsisEnum {
    pub fn is_none(&self) -> bool {
        matches!(self, GraphNodeEllipsisEnum::None)
    }
}

#[derive(
    Debug, Default, Clone, serde::Serialize, bincode::Encode, bincode::Decode, PartialEq, Eq,
)]
pub struct GraphNode {
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    pub edge: HashMap<GraphTokenVariant, Vec<SetStartEnum>>,
    #[serde(skip_serializing_if = "GraphNodeEllipsisEnum::is_none")]
    pub ellipsis: GraphNodeEllipsisEnum,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub scope_blocking: Vec<usize>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub statement_blocking: Vec<usize>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub create_capture: Vec<SetStartEnum>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub non_capture: Vec<SetStartEnum>,
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
