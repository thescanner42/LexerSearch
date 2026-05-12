use crate::lexer::{EllipsisEnum, LexerToken, LexerTokenVariant, MaybeSliceRef, Position};

use rand::{RngExt, rng};
use std::sync::OnceLock;

/// bounded length owned token
#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum TokenVariant {
    /// stops non-reflexive patterns
    OverlongIdentifier,
    /// stops non-reflexive patterns
    OverlongString,
    Byte(u8),
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

impl TokenVariant {
    pub fn add_double_quotes(&mut self) {
        match self {
            TokenVariant::String(items) => {
                let mut new_items = Vec::with_capacity(items.len() + 2);
                new_items.push(b'"');
                new_items.extend_from_slice(items);
                new_items.push(b'"');
                *items = new_items.into_boxed_slice();
            }
            _ => {}
        }
    }
}

#[derive(Clone)]
pub struct Token {
    pub variant: TokenVariant,
    pub start: Position,
    pub end: Position,
}

impl Token {
    pub fn from_lexer_token(token: LexerToken) -> Self {
        Token {
            variant: match token.variant {
                LexerTokenVariant::Byte(b) => TokenVariant::Byte(b),
                LexerTokenVariant::String(MaybeSliceRef::Len(_), _) => TokenVariant::OverlongString,
                LexerTokenVariant::Identifier(MaybeSliceRef::Len(_)) => {
                    TokenVariant::OverlongIdentifier
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum RepititionTokenVariant {
    Byte(u8),
    Captureable(Box<[u8]>),
    /// see LexerTokenVariant for more details
    LexicalLevelChange(i32),

    /// ellipsis types inside of repitition
    Uncontained,
    Corner,
    Round,
    Square,
    Curly,
    ScopeBlocking,
    StatementBlocking,
    SetStart,

    NonCapturingCapture,
    Backref(usize),
    CreateCapture,
    BackrefReplace(usize),
    CreateReplace(usize),
}

impl RepititionTokenVariant {
    pub fn as_hash_bytes(&self) -> Vec<u8> {
        let mut out = Vec::with_capacity(32);

        match self {
            RepititionTokenVariant::Byte(b) => {
                out.push(0);
                out.push(*b);
            }

            RepititionTokenVariant::Captureable(bytes) => {
                out.push(1);
                out.extend_from_slice(&(bytes.len() as u32).to_le_bytes());
                out.extend_from_slice(bytes);
            }

            RepititionTokenVariant::LexicalLevelChange(d) => {
                out.push(2);
                out.extend_from_slice(&d.to_le_bytes());
            }

            RepititionTokenVariant::Uncontained => out.push(3),
            RepititionTokenVariant::Corner => out.push(4),
            RepititionTokenVariant::Round => out.push(5),
            RepititionTokenVariant::Square => out.push(6),
            RepititionTokenVariant::Curly => out.push(7),
            RepititionTokenVariant::ScopeBlocking => out.push(8),
            RepititionTokenVariant::StatementBlocking => out.push(9),
            RepititionTokenVariant::SetStart => out.push(10),

            RepititionTokenVariant::NonCapturingCapture => {
                out.push(11);
            }
            RepititionTokenVariant::Backref(v) => {
                out.push(12);
                out.extend_from_slice(&(*v as u32).to_le_bytes());
            }
            RepititionTokenVariant::CreateCapture => out.push(13),
            RepititionTokenVariant::BackrefReplace(v) => {
                out.push(14);
                out.extend_from_slice(&(*v as u32).to_le_bytes());
            },
            RepititionTokenVariant::CreateReplace(v) => {
                out.push(15);
                out.extend_from_slice(&(*v as u32).to_le_bytes());
            },
        }

        out
    }
}

static GLOBAL_SEED: OnceLock<[u8; 32]> = OnceLock::new();

fn seed() -> &'static [u8; 32] {
    GLOBAL_SEED.get_or_init(|| {
        let mut b = [0u8; 32];
        rng().fill(&mut b);
        b
    })
}

#[derive(Debug)]
pub struct MultiRepitition {
    incremental_hasher: blake3::Hasher,
    v: Vec<Vec<RepititionTokenVariant>>,
}

impl MultiRepitition {
    pub fn push(&mut self, v: RepititionTokenVariant) {
        self.incremental_hasher.update(&v.as_hash_bytes());

        self.v
            .last_mut()
            .expect("MultiRepitition always contains at least one repetition")
            .push(v);
    }

    pub fn push_repitition(&mut self) {
        self.incremental_hasher.update(b"\xff");
        self.v.push(Vec::new());
    }

    pub fn get_vec(self) -> Vec<Vec<RepititionTokenVariant>> {
        self.v
    }
}

impl Default for MultiRepitition {
    fn default() -> Self {
        Self {
            incremental_hasher: blake3::Hasher::new_keyed(seed()),
            v: vec![Vec::new()],
        }
    }
}

impl PartialEq for MultiRepitition {
    fn eq(&self, other: &Self) -> bool {
        self.v == other.v
    }
}

impl Eq for MultiRepitition {}

impl std::hash::Hash for MultiRepitition {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let digest = self.incremental_hasher.finalize();
        state.write(digest.as_bytes());
    }
}

#[derive(PartialEq, Eq)]
pub enum GraphBuilderBracketType {
    Round,
    Square,
    Curly,
}

#[derive(Default)]
pub struct BracketStack(Vec<GraphBuilderBracketType>);

impl BracketStack {
    pub fn handle(&mut self, b: u8) {
        let maybe_bracket_open = match b {
            b'{' => Some(GraphBuilderBracketType::Curly),
            b'[' => Some(GraphBuilderBracketType::Square),
            b'(' => Some(GraphBuilderBracketType::Round),
            // corner brackets not used here. see cbee
            _ => None, // not a bracket byte from the pattern
        };

        if let Some(bracket_type) = maybe_bracket_open {
            self.0.push(bracket_type);
        }

        let maybe_bracket_close = match b {
            b'}' => Some(GraphBuilderBracketType::Curly),
            b']' => Some(GraphBuilderBracketType::Square),
            b')' => Some(GraphBuilderBracketType::Round),
            _ => None,
        };

        if let Some(bracket_type) = maybe_bracket_close {
            let mut eq = false;
            if let Some(pushed_type) = self.0.last() {
                if bracket_type == *pushed_type {
                    eq = true;
                }
            }

            if eq {
                self.0.pop();
            }
        }
    }

    pub fn last(&self) -> Option<&GraphBuilderBracketType> {
        self.0.last()
    }
}
