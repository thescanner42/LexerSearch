use crate::lexer::{EllipsisEnum, LexerToken, LexerTokenVariant, MaybeSliceRef, Position};

use rand::RngCore;
use smallvec::SmallVec;
use std::{
    hash::{Hash},
    sync::OnceLock,
};

/// bounded length owned token
#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum TokenVariant {
    /// stops non-reflexive patterns
    OverlongIdentifier,
    /// stops non-reflexive patterns
    OverlongString,
    Byte(u8),
    String(SmallVec<[u8; 0x40]>),
    Identifier(SmallVec<[u8; 0x40]>),
    /// see LexerTokenVariant for more details
    LexicalLevelChange(i32),

    /// only used by pattern creation logic, not matching logic
    Ellipsis(EllipsisEnum),
    /// only used by pattern creation logic, not matching logic
    Capture(SmallVec<[u8; 0x40]>),
    /// only used by pattern creation logic, not matching logic
    OverlongCapture,
}

impl TokenVariant {
    pub fn add_double_quotes(&mut self) {
        if let TokenVariant::String(items) = self {
            items.reserve(2);
            items.insert(0, b'"');
            items.push(b'"');
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
                    TokenVariant::String(SmallVec::from_slice(
                        &items[quote_len..items.len() - quote_len],
                    ))
                }

                LexerTokenVariant::Identifier(MaybeSliceRef::Some(items)) => {
                    TokenVariant::Identifier(SmallVec::from_slice(items))
                }

                LexerTokenVariant::LexicalLevelChange(delta, _maybe_items) => {
                    TokenVariant::LexicalLevelChange(delta)
                }

                // not produced by lexer not configured for pattern
                LexerTokenVariant::Ellipsis(b) => TokenVariant::Ellipsis(b),

                LexerTokenVariant::Capture(MaybeSliceRef::Len(_)) => TokenVariant::OverlongCapture,

                LexerTokenVariant::Capture(MaybeSliceRef::Some(items)) => {
                    TokenVariant::Capture(SmallVec::from_slice(items))
                }
            },
            start: token.start,
            end: token.end,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ParrallelPathTokenVariant {
    Byte(u8),
    Captureable(SmallVec<[u8; 0x40]>),
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

    NonCapturingCapture,
    Backref(usize),
    CreateCapture,
    PopReplace(usize),
    CreateReplace(usize),
}

impl ParrallelPathTokenVariant {
    pub fn as_hash_bytes(&self) -> Vec<u8> {
        let mut out = Vec::with_capacity(32);

        match self {
            ParrallelPathTokenVariant::Byte(b) => {
                out.push(0);
                out.push(*b);
            }

            ParrallelPathTokenVariant::Captureable(bytes) => {
                out.push(1);
                out.extend_from_slice(&(bytes.len() as u32).to_le_bytes());
                out.extend_from_slice(bytes);
            }

            ParrallelPathTokenVariant::LexicalLevelChange(d) => {
                out.push(2);
                out.extend_from_slice(&d.to_le_bytes());
            }

            ParrallelPathTokenVariant::Uncontained => out.push(3),
            ParrallelPathTokenVariant::Corner => out.push(4),
            ParrallelPathTokenVariant::Round => out.push(5),
            ParrallelPathTokenVariant::Square => out.push(6),
            ParrallelPathTokenVariant::Curly => out.push(7),
            ParrallelPathTokenVariant::ScopeBlocking => out.push(8),
            ParrallelPathTokenVariant::StatementBlocking => out.push(9),

            ParrallelPathTokenVariant::NonCapturingCapture => {
                out.push(11);
            }
            ParrallelPathTokenVariant::Backref(v) => {
                out.push(12);
                out.extend_from_slice(&(*v as u32).to_le_bytes());
            }
            ParrallelPathTokenVariant::CreateCapture => out.push(13),
            ParrallelPathTokenVariant::PopReplace(v) => {
                out.push(14);
                out.extend_from_slice(&(*v as u32).to_le_bytes());
            }
            ParrallelPathTokenVariant::CreateReplace(v) => {
                out.push(15);
                out.extend_from_slice(&(*v as u32).to_le_bytes());
            }
        }

        out
    }
}

static GLOBAL_SEED: OnceLock<[u8; 32]> = OnceLock::new();

fn seed() -> &'static [u8; 32] {
    GLOBAL_SEED.get_or_init(|| {
        let mut b = [0u8; 32];
        rand::rngs::OsRng.fill_bytes(&mut b);
        b
    })
}

/// used for both multirepitition and alternation
#[derive(Debug)]
pub struct ParallelPath {
    incremental_hasher: blake3::Hasher,
    v: Vec<Vec<ParrallelPathTokenVariant>>,
}

impl ParallelPath {
    pub fn push(&mut self, v: ParrallelPathTokenVariant) {
        self.incremental_hasher.update(&v.as_hash_bytes());

        self.v
            .last_mut()
            .expect("ParallelPath always contains at least one repetition")
            .push(v);
    }

    pub fn push_branch(&mut self) {
        self.incremental_hasher.update(b"\xff");
        self.v.push(Vec::new());
    }

    pub fn vec_ref(&self) -> &Vec<Vec<ParrallelPathTokenVariant>> {
        &self.v
        }

    pub fn get_vec(self) -> Vec<Vec<ParrallelPathTokenVariant>> {
        self.v
    }
}

impl Default for ParallelPath {
    fn default() -> Self {
        Self {
            incremental_hasher: blake3::Hasher::new_keyed(seed()),
            v: vec![Vec::new()],
        }
    }
}

impl PartialEq for ParallelPath {
    fn eq(&self, other: &Self) -> bool {
        self.v == other.v
    }
}

impl Eq for ParallelPath {}

impl std::hash::Hash for ParallelPath {
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
