use std::num::NonZero;

use crate::{
    engine::matcher::{Token, TokenVariant},
    lexer::Position,
};

/// the canonicalizer sits after the lexer but before the matcher. it does basic
/// forms of canonicalization / parsing, and in a bounded way
///
/// in the future, this can be expanded to do more complex (yet still bounded)
/// parsing
pub struct Canonicalizer {
    /// emit this token next without consulting lexer of state
    emit: Vec<Token>,
    state: CanonicalizerEnum,
    max_token_length: NonZero<usize>,
}

#[derive(Default)]
enum CanonicalizerEnum {
    #[default]
    Start,
    /// "123", start, end, respectively
    String(Vec<u8>, Position, Position),
    /// "123" +
    StringOp(Vec<u8>, Position, Position, u8, Position, Position),
    /// continue to consume overlong string token
    OverlongString(Position, Position),
    // more forms may be added here in the future
}

impl Canonicalizer {
    pub fn new(max_token_length: NonZero<usize>) -> Self {
        Self {
            emit: Default::default(),
            state: Default::default(),
            max_token_length,
        }
    }

    pub fn next_and_drain<F: FnMut() -> Result<Option<Token>, String>>(
        &mut self,
        source: F,
    ) -> Result<Option<Token>, String> {
        let tok = self.next(source)?;

        if tok.is_some() {
            return Ok(tok);
        }

        Ok(self.drain())
    }

    pub fn next<F: FnMut() -> Result<Option<Token>, String>>(
        &mut self,
        mut source: F,
    ) -> Result<Option<Token>, String> {
        // current functionality only squashes immediately adjacent string
        // literals

        // additional follow up, basic arithmetic recognition:
        // = 123 + 456 LEXICAL_LEVEL_CHANGE(0)
        // translate to:
        // = 579 LEXICAL_LEVEL_CHANGE(0)

        // additional follow up: a least recently used cache which recognizes
        // identifier assignment and then replaces the use of that same literal
        // with the literal equivalent. probably requires unbounded memory +
        // parsing (no go)

        if let Some(t) = self.emit.pop() {
            return Ok(Some(t));
        }
        loop {
            match (source)()? {
                Some(t) => {
                    let state = std::mem::take(&mut self.state);
                    match (state, &t.variant) {
                        (CanonicalizerEnum::Start, TokenVariant::String(b)) => {
                            self.state = CanonicalizerEnum::String(b.to_vec(), t.start, t.end);
                        }
                        (CanonicalizerEnum::Start, TokenVariant::OverlongString) => {
                            self.state = CanonicalizerEnum::OverlongString(t.start, t.end);
                        }
                        (CanonicalizerEnum::Start, _) => {
                            return Ok(Some(t));
                        }
                        (
                            CanonicalizerEnum::String(mut lhs, lhs_start, _),
                            TokenVariant::String(rhs),
                        ) => {
                            if lhs.len() + rhs.len() > self.max_token_length.get() {
                                self.state = CanonicalizerEnum::OverlongString(lhs_start, t.end);
                            } else {
                                lhs.extend_from_slice(&rhs);
                                self.state = CanonicalizerEnum::String(lhs, lhs_start, t.end);
                            }
                        }
                        (
                            CanonicalizerEnum::String(_, lhs_start, _),
                            TokenVariant::OverlongString,
                        ) => {
                            self.state = CanonicalizerEnum::OverlongString(lhs_start, t.end);
                        }
                        (
                            CanonicalizerEnum::String(lhs, lhs_start, lhs_end),
                            TokenVariant::Byte(b'+'),
                        ) => {
                            self.state = CanonicalizerEnum::StringOp(
                                lhs, lhs_start, lhs_end, b'+', t.start, t.end,
                            );
                        }
                        (
                            CanonicalizerEnum::String(lhs, lhs_start, lhs_end),
                            TokenVariant::Byte(b'\\'),
                        ) => {
                            self.state = CanonicalizerEnum::StringOp(
                                lhs, lhs_start, lhs_end, b'\\', t.start, t.end,
                            );
                        }
                        (CanonicalizerEnum::String(lhs, lhs_start, lhs_end), _) => {
                            let out = Token {
                                variant: TokenVariant::String(lhs.into_boxed_slice()),
                                start: lhs_start,
                                end: lhs_end,
                            };
                            self.emit.push(t);
                            return Ok(Some(out));
                        }

                        (
                            CanonicalizerEnum::StringOp(mut lhs, lhs_start, _, _, _, _),
                            TokenVariant::String(rhs),
                        ) => {
                            if lhs.len() + rhs.len() > self.max_token_length.get() {
                                self.state = CanonicalizerEnum::OverlongString(lhs_start, t.end);
                            } else {
                                lhs.extend_from_slice(&rhs);
                                self.state = CanonicalizerEnum::String(lhs, lhs_start, t.end);
                            }
                        }
                        (
                            CanonicalizerEnum::StringOp(_, lhs_start, _, _, _, _),
                            TokenVariant::OverlongString,
                        ) => {
                            self.state = CanonicalizerEnum::OverlongString(lhs_start, t.end);
                        }
                        (
                            CanonicalizerEnum::StringOp(
                                lhs,
                                lhs_start,
                                lhs_end,
                                op,
                                op_start,
                                op_end,
                            ),
                            _,
                        ) => {
                            self.emit.push(t);
                            self.emit.push(Token {
                                variant: TokenVariant::Byte(op),
                                start: op_start,
                                end: op_end,
                            });
                            return Ok(Some(Token {
                                variant: TokenVariant::String(lhs.into_boxed_slice()),
                                start: lhs_start,
                                end: lhs_end,
                            }));
                        }

                        (
                            CanonicalizerEnum::OverlongString(lhs_start, _),
                            TokenVariant::String(_),
                        ) => {
                            self.state = CanonicalizerEnum::OverlongString(lhs_start, t.end);
                        }

                        (
                            CanonicalizerEnum::OverlongString(lhs_start, _),
                            TokenVariant::OverlongString,
                        ) => {
                            self.state = CanonicalizerEnum::OverlongString(lhs_start, t.end);
                        }
                        (CanonicalizerEnum::OverlongString(lhs_start, lhs_end), _) => {
                            let out = Token {
                                variant: TokenVariant::OverlongString,
                                start: lhs_start,
                                end: lhs_end,
                            };
                            self.emit.push(t);
                            return Ok(Some(out));
                        }
                    }
                }
                None => {
                    return Ok(None);
                }
            }
        }
    }

    pub fn drain(&mut self) -> Option<Token> {
        if let Some(t) = self.emit.pop() {
            return Some(t);
        }

        let state = std::mem::take(&mut self.state);
        match state {
            CanonicalizerEnum::Start => None,
            CanonicalizerEnum::String(lhs, lhs_start, lhs_end) => {
                let out = Token {
                    variant: TokenVariant::String(lhs.into_boxed_slice()),
                    start: lhs_start,
                    end: lhs_end,
                };
                Some(out)
            }
            CanonicalizerEnum::OverlongString(lhs_start, lhs_end) => {
                let out = Token {
                    variant: TokenVariant::OverlongString,
                    start: lhs_start,
                    end: lhs_end,
                };
                Some(out)
            }
            CanonicalizerEnum::StringOp(lhs, lhs_start, lhs_end, op, op_start, op_end) => {
                self.emit.push(Token {
                    variant: TokenVariant::Byte(op),
                    start: op_start,
                    end: op_end,
                });
                Some(Token {
                    variant: TokenVariant::String(lhs.into_boxed_slice()),
                    start: lhs_start,
                    end: lhs_end,
                })
            }
        }
    }
}
