use std::num::NonZero;

use crate::lexer::{
    LexerToken, LexerTokenVariant, MaybeSliceRef, Position, c_like::CaptureType, calc_start_offset,
};

use super::utf8_multibyte_part;

/// lexer for a rust-like language. similar to the c-like lexer, but
/// differentiates single quote between character literals and lifetimes /
/// labels
pub struct Lexer {
    /// byte offset counting upwards as the reader is read
    bytes_read: usize,

    col: usize,
    line: usize,
    previous_col: usize,
    previous_line: usize,

    col_at_token_start: usize,
    line_at_token_start: usize,

    max_token_length: NonZero<usize>,
    state: LexerEnum,
    /// fixed length buffer with a length >= max_token_length (required or else
    /// unsafe offsets)
    buffer: Box<[u8]>,
    buffer_start_offset: usize,
    buffer_end_offset: usize,
    pattern_enabled: bool,
}

impl Lexer {
    /// err only when buffer length not greater than max token length
    ///
    /// indicate if the pattern is being parsed (enable ellipsis, etc)
    pub fn new(
        max_token_length: NonZero<usize>,
        buffer_length: usize,
        pattern_enabled: bool,
    ) -> Result<Self, String> {
        if buffer_length <= max_token_length.get() {
            // suppose the token fills up the whole buffer. it can't keep onto
            // everything while also inspecting the next token to check if the
            // token so far is complete. so explicitly >, NOT >=
            return Err("buffer length must be > max token length".to_owned());
        }

        Ok(Lexer {
            bytes_read: 0,
            col: 1,
            line: 1,
            previous_col: 1,
            previous_line: 1,
            col_at_token_start: 1,
            line_at_token_start: 1,
            max_token_length,
            state: Default::default(),
            buffer: vec![0u8; buffer_length].into_boxed_slice(),
            buffer_start_offset: 0,
            buffer_end_offset: 0,
            pattern_enabled,
        })
    }

    fn back_one_byte(&mut self) {
        self.buffer_start_offset -= 1;
        self.col = self.previous_col;
        self.line = self.previous_line
    }
}

/// has the escape character '\' been seen previously
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum StringEscaped {
    Normal,
    Escaped,
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
enum LexerEnum {
    #[default]
    Start,
    /// '/' has been been consumed - checking for comment begin
    Slash,
    /// identifiers do not start with a digit
    ///
    /// the n bytes before buffer_start_offset form the part of the token read
    /// so far (buffer_start_offset is the exclusive end of the token range).
    /// however, this only applies if the n bytes does not exceed the maximum
    /// token length. if it is exceeded, then there's no guarantee that any of
    /// the bytes are retained in the buffer
    Identifier(usize),
    /// similar to identifier, but digits only
    Number(usize),
    /// similar to identifier or number, but for strings.
    ///
    /// length of token includes quotes
    DoubleQuoteString(usize, StringEscaped),
    /// one single quote was received so far - unknown if this is a lifetime,
    /// label, or char literal
    SingleQuoteStart,
    /// after the single quote was received, a character which could be the
    /// start of an identifier was received. still unknown if this is
    /// lifetime/label or ident
    SingleQuoteOneChar,
    SingleQuoteString(usize),
    /// currently traversing a comment
    Comment,
    /// currently traversing a long comment
    LongComment,
    /// traversing a long comment and this might be the next since a '*' was consumed
    LongCommentAfterAsterisk,

    // pattern-only states next
    /// .
    EllipsisDot,
    /// ..
    EllipsisDotDot,
    /// used when unwinding something which could have been an ellipsis
    EmitDot,
    /// a capture, like $str #num &ident. double to escape
    Capture((usize, CaptureType)),
}

fn ret_token<'state>(me: &'state Lexer, var: LexerTokenVariant<'state>) -> LexerToken<'state> {
    let start = calc_start_offset(
        var.len(),
        me.bytes_read,
        me.buffer_start_offset,
        me.buffer_end_offset,
    );
    LexerToken {
        start: Position {
            offset: start,
            column: me.col_at_token_start,
            line: me.line_at_token_start,
        },
        end: Position {
            offset: start + var.len(),
            column: me.col,
            line: me.line,
        },
        variant: var,
    }
}

impl super::Lexer for Lexer {
    /// as indicated by lifetime, token refers to ephemeral buffer (token must
    /// be dropped before moving to the next token)
    fn next<'state, R: std::io::Read>(
        &'state mut self,
        r: &mut R,
    ) -> Result<Option<LexerToken<'state>>, String> {
        loop {
            while self.buffer_start_offset != self.buffer_end_offset {
                // work with the buffer that we have already before reading in more content
                let byte = self.buffer[self.buffer_start_offset];
                self.buffer_start_offset += 1;

                self.previous_col = self.col;
                self.previous_line = self.line;
                if byte == b'\n' {
                    self.line += 1;
                    self.col = 1;
                } else {
                    self.col += 1;
                }

                match self.state {
                    LexerEnum::Start => {
                        self.col_at_token_start = self.previous_col;
                        self.line_at_token_start = self.previous_line;
                        if byte.is_ascii_alphabetic() || byte == b'_' || utf8_multibyte_part(byte) {
                            self.state = LexerEnum::Identifier(1);
                        } else if byte.is_ascii_digit() {
                            self.state = LexerEnum::Number(1);
                        } else if byte.is_ascii_whitespace() || byte.is_ascii_control() {
                            // ignore
                        } else if byte == b'/' {
                            self.state = LexerEnum::Slash;
                        } else if byte == b'\"' {
                            self.state = LexerEnum::DoubleQuoteString(1, StringEscaped::Normal);
                        } else if byte == b'\'' {
                            self.state = LexerEnum::SingleQuoteStart;
                        } else if byte == b'{' || byte == b'}' || byte == b';' {
                            let v = match byte {
                                b'{' => 1,
                                b'}' => -1,
                                b';' => 0,
                                _ => unreachable!(),
                            };
                            return Ok(Some(ret_token(
                                self,
                                LexerTokenVariant::LexicalLevelChange(
                                    v,
                                    MaybeSliceRef::Some(
                                        &self.buffer[self.buffer_start_offset - 1
                                            ..self.buffer_start_offset],
                                    ),
                                ),
                            )));
                        } else if byte == b'.' && self.pattern_enabled {
                            self.state = LexerEnum::EllipsisDot;
                        } else if byte == b'$' && self.pattern_enabled {
                            self.state = LexerEnum::Capture((1, CaptureType::Dollar));
                        } else if byte == b'&' && self.pattern_enabled {
                            self.state = LexerEnum::Capture((1, CaptureType::Ampersand));
                        } else if byte == b'#' && self.pattern_enabled {
                            self.state = LexerEnum::Capture((1, CaptureType::Number));
                        } else {
                            return Ok(Some(ret_token(self, LexerTokenVariant::Byte(byte))));
                        }
                    }
                    LexerEnum::SingleQuoteStart => {
                        //   V
                        // '?
                        if byte.is_ascii_alphabetic() || byte == b'_' || utf8_multibyte_part(byte) {
                            self.state = LexerEnum::SingleQuoteOneChar;
                        } else if byte == b'\'' {
                            // '' is not valid syntax; just being thorough
                            self.buffer_start_offset -= 1; // reusing logic below, simplest
                            self.col -= 1;
                            self.state = LexerEnum::SingleQuoteString(1);
                        } else {
                            self.state = LexerEnum::SingleQuoteString(2);
                        }
                    }
                    LexerEnum::SingleQuoteOneChar => {
                        //    V
                        // 'a?
                        if byte == b'\'' {
                            self.buffer_start_offset -= 1; // reusing logic below, simplest
                            self.col -= 1;
                            self.state = LexerEnum::SingleQuoteString(2);
                        } else {
                            self.back_one_byte();
                            self.buffer_start_offset -= 1;
                            self.col -= 1;
                            self.state = Default::default();
                            return Ok(Some(ret_token(self, LexerTokenVariant::Byte(b'\''))));
                        }
                    }
                    LexerEnum::SingleQuoteString(mut len) => {
                        len += 1;
                        if byte == b'\'' {
                            self.state = Default::default();
                            // end of character literal
                            if len <= self.max_token_length.get() {
                                let end_position = self.buffer_start_offset;
                                let start_position = end_position - len;
                                return Ok(Some(ret_token(
                                    self,
                                    LexerTokenVariant::String(
                                        MaybeSliceRef::Some(
                                            &self.buffer[start_position..end_position],
                                        ),
                                        1,
                                    ),
                                )));
                            } else {
                                return Ok(Some(ret_token(
                                    self,
                                    LexerTokenVariant::String(MaybeSliceRef::Len(len), 1),
                                )));
                            }
                        } else {
                            // continuation of character literal
                            self.state = LexerEnum::SingleQuoteString(len);
                        }
                    }
                    LexerEnum::Slash => {
                        if byte == b'/' {
                            self.state = LexerEnum::Comment;
                        } else if byte == b'*' {
                            self.state = LexerEnum::LongComment;
                        } else {
                            self.state = LexerEnum::Start;
                            self.back_one_byte();
                            return Ok(Some(ret_token(self, LexerTokenVariant::Byte(b'/'))));
                        }
                    }
                    LexerEnum::Identifier(len) => {
                        if byte.is_ascii_alphabetic()
                            || byte == b'_'
                            || utf8_multibyte_part(byte)
                            || byte.is_ascii_digit()
                        {
                            self.state = LexerEnum::Identifier(len + 1);
                        } else {
                            self.state = LexerEnum::Start;
                            self.back_one_byte();
                            if len <= self.max_token_length.get() {
                                let end_position = self.buffer_start_offset;
                                let start_position = end_position - len;
                                let maybe_ref =
                                    MaybeSliceRef::Some(&self.buffer[start_position..end_position]);
                                return Ok(Some(ret_token(
                                    self,
                                    LexerTokenVariant::Identifier(maybe_ref),
                                )));
                            } else {
                                return Ok(Some(ret_token(
                                    self,
                                    LexerTokenVariant::Identifier(MaybeSliceRef::Len(len)),
                                )));
                            }
                        }
                    }
                    LexerEnum::Capture((len, capture_type)) => {
                        if byte.is_ascii_alphabetic()
                            || byte == b'_'
                            || utf8_multibyte_part(byte)
                            || byte.is_ascii_digit()
                        {
                            self.state = LexerEnum::Capture((len + 1, capture_type));
                        } else {
                            self.state = LexerEnum::Start;
                            self.back_one_byte();
                            if len == 1 {
                                return Ok(Some(ret_token(
                                    self,
                                    LexerTokenVariant::Byte(match capture_type {
                                        CaptureType::Ampersand => b'&',
                                        CaptureType::Number => b'#',
                                        CaptureType::Dollar => b'$',
                                    }),
                                )));
                            }
                            if len <= self.max_token_length.get() {
                                let end_position = self.buffer_start_offset;
                                let start_position = end_position - len;
                                let maybe_ref =
                                    MaybeSliceRef::Some(&self.buffer[start_position..end_position]);

                                return Ok(Some(ret_token(
                                    self,
                                    LexerTokenVariant::Capture(maybe_ref),
                                )));
                            } else {
                                return Ok(Some(ret_token(
                                    self,
                                    LexerTokenVariant::Capture(MaybeSliceRef::Len(len)),
                                )));
                            }
                        }
                    }
                    LexerEnum::Number(len) => {
                        if byte.is_ascii_digit() || byte == b'_' {
                            self.state = LexerEnum::Number(len + 1);
                        } else {
                            self.state = LexerEnum::Start;
                            self.back_one_byte();
                            if len <= self.max_token_length.get() {
                                let end_position = self.buffer_start_offset;
                                let start_position = end_position - len;
                                return Ok(Some(ret_token(
                                    self,
                                    LexerTokenVariant::Number(MaybeSliceRef::Some(
                                        &self.buffer[start_position..end_position],
                                    )),
                                )));
                            } else {
                                return Ok(Some(ret_token(
                                    self,
                                    LexerTokenVariant::Number(MaybeSliceRef::Len(len)),
                                )));
                            }
                        }
                    }
                    LexerEnum::Comment => {
                        if byte == b'\n' {
                            self.state = LexerEnum::Start;
                        }
                    }
                    LexerEnum::LongComment => {
                        if byte == b'*' {
                            self.state = LexerEnum::LongCommentAfterAsterisk;
                        }
                    }
                    LexerEnum::LongCommentAfterAsterisk => {
                        if byte == b'/' {
                            self.state = LexerEnum::Start;
                        } else {
                            self.state = LexerEnum::LongComment;
                        }
                    }
                    LexerEnum::DoubleQuoteString(mut len, string_escaped) => {
                        len += 1;
                        if string_escaped == StringEscaped::Normal && byte == b'"' {
                            // end of string
                            self.state = LexerEnum::Start;
                            if len <= self.max_token_length.get() {
                                let end_position = self.buffer_start_offset;
                                let start_position = end_position - len;
                                return Ok(Some(ret_token(
                                    self,
                                    LexerTokenVariant::String(
                                        MaybeSliceRef::Some(
                                            &self.buffer[start_position..end_position],
                                        ),
                                        1,
                                    ),
                                )));
                            } else {
                                return Ok(Some(ret_token(
                                    self,
                                    LexerTokenVariant::String(MaybeSliceRef::Len(len), 1),
                                )));
                            }
                        } else {
                            // continuation of string
                            let next_escaped = if byte == b'\\' {
                                match string_escaped {
                                    // toggle escaped
                                    StringEscaped::Normal => StringEscaped::Escaped,
                                    StringEscaped::Escaped => StringEscaped::Normal,
                                }
                            } else {
                                StringEscaped::Normal // reset escaped
                            };
                            self.state = LexerEnum::DoubleQuoteString(len, next_escaped);
                        }
                    }
                    LexerEnum::EllipsisDot => {
                        if byte == b'.' {
                            self.state = LexerEnum::EllipsisDotDot;
                        } else {
                            self.back_one_byte();
                            self.state = LexerEnum::Start;
                            return Ok(Some(ret_token(self, LexerTokenVariant::Byte(b'.'))));
                        }
                    }
                    LexerEnum::EllipsisDotDot => {
                        if byte == b'.' {
                            self.state = LexerEnum::Start;
                            return Ok(Some(ret_token(self, LexerTokenVariant::Ellipsis)));
                        } else {
                            //    V
                            // ..?
                            self.back_one_byte();
                            self.state = LexerEnum::EmitDot;
                            let mut out = ret_token(self, LexerTokenVariant::Byte(b'.'));
                            out.start.offset -= 1;
                            out.end.offset -= 1;
                            out.end.column -= 1;
                            return Ok(Some(out));
                        }
                    }
                    LexerEnum::EmitDot => {
                        self.state = LexerEnum::Start;
                        self.back_one_byte();
                        let mut out = ret_token(self, LexerTokenVariant::Byte(b'.'));
                        out.start.column += 1;
                        return Ok(Some(out));
                    }
                };
            }

            // make room for the new bytes to be read
            let num_bytes_to_remove = self.buffer_start_offset
                - match self.state {
                    LexerEnum::Identifier(len)
                    | LexerEnum::Capture((len, _))
                    | LexerEnum::Number(len)
                    | LexerEnum::SingleQuoteString(len)
                    | LexerEnum::DoubleQuoteString(len, ..) => {
                        if len <= self.max_token_length.get() {
                            len
                        } else {
                            0 // token too long => don't retain anything
                        }
                    }
                    LexerEnum::SingleQuoteStart => 1,
                    LexerEnum::SingleQuoteOneChar => 1,
                    _ => 0,
                };

            self.buffer
                .copy_within(num_bytes_to_remove..self.buffer_end_offset, 0);
            self.buffer_start_offset -= num_bytes_to_remove;
            self.buffer_end_offset -= num_bytes_to_remove;

            // read in the new range
            let len = self.buffer.len();
            let range = &mut self.buffer[self.buffer_end_offset..len];
            debug_assert_ne!(range.len(), 0, "read make progress");
            let bytes_read = r.read(range).map_err(|e| e.to_string())?;
            self.buffer_end_offset += bytes_read;
            self.bytes_read += bytes_read;

            if bytes_read == 0 {
                return Ok(None);
            }
        }
    }

    fn drain<'state>(&'state mut self) -> Option<LexerToken<'state>> {
        let ret: Option<LexerTokenVariant> = match self.state {
            LexerEnum::Slash => Some(LexerTokenVariant::Byte(b'/')),
            LexerEnum::Identifier(len) => {
                if len <= self.max_token_length.get() {
                    let end_position = self.buffer_start_offset;
                    let start_position = end_position - len;
                    Some(LexerTokenVariant::Identifier(MaybeSliceRef::Some(
                        &self.buffer[start_position..end_position],
                    )))
                } else {
                    Some(LexerTokenVariant::Identifier(MaybeSliceRef::Len(len)))
                }
            }
            LexerEnum::Number(len) => {
                if len <= self.max_token_length.get() {
                    let end_position = self.buffer_start_offset;
                    let start_position = end_position - len;
                    Some(LexerTokenVariant::Number(MaybeSliceRef::Some(
                        &self.buffer[start_position..end_position],
                    )))
                } else {
                    Some(LexerTokenVariant::Number(MaybeSliceRef::Len(len)))
                }
            }
            LexerEnum::Capture((len, _capture_type)) => {
                if len <= self.max_token_length.get() {
                    let end_position = self.buffer_start_offset;
                    let start_position = end_position - len;
                    Some(LexerTokenVariant::Capture(MaybeSliceRef::Some(
                        &self.buffer[start_position..end_position],
                    )))
                } else {
                    Some(LexerTokenVariant::Capture(MaybeSliceRef::Len(len)))
                }
            }
            LexerEnum::EllipsisDot => Some(LexerTokenVariant::Byte(b'.')),
            LexerEnum::EmitDot => {
                self.state = LexerEnum::Start;
                let mut out = ret_token(self, LexerTokenVariant::Byte(b'.'));
                out.start.column += 1;
                return Some(out);
            }
            LexerEnum::EllipsisDotDot => {
                //   V
                // ..
                self.state = LexerEnum::EmitDot;
                let mut out = ret_token(self, LexerTokenVariant::Byte(b'.'));
                out.start.offset -= 1;
                out.end.offset -= 1;
                out.end.column -= 1;
                return Some(out);
            }
            _ => None,
        };
        self.state = LexerEnum::Start;
        return ret.map(|r| ret_token(self, r));
    }

    fn configured_for_pattern(&self) -> bool {
        self.pattern_enabled
    }
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use crate::lexer::Lexer;

    use super::*;

    fn lexer_for_test() -> impl Lexer {
        super::Lexer::new(99.try_into().unwrap(), 100, true).unwrap()
    }

    #[test]
    fn a_test() {
        let s = b"'a' 123 ";
        let mut c = Cursor::new(s);
        let mut lexer = lexer_for_test();

        let tok = lexer.next(&mut c).unwrap().unwrap();
        assert_eq!(tok.start.offset, 0);
        assert_eq!(tok.start.line, 1);
        assert_eq!(tok.start.column, 1);
        assert_eq!(
            tok.variant,
            LexerTokenVariant::String(MaybeSliceRef::Some(&s[0..3]), 1)
        );

        let tok = lexer.next(&mut c).unwrap().unwrap();
        assert_eq!(tok.start.offset, 4);
        assert_eq!(tok.start.line, 1);
        assert_eq!(tok.start.column, 5);
        assert_eq!(
            tok.variant,
            LexerTokenVariant::Number(MaybeSliceRef::Some(&s[4..7]))
        );

        assert!(lexer.next(&mut c).unwrap().is_none());
        assert!(lexer.drain().is_none());
    }

    #[test]
    fn empty_quote() {
        let s = b"'' ";
        let mut c = Cursor::new(s);
        let mut lexer = lexer_for_test();

        let tok = lexer.next(&mut c).unwrap().unwrap();
        assert_eq!(tok.start.offset, 0);
        assert_eq!(tok.start.line, 1);
        assert_eq!(tok.start.column, 1);
        assert_eq!(
            tok.variant,
            LexerTokenVariant::String(MaybeSliceRef::Some(&s[0..2]), 1)
        );

        assert!(lexer.next(&mut c).unwrap().is_none());
        assert!(lexer.drain().is_none());
    }

    #[test]
    fn weird_case() {
        let s = b"'a\n123";
        let mut c = Cursor::new(s);
        let mut lexer = lexer_for_test();

        let tok = lexer.next(&mut c).unwrap().unwrap();
        assert_eq!(tok.start.offset, 0);
        assert_eq!(tok.start.line, 1);
        assert_eq!(tok.start.column, 1);
        assert_eq!(tok.variant, LexerTokenVariant::Byte(b'\''));

        let tok = lexer.next(&mut c).unwrap().unwrap();
        assert_eq!(tok.start.offset, 1);
        assert_eq!(tok.start.line, 1);
        assert_eq!(tok.start.column, 2);
        assert_eq!(
            tok.variant,
            LexerTokenVariant::Identifier(MaybeSliceRef::Some(&s[1..2]))
        );
        assert!(lexer.next(&mut c).unwrap().is_none());

        let tok = lexer.drain().unwrap();
        assert_eq!(tok.start.offset, 3);
        assert_eq!(tok.start.line, 2);
        assert_eq!(tok.start.column, 1);
        assert_eq!(
            tok.variant,
            LexerTokenVariant::Number(MaybeSliceRef::Some(&s[3..]))
        );

        assert!(lexer.drain().is_none());
    }

    #[test]
    fn dot_dot() {
        let s = b".. ";
        let mut c = Cursor::new(s);
        let mut lexer = lexer_for_test();

        let tok = lexer.next(&mut c).unwrap().unwrap();
        assert_eq!(tok.start.offset, 0);
        assert_eq!(tok.start.line, 1);
        assert_eq!(tok.start.column, 1);
        assert_eq!(tok.variant, LexerTokenVariant::Byte(b'.'));

        let tok = lexer.next(&mut c).unwrap().unwrap();
        assert_eq!(tok.start.offset, 1, "offset");
        assert_eq!(tok.start.line, 1, "line");
        assert_eq!(tok.start.column, 2, "column");
        assert_eq!(tok.variant, LexerTokenVariant::Byte(b'.'));

        assert!(lexer.next(&mut c).unwrap().is_none());
        assert!(lexer.drain().is_none());
    }

    #[test]
    fn dot_dot_newline() {
        let s = b"..\na ";
        let mut c = Cursor::new(s);
        let mut lexer = lexer_for_test();

        let tok = lexer.next(&mut c).unwrap().unwrap();
        assert_eq!(tok.start.offset, 0);
        assert_eq!(tok.start.line, 1);
        assert_eq!(tok.start.column, 1);
        assert_eq!(tok.variant, LexerTokenVariant::Byte(b'.'));

        let tok = lexer.next(&mut c).unwrap().unwrap();
        assert_eq!(tok.start.offset, 1, "offset");
        assert_eq!(tok.start.line, 1, "line");
        assert_eq!(tok.start.column, 2, "column");
        assert_eq!(tok.variant, LexerTokenVariant::Byte(b'.'));

        let tok = lexer.next(&mut c).unwrap().unwrap();
        assert_eq!(tok.start.offset, 3);
        assert_eq!(tok.start.line, 2);
        assert_eq!(tok.start.column, 1);
        assert_eq!(
            tok.variant,
            LexerTokenVariant::Identifier(MaybeSliceRef::Some(&s[3..4]))
        );

        assert!(lexer.next(&mut c).unwrap().is_none());
        assert!(lexer.drain().is_none());
    }
}
