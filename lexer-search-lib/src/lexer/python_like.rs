use std::num::NonZero;

use super::c_like::{QuoteType, StringEscaped};

use crate::lexer::{
    LexerToken, LexerTokenVariant, MaybeSliceRef, Position, c_like::CaptureType, calc_start_offset,
};

use super::utf8_multibyte_part;

/// lexer for a python-like language. removes comments, identifies identifiers,
/// strings and numbers, and removes unnecessary whitespace
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

    /// if inside round brackets, no lexical level changes can be emitted.
    ///
    /// 0 for unlocked
    round_bracket_lock: usize,

    /// used to check for change in lexical level
    ///
    /// configurable max length via max_indentation_levels
    ///
    /// it's a stack, from lowest to highest previous levels
    previous_indentations: Vec<usize>,
    max_indentation_levels: NonZero<usize>,
    pattern_enabled: bool,
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

impl Lexer {
    /// err only when buffer length not greater than max token length
    ///
    /// indicate if the pattern is being parsed (enable ellipsis, etc)
    ///
    /// max_indentation_levels: number of indentation levels to keep track of
    /// (for calculating lexical level change when indentation matches previous
    /// level)
    pub fn new(
        max_token_length: NonZero<usize>,
        buffer_length: usize,
        max_indentation_levels: NonZero<usize>,
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
            // don't emit a LexicalLevelChange at the start of the file (unless
            // it has extra empty lines)
            state: LexerEnum::NotLineStart,
            buffer: vec![0u8; buffer_length].into_boxed_slice(),
            buffer_start_offset: 0,
            buffer_end_offset: 0,
            round_bracket_lock: 0,
            previous_indentations: Default::default(),
            max_indentation_levels,
            pattern_enabled,
        })
    }

    fn back_one_byte(&mut self) {
        self.buffer_start_offset -= 1;
        self.col = self.previous_col;
        self.line = self.previous_line
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct LexerEnumIndentation {
    // indentation amount, in spaces
    amount: usize,
    // number of characters used to create this indentation amount
    len: usize,
}

/// lexer for python-like language. python-like means comments are # and '''
/// identifiers and numbers follow the typical naming pattern, literal strings
/// and characters exist, and whitespace is ignored except for indentation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LexerEnum {
    /// leading indentation for a line. an empty line will contain 0
    Indentation(LexerEnumIndentation),

    /// indicates that it's not at the start of the line but no other state
    /// applies here (blank)
    NotLineStart,

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
    /// 'stuff \nhere
    ///
    /// length of token includes quotes
    String(usize, QuoteType, StringEscaped),
    /// '
    SingleQuote,
    /// "
    DoubleQuote,
    /// ''
    DoubleSingleQuote,
    /// ""
    DoubleDoubleQuote,
    /// ''' stuff here
    SingleQuoteBlockString(usize),
    /// """ stuff here
    DoubleQuoteBlockString(usize),
    /// ''' stuff here '
    SingleQuoteBlockCommentOneOut(usize),
    /// """ stuff here "
    DoubleQuoteBlockCommentOneOut(usize),
    /// ''' stuff here ''
    SingleQuoteBlockCommentTwoOut(usize),
    /// """ stuff here ""
    DoubleQuoteBlockCommentTwoOut(usize),

    /// currently traversing a comment
    Comment,

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

impl LexerEnum {
    fn line_start() -> Self {
        LexerEnum::Indentation(LexerEnumIndentation { amount: 0, len: 0 })
    }
}

impl super::Lexer for Lexer {
    fn pythonic_scopes(&self) -> bool {
        true
    }

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
                    LexerEnum::NotLineStart => {
                        self.col_at_token_start = self.previous_col;
                        self.line_at_token_start = self.previous_line;
                        if byte.is_ascii_alphabetic() || byte == b'_' || utf8_multibyte_part(byte) {
                            self.state = LexerEnum::Identifier(1);
                        } else if byte.is_ascii_digit() {
                            self.state = LexerEnum::Number(1);
                        } else if byte == b'\n' {
                            // indentation should start after the newline
                            self.col_at_token_start = self.col;
                            self.line_at_token_start = self.line;
                            self.state = LexerEnum::line_start();
                        } else if byte.is_ascii_whitespace() || byte.is_ascii_control() {
                            // ignore
                        } else if byte == b'#' && !self.pattern_enabled {
                            self.state = LexerEnum::Comment;
                        } else if byte == b'\"' {
                            self.state = LexerEnum::DoubleQuote;
                        } else if byte == b'\'' {
                            self.state = LexerEnum::SingleQuote;
                        } else if byte == b'.' && self.pattern_enabled {
                            self.state = LexerEnum::EllipsisDot;
                        } else if byte == b'$' && self.pattern_enabled {
                            self.state = LexerEnum::Capture((1, CaptureType::Dollar));
                        } else if byte == b'&' && self.pattern_enabled {
                            self.state = LexerEnum::Capture((1, CaptureType::Ampersand));
                        } else if byte == b'#' && self.pattern_enabled {
                            self.state = LexerEnum::Capture((1, CaptureType::Number));
                        } else {
                            if byte == b'(' {
                                self.round_bracket_lock += 1;
                            } else if byte == b')' && self.round_bracket_lock > 0 {
                                self.round_bracket_lock -= 1;
                            }
                            return Ok(Some(ret_token(self, LexerTokenVariant::Byte(byte))));
                        }
                    }
                    LexerEnum::Indentation(val) => {
                        if self.round_bracket_lock != 0 {
                            self.state = LexerEnum::NotLineStart;
                            self.back_one_byte();
                            continue;
                        }
                        if byte == b'\t' {
                            // handle mixed indentation
                            self.state = LexerEnum::Indentation(LexerEnumIndentation {
                                amount: (val.amount / 8 + 1) * 8,
                                len: val.len + 1,
                            })
                        } else if byte == b' ' {
                            self.state = LexerEnum::Indentation(LexerEnumIndentation {
                                amount: val.amount + 1,
                                len: val.len + 1,
                            });
                        } else {
                            // end of leading indentation on a line
                            self.state = LexerEnum::NotLineStart;
                            self.back_one_byte();

                            // check this indentation level and compare it to
                            // the previous indentation levels. it either aligns
                            // with a previous level, or creates a new level
                            let mut level_change = 0;
                            loop {
                                match self.previous_indentations.last() {
                                    Some(previous_indentation) => {
                                        if *previous_indentation == val.amount {
                                            // no change in indentation from previous
                                            break;
                                        } else if *previous_indentation < val.amount {
                                            // a new indentation level has been created
                                            if self.previous_indentations.len()
                                                >= self.max_indentation_levels.get()
                                            {
                                                self.previous_indentations.remove(0);
                                            }
                                            self.previous_indentations.push(val.amount);
                                            level_change += 1;
                                            break;
                                        } else {
                                            // lowered back to a previous indentation
                                            self.previous_indentations.pop();
                                            level_change -= 1;
                                        }
                                    }
                                    None => {
                                        // no previous indentation levels have
                                        // been saved and some (maybe zero)
                                        // leading indentation on a line has
                                        // been found
                                        self.previous_indentations.push(val.amount);
                                        if val.amount > 0 {
                                            level_change += 1;
                                        }
                                        break;
                                    }
                                }
                            }
                            if val.len <= self.max_token_length.get() {
                                return Ok(Some(ret_token(
                                    self,
                                    LexerTokenVariant::LexicalLevelChange(
                                        level_change,
                                        MaybeSliceRef::Some(
                                            &self.buffer[self.buffer_start_offset - val.len
                                                ..self.buffer_start_offset],
                                        ),
                                    ),
                                )));
                            } else {
                                return Ok(Some(ret_token(
                                    self,
                                    LexerTokenVariant::LexicalLevelChange(
                                        level_change,
                                        MaybeSliceRef::Len(val.len),
                                    ),
                                )));
                            }
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
                            self.state = LexerEnum::NotLineStart;
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
                            self.state = LexerEnum::NotLineStart;
                            self.back_one_byte();
                            if len == 1 {
                                match capture_type {
                                    CaptureType::Number => {
                                        //   V
                                        // #?

                                        // ok to type # comment here, but leading space is required
                                        // #NUM is for captures
                                        self.state = LexerEnum::Comment;
                                        continue;
                                    }
                                    CaptureType::Ampersand => {
                                        return Ok(Some(ret_token(
                                            self,
                                            LexerTokenVariant::Byte(b'&'),
                                        )));
                                    }
                                    CaptureType::Dollar => {
                                        return Ok(Some(ret_token(
                                            self,
                                            LexerTokenVariant::Byte(b'$'),
                                        )));
                                    }
                                }
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
                            self.state = LexerEnum::NotLineStart;
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
                    LexerEnum::SingleQuote => {
                        if byte == b'\'' {
                            self.state = LexerEnum::DoubleSingleQuote;
                        } else {
                            self.back_one_byte();
                            self.state =
                                LexerEnum::String(1, QuoteType::SingleQuote, StringEscaped::Normal);
                        }
                    }
                    LexerEnum::DoubleQuote => {
                        if byte == b'"' {
                            self.state = LexerEnum::DoubleDoubleQuote;
                        } else {
                            self.back_one_byte();
                            self.state =
                                LexerEnum::String(1, QuoteType::DoubleQuote, StringEscaped::Normal);
                        }
                    }
                    LexerEnum::String(mut len, string_type, string_escaped) => {
                        len += 1;
                        if string_escaped == StringEscaped::Normal
                            && ((byte == b'\'' && string_type == QuoteType::SingleQuote)
                                || (byte == b'"' && string_type == QuoteType::DoubleQuote))
                        {
                            // end of  string
                            self.state = LexerEnum::NotLineStart;
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
                            self.state = LexerEnum::String(len, string_type, next_escaped);
                        }
                    }
                    LexerEnum::DoubleSingleQuote | LexerEnum::DoubleDoubleQuote => {
                        //    V
                        // '''
                        if byte == b'\'' && self.state == LexerEnum::DoubleSingleQuote {
                            self.state = LexerEnum::SingleQuoteBlockString(3);
                        } else if byte == b'"' && self.state == LexerEnum::DoubleDoubleQuote {
                            self.state = LexerEnum::DoubleQuoteBlockString(3);
                        } else {
                            self.state = LexerEnum::NotLineStart;
                            self.back_one_byte();
                            if self.max_token_length.get() >= 2 {
                                let start = self.buffer_start_offset - 2;
                                let end = self.buffer_start_offset;
                                return Ok(Some(ret_token(
                                    self,
                                    LexerTokenVariant::String(
                                        MaybeSliceRef::Some(&self.buffer[start..end]),
                                        1,
                                    ),
                                )));
                            } else {
                                return Ok(Some(ret_token(
                                    self,
                                    LexerTokenVariant::String(MaybeSliceRef::Len(2), 1),
                                )));
                            }
                        }
                    }
                    LexerEnum::SingleQuoteBlockString(len) => {
                        if byte == b'\'' {
                            self.state = LexerEnum::SingleQuoteBlockCommentOneOut(len + 1);
                        } else {
                            self.state = LexerEnum::SingleQuoteBlockString(len + 1);
                        }
                    }
                    LexerEnum::DoubleQuoteBlockString(len) => {
                        if byte == b'"' {
                            self.state = LexerEnum::DoubleQuoteBlockCommentOneOut(len + 1);
                        } else {
                            self.state = LexerEnum::DoubleQuoteBlockString(len + 1);
                        }
                    }
                    LexerEnum::SingleQuoteBlockCommentOneOut(len) => {
                        if byte == b'\'' {
                            self.state = LexerEnum::SingleQuoteBlockCommentTwoOut(len + 1);
                        } else {
                            self.state = LexerEnum::SingleQuoteBlockString(len + 1);
                        }
                    }
                    LexerEnum::DoubleQuoteBlockCommentOneOut(len) => {
                        if byte == b'"' {
                            self.state = LexerEnum::DoubleQuoteBlockCommentTwoOut(len + 1);
                        } else {
                            self.state = LexerEnum::DoubleQuoteBlockString(len + 1);
                        }
                    }
                    LexerEnum::SingleQuoteBlockCommentTwoOut(mut len) => {
                        len += 1;
                        if byte == b'\'' {
                            self.state = LexerEnum::NotLineStart;
                            if len <= self.max_token_length.get() {
                                let end_position = self.buffer_start_offset;
                                let start_position = end_position - len;
                                return Ok(Some(ret_token(
                                    self,
                                    LexerTokenVariant::String(
                                        MaybeSliceRef::Some(
                                            &self.buffer[start_position..end_position],
                                        ),
                                        3,
                                    ),
                                )));
                            } else {
                                return Ok(Some(ret_token(
                                    self,
                                    LexerTokenVariant::String(MaybeSliceRef::Len(len), 3),
                                )));
                            }
                        } else {
                            self.state = LexerEnum::SingleQuoteBlockString(len);
                        }
                    }
                    LexerEnum::DoubleQuoteBlockCommentTwoOut(mut len) => {
                        len += 1;
                        if byte == b'"' {
                            self.state = LexerEnum::NotLineStart;
                            if len <= self.max_token_length.get() {
                                let end_position = self.buffer_start_offset;
                                let start_position = end_position - len;
                                return Ok(Some(ret_token(
                                    self,
                                    LexerTokenVariant::String(
                                        MaybeSliceRef::Some(
                                            &self.buffer[start_position..end_position],
                                        ),
                                        3,
                                    ),
                                )));
                            } else {
                                return Ok(Some(ret_token(
                                    self,
                                    LexerTokenVariant::String(MaybeSliceRef::Len(len), 3),
                                )));
                            }
                        } else {
                            self.state = LexerEnum::DoubleQuoteBlockString(len);
                        }
                    }
                    LexerEnum::Comment => {
                        if byte == b'\n' {
                            self.state = LexerEnum::line_start();
                        }
                    }
                    LexerEnum::EllipsisDot => {
                        if byte == b'.' {
                            self.state = LexerEnum::EllipsisDotDot;
                        } else {
                            self.back_one_byte();
                            self.state = LexerEnum::NotLineStart;
                            return Ok(Some(ret_token(self, LexerTokenVariant::Byte(b'.'))));
                        }
                    }
                    LexerEnum::EllipsisDotDot => {
                        if byte == b'.' {
                            self.state = LexerEnum::NotLineStart;
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
                        self.state = LexerEnum::NotLineStart;
                        self.back_one_byte();
                        let mut out = ret_token(self, LexerTokenVariant::Byte(b'.'));
                        out.start.column += 1;
                        return Ok(Some(out));
                    }
                }
            }

            // make room for the new bytes to be read
            let num_bytes_to_remove = self.buffer_start_offset
                - match self.state {
                    LexerEnum::Indentation(LexerEnumIndentation { amount: _, len })
                    | LexerEnum::Identifier(len)
                    | LexerEnum::Capture((len, _))
                    | LexerEnum::Number(len)
                    | LexerEnum::String(len, ..)
                    | LexerEnum::SingleQuoteBlockString(len)
                    | LexerEnum::DoubleQuoteBlockString(len)
                    | LexerEnum::SingleQuoteBlockCommentOneOut(len)
                    | LexerEnum::DoubleQuoteBlockCommentOneOut(len)
                    | LexerEnum::SingleQuoteBlockCommentTwoOut(len)
                    | LexerEnum::DoubleQuoteBlockCommentTwoOut(len) => {
                        if len <= self.max_token_length.get() {
                            len
                        } else {
                            0 // token too long => don't retain anything
                        }
                    }
                    LexerEnum::SingleQuote | LexerEnum::DoubleQuote => 1,
                    LexerEnum::DoubleSingleQuote | LexerEnum::DoubleDoubleQuote => {
                        if 2 <= self.max_token_length.get() {
                            2
                        } else {
                            0
                        }
                    }
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
            LexerEnum::DoubleDoubleQuote | LexerEnum::DoubleSingleQuote => {
                if self.max_token_length.get() >= 2 {
                    let start = self.buffer_start_offset - 2;
                    let end = self.buffer_start_offset;
                    Some(LexerTokenVariant::String(
                        MaybeSliceRef::Some(&self.buffer[start..end]),
                        1,
                    ))
                } else {
                    Some(LexerTokenVariant::String(MaybeSliceRef::Len(2), 1))
                }
            }
            LexerEnum::Capture((len, _)) => {
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
                self.state = LexerEnum::NotLineStart;
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
        self.state = LexerEnum::NotLineStart;
        return ret.map(|r| ret_token(self, r));
    }

    fn configured_for_pattern(&self) -> bool {
        self.pattern_enabled
    }
}

#[cfg(test)]
mod ellipsis_capture_tests {
    use std::io::Cursor;

    use crate::lexer::Lexer;

    use super::*;

    fn lexer_for_test() -> super::Lexer {
        super::Lexer::new(
            NonZero::new(99).unwrap(),
            100,
            NonZero::new(100).unwrap(),
            true, // pattern_enabled
        )
        .unwrap()
    }

    #[test]
    fn indentation_tester() {
        let s = b"\n literal";
        let mut c = Cursor::new(s);
        let mut lexer = lexer_for_test();
        assert_eq!(
            lexer.next(&mut c).unwrap(),
            Some(LexerToken {
                variant: LexerTokenVariant::LexicalLevelChange(1, MaybeSliceRef::Some(&[b' '])),
                start: Position {
                    offset: 1,
                    column: 1,
                    line: 2,
                },
                end: Position {
                    offset: 2,
                    column: 2,
                    line: 2,
                }
            })
        );
        assert_eq!(lexer.next(&mut c).unwrap(), None);
        assert_eq!(
            lexer.drain(),
            Some(LexerToken {
                variant: LexerTokenVariant::Identifier(MaybeSliceRef::Some(&s[2..])),
                start: Position {
                    offset: 2,
                    column: 2,
                    line: 2,
                },
                end: Position {
                    offset: 9,
                    column: 9,
                    line: 2,
                }
            })
        );
    }

    #[test]
    fn ellipsis_single_dot() {
        let s = b". ";
        let mut c = Cursor::new(s);
        let mut lexer = lexer_for_test();

        let token = lexer.next(&mut c).unwrap().unwrap();
        match token.variant {
            LexerTokenVariant::Byte(b'.') => {}
            _ => panic!("expected single dot token"),
        }
        assert_eq!(lexer.next(&mut c).unwrap(), None);
        assert_eq!(lexer.drain(), None);
    }

    #[test]
    fn ellipsis_double_dot() {
        let s = b".. ";
        let mut c = Cursor::new(s);
        let mut lexer = lexer_for_test();

        let first_dot = lexer.next(&mut c).unwrap().unwrap();
        match first_dot.variant {
            LexerTokenVariant::Byte(b'.') => {}
            _ => panic!("expected first dot"),
        }

        let second_dot = lexer.next(&mut c).unwrap().unwrap();
        match second_dot.variant {
            LexerTokenVariant::Byte(b'.') => {} // emit dot for ..?
            _ => panic!("expected second dot"),
        }
        assert_eq!(lexer.next(&mut c).unwrap(), None);
        assert_eq!(lexer.drain(), None);
    }

    #[test]
    fn ellipsis_three_dots() {
        let s = b"... "; // ok also with just "..."
        let mut c = Cursor::new(s);
        let mut lexer = lexer_for_test();

        let token = lexer.next(&mut c).unwrap().unwrap();
        match token.variant {
            LexerTokenVariant::Ellipsis => {}
            _ => panic!("expected ellipsis token"),
        }

        assert_eq!(lexer.next(&mut c).unwrap(), None);
        assert_eq!(lexer.drain(), None);
    }

    #[test]
    fn capture_single() {
        let s = b"$x ";
        let mut c = Cursor::new(s);
        let mut lexer = lexer_for_test();

        let token = lexer.next(&mut c).unwrap().unwrap();
        match token.variant {
            LexerTokenVariant::Capture(slice_ref) => {
                if let MaybeSliceRef::Some(slice) = slice_ref {
                    assert_eq!(slice, b"$x");
                } else {
                    panic!("expected slice ref for capture");
                }
            }
            _ => panic!("expected capture token"),
        }

        assert_eq!(lexer.next(&mut c).unwrap(), None);
        assert_eq!(lexer.drain(), None);
    }

    #[test]
    fn capture_multiple() {
        let s = b"$foo_bar123 ";
        let mut c = Cursor::new(s);
        let mut lexer = lexer_for_test();

        let token = lexer.next(&mut c).unwrap().unwrap();
        match token.variant {
            LexerTokenVariant::Capture(slice_ref) => {
                if let MaybeSliceRef::Some(slice) = slice_ref {
                    assert_eq!(slice, b"$foo_bar123");
                } else {
                    panic!("expected slice ref for capture");
                }
            }
            _ => panic!("expected capture token"),
        }

        assert_eq!(lexer.next(&mut c).unwrap(), None);
        assert_eq!(lexer.drain(), None);
    }

    #[test]
    fn round_bracket_lock() {
        let s = br#"test (
        
     
    )
        "#;
        let mut c = Cursor::new(s);
        let mut lexer = lexer_for_test();

        assert_eq!(
            lexer.next(&mut c).unwrap(),
            Some(LexerToken {
                variant: LexerTokenVariant::Identifier(MaybeSliceRef::Some(&[116, 101, 115, 116])),
                start: Position {
                    offset: 0,
                    column: 1,
                    line: 1,
                },
                end: Position {
                    offset: 4,
                    column: 5,
                    line: 1,
                }
            })
        );
        assert_eq!(
            lexer.next(&mut c).unwrap(),
            Some(LexerToken {
                variant: LexerTokenVariant::Byte(40),
                start: Position {
                    offset: 5,
                    column: 6,
                    line: 1,
                },
                end: Position {
                    offset: 6,
                    column: 7,
                    line: 1,
                }
            })
        );
        assert_eq!(
            lexer.next(&mut c).unwrap(),
            Some(LexerToken {
                variant: LexerTokenVariant::Byte(41),
                start: Position {
                    offset: 26,
                    column: 5,
                    line: 4,
                },
                end: Position {
                    offset: 27,
                    column: 6,
                    line: 4,
                }
            })
        );
        assert_eq!(lexer.next(&mut c).unwrap(), None);
        assert_eq!(lexer.drain(), None);
    }

    #[test]
    fn line_comment() {
        let s = b"# NUM";
        let mut c = Cursor::new(s);
        let mut lexer = lexer_for_test();

        assert_eq!(lexer.next(&mut c).unwrap(), None);
        assert_eq!(lexer.drain(), None);
    }

    #[test]
    fn number_capture() {
        let s = b"#NUM\nabc";
        let mut c = Cursor::new(s);
        let mut lexer = lexer_for_test();

        let tok = lexer.next(&mut c).unwrap().unwrap();
        assert_eq!(tok.start.offset, 0);
        assert_eq!(
            tok.variant,
            LexerTokenVariant::Capture(MaybeSliceRef::Some(&s[0..4]))
        );

        let tok = lexer.next(&mut c).unwrap().unwrap();
        assert_eq!(tok.start.offset, 5);
        assert_eq!(tok.start.line, 2);
        assert_eq!(tok.start.column, 1);
        assert_eq!(tok.end.offset, 5);
        assert_eq!(tok.end.line, 2);
        assert_eq!(tok.end.column, 1);
        assert_eq!(
            tok.variant,
            LexerTokenVariant::LexicalLevelChange(0, MaybeSliceRef::Some(&s[5..5]))
        );

        assert_eq!(lexer.next(&mut c).unwrap(), None);
        let tok = lexer.drain().unwrap();
        assert_eq!(tok.start.offset, 5);
        assert_eq!(
            tok.variant,
            LexerTokenVariant::Identifier(MaybeSliceRef::Some(&s[5..8]))
        );
    }

    #[test]
    fn multiline_string_literal() {
        // Triple-double-quoted multiline string
        let s = b"\"\"\"This is a\nmultiline\nstring\"\"\" rest ";
        let mut c = Cursor::new(s);
        let mut lexer = lexer_for_test();

        // Expect one token for the entire multiline string
        let token = lexer.next(&mut c).unwrap().unwrap();
        match token.variant {
            LexerTokenVariant::String(slice_ref, quote_len) => {
                if let MaybeSliceRef::Some(slice) = slice_ref {
                    assert_eq!(slice, b"\"\"\"This is a\nmultiline\nstring\"\"\"");
                    assert_eq!(quote_len, 3);
                } else {
                    panic!("expected slice ref for multiline string");
                }
            }
            _ => panic!("expected multiline string token"),
        }

        // The next token should be "rest"
        let token = lexer.next(&mut c).unwrap().unwrap();
        match token.variant {
            LexerTokenVariant::Identifier(slice_ref) => {
                if let MaybeSliceRef::Some(slice) = slice_ref {
                    assert_eq!(slice, b"rest");
                } else {
                    panic!("expected slice ref for identifier");
                }
            }
            _ => panic!("expected identifier token"),
        }

        assert_eq!(lexer.next(&mut c).unwrap(), None);
        assert_eq!(lexer.drain(), None);
    }

    #[test]
    fn single_quote_multiline_string() {
        // Triple-single-quoted multiline string
        let s = b"'''Line1\nLine2\nLine3''' ";
        let mut c = Cursor::new(s);
        let mut lexer = lexer_for_test();

        let token = lexer.next(&mut c).unwrap().unwrap();
        match token.variant {
            LexerTokenVariant::String(slice_ref, quote_len) => {
                if let MaybeSliceRef::Some(slice) = slice_ref {
                    assert_eq!(slice, b"'''Line1\nLine2\nLine3'''");
                    assert_eq!(quote_len, 3);
                } else {
                    panic!("expected slice ref for multiline string");
                }
            }
            _ => panic!("expected single-quote multiline string token"),
        }

        assert_eq!(lexer.next(&mut c).unwrap(), None);
        assert_eq!(lexer.drain(), None);
    }
}
