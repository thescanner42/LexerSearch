use std::num::NonZeroUsize;

pub mod c_like;
pub mod python_like;
pub mod rust_like;

pub const DEFAULT_MAX_TOKEN_LENGTH: NonZeroUsize = unsafe { NonZeroUsize::new_unchecked(300) };

/// utf8 multibyte sequences are treated as part of the identifier (even though
/// it could be whitespace). erroneous case never happens in practice. malformed
/// utf8 is not treated specially
pub(super) fn utf8_multibyte_part(i: u8) -> bool {
    i & 0b1000_0000 != 0
}

pub(super) fn calc_start_offset(
    t_len: usize,
    bytes_read: usize,
    buffer_start_offset: usize,
    buffer_end_offset: usize,
) -> usize {
    bytes_read - buffer_end_offset + buffer_start_offset - t_len
}

pub trait Lexer {
    /// as indicated by lifetime, token refers to ephemeral buffer (token must
    /// be dropped before moving to the next token)
    ///
    /// if a token could be extended with more input then it is not returned
    /// until it's known that this is eof. to indicate eof, see drain
    fn next<'state, R: std::io::Read>(
        &'state mut self,
        r: &mut R,
    ) -> Result<Option<LexerToken<'state>>, String>;

    /// indicates stream eof. draining any partial (now complete) tokens
    fn drain<'state>(&'state mut self) -> Option<LexerToken<'state>>;

    fn next_and_drain<'state, R: std::io::Read>(
        &'state mut self,
        r: &mut R,
    ) -> Result<Option<LexerToken<'state>>, String> {
        // Erase borrow tracking
        let self_ptr: *mut Self = self;

        let tok = unsafe {
            // first mutable borrow
            (&mut *self_ptr).next(r)?
        };

        if tok.is_some() {
            return Ok(tok);
        }

        // SAFETY:
        // - next() returned None, so no LexerToken<'state> exists
        // - therefore no outstanding borrow into `self`
        // - we are reusing the *same* exclusive access
        unsafe { Ok((&mut *self_ptr).drain()) }
    }

    /// lexer can be configured for producing tokens in the subject source code
    /// file, or the target pattern file
    ///
    /// the pattern file recognized more tokens: ellipsis or captures
    fn configured_for_pattern(&self) -> bool;

    /// a property of this languages source
    fn pythonic_scopes(&self) -> bool {
        false
    }
}

/// dyn or any-like lexer
pub enum EnumLexer {
    CLike(c_like::Lexer),
    PythonLike(python_like::Lexer),
    RustLike(rust_like::Lexer),
}

impl Lexer for EnumLexer {
    fn next<'state, R: std::io::Read>(
        &'state mut self,
        r: &mut R,
    ) -> Result<Option<LexerToken<'state>>, String> {
        match self {
            EnumLexer::CLike(lexer) => lexer.next(r),
            EnumLexer::PythonLike(lexer) => lexer.next(r),
            EnumLexer::RustLike(lexer) => lexer.next(r),
        }
    }

    fn drain<'state>(&'state mut self) -> Option<LexerToken<'state>> {
        match self {
            EnumLexer::CLike(lexer) => lexer.drain(),
            EnumLexer::PythonLike(lexer) => lexer.drain(),
            EnumLexer::RustLike(lexer) => lexer.drain(),
        }
    }

    fn configured_for_pattern(&self) -> bool {
        match self {
            EnumLexer::CLike(lexer) => lexer.configured_for_pattern(),
            EnumLexer::PythonLike(lexer) => lexer.configured_for_pattern(),
            EnumLexer::RustLike(lexer) => lexer.configured_for_pattern(),
        }
    }

    fn pythonic_scopes(&self) -> bool {
        match self {
            EnumLexer::CLike(lexer) => lexer.pythonic_scopes(),
            EnumLexer::PythonLike(lexer) => lexer.pythonic_scopes(),
            EnumLexer::RustLike(lexer) => lexer.pythonic_scopes(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MaybeSliceRef<'a> {
    Some(&'a [u8]),
    Len(usize),
}

/// reference to token emitted by lexer
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LexerTokenVariant<'a> {
    /// some character. like "*". this has lowest precedence. for example in
    /// c-like languages the '{' character instead is represented with an
    /// increment in lexical level
    Byte(u8),
    Number(MaybeSliceRef<'a>),
    /// indicates maybe range and the length of the quotation at the beginning
    /// and end of the string
    String(MaybeSliceRef<'a>, usize),
    Identifier(MaybeSliceRef<'a>),
    /// contains level change and maybe slice
    ///
    /// lexical level change is meant to be agnostic between languages
    ///
    /// in python-like:
    ///  - lexical level change of 0 occurs at end of statement if no
    ///    indentation change happens on the next line
    ///  - lexical level change matches change in indentation level, comparing
    ///    against previous indentation levels
    ///
    /// in c-like and rust-like:
    ///  - lexical level change of 0 occurs for ';', since it's semantically
    ///    similar to the equivalent in python. this means no Byte(';')
    ///  - lexical level occurs from the curly brackets
    LexicalLevelChange(i32, MaybeSliceRef<'a>),
    /// ...
    ///
    /// only emitted when the lexer is set to process a pattern
    Ellipsis,
    /// $VAR, #VAR or &VAR
    ///
    /// only emitted when the lexer is set to process a pattern
    ///
    /// on first declaration of the capture, it captures a value. on subsequent
    /// mentioned, this instead references the previously created capture
    Capture(MaybeSliceRef<'a>),
}

impl<'a> LexerTokenVariant<'a> {
    pub(super) fn len(&self) -> usize {
        match self {
            LexerTokenVariant::Byte(_) => 1,
            LexerTokenVariant::Number(ms)
            | LexerTokenVariant::String(ms, _)
            | LexerTokenVariant::Identifier(ms) => match ms {
                MaybeSliceRef::Some(buf) => buf.len(),
                MaybeSliceRef::Len(len) => *len,
            },
            LexerTokenVariant::LexicalLevelChange(_, ms) => match ms {
                MaybeSliceRef::Some(buf) => buf.len(),
                MaybeSliceRef::Len(len) => *len,
            },
            LexerTokenVariant::Ellipsis => 3,
            LexerTokenVariant::Capture(ms) => match ms {
                MaybeSliceRef::Some(buf) => buf.len(),
                MaybeSliceRef::Len(len) => *len,
            },
        }
    }
}

#[derive(Default, Debug, PartialEq, Eq, Clone, Copy, serde::Serialize)]
pub struct Position {
    pub offset: usize,
    pub column: usize,
    pub line: usize,
}

#[derive(Debug, PartialEq, Eq)]
pub struct LexerToken<'a> {
    pub variant: LexerTokenVariant<'a>,
    pub start: Position,
    pub end: Position,
}

#[cfg(feature = "fuzzer")]
#[cfg(test)]
mod tests {

    use std::{
        sync::atomic::{AtomicU64, Ordering},
        time::Instant,
    };

    use crate::lexer::Lexer;

    use super::*;
    fn fuzz_lexer<'a, L, F>(make_lexer: F)
    where
        L: Lexer,
        F: Fn() -> L + Sync,
    {
        use rayon::iter::{IntoParallelIterator, ParallelIterator};

        const FUZZ_INPUT_ELEMENT: [u8; 16] = [
            b'a', b'\n', b' ', b'#', b'$', b'&', b'#', b'(', b')', b'\'', b'"', b'.', b'/', b'{',
            b'}', b'*',
        ];

        let total = u32::MAX as u64 + 1;
        let counter = AtomicU64::new(0);
        let start = Instant::now();

        (0u32..=u32::MAX).into_par_iter().for_each(|fuzz_seed| {
            // ---------------- progress ----------------
            let n = counter.fetch_add(1, Ordering::Relaxed) + 1;
            if n % 10_000_000 == 0 {
                let elapsed = start.elapsed().as_secs_f64();
                let pct = (n as f64 / total as f64) * 100.0;
                eprintln!(
                    "[fuzz] {:>6.2}% ({}/{}) elapsed: {:.1}s",
                    pct, n, total, elapsed
                );
            }

            let mut data = [0u8; 8];
            for i in 0..8 {
                data[i] = FUZZ_INPUT_ELEMENT[((fuzz_seed >> (i * 4)) & 0xF) as usize];
            }
            let data = data;

            {
                let mut lexer = make_lexer();
                let pythonic = lexer.pythonic_scopes();
                let mut previous_token_end: Option<Position> = None;
                let mut token_produced_count = 0;

                let mut cursor = std::io::Cursor::new(data);
                while let Some(token) = lexer.next_and_drain(&mut cursor).unwrap() {
                    token_produced_count += 1;
                    if token_produced_count > 100 {
                        panic!("infinite loop detected in lexer for input: {:?}", data);
                    }

                    if pythonic {
                        assert!(token.start.offset <= token.end.offset, "{:?}", data);
                    } else {
                        assert!(token.start.offset < token.end.offset, "{:?}", data);
                    }

                    assert!(token.start.line <= token.end.line, "{:?}", data);
                    if token.start.line == token.end.line {
                        assert!(token.start.column <= token.end.column, "{:?}", data);
                    }

                    if let (Some(prev_end), curr_start) = (previous_token_end, token.start) {
                        assert!(prev_end.offset <= curr_start.offset, "{:?}", data);
                        assert!(prev_end.line <= curr_start.line, "{:?}", data);
                        if prev_end.line == curr_start.line {
                            assert!(prev_end.column <= curr_start.column, "{:?}", data);
                        }
                    }

                    previous_token_end = Some(token.end);
                }
            }
        });
    }

    #[test]
    fn fuzz_rust_like() {
        fuzz_lexer(|| rust_like::Lexer::new(99.try_into().unwrap(), 100, false).unwrap());
        fuzz_lexer(|| rust_like::Lexer::new(99.try_into().unwrap(), 100, true).unwrap());
    }

    #[test]
    fn fuzz_rust_like_small() {
        fuzz_lexer(|| rust_like::Lexer::new(1.try_into().unwrap(), 2, true).unwrap());
    }

    // --------------------------------------------

    #[test]
    fn fuzz_c_like() {
        fuzz_lexer(|| c_like::Lexer::new(99.try_into().unwrap(), 100, false, false).unwrap());
        fuzz_lexer(|| c_like::Lexer::new(99.try_into().unwrap(), 100, true, false).unwrap());
        fuzz_lexer(|| c_like::Lexer::new(99.try_into().unwrap(), 100, false, true).unwrap());
        fuzz_lexer(|| c_like::Lexer::new(99.try_into().unwrap(), 100, true, true).unwrap());
    }

    #[test]
    fn fuzz_c_like_small() {
        fuzz_lexer(|| c_like::Lexer::new(1.try_into().unwrap(), 2, true, true).unwrap());
    }

    // ----------------------------------

    #[test]
    fn fuzz_py_like() {
        fuzz_lexer(|| {
            python_like::Lexer::new(99.try_into().unwrap(), 100, 1000.try_into().unwrap(), true)
                .unwrap()
        });
        fuzz_lexer(|| {
            python_like::Lexer::new(99.try_into().unwrap(), 100, 1000.try_into().unwrap(), false)
                .unwrap()
        });
    }

    #[test]
    fn fuzz_py_like_small() {
        fuzz_lexer(|| {
            python_like::Lexer::new(1.try_into().unwrap(), 2, 1.try_into().unwrap(), true).unwrap()
        });
    }
}
