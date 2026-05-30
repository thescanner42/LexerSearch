use std::num::NonZero;

use smallvec::SmallVec;

use crate::{
    engine::token::{Token, TokenVariant},
    lexer::Position,
};

/// the canonicalizer sits after the lexer but before the matcher. it does basic
/// forms of canonicalization / parsing, and in a bounded way
///
/// in the future, this can be expanded to do more complex (yet still bounded)
/// parsing
pub struct Canonicalizer {
    /// push-down buffer for at most 2 tokens to re-emit (op + lookahead).
    /// stored in reverse order so index 0 is the next token to pop.
    emit: [Option<Token>; 2],
    state: CanonicalizerState,
    max_token_length: usize,
}

/// Flat, allocation-free representation of canonicalizer state.
struct CanonicalizerState {
    /// The accumulated string bytes.  Only meaningful in `String` and
    /// `StringOp` modes.
    buf: SmallVec<[u8; 0x40]>,
    /// Start position of the current accumulation (all modes except `Start`).
    start: Position,
    /// End position of the most recently consumed token in the current run.
    end: Position,
    /// If we are in `StringOp` mode: the pending operator byte and its span.
    op: Option<(u8, Position, Position)>,
    /// Current mode.
    mode: Mode,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Mode {
    Start,
    String,
    StringOp,
    OverlongString,
}

// A zeroed Position is used as a placeholder; it is always overwritten before
// it is read in any non-Start mode.
const ZERO_POS: Position = Position { line: 0, column: 0, offset: 0 };

impl Default for CanonicalizerState {
    #[inline]
    fn default() -> Self {
        Self {
            buf: SmallVec::new(),
            start: ZERO_POS,
            end: ZERO_POS,
            op: None,
            mode: Mode::Start,
        }
    }
}

impl CanonicalizerState {
    /// Reset to `Start` without deallocating the buffer.
    #[inline]
    fn reset(&mut self) {
        self.buf.clear();
        self.op = None;
        self.mode = Mode::Start;
    }
}

impl Canonicalizer {
    pub fn new(max_token_length: NonZero<usize>) -> Self {
        Self {
            emit: [None, None],
            state: CanonicalizerState::default(),
            max_token_length: max_token_length.get(),
        }
    }

    /// Push a token onto the re-emit stack (at most 2 slots are ever needed).
    #[inline]
    fn push_emit(&mut self, t: Token) {
        // Slot 0 is popped first; we fill from the back so that the first
        // pushed token is popped last, preserving LIFO order for two pushes.
        if self.emit[0].is_none() {
            self.emit[0] = Some(t);
        } else {
            debug_assert!(self.emit[1].is_none(), "emit overflow");
            self.emit[1] = Some(t);
        }
    }

    /// Pop the next pending re-emit token, if any.
    #[inline]
    fn pop_emit(&mut self) -> Option<Token> {
        // Swap so that the most-recently-pushed comes out first (LIFO), which
        // matches the original `Vec::pop` semantics.
        if self.emit[1].is_some() {
            self.emit[1].take()
        } else {
            self.emit[0].take()
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
        if let Some(t) = self.pop_emit() {
            return Ok(Some(t));
        }

        loop {
            let t = match (source)()? {
                Some(t) => t,
                None => return Ok(None),
            };

            match self.state.mode {
                Mode::Start => {
                    match &t.variant {
                        TokenVariant::String(b) => {
                            self.state.buf.clear();
                            self.state.buf.extend_from_slice(b);
                            self.state.start = t.start;
                            self.state.end = t.end;
                            self.state.mode = Mode::String;
                        }
                        TokenVariant::OverlongString => {
                            self.state.start = t.start;
                            self.state.end = t.end;
                            self.state.mode = Mode::OverlongString;
                        }
                        _ => return Ok(Some(t)),
                    }
                }

                // ── Accumulating a normal string ─────────────────────────────
                Mode::String => {
                    match &t.variant {
                        TokenVariant::String(rhs) => {
                            if self.state.buf.len() + rhs.len() > self.max_token_length {
                                self.state.buf.clear();
                                self.state.end = t.end;
                                self.state.mode = Mode::OverlongString;
                            } else {
                                self.state.buf.extend_from_slice(rhs);
                                self.state.end = t.end;
                            }
                        }
                        TokenVariant::OverlongString => {
                            self.state.buf.clear();
                            self.state.end = t.end;
                            self.state.mode = Mode::OverlongString;
                        }
                        TokenVariant::Byte(b @ (b'+' | b'\\')) => {
                            self.state.op = Some((*b, t.start, t.end));
                            self.state.mode = Mode::StringOp;
                        }
                        _ => {
                            // Emit the accumulated string; stash the current for next call.
                            let out = self.flush_string();
                            self.push_emit(t);
                            return Ok(Some(out));
                        }
                    }
                }

                // ── String followed by a pending operator ────────────────────
                Mode::StringOp => {
                    match &t.variant {
                        TokenVariant::String(rhs) => {
                            if self.state.buf.len() + rhs.len() > self.max_token_length {
                                self.state.buf.clear();
                                self.op_take(); // discard op
                                self.state.end = t.end;
                                self.state.mode = Mode::OverlongString;
                            } else {
                                self.state.buf.extend_from_slice(rhs);
                                self.op_take(); // consume op — strings merged
                                self.state.end = t.end;
                                self.state.mode = Mode::String;
                            }
                        }
                        TokenVariant::OverlongString => {
                            self.state.buf.clear();
                            self.op_take();
                            self.state.end = t.end;
                            self.state.mode = Mode::OverlongString;
                        }
                        _ => {
                            // Cannot fold — emit string, then op, stash lookahead.
                            let (op_byte, op_start, op_end) = self.op_take();
                            let out = self.flush_string();
                            self.push_emit(t);
                            self.push_emit(Token {
                                variant: TokenVariant::Byte(op_byte),
                                start: op_start,
                                end: op_end,
                            });
                            return Ok(Some(out));
                        }
                    }
                }

                // ── Overlong string — consume until a non-string token ────────
                Mode::OverlongString => {
                    match &t.variant {
                        TokenVariant::String(_) | TokenVariant::OverlongString => {
                            self.state.end = t.end;
                            // mode stays OverlongString
                        }
                        _ => {
                            let out = self.flush_overlong();
                            self.push_emit(t);
                            return Ok(Some(out));
                        }
                    }
                }
            }
        }
    }

    pub fn drain(&mut self) -> Option<Token> {
        if let Some(t) = self.pop_emit() {
            return Some(t);
        }

        match self.state.mode {
            Mode::Start => None,
            Mode::String => Some(self.flush_string()),
            Mode::OverlongString => Some(self.flush_overlong()),
            Mode::StringOp => {
                let (op_byte, op_start, op_end) = self.op_take();
                let out = self.flush_string();
                self.push_emit(Token {
                    variant: TokenVariant::Byte(op_byte),
                    start: op_start,
                    end: op_end,
                });
                Some(out)
            }
        }
    }

    // ── Helpers ───────────────────────────────────────────────────────────────

    /// Build and return a `String` token from the current buffer, then reset.
    #[inline]
    fn flush_string(&mut self) -> Token {
        // SmallVec::drain(..) would let us move bytes out, but we want to keep
        // the allocation for reuse.  Clone only the bytes (stack-allocated for
        // the common case) and clear in place.
        let bytes: SmallVec<[u8; 0x40]> = self.state.buf.clone();
        let out = Token {
            variant: TokenVariant::String(bytes),
            start: self.state.start,
            end: self.state.end,
        };
        self.state.reset();
        out
    }

    /// Build and return an `OverlongString` token, then reset.
    #[inline]
    fn flush_overlong(&mut self) -> Token {
        let out = Token {
            variant: TokenVariant::OverlongString,
            start: self.state.start,
            end: self.state.end,
        };
        self.state.reset();
        out
    }

    /// Take the pending operator out of state, panicking in debug if absent.
    #[inline]
    fn op_take(&mut self) -> (u8, Position, Position) {
        debug_assert!(self.state.op.is_some(), "op_take called without an op");
        // Safety: we only call this from StringOp branches where op is Some.
        unsafe { self.state.op.take().unwrap_unchecked() }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
 
    // ── Helpers ───────────────────────────────────────────────────────────────
 
    fn pos(offset: usize) -> Position {
        Position { line: 0, column: offset, offset }
    }
 
    fn str_tok(s: &[u8], start: usize, end: usize) -> Token {
        Token {
            variant: TokenVariant::String(SmallVec::from_slice(s)),
            start: pos(start),
            end: pos(end),
        }
    }
 
    fn byte_tok(b: u8, at: usize) -> Token {
        Token { variant: TokenVariant::Byte(b), start: pos(at), end: pos(at) }
    }
 
    fn overlong_tok(start: usize, end: usize) -> Token {
        Token { variant: TokenVariant::OverlongString, start: pos(start), end: pos(end) }
    }
 
    /// Drive the canonicalizer to exhaustion and collect every token it emits.
    fn run(max: usize, tokens: Vec<Token>) -> Vec<Token> {
        let mut c = Canonicalizer::new(NonZero::new(max).unwrap());
        let mut it = tokens.into_iter();
        let mut out = Vec::new();
 
        loop {
            let tok = c.next_and_drain(|| Ok(it.next())).unwrap();
            match tok {
                Some(t) => out.push(t),
                None => break,
            }
        }
 
        // Flush any residue left in the canonicalizer after the iterator ends.
        while let Some(t) = c.drain() {
            out.push(t);
        }
 
        out
    }
 
    fn variants(toks: &[Token]) -> Vec<String> {
        toks.iter()
            .map(|t| match &t.variant {
                TokenVariant::String(b) => {
                    format!("String({:?})", std::str::from_utf8(b).unwrap_or("<bytes>"))
                }
                TokenVariant::Byte(b) => format!("Byte({})", *b as char),
                TokenVariant::OverlongString => "OverlongString".into(),
                _ => unreachable!(),
            })
            .collect()
    }
 
    // ── Basic passthrough ─────────────────────────────────────────────────────
 
    /// Non-string tokens pass straight through unchanged.
    #[test]
    fn passthrough_non_string() {
        let input = vec![byte_tok(b'=', 0), byte_tok(b';', 1)];
        let out = run(64, input);
        assert_eq!(variants(&out), ["Byte(=)", "Byte(;)"]);
    }
 
    /// A single string with nothing following it is emitted by drain.
    #[test]
    fn single_string_drained() {
        let input = vec![str_tok(b"hello", 0, 5)];
        let out = run(64, input);
        assert_eq!(variants(&out), [r#"String("hello")"#]);
    }
 
    // ── String concatenation ──────────────────────────────────────────────────
 
    /// Two adjacent strings are merged into one.
    #[test]
    fn two_adjacent_strings_merged() {
        let input = vec![str_tok(b"foo", 0, 3), str_tok(b"bar", 3, 6)];
        let out = run(64, input);
        assert_eq!(variants(&out), [r#"String("foobar")"#]);
        // Start/end span should cover the full range.
        assert_eq!(out[0].start, pos(0));
        assert_eq!(out[0].end, pos(6));
    }
 
    /// Three adjacent strings are all merged.
    #[test]
    fn three_adjacent_strings_merged() {
        let input = vec![
            str_tok(b"a", 0, 1),
            str_tok(b"b", 1, 2),
            str_tok(b"c", 2, 3),
        ];
        let out = run(64, input);
        assert_eq!(variants(&out), [r#"String("abc")"#]);
    }
 
    /// String followed by a non-string: string is emitted, then the other token.
    #[test]
    fn string_then_non_string() {
        let input = vec![str_tok(b"hello", 0, 5), byte_tok(b';', 5)];
        let out = run(64, input);
        assert_eq!(variants(&out), [r#"String("hello")"#, "Byte(;)"]);
        assert_eq!(out[0].end, pos(5));
        assert_eq!(out[1].start, pos(5));
    }
 
    // ── String + operator folding ─────────────────────────────────────────────
 
    /// "abc" + "def"  →  "abcdef"  (plus operator consumed).
    #[test]
    fn string_plus_string_folded() {
        let input = vec![
            str_tok(b"abc", 0, 3),
            byte_tok(b'+', 3),
            str_tok(b"def", 4, 7),
        ];
        let out = run(64, input);
        assert_eq!(variants(&out), [r#"String("abcdef")"#]);
    }
 
    /// "abc" \ "def"  →  "abcdef"  (backslash operator consumed).
    #[test]
    fn string_backslash_string_folded() {
        let input = vec![
            str_tok(b"abc", 0, 3),
            byte_tok(b'\\', 3),
            str_tok(b"def", 4, 7),
        ];
        let out = run(64, input);
        assert_eq!(variants(&out), [r#"String("abcdef")"#]);
    }
 
    /// "abc" + <non-string>: operator cannot be folded, so string and op are
    /// emitted separately and the lookahead token follows.
    #[test]
    fn string_plus_non_string_not_folded() {
        let input = vec![
            str_tok(b"abc", 0, 3),
            byte_tok(b'+', 3),
            byte_tok(b';', 4),
        ];
        let out = run(64, input);
        assert_eq!(variants(&out), [r#"String("abc")"#, "Byte(+)", "Byte(;)"]);
    }
 
    // ── Overlong string handling ──────────────────────────────────────────────
 
    /// Strings whose combined length exceeds max are collapsed to OverlongString.
    #[test]
    fn exceeds_max_becomes_overlong() {
        // max=5; "foo"(3) + "bar"(3) = 6 > 5
        let input = vec![str_tok(b"foo", 0, 3), str_tok(b"bar", 3, 6)];
        let out = run(5, input);
        assert_eq!(variants(&out), ["OverlongString"]);
        assert_eq!(out[0].start, pos(0));
        assert_eq!(out[0].end, pos(6));
    }
 
    /// An OverlongString token from the lexer is passed through as-is.
    #[test]
    fn lexer_overlong_passthrough() {
        let input = vec![overlong_tok(0, 10)];
        let out = run(64, input);
        assert_eq!(variants(&out), ["OverlongString"]);
    }
 
    /// A normal string followed immediately by a lexer OverlongString becomes
    /// a single OverlongString spanning both.
    #[test]
    fn string_then_lexer_overlong_merges() {
        let input = vec![str_tok(b"hi", 0, 2), overlong_tok(2, 20)];
        let out = run(64, input);
        assert_eq!(variants(&out), ["OverlongString"]);
        assert_eq!(out[0].start, pos(0));
        assert_eq!(out[0].end, pos(20));
    }
 
    /// Overlong string followed by a non-string emits the overlong first.
    #[test]
    fn overlong_then_non_string() {
        let input = vec![overlong_tok(0, 10), byte_tok(b';', 10)];
        let out = run(64, input);
        assert_eq!(variants(&out), ["OverlongString", "Byte(;)"]);
    }
 
    /// String + op, but the concatenation would be overlong → OverlongString,
    /// op is discarded.
    #[test]
    fn string_op_string_overlong() {
        // max=4; "foo"(3) + "bar"(3) = 6 > 4
        let input = vec![
            str_tok(b"foo", 0, 3),
            byte_tok(b'+', 3),
            str_tok(b"bar", 4, 7),
        ];
        let out = run(4, input);
        assert_eq!(variants(&out), ["OverlongString"]);
    }
 
    // ── Empty input ───────────────────────────────────────────────────────────
 
    #[test]
    fn empty_input_produces_nothing() {
        let out = run(64, vec![]);
        assert!(out.is_empty());
    }
 
    // ── Span correctness ──────────────────────────────────────────────────────
 
    /// Verify that flushing a string mid-stream (followed by a byte) preserves
    /// the correct start/end on both output tokens.
    #[test]
    fn span_after_flush() {
        let input = vec![
            str_tok(b"ab", 0, 2),
            str_tok(b"cd", 2, 4),
            byte_tok(b'=', 4),
            str_tok(b"ef", 5, 7),
        ];
        let out = run(64, input);
        assert_eq!(variants(&out), [r#"String("abcd")"#, "Byte(=)", r#"String("ef")"#]);
        assert_eq!(out[0].start, pos(0));
        assert_eq!(out[0].end, pos(4));
        assert_eq!(out[2].start, pos(5));
        assert_eq!(out[2].end, pos(7));
    }
}
