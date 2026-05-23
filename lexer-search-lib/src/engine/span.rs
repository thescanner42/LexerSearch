use serde::Serialize;

use crate::{
    engine::token::{GraphBuilderBracketType, Token, TokenVariant},
    lexer::EllipsisEnum,
};

#[derive(Default)]
pub struct InputSpanPreprocess {
    /// applies to next real token
    pending_start: bool,

    /// previous real token waiting to see if a SetEnd arrives
    hold: Option<HeldToken>,
}

struct HeldToken {
    token: Token,
    set_start: bool,
    set_end: bool,
}

pub struct InputSpanPreprocessOutput {
    pub set_start: bool,
    pub set_end: bool,

    /// guaranteed not to be SetStart/SetEnd
    pub token: Token,
}

impl InputSpanPreprocess {
    pub fn next_and_drain<F>(
        &mut self,
        source: F,
    ) -> Result<Option<InputSpanPreprocessOutput>, String>
    where
        F: FnMut() -> Result<Option<Token>, String>,
    {
        match self.next(source)? {
            Some(v) => Ok(Some(v)),
            None => Ok(self.drain()),
        }
    }

    pub fn next<F>(&mut self, mut source: F) -> Result<Option<InputSpanPreprocessOutput>, String>
    where
        F: FnMut() -> Result<Option<Token>, String>,
    {
        loop {
            match source()? {
                Some(tok) => {
                    match tok.variant {
                        TokenVariant::Ellipsis(EllipsisEnum::SetStart) => {
                            self.pending_start = true;
                        }

                        TokenVariant::Ellipsis(EllipsisEnum::SetEnd) => {
                            if let Some(hold) = &mut self.hold {
                                hold.set_end = true;
                            }
                        }

                        _ => {
                            let new_hold = HeldToken {
                                token: tok,
                                set_start: std::mem::take(&mut self.pending_start),
                                set_end: false,
                            };

                            match self.hold.replace(new_hold) {
                                Some(prev) => {
                                    return Ok(Some(InputSpanPreprocessOutput {
                                        set_start: prev.set_start,
                                        set_end: prev.set_end,
                                        token: prev.token,
                                    }));
                                }

                                None => {
                                    // first real token cannot yet be emitted
                                }
                            }
                        }
                    }
                }

                None => return Ok(None),
            }
        }
    }

    pub fn drain(&mut self) -> Option<InputSpanPreprocessOutput> {
        self.hold.take().map(|v| InputSpanPreprocessOutput {
            set_start: v.set_start,
            set_end: v.set_end,
            token: v.token,
        })
    }
}

#[derive(
    Debug,
    Clone,
    Default,
    PartialEq,
    Eq,
    bincode::Encode,
    bincode::Decode,
)]
pub struct SetSpan {
    /// [none, start, end, both]
    slots: [Option<usize>; 4],
}

impl Serialize for SetSpan {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeMap;

        let mut map = serializer.serialize_map(None)?;

        for (idx, slot) in self.slots.iter().enumerate() {
            if let Some(v) = slot {
                let key = match idx {
                    0 => "none",
                    1 => "start",
                    2 => "end",
                    3 => "both",
                    _ => unreachable!(),
                };
                map.serialize_entry(key, v)?;
            }
        }

        map.end()
    }
}

impl SetSpan {
    #[inline]
    const fn index(set_start: bool, set_end: bool) -> usize {
        match (set_start, set_end) {
            (false, false) => 0,
            (true, false) => 1,
            (false, true) => 2,
            (true, true) => 3,
        }
    }

    #[inline]
    const fn flags(index: usize) -> (bool, bool) {
        match index {
            0 => (false, false),
            1 => (true, false),
            2 => (false, true),
            3 => (true, true),
            _ => unreachable!(),
        }
    }

    pub fn from(i: usize, set_start: bool, set_end: bool) -> Self {
        let mut slots = [None; 4];
        slots[Self::index(set_start, set_end)] = Some(i);

        Self { slots }
    }

    /// Get existing route or create a new one.
    ///
    /// Returns the route index corresponding to the requested flags.
    pub fn follow_or_update(
        &mut self,
        set_start: bool,
        set_end: bool,
        mut create_if_not_available: impl FnMut() -> usize,
    ) -> usize {
        let idx = Self::index(set_start, set_end);

        match self.slots[idx] {
            Some(v) => v,
            None => {
                let new = create_if_not_available();
                self.slots[idx] = Some(new);
                new
            }
        }
    }

    /// f is:
    ///     (where_to_go, set_start, set_end)
    pub fn handle(
        &self,
        mut f: impl FnMut(usize, bool, bool) -> Result<(), String>,
    ) -> Result<(), String> {
        for (idx, slot) in self.slots.iter().enumerate() {
            if let Some(v) = slot {
                let (set_start, set_end) = Self::flags(idx);
                f(*v, set_start, set_end)?;
            }
        }
        Ok(())
    }

    pub fn iter(&self) -> impl Iterator<Item = (usize, bool, bool)> + '_ {
        self.slots.iter().enumerate().filter_map(|(idx, slot)| {
            slot.map(|v| {
                let (set_start, set_end) = Self::flags(idx);
                (v, set_start, set_end)
            })
        })
    }

    pub fn rewrite(&mut self, src: usize, dst: usize) {
        for slot in &mut self.slots {
            if *slot == Some(src) {
                *slot = Some(dst);
            }
        }
    }
}

#[derive(Default, Debug, PartialEq, Eq)]
pub struct GraphBuilderNodeEllipsisInfo {
    // corner is separate since ..> can be stated in an arbitrary position
    corner: Option<usize>,
    other: Option<GraphBuilderNodeEllipsisInfoEnum>,
}

/// it's not possible to have ... be contained and not contained in brackets yet
/// reaching the same node, inside of a pattern. this structure represents that
#[derive(Debug, PartialEq, Eq)]
pub enum GraphBuilderNodeEllipsisInfoEnum {
    Uncontained(usize),
    Round(usize),
    Square(usize),
    Curly(usize),
}

#[derive(Debug, PartialEq, Eq)]
pub enum EllipsisInfoHandleEnum {
    Uncontained,
    Round,
    Square,
    Curly,
    Corner,
}

impl GraphBuilderNodeEllipsisInfo {
    pub fn handle(
        &self,
        mut f: impl FnMut(usize, EllipsisInfoHandleEnum) -> Result<(), String>,
    ) -> Result<(), String> {
        if let Some(corner) = self.corner.as_ref() {
            f(*corner, EllipsisInfoHandleEnum::Corner)?;
        }

        if let Some(other) = self.other.as_ref() {
            match other {
                GraphBuilderNodeEllipsisInfoEnum::Uncontained(v) => {
                    f(*v, EllipsisInfoHandleEnum::Uncontained)?;
                }
                GraphBuilderNodeEllipsisInfoEnum::Round(v) => {
                    f(*v, EllipsisInfoHandleEnum::Round)?;
                }
                GraphBuilderNodeEllipsisInfoEnum::Square(v) => {
                    f(*v, EllipsisInfoHandleEnum::Square)?;
                }
                GraphBuilderNodeEllipsisInfoEnum::Curly(v) => {
                    f(*v, EllipsisInfoHandleEnum::Curly)?;
                }
            }
        }

        Ok(())
    }

    pub fn follow_or_add_corner(
        &mut self,
        mut create_if_not_available: impl FnMut() -> usize,
    ) -> usize {
        match self.corner.as_ref() {
            Some(v) => *v,
            None => {
                let r = create_if_not_available();
                self.corner = Some(r);
                r
            }
        }
    }

    pub fn follow_or_add(
        &mut self,
        bracket: Option<&GraphBuilderBracketType>,
        mut create_if_not_available: impl FnMut() -> usize,
    ) -> usize {
        match self.other.as_ref() {
            None => {
                let r = create_if_not_available();
                self.other = Some(match bracket {
                    Some(v) => match v {
                        GraphBuilderBracketType::Round => {
                            GraphBuilderNodeEllipsisInfoEnum::Round(r)
                        }
                        GraphBuilderBracketType::Square => {
                            GraphBuilderNodeEllipsisInfoEnum::Square(r)
                        }
                        GraphBuilderBracketType::Curly => {
                            GraphBuilderNodeEllipsisInfoEnum::Curly(r)
                        }
                    },
                    None => GraphBuilderNodeEllipsisInfoEnum::Uncontained(r),
                });
                r
            }
            Some(v) => match v {
                GraphBuilderNodeEllipsisInfoEnum::Uncontained(v) => {
                    debug_assert!(matches!(bracket, None));
                    *v
                }
                GraphBuilderNodeEllipsisInfoEnum::Round(v) => {
                    debug_assert!(matches!(bracket, Some(GraphBuilderBracketType::Round)));
                    *v
                }
                GraphBuilderNodeEllipsisInfoEnum::Square(v) => {
                    debug_assert!(matches!(bracket, Some(GraphBuilderBracketType::Square)));
                    *v
                }
                GraphBuilderNodeEllipsisInfoEnum::Curly(v) => {
                    debug_assert!(matches!(bracket, Some(GraphBuilderBracketType::Curly)));
                    *v
                }
            },
        }
    }
}


#[derive(Default, Debug, PartialEq, Eq, serde::Serialize, bincode::Encode, bincode::Decode, Clone)]
pub struct GraphNodeEllipsisInfo {
    // corner is separate since ..> can be stated in an arbitrary position
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub corner: Vec<usize>,
    #[serde(skip_serializing_if = "GraphNodeEllipsisInfoEnum::is_default")]
    pub other: GraphNodeEllipsisInfoEnum,
}

/// it's not possible to have ... be contained and not contained in brackets yet
/// reaching the same node, inside of a pattern. this structure represents that
#[derive(Default, Debug, PartialEq, Eq, serde::Serialize, bincode::Encode, bincode::Decode, Clone)]
pub enum GraphNodeEllipsisInfoEnum {
    #[default]
    None,
    Uncontained(Vec<usize>),
    Round(Vec<usize>),
    Square(Vec<usize>),
    Curly(Vec<usize>),
}

impl GraphNodeEllipsisInfoEnum {
    pub fn is_default(&self) -> bool {
        *self == Default::default()
    }
}

impl GraphNodeEllipsisInfo {
    pub fn is_default(&self) -> bool {
        *self == Default::default()
    }

    pub fn handle(
        &self,
        mut f: impl FnMut(usize, EllipsisInfoHandleEnum),
    ) {
        for corner in self.corner.iter() {
            f(*corner, EllipsisInfoHandleEnum::Corner);
        }

        match &self.other {
            GraphNodeEllipsisInfoEnum::None => {},
            GraphNodeEllipsisInfoEnum::Uncontained(items) => {
                for v in items.iter() {
                    f(*v, EllipsisInfoHandleEnum::Uncontained);
                }
            },
            GraphNodeEllipsisInfoEnum::Round(items) => {
                for v in items.iter() {
                    f(*v, EllipsisInfoHandleEnum::Round);
                }
            },
            GraphNodeEllipsisInfoEnum::Square(items) => {
                for v in items.iter() {
                    f(*v, EllipsisInfoHandleEnum::Square);
                }
            },
            GraphNodeEllipsisInfoEnum::Curly(items) => {
                for v in items.iter() {
                    f(*v, EllipsisInfoHandleEnum::Curly);
                }
            },
        }
    }
}