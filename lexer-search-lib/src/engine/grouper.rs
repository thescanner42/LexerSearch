use std::{collections::BTreeMap, num::NonZero};

use crate::{engine::matcher::FullMatch, lexer::Position};

#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct FullMatchInGroup {
    name: String,
    start: Position,
    end: Position,
    captures: BTreeMap<Box<[u8]>, Box<[u8]>>,
}

impl FullMatchInGroup {
    pub fn from_full_match(i: FullMatch) -> Result<(FullMatchInGroup, String), FullMatch> {
        if i.group.is_empty() {
            Err(i)
        } else {
            Ok((
                FullMatchInGroup {
                    name: i.name,
                    start: i.start,
                    end: i.end,
                    captures: i.captures,
                },
                i.group,
            ))
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Group {
    group_name: String,
    data: Box<[FullMatchInGroup]>,
    data_len: usize,
    intersection: (Position, Position),
}

impl Group {
    pub fn new(cap: NonZero<usize>, group_name: String, first: FullMatchInGroup) -> Self {
        let union = (first.start, first.end);
        let intersection = union.clone();
        let mut data = vec![Default::default(); cap.get()].into_boxed_slice();
        data[0] = first;
        Self {
            data,
            data_len: 1,
            intersection,
            group_name,
        }
    }

    fn union(&self) -> (Position, Position) {
        let first = &self.data[0];
        let mut start = first.start;
        let mut end = first.end;

        for m in &self.data[1..self.data_len] {
            if m.start.offset < start.offset {
                start = m.start;
            }
            if m.end.offset > end.offset {
                end = m.end;
            }
        }
        (start, end)
    }

    fn recalculate_intersection(&mut self) {
        let strt = &self.data[0];
        let mut start = strt.start;
        let mut end = strt.end;

        for m in &self.data[1..self.data_len] {
            if m.start.offset > start.offset {
                start = m.start;
            }
            if m.end.offset < end.offset {
                end = m.end;
            }
        }
        self.intersection = (start, end);
    }

    /// might evict a clashing full match in group, or might not accept due to
    /// cap
    pub fn insert(&mut self, mut m: FullMatchInGroup) -> Option<FullMatchInGroup> {
        let self_start = self.intersection.0.offset;
        let self_end = self.intersection.1.offset;
        let m_start = m.start.offset;
        let m_end = m.end.offset;

        if self_start != m_start && self_end != m_end {
            return Some(m);
        }

        for i in 0..self.data_len {
            if self.data[i].name == m.name && m.start.offset > self.data[i].start.offset {
                // the upstream findings are returned in order of end position
                // then start position. it makes sense to replace where variable
                // redeclaration occurs (use the last instance).
                std::mem::swap(&mut m, &mut self.data[i]);
                self.recalculate_intersection();
                let _ = m; // discard overridden match
                return None;
            }
        }

        if self.data_len == self.data.len() {
            return Some(m); // cap hit
        }

        // update intersection
        if m.start.offset > self.intersection.0.offset {
            self.intersection.0 = m.start;
        }
        if m.end.offset < self.intersection.1.offset {
            self.intersection.1 = m.end;
        }

        self.data[self.data_len] = m;
        self.data_len += 1;
        None
    }

    pub fn consume(mut self) -> FullMatch {
        let union = self.union();
        let first = &mut self.data[0];
        let mut ret = FullMatch {
            start: union.0,
            end: union.1,
            name: self.group_name,
            group: "".to_string(),
            captures: std::mem::take(&mut first.captures),
        };
        for m in &mut self.data[1..self.data_len] {
            ret.captures.extend(std::mem::take(&mut m.captures));
        }
        ret
    }
}

#[derive(Default)]
pub struct Grouper {
    data_len: usize,
    data: Box<[Group]>,
}

impl Grouper {
    pub fn new(cap: NonZero<usize>) -> Self {
        Self {
            data: vec![Default::default(); cap.get()].into_boxed_slice(),
            data_len: 0,
        }
    }

    pub fn drain(&mut self, mut out: impl FnMut(FullMatch)) {
        for elem in &mut self.data[0..self.data_len] {
            let elem = std::mem::take(elem);
            out(elem.consume());
        }
        self.data_len = 0;
    }

    pub fn process(&mut self, m: FullMatch, mut out: impl FnMut(FullMatch)) {
        let (mut m, group_name) = match FullMatchInGroup::from_full_match(m) {
            Ok(v) => v,
            Err(m) => {
                out(m); // match does not come from a pattern that does grouping
                return;
            }
        };

        for i in 0..self.data_len {
            let group = &mut self.data[i];
            if group_name != group.group_name {
                continue;
            }

            match group.insert(m) {
                Some(v) => {
                    m = v;
                }
                None => return,
            }
        }

        // the full match could not be added to any group. create a new one
        let new_group = Group::new(self.data.len().try_into().unwrap(), group_name, m);
        self.insert_group(new_group, out);
    }

    fn insert_group(&mut self, new_group: Group, mut out: impl FnMut(FullMatch)) {
        if self.data.len() == self.data_len {
            // out of capacity. find which one to send
            let mut index = 0;
            let mut earliest_start_offset = self.data[0].intersection.0.offset;
            for i in 1..self.data_len {
                let o = self.data[i].intersection.0.offset;
                if o < earliest_start_offset {
                    earliest_start_offset = o;
                    index = i;
                }
            }

            let old_group = std::mem::replace(&mut self.data[index], new_group);
            out(old_group.consume());
            return;
        }

        self.data[self.data_len] = new_group;
        self.data_len += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::{collections::BTreeMap, num::NonZero};

    fn pos(offset: usize) -> Position {
        Position {
            offset,
            column: 0,
            line: 0,
        }
    }

    fn fm(name: &str, group: &str, start: usize, end: usize, caps: &[(&[u8], &[u8])]) -> FullMatch {
        let mut captures = BTreeMap::new();
        for (k, v) in caps {
            captures.insert(Box::from(*k), Box::from(*v));
        }

        FullMatch {
            name: name.to_string(),
            group: group.to_string(),
            start: pos(start),
            end: pos(end),
            captures,
        }
    }

    #[test]
    fn full_match_in_group_rejects_empty_group() {
        let m = fm("x", "", 0, 10, &[]);
        let res = FullMatchInGroup::from_full_match(m.clone());

        assert_eq!(res, Err(m));
    }

    #[test]
    fn full_match_in_group_accepts_non_empty_group() {
        let m = fm("x", "g", 0, 10, &[]);
        let (mig, group) = FullMatchInGroup::from_full_match(m).unwrap();

        assert_eq!(group, "g");
        assert_eq!(mig.name, "x");
        assert_eq!(mig.start.offset, 0);
        assert_eq!(mig.end.offset, 10);
    }

    #[test]
    fn group_insert_updates_intersection() {
        let first = FullMatchInGroup::from_full_match(fm("a", "g", 0, 10, &[]))
            .unwrap()
            .0;

        let mut group = Group::new(NonZero::new(4).unwrap(), "g".into(), first);

        // Same start, tighter end
        let second = FullMatchInGroup::from_full_match(fm("b", "g", 0, 8, &[]))
            .unwrap()
            .0;

        assert!(group.insert(second).is_none());

        assert_eq!(group.intersection.0.offset, 0);
        assert_eq!(group.intersection.1.offset, 8);
    }

    #[test]
    fn group_replaces_same_name_with_later_start() {
        let first = FullMatchInGroup::from_full_match(fm(
            "x",
            "g",
            0,
            10,
            &[("a".as_bytes(), "1".as_bytes())],
        ))
        .unwrap()
        .0;

        let mut group = Group::new(NonZero::new(2).unwrap(), "g".into(), first);

        // Same end, later start
        let replacement = FullMatchInGroup::from_full_match(fm(
            "x",
            "g",
            5,
            10,
            &[("b".as_bytes(), "2".as_bytes())],
        ))
        .unwrap()
        .0;

        assert!(group.insert(replacement).is_none());

        let consumed = group.consume();
        assert_eq!(consumed.start.offset, 5);
        assert_eq!(consumed.end.offset, 10);
        assert!(consumed.captures.contains_key(b"b".as_ref()));
        assert!(!consumed.captures.contains_key(b"a".as_ref()));
    }

    #[test]
    fn group_insert_respects_capacity() {
        let first = FullMatchInGroup::from_full_match(fm("a", "g", 0, 10, &[]))
            .unwrap()
            .0;

        let mut group = Group::new(NonZero::new(1).unwrap(), "g".into(), first);

        let second = FullMatchInGroup::from_full_match(fm("b", "g", 0, 10, &[]))
            .unwrap()
            .0;

        assert!(group.insert(second).is_some());
    }

    #[test]
    fn grouper_passes_through_ungrouped_matches() {
        let mut grouper = Grouper::new(NonZero::new(2).unwrap());

        let mut out = Vec::new();
        grouper.process(fm("x", "", 0, 5, &[]), |m| out.push(m));

        assert_eq!(out.len(), 1);
        assert_eq!(out[0].name, "x");
    }

    #[test]
    fn grouper_groups_and_drains() {
        let mut grouper = Grouper::new(NonZero::new(2).unwrap());
        let mut out = Vec::new();

        grouper.process(
            fm("a", "g", 0, 10, &[("a".as_bytes(), "1".as_bytes())]),
            |_| {},
        );

        // Same start, different end → valid group member
        grouper.process(
            fm("b", "g", 0, 8, &[("b".as_bytes(), "2".as_bytes())]),
            |_| {},
        );

        grouper.drain(|m| out.push(m));

        assert_eq!(out.len(), 1);
        let m = &out[0];

        assert_eq!(m.start.offset, 0);
        assert_eq!(m.end.offset, 10);
        assert!(m.captures.contains_key(b"a".as_ref()));
        assert!(m.captures.contains_key(b"b".as_ref()));
    }

    #[test]
    fn grouper_evicts_earliest_intersection_on_overflow() {
        let mut grouper = Grouper::new(NonZero::new(1).unwrap());
        let mut out = Vec::new();

        grouper.process(fm("a", "g1", 0, 10, &[]), |_| {});
        grouper.process(fm("b", "g2", 20, 30, &[]), |m| out.push(m));

        assert_eq!(out.len(), 1);
        assert_eq!(out[0].name, "g1");
    }
}
