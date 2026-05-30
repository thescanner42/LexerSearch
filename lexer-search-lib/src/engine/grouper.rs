use std::{
    collections::{HashMap, VecDeque},
    num::NonZero,
};

use crate::engine::matcher::FullMatch;

#[derive(Debug, Clone)]
struct FullMatchWrapper {
    inner: FullMatch,
    used: bool,
}

/// it was difficult to write this component
///
/// the time complexity is O(n^2) but because N is capped it's O(1). the default
/// cap is small so for smaller workloads this naive linear solution is probably
/// faster than more complicated solutions that have a better time complexity,
/// but this isn't ideal if the cap is raised in the future
///
/// at a later time this can be revisited (maybe slotmap storage + priority
/// queue dropping + interval tree lookup?). not clear that more is necessary
pub struct Grouper {
    /// associates the group name with the full match queue -  this gives better
    /// insulation in case one rule is producing a lot of full matches and
    /// overwritting a global queue - could drown out and affect other rules
    full_matches: lru::LruCache<String, VecDeque<FullMatchWrapper>>,

    max_full_matches: NonZero<usize>,
    max_unique_expansions: NonZero<usize>,
}

#[derive(Debug, Default)]
struct UniqueOuts<'a> {
    unique_rules: HashMap<&'a str, Vec<&'a FullMatch>>,
}

impl<'a> UniqueOuts<'a> {
    fn apply(o: &mut FullMatchWrapper, m: &FullMatch) {
        if o.inner.group.unique && o.inner.group.name == m.group.name {
            return;
        }
        o.inner.captures.extend(m.captures.clone());
        // visual span should be the union of applicable elements but prioritize the span override
        if !o.inner.visual_start_position_overridden && m.start.offset < o.inner.start.offset {
            o.inner.start = m.start;
        }
        if !o.inner.visual_end_position_overridden && m.end.offset > o.inner.end.offset {
            o.inner.end = m.end;
        }

        o.inner.group.cancel |= m.group.cancel;
    }

    pub fn handle_applicable(&mut self, rm: &mut FullMatchWrapper, m: &'a FullMatch) {
        if !m.group.unique {
            Self::apply(rm, m);
        } else {
            self.unique_rules.entry(&m.name).or_default().push(m);
        }
    }

    pub fn drain(
        self,
        rm: FullMatchWrapper,
        out: &mut impl FnMut(FullMatch),
        max_unique_expansions: NonZero<usize>,
    ) {
        let mut axes: Vec<_> = self.unique_rules.into_iter().collect();
        axes.sort_by_key(|(k, _)| *k); // required for deterministic output match order
        let axes: Vec<Vec<&FullMatch>> = axes.into_iter().map(|(_, v)| v).collect();

        let mut count = 0;

        fn cartesian<'a>(
            axes: &[Vec<&'a FullMatch>],
            combo: &mut Vec<&'a FullMatch>,
            f: &mut impl FnMut(&[&'a FullMatch]) -> bool, // false = stop
        ) -> bool {
            if axes.is_empty() {
                return f(combo);
            }
            for &m in &axes[0] {
                combo.push(m);
                let keep_going = cartesian(&axes[1..], combo, f);
                combo.pop();
                if !keep_going {
                    return false;
                }
            }
            true
        }

        cartesian(&axes, &mut vec![], &mut |combo| {
            if count >= max_unique_expansions.get() {
                return false;
            }
            let mut result = rm.clone();
            for m in combo {
                Self::apply(&mut result, m);
            }
            let grp = std::mem::take(&mut result.inner.group);
            result.inner.name = grp.name;
            result.inner.group.cancel = grp.cancel;
            out(result.inner);
            count += 1;
            count < max_unique_expansions.get()
        });
    }
}

impl Grouper {
    pub fn new(
        max_distinct_groups: NonZero<usize>,
        max_full_matches: NonZero<usize>,
        max_unique_expansions: NonZero<usize>,
    ) -> Self {
        Self {
            full_matches: lru::LruCache::new(max_distinct_groups),
            max_full_matches,
            max_unique_expansions,
        }
    }

    pub fn drain(&mut self, mut out: impl FnMut(FullMatch)) {
        let current = self.full_matches.cap();
        let taken = std::mem::replace(&mut self.full_matches, lru::LruCache::new(current));
        for (_evicted_k, mut evicted_v) in taken {
            // O(n^2) but constant since full_matches is capped
            while let Some(v) = evicted_v.pop_front() {
                Self::drain_one(&mut evicted_v, v, &mut out, self.max_unique_expansions);
            }
        }
    }

    fn drain_one(
        me: &mut VecDeque<FullMatchWrapper>,
        mut rm: FullMatchWrapper,
        out: &mut impl FnMut(FullMatch),
        max_unique_expansions: NonZero<usize>,
    ) {
        if rm.used {
            // dropped. this was already used by a different group. it was
            // marked and not removed right away because other things could have
            // still used it
            return;
        }

        let mut outs = UniqueOuts::default();
        'outer: for m in me.iter_mut() {
            if m.inner.actual_start_offset > rm.inner.actual_start_offset
                || m.inner.actual_end_offset < rm.inner.actual_end_offset
            {
                continue; // non-applicable span
            }

            // each queue only has that one group
            debug_assert!(m.inner.group.name == rm.inner.group.name);

            for k in rm.inner.group.r#match.iter() {
                if let Some(rm_v) = rm.inner.captures.get(k) {
                    // both declare a same variable then those variables
                    // must match - otherwise these are unrelated
                    if let Some(m_v) = m.inner.captures.get(k) {
                        if rm_v != m_v {
                            continue 'outer;
                        }
                    }
                }
            }

            // this match is applicable. if for some reason there is a
            // standalone unique rule with no anchor, then we don't mark it used
            // and each are sent to the output individually (more consistent)
            m.used |= !rm.inner.group.unique;
            outs.handle_applicable(&mut rm, &m.inner);
        }
        outs.drain(rm, out, max_unique_expansions);
    }

    pub fn process(&mut self, m: FullMatch, mut out: impl FnMut(FullMatch)) {
        if m.group.name.is_empty() {
            out(m); // does not opt into grouping
            return;
        }

        let full_matches = if let Some(v) = self.full_matches.get_mut(&m.group.name) {
            v
        } else {
            if let Some((evicted_k, mut evicted_v)) = self
                .full_matches
                .push(m.group.name.clone(), Default::default())
            {
                // this is not a key replace (the key is not already in the
                // map). this is evicting the least recently used
                debug_assert!(evicted_k != m.group.name);
                // O(n^2) but constant since it's capped
                while let Some(v) = evicted_v.pop_front() {
                    Self::drain_one(&mut evicted_v, v, &mut out, self.max_unique_expansions);
                }
            }

            // unwrap ok since it was just added
            self.full_matches.get_mut(&m.group.name).unwrap()
        };

        if full_matches.len() == self.max_full_matches.get() {
            // we are at capacity. must produce a group early based on available
            // info. precondition: full_matches not empty
            let rm = full_matches.pop_front().unwrap();
            Self::drain_one(full_matches, rm, &mut out, self.max_unique_expansions);
        }

        // room has been made if necessary above
        //
        // upstream matches are produced by end position
        //
        // ideal receival order is by end position then reverse start position.
        // tried changing this upstream but there's a small performance hit.
        // instead, changing the order here
        let mut insertion_position = full_matches.len();
        loop {
            if insertion_position == 0 {
                break;
            }
            let prev = &full_matches[insertion_position - 1];
            if prev.inner.actual_end_offset != m.actual_end_offset {
                // very likely
                //
                // worst case O(n) but this is constant since full_matches is
                // capped
                break;
            }

            if prev.inner.actual_start_offset >= m.actual_start_offset {
                break;
            }
            insertion_position -= 1;
        }
        full_matches.insert(
            insertion_position,
            FullMatchWrapper {
                inner: m,
                used: false,
            },
        );
    }
}
