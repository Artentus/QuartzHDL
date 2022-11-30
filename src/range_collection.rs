#![allow(dead_code)]

use std::ops::{Range, RangeInclusive};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InclusiveRange {
    start_inclusive: u64,
    end_inclusive: u64,
}

impl InclusiveRange {
    #[inline]
    pub const fn new(start_inclusive: u64, end_inclusive: u64) -> Self {
        assert!(start_inclusive <= end_inclusive);

        Self {
            start_inclusive,
            end_inclusive,
        }
    }

    #[inline]
    pub const fn n_bit(n: u64) -> Self {
        assert!(n <= 64);

        Self {
            start_inclusive: 0,
            end_inclusive: ((1u128 << n) - 1) as u64,
        }
    }

    #[inline]
    const fn contains_inner(&self, value: u64) -> bool {
        (self.start_inclusive <= value) && (self.end_inclusive >= value)
    }

    pub fn contains(&self, range: impl IntoRange) -> bool {
        if let Some(range) = range.into_range() {
            self.contains_inner(range.start_inclusive) && self.contains_inner(range.end_inclusive)
        } else {
            true
        }
    }

    #[inline]
    const fn touches_from_above(&self, value: u64) -> bool {
        (self.start_inclusive > value) && (self.start_inclusive == value.wrapping_add(1))
    }

    #[inline]
    const fn touches_from_below(&self, value: u64) -> bool {
        (self.end_inclusive < value) && (self.end_inclusive == value.wrapping_sub(1))
    }
}

pub trait IntoRange {
    fn into_range(self) -> Option<InclusiveRange>;
}

impl IntoRange for u64 {
    #[inline]
    fn into_range(self) -> Option<InclusiveRange> {
        Some(InclusiveRange::new(self, self))
    }
}

impl<'a> IntoRange for &'a u64 {
    #[inline]
    fn into_range(self) -> Option<InclusiveRange> {
        Some(InclusiveRange::new(*self, *self))
    }
}

impl IntoRange for Range<u64> {
    fn into_range(self) -> Option<InclusiveRange> {
        if self.is_empty() {
            None
        } else {
            Some(InclusiveRange::new(self.start, self.end - 1))
        }
    }
}

impl<'a> IntoRange for &'a Range<u64> {
    fn into_range(self) -> Option<InclusiveRange> {
        if self.is_empty() {
            None
        } else {
            Some(InclusiveRange::new(self.start, self.end - 1))
        }
    }
}

impl IntoRange for RangeInclusive<u64> {
    fn into_range(self) -> Option<InclusiveRange> {
        if self.is_empty() {
            None
        } else {
            Some(InclusiveRange::new(*self.start(), *self.end()))
        }
    }
}

impl<'a> IntoRange for &'a RangeInclusive<u64> {
    fn into_range(self) -> Option<InclusiveRange> {
        if self.is_empty() {
            None
        } else {
            Some(InclusiveRange::new(*self.start(), *self.end()))
        }
    }
}

impl IntoRange for InclusiveRange {
    #[inline]
    fn into_range(self) -> Option<InclusiveRange> {
        Some(self)
    }
}

impl<'a> IntoRange for &'a InclusiveRange {
    #[inline]
    fn into_range(self) -> Option<InclusiveRange> {
        Some(*self)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InsertResult {
    Inserted,
    AlreadyContained,
    Empty,
}

#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct RangeCollection {
    // INVARIANT: These ranges are not overlapping (including not touching each other)
    //            and sorted in ascending order by `start_inclusive`.
    //            By this follows `ranges[i].end_inclusive + 1 < ranges[i + 1].start_inclusive`.
    ranges: Vec<InclusiveRange>,
}

impl RangeCollection {
    #[inline]
    pub const fn new() -> Self {
        Self { ranges: Vec::new() }
    }

    pub fn contains(&self, range: impl IntoRange) -> bool {
        if let Some(range) = range.into_range() {
            for r in self.ranges.iter() {
                if r.contains(range) {
                    return true;
                }
            }

            false
        } else {
            true
        }
    }

    fn merge(&mut self, index_range: RangeInclusive<usize>) {
        assert!(!index_range.is_empty());

        // Here the ranges in `ranges[index_range]` are still sorted by `start_inclusive` but are overlapping.
        // Assuming the range at the last index contains the greatest `end_inclusive`, we restore the invariant
        // by merging all ranges in `ranges[index_range]` into one by taking `start_inclusive` from the first
        // range and `end_inclusive` from the last range.
        self.ranges[*index_range.end()].start_inclusive =
            self.ranges[*index_range.start()].start_inclusive;

        // Using the exclusive range here leaves the last range in the list.
        self.ranges.drain(*index_range.start()..*index_range.end());
    }

    fn insert_matching_start(&mut self, new_range: InclusiveRange, index: usize) -> InsertResult {
        if self.ranges[index].contains(new_range) {
            InsertResult::AlreadyContained
        } else {
            let mut candidate_index = index + 1;
            loop {
                if let Some(candidate_range) = self.ranges.get(candidate_index) {
                    if candidate_range.contains(new_range.end_inclusive)
                        || candidate_range.touches_from_above(new_range.end_inclusive)
                    {
                        // We found an existing range that contains or touches `new_range.end_inclusive`,
                        // so we merge all ranges up to this one.
                        self.merge(index..=candidate_index);

                        break;
                    } else if candidate_range.start_inclusive > new_range.end_inclusive {
                        // We found an existing range that is strictly greater than `new_range.end_inclusive`,
                        // so we have to extend the previous range and merge.
                        let last_index = candidate_index - 1;
                        self.ranges[last_index].end_inclusive = new_range.end_inclusive;
                        self.merge(index..=last_index);

                        break;
                    }

                    candidate_index += 1;
                } else {
                    // There is no existing range which contains `new_range.end_inclusive`,
                    // which means that is the new greatest value in the collection.
                    self.ranges[index].end_inclusive = new_range.end_inclusive;
                    self.ranges.drain((index + 1)..);

                    break;
                }
            }

            InsertResult::Inserted
        }
    }

    pub fn insert(&mut self, range: impl IntoRange) -> InsertResult {
        if let Some(new_range) = range.into_range() {
            match self
                .ranges
                .binary_search_by_key(&&new_range.start_inclusive, |r| &r.start_inclusive)
            {
                Ok(index) => {
                    // We already have a range in the list starting at the exact same value as `new_range`.
                    self.insert_matching_start(new_range, index)
                }
                Err(index) => {
                    // We do not have a range in the list starting at the exact same value as `new_range`.

                    if let Some(prev_index) = index.checked_sub(1) {
                        let prev_range = &self.ranges[prev_index];
                        if prev_range.contains(new_range.start_inclusive)
                            || prev_range.touches_from_below(new_range.start_inclusive)
                        {
                            // The previous range contains or touches `new_range.start_inclusive`,
                            // so we merge `new_range` with the previous.
                            let mut new_range = new_range;
                            new_range.start_inclusive = prev_range.start_inclusive;

                            self.insert_matching_start(new_range, prev_index)
                        } else if let Some(next_range) = self.ranges.get_mut(index)
                            && ((new_range.end_inclusive >= next_range.start_inclusive)
                                || next_range.touches_from_above(new_range.end_inclusive))
                        {
                            // The new range overlaps with the next one.
                            next_range.start_inclusive = new_range.start_inclusive;

                            self.insert_matching_start(new_range, index)
                        } else {
                            // The new range does not overlap with any existing ranges in the collection,
                            // no merging needs to occur.
                            self.ranges.insert(index, new_range);

                            InsertResult::Inserted
                        }
                    } else {
                        // There is no range with `start_inclusive < new_range.start_inclusive`,
                        // which means that is the new smallest value in the collection.
                        if let Some(first_range) = self.ranges.get_mut(0) {
                            // The collection contains at least one range already, so extend the smallest one down.
                            first_range.start_inclusive = new_range.start_inclusive;

                            self.insert_matching_start(new_range, 0)
                        } else {
                            // There is no range in the collection yet, insert the new one.
                            self.ranges.push(new_range);

                            InsertResult::Inserted
                        }
                    }
                }
            }
        } else {
            InsertResult::Empty
        }
    }
}

#[test]
fn test_inclusive_range_n_bit() {
    assert_eq!(InclusiveRange::n_bit(8).end_inclusive, u8::MAX.into());
    assert_eq!(InclusiveRange::n_bit(16).end_inclusive, u16::MAX.into());
    assert_eq!(InclusiveRange::n_bit(32).end_inclusive, u32::MAX.into());
    assert_eq!(InclusiveRange::n_bit(64).end_inclusive, u64::MAX.into());
    assert_eq!(InclusiveRange::n_bit(0).end_inclusive, 0);
    assert_eq!(InclusiveRange::n_bit(1).end_inclusive, 1);
    assert_eq!(InclusiveRange::n_bit(7).end_inclusive, 127);
    assert_eq!(InclusiveRange::n_bit(9).end_inclusive, 511);
}

#[test]
fn test_inclusive_range_contains() {
    let range = InclusiveRange::new(0, 10);
    assert!(range.contains(0));
    assert!(range.contains(5));
    assert!(range.contains(10));
    assert!(!range.contains(11));
}

#[test]
fn test_inclusive_range_contains_range() {
    let range1 = InclusiveRange::new(0, 10);
    let range2 = InclusiveRange::new(0, 5);
    let range3 = InclusiveRange::new(5, 10);
    let range4 = InclusiveRange::new(5, 15);
    let range5 = InclusiveRange::new(10, 20);
    let range6 = InclusiveRange::new(20, 30);
    assert!(range1.contains(range1));
    assert!(range1.contains(range2));
    assert!(range1.contains(range3));
    assert!(!range1.contains(range4));
    assert!(!range1.contains(range5));
    assert!(!range1.contains(range6));
}

#[test]
fn test_inclusive_range_touches() {
    let range = InclusiveRange::new(5, 10);
    assert!(range.touches_from_above(4));
    assert!(!range.touches_from_above(5));
    assert!(!range.touches_from_above(6));
    assert!(!range.touches_from_above(9));
    assert!(!range.touches_from_above(10));
    assert!(!range.touches_from_above(11));
    assert!(!range.touches_from_below(4));
    assert!(!range.touches_from_below(5));
    assert!(!range.touches_from_below(6));
    assert!(!range.touches_from_below(9));
    assert!(!range.touches_from_below(10));
    assert!(range.touches_from_below(11));
}

#[cfg(test)]
fn range_collection_equals(collection: &RangeCollection, ranges: &[(u64, u64)]) {
    assert!(collection.ranges.len() == ranges.len());
    for (a, b) in collection.ranges.iter().zip(ranges.iter()) {
        assert_eq!(a.start_inclusive, b.0);
        assert_eq!(a.end_inclusive, b.1);
    }
}

#[test]
fn test_range_collection_insert_non_overlapping() {
    let mut collection = RangeCollection::new();

    collection.insert(0);
    range_collection_equals(&collection, &[(0, 0)]);

    collection.insert(2);
    range_collection_equals(&collection, &[(0, 0), (2, 2)]);

    collection.insert(4..5);
    range_collection_equals(&collection, &[(0, 0), (2, 2), (4, 4)]);

    collection.insert(6..=6);
    range_collection_equals(&collection, &[(0, 0), (2, 2), (4, 4), (6, 6)]);
}

#[test]
fn test_range_collection_insert_overlapping() {
    let mut collection = RangeCollection::new();

    collection.insert(0..=10);
    range_collection_equals(&collection, &[(0, 10)]);

    collection.insert(2..=8);
    range_collection_equals(&collection, &[(0, 10)]);

    collection.insert(5..=15);
    range_collection_equals(&collection, &[(0, 15)]);

    collection.insert(30..=40);
    range_collection_equals(&collection, &[(0, 15), (30, 40)]);

    collection.insert(32..=38);
    range_collection_equals(&collection, &[(0, 15), (30, 40)]);

    collection.insert(25..=35);
    range_collection_equals(&collection, &[(0, 15), (25, 40)]);

    collection.insert(10..=30);
    range_collection_equals(&collection, &[(0, 40)]);
}

#[test]
fn test_range_collection_insert_touching() {
    let mut collection = RangeCollection::new();

    collection.insert(0..=10);
    range_collection_equals(&collection, &[(0, 10)]);

    collection.insert(11);
    range_collection_equals(&collection, &[(0, 11)]);

    collection.insert(30..=40);
    range_collection_equals(&collection, &[(0, 11), (30, 40)]);

    collection.insert(29);
    range_collection_equals(&collection, &[(0, 11), (29, 40)]);

    collection.insert(12..=28);
    range_collection_equals(&collection, &[(0, 40)]);
}
