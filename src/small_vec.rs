#![allow(dead_code)]

use std::mem::MaybeUninit;
use std::ops::{Deref, DerefMut, Index, IndexMut, Range};
use std::slice::SliceIndex;

struct StackVec<T, const N: usize> {
    mem: [MaybeUninit<T>; N],
    range: Range<usize>,
}

impl<T, const N: usize> StackVec<T, N> {
    #[inline]
    const fn new() -> Self {
        Self {
            mem: MaybeUninit::uninit_array(),
            range: 0..0,
        }
    }

    #[inline]
    const fn len(&self) -> usize {
        self.range.end - self.range.start
    }

    #[inline]
    const fn capacity(&self) -> usize {
        N
    }

    #[inline]
    unsafe fn push_unchecked(&mut self, value: T) {
        self.mem.get_unchecked_mut(self.range.end).write(value);
        self.range.end += 1;
    }

    #[inline]
    #[must_use]
    fn try_push(&mut self, value: T) -> Result<(), T> {
        if self.len() < self.capacity() {
            unsafe {
                self.push_unchecked(value);
            }
            Ok(())
        } else {
            Err(value)
        }
    }

    #[inline]
    fn pop(&mut self) -> Option<T> {
        if self.len() == 0 {
            None
        } else {
            self.range.end -= 1;
            Some(unsafe { self.mem.get_unchecked(self.range.end).assume_init_read() })
        }
    }

    #[inline]
    fn as_uninit_slice(&self) -> &[MaybeUninit<T>] {
        unsafe { self.mem.get_unchecked(self.range.clone()) }
    }

    #[inline]
    fn as_uninit_mut_slice(&mut self) -> &mut [MaybeUninit<T>] {
        unsafe { self.mem.get_unchecked_mut(self.range.clone()) }
    }

    #[inline]
    fn as_slice(&self) -> &[T] {
        unsafe { MaybeUninit::slice_assume_init_ref(self.as_uninit_slice()) }
    }

    #[inline]
    fn as_mut_slice(&mut self) -> &mut [T] {
        unsafe { MaybeUninit::slice_assume_init_mut(self.as_uninit_mut_slice()) }
    }
}

impl<T, const N: usize> Drop for StackVec<T, N> {
    fn drop(&mut self) {
        for item in self.as_uninit_mut_slice().iter_mut() {
            unsafe {
                item.assume_init_drop();
            }
        }
    }
}

impl<T: Clone, const N: usize> Clone for StackVec<T, N> {
    fn clone(&self) -> Self {
        let mut mem = MaybeUninit::uninit_array();
        for (src, dst) in self
            .as_slice()
            .iter()
            .zip(unsafe { mem.get_unchecked_mut(self.range.clone()) }.iter_mut())
        {
            dst.write(src.clone());
        }

        Self {
            mem,
            range: self.range.clone(),
        }
    }
}

#[derive(Clone)]
enum SmallVecInternal<T, const N: usize> {
    Stack(StackVec<T, N>),
    Heap(Vec<T>),
}

#[derive(Clone)]
#[repr(transparent)]
pub struct SmallVec<T, const N: usize> {
    inner: SmallVecInternal<T, N>,
}

impl<T, const N: usize> SmallVec<T, N> {
    #[inline]
    pub const fn new() -> Self {
        Self {
            inner: SmallVecInternal::Stack(StackVec::new()),
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        if capacity <= N {
            Self {
                inner: SmallVecInternal::Stack(StackVec::new()),
            }
        } else {
            Self {
                inner: SmallVecInternal::Heap(Vec::with_capacity(capacity)),
            }
        }
    }

    pub fn len(&self) -> usize {
        match &self.inner {
            SmallVecInternal::Stack(data) => data.len(),
            SmallVecInternal::Heap(data) => data.len(),
        }
    }

    pub fn capacity(&self) -> usize {
        match &self.inner {
            SmallVecInternal::Stack(data) => data.capacity(),
            SmallVecInternal::Heap(data) => data.capacity(),
        }
    }

    pub fn push(&mut self, value: T) {
        match &mut self.inner {
            SmallVecInternal::Stack(data) => {
                if let Err(value) = data.try_push(value) {
                    let mut heap_data = Vec::with_capacity(N * 2);
                    for item in data.mem.iter() {
                        heap_data.push(unsafe { item.assume_init_read() });
                    }
                    heap_data.push(value);

                    data.range = 0..0;
                    self.inner = SmallVecInternal::Heap(heap_data);
                }
            }
            SmallVecInternal::Heap(data) => data.push(value),
        }
    }

    pub fn pop(&mut self) -> Option<T> {
        match &mut self.inner {
            SmallVecInternal::Stack(data) => data.pop(),
            SmallVecInternal::Heap(data) => data.pop(),
        }
    }

    pub fn as_slice(&self) -> &[T] {
        match &self.inner {
            SmallVecInternal::Stack(data) => data.as_slice(),
            SmallVecInternal::Heap(data) => data.as_slice(),
        }
    }

    pub fn as_mut_slice(&mut self) -> &mut [T] {
        match &mut self.inner {
            SmallVecInternal::Stack(data) => data.as_mut_slice(),
            SmallVecInternal::Heap(data) => data.as_mut_slice(),
        }
    }

    #[inline]
    pub fn iter<'a>(&'a self) -> impl Iterator<Item = &'a T> {
        self.into_iter()
    }

    #[inline]
    pub fn iter_mut<'a>(&'a mut self) -> impl Iterator<Item = &'a mut T> {
        self.into_iter()
    }

    pub fn reserve(&mut self, additional: usize) {
        match &mut self.inner {
            SmallVecInternal::Stack(data) => {
                if (data.len() + additional) > N {
                    let mut heap_data = Vec::new();
                    heap_data.reserve(data.len() + additional);

                    for item in data.as_uninit_slice().iter() {
                        heap_data.push(unsafe { item.assume_init_read() });
                    }

                    data.range = 0..0;
                    self.inner = SmallVecInternal::Heap(heap_data);
                }
            }
            SmallVecInternal::Heap(data) => data.reserve(additional),
        }
    }

    pub fn reserve_exact(&mut self, additional: usize) {
        match &mut self.inner {
            SmallVecInternal::Stack(data) => {
                if (data.len() + additional) > N {
                    let mut heap_data = Vec::new();
                    heap_data.reserve_exact(data.len() + additional);

                    for item in data.as_uninit_slice().iter() {
                        heap_data.push(unsafe { item.assume_init_read() });
                    }

                    data.range = 0..0;
                    self.inner = SmallVecInternal::Heap(heap_data);
                }
            }
            SmallVecInternal::Heap(data) => data.reserve_exact(additional),
        }
    }

    pub fn shrink_to_fit(&mut self) {
        match &mut self.inner {
            SmallVecInternal::Stack(_) => {}
            SmallVecInternal::Heap(data) => {
                if data.len() > N {
                    data.shrink_to_fit();
                } else {
                    let mut stack_data = StackVec::new();
                    for item in data.drain(..) {
                        unsafe {
                            stack_data.push_unchecked(item);
                        }
                    }

                    self.inner = SmallVecInternal::Stack(stack_data);
                }
            }
        }
    }
}

impl<T, const N: usize> Deref for SmallVec<T, N> {
    type Target = [T];

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

impl<T, const N: usize> DerefMut for SmallVec<T, N> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_mut_slice()
    }
}

impl<T, I: SliceIndex<[T]>, const N: usize> Index<I> for SmallVec<T, N> {
    type Output = I::Output;

    #[inline]
    fn index(&self, index: I) -> &Self::Output {
        self.get(index).unwrap()
    }
}

impl<T, I: SliceIndex<[T]>, const N: usize> IndexMut<I> for SmallVec<T, N> {
    #[inline]
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        self.get_mut(index).unwrap()
    }
}

#[repr(transparent)]
struct StackIter<T, const N: usize> {
    data: StackVec<T, N>,
}

impl<T, const N: usize> Iterator for StackIter<T, N> {
    type Item = T;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.data.range.start < self.data.range.end {
            let item = unsafe {
                self.data
                    .mem
                    .get_unchecked(self.data.range.start)
                    .assume_init_read()
            };
            self.data.range.start += 1;
            Some(item)
        } else {
            None
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let size = self.data.range.start - self.data.range.end;
        (size, Some(size))
    }
}

enum IntoIterInternal<T, const N: usize> {
    Stack(StackIter<T, N>),
    Heap(std::vec::IntoIter<T>),
}

#[repr(transparent)]
pub struct IntoIter<T, const N: usize> {
    inner: IntoIterInternal<T, N>,
}

impl<T, const N: usize> Iterator for IntoIter<T, N> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.inner {
            IntoIterInternal::Stack(iter) => iter.next(),
            IntoIterInternal::Heap(iter) => iter.next(),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match &self.inner {
            IntoIterInternal::Stack(iter) => iter.size_hint(),
            IntoIterInternal::Heap(iter) => iter.size_hint(),
        }
    }
}

impl<T, const N: usize> IntoIterator for SmallVec<T, N> {
    type Item = T;
    type IntoIter = IntoIter<T, N>;

    fn into_iter(self) -> Self::IntoIter {
        match self.inner {
            SmallVecInternal::Stack(data) => IntoIter {
                inner: IntoIterInternal::Stack(StackIter { data }),
            },
            SmallVecInternal::Heap(data) => IntoIter {
                inner: IntoIterInternal::Heap(data.into_iter()),
            },
        }
    }
}

impl<'a, T, const N: usize> IntoIterator for &'a SmallVec<T, N> {
    type Item = &'a T;
    type IntoIter = std::slice::Iter<'a, T>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.as_slice().into_iter()
    }
}

impl<'a, T, const N: usize> IntoIterator for &'a mut SmallVec<T, N> {
    type Item = &'a mut T;
    type IntoIter = std::slice::IterMut<'a, T>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.as_mut_slice().into_iter()
    }
}
