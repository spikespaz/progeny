use core::borrow::Borrow;
use core::cmp::Ordering;
use core::fmt;
use core::hash::{Hash, Hasher};
use core::ops::Deref;
use std::borrow::Cow;

use self::BoxOrRef::{Borrowed, Owned};

pub enum BoxOrRef<'a, T: ?Sized + 'a> {
    Borrowed(&'a T),
    Owned(Box<T>),
}

impl<T: ?Sized> BoxOrRef<'_, T> {
    pub const fn is_borrowed(&self) -> bool {
        matches!(self, Borrowed(_))
    }

    pub const fn is_owned(&self) -> bool {
        matches!(self, Owned(_))
    }
}

impl<T: Sized + Clone> BoxOrRef<'_, T> {
    pub fn box_owned(owned: T) -> Self {
        Self::Owned(Box::new(owned))
    }

    pub fn to_mut(&mut self) -> &mut T {
        match self {
            Borrowed(borrow) => {
                *self = Owned(Box::new(borrow.clone()));
                let Owned(boxed) = self else { unreachable!() };
                boxed
            }
            Owned(owned) => owned,
        }
    }

    pub fn into_owned(self) -> T {
        match self {
            Borrowed(borrow) => borrow.to_owned(),
            Owned(boxed) => *boxed,
        }
    }
}

impl<T: ?Sized> Borrow<T> for BoxOrRef<'_, T> {
    fn borrow(&self) -> &T {
        match *self {
            Borrowed(borrow) => borrow,
            Owned(ref boxed) => boxed.borrow(),
        }
    }
}

impl<T: Sized + Clone> Clone for BoxOrRef<'_, T> {
    fn clone(&self) -> Self {
        match *self {
            Borrowed(borrow) => Borrowed(borrow),
            Owned(ref boxed) => {
                let borrow = boxed.as_ref().borrow();
                Owned(Box::new(borrow.clone()))
            }
        }
    }

    fn clone_from(&mut self, source: &Self) {
        match (self, source) {
            (Owned(dest), Owned(boxed)) => {
                let borrow = boxed.as_ref().borrow();
                borrow.clone_into(dest)
            }
            (dest, source) => *dest = source.clone(),
        }
    }
}

impl<T: Sized + Default> Default for BoxOrRef<'_, T> {
    fn default() -> Self {
        Owned(Box::default())
    }
}

impl<T: ?Sized> AsRef<T> for BoxOrRef<'_, T> {
    fn as_ref(&self) -> &T {
        match *self {
            Borrowed(borrow) => borrow,
            Owned(ref boxed) => boxed.as_ref(),
        }
    }
}

impl<T: ?Sized> Deref for BoxOrRef<'_, T> {
    type Target = T;

    fn deref(&self) -> &T {
        match *self {
            Borrowed(borrowed) => borrowed,
            Owned(ref boxed) => boxed,
        }
    }
}

impl<T: ?Sized> Eq for BoxOrRef<'_, T> where T: Eq {}

impl<A: ?Sized, B: ?Sized> PartialEq<BoxOrRef<'_, B>> for BoxOrRef<'_, A>
where
    A: PartialEq<B>,
{
    #[inline]
    fn eq(&self, other: &BoxOrRef<'_, B>) -> bool {
        PartialEq::eq(self.deref(), other.deref())
    }
}

impl<T: ?Sized + Ord> Ord for BoxOrRef<'_, T> {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        Ord::cmp(self.deref(), other.deref())
    }
}

impl<T: ?Sized + PartialOrd> PartialOrd for BoxOrRef<'_, T> {
    #[inline]
    fn partial_cmp(&self, other: &BoxOrRef<'_, T>) -> Option<Ordering> {
        PartialOrd::partial_cmp(self.deref(), other.deref())
    }
}

impl<T: ?Sized + Hash> Hash for BoxOrRef<'_, T> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        Hash::hash(self.deref(), state)
    }
}

impl<T: ?Sized + fmt::Debug> fmt::Debug for BoxOrRef<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Borrowed(borrow) => fmt::Debug::fmt(borrow, f),
            Owned(ref boxed) => fmt::Debug::fmt(boxed, f),
        }
    }
}

impl<T: ?Sized + fmt::Display> fmt::Display for BoxOrRef<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Borrowed(borrow) => fmt::Display::fmt(borrow, f),
            Owned(ref boxed) => fmt::Display::fmt(boxed, f),
        }
    }
}

impl<'a, T: Sized + Clone> From<Cow<'a, T>> for BoxOrRef<'a, T> {
    fn from(other: Cow<'a, T>) -> Self {
        match other {
            Cow::Borrowed(borrow) => Borrowed(borrow),
            Cow::Owned(owned) => Owned(Box::new(owned)),
        }
    }
}

impl<'a, T: Sized + Clone> From<BoxOrRef<'a, T>> for Cow<'a, T> {
    fn from(other: BoxOrRef<'a, T>) -> Self {
        match other {
            Borrowed(borrowed) => Cow::Borrowed(borrowed),
            Owned(owned) => Cow::Owned(*owned),
        }
    }
}
