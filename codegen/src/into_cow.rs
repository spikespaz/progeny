use std::borrow::{Cow, ToOwned};

pub trait IntoCow<'a, B: ?Sized + ToOwned> {
    fn into_cow(self) -> Cow<'a, B>;
}

impl<'a, B> IntoCow<'a, B> for B
where
    B: ToOwned<Owned = B>,
{
    #[inline]
    fn into_cow(self) -> Cow<'a, B> {
        Cow::Owned(self)
    }
}

impl<'a, B: ?Sized + ToOwned> IntoCow<'a, B> for &'a B {
    #[inline]
    fn into_cow(self) -> Cow<'a, B> {
        Cow::Borrowed(self)
    }
}

impl<'a, B: ?Sized + ToOwned> IntoCow<'a, B> for Cow<'a, B> {
    #[inline]
    fn into_cow(self) -> Cow<'a, B> {
        self
    }
}
