pub mod formatting;
pub mod generator;
pub mod resolver;
pub mod type_model;

mod into_cow;

pub use self::generator::{Generator, Settings};
pub(crate) use crate::__sealed::Sealed;
pub(crate) use crate::into_cow::IntoCow;

#[doc(hidden)]
mod __sealed {
    pub trait Sealed {}

    impl Sealed for () {}
}
