pub mod formatting;
pub mod generator;
pub mod macros;
pub mod resolver;
pub mod type_model;
pub mod type_names;

mod into_cow;
mod openapi_ext;

pub use self::generator::{Generator, Settings};
pub(crate) use crate::__sealed::Sealed;
pub(crate) use crate::into_cow::IntoCow;

#[doc(hidden)]
mod __sealed {
    pub trait Sealed {}

    impl Sealed for () {}
}
