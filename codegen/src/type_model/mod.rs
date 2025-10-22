mod graph;

pub mod kinds;

pub use graph::{Error, TypeGraph, TypeId};
#[doc(inline)]
pub use kinds::{ScalarType, TypeKind};
