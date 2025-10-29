use openapiv3::ReferenceOr;

pub trait ReferenceOrExt: crate::Sealed {
    fn as_reference(&self) -> Option<&str>;
}

impl<T> crate::Sealed for ReferenceOr<T> {}

impl<T> ReferenceOrExt for ReferenceOr<T> {
    /// <https://github.com/glademiller/openapiv3/pull/99>
    fn as_reference(&self) -> Option<&str> {
        match self {
            ReferenceOr::Reference { reference } => Some(reference),
            ReferenceOr::Item(_) => None,
        }
    }
}
