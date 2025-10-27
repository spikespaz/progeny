/// Create a `static` for a concrete [`DeserializeOwned`] type.
///
/// The expression on the right-hand side of the assignment is expected to be
/// in [`json!`]-literal syntax. The input must be a valid JSON object.
///
/// The generated [`LazyLock`] will panic upon first access if the input fails
/// to deserialize as the requested type.
///
/// [`DeserializeOwned`]: https://docs.rs/serde/latest/serde/de/trait.DeserializeOwned.html
/// [`json!`]: serde_json::json
/// [`LazyLock`]: std::sync::LazyLock
///
/// ## Example
///
/// ```rust
/// use progeny_codegen::macros::static_json;
///
/// static_json! {
///     pub static SCHEMA_PLAIN_STRING: openapiv3::Schema = {
///         "type": "string"
///     };
///     pub static SCHEMA_BINARY_STRING: openapiv3::Schema = {
///         "type": "string",
///         "format": "binary"
///     };
/// }
/// ```
#[doc(inline)]
pub use crate::__static_json as static_json;

#[doc(hidden)]
#[macro_export]
macro_rules! __static_json {
    (
        $( $vis:vis static $IDENT:ident: $Type:ty = { $( $json:tt )* }; )*
    ) => {
        $(
            $vis static $IDENT: ::std::sync::LazyLock<$Type> = ::std::sync::LazyLock::new(|| {
                ::serde_json::from_value(::serde_json::json!({ $($json)* }))
                    .expect(concat!(
                        "static_json!: failed to deserialize into `",
                        stringify!($Type), "` for static `", stringify!($IDENT), "`"
                    ))
            });
        )*
    }
}
