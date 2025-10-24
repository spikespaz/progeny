#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum InferNameError {
    #[error("invalid JSON pointer in reference '{reference}': {reason}")]
    InvalidPointer {
        reference: String,
        reason: &'static str,
    },
    #[error("failed to decode JSON pointer in reference '{reference}': {source}")]
    DecodePointer {
        reference: String,
        #[source]
        source: std::str::Utf8Error,
    },
    #[error("unable to infer type name from reference '{reference}'")]
    MissingName { reference: String },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeRef {
    pub ident: syn::Ident,
    pub reference: Option<String>,
}

impl TypeRef {
    pub fn from_reference(reference: impl Into<String>) -> Result<Self, InferNameError> {
        let reference = reference.into();

        let (url, fragment) = reference
            .split_once('#')
            .unwrap_or((reference.as_str(), ""));

        let inferred_name = if fragment.is_empty() {
            if url.is_empty() {
                return Err(InferNameError::MissingName { reference });
            }
            // Parsing a URL as a path works for 99% of expected usage.
            // See the ignored tests for _some_ known failing edge-cases.
            let Some(first_label) = std::path::Path::new(url)
                .file_name()
                .map(|s| s.to_str().unwrap())
                .and_then(|fname| fname.split('.').find(|s| !s.is_empty()))
            else {
                return Err(InferNameError::MissingName { reference });
            };

            std::borrow::Cow::Borrowed(first_label)
        } else {
            use percent_encoding::percent_decode_str;
            let pointer = match percent_decode_str(fragment).decode_utf8() {
                Ok(pointer) => pointer,
                Err(e) => {
                    return Err(InferNameError::DecodePointer {
                        reference,
                        source: e,
                    });
                }
            };
            if !pointer.starts_with('/') {
                return Err(InferNameError::InvalidPointer {
                    reference,
                    reason: "missing leading slash",
                });
            }
            if pointer.ends_with('/') {
                return Err(InferNameError::MissingName { reference });
            }
            let last_segment = pointer[1..].rsplit('/').next().unwrap();
            let last_segment = last_segment.replace("~1", "/").replace("~0", "~");

            std::borrow::Cow::Owned(last_segment)
        };

        Ok(Self {
            ident: format_ident_safe(inferred_name, convert_case::Case::Pascal),
            reference: Some(reference),
        })
    }
}

pub fn format_ident_safe(name: impl AsRef<str>, case: convert_case::Case) -> syn::Ident {
    use check_keyword::CheckKeyword;
    use convert_case::{Boundary, Casing as _};

    const NON_ALPHANUMERIC: Boundary = Boundary {
        name: "NonAlphanumeric",
        condition: |gs, _| {
            gs.first()
                .and_then(|g| g.chars().next())
                .is_some_and(|ch| !ch.is_alphanumeric())
        },
        arg: None,
        start: 0, // boundary before char
        len: 1,   // consume one char
    };
    const UPPER_UPPER_LOWER: Boundary = Boundary {
        name: "UpperUpperLower",
        condition: |gs, _| {
            matches!(
                std::array::from_fn(|i| gs.get(i).and_then(|g| g.chars().next())),
                [Some(a), Some(b), Some(c)] if a.is_uppercase() && b.is_uppercase() && c.is_lowercase()
            )
        },
        arg: None,
        start: 1, // boundary after the first uppercase char
        len: 0,   // consume nothing
    };
    const IDENT_WORD_BOUNDARIES: &[Boundary] = &[
        NON_ALPHANUMERIC,
        Boundary::LOWER_DIGIT,
        Boundary::UPPER_DIGIT,
        Boundary::DIGIT_LOWER,
        Boundary::DIGIT_UPPER,
        Boundary::LOWER_UPPER,
        UPPER_UPPER_LOWER,
    ];

    let name = name
        .as_ref()
        .with_boundaries(IDENT_WORD_BOUNDARIES)
        .to_case(case);

    if name.starts_with(|c: char| !c.is_alphabetic()) {
        quote::format_ident!("_{name}")
    } else if name.is_keyword() {
        quote::format_ident!("{}", name.into_safe())
    } else {
        quote::format_ident!("{name}")
    }
}

#[cfg(test)]
mod tests {
    use test_case::test_case;

    use super::{InferNameError, TypeRef, format_ident_safe};

    #[test_case("foo" => "Foo" ; "simple lower")]
    #[test_case("foo_bar" => "FooBar" ; "snake case")]
    #[test_case("user id" => "UserId" ; "string with spaces")]
    #[test_case("from-train-case" => "FromTrainCase" ; "train case")]
    #[test_case("any word_boundary-works$" => "AnyWordBoundaryWorks" ; "mixed word boundaries")]
    #[test_case(r#"a~b`c!d@e#f$g%h^i&j*k(l)m-n_o+p=q{r}s|t\u:v;w"x'y<z>A,B.C?D/E"# => "ABCDEFGHIJKLMNOPQRSTUVWXYZABCDE" ; "sanitize non-alphanumeric")]
    #[test_case("OpenAPI" => "OpenApi" ; "acronym runs to lowercase")]
    #[test_case("HTTPServer" => "HttpServer" ; "acronym then word")]
    #[test_case("123abc" => "_123Abc" ; "capitalize after digit")]
    #[test_case("123" => "_123" ; "leading digit underscore")]
    #[test_case("Self" => "Self_" ; "pascal keyword underscore")]
    #[test_case("_leading_underscore" => "LeadingUnderscore" ; "drop leading underscore")]
    fn format_ident(input: &str) -> String {
        format_ident_safe(input, convert_case::Case::Pascal).to_string()
    }

    //
    // identifier from reference URL
    //
    #[test_case("external-schema.json" => "ExternalSchema" ; "simple file")]
    #[test_case("dir/file.name.ext" => "File" ; "first non empty label before dot")]
    #[test_case(".hidden.yaml" => "Hidden" ; "hidden file")]
    #[test_case("some/dir/" => "Dir" ; "trailing slash uses last segment")]
    #[test_case("dir/." => "Dir" ; "dir dot pseudo file")]
    #[test_case("https://example.com/api/image-metadata.schema.json" => "ImageMetadata" ; "name from https url")]
    #[test_case("file:///etc/schemas/user.json" => "User" ; "name from file url")]
    #[test_case("https://example.com/api/foo/bar/." => "Bar" ; "https url trailing dot")]
    #[test_case("https://example.com/api/foo/bar/../baz.json" => "Baz" ; "https url with two dots in middle")]
    #[test_case("https://example.com/api/foo/./bar.json" => "Bar" ; "https url with one dot in middle")]
    #[test_case("https://example.com/dir;v=1/file.schema.json" => "File" ; "matrix param in path segment")]
    //
    // known broken (url branch), requires full URL parsing to fix
    //
    #[test_case("https://example.com/api/foo/bar/.." => ignore "Foo" ; "https url trailing two dots")]
    //
    // identifier from reference fragment
    //
    #[test_case("#/components/schemas/Foo" => "Foo" ; "basic fragment")]
    #[test_case("#/components/schemas/Foo~1Bar" => "FooBar" ; "json pointer escape slash")]
    #[test_case("#/components/schemas/~0tilde" => "Tilde" ; "json pointer escape tilde")]
    #[test_case("#/components/schemas/caf%C3%A9" => "Café" ; "percent decoded utf8 letter")]
    #[test_case("#%2Fcomponents%2Fschemas%2FFoo" => "Foo" ; "percent decoded slash")]
    #[test_case("#/components/schemas/Name+With+Plus" => "NameWithPlus" ; "literal plus chars")]
    //
    // precedence
    //
    #[test_case("schema.json#/components/schemas/Foo" => "Foo" ; "fragment takes precedence over url")]
    #[test_case("schema.json#" => "Schema" ; "empty fragment falls back to url")]
    fn infer_name_from_reference(reference: &str) -> String {
        TypeRef::from_reference(reference)
            .unwrap()
            .ident
            .to_string()
    }

    //
    // invalid pointers
    //
    #[test_case("#components/schemas/Foo" => matches InferNameError::InvalidPointer { reason: "missing leading slash", .. } ; "pointer missing leading slash")]
    //
    // invalid UTF8 when percent decoded
    //
    #[test_case("#/%E0%A4%A" => matches InferNameError::DecodePointer { .. } ; "invalid utf8 in fragment percent decode")]
    //
    // missing name (URL branch)
    //
    #[test_case("" => matches InferNameError::MissingName { .. } ; "empty string")]
    #[test_case("." => matches InferNameError::MissingName { .. } ; "one dot")]
    #[test_case(".." => matches InferNameError::MissingName { .. } ; "two dots")]
    #[test_case("/" => matches InferNameError::MissingName { .. } ; "root path")]
    #[test_case("file://.." => matches InferNameError::MissingName { .. } ; "file url two dots")]
    //
    // known broken (URL branch), requires full URL parsing to fix
    //
    #[test_case("file://." => ignore matches InferNameError::MissingName { .. } ; "file url trailing dot")]
    #[test_case("https://" => ignore matches InferNameError::MissingName { .. } ; "https url no host")]
    //
    // missing name (fragment branch)
    //
    #[test_case("#/" => matches InferNameError::MissingName { .. } ; "root pointer")]
    #[test_case("#/components/schemas/" => matches InferNameError::MissingName { .. } ; "fragment trailing slash no last segment")]
    //
    // both empty URL and fragment
    //
    #[test_case("#" => matches InferNameError::MissingName { .. } ; "empty fragment with empty url")]
    fn fail_infer_name_from_reference(reference: &str) -> InferNameError {
        TypeRef::from_reference(reference).unwrap_err()
    }
}
