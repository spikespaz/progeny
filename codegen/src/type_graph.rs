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

#[derive(Clone, Debug, PartialEq, Eq)]
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
            ident: Self::format_ident(inferred_name),
            reference: Some(reference),
        })
    }

    pub fn format_ident(name: impl AsRef<str>) -> syn::Ident {
        use convert_case::{Case, Casing as _};

        const PASCAL_CASE_KEYWORDS: &[&str] = &["Self"];

        let name = name.as_ref().to_case(Case::Pascal);
        if name.starts_with(|c: char| !c.is_alphabetic())
            || PASCAL_CASE_KEYWORDS.contains(&name.as_str())
        {
            quote::format_ident!("_{name}")
        } else {
            quote::format_ident!("{name}")
        }
    }
}

#[cfg(test)]
mod tests {
    use test_case::test_case;

    use super::TypeRef;

    #[test_case("foo" => "Foo" ; "simple lower")]
    #[test_case("foo_bar" => "FooBar" ; "snake case")]
    #[test_case("user id" => "UserId" ; "string with spaces")]
    #[test_case("from-train-case" => "FromTrainCase" ; "train case")]
    #[test_case("any word_boundary-works$" => "AnyWordBoundaryWorks" ; "mixed word boundaries")]
    #[test_case("123abc" => "_123Abc" ; "capitalize after digit")]
    #[test_case("123" => "_123" ; "leading digit underscore")]
    #[test_case("Self" => "_Self" ; "pascal keyword underscore")]
    #[test_case("_leading_underscore" => "LeadingUnderscore" ; "drop leading underscore")]
    fn typeref_format_ident(input: &str) -> String {
        TypeRef::format_ident(input).to_string()
    }
}
