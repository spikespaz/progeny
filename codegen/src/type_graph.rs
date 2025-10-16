use heck::ToPascalCase as _;
use percent_encoding::percent_decode_str;
use quote::format_ident;

#[derive(Debug, thiserror::Error)]
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

        let name = inferred_name.to_pascal_case();

        const PASCAL_CASE_KEYWORDS: &[&str] = &["Self"];

        let ident = if name.starts_with(|c: char| !c.is_alphabetic())
            || PASCAL_CASE_KEYWORDS.contains(&name.as_str())
        {
            format_ident!("_{name}")
        } else {
            format_ident!("{name}")
        };

        Ok(Self {
            ident,
            reference: Some(reference),
        })
    }
}
