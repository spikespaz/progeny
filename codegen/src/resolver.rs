#![expect(unused)]

use std::borrow::Cow;
use std::collections::HashMap;

use openapiv3::{OpenAPI, ReferenceOr};

use crate::IntoCow;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("reference '{0}' is invalid")]
    InvalidReference(String),
    #[error("no document found for reference '{0}'")]
    DocumentNotFound(String),
    #[error("no component found for reference '{0}'")]
    ComponentNotFound(String),
    #[error("failed to deserialize component for reference '{reference}': {source}")]
    Deserialize {
        reference: String,
        #[source]
        source: serde_json::Error,
    },
}

#[derive(Debug)]
pub struct ReferenceResolver<'doc> {
    root: &'doc OpenAPI,
    documents: HashMap<String, Cow<'doc, serde_json::Value>>,
}

impl<'doc> ReferenceResolver<'doc> {
    pub fn new(root: &'doc OpenAPI) -> Self {
        let root_entry = (
            "".to_string(),
            Cow::Owned(serde_json::to_value(root).unwrap()),
        );
        let documents = HashMap::from_iter([root_entry]);
        Self { root, documents }
    }

    pub fn add_document(
        &mut self,
        url: impl Into<String>,
        document: impl IntoCow<'doc, serde_json::Value>,
    ) {
        self.documents.insert(url.into(), document.into_cow());
    }

    // TODO: Cache and return references to owned values where possible.
    fn resolve<'r, O>(&self, ref_or: &'r ReferenceOr<O>) -> Result<Cow<'r, O>, Error>
    where
        O: Clone + serde::de::DeserializeOwned,
    {
        match ref_or {
            ReferenceOr::Reference { reference } => {
                let (url, pointer) = match reference.split_once('#') {
                    Some((url, fragment)) => (url, fragment),
                    None => (reference.as_str(), ""),
                };
                let document = self
                    .documents
                    .get(url)
                    .ok_or_else(|| Error::DocumentNotFound(url.to_string()))?;
                let component = document
                    .pointer(pointer)
                    .ok_or_else(|| Error::InvalidReference(reference.clone()))?;
                let component =
                    serde_json::from_value(component.clone()).map_err(|e| Error::Deserialize {
                        reference: reference.clone(),
                        source: e,
                    })?;

                Ok(Cow::Owned(component))
            }
            ReferenceOr::Item(item) => Ok(Cow::Borrowed(item)),
        }
    }
}
