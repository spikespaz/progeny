#![expect(unused)]

use std::borrow::Cow;
use std::collections::HashMap;

use openapiv3::{Components, OpenAPI, ReferenceOr};

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
    #[error("component at '{reference}' is a `{found}`, expected `{expected}`")]
    TypeMismatch {
        reference: String,
        found: &'static str,
        expected: &'static str,
    },
}

#[derive(Debug)]
pub struct ReferenceResolver<'doc> {
    root: &'doc OpenAPI,
    documents: HashMap<String, Cow<'doc, serde_json::Value>>,
    cache: HashMap<String, Component<'doc>>,
}

#[derive(Debug)]
pub enum Component<'a> {
    Schema(Cow<'a, openapiv3::Schema>),
    Response(Cow<'a, openapiv3::Response>),
    Parameter(Cow<'a, openapiv3::Parameter>),
    Example(Cow<'a, openapiv3::Example>),
    RequestBody(Cow<'a, openapiv3::RequestBody>),
    Header(Cow<'a, openapiv3::Header>),
    SecurityScheme(Cow<'a, openapiv3::SecurityScheme>),
    Link(Cow<'a, openapiv3::Link>),
    Callback(Cow<'a, openapiv3::Callback>),
    Other(Cow<'a, serde_json::Value>),
}

impl<'doc> ReferenceResolver<'doc> {
    pub fn new(root: &'doc OpenAPI) -> Self {
        let mut new = Self {
            root,
            documents: HashMap::with_capacity(1),
            cache: HashMap::new(),
        };

        new.add_document("", serde_json::to_value(root).unwrap());

        if let Some(components) = &root.components {
            new.cache_components(components);
        }

        new
    }

    pub fn add_document(
        &mut self,
        url: impl Into<String>,
        document: impl IntoCow<'doc, serde_json::Value>,
    ) {
        self.documents.insert(url.into(), document.into_cow());
    }

    /// Ignores errors; they will resurface in `resolve`.
    pub fn cache_components(&mut self, components: &'doc Components) {
        macro_rules! cache {
            ($field:ident, $ref_infix:literal) => {
                components.$field.iter().filter_map(|(name, ref_or)| {
                    let object = match ref_or {
                        ReferenceOr::Reference { reference } => {
                            Cow::Owned(Self::resolve_(reference, &self.documents).ok()?)
                        }
                        ReferenceOr::Item(object) => Cow::Borrowed(object),
                    };
                    Some((
                        format!("#/components/{}/{}", $ref_infix, name),
                        object.into(),
                    ))
                })
            };
        }

        let entries = std::iter::empty()
            .chain(cache!(schemas, "schemas"))
            .chain(cache!(responses, "responses"))
            .chain(cache!(parameters, "parameters"))
            .chain(cache!(examples, "examples"))
            .chain(cache!(request_bodies, "requestBodies"))
            .chain(cache!(headers, "headers"))
            .chain(cache!(security_schemes, "securitySchemes"))
            .chain(cache!(links, "links"))
            .chain(cache!(callbacks, "callbacks"))
            .collect::<Vec<_>>();

        self.cache.extend(entries);
    }

    pub fn resolve<'a, O>(&'a mut self, ref_or: &'a ReferenceOr<O>) -> Result<&'a O, Error>
    where
        O: serde::de::DeserializeOwned,
        &'a O: TryFrom<&'a Component<'doc>>,
        Component<'doc>: From<O>,
    {
        match ref_or {
            ReferenceOr::Reference { reference } => {
                // <https://docs.rs/polonius-the-crab/latest/polonius_the_crab/#explanation>
                if self.cache.contains_key(reference) {
                    let cached = self.cache.get(reference).unwrap();
                    cached.try_into().map_err(|_| Error::TypeMismatch {
                        reference: reference.clone(),
                        found: cached.kind(),
                        expected: std::any::type_name::<O>(),
                    })
                } else {
                    let object = Self::resolve_::<O>(reference, &self.documents)?;
                    self.cache.insert(reference.clone(), object.into());
                    let cached = self.cache.get(reference).unwrap();
                    Ok(cached.try_into().unwrap_or_else(|_| unreachable!()))
                }
            }
            ReferenceOr::Item(item) => Ok(item),
        }
    }

    /// Private, bypasses the cache.
    fn resolve_<O>(
        reference: &str,
        documents: &HashMap<String, Cow<'doc, serde_json::Value>>,
    ) -> Result<O, Error>
    where
        O: serde::de::DeserializeOwned,
    {
        let (url, pointer) = match reference.split_once('#') {
            Some((url, fragment)) => (url, fragment),
            None => (reference, ""),
        };

        let document = documents
            .get(url)
            .ok_or_else(|| Error::DocumentNotFound(url.to_string()))?;

        let component = document
            .pointer(pointer)
            .ok_or_else(|| Error::InvalidReference(reference.to_owned()))?;

        serde_json::from_value(component.clone()).map_err(|e| Error::Deserialize {
            reference: reference.to_owned(),
            source: e,
        })
    }
}

impl Component<'_> {
    fn kind(&self) -> &'static str {
        match self {
            Component::Schema(_) => "Schema",
            Component::Response(_) => "Response",
            Component::Parameter(_) => "Parameter",
            Component::Example(_) => "Example",
            Component::RequestBody(_) => "RequestBody",
            Component::Header(_) => "Header",
            Component::SecurityScheme(_) => "SecurityScheme",
            Component::Link(_) => "Link",
            Component::Callback(_) => "Callback",
            Component::Other(_) => "Other",
        }
    }
}

macro_rules! impl_component_variant {
    ($Variant:ident <-> $Type:ty) => {
        impl From<$Type> for Component<'_> {
            fn from(other: $Type) -> Self {
                Self::$Variant(Cow::Owned(other))
            }
        }

        impl<'a> From<&'a $Type> for Component<'a> {
            fn from(other: &'a $Type) -> Self {
                Self::$Variant(Cow::Borrowed(other))
            }
        }

        impl<'a> From<Cow<'a, $Type>> for Component<'a> {
            fn from(other: Cow<'a, $Type>) -> Self {
                Self::$Variant(other)
            }
        }

        impl<'c> TryFrom<&'c Component<'_>> for &'c $Type {
            type Error = ();

            fn try_from(other: &'c Component<'_>) -> Result<Self, Self::Error> {
                match other {
                    Component::$Variant(cow) => Ok(cow),
                    _ => Err(()),
                }
            }
        }

        impl<'a> TryFrom<Component<'a>> for Cow<'a, $Type> {
            type Error = ();

            fn try_from(other: Component<'a>) -> Result<Self, Self::Error> {
                match other {
                    Component::$Variant(cow) => Ok(cow),
                    _ => Err(()),
                }
            }
        }
    };
}

impl_component_variant!(Schema <-> openapiv3::Schema);
impl_component_variant!(Response <-> openapiv3::Response);
impl_component_variant!(Parameter <-> openapiv3::Parameter);
impl_component_variant!(Example <-> openapiv3::Example);
impl_component_variant!(RequestBody <-> openapiv3::RequestBody);
impl_component_variant!(Header <-> openapiv3::Header);
impl_component_variant!(SecurityScheme <-> openapiv3::SecurityScheme);
impl_component_variant!(Link <-> openapiv3::Link);
impl_component_variant!(Callback <-> openapiv3::Callback);
impl_component_variant!(Other <-> serde_json::Value);
