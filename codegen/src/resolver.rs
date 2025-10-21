use std::borrow::Cow;
use std::collections::HashMap;
use std::ops::Deref;
use std::rc::Rc;

use openapiv3::{Components, OpenAPI, ReferenceOr};
use percent_encoding::percent_decode_str;
use slotmap::{SlotMap, new_key_type};

use crate::IntoCow;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("failed to decode JSON pointer in reference '{reference}': {source}")]
    DecodePointer {
        reference: String,
        #[source]
        source: std::str::Utf8Error,
    },
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

new_key_type! { pub struct ComponentId; }

#[derive(Debug)]
pub struct ReferenceResolver<'doc> {
    #[expect(unused)]
    root: &'doc OpenAPI,
    /// All documents used for reference resolution, keyed by URL.
    documents: HashMap<String, Cow<'doc, serde_json::Value>>,
    /// All components resolved thus-far.
    cache: SlotMap<ComponentId, Component<'doc>>,
    /// Lookup mapping of reference to component.
    references: HashMap<String, ComponentId>,
}

#[derive(Clone, Debug)]
pub enum Handle<'a, T> {
    Borrowed(&'a T),
    Shared(Rc<T>),
}

#[derive(Clone, Debug)]
pub enum Component<'a> {
    Schema(Handle<'a, openapiv3::Schema>),
    Response(Handle<'a, openapiv3::Response>),
    Parameter(Handle<'a, openapiv3::Parameter>),
    Example(Handle<'a, openapiv3::Example>),
    RequestBody(Handle<'a, openapiv3::RequestBody>),
    Header(Handle<'a, openapiv3::Header>),
    SecurityScheme(Handle<'a, openapiv3::SecurityScheme>),
    Link(Handle<'a, openapiv3::Link>),
    Callback(Handle<'a, openapiv3::Callback>),
    /// "Path Item Object" is not technically a recognized component in OAS 3.0.x,
    /// but the specification still allows for references to them.
    /// This type will be a component in OAS 3.1.0, which warrants an exception presently.
    PathItem(Handle<'a, openapiv3::PathItem>),
    /// The name `Other` might be misleading, this is just yet deserialized.
    Other(Handle<'a, serde_json::Value>),
}

impl<'doc> ReferenceResolver<'doc> {
    pub fn new(root: &'doc OpenAPI) -> Self {
        let mut new = Self {
            root,
            documents: HashMap::with_capacity(1),
            cache: SlotMap::with_key(),
            references: HashMap::new(),
        };

        new.add_document("", serde_json::to_value(root).unwrap());

        if let Some(components) = &root.components {
            new.cache_components("", components);
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

    /// Given a document URL and [`Components`], eagerly resolve and cache.
    ///
    /// For each component, a synthetic reference will be generated with `url`
    /// as the base, and a fragment that points into the OpenAPI *Components Object*.
    ///
    /// The document URL must already have been added by [`add_document`].
    ///
    /// Ignores errors; they will resurface in `resolve`.
    pub fn cache_components(&mut self, url: &str, components: &'doc Components) {
        macro_rules! cache {
            ($field:ident, $ref_infix:literal) => {
                for (name, ref_or) in &components.$field {
                    let handle = match ref_or {
                        ReferenceOr::Reference { reference } => {
                            let Ok(object) = Self::resolve_(reference, &self.documents) else {
                                continue;
                            };
                            Handle::Shared(Rc::new(object))
                        }
                        ReferenceOr::Item(object) => Handle::Borrowed(object),
                    };
                    let id = self.cache.insert(Component::from(handle));

                    let synth_ref = format!("{url}#/components/{}/{name}", $ref_infix);
                    self.references.insert(synth_ref, id);

                    if let ReferenceOr::Reference { reference } = ref_or {
                        self.references.insert(reference.clone(), id);
                    }
                }
            };
        }

        cache!(schemas, "schemas");
        cache!(responses, "responses");
        cache!(parameters, "parameters");
        cache!(examples, "examples");
        cache!(request_bodies, "requestBodies");
        cache!(headers, "headers");
        cache!(security_schemes, "securitySchemes");
        cache!(links, "links");
        cache!(callbacks, "callbacks");
    }

    pub fn resolve<O>(
        &mut self,
        ref_or: &ReferenceOr<O>,
    ) -> Result<(ComponentId, Handle<'doc, O>), Error>
    where
        O: Clone + serde::de::DeserializeOwned,
        Component<'doc>: From<O>,
        Handle<'doc, O>: TryFrom<Component<'doc>>,
    {
        match ref_or {
            ReferenceOr::Reference { reference } => self.resolve_reference(reference),
            // Inline items beget no synthetic references; the returned
            // `ComponentId` is an anchor for future metadata.
            ReferenceOr::Item(object) => {
                let component = Component::from(object.clone());
                let id = self.cache.insert(component.clone());
                Ok((id, component.handle().unwrap()))
            }
        }
    }

    pub fn resolve_reference<O>(
        &mut self,
        reference: &str,
    ) -> Result<(ComponentId, Handle<'doc, O>), Error>
    where
        O: serde::de::DeserializeOwned,
        Component<'doc>: From<O>,
        Handle<'doc, O>: TryFrom<Component<'doc>>,
    {
        let (id, handle) = if let Some(&id) = self.references.get(reference) {
            let cached = self.cache.get_mut(id).unwrap();
            cached.promote::<O>().map_err(|e| Error::Deserialize {
                reference: reference.to_owned(),
                source: e,
            })?;

            let handle = cached.handle().ok_or_else(|| Error::TypeMismatch {
                reference: reference.to_owned(),
                found: cached.kind(),
                expected: std::any::type_name::<O>(),
            })?;

            (id, handle)
        } else {
            let component = Component::from(Self::resolve_(reference, &self.documents)?);
            let handle = component.handle().unwrap();
            let id = self.cache.insert(component);

            (id, handle)
        };

        self.references.insert(reference.to_owned(), id);

        Ok((id, handle))
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
            Some((url, fragment)) => {
                let pointer = percent_decode_str(fragment).decode_utf8().map_err(|e| {
                    Error::DecodePointer {
                        reference: reference.to_owned(),
                        source: e,
                    }
                })?;
                (url, pointer)
            }
            None => (reference, Cow::Borrowed("")),
        };

        let document = documents
            .get(url)
            .ok_or_else(|| Error::DocumentNotFound(url.to_string()))?;

        let component = document
            .pointer(pointer.deref())
            .ok_or_else(|| Error::ComponentNotFound(reference.to_owned()))?;

        serde_json::from_value(component.clone()).map_err(|e| Error::Deserialize {
            reference: reference.to_owned(),
            source: e,
        })
    }
}

impl<T> Deref for Handle<'_, T> {
    type Target = T;

    fn deref(&self) -> &T {
        match *self {
            Handle::Borrowed(borrow) => borrow,
            Handle::Shared(ref rc) => rc.as_ref(),
        }
    }
}

impl<T> From<T> for Handle<'_, T> {
    fn from(other: T) -> Self {
        Handle::Shared(Rc::new(other))
    }
}

impl<'a, T> From<&'a T> for Handle<'a, T> {
    fn from(other: &'a T) -> Self {
        Handle::Borrowed(other)
    }
}

impl<'a> Component<'a> {
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
            Component::PathItem(_) => "PathItem",
            Component::Other(_) => "Other",
        }
    }

    fn handle<O>(&self) -> Option<Handle<'a, O>>
    where
        Handle<'a, O>: TryFrom<Self>,
    {
        Handle::try_from(self.clone()).ok()
    }

    fn promote<O>(&mut self) -> serde_json::Result<&mut Self>
    where
        O: serde::de::DeserializeOwned,
        Self: From<O>,
    {
        if let Self::Other(handle) = &self {
            let object = serde_json::from_value(handle.deref().clone())?;
            *self = Self::from(object);
        }

        Ok(self)
    }
}

macro_rules! impl_component_variant {
    ($Variant:ident <-> $Type:ty) => {
        impl From<$Type> for Component<'_> {
            fn from(other: $Type) -> Self {
                Self::$Variant(Handle::from(other))
            }
        }

        impl<'a> From<&'a $Type> for Component<'a> {
            fn from(other: &'a $Type) -> Self {
                Self::$Variant(Handle::from(other))
            }
        }

        impl<'a> From<Handle<'a, $Type>> for Component<'a> {
            fn from(other: Handle<'a, $Type>) -> Self {
                Self::$Variant(other)
            }
        }

        impl<'a> TryFrom<Component<'a>> for Handle<'a, $Type> {
            type Error = ();

            fn try_from(other: Component<'a>) -> Result<Self, Self::Error> {
                match other {
                    Component::$Variant(handle) => Ok(handle.clone()),
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
impl_component_variant!(PathItem <-> openapiv3::PathItem);
impl_component_variant!(Other <-> serde_json::Value);
