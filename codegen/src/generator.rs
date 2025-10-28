use std::borrow::Cow;
use std::rc::Rc;

use indexmap::{IndexMap, IndexSet};
use mediatype::MediaType;
use openapiv3::{
    MediaType as ContentObject, OpenAPI, Parameter, ParameterSchemaOrContent as ParameterFormat,
    Schema,
};
use proc_macro2::TokenStream;
use quote::{ToTokens, format_ident, quote};

use self::stage::{Building, Finished, Prepared};
use crate::IntoCow;
use crate::formatting::{to_snake_ident, to_type_ident};
use crate::macros::static_json;
use crate::resolver::ReferenceResolver;
use crate::type_model::{TypeGraph, TypeId};

#[derive(Debug)]
pub struct Settings {
    /// Whether each operation will be prefixed by its HTTP method.
    pub prefix_operations: bool,
    /// Media type classifications to choose among a body [`Content`][openapiv3::Content]
    /// with multiple objects, in order of descending preference.
    pub content_preference: &'static [MediaClass],
}

impl Default for Settings {
    fn default() -> Self {
        Self {
            prefix_operations: false,
            content_preference: {
                use MediaClass::*;
                &[Json, FormUrlEncoded, MultipartForm, PlainText, OpaqueBytes]
            },
        }
    }
}

pub mod stage {
    use proc_macro2::TokenStream;

    use crate::type_model::TypeGraph;

    pub trait GeneratorStage: crate::Sealed {}

    /// The [`Generator`][super::Generator] is still in construction phase,
    /// where builder methods are accessible.
    pub struct Building(pub(crate) ());

    /// The [`Generator`][super::Generator] has been fully constructed
    /// and is ready to produce tokens.
    pub struct Prepared<'cx> {
        pub(crate) types: TypeGraph<'cx>,
        pub(crate) tokens: TokenStream,
    }

    /// The [`Generator`][super::Generator] has tokens ready for ingestion.
    pub struct Finished<'cx> {
        pub(crate) types: TypeGraph<'cx>,
        pub(crate) tokens: TokenStream,
    }

    const _: () = {
        impl crate::Sealed for Building {}
        impl crate::Sealed for Prepared<'_> {}
        impl crate::Sealed for Finished<'_> {}

        impl GeneratorStage for () {}
        impl GeneratorStage for Building {}
        impl GeneratorStage for Prepared<'_> {}
        impl GeneratorStage for Finished<'_> {}
    };
}

/// The entry-point for the crate; a stateful generator to read an
/// OpenAPI 3.0.x specification, generate Rust types from schemas, and builder
/// functions for HTTP requests.
///
/// # Example `build.rs`
///
/// ```rust
#[doc = include_str!("../../examples/petstore/build.rs")]
/// ```
#[derive(Debug)]
pub struct Generator<'cx, STAGE: stage::GeneratorStage> {
    state: STAGE,
    spec: &'cx OpenAPI,
    settings: &'cx Settings,
    resolver: ReferenceResolver<'cx>,
}

impl Generator<'_, ()> {
    #[must_use]
    pub fn new<'cx>(spec: &'cx OpenAPI, settings: &'cx Settings) -> Generator<'cx, Building> {
        Generator {
            state: Building(()),
            spec,
            settings,
            resolver: ReferenceResolver::new(spec),
        }
    }
}

impl<'cx> Generator<'cx, Building> {
    pub fn add_document(
        &mut self,
        url: impl Into<String>,
        document: impl IntoCow<'cx, serde_json::Value>,
    ) {
        self.resolver.add_document(url, document);
    }

    #[must_use]
    pub fn build(self) -> Generator<'cx, Prepared<'cx>> {
        let types = TypeGraph::new(self.resolver.clone());
        Generator {
            state: Prepared {
                types,
                tokens: TokenStream::new(),
            },
            spec: self.spec,
            settings: self.settings,
            resolver: self.resolver,
        }
    }
}

impl<'cx> Generator<'cx, Prepared<'cx>> {
    pub fn run(mut self) -> anyhow::Result<Generator<'cx, Finished<'cx>>> {
        let mut type_names = IndexMap::<TypeId, IndexSet<String>>::new();

        let insert_type_alias = &mut |type_id, name| {
            use indexmap::map::Entry;
            match type_names.entry(type_id) {
                Entry::Occupied(mut entry) => {
                    entry.get_mut().insert(name);
                }
                Entry::Vacant(slot) => {
                    let names = IndexSet::<_>::from_iter([name]);
                    slot.insert(names);
                }
            }
        };

        for (template, path) in &self.spec.paths.paths {
            let (_, path) = self.resolver.resolve(path)?;
            for (method, op) in path.iter() {
                let op_name = match &op.operation_id {
                    Some(op_id) if self.settings.prefix_operations => {
                        Cow::Owned(format!("{method}_{op_id}"))
                    }
                    Some(op_id) => Cow::Borrowed(op_id.as_ref()),
                    None => Cow::Owned(format!("{method}_{template}")),
                };

                let fn_name = to_snake_ident(&op_name);

                let mut path_args = Vec::new();
                let mut query_args = Vec::new();

                for param in &op.parameters {
                    let (_, param) = self.resolver.resolve(param)?;
                    let param_data = param.parameter_data_ref();

                    let arg_name = to_snake_ident(&param_data.name);

                    let (type_id, schema) = match &param_data.format {
                        ParameterFormat::Schema(ref_or) => {
                            let (component_id, schema) = self.resolver.resolve(ref_or)?;
                            let type_id = self.state.types.intern_schema(component_id, &schema)?;

                            (type_id, schema)
                        }
                        ParameterFormat::Content(content) => {
                            let (1, Some((media_type, object))) =
                                (content.len(), content.into_iter().next())
                            else {
                                anyhow::bail!(
                                    "parameter '{}' must have exactly one content type",
                                    param_data.name
                                );
                            };
                            let media_type = MediaType::parse(media_type).map_err(|e| {
                                anyhow::anyhow!("invalid media type '{media_type}': {e}")
                            })?;
                            self.intern_content_schema(&media_type, object)?
                        }
                    };

                    let arg_type = {
                        let schema_title = schema.schema_data.title.as_deref();
                        let name = schema_title.unwrap_or(param_data.name.as_str());
                        let arg_type = to_type_ident(name).into_token_stream();

                        insert_type_alias(type_id, arg_type.to_string());

                        if !param_data.required {
                            quote!(::core::option::Option<#arg_type>)
                        } else {
                            arg_type
                        }
                    };

                    match &*param {
                        Parameter::Path {
                            parameter_data: _,
                            style,
                        } => {
                            path_args.push(quote!(#arg_name: #arg_type));
                        }
                        Parameter::Query {
                            parameter_data: _,
                            allow_reserved,
                            style,
                            allow_empty_value,
                        } => {
                            query_args.push(quote!(#arg_name: #arg_type));
                        }
                        Parameter::Header {
                            parameter_data: _,
                            style,
                        } => eprintln!("header parameters not yet implemented"),
                        Parameter::Cookie {
                            parameter_data: _,
                            style,
                        } => eprintln!("cookie parameters not yet implemented"),
                    }
                }

                let mut body_arg = None;

                if let Some(body) = &op.request_body {
                    let (_, body) = self.resolver.resolve(body)?;

                    // TODO: Proper conflict resolution.
                    let arg_name = format_ident!("{method}_body");

                    let Some((_, (media_type, object))) = body
                        .content
                        .iter()
                        .filter_map(|(media_type, object)| {
                            Some((MediaType::parse(media_type).ok()?, object))
                        })
                        .enumerate()
                        .min_by_key(|&(index, (ref media_type, _))| {
                            rank_media_type(media_type, index, self.settings.content_preference)
                        })
                    else {
                        anyhow::bail!(
                            "no useful media type for body of path '{template}' method '{method}'",
                        );
                    };

                    let (type_id, schema) = self.intern_content_schema(&media_type, object)?;

                    let arg_type = {
                        let schema_title = &schema.schema_data.title;
                        let name = schema_title.as_deref().unwrap_or(op_name.as_ref());
                        let arg_type = to_type_ident(name).into_token_stream();

                        insert_type_alias(type_id, arg_type.to_string());

                        if !body.required {
                            quote!(::core::option::Option<#arg_type>)
                        } else {
                            arg_type
                        }
                    };

                    body_arg = Some(quote!(#arg_name: #arg_type));
                }

                let fn_args = std::iter::empty()
                    .chain(path_args)
                    .chain(query_args)
                    .chain(body_arg);

                self.state.tokens.extend(quote! {
                    pub fn #fn_name(#(#fn_args,)*) {
                        todo!()
                    }
                });
            }
        }

        Ok(Generator {
            state: Finished {
                types: self.state.types,
                tokens: self.state.tokens,
            },
            spec: self.spec,
            settings: self.settings,
            resolver: self.resolver,
        })
    }

    fn intern_content_schema(
        &mut self,
        media_type: &MediaType<'_>,
        object: &ContentObject,
    ) -> anyhow::Result<(TypeId, Rc<Schema>)> {
        if let Some(ref_or) = &object.schema {
            let (component_id, schema) = self.resolver.resolve(ref_or)?;
            let type_id = self.state.types.intern_schema(component_id, &schema)?;

            Ok((type_id, schema))
        } else {
            let schema = default_content_schema(&media_type)?;
            let type_id = self.state.types.add_schema(schema).unwrap();

            Ok((type_id, Rc::new(schema.clone())))
        }
    }
}

impl Generator<'_, Finished<'_>> {
    pub fn types(&self) -> &TypeGraph<'_> {
        &self.state.types
    }

    pub fn take_tokens(&mut self) -> TokenStream {
        std::mem::take(&mut self.state.tokens)
    }
}

fn rank_media_type(
    media_type: &MediaType<'_>,
    index: usize,
    preference: &[MediaClass],
) -> impl Ord + use<> {
    use crate::macros::media_type as new;

    let media_class = MediaClass::from(media_type);
    let essence = media_type.essence();

    let base_ord = preference
        .iter()
        .position(|&pref| pref == media_class)
        .unwrap_or(usize::MAX);

    let tie_break = match media_class {
        MediaClass::Json if essence == new!(APPLICATION / JSON) => 0,
        MediaClass::Json if essence == new!(TEXT / JSON) => 1,
        MediaClass::Json => 2,
        _ => 0_usize,
    };

    // Prefer fewer parameters, since they may be disrespected.
    let params_len = media_type.params.len();

    // Relies on tuple lexicographic ordering.
    (base_ord, tie_break, params_len, index)
}

fn default_content_schema(media_type: &MediaType<'_>) -> anyhow::Result<&'static Schema> {
    static_json! {
        static SCHEMA_ANYTHING: Schema = {};
        static SCHEMA_PLAIN_STRING: Schema = {
            "type": "string"
        };
        static SCHEMA_BINARY_STRING: Schema = {
            "type": "string",
            "format": "binary"
        };
        static SCHEMA_STRING_RECORD: Schema = {
            "type": "object",
            "additionalProperties": { "type": "string" }
        };
        static SCHEMA_MULTIPART_RECORD: Schema = {
            "type": "object",
            "additionalProperties": {
                "oneOf": [
                    { "type": "string" },
                    { "type": "string", "format": "binary" }
                ]
            }
        };
    };

    let schema = match MediaClass::from(media_type) {
        MediaClass::Json => &SCHEMA_ANYTHING,
        MediaClass::PlainText => &SCHEMA_PLAIN_STRING,
        MediaClass::OpaqueBytes => &SCHEMA_BINARY_STRING,
        MediaClass::FormUrlEncoded => &SCHEMA_STRING_RECORD,
        MediaClass::MultipartForm => &SCHEMA_MULTIPART_RECORD,
        MediaClass::Unknown => anyhow::bail!("unknown media type with no schema: {media_type}"),
    };

    Ok(schema)
}

/// The classification of a [`MediaType`][mediatype::MediaType].
///
/// This would typically come from [`Content`].
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum MediaClass {
    /// `application/json`, `text/json`, or `*+json`.
    Json,
    /// `text/plain`
    PlainText,
    /// `application/octet-stream`
    OpaqueBytes,
    /// `application/x-www-form-urlencoded`
    FormUrlEncoded,
    /// `multipart/form-data`
    MultipartForm,
    /// The provided media type could not be classified by this crate.
    Unknown,
}

impl From<&MediaType<'_>> for MediaClass {
    fn from(other: &MediaType) -> Self {
        use mediatype::names::*;

        use crate::macros::media_type as new;

        let essence = other.essence();

        if essence == new!(APPLICATION / JSON)
            || essence == new!(TEXT / JSON)
            || essence.suffix == Some(JSON)
        {
            Self::Json
        } else if essence == new!(TEXT / PLAIN) {
            Self::PlainText
        } else if essence == new!(APPLICATION / OCTET_STREAM) {
            Self::OpaqueBytes
        } else if essence == new!(APPLICATION / x_::WWW_FORM_URLENCODED) {
            Self::FormUrlEncoded
        } else if essence == new!(MULTIPART / FORM_DATA) {
            Self::MultipartForm
        } else {
            Self::Unknown
        }
    }
}
