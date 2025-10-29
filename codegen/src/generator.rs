use std::borrow::Cow;
use std::rc::Rc;

use indexmap::IndexSet;
use mediatype::MediaType;
use openapiv3::{
    MediaType as ContentObject, OpenAPI, Parameter, ParameterSchemaOrContent as ParameterFormat,
    Schema,
};
use proc_macro2::TokenStream;
use quote::{ToTokens, format_ident, quote};
use slotmap::SecondaryMap;
use syn::parse_quote;

use self::stage::{Building, Finished, Prepared};
use crate::IntoCow;
use crate::formatting::{name_from_reference, to_snake_ident};
use crate::macros::static_json;
use crate::openapi_ext::ReferenceOrExt as _;
use crate::resolver::ReferenceResolver;
use crate::type_model::kinds::{FloatKind, IntegerKind, Scalar, Sequence};
use crate::type_model::{TypeGraph, TypeId, TypeKind};
use crate::type_names::TypeNameTable;

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
    use slotmap::SecondaryMap;

    use crate::type_model::{TypeGraph, TypeId};
    use crate::type_names::TypeNameTable;

    pub trait GeneratorStage: crate::Sealed {}

    /// The [`Generator`][super::Generator] is still in construction phase,
    /// where builder methods are accessible.
    pub struct Building(pub(crate) ());

    /// The [`Generator`][super::Generator] has been fully constructed
    /// and is ready to produce tokens.
    pub struct Prepared<'cx> {
        pub(crate) types: TypeGraph<'cx>,
        pub(crate) type_names: TypeNameTable,
        pub(crate) type_defs: SecondaryMap<TypeId, syn::Item>,
        pub(crate) alias_defs: Vec<syn::ItemType>,
        pub(crate) fn_defs: Vec<syn::ItemFn>,
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
                type_names: TypeNameTable::default(),
                type_defs: SecondaryMap::new(),
                alias_defs: Vec::new(),
                fn_defs: Vec::new(),
            },
            spec: self.spec,
            settings: self.settings,
            resolver: self.resolver,
        }
    }
}

impl<'cx> Generator<'cx, Prepared<'cx>> {
    pub fn run(mut self) -> anyhow::Result<Generator<'cx, Finished<'cx>>> {
        let mut needed_types = IndexSet::new();

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

                    let (type_id, reference, schema) = match &param_data.format {
                        ParameterFormat::Schema(ref_or) => {
                            let (component_id, schema) = self.resolver.resolve(ref_or)?;
                            let type_id = self.state.types.intern_schema(component_id, &schema)?;

                            (type_id, ref_or.as_reference(), schema)
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
                            let (type_id, schema) =
                                self.intern_content_schema(&media_type, object)?;

                            (type_id, None, schema)
                        }
                    };

                    let schema_title = schema.schema_data.title.as_deref();
                    let reference_name = || reference.map(|s| name_from_reference(s).unwrap());
                    let canon_name = schema_title.map(Cow::Borrowed).or_else(reference_name);
                    let alias_name = &param_data.name;

                    if let Some(name) = canon_name {
                        self.state.type_names.set_canonical(type_id, name);
                    } else {
                        self.state.type_names.set_canonical(type_id, alias_name);
                    }

                    self.visit_type(type_id);

                    let arg_type = self.render_type(type_id, Some(alias_name));
                    let arg_type = if !param_data.required {
                        quote!(::core::option::Option<#arg_type>)
                    } else {
                        arg_type.into_token_stream()
                    };

                    match &*param {
                        Parameter::Path {
                            parameter_data: _,
                            style,
                        } => {
                            needed_types.insert(type_id);
                            path_args.push(quote!(#arg_name: #arg_type));
                        }
                        Parameter::Query {
                            parameter_data: _,
                            allow_reserved,
                            style,
                            allow_empty_value,
                        } => {
                            needed_types.insert(type_id);
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

                if let Some(ref_or) = &op.request_body {
                    let (_, body) = self.resolver.resolve(ref_or)?;

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
                    let reference = ref_or.as_reference();

                    let schema_title = schema.schema_data.title.as_deref();
                    let reference_name = || reference.map(|s| name_from_reference(s).unwrap());
                    let canon_name = schema_title.map(Cow::Borrowed).or_else(reference_name);
                    let alias_name = &op_name;

                    if let Some(name) = canon_name {
                        self.state.type_names.set_canonical(type_id, name);
                    } else {
                        self.state.type_names.set_canonical(type_id, alias_name);
                    }

                    self.visit_type(type_id);

                    let arg_type = self.render_type(type_id, Some(alias_name));
                    let arg_type = if !body.required {
                        quote!(::core::option::Option<#arg_type>)
                    } else {
                        arg_type.into_token_stream()
                    };

                    needed_types.insert(type_id);
                    body_arg = Some(quote!(#arg_name: #arg_type));
                }

                let fn_args = std::iter::empty()
                    .chain(path_args)
                    .chain(query_args)
                    .chain(body_arg);

                self.state.fn_defs.push(parse_quote! {
                    pub fn #fn_name(#(#fn_args,)*) {
                        todo!()
                    }
                });
            }
        }

        for type_id in needed_types {
            let canon_ident = self.state.type_names.ident_for(type_id);
            for alias in self.state.type_names.aliases_for(type_id) {
                self.state.alias_defs.push(parse_quote! {
                    pub type #alias = #canon_ident;
                });
            }
        }

        let tokens = {
            let type_defs = self.state.type_defs.values();
            let alias_defs = self.state.alias_defs;
            let fn_defs = self.state.fn_defs;

            quote! {
                #(#type_defs)*
                #(#alias_defs)*
                #(#fn_defs)*
            }
        };

        Ok(Generator {
            state: Finished {
                types: self.state.types,
                tokens,
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
            let schema = default_content_schema(media_type)?;
            let type_id = self.state.types.add_schema(schema).unwrap();

            Ok((type_id, Rc::new(schema.clone())))
        }
    }

    fn is_type_inline(&self, id: TypeId) -> bool {
        matches!(
            self.state.types.get_by_id(id),
            TypeKind::Anything
                | TypeKind::Scalar(_)
                | TypeKind::Sequence(Sequence::List { .. })
                | TypeKind::Sequence(Sequence::Exactly { unique: false, .. })
                | TypeKind::Nullable(_)
                | TypeKind::Uninhabited
        )
    }

    fn visit_type(&mut self, id: TypeId) {
        // TODO: Prevent clone without requiring partial borrow.
        let type_kind = self.state.types.get_by_id(id).clone();
        // TODO: Branches with unused variables have stand-in output to keep
        // the examples compiling.
        let type_def = match type_kind {
            TypeKind::Anything => return,
            TypeKind::Scalar(_) => return,
            TypeKind::Record(record) => {
                let canon_ident = self.ensure_canon_type_ident(id);
                parse_quote! {
                    pub struct #canon_ident {}
                }
            }
            TypeKind::Sequence(sequence) => {
                self.visit_type(sequence.items_type());
                match sequence {
                    Sequence::Exactly {
                        items,
                        count,
                        unique: true,
                    } => {
                        let canon_ident = self.ensure_canon_type_ident(id);
                        parse_quote! {
                            pub struct #canon_ident {}
                        }
                    }
                    Sequence::Bounded {
                        items,
                        min_items,
                        max_items,
                        unique,
                    } => {
                        let canon_ident = self.ensure_canon_type_ident(id);
                        parse_quote! {
                            pub struct #canon_ident {}
                        }
                    }
                    // Anything else is inlined by `render_type`.
                    _ => return,
                }
            }
            TypeKind::Refinement(refinement) => {
                let canon_ident = self.ensure_canon_type_ident(id);
                parse_quote! {
                    pub struct #canon_ident {}
                }
            }
            TypeKind::Nullable(inner_id) => {
                self.visit_type(inner_id);
                return;
            }
            TypeKind::Coproduct(type_ids) => {
                let canon_ident = self.ensure_canon_type_ident(id);
                parse_quote! {
                    pub struct #canon_ident {}
                }
            }
            TypeKind::Intersection(type_ids) => {
                let canon_ident = self.ensure_canon_type_ident(id);
                parse_quote! {
                    pub struct #canon_ident {}
                }
            }
            TypeKind::Union(type_ids) => {
                let canon_ident = self.ensure_canon_type_ident(id);
                parse_quote! {
                    pub struct #canon_ident {}
                }
            }
            TypeKind::Complement(type_id) => {
                let canon_ident = self.ensure_canon_type_ident(id);
                parse_quote! {
                    pub struct #canon_ident {}
                }
            }
            TypeKind::Uninhabited => return,
        };

        self.state.type_defs.insert(id, type_def);
    }

    /// Render the Rust type for a given `TypeId` for use in a function signature
    /// or product type field.
    ///
    /// This will try to inline simple shapes (scalars, lists, etc.).
    /// For complex shapes that must be defined as their own named item
    /// (`struct` or `enum`), this returns an identifier.
    ///
    /// The optional `alias` is used in two ways, when provided:
    /// - If the type is [`TypeKind::Nullable`], the `alias` is used for the generic of `Option`.
    /// - If the type **cannot** be represented inline, the alias will be registered
    ///   as an alias for this `TypeId` and returned as a [`syn::Type`].
    ///
    /// If the [`TypeKind`] can be represented structurally, instead of requiring
    /// a definition item, it will be produced with no alias registration.
    fn render_type(&mut self, id: TypeId, alias: Option<&str>) -> syn::Type {
        match *self.state.types.get_by_id(id) {
            TypeKind::Anything => parse_quote!(::serde_json::Value),
            TypeKind::Scalar(scalar) => scalar_to_tokens(scalar),
            TypeKind::Sequence(Sequence::List { items, unique }) => {
                let item_ty = self.render_type(items, None);
                if unique {
                    parse_quote!(::indexmap::IndexSet<#item_ty>)
                } else {
                    parse_quote!(::std::vec::Vec<#item_ty>)
                }
            }
            TypeKind::Sequence(Sequence::Exactly {
                items,
                count,
                unique: false,
            }) => {
                let item_ty = self.render_type(items, None);
                parse_quote!([#item_ty; #count])
            }
            TypeKind::Nullable(inner_id) => {
                let inner_ty = self.render_type(inner_id, alias);
                parse_quote!(::core::option::Option<#inner_ty>)
            }
            TypeKind::Uninhabited => parse_quote!(!),
            _ => {
                // TODO: For types which are recursed to, but have not been assigned
                // a canonical identifier elsewhere, discover the original reference
                // from which they came, and derive a name from it.
                // For inline/anonymous schemas, probably nested ones, a reference
                // won't be available. In this event, synthesize some reasonable name,
                // if it needs a definition.
                let ident = alias
                    .map(|name| self.state.type_names.insert_alias(id, name))
                    .or_else(|| self.state.type_names.ident_for(id).cloned())
                    .expect("render_type called for type without canonical name");
                parse_quote!(#ident)
            }
        }
    }

    fn ensure_canon_type_ident(&mut self, id: TypeId) -> syn::Ident {
        debug_assert!(
            !self.is_type_inline(id),
            "ensure_canon_type_ident called for an inline/structural type"
        );

        if let Some(existing) = self.state.type_names.ident_for(id) {
            return existing.clone();
        }

        let component_name = self.state.types.get_component(id).and_then(|component_id| {
            let schema = self
                .resolver
                .get_object::<Schema>(component_id)
                .expect("given TypeId must have come from a Schema");

            if let Some(title) = schema.schema_data.title.as_deref() {
                return Some(title.to_string());
            }

            let meta = self.resolver.get_meta(component_id)?;
            let first_ref = meta.references.first()?;
            let ref_name = name_from_reference(first_ref)
                .expect("reference already resolved and must be valid")
                .to_string();

            Some(ref_name)
        });

        if let Some(name) = component_name {
            return self.state.type_names.set_canonical(id, name);
        }

        // Absolute last resort for anonymous inline schemas (with no component).
        // TODO: This could be fancier and synthesize a more descriptive name
        // by the `TypeKind`.
        self.state.type_names.set_canonical(id, "Schema")
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
    use mediatype::media_type as new;

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
        use mediatype::media_type as new;
        use mediatype::names::*;

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

pub fn scalar_to_tokens(scalar: Scalar) -> syn::Type {
    match scalar {
        Scalar::String => parse_quote!(::std::string::String),
        Scalar::Float(FloatKind::F32) => parse_quote!(f32),
        Scalar::Float(FloatKind::F64) => parse_quote!(f64),
        Scalar::Integer(IntegerKind::U8) => parse_quote!(u8),
        Scalar::Integer(IntegerKind::U16) => parse_quote!(u16),
        Scalar::Integer(IntegerKind::U32) => parse_quote!(u32),
        Scalar::Integer(IntegerKind::U64) => parse_quote!(u64),
        Scalar::Integer(IntegerKind::U128) => parse_quote!(u128),
        Scalar::Integer(IntegerKind::I8) => parse_quote!(i8),
        Scalar::Integer(IntegerKind::I16) => parse_quote!(i16),
        Scalar::Integer(IntegerKind::I32) => parse_quote!(i32),
        Scalar::Integer(IntegerKind::I64) => parse_quote!(i64),
        Scalar::Integer(IntegerKind::I128) => parse_quote!(i128),
        Scalar::Boolean => parse_quote!(bool),
    }
}
