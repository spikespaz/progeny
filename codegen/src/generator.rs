use std::borrow::Cow;
use std::rc::Rc;

use indexmap::IndexMap;
use openapiv3::{Content, OpenAPI, Parameter, ParameterSchemaOrContent as ParameterFormat, Schema};
use proc_macro2::TokenStream;
use quote::{ToTokens, format_ident, quote};

use self::stage::{Building, Finished, Prepared};
use crate::IntoCow;
use crate::formatting::{to_snake_ident, to_type_ident};
use crate::macros::static_json;
use crate::resolver::ReferenceResolver;
use crate::type_model::{TypeGraph, TypeId};

#[derive(Debug, Default)]
pub struct Settings {
    /// Whether each operation will be prefixed by its HTTP method.
    pub prefix_operations: bool,
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
                            let content_schemas = self.intern_content_schemas(content)?;
                            let (1, Some((_media_type, (type_id, schema)))) =
                                (content_schemas.len(), content_schemas.into_iter().next())
                            else {
                                anyhow::bail!(
                                    "parameter '{}' must have exactly one content type",
                                    param_data.name
                                );
                            };

                            (type_id, schema)
                        }
                    };

                    let arg_type = {
                        let schema_title = schema.schema_data.title.as_deref();
                        let name = schema_title.unwrap_or(param_data.name.as_str());
                        let arg_type = to_type_ident(name).into_token_stream();
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
                    let mut arg_type = to_type_ident(&op_name).into_token_stream();

                    if !body.required {
                        arg_type = quote!(::core::option::Option<#arg_type>);
                    }

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

    fn intern_content_schemas<'a>(
        &mut self,
        content: &'a Content,
    ) -> anyhow::Result<IndexMap<&'a str, (TypeId, Rc<Schema>)>> {
        let mut content_schemas = IndexMap::new();

        for (media_type, object) in content {
            let media_type = media_type.as_str();

            let (type_id, schema) = if let Some(ref_or) = &object.schema {
                let (component_id, schema) = self.resolver.resolve(ref_or)?;
                let type_id = self.state.types.intern_schema(component_id, &schema)?;

                (type_id, schema)
            } else {
                let schema = default_content_schema(media_type)?;
                let type_id = self.state.types.add_schema(schema).unwrap();

                (type_id, Rc::new(schema.clone()))
            };

            content_schemas.insert(media_type, (type_id, schema));
        }

        Ok(content_schemas)
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

fn default_content_schema(media_type: &str) -> anyhow::Result<&'static Schema> {
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
        static SCHEMA_ANYTHING_RECORD: Schema = {
            "type": "object",
            "additionalProperties": true
        };
    };

    let schema = match media_type {
        "text/plain" => &SCHEMA_PLAIN_STRING,
        "application/json" => &SCHEMA_ANYTHING,
        "application/octet-stream" => &SCHEMA_BINARY_STRING,
        "application/x-www-form-urlencoded" => &SCHEMA_STRING_RECORD,
        "multipart/form-data" => &SCHEMA_ANYTHING_RECORD,
        _ => anyhow::bail!("unknown media type with no schema: {media_type}"),
    };

    Ok(schema)
}
