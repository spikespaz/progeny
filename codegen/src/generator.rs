use std::borrow::Cow;

use openapiv3::{OpenAPI, Parameter, ParameterData, ParameterSchemaOrContent as ParameterFormat};
use proc_macro2::TokenStream;
use quote::{ToTokens, format_ident, quote};
use syn::parse_quote;

use self::stage::{Building, Finished, Prepared};
use crate::IntoCow;
use crate::formatting::{to_snake_ident, to_type_ident};
use crate::resolver::{Error as ResolveError, ReferenceResolver};
use crate::type_model::TypeGraph;

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
    pub struct Prepared<'a> {
        pub(crate) types: TypeGraph<'a>,
        pub(crate) tokens: TokenStream,
    }

    /// The [`Generator`][super::Generator] has tokens ready for ingestion.
    pub struct Finished<'a> {
        pub(crate) types: TypeGraph<'a>,
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
pub struct Generator<'a, STAGE: stage::GeneratorStage> {
    state: STAGE,
    spec: &'a OpenAPI,
    settings: &'a Settings,
    resolver: ReferenceResolver<'a>,
}

impl Generator<'_, ()> {
    #[must_use]
    pub fn new<'a>(spec: &'a OpenAPI, settings: &'a Settings) -> Generator<'a, Building> {
        Generator {
            state: Building(()),
            spec,
            settings,
            resolver: ReferenceResolver::new(spec),
        }
    }
}

impl<'a> Generator<'a, Building> {
    pub fn add_document(
        &mut self,
        url: impl Into<String>,
        document: impl IntoCow<'a, serde_json::Value>,
    ) {
        self.resolver.add_document(url, document);
    }

    #[must_use]
    pub fn build(self) -> Generator<'a, Prepared<'a>> {
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

impl<'a> Generator<'a, Prepared<'a>> {
    pub fn run(mut self) -> anyhow::Result<Generator<'a, Finished<'a>>> {
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
                    let arg_type = resolve_param_type(param_data, &mut self.resolver)?;

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
}

impl Generator<'_, Finished<'_>> {
    pub fn types(&self) -> &TypeGraph<'_> {
        &self.state.types
    }

    pub fn take_tokens(&mut self) -> TokenStream {
        std::mem::take(&mut self.state.tokens)
    }
}

fn resolve_param_type(
    parameter_data: &ParameterData,
    resolver: &mut ReferenceResolver,
) -> Result<syn::Type, ResolveError> {
    let ident = match &parameter_data.format {
        ParameterFormat::Schema(ref_or) => {
            let (_component_id, schema) = resolver.resolve(ref_or)?;
            let schema_title = schema.schema_data.title.as_deref();
            to_type_ident(schema_title.unwrap_or(&parameter_data.name))
        }
        ParameterFormat::Content(_index_map) => to_type_ident(&parameter_data.name),
    };

    let mut param_type = ident.into_token_stream();

    if !parameter_data.required {
        param_type = quote!(::core::option::Option<#param_type>);
    }

    Ok(parse_quote!(#param_type))
}
