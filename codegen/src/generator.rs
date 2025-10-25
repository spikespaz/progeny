use std::borrow::Cow;

use openapiv3::{OpenAPI, Parameter, ParameterData, ParameterSchemaOrContent as ParameterFormat};
use proc_macro2::TokenStream;
use quote::{ToTokens, quote};
use syn::parse_quote;

use crate::IntoCow;
use crate::formatting::{to_snake_ident, to_type_ident};
use crate::resolver::{Error as ResolveError, ReferenceResolver};

#[derive(Debug, Default)]
pub struct Settings {
    /// Whether each operation will be prefixed by its HTTP method.
    pub prefix_operations: bool,
}

#[derive(Debug)]
pub struct Generator<'a> {
    spec: &'a OpenAPI,
    settings: &'a Settings,
    resolver: ReferenceResolver<'a>,
}

impl<'a> Generator<'a> {
    pub fn new(spec: &'a OpenAPI, settings: &'a Settings) -> Self {
        Self {
            spec,
            settings,
            resolver: ReferenceResolver::new(spec),
        }
    }

    pub fn add_document(
        &mut self,
        url: impl Into<String>,
        document: impl IntoCow<'a, serde_json::Value>,
    ) {
        self.resolver.add_document(url, document);
    }

    pub fn run(&mut self) -> anyhow::Result<TokenStream> {
        let mut tokens = TokenStream::new();

        for (template, path) in &self.spec.paths.paths {
            let (_, path) = self.resolver.resolve(path)?;
            for (method, op) in path.iter() {
                let fn_name = to_snake_ident(match &op.operation_id {
                    Some(op_id) if self.settings.prefix_operations => {
                        Cow::Owned(format!("{method}_{op_id}"))
                    }
                    Some(op_id) => Cow::Borrowed(op_id.as_ref()),
                    None => Cow::Owned(format!("{method}_{template}")),
                });

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

                let fn_args = std::iter::empty().chain(path_args).chain(query_args);

                tokens.extend(quote! {
                    pub fn #fn_name(#(#fn_args,)*) {
                        todo!()
                    }
                });
            }
        }

        Ok(tokens)
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
