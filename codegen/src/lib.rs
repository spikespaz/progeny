mod box_or_ref;
mod into_cow;
mod resolver;
mod type_def;
mod type_model;
mod type_ref;

use std::ops::Deref as _;

use convert_case::{Case, Casing};
use openapiv3::{OpenAPI, Parameter};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

pub(crate) use crate::__sealed::Sealed;
pub(crate) use crate::into_cow::IntoCow;
pub(crate) use crate::resolver::ReferenceResolver;

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
                let fn_name = {
                    let mut name = match &op.operation_id {
                        Some(op_id) => op_id.to_case(Case::Snake),
                        None => template.to_case(Case::Snake),
                    };
                    if self.settings.prefix_operations {
                        name = format!("{method}_{name}")
                    }
                    format_ident!("{name}")
                };

                let mut path_args = Vec::new();
                let mut query_args = Vec::new();
                for param in &op.parameters {
                    let (_, param) = self.resolver.resolve(param)?;
                    match param.deref() {
                        Parameter::Path {
                            parameter_data,
                            style,
                        } => {
                            let arg_name = parameter_data.name.to_case(Case::Snake);
                            let arg_name = format_ident!("{arg_name}");
                            path_args.push(quote!(#arg_name: ()));
                        }
                        Parameter::Query {
                            parameter_data,
                            allow_reserved,
                            style,
                            allow_empty_value,
                        } => {
                            if parameter_data.required {
                                let arg_name = parameter_data.name.to_case(Case::Snake);
                                let arg_name = format_ident!("{arg_name}");
                                query_args.push(quote!(#arg_name: ()));
                            } else {
                                eprintln!("optional query parameters to be in a type")
                            }
                        }
                        Parameter::Header {
                            parameter_data,
                            style,
                        } => eprintln!("header parameters not yet implemented"),
                        Parameter::Cookie {
                            parameter_data,
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

pub fn generate_openapi(spec: &OpenAPI, settings: &Settings) -> anyhow::Result<TokenStream> {
    let mut generator = Generator::new(spec, settings);
    generator.run()
}

#[doc(hidden)]
mod __sealed {
    pub trait Sealed {}
}
