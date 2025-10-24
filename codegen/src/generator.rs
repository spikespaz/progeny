use std::borrow::Cow;

use openapiv3::{OpenAPI, Parameter};
use proc_macro2::TokenStream;
use quote::quote;

use crate::IntoCow;
use crate::formatting::to_snake_ident;
use crate::resolver::ReferenceResolver;

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
                    match &*param {
                        Parameter::Path {
                            parameter_data,
                            style,
                        } => {
                            let arg_name = to_snake_ident(&parameter_data.name);
                            path_args.push(quote!(#arg_name: ()));
                        }
                        Parameter::Query {
                            parameter_data,
                            allow_reserved,
                            style,
                            allow_empty_value,
                        } => {
                            if parameter_data.required {
                                let arg_name = to_snake_ident(&parameter_data.name);
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
