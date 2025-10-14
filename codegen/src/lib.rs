mod box_or_ref;
mod into_cow;
mod resolver;

use heck::ToSnakeCase;
use openapiv3::OpenAPI;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

pub(crate) use into_cow::IntoCow;
pub(crate) use resolver::ReferenceResolver;

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
            let path = self.resolver.resolve(path)?;
            for (method, op) in path.iter() {
                let fn_name = {
                    let mut name = match &op.operation_id {
                        Some(op_id) => op_id.to_snake_case(),
                        None => template.to_snake_case(),
                    };
                    if self.settings.prefix_operations {
                        name = format!("{method}_{name}")
                    }
                    format_ident!("{name}")
                };
                tokens.extend(quote! {
                    pub fn #fn_name() {
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
