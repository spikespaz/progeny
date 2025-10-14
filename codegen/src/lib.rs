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
pub struct Settings {}

#[derive(Debug)]
pub struct Generator<'a> {
    spec: &'a OpenAPI,
    #[expect(unused)]
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
                let fn_suffix = match &op.operation_id {
                    Some(op_id) => op_id.to_snake_case(),
                    None => template.to_snake_case(),
                };
                let fn_name = format_ident!("{method}_{fn_suffix}");
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
