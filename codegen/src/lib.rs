mod into_cow;
mod resolver;

use openapiv3::OpenAPI;
use proc_macro2::TokenStream;
use quote::quote;

pub(crate) use into_cow::IntoCow;
pub(crate) use resolver::ReferenceResolver;

#[derive(Debug, Default)]
pub struct Settings {}

#[derive(Debug)]
pub struct Generator<'a> {
    #[expect(unused)]
    spec: &'a OpenAPI,
    #[expect(unused)]
    settings: &'a Settings,
    #[expect(unused)]
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
}

pub fn generate_openapi(_spec: &OpenAPI, _settings: &Settings) -> anyhow::Result<TokenStream> {
    Ok(quote! {})
}
