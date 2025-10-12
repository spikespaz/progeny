use openapiv3::OpenAPI;
use proc_macro2::TokenStream;
use quote::quote;

#[derive(Default)]
pub struct Settings {}

pub fn generate_openapi(_spec: &OpenAPI, _settings: &Settings) -> anyhow::Result<TokenStream> {
    Ok(quote! {})
}
