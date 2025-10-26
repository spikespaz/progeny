use progeny_codegen::{Generator, Settings};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let spec = serde_json::from_str(include_str!("./spec/openapi.json"))?;
    let settings = Settings::default();
    let generator = Generator::new(&spec, &settings);
    let tokens = generator.build().run()?;
    let tokens = prettyplease::unparse(&syn::parse2(tokens)?);
    std::fs::write(format!("{}/openapi.rs", std::env::var("OUT_DIR")?), &tokens)?;
    Ok(())
}
