use progeny_codegen::{Generator, Settings};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let spec = serde_json::from_str(include_str!("./spec/openapi.json"))?;
    let settings = Settings::default();
    let generated = Generator::new(&spec, &settings).run()?;
    let generated = prettyplease::unparse(&syn::parse2(generated)?);
    std::fs::write(
        format!("{}/openapi.rs", std::env::var("OUT_DIR")?),
        &generated,
    )?;
    Ok(())
}
