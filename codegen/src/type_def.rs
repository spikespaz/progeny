use openapiv3::{Schema, SchemaKind, Type};
use syn::parse_quote;

#[derive(Clone, Debug, PartialEq)]
pub enum TypeDef {
    Inline(syn::Type),
}

impl TypeDef {
    pub fn from_schema(schema: &Schema) -> Result<Self, &'static str> {
        match &schema.schema_kind {
            SchemaKind::Type(Type::String(_string_type)) => todo!(),
            SchemaKind::Type(Type::Number(_number_type)) => todo!(),
            SchemaKind::Type(Type::Integer(_integer_type)) => todo!(),
            SchemaKind::Type(Type::Object(_object_type)) => todo!(),
            SchemaKind::Type(Type::Array(_array_type)) => todo!(),
            SchemaKind::Type(Type::Boolean(boolean_type)) => Ok(Self::from(boolean_type)),
            SchemaKind::OneOf { one_of: _ } => todo!(),
            SchemaKind::AllOf { all_of: _ } => todo!(),
            SchemaKind::AnyOf { any_of: _ } => todo!(),
            SchemaKind::Not { not: _ } => todo!(),
            SchemaKind::Any(_any_schema) => todo!(),
        }
    }
}

impl From<&openapiv3::BooleanType> for TypeDef {
    fn from(value: &openapiv3::BooleanType) -> Self {
        #[allow(non_snake_case)]
        let [HAS_NULL, HAS_TRUE, HAS_FALSE] = [0b100, 0b010, 0b001];

        let enum_flags = value.enumeration.iter().fold(0b000_u8, |acc, v| {
            acc | match v {
                None => HAS_NULL,
                Some(true) => HAS_TRUE,
                Some(false) => HAS_FALSE,
            }
        });

        let ty = match enum_flags {
            // No values, or exactly {true,false}
            0b000 | 0b011 => parse_quote! { bool },
            // Exactly {null}
            0b100 => parse_quote! { ::core::option::Option<::core::convert::Infallible> },
            // Exactly {true}
            0b010 => parse_quote! { ::monostate::MustBe!(true) },
            // Exactly {false}
            0b001 => parse_quote! { ::monostate::MustBe!(false) },
            // Exactly {null,true}
            0b110 => parse_quote! { ::core::option::Option<::monostate::MustBe!(true)> },
            // Exactly {null,false}
            0b101 => parse_quote! { ::core::option::Option<::monostate::MustBe!(false)> },
            // Exactly {null,true,false}
            0b111 => parse_quote! { ::core::option::Option<bool> },
            _ => unreachable!(),
        };

        Self::Inline(ty)
    }
}
