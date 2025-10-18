use openapiv3::{Schema, SchemaKind, Type};
use syn::parse_quote;

#[derive(Clone, Debug, PartialEq)]
pub enum TypeDef {
    Inline(syn::Type),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum IntegerKind {
    U8,
    U16,
    U32,
    U64,
    U128,
    I8,
    I16,
    I32,
    I64,
    I128,
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

impl IntegerKind {
    pub fn from_bounds(min_bound: Option<i64>, max_bound: Option<i64>) -> Self {
        debug_assert!(
            matches!((min_bound, max_bound), (Some(min), Some(max)) if min <= max),
            "min_bound must be <= max_bound"
        );
        let non_negative = min_bound.is_some_and(|min| min >= 0);
        if non_negative {
            match max_bound.map(|max| max as u64) {
                Some(max) if max <= u8::MAX as u64 => IntegerKind::U8,
                Some(max) if max <= u16::MAX as u64 => IntegerKind::U16,
                Some(max) if max <= u32::MAX as u64 => IntegerKind::U32,
                _ => IntegerKind::U64,
            }
        } else {
            match (min_bound.unwrap_or(i64::MIN), max_bound.unwrap_or(i64::MAX)) {
                (min, max) if min >= i8::MIN as i64 && max <= i8::MAX as i64 => IntegerKind::I8,
                (min, max) if min >= i16::MIN as i64 && max <= i16::MAX as i64 => IntegerKind::I16,
                (min, max) if min >= i32::MIN as i64 && max <= i32::MAX as i64 => IntegerKind::I32,
                _ => IntegerKind::I64,
            }
        }
    }

    pub fn to_type(self) -> syn::Type {
        match self {
            IntegerKind::I8 => parse_quote!(i8),
            IntegerKind::I16 => parse_quote!(i16),
            IntegerKind::I32 => parse_quote!(i32),
            IntegerKind::I64 => parse_quote!(i64),
            IntegerKind::I128 => parse_quote!(i128),
            IntegerKind::U8 => parse_quote!(u8),
            IntegerKind::U16 => parse_quote!(u16),
            IntegerKind::U32 => parse_quote!(u32),
            IntegerKind::U64 => parse_quote!(u64),
            IntegerKind::U128 => parse_quote!(u128),
        }
    }
}
