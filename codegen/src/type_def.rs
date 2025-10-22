use openapiv3::{Schema, SchemaKind, Type};
use syn::parse_quote;

use crate::type_model::IntegerKind;

#[derive(Clone, Debug, PartialEq)]
pub enum TypeDef {
    Inline(syn::Type),
}

impl From<&openapiv3::IntegerType> for TypeDef {
    fn from(integer_type: &openapiv3::IntegerType) -> Self {
        let &openapiv3::IntegerType {
            ref format,
            multiple_of: _, // TODO
            exclusive_minimum,
            exclusive_maximum,
            minimum,
            maximum,
            ref enumeration,
        } = integer_type;

        let (integer_kind, format) = match IntegerKind::try_from(format) {
            Ok(kind) => (Some(kind), None),
            Err(Some(special)) => (None, Some(special)),
            Err(None) => (None, None),
        };
        if let Some(_special) = format {
            // unimplemented!("special format '{special}' for integer")
        }

        let bound_min = minimum.and_then(|min| min.checked_add(exclusive_minimum as i64));
        let bound_max = maximum.and_then(|max| max.checked_sub(exclusive_maximum as i64));

        let overflow_min = minimum.is_some() && bound_min.is_none();
        let overflow_max = maximum.is_some() && bound_max.is_none();

        let non_negative = bound_min.or(minimum).is_some_and(|min| min >= 0);

        let enum_min = enumeration.iter().flatten().min().copied();
        let enum_max = enumeration.iter().flatten().max().copied();

        // intersection extrema
        let effective_min = [bound_min, enum_min].iter().flatten().max().copied();
        let effective_max = [bound_max, enum_max].iter().flatten().min().copied();

        let empty_domain =
            matches!((effective_min, effective_max), (Some(min), Some(max)) if min > max);

        if empty_domain {
            return Self::Inline(parse_quote! { ! });
        }

        // Policy: widen on overflow
        if overflow_max && non_negative {
            return Self::Inline(parse_quote! { u128 });
        } else if overflow_min || overflow_max {
            return Self::Inline(parse_quote! { i128 });
        }

        let integer_kind =
            integer_kind.unwrap_or(IntegerKind::from_bounds(effective_min, effective_max));

        // TODO: construct a refinement type using the `nutype` crate
        Self::Inline(integer_kind.to_type())
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
