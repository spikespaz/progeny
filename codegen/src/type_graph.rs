use std::collections::HashMap;

use indexmap::IndexSet;
use openapiv3::{Schema, SchemaKind, StringType, Type};
use slotmap::{SlotMap, new_key_type};

use crate::ReferenceResolver;
use crate::type_ref::TypeRef;

new_key_type! { pub struct TypeId; }

#[derive(Debug)]
pub struct TypeGraph {
    types: SlotMap<TypeId, TypeKind>,
    by_ref: HashMap<TypeRef, TypeId>,
    scalar_ids: [TypeId; Scalar::COUNT],
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeKind {
    Scalar(Scalar),
    Refinement(Refinement),
    Nullable(TypeId),
    Coproduct(Vec<TypeId>),
    Intersection(Vec<TypeId>),
    Union(Vec<TypeId>),
    Complement(TypeId),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Scalar {
    String,
    Float(FloatKind),
    Integer(IntegerKind),
    Boolean,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Refinement {
    String {
        format: Option<StringFormat>,
        pattern: Option<String>,
        min_length: Option<usize>,
        max_length: Option<usize>,
        enumeration: IndexSet<String>,
    },
    Float {
        kind: FloatKind,
        format: Option<String>,
        multiple_of: Option<f64>,
        minimum: Option<(bool, f64)>,
        maximum: Option<(bool, f64)>,
        enumeration: Vec<f64>,
    },
    Integer {
        kind: IntegerKind,
        format: Option<String>,
        multiple_of: Option<i64>,
        minimum: Option<(bool, i64)>,
        maximum: Option<(bool, i64)>,
        enumeration: IndexSet<i64>,
    },
    Boolean {
        has_true: bool,
        has_false: bool,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StringFormat {
    /// <https://datatracker.ietf.org/doc/html/rfc3339#section-5.6>
    Date,
    /// <https://datatracker.ietf.org/doc/html/rfc3339#section-5.6>
    DateTime,
    /// Obscured value
    Password,
    /// Base64-encoded characters
    /// <https://datatracker.ietf.org/doc/html/rfc4648#section-4>
    Byte,
    /// Sequence of octets
    Binary,
    /// External format
    Other(String),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum FloatKind {
    F32,
    F64,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum IntegerKind {
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

impl TypeGraph {
    pub fn new() -> Self {
        let mut types = SlotMap::with_key();
        let scalar_ids = std::array::from_fn(|i| {
            let kind = TypeKind::Scalar(Scalar::TYPES[i]);
            types.insert(kind)
        });

        Self {
            types,
            by_ref: HashMap::new(),
            scalar_ids,
        }
    }

    pub fn add_schema(&mut self, schema: &Schema, resolver: &mut ReferenceResolver<'_>) -> TypeId {
        match &schema.schema_kind {
            SchemaKind::Type(Type::String(string_type)) => self.add_string_type(string_type),
            SchemaKind::Type(Type::Number(_number_type)) => todo!(),
            SchemaKind::Type(Type::Integer(_integer_type)) => todo!(),
            SchemaKind::Type(Type::Object(_object_type)) => todo!(),
            SchemaKind::Type(Type::Array(_array_type)) => todo!(),
            SchemaKind::Type(Type::Boolean(_boolean_type)) => todo!(),
            SchemaKind::OneOf { one_of: _ } => todo!(),
            SchemaKind::AllOf { all_of: _ } => todo!(),
            SchemaKind::AnyOf { any_of: _ } => todo!(),
            SchemaKind::Not { not: _ } => todo!(),
            SchemaKind::Any(any_schema) => {
                unimplemented!("{}", std::any::type_name_of_val(any_schema))
            }
        }
    }

    fn insert(&mut self, type_kind: TypeKind) -> TypeId {
        if let TypeKind::Scalar(ty) = type_kind {
            self.scalar_id(ty)
        } else {
            self.types.insert(type_kind)
        }
    }

    fn insert_named(&mut self, type_ref: TypeRef, type_kind: TypeKind) -> TypeId {
        let type_id = self.insert(type_kind);
        self.by_ref.insert(type_ref, type_id);
        type_id
    }

    pub fn scalar_id(&self, ty: Scalar) -> TypeId {
        self.scalar_ids[ty.index()]
    }

    pub fn get_by_id(&self, type_id: TypeId) -> &TypeKind {
        &self.types[type_id]
    }

    pub fn get_by_ref(&self, type_ref: &TypeRef) -> Option<(TypeId, &TypeKind)> {
        let type_id = *self.by_ref.get(type_ref)?;
        let type_kind = self.get_by_id(type_id);
        Some((type_id, type_kind))
    }

    pub fn add_string_type(&mut self, string_type: &StringType) -> TypeId {
        let &StringType {
            ref format,
            ref pattern,
            ref enumeration,
            min_length,
            max_length,
        } = string_type;

        let format = StringFormat::try_from(format).ok();
        let is_nullable = enumeration.contains(&None);
        let enumeration = enumeration
            .iter()
            .flatten()
            .cloned()
            .collect::<IndexSet<_>>();
        let is_unconstrained = format.is_none()
            && pattern.is_none()
            && enumeration.is_empty()
            && (min_length.is_none() || min_length == Some(0))
            && max_length.is_none();

        let type_kind = if is_unconstrained && !is_nullable {
            TypeKind::Scalar(String::TYPE)
        } else if is_unconstrained && is_nullable {
            TypeKind::Nullable(self.scalar_id(String::TYPE))
        } else {
            TypeKind::Refinement(Refinement::String {
                format,
                pattern: pattern.clone(),
                min_length,
                max_length,
                enumeration,
            })
        };

        self.insert(type_kind)
    }
}

pub trait ScalarType: crate::Sealed {
    const TYPE: Scalar;
}

macro_rules! impl_scalar_types {
    ( @impl, $( $rust_ty:ty => $Scalar:expr )* ) => {
        $(
            impl crate::Sealed for $rust_ty {}

            impl ScalarType for $rust_ty {
                const TYPE: Scalar = $Scalar;
            }
        )*

        impl Scalar {
            pub const COUNT: usize = [$($Scalar,)*].len();
            pub const TYPES: [Scalar; Self::COUNT] = [$($Scalar,)*];
        }
    };
    ( @impl, $( $Scalar:pat )* ) => {
        impl Scalar {
            pub const fn index(self) -> usize {
                let mut i = 0;
                while i < Self::COUNT {
                    match (self, Self::TYPES[i]) {
                        $(($Scalar, $Scalar) => return i,)*
                        _ => i += 1,
                    }
                }
                unreachable!();
            }
        }
    };
    ( $( ( $rust_ty:ty => $( $Scalar:tt )+ ) ; )* ) => {
        impl_scalar_types!(@impl, $($rust_ty => $($Scalar)+)*);
        impl_scalar_types!(@impl, $($($Scalar)+)*);
    };
}

impl_scalar_types! {
    (String => Scalar::String);
    (f32 => Scalar::Float(FloatKind::F32));
    (f64 => Scalar::Float(FloatKind::F64));
    (i8 => Scalar::Integer(IntegerKind::I8));
    (i16 => Scalar::Integer(IntegerKind::I16));
    (i32 => Scalar::Integer(IntegerKind::I32));
    (i64 => Scalar::Integer(IntegerKind::I64));
    (i128 => Scalar::Integer(IntegerKind::I128));
    (u8 => Scalar::Integer(IntegerKind::U8));
    (u16 => Scalar::Integer(IntegerKind::U16));
    (u32 => Scalar::Integer(IntegerKind::U32));
    (u64 => Scalar::Integer(IntegerKind::U64));
    (u128 => Scalar::Integer(IntegerKind::U128));
    (bool => Scalar::Boolean);
}

type StringTypeFormat = openapiv3::VariantOrUnknownOrEmpty<openapiv3::StringFormat>;

impl<'a> TryFrom<&'a StringTypeFormat> for StringFormat {
    type Error = ();

    fn try_from(other: &'a StringTypeFormat) -> Result<Self, Self::Error> {
        use openapiv3::StringFormat;

        match other {
            StringTypeFormat::Item(StringFormat::Date) => Ok(Self::Date),
            StringTypeFormat::Item(StringFormat::DateTime) => Ok(Self::DateTime),
            StringTypeFormat::Item(StringFormat::Password) => Ok(Self::Password),
            StringTypeFormat::Item(StringFormat::Byte) => Ok(Self::Byte),
            StringTypeFormat::Item(StringFormat::Binary) => Ok(Self::Binary),
            StringTypeFormat::Unknown(unknown) => Ok(Self::Other(unknown.clone())),
            StringTypeFormat::Empty => Err(()),
        }
    }
}

type FloatTypeFormat = openapiv3::VariantOrUnknownOrEmpty<openapiv3::NumberFormat>;

impl<'a> TryFrom<&'a FloatTypeFormat> for FloatKind {
    type Error = Option<&'a str>;

    fn try_from(other: &'a FloatTypeFormat) -> Result<Self, Self::Error> {
        use openapiv3::NumberFormat;

        match other {
            FloatTypeFormat::Item(NumberFormat::Float) => Ok(Self::F32),
            FloatTypeFormat::Item(NumberFormat::Double) => Ok(Self::F64),
            FloatTypeFormat::Unknown(unknown) => Err(Some(unknown.as_str())),
            FloatTypeFormat::Empty => Err(None),
        }
    }
}

type IntegerTypeFormat = openapiv3::VariantOrUnknownOrEmpty<openapiv3::IntegerFormat>;

impl<'a> TryFrom<&'a IntegerTypeFormat> for IntegerKind {
    type Error = Option<&'a str>;

    fn try_from(other: &'a IntegerTypeFormat) -> Result<Self, Self::Error> {
        use openapiv3::IntegerFormat;

        match other {
            IntegerTypeFormat::Item(IntegerFormat::Int32) => Ok(Self::I32),
            IntegerTypeFormat::Item(IntegerFormat::Int64) => Ok(Self::I64),
            IntegerTypeFormat::Unknown(unknown) => match unknown.as_str() {
                "int8" => Ok(Self::I8),
                "int16" => Ok(Self::I16),
                "int32" => Ok(Self::I32),
                "int64" => Ok(Self::I64),
                "int128" => Ok(Self::I128),
                "uint8" => Ok(Self::U8),
                "uint16" => Ok(Self::U16),
                "uint32" => Ok(Self::U32),
                "uint64" => Ok(Self::U64),
                "uint128" => Ok(Self::U128),
                unknown => Err(Some(unknown)),
            },
            IntegerTypeFormat::Empty => Err(None),
        }
    }
}
