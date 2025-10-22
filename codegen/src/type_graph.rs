use indexmap::IndexSet;
use openapiv3::{ArrayType, Schema, SchemaKind, StringType, Type};
use slotmap::{SecondaryMap, SlotMap, new_key_type};

use crate::ReferenceResolver;
use crate::resolver::ComponentId;

#[derive(Debug, thiserror::Error)]
#[non_exhaustive]
pub enum Error {
    #[error("{0}")]
    Resolve(#[from] crate::resolver::Error),
}

new_key_type! { pub struct TypeId; }

#[derive(Debug)]
pub struct TypeGraph<'doc> {
    resolver: ReferenceResolver<'doc>,
    types: SlotMap<TypeId, TypeKind>,
    by_component: SecondaryMap<ComponentId, TypeId>,
    scalar_ids: [TypeId; Scalar::COUNT],
    anything_id: TypeId,
    uninhabited_id: TypeId,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeKind {
    Anything,
    Scalar(Scalar),
    Sequence(Sequence),
    Refinement(Refinement),
    Nullable(TypeId),
    Coproduct(Vec<TypeId>),
    Intersection(Vec<TypeId>),
    Union(Vec<TypeId>),
    Complement(TypeId),
    Uninhabited,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Scalar {
    String,
    Float(FloatKind),
    Integer(IntegerKind),
    Boolean,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Sequence {
    /// Represents a basic `Vec<T>`, optionally refined if `unique`.
    List { items: TypeId, unique: bool },
    /// Represents `[T; N]`, optionally refined with `unique`.
    Exactly {
        items: TypeId,
        count: usize,
        unique: bool,
    },
    /// Represents a refined `Vec<T>`.
    Bounded {
        items: TypeId,
        min_items: Option<usize>,
        max_items: Option<usize>,
        unique: bool,
    },
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

impl<'doc> TypeGraph<'doc> {
    pub fn new(resolver: ReferenceResolver<'doc>) -> Self {
        let mut types = SlotMap::with_key();
        let scalar_ids = std::array::from_fn(|i| {
            let kind = TypeKind::Scalar(Scalar::TYPES[i]);
            types.insert(kind)
        });
        let anything_id = types.insert(TypeKind::Anything);
        let uninhabited_id = types.insert(TypeKind::Uninhabited);

        Self {
            resolver,
            types,
            by_component: SecondaryMap::new(),
            scalar_ids,
            anything_id,
            uninhabited_id,
        }
    }

    pub fn add_schema(&mut self, schema: &Schema) -> Result<TypeId, Error> {
        match &schema.schema_kind {
            SchemaKind::Type(Type::String(string_type)) => Ok(self.add_string_type(string_type)),
            SchemaKind::Type(Type::Number(_number_type)) => todo!(),
            SchemaKind::Type(Type::Integer(_integer_type)) => todo!(),
            SchemaKind::Type(Type::Object(_object_type)) => todo!(),
            SchemaKind::Type(Type::Array(array_type)) => self.add_array_type(array_type),
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
        match type_kind {
            TypeKind::Anything => self.anything_id,
            TypeKind::Scalar(ty) => self.scalar_id(ty),
            TypeKind::Uninhabited => self.uninhabited_id,
            type_kind => self.types.insert(type_kind),
        }
    }

    pub fn scalar_id(&self, ty: Scalar) -> TypeId {
        self.scalar_ids[ty.index()]
    }

    pub fn uninhabited_id(&self) -> TypeId {
        self.uninhabited_id
    }

    pub fn get_by_id(&self, type_id: TypeId) -> &TypeKind {
        &self.types[type_id]
    }

    pub fn get_by_component(&self, component_id: ComponentId) -> Option<(TypeId, &TypeKind)> {
        let type_id = *self.by_component.get(component_id)?;
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

        let mut type_kind = if is_unconstrained {
            TypeKind::Scalar(String::TYPE)
        } else {
            TypeKind::Refinement(Refinement::String {
                format,
                pattern: pattern.clone(),
                min_length,
                max_length,
                enumeration,
            })
        };

        if is_nullable {
            type_kind = TypeKind::Nullable(self.insert(type_kind))
        }

        self.insert(type_kind)
    }

    pub fn add_array_type(&mut self, array_type: &ArrayType) -> Result<TypeId, Error> {
        let &ArrayType {
            items: ref item_schema,
            min_items,
            max_items,
            unique_items,
        } = array_type;

        let is_uninhabited = matches!(
            (min_items, max_items),
            (Some(min_items), Some(max_items)) if min_items > max_items
        );

        if is_uninhabited {
            return Ok(self.uninhabited_id);
        }

        let item_type_id = if let Some(item_schema) = item_schema {
            let (component_id, schema) = self.resolver.resolve(item_schema)?;
            if let Some((type_id, _)) = self.get_by_component(component_id) {
                type_id
            } else {
                let type_id = self.add_schema(&schema)?;
                self.by_component.insert(component_id, type_id);
                type_id
            }
        } else {
            // Policy: permissive for future compatibility with OAS 3.1
            self.anything_id
        };

        let sequence_kind = match (min_items, max_items) {
            (None, None) => Sequence::List {
                items: item_type_id,
                unique: unique_items,
            },
            (Some(min_items), Some(max_items)) if min_items == max_items => Sequence::Exactly {
                items: item_type_id,
                count: max_items,
                unique: unique_items,
            },
            _ => Sequence::Bounded {
                items: item_type_id,
                min_items,
                max_items,
                unique: unique_items,
            },
        };

        Ok(self.insert(TypeKind::Sequence(sequence_kind)))
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

impl IntegerKind {
    pub const fn is_signed(self) -> bool {
        matches!(
            self,
            Self::I8 | Self::I16 | Self::I32 | Self::I64 | Self::I128
        )
    }

    pub const fn is_unsigned(self) -> bool {
        !self.is_signed()
    }

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
}

macro_rules! impl_integer_kind {
    ( $( ( $rust_ty:ty => $IntegerKind:pat ) ; )* ) => {
        impl IntegerKind {
            pub const fn min(self) -> i128 {
                match self {
                    $($IntegerKind => <$rust_ty>::MIN as i128,)*
                }
            }

            pub const fn max(self) -> u128 {
                match self {
                    $($IntegerKind => <$rust_ty>::MAX as u128,)*
                }
            }

            pub const fn bits(self) -> u32 {
                match self {
                    $($IntegerKind => <$rust_ty>::BITS as u32,)*
                }
            }
        }
    };
}

impl_integer_kind! {
    (i8 => IntegerKind::I8);
    (i16 => IntegerKind::I16);
    (i32 => IntegerKind::I32);
    (i64 => IntegerKind::I64);
    (i128 => IntegerKind::I128);
    (u8 => IntegerKind::U8);
    (u16 => IntegerKind::U16);
    (u32 => IntegerKind::U32);
    (u64 => IntegerKind::U64);
    (u128 => IntegerKind::U128);
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
