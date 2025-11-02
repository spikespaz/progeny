use std::num::NonZeroU64;

use indexmap::{IndexMap, IndexSet};

use super::TypeId;

#[derive(Clone, Debug, PartialEq)]
pub enum TypeKind {
    Anything,
    Scalar(Scalar),
    Record(Record),
    Sequence(Sequence),
    Refinement(Refinement),
    Nullable(TypeId),
    Union(IndexSet<TypeId>),
    Intersection(IndexSet<TypeId>),
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
pub struct Record {
    /// Mapping of field names to types.
    ///
    /// The first value in the tuple determines whether a field is required or
    /// optional, with `#[serde(default, skip_serializing_if = "Option::is_none")]`.
    pub fields: IndexMap<String, (bool, TypeId)>,
    /// The type of unknown fields (from `additionalProperties`).
    ///
    /// Produces a field annotated with `#[serde(default, flatten)]`,
    /// with a type of `IndexMap<String, T>`, where `T` is the resolved `TypeId`.
    pub unknown_of: Option<TypeId>,
    // TODO
    pub _min_props: usize,
    pub _max_props: Option<usize>,
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
        multiple_of: Option<NonZeroU64>,
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
    ( @impl index, $( $Scalar:pat )* ) => {
        const fn index_(scalar: Scalar) -> usize {
            let mut i = 0;
            while i < Scalar::COUNT {
                match (scalar, Scalar::TYPES[i]) {
                    $(($Scalar, $Scalar) => return i,)*
                    _ => i += 1,
                }
            }
            unreachable!();
        }
    };
    ( $( ( $rust_ty:ty => $( $Scalar:tt )+ ) ; )* ) => {
        impl_scalar_types!(@impl, $($rust_ty => $($Scalar)+)*);

        impl Scalar {
            pub const fn index(self) -> usize {
                impl_scalar_types!(@impl index, $($($Scalar)+)*);

                match self {
                    $($($Scalar)+ => const { index_($($Scalar)+) },)*
                }
            }
        }
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
        assert!(
            Option::zip(min_bound, max_bound).is_none_or(|(min, max)| min <= max),
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

impl Sequence {
    pub fn items_type(&self) -> TypeId {
        match *self {
            Sequence::List { items, .. }
            | Sequence::Exactly { items, .. }
            | Sequence::Bounded { items, .. } => items,
        }
    }
}

const _: () = {
    use openapiv3::VariantOrUnknownOrEmpty;

    type StringTypeFormat = VariantOrUnknownOrEmpty<openapiv3::StringFormat>;

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

    type FloatTypeFormat = VariantOrUnknownOrEmpty<openapiv3::NumberFormat>;

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

    type IntegerTypeFormat = VariantOrUnknownOrEmpty<openapiv3::IntegerFormat>;

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
};
