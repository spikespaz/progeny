use std::collections::{HashMap, HashSet};

use slotmap::{SlotMap, new_key_type};

use crate::type_ref::TypeRef;

new_key_type! { pub struct TypeId; }

#[derive(Debug)]
pub struct TypeGraph {
    types: SlotMap<TypeId, TypeKind>,
    by_ref: HashMap<TypeRef, TypeId>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeKind {
    Primitive(Primitive),
    Refinement(Refinement),
    Nullable(TypeId),
    Coproduct(Vec<TypeId>),
    Intersection(Vec<TypeId>),
    Union(Vec<TypeId>),
    Complement(TypeId),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Primitive {
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
        enumeration: HashSet<String>,
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
        enumeration: Vec<i64>,
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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum FloatKind {
    F32,
    F64,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
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
    fn insert(&mut self, type_ref: TypeRef, type_kind: TypeKind) -> TypeId {
        let type_id = self.types.insert(type_kind);
        self.by_ref.insert(type_ref, type_id);
        type_id
    }

    pub fn get_by_id(&self, type_id: TypeId) -> &TypeKind {
        debug_assert!(self.types.contains_key(type_id));
        self.types.get(type_id).unwrap()
    }

    pub fn get_by_ref(&self, type_ref: &TypeRef) -> Option<(TypeId, &TypeKind)> {
        let type_id = *self.by_ref.get(type_ref)?;
        let type_kind = self.get_by_id(type_id);
        Some((type_id, type_kind))
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
