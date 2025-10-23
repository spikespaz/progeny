use std::num::NonZeroU64;

use indexmap::IndexSet;
use openapiv3::{ArrayType, BooleanType, IntegerType, Schema, SchemaKind, StringType, Type};
use slotmap::{SecondaryMap, SlotMap, new_key_type};

use super::kinds::{Refinement, Scalar, Sequence, StringFormat};
use super::{ScalarType as _, TypeKind};
use crate::ReferenceResolver;
use crate::resolver::ComponentId;
use crate::type_model::kinds::IntegerKind;

#[derive(Debug, thiserror::Error)]
#[non_exhaustive]
pub enum Error {
    #[error("{0}")]
    Resolve(#[from] crate::resolver::Error),
    #[error("invalid schema: {reason}")]
    InvalidSchema { reason: &'static str },
}

type Result<T> = std::result::Result<T, Error>;

new_key_type! { pub struct TypeId; }

#[derive(Debug)]
pub struct TypeGraph<'doc> {
    resolver: ReferenceResolver<'doc>,
    types: SlotMap<TypeId, TypeKind>,
    by_component: SecondaryMap<ComponentId, TypeId>,
    scalar_ids: [TypeId; Scalar::COUNT],
    anything_id: TypeId,
    uninhabited_id: TypeId,
    null_id: TypeId,
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
        let null_id = types.insert(TypeKind::Nullable(uninhabited_id));

        Self {
            resolver,
            types,
            by_component: SecondaryMap::new(),
            scalar_ids,
            anything_id,
            uninhabited_id,
            null_id,
        }
    }

    pub fn add_schema(&mut self, schema: &Schema) -> Result<TypeId> {
        match &schema.schema_kind {
            SchemaKind::Type(Type::String(string_type)) => Ok(self.add_string_type(string_type)),
            SchemaKind::Type(Type::Number(_number_type)) => todo!(),
            SchemaKind::Type(Type::Integer(integer_type)) => self.add_integer_type(integer_type),
            SchemaKind::Type(Type::Object(_object_type)) => todo!(),
            SchemaKind::Type(Type::Array(array_type)) => self.add_array_type(array_type),
            SchemaKind::Type(Type::Boolean(boolean_type)) => {
                Ok(self.add_boolean_type(boolean_type))
            }
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
            TypeKind::Nullable(ty) if ty == self.uninhabited_id => self.null_id,
            TypeKind::Uninhabited => self.uninhabited_id,
            type_kind => self.types.insert(type_kind),
        }
    }

    /// Given a pre-resolved component and a [`Schema`], get the associated `TypeId`
    /// if it exists, otherwise, add the schema and register an association.
    fn intern_schema(&mut self, component_id: ComponentId, schema: &Schema) -> Result<TypeId> {
        if let Some((type_id, _)) = self.get_by_component(component_id) {
            Ok(type_id)
        } else {
            let type_id = self.add_schema(schema)?;
            self.by_component.insert(component_id, type_id);
            Ok(type_id)
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

    pub fn add_integer_type(&mut self, integer_type: &IntegerType) -> Result<TypeId> {
        let &IntegerType {
            ref format,
            multiple_of,
            exclusive_minimum: _,
            exclusive_maximum: _,
            minimum,
            maximum,
            ref enumeration,
        } = integer_type;

        // <https://datatracker.ietf.org/doc/html/draft-wright-json-schema-validation-00#section-5.1>
        // <https://github.com/glademiller/openapiv3/pull/97>
        let multiple_of = match multiple_of {
            Some(m) if m > 0 => Some(NonZeroU64::new(m as u64).unwrap()),
            Some(_) => {
                return Err(Error::InvalidSchema {
                    reason: "`multipleOf` must be greater than zero",
                });
            }
            None => None,
        };

        let (integer_kind, format) = IntegerKind::try_from(format)
            .map_or_else(|special| (None, special), |kind| (Some(kind), None));

        let minimum = minimum.map(|min| (integer_type.exclusive_minimum, min));
        let maximum = maximum.map(|max| (integer_type.exclusive_maximum, max));

        let is_nullable = enumeration.contains(&None);
        let enumeration = enumeration
            .iter()
            .flatten()
            .copied()
            .collect::<IndexSet<_>>();

        // TODO: fix the FIXME below by using a 128-bit interval.
        let interval_min = minimum.and_then(|(ne, min)| min.checked_add(ne as i64));
        let interval_max = maximum.and_then(|(ne, max)| max.checked_sub(ne as i64));

        let is_overflow_min = minimum.is_some() && interval_min.is_none();
        let is_underflow_max = maximum.is_some() && interval_max.is_none();

        let is_non_negative = interval_min
            .or(minimum.map(|(_, min)| min))
            .is_some_and(|min| min >= 0);

        // Cast is safe; it came from an `i64` originally.
        let has_valid_enum = enumeration.iter().any(|&v| {
            multiple_of.is_none_or(|m| v % m.get() as i64 == 0)
                && (minimum.is_none_or(|(ne, min)| (ne && v > min) || (!ne && v >= min)))
                && (maximum.is_none_or(|(ne, max)| (ne && v < max) || (!ne && v <= max)))
        });

        // FIXME: if one side overflows, empty domain won't be caught.
        let is_empty_domain = (!enumeration.is_empty() && !has_valid_enum)
            || matches!((interval_min, interval_max), (Some(min), Some(max)) if min > max);

        // Policy: widen on overflow
        let integer_kind = if is_empty_domain {
            None
        } else if is_underflow_max && is_non_negative {
            Some(IntegerKind::U128)
        } else if is_overflow_min || is_underflow_max {
            Some(IntegerKind::I128)
        } else {
            // enum value extrema, causes widen if necessary
            let enum_min = enumeration.iter().min().copied();
            let enum_max = enumeration.iter().max().copied();
            // union extrema, scalar selection
            let union_min = [interval_min, enum_min].iter().flatten().min().copied();
            let union_max = [interval_max, enum_max].iter().flatten().max().copied();

            Some(integer_kind.unwrap_or_else(|| IntegerKind::from_bounds(union_min, union_max)))
        };

        let multiple_of_one = multiple_of.is_none_or(|m| m.get() == 1);

        let has_exact_min = Option::zip(integer_kind, interval_min).is_some_and(|(kind, min)| {
            (kind.is_signed() && min as i128 == kind.min()) || (kind.is_unsigned() && min == 0)
        });
        let has_exact_max = Option::zip(integer_kind, interval_max).is_some_and(|(kind, max)| {
            (kind.is_signed() && max as i128 == kind.max() as i128)
                || (kind.is_unsigned() && max >= 0 && max as u128 == kind.max())
        });

        let is_unconstrained = format.is_none()
            && multiple_of_one
            && (minimum.is_none() || has_exact_min)
            && (maximum.is_none() || has_exact_max)
            && enumeration.is_empty();

        let mut type_kind = match integer_kind {
            Some(kind) if is_unconstrained => TypeKind::Scalar(Scalar::Integer(kind)),
            Some(kind) => TypeKind::Refinement(Refinement::Integer {
                kind,
                format: format.map(ToOwned::to_owned),
                multiple_of,
                minimum,
                maximum,
                enumeration,
            }),
            None => {
                debug_assert!(is_empty_domain);
                TypeKind::Uninhabited
            }
        };

        if is_nullable {
            type_kind = TypeKind::Nullable(self.insert(type_kind));
        }

        Ok(self.insert(type_kind))
    }

    pub fn add_array_type(&mut self, array_type: &ArrayType) -> Result<TypeId> {
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
            self.intern_schema(component_id, &schema)?
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

    pub fn add_boolean_type(&mut self, boolean_type: &BooleanType) -> TypeId {
        let BooleanType { enumeration } = boolean_type;

        let is_nullable = enumeration.contains(&None);
        let has_true = enumeration.contains(&Some(true));
        let has_false = enumeration.contains(&Some(false));

        let is_unconstrained = enumeration.is_empty() || (has_true && has_false);

        let mut type_kind = if is_unconstrained {
            TypeKind::Scalar(bool::TYPE)
        } else if !has_true && !has_false {
            TypeKind::Uninhabited
        } else {
            debug_assert!(has_true ^ has_false);
            TypeKind::Refinement(Refinement::Boolean {
                has_true,
                has_false,
            })
        };

        if is_nullable {
            type_kind = TypeKind::Nullable(self.insert(type_kind))
        }

        self.insert(type_kind)
    }
}

#[cfg(test)]
mod tests {
    use std::sync::LazyLock;

    use openapiv3::{IntegerType, OpenAPI};
    use test_case::test_case;

    use super::TypeGraph;
    use crate::ReferenceResolver;
    use crate::type_model::TypeKind;

    static EMPTY_SPEC: LazyLock<OpenAPI> = LazyLock::new(|| OpenAPI {
        openapi: "3.0.4".to_owned(),
        info: openapiv3::Info {
            title: "".to_owned(),
            version: "0".to_owned(),
            ..Default::default()
        },
        ..Default::default()
    });

    fn empty_type_graph() -> TypeGraph<'static> {
        TypeGraph::new(ReferenceResolver::new(&EMPTY_SPEC))
    }

    #[test_case(
        &IntegerType {
            minimum: Some(i64::MAX),
            maximum: Some(i64::MIN),
            ..Default::default()
        }
        ; "invalid i64 interval extrema with empty domain is uninhabited"
    )]
    #[test_case(
        &IntegerType {
            minimum: Some(i64::MAX),
            maximum: Some(i64::MIN),
            exclusive_minimum: true, // exclusive will overflow
            exclusive_maximum: true, // exclusive will underflow
            ..Default::default()
        }
        ; "overflowed invalid i64 interval extrema with empty domain is uninhabited"
    )]
    #[test_case(
        &IntegerType {
            minimum: Some(i64::MAX),
            maximum: Some(i64::MAX),
            exclusive_minimum: true, // exclusive will overflow
            ..Default::default()
        }
        ; "overflow i64 exclusive minimum with empty domain is uninhabited"
    )]
    #[test_case(
        &IntegerType {
            minimum: Some(i64::MIN),
            maximum: Some(i64::MIN),
            exclusive_maximum: true, // exclusive will underflow
            ..Default::default()
        }
        ; "underflow i64 exclusive maximum with empty domain is uninhabited"
    )]
    fn uninhabited_integer_type(schema: &IntegerType) {
        let mut graph = empty_type_graph();
        let type_id = graph.add_integer_type(schema).unwrap();
        match graph.get_by_id(type_id) {
            TypeKind::Uninhabited => assert_eq!(type_id, graph.uninhabited_id()),
            kind => panic!("expected `TypeKind::Uninhabited`, found: {kind:?}"),
        }
    }
}
