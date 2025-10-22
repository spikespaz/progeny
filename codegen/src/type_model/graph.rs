use indexmap::IndexSet;
use openapiv3::{ArrayType, Schema, SchemaKind, StringType, Type};
use slotmap::{SecondaryMap, SlotMap, new_key_type};

use super::kinds::{Refinement, Scalar, Sequence, StringFormat};
use super::{ScalarType as _, TypeKind};
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
