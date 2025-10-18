use openapiv3::{Schema, SchemaKind, Type};

#[derive(Clone, Debug, PartialEq)]
pub enum TypeDef {}

impl TypeDef {
    pub fn from_schema(schema: &Schema) -> Result<Self, &'static str> {
        match &schema.schema_kind {
            SchemaKind::Type(Type::String(_string_type)) => todo!(),
            SchemaKind::Type(Type::Number(_number_type)) => todo!(),
            SchemaKind::Type(Type::Integer(_integer_type)) => todo!(),
            SchemaKind::Type(Type::Object(_object_type)) => todo!(),
            SchemaKind::Type(Type::Array(_array_type)) => todo!(),
            SchemaKind::Type(Type::Boolean(_boolean_type)) => todo!(),
            SchemaKind::OneOf { one_of: _ } => todo!(),
            SchemaKind::AllOf { all_of: _ } => todo!(),
            SchemaKind::AnyOf { any_of: _ } => todo!(),
            SchemaKind::Not { not: _ } => todo!(),
            SchemaKind::Any(_any_schema) => todo!(),
        }
    }
}
