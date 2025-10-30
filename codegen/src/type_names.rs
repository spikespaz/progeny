use std::collections::HashMap;

use indexmap::IndexSet;
use slotmap::SecondaryMap;

use crate::type_model::TypeId;

#[derive(Debug, Default)]
pub struct TypeNameTable {
    /// The ultimate source of truth for the name of a type definition.
    canonical: SecondaryMap<TypeId, syn::Ident>,
    /// Aliases that each type is guaranteed to need.
    aliases: SecondaryMap<TypeId, IndexSet<syn::Ident>>,
    /// Reverse-map of identifier to its owning type, used for conflict resolution.
    owners: HashMap<syn::Ident, TypeId>,
}

impl TypeNameTable {
    pub fn ident_for(&self, id: TypeId) -> Option<&syn::Ident> {
        self.canonical.get(id)
    }

    pub fn aliases_for(&self, id: TypeId) -> impl Iterator<Item = &syn::Ident> {
        self.aliases.get(id).into_iter().flat_map(|set| set.iter())
    }
}
