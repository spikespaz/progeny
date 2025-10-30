use std::collections::HashMap;

use indexmap::IndexSet;
use quote::format_ident;
use slotmap::SecondaryMap;

use crate::formatting::to_type_ident;
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

    /// Returns an identifier derived from `base_ident` that this type can safely use.
    ///
    /// The returned [`syn::Ident`] is guaranteed not to conflict with any identifiers
    /// currently owned by other types in the table. Conflicts are resolved by
    /// a monotonically-increasing suffix.
    ///
    /// While solving for a conflict-free identifier, if `id` already owns the current
    /// candidate, that derivative identifier is returned unchanged.
    ///
    /// Otherwise, the first derived identifier that is not owned by any type is returned.
    ///
    /// This function does not mutate the table, and callers are responsible for assigning
    /// a persistent ownership.
    pub fn stable_ident_for(&self, id: TypeId, base_ident: &syn::Ident) -> syn::Ident {
        let (mut attempt, mut new_ident) = (0_usize, base_ident.clone());
        loop {
            match self.owners.get(&new_ident) {
                None => break new_ident,
                Some(&owner_id) if owner_id == id => break new_ident,
                Some(_) => {
                    attempt += 1;
                    new_ident = format_ident!("{base_ident}_{attempt}");
                }
            }
        }
    }

    pub fn set_canonical(&mut self, id: TypeId, name: impl AsRef<str>) -> syn::Ident {
        let new_ident = self.stable_ident_for(id, &to_type_ident(name));
        self.canonical.insert(id, new_ident.clone());
        new_ident
    }

    pub fn insert_alias(&mut self, id: TypeId, name: impl AsRef<str>) -> syn::Ident {
        let new_ident = self.stable_ident_for(id, &to_type_ident(name));
        // Do not create an alias if this type already has `new_ident` as the canon identifier.
        // This relies on `stable_ident_for` early-exit in the event that this type already owns the name.
        if self.ident_for(id) != Some(&new_ident) {
            let aliases = self.aliases.entry(id).unwrap().or_default();
            aliases.insert(new_ident.clone());
        }
        new_ident
    }
}
