use proc_macro::{TokenStream, TokenTree};
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{format_ident, quote, ToTokens};
use std::collections::HashSet;
use syn::{
    parenthesized,
    parse::Parse,
    parse_macro_input, parse_quote,
    punctuated::Punctuated,
    spanned::Spanned,
    token::{Comma, Paren},
    Data, DataEnum, DataStruct, DeriveInput, Expr, ExprClosure, ExprPath, Field, Fields, Ident,
    LitStr, Member, Meta, Path, Result, Token, Visibility,
};

pub fn derive_event(input: TokenStream) -> TokenStream {
    let mut ast = parse_macro_input!(input as DeriveInput);
    let mut auto_propagate = false;
    let mut traversal = quote!(());
    let bevy_ecs_path: Path = crate::bevy_ecs_path();

    ast.generics
        .make_where_clause()
        .predicates
        .push(parse_quote! { Self: Send + Sync + 'static });

    if let Some(attr) = ast.attrs.iter().find(|attr| attr.path().is_ident("event")) {
        if let Err(e) = attr.parse_nested_meta(|meta| match meta.path.get_ident() {
            Some(ident) if ident == "auto_propagate" => {
                auto_propagate = true;
                Ok(())
            }
            Some(ident) if ident == "traversal" => {
                traversal = meta.value()?.parse()?;
                Ok(())
            }
            _ => Err(meta.error("unsupported attribute")),
        }) {
            return e.to_compile_error().into();
        }
    }

    let struct_name = &ast.ident;
    let (impl_generics, type_generics, where_clause) = &ast.generics.split_for_impl();

    TokenStream::from(quote! {
        impl #impl_generics #bevy_ecs_path::event::Event for #struct_name #type_generics #where_clause {
            type Traversal = #traversal;
            const AUTO_PROPAGATE: bool = #auto_propagate;
        }
    })
}

pub fn derive_resource(input: TokenStream) -> TokenStream {
    let mut ast = parse_macro_input!(input as DeriveInput);
    let bevy_ecs_path: Path = crate::bevy_ecs_path();

    ast.generics
        .make_where_clause()
        .predicates
        .push(parse_quote! { Self: Send + Sync + 'static });

    let struct_name = &ast.ident;
    let (impl_generics, type_generics, where_clause) = &ast.generics.split_for_impl();

    TokenStream::from(quote! {
        impl #impl_generics #bevy_ecs_path::resource::Resource for #struct_name #type_generics #where_clause {
        }
    })
}

const ENTITIES_ATTR: &str = "entities";

pub fn derive_component(input: TokenStream) -> TokenStream {
    let mut ast = parse_macro_input!(input as DeriveInput);
    let bevy_ecs_path: Path = crate::bevy_ecs_path();

    let attrs = match parse_component_attr(&ast) {
        Ok(attrs) => attrs,
        Err(e) => return e.into_compile_error().into(),
    };

    let relationship = match derive_relationship(&ast, &attrs, &bevy_ecs_path) {
        Ok(value) => value,
        Err(err) => err.into_compile_error().into(),
    };
    let relationship_target = match derive_relationship_target(&ast, &attrs, &bevy_ecs_path) {
        Ok(value) => value,
        Err(err) => err.into_compile_error().into(),
    };

    let visit_entities = visit_entities(&ast.data, &bevy_ecs_path, relationship.is_some());

    let storage = storage_path(&bevy_ecs_path, attrs.storage);

    let on_add_path = attrs.on_add.map(|path| path.to_token_stream());
    let on_remove_path = attrs.on_remove.map(|path| path.to_token_stream());

    let on_insert_path = if relationship.is_some() {
        if attrs.on_insert.is_some() {
            return syn::Error::new(
                ast.span(),
                "Custom on_insert hooks are not supported as relationships already define an on_insert hook",
            )
            .into_compile_error()
            .into();
        }

        Some(quote!(<Self as #bevy_ecs_path::relationship::Relationship>::on_insert))
    } else {
        attrs.on_insert.map(|path| path.to_token_stream())
    };

    let on_replace_path = if relationship.is_some() {
        if attrs.on_replace.is_some() {
            return syn::Error::new(
                ast.span(),
                "Custom on_replace hooks are not supported as Relationships already define an on_replace hook",
            )
            .into_compile_error()
            .into();
        }

        Some(quote!(<Self as #bevy_ecs_path::relationship::Relationship>::on_replace))
    } else if attrs.relationship_target.is_some() {
        if attrs.on_replace.is_some() {
            return syn::Error::new(
                ast.span(),
                "Custom on_replace hooks are not supported as RelationshipTarget already defines an on_replace hook",
            )
            .into_compile_error()
            .into();
        }

        Some(quote!(<Self as #bevy_ecs_path::relationship::RelationshipTarget>::on_replace))
    } else {
        attrs.on_replace.map(|path| path.to_token_stream())
    };

    let on_despawn_path = if attrs
        .relationship_target
        .is_some_and(|target| target.linked_spawn)
    {
        if attrs.on_despawn.is_some() {
            return syn::Error::new(
                ast.span(),
                "Custom on_despawn hooks are not supported as this RelationshipTarget already defines an on_despawn hook, via the 'linked_spawn' attribute",
            )
            .into_compile_error()
            .into();
        }

        Some(quote!(<Self as #bevy_ecs_path::relationship::RelationshipTarget>::on_despawn))
    } else {
        attrs.on_despawn.map(|path| path.to_token_stream())
    };

    let on_add = hook_register_function_call(&bevy_ecs_path, quote! {on_add}, on_add_path);
    let on_insert = hook_register_function_call(&bevy_ecs_path, quote! {on_insert}, on_insert_path);
    let on_replace =
        hook_register_function_call(&bevy_ecs_path, quote! {on_replace}, on_replace_path);
    let on_remove = hook_register_function_call(&bevy_ecs_path, quote! {on_remove}, on_remove_path);
    let on_despawn =
        hook_register_function_call(&bevy_ecs_path, quote! {on_despawn}, on_despawn_path);

    ast.generics
        .make_where_clause()
        .predicates
        .push(parse_quote! { Self: Send + Sync + 'static });

    let requires = &attrs.requires;
    let mut register_required = Vec::with_capacity(attrs.requires.iter().len());
    let mut register_recursive_requires = Vec::with_capacity(attrs.requires.iter().len());
    if let Some(requires) = requires {
        for require in requires {
            let ident = &require.path;
            register_recursive_requires.push(quote! {
                <#ident as #bevy_ecs_path::component::Component>::register_required_components(
                    requiree,
                    components,
                    required_components,
                    inheritance_depth + 1,
                    recursion_check_stack
                );
            });
            match &require.func {
                Some(RequireFunc::Path(func)) => {
                    register_required.push(quote! {
                        components.register_required_components_manual::<Self, #ident>(
                            required_components,
                            || { let x: #ident = #func().into(); x },
                            inheritance_depth,
                            recursion_check_stack
                        );
                    });
                }
                Some(RequireFunc::Closure(func)) => {
                    register_required.push(quote! {
                        components.register_required_components_manual::<Self, #ident>(
                            required_components,
                            || { let x: #ident = (#func)().into(); x },
                            inheritance_depth,
                            recursion_check_stack
                        );
                    });
                }
                None => {
                    register_required.push(quote! {
                        components.register_required_components_manual::<Self, #ident>(
                            required_components,
                            <#ident as Default>::default,
                            inheritance_depth,
                            recursion_check_stack
                        );
                    });
                }
            }
        }
    }
    let struct_name = &ast.ident;
    let (impl_generics, type_generics, where_clause) = &ast.generics.split_for_impl();

    let mutable_type = (attrs.immutable || relationship.is_some())
        .then_some(quote! { #bevy_ecs_path::component::Immutable })
        .unwrap_or(quote! { #bevy_ecs_path::component::Mutable });

    let clone_behavior = if relationship_target.is_some() {
        quote!(#bevy_ecs_path::component::ComponentCloneBehavior::RelationshipTarget(#bevy_ecs_path::relationship::clone_relationship_target::<Self>))
    } else {
        quote!(
            use #bevy_ecs_path::component::{DefaultCloneBehaviorBase, DefaultCloneBehaviorViaClone};
            (&&&#bevy_ecs_path::component::DefaultCloneBehaviorSpecialization::<Self>::default()).default_clone_behavior()
        )
    };

    // This puts `register_required` before `register_recursive_requires` to ensure that the constructors of _all_ top
    // level components are initialized first, giving them precedence over recursively defined constructors for the same component type
    TokenStream::from(quote! {
        impl #impl_generics #bevy_ecs_path::component::Component for #struct_name #type_generics #where_clause {
            const STORAGE_TYPE: #bevy_ecs_path::component::StorageType = #storage;
            type Mutability = #mutable_type;
            fn register_required_components(
                requiree: #bevy_ecs_path::component::ComponentId,
                components: &mut #bevy_ecs_path::component::Components,
                required_components: &mut #bevy_ecs_path::component::RequiredComponents,
                inheritance_depth: u16,
                recursion_check_stack: &mut #bevy_ecs_path::__macro_exports::Vec<#bevy_ecs_path::component::ComponentId>
            ) {
                #bevy_ecs_path::component::enforce_no_required_components_recursion(components, recursion_check_stack);
                let self_id = components.register_component::<Self>();
                recursion_check_stack.push(self_id);
                #(#register_required)*
                #(#register_recursive_requires)*
                recursion_check_stack.pop();
            }

            #on_add
            #on_insert
            #on_replace
            #on_remove
            #on_despawn

            fn clone_behavior() -> #bevy_ecs_path::component::ComponentCloneBehavior {
                #clone_behavior
            }

            #visit_entities
        }

        #relationship

        #relationship_target
    })
}

fn visit_entities(data: &Data, bevy_ecs_path: &Path, is_relationship: bool) -> TokenStream2 {
    match data {
        Data::Struct(DataStruct { fields, .. }) => {
            let mut visit = Vec::with_capacity(fields.len());
            let mut visit_mut = Vec::with_capacity(fields.len());

            let maybe_relationship = if is_relationship {
                relationship_field(fields, "VisitEntities", fields.span()).ok()
            } else {
                None
            };
            fields
                .iter()
                .enumerate()
                .filter(|(_, field)| {
                    field.attrs.iter().any(|a| a.path().is_ident(ENTITIES_ATTR))
                        || maybe_relationship
                            .as_ref()
                            .is_some_and(|relationship| relationship == field)
                })
                .for_each(|(index, field)| {
                    let field_member = ident_or_index(field.ident.as_ref(), index);

                    visit.push(quote!(this.#field_member.visit_entities(&mut func);));
                    visit_mut.push(quote!(this.#field_member.visit_entities_mut(&mut func);));
                });
            if visit.is_empty() && visit_mut.is_empty() {
                return quote!();
            };
            quote!(
                fn visit_entities(this: &Self, mut func: impl FnMut(Entity)) {
                    use #bevy_ecs_path::entity::VisitEntities;
                    #(#visit)*
                }

                fn visit_entities_mut(this: &mut Self, mut func: impl FnMut(&mut Entity)) {
                    use #bevy_ecs_path::entity::VisitEntitiesMut;
                    #(#visit_mut)*
                }
            )
        }
        Data::Enum(DataEnum { variants, .. }) => {
            let mut visit = Vec::with_capacity(variants.len());
            let mut visit_mut = Vec::with_capacity(variants.len());

            for variant in variants.iter() {
                let field_members = variant
                    .fields
                    .iter()
                    .enumerate()
                    .filter_map(|(index, field)| {
                        if field
                            .attrs
                            .iter()
                            .any(|a| a.meta.path().is_ident(ENTITIES_ATTR))
                        {
                            let field_member = ident_or_index(field.ident.as_ref(), index);
                            Some(field_member)
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<_>>();

                let ident = &variant.ident;
                let field_names = field_members
                    .iter()
                    .map(|member| format_ident!("__self_{}", member))
                    .collect::<Vec<_>>();

                visit.push(
                    quote!(Self::#ident {#(#field_members: #field_names,)* ..} => {
                        #(#field_names.visit_entities(func);)*
                    }),
                );
                visit_mut.push(
                    quote!(Self::#ident {#(#field_members: #field_names,)* ..} => {
                        #(#field_names.visit_entities_mut(func);)*
                    }),
                );
            }

            if visit.is_empty() && visit_mut.is_empty() {
                return quote!();
            };
            quote!(
                fn visit_entities(this: &Self, mut func: impl FnMut(Entity)) {
                    use #bevy_ecs_path::entity::VisitEntities;
                    match this {
                        #(#visit,)*
                        _ => {}
                    }
                }

                fn visit_entities_mut(this: &mut Self, mut func: impl FnMut(&mut Entity)) {
                    use #bevy_ecs_path::entity::VisitEntitiesMut;
                    match this {
                        #(#visit_mut,)*
                        _ => {}
                    }
                }
            )
        }
        Data::Union(_) => quote!(),
    }
}

pub(crate) fn ident_or_index(ident: Option<&Ident>, index: usize) -> Member {
    ident.map_or_else(
        || Member::Unnamed(index.into()),
        |ident| Member::Named(ident.clone()),
    )
}

pub fn document_required_components(attr: TokenStream, item: TokenStream) -> TokenStream {
    let paths = parse_macro_input!(attr with Punctuated::<Require, Comma>::parse_terminated)
        .iter()
        .map(|r| format!("[`{}`]", r.path.to_token_stream()))
        .collect::<Vec<_>>()
        .join(", ");

    let bevy_ecs_path = crate::bevy_ecs_path()
        .to_token_stream()
        .to_string()
        .replace(' ', "");
    let required_components_path = bevy_ecs_path + "::component::Component#required-components";

    // Insert information about required components after any existing doc comments
    let mut out = TokenStream::new();
    let mut end_of_attributes_reached = false;
    for tt in item {
        if !end_of_attributes_reached & matches!(tt, TokenTree::Ident(_)) {
            end_of_attributes_reached = true;
            let doc: TokenStream = format!("#[doc = \"\n\n# Required Components\n{paths} \n\n A component's [required components]({required_components_path}) are inserted whenever it is inserted. Note that this will also insert the required components _of_ the required components, recursively, in depth-first order.\"]").parse().unwrap();
            out.extend(doc);
        }
        out.extend(Some(tt));
    }

    out
}

pub const COMPONENT: &str = "component";
pub const STORAGE: &str = "storage";
pub const REQUIRE: &str = "require";
pub const RELATIONSHIP: &str = "relationship";
pub const RELATIONSHIP_TARGET: &str = "relationship_target";

pub const ON_ADD: &str = "on_add";
pub const ON_INSERT: &str = "on_insert";
pub const ON_REPLACE: &str = "on_replace";
pub const ON_REMOVE: &str = "on_remove";
pub const ON_DESPAWN: &str = "on_despawn";

pub const IMMUTABLE: &str = "immutable";

struct Attrs {
    storage: StorageTy,
    requires: Option<Punctuated<Require, Comma>>,
    on_add: Option<ExprPath>,
    on_insert: Option<ExprPath>,
    on_replace: Option<ExprPath>,
    on_remove: Option<ExprPath>,
    on_despawn: Option<ExprPath>,
    relationship: Option<Relationship>,
    relationship_target: Option<RelationshipTarget>,
    immutable: bool,
}

#[derive(Clone, Copy)]
enum StorageTy {
    Table,
    SparseSet,
}

struct Require {
    path: Path,
    func: Option<RequireFunc>,
}

enum RequireFunc {
    Path(Path),
    Closure(ExprClosure),
}

struct Relationship {
    relationship_target: Ident,
}

struct RelationshipTarget {
    relationship: Ident,
    linked_spawn: bool,
}

// values for `storage` attribute
const TABLE: &str = "Table";
const SPARSE_SET: &str = "SparseSet";

fn parse_component_attr(ast: &DeriveInput) -> Result<Attrs> {
    let mut attrs = Attrs {
        storage: StorageTy::Table,
        on_add: None,
        on_insert: None,
        on_replace: None,
        on_remove: None,
        on_despawn: None,
        requires: None,
        relationship: None,
        relationship_target: None,
        immutable: false,
    };

    let mut require_paths = HashSet::new();
    for attr in ast.attrs.iter() {
        if attr.path().is_ident(COMPONENT) {
            attr.parse_nested_meta(|nested| {
                if nested.path.is_ident(STORAGE) {
                    attrs.storage = match nested.value()?.parse::<LitStr>()?.value() {
                        s if s == TABLE => StorageTy::Table,
                        s if s == SPARSE_SET => StorageTy::SparseSet,
                        s => {
                            return Err(nested.error(format!(
                                "Invalid storage type `{s}`, expected '{TABLE}' or '{SPARSE_SET}'.",
                            )));
                        }
                    };
                    Ok(())
                } else if nested.path.is_ident(ON_ADD) {
                    attrs.on_add = Some(nested.value()?.parse::<ExprPath>()?);
                    Ok(())
                } else if nested.path.is_ident(ON_INSERT) {
                    attrs.on_insert = Some(nested.value()?.parse::<ExprPath>()?);
                    Ok(())
                } else if nested.path.is_ident(ON_REPLACE) {
                    attrs.on_replace = Some(nested.value()?.parse::<ExprPath>()?);
                    Ok(())
                } else if nested.path.is_ident(ON_REMOVE) {
                    attrs.on_remove = Some(nested.value()?.parse::<ExprPath>()?);
                    Ok(())
                } else if nested.path.is_ident(ON_DESPAWN) {
                    attrs.on_despawn = Some(nested.value()?.parse::<ExprPath>()?);
                    Ok(())
                } else if nested.path.is_ident(IMMUTABLE) {
                    attrs.immutable = true;
                    Ok(())
                } else {
                    Err(nested.error("Unsupported attribute"))
                }
            })?;
        } else if attr.path().is_ident(REQUIRE) {
            let punctuated =
                attr.parse_args_with(Punctuated::<Require, Comma>::parse_terminated)?;
            for require in punctuated.iter() {
                if !require_paths.insert(require.path.to_token_stream().to_string()) {
                    return Err(syn::Error::new(
                        require.path.span(),
                        "Duplicate required components are not allowed.",
                    ));
                }
            }
            if let Some(current) = &mut attrs.requires {
                current.extend(punctuated);
            } else {
                attrs.requires = Some(punctuated);
            }
        } else if attr.path().is_ident(RELATIONSHIP) {
            let relationship = attr.parse_args::<Relationship>()?;
            attrs.relationship = Some(relationship);
        } else if attr.path().is_ident(RELATIONSHIP_TARGET) {
            let relationship_target = attr.parse_args::<RelationshipTarget>()?;
            attrs.relationship_target = Some(relationship_target);
        }
    }

    Ok(attrs)
}

impl Parse for Require {
    fn parse(input: syn::parse::ParseStream) -> Result<Self> {
        let path = input.parse::<Path>()?;
        let func = if input.peek(Paren) {
            let content;
            parenthesized!(content in input);
            if let Ok(func) = content.parse::<ExprClosure>() {
                Some(RequireFunc::Closure(func))
            } else {
                let func = content.parse::<Path>()?;
                Some(RequireFunc::Path(func))
            }
        } else {
            None
        };
        Ok(Require { path, func })
    }
}

fn storage_path(bevy_ecs_path: &Path, ty: StorageTy) -> TokenStream2 {
    let storage_type = match ty {
        StorageTy::Table => Ident::new("Table", Span::call_site()),
        StorageTy::SparseSet => Ident::new("SparseSet", Span::call_site()),
    };

    quote! { #bevy_ecs_path::component::StorageType::#storage_type }
}

fn hook_register_function_call(
    bevy_ecs_path: &Path,
    hook: TokenStream2,
    function: Option<TokenStream2>,
) -> Option<TokenStream2> {
    function.map(|meta| {
        quote! {
            fn #hook() -> ::core::option::Option<#bevy_ecs_path::component::ComponentHook> {
                ::core::option::Option::Some(#meta)
            }
        }
    })
}

impl Parse for Relationship {
    fn parse(input: syn::parse::ParseStream) -> Result<Self> {
        syn::custom_keyword!(relationship_target);
        input.parse::<relationship_target>()?;
        input.parse::<Token![=]>()?;
        Ok(Relationship {
            relationship_target: input.parse::<Ident>()?,
        })
    }
}

impl Parse for RelationshipTarget {
    fn parse(input: syn::parse::ParseStream) -> Result<Self> {
        let mut relationship: Option<Ident> = None;
        let mut linked_spawn: bool = false;

        let metas = input.parse_terminated(Meta::parse, Token![,])?;

        for meta in metas {
            match meta {
                Meta::Path(path) if path.is_ident("linked_spawn") => linked_spawn = true,
                Meta::NameValue(nv) if nv.path.is_ident(RELATIONSHIP) => {
                    if let Expr::Path(ExprPath { path, .. }) = nv.value {
                        relationship = Some(path.require_ident()?.to_owned());
                    }
                }
                _ => return Err(syn::Error::new(meta.span(), "Invalid attribute")),
            };
        }

        Ok(RelationshipTarget {
            relationship: relationship.ok_or_else(|| {
                syn::Error::new(input.span(), "Missing `relationship = X` attribute")
            })?,
            linked_spawn,
        })
    }
}

fn derive_relationship(
    ast: &DeriveInput,
    attrs: &Attrs,
    bevy_ecs_path: &Path,
) -> Result<Option<TokenStream2>> {
    let Some(relationship) = &attrs.relationship else {
        return Ok(None);
    };
    let Data::Struct(DataStruct {
        fields,
        struct_token,
        ..
    }) = &ast.data
    else {
        return Err(syn::Error::new(
            ast.span(),
            "Relationship can only be derived for structs.",
        ));
    };
    let field = relationship_field(fields, "Relationship", struct_token.span())?;

    let relationship_member = field.ident.clone().map_or(Member::from(0), Member::Named);
    let members = fields
        .members()
        .filter(|member| member != &relationship_member);

    let struct_name = &ast.ident;
    let (impl_generics, type_generics, where_clause) = &ast.generics.split_for_impl();

    let relationship_target = &relationship.relationship_target;

    Ok(Some(quote! {
        impl #impl_generics #bevy_ecs_path::relationship::Relationship for #struct_name #type_generics #where_clause {
            type RelationshipTarget = #relationship_target;

            #[inline(always)]
            fn get(&self) -> #bevy_ecs_path::entity::Entity {
                self.#relationship_member
            }

            #[inline]
            fn from(entity: #bevy_ecs_path::entity::Entity) -> Self {
                Self {
                    #(#members: core::default::Default::default(),),*
                    #relationship_member: entity
                }
            }
        }
    }))
}

fn derive_relationship_target(
    ast: &DeriveInput,
    attrs: &Attrs,
    bevy_ecs_path: &Path,
) -> Result<Option<TokenStream2>> {
    let Some(relationship_target) = &attrs.relationship_target else {
        return Ok(None);
    };

    let Data::Struct(DataStruct {
        fields,
        struct_token,
        ..
    }) = &ast.data
    else {
        return Err(syn::Error::new(
            ast.span(),
            "RelationshipTarget can only be derived for structs.",
        ));
    };
    let field = relationship_field(fields, "RelationshipTarget", struct_token.span())?;

    if field.vis != Visibility::Inherited {
        return Err(syn::Error::new(field.span(), "The collection in RelationshipTarget must be private to prevent users from directly mutating it, which could invalidate the correctness of relationships."));
    }
    let collection = &field.ty;
    let relationship_member = field.ident.clone().map_or(Member::from(0), Member::Named);

    let members = fields
        .members()
        .filter(|member| member != &relationship_member);

    let relationship = &relationship_target.relationship;
    let struct_name = &ast.ident;
    let (impl_generics, type_generics, where_clause) = &ast.generics.split_for_impl();
    let linked_spawn = relationship_target.linked_spawn;
    Ok(Some(quote! {
        impl #impl_generics #bevy_ecs_path::relationship::RelationshipTarget for #struct_name #type_generics #where_clause {
            const LINKED_SPAWN: bool = #linked_spawn;
            type Relationship = #relationship;
            type Collection = #collection;

            #[inline]
            fn collection(&self) -> &Self::Collection {
                &self.#relationship_member
            }

            #[inline]
            fn collection_mut_risky(&mut self) -> &mut Self::Collection {
                &mut self.#relationship_member
            }

            #[inline]
            fn from_collection_risky(collection: Self::Collection) -> Self {
                Self {
                    #(#members: core::default::Default::default(),),*
                    #relationship_member: collection
                }
            }
        }
    }))
}

/// Returns the field with the `#[relationship]` attribute, the only field if unnamed,
/// or the only field in a [`Fields::Named`] with one field, otherwise `Err`.
fn relationship_field<'a>(
    fields: &'a Fields,
    derive: &'static str,
    span: Span,
) -> Result<&'a Field> {
    match fields {
        Fields::Named(fields) if fields.named.len() == 1 => Ok(fields.named.first().unwrap()),
        Fields::Named(fields) => fields.named.iter().find(|field| {
            field
                .attrs
                .iter()
                .any(|attr| attr.path().is_ident("relationship"))
        }).ok_or(syn::Error::new(
            span,
            format!("{derive} derive expected named structs with a single field or with a field annotated with #[relationship].")
        )),
        Fields::Unnamed(fields) => fields
            .unnamed
            .len()
            .eq(&1)
            .then(|| fields.unnamed.first())
            .flatten()
            .ok_or(syn::Error::new(
                span,
                format!("{derive} derive expected unnamed structs with one field."),
            )),
        Fields::Unit => Err(syn::Error::new(
            span,
            format!("{derive} derive expected named or unnamed struct, found unit struct."),
        )),
    }
}
