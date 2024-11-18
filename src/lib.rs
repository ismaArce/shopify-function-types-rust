extern crate proc_macro;

use graphql_parser::{
    query::{
        parse_query, Definition as QueryDefinition, Document as QueryDocument, Selection,
        SelectionSet,
    },
    schema::{
        parse_schema, Definition, Document as SchemaDocument, EnumType, InterfaceType, ScalarType,
        Type, TypeDefinition, UnionType,
    },
};
use heck::ToUpperCamelCase;
use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::quote;
use std::collections::HashMap;
use std::path::Path;
use syn::parse::{Parse, Parser};
use syn::punctuated::Punctuated;
use syn::{parse_macro_input, LitStr, Token};

#[proc_macro]
pub fn graphql_schema(input: TokenStream) -> TokenStream {
    let input: Punctuated<LitStr, Token![,]> =
        parse_macro_input!(input with Punctuated::parse_separated_nonempty);

    if input.len() != 2 {
        return syn::Error::new_spanned(
            input,
            "Expected two arguments: schema path and query path",
        )
            .to_compile_error()
            .into();
    }

    let schema_path_literal = &input[0];
    let query_path_literal = &input[1];

    let schema_path_value = schema_path_literal.value();
    let query_path_value = query_path_literal.value();

    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
    let schema_path = Path::new(&manifest_dir).join(&schema_path_value);
    let query_path = Path::new(&manifest_dir).join(&query_path_value);

    let schema_str = match std::fs::read_to_string(&schema_path) {
        Ok(content) => content,
        Err(err) => {
            return syn::Error::new_spanned(
                schema_path_literal,
                format!(
                    "Failed to read schema file '{}': {}",
                    schema_path.display(),
                    err
                ),
            )
                .to_compile_error()
                .into();
        }
    };

    let query_str = match std::fs::read_to_string(&query_path) {
        Ok(content) => content,
        Err(err) => {
            return syn::Error::new_spanned(
                query_path_literal,
                format!(
                    "Failed to read query file '{}': {}",
                    query_path.display(),
                    err
                ),
            )
                .to_compile_error()
                .into();
        }
    };

    let schema_ast = match parse_schema::<String>(&schema_str) {
        Ok(ast) => ast,
        Err(err) => {
            return syn::Error::new_spanned(
                schema_path_literal,
                format!("Failed to parse schema: {}", err),
            )
                .to_compile_error()
                .into();
        }
    };

    let mut root_query_type_name = None;
    for def in &schema_ast.definitions {
        if let Definition::SchemaDefinition(schema_def) = def {
            if let Some(query) = &schema_def.query {
                root_query_type_name = Some(query.clone());
                break;
            }
        }
    }

    if root_query_type_name.is_none() {
        for def in &schema_ast.definitions {
            if let Definition::TypeDefinition(TypeDefinition::Object(obj)) = def {
                if obj.name == "Query" {
                    root_query_type_name = Some("Query".to_string());
                    break;
                }
            }
        }
    }

    let root_query_type_name = match root_query_type_name {
        Some(name) => name,
        None => {
            return syn::Error::new_spanned(
                schema_path_literal,
                "Failed to find the root query type in the schema. Please define it using 'schema { query: QueryType }' or ensure there is a type named 'Query'.",
            )
                .to_compile_error()
                .into();
        }
    };

    let query_ast = match parse_query::<String>(&query_str) {
        Ok(ast) => ast,
        Err(err) => {
            return syn::Error::new_spanned(
                query_path_literal,
                format!("Failed to parse query: {}", err),
            )
                .to_compile_error()
                .into();
        }
    };

    let mut generated_code = Vec::new();

    // Generate types from the schema
    let schema_types = generate_types_from_schema(&schema_ast);
    generated_code.extend(schema_types);

    // Generate types from the query
    match generate_types_from_query(&query_ast, &schema_ast, &root_query_type_name) {
        Ok(types) => generated_code.extend(types),
        Err(err) => {
            return syn::Error::new_spanned(
                query_path_literal,
                format!("Failed to generate types from query: {}", err),
            )
                .to_compile_error()
                .into();
        }
    }

    let expanded = quote! {
        #(#generated_code)*
    };

    expanded.into()
}

// Generate types...
fn generate_types_from_schema(
    schema_ast: &SchemaDocument<String>,
) -> Vec<proc_macro2::TokenStream> {
    let mut generated_code = Vec::new();
    let mut interfaces = HashMap::new();

    for def in &schema_ast.definitions {
        if let Definition::TypeDefinition(TypeDefinition::Interface(interface_def)) = def {
            interfaces.insert(interface_def.name.clone(), interface_def.clone());
        }
    }

    for def in &schema_ast.definitions {
        if let Definition::TypeDefinition(type_def) = def {
            match type_def {
                TypeDefinition::Object(obj) => {
                    let struct_def = generate_struct(&obj, &interfaces);
                    generated_code.push(struct_def);
                }
                TypeDefinition::Enum(enum_def) => {
                    let enum_def = generate_enum(&enum_def);
                    generated_code.push(enum_def);
                }
                TypeDefinition::InputObject(input_obj) => {
                    let struct_def = generate_input_object_struct(input_obj);
                    generated_code.push(struct_def);
                }
                TypeDefinition::Interface(interface_def) => {
                    let struct_def = generate_interface_struct(&interface_def);
                    generated_code.push(struct_def);
                }
                TypeDefinition::Scalar(scalar_def) => {
                    let scalar_def = generate_scalar(&scalar_def);
                    generated_code.push(scalar_def);
                }
                // todo: handle other types??
                _ => {}
            }
        }
    }

    generated_code
}

fn generate_input_object_struct(
    input_obj: &graphql_parser::schema::InputObjectType<String>,
) -> proc_macro2::TokenStream {
    let struct_name = make_ident(&input_obj.name);
    let fields = input_obj.fields.iter().map(|input_value| {
        let field_name = make_ident(&input_value.name);
        let field_type = map_input_value_type_to_rust(&input_value.value_type);
        let doc_comment = input_value.description.as_deref().unwrap_or("");
        let doc_attr = if !doc_comment.is_empty() {
            quote! { #[doc = #doc_comment] }
        } else {
            quote! {}
        };

        quote! {
            #doc_attr
            pub #field_name: #field_type,
        }
    });

    let doc_comment = input_obj.description.as_deref().unwrap_or("");
    let doc_attr = if !doc_comment.is_empty() {
        quote! { #[doc = #doc_comment] }
    } else {
        quote! {}
    };

    quote! {
        #doc_attr
        #[derive(Debug, Clone, miniserde::Serialize, miniserde::Deserialize)]
        pub struct #struct_name {
            #(#fields)*
        }
    }
}

fn map_input_value_type_to_rust(value_type: &Type<String>) -> proc_macro2::TokenStream {
    map_input_value_type_to_rust_impl(value_type, false)
}

fn map_input_value_type_to_rust_impl(
    value_type: &Type<String>,
    parent_is_non_null: bool,
) -> proc_macro2::TokenStream {
    match value_type {
        Type::NonNullType(inner) => map_input_value_type_to_rust_impl(inner, true),
        Type::ListType(inner) => {
            let inner_type = map_input_value_type_to_rust_impl(inner, false);
            let list_type = quote! { Vec<#inner_type> };
            if parent_is_non_null {
                quote! { #list_type }
            } else {
                quote! { Option<#list_type> }
            }
        }
        Type::NamedType(name) => {
            let ty = map_scalar_type(name);
            if parent_is_non_null {
                quote! { #ty }
            } else {
                quote! { Option<#ty> }
            }
        }
    }
}

fn generate_types_from_query(
    query_ast: &QueryDocument<String>,
    schema_ast: &SchemaDocument<String>,
    root_query_type_name: &str,
) -> Result<Vec<proc_macro2::TokenStream>, String> {
    let mut generated_code = Vec::new();

    let mut type_map = HashMap::new();
    for def in &schema_ast.definitions {
        if let Definition::TypeDefinition(type_def) = def {
            let type_name = match type_def {
                TypeDefinition::Object(obj) => &obj.name,
                TypeDefinition::Interface(interface) => &interface.name,
                TypeDefinition::Enum(enum_def) => &enum_def.name,
                TypeDefinition::Union(union_def) => &union_def.name,
                TypeDefinition::Scalar(scalar_def) => &scalar_def.name,
                TypeDefinition::InputObject(input_obj) => &input_obj.name,
            };
            type_map.insert(type_name.clone(), type_def);
        }
    }

    for def in &query_ast.definitions {
        if let QueryDefinition::Operation(operation) = def {
            match operation {
                graphql_parser::query::OperationDefinition::Query(query) => {
                    let operation_name = query.name.clone().unwrap_or_else(|| "Query".to_string());
                    let struct_name = format!("{}Data", operation_name);

                    let struct_tokens = generate_struct_from_selection_set(
                        &struct_name,
                        &query.selection_set,
                        &type_map,
                        root_query_type_name,
                    )?;
                    generated_code.extend(struct_tokens);
                }
                _ => {
                    // todo: handle mutations and subscription...
                }
            }
        }
    }

    Ok(generated_code)
}

fn generate_struct_from_selection_set(
    struct_name: &str,
    selection_set: &SelectionSet<String>,
    type_map: &HashMap<String, &TypeDefinition<String>>,
    parent_type_name: &str,
) -> Result<Vec<proc_macro2::TokenStream>, String> {
    let mut generated_code = Vec::new();
    let mut fields = Vec::new();

    let parent_type = match type_map.get(parent_type_name) {
        Some(typ) => typ,
        None => {
            return Err(format!(
                "Type '{}' not found in schema when processing struct '{}'",
                parent_type_name, struct_name
            ))
        }
    };

    let field_map = match parent_type {
        TypeDefinition::Object(obj) => obj
            .fields
            .iter()
            .map(|f| (f.name.clone(), f))
            .collect::<HashMap<_, _>>(),
        TypeDefinition::Interface(interface) => interface
            .fields
            .iter()
            .map(|f| (f.name.clone(), f))
            .collect::<HashMap<_, _>>(),
        _ => {
            return Err(format!(
                "Type '{}' is not an object or interface when processing struct '{}'",
                parent_type_name, struct_name
            ))
        }
    };

    for selection in &selection_set.items {
        match selection {
            Selection::Field(field) => {
                let field_name = &field.name;
                if field_name == "__typename" {
                    continue;
                }

                let alias = field.alias.as_ref().unwrap_or(field_name);
                let field_ident = make_ident(alias);
                let field_def = field_map.get(field_name).ok_or_else(|| {
                    format!(
                        "Field '{}' not found on type '{}' when processing struct '{}'",
                        field_name, parent_type_name, struct_name
                    )
                })?;

                let rust_type = if field.selection_set.items.is_empty() {
                    map_graphql_type_to_rust(&field_def.field_type)
                } else {
                    let nested_struct_name = format!("{}{}", struct_name, field_ident.to_string());
                    let field_type_name = get_type_name(&field_def.field_type);

                    match type_map.get(&field_type_name) {
                        Some(TypeDefinition::Object(_)) | Some(TypeDefinition::Interface(_)) => {
                            let nested_tokens = generate_struct_from_selection_set(
                                &nested_struct_name,
                                &field.selection_set,
                                type_map,
                                &field_type_name,
                            )?;
                            generated_code.extend(nested_tokens);
                            let nested_ident = make_ident(&nested_struct_name);
                            quote! { #nested_ident }
                        }
                        Some(TypeDefinition::Union(union_def)) => {
                            if field_type_name == "Merchandise" {
                                let nested_tokens = generate_struct_from_selection_set(
                                    &nested_struct_name,
                                    &field.selection_set,
                                    type_map,
                                    "ProductVariant",
                                )?;
                                generated_code.extend(nested_tokens);
                                let nested_ident = make_ident(&nested_struct_name);
                                quote! { #nested_ident }
                            } else {
                                return Err(format!(
                                    "Union type '{}' is not handled when processing field '{}' on type '{}'",
                                    field_type_name, field_name, parent_type_name
                                ));
                            }
                        }
                        _ => {
                            return Err(format!(
                                "Field '{}' on type '{}' is of non-object type '{}' when processing struct '{}'",
                                field_name, parent_type_name, field_type_name, struct_name
                            ));
                        }
                    }
                };

                fields.push(quote! {
                    pub #field_ident: #rust_type,
                });
            }
            Selection::InlineFragment(inline_fragment) => {
                if let Some(type_condition) = &inline_fragment.type_condition {
                    let type_name = match type_condition {
                        graphql_parser::query::TypeCondition::On(name) => name.as_str(),
                    };
                    let fragment_struct_name = format!("{}On{}", struct_name, type_name);
                    let nested_tokens = generate_struct_from_selection_set(
                        &fragment_struct_name,
                        &inline_fragment.selection_set,
                        type_map,
                        type_name,
                    )?;
                    generated_code.extend(nested_tokens);

                    let fragment_struct_ident = make_ident(&fragment_struct_name);
                    let field_ident = make_ident(&type_name.to_upper_camel_case());
                    fields.push(quote! {
                        pub #field_ident: Option<#fragment_struct_ident>,
                    });
                } else { // todo: handle this case too
                }
            }
            Selection::FragmentSpread(fragment_spread) => {
                // todo: hdle fragment spreads..
            }
        }
    }

    let struct_ident = make_ident(struct_name);

    let struct_def = quote! {
        #[derive(Debug, Clone, miniserde::Serialize, miniserde::Deserialize, PartialEq)]
        pub struct #struct_ident {
            #(#fields)*
        }
    };

    generated_code.push(struct_def);

    Ok(generated_code)
}

fn generate_union(union_def: &UnionType<String>) -> Option<proc_macro2::TokenStream> {
    if union_def.name == "Merchandise" {
        // here, we are handling 'Merchandise' as 'ProductVariant'..., todo: handle CustomProduct
        let alias_code = quote! {
            pub type Merchandise = ProductVariant;
        };
        Some(alias_code)
    } else {
        None
    }
}

fn generate_interface_struct(interface_def: &InterfaceType<String>) -> proc_macro2::TokenStream {
    let struct_name = make_ident(&interface_def.name);
    let fields = interface_def.fields.iter().map(|field| {
        let field_name = make_ident(&field.name);
        let field_type = map_graphql_type_to_rust(&field.field_type);
        let doc_comment = field.description.as_deref().unwrap_or("");
        let doc_attr = if !doc_comment.is_empty() {
            quote! { #[doc = #doc_comment] }
        } else {
            quote! {}
        };

        quote! {
            #doc_attr
            pub #field_name: #field_type,
        }
    });

    let doc_comment = interface_def.description.as_deref().unwrap_or("");
    let doc_attr = if !doc_comment.is_empty() {
        quote! { #[doc = #doc_comment] }
    } else {
        quote! {}
    };

    quote! {
        #doc_attr
        #[derive(Debug, Clone, miniserde::Serialize, miniserde::Deserialize, PartialEq)]
        pub struct #struct_name {
            #(#fields)*
        }
    }
}

fn map_graphql_type_to_rust(field_type: &Type<String>) -> proc_macro2::TokenStream {
    match field_type {
        Type::NamedType(name) => {
            let name = if name == "Merchandise" {
                "ProductVariant"
            } else {
                name
            };
            let ty = map_scalar_type(name);
            quote! { Option<#ty> }
        }
        Type::NonNullType(inner) => {
            let inner_type = map_graphql_type_to_rust(inner);
            quote! { #inner_type }
        }
        Type::ListType(inner) => {
            let inner_type = map_graphql_type_to_rust(inner);
            quote! { Option<Vec<#inner_type>> }
        }
    }
}

fn map_scalar_type(name: &str) -> proc_macro2::TokenStream {
    match name {
        "Int" => quote! { i32 },
        "Float" => quote! { f64 },
        "String" => quote! { String },
        "Boolean" => quote! { bool },
        "ID" => quote! { String },
        "Merchandise" => {
            let ident = make_ident("ProductVariant");
            quote! { #ident }
        }
        _ => {
            let ident = make_ident(name);
            quote! { #ident }
        }
    }
}

fn get_type_name(field_type: &Type<String>) -> String {
    match field_type {
        Type::NamedType(name) => name.clone(),
        Type::NonNullType(inner) => get_type_name(inner),
        Type::ListType(inner) => get_type_name(inner),
    }
}

fn make_ident(name: &str) -> Ident {
    let sanitized_name = sanitize_identifier(name);
    if is_rust_keyword(&sanitized_name) {
        Ident::new_raw(&sanitized_name, Span::call_site())
    } else {
        Ident::new(&sanitized_name, Span::call_site())
    }
}

fn is_rust_keyword(ident: &str) -> bool {
    matches!(
        ident,
        // keywords
        "as" | "break" | "const" | "continue" | "crate" | "else" |
        "enum" | "extern" | "false" | "fn" | "for" | "if" | "impl" |
        "in" | "let" | "loop" | "match" | "mod" | "move" | "mut" |
        "pub" | "ref" | "return" | "self" | "Self" | "static" |
        "struct" | "super" | "trait" | "true" | "type" | "unsafe" |
        "use" | "where" | "while" | "async" | "await" | "dyn" |
        // reserved keywords
        "abstract" | "become" | "box" | "do" | "final" | "macro" |
        "override" | "priv" | "try" | "typeof" | "unsized" |
        "virtual" | "yield" | "union"
    )
}

fn generate_struct(
    obj: &graphql_parser::schema::ObjectType<String>,
    interfaces: &HashMap<String, InterfaceType<String>>,
) -> proc_macro2::TokenStream {
    let struct_name = make_ident(&obj.name);

    let mut fields = Vec::new();

    for interface_name in &obj.implements_interfaces {
        if let Some(interface_def) = interfaces.get(interface_name) {
            let interface_fields = interface_def.fields.iter().map(|field| {
                let field_name = make_ident(&field.name);
                let field_type = map_graphql_type_to_rust(&field.field_type);
                let doc_comment = field.description.as_deref().unwrap_or("");
                let doc_attr = if !doc_comment.is_empty() {
                    quote! { #[doc = #doc_comment] }
                } else {
                    quote! {}
                };

                quote! {
                    #doc_attr
                    pub #field_name: #field_type,
                }
            });
            fields.extend(interface_fields);
        }
    }

    let object_fields = obj.fields.iter().map(|field| {
        let field_name = make_ident(&field.name);
        let field_type = map_graphql_type_to_rust(&field.field_type);
        let doc_comment = field.description.as_deref().unwrap_or("");
        let doc_attr = if !doc_comment.is_empty() {
            quote! { #[doc = #doc_comment] }
        } else {
            quote! {}
        };

        quote! {
            #doc_attr
            pub #field_name: #field_type,
        }
    });

    fields.extend(object_fields);

    let doc_comment = obj.description.as_deref().unwrap_or("");
    let doc_attr = if !doc_comment.is_empty() {
        quote! { #[doc = #doc_comment] }
    } else {
        quote! {}
    };

    let unique_fields = remove_duplicate_fields(fields);

    quote! {
        #doc_attr
        #[derive(Debug, Clone, miniserde::Serialize, miniserde::Deserialize, PartialEq)]
        pub struct #struct_name {
            #(#unique_fields)*
        }
    }
}

fn remove_duplicate_fields(fields: Vec<proc_macro2::TokenStream>) -> Vec<proc_macro2::TokenStream> {
    use std::collections::HashSet;
    let mut seen = HashSet::new();
    let mut unique_fields = Vec::new();

    for field in fields {
        let field_str = field.to_string();
        if let Some(name_start) = field_str.find("pub ") {
            if let Some(name_end) = field_str[name_start + 4..].find(':') {
                let name = &field_str[name_start + 4..name_start + 4 + name_end];
                if seen.insert(name.to_string()) {
                    unique_fields.push(field);
                }
            }
        }
    }

    unique_fields
}

fn generate_enum(enum_def: &EnumType<String>) -> proc_macro2::TokenStream {
    let enum_name = make_ident(&enum_def.name);
    let variants = enum_def.values.iter().map(|enum_value| {
        let variant_name = make_ident(&enum_value.name);

        let doc_comment = enum_value.description.as_deref().unwrap_or("");
        let doc_attr = if !doc_comment.is_empty() {
            quote! { #[doc = #doc_comment] }
        } else {
            quote! {}
        };

        quote! {
            #doc_attr
            #variant_name,
        }
    });

    let doc_comment = enum_def.description.as_deref().unwrap_or("");
    let doc_attr = if !doc_comment.is_empty() {
        quote! { #[doc = #doc_comment] }
    } else {
        quote! {}
    };

    quote! {
        #doc_attr
        #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
        pub enum #enum_name {
            #(#variants)*
        }
    }
}

fn generate_scalar(scalar_def: &ScalarType<String>) -> proc_macro2::TokenStream {
    let scalar_name = make_ident(&scalar_def.name);

    let scalar_type = match scalar_def.name.as_str() {
        "Handle" => {
            quote! {
                pub type #scalar_name = String;
            }
        }
        "CountryCode" => {
            quote! {
                pub type #scalar_name = String;
            }
        }
        _ => {
            quote! {
                pub type #scalar_name = String;
            }
        }
    };

    scalar_type
}

fn sanitize_identifier(name: &str) -> String {
    let mut ident = String::new();
    for (i, ch) in name.chars().enumerate() {
        if i == 0 {
            if ch.is_ascii_alphabetic() || ch == '_' {
                ident.push(ch);
            } else {
                ident.push('_');
            }
        } else {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                ident.push(ch);
            } else {
                ident.push('_');
            }
        }
    }
    ident
}
