use proc_macro::TokenStream;
use syn::{DeriveInput, Fields};

/// Derive `Trace` trait.
#[proc_macro_derive(Trace)]
pub fn trace_derive(tokens: TokenStream) -> TokenStream {
    let ast = match syn::parse(tokens) {
        Ok(ast) => ast,
        Err(err) => return err.into_compile_error().into(),
    };

    impl_trace(ast)
}

fn impl_trace(ast: DeriveInput) -> TokenStream {
    use quote::quote;
    use syn::Data;

    let name = &ast.ident;
    let (impl_generics, type_generics, where_clause) = ast.generics.split_for_impl();
    let generic_type_names = ast.generics.type_params().map(|t| &t.ident);
    let where_clause = match where_clause {
        Some(where_clause) => {
            let where_token = &where_clause.where_token;
            let predicates = &where_clause.predicates;

            quote! { #where_token #(#generic_type_names: ::gc::Trace,)* #predicates }
        }
        None => quote! {where #(#generic_type_names: ::gc::Trace,)*},
    };

    let branches = match &ast.data {
        Data::Struct(data) => {
            let ident = name;
            let branch = branch(&data.fields);
            vec![quote! {
                #ident #branch
            }]
        }
        Data::Enum(data) => {
            if data.variants.is_empty() {
                // References to empty enums are considered inhabited,
                // adding a fictional branch is the easiest way to handle that.
                vec![quote! {
                    _ => {},
                }]
            } else {
                data.variants
                    .iter()
                    .map(|variant| {
                        let ident = &variant.ident;
                        let branch = branch(&variant.fields);
                        quote! { #name::#ident #branch }
                    })
                    .collect()
            }
        }
        Data::Union(_) => todo!(),
    };

    let tokens = quote! {
        impl #impl_generics ::gc::Trace for #name #type_generics #where_clause {
            fn trace(&self, _collector: &mut ::gc::Collector) {
                match self {
                    #(#branches)*
                }
            }
        }
    };

    tokens.into()
}

fn branch(fields: &Fields) -> proc_macro2::TokenStream {
    use quote::{format_ident, quote};

    match fields {
        Fields::Named(fields) => {
            let original_idents = fields
                .named
                .iter()
                .map(|field| field.ident.as_ref().unwrap());
            let idents: Vec<_> = (0..fields.named.len())
                .map(|i| format_ident!("t{}", i))
                .collect();

            quote! {
                { #(#original_idents: #idents,)* } => {
                    #(::gc::Trace::trace(#idents, _collector);)*
                }
            }
        }
        Fields::Unnamed(fields) => {
            let idents: Vec<_> = fields
                .unnamed
                .iter()
                .enumerate()
                .map(|(i, _field)| format_ident!("t{}", i))
                .collect();
            quote! {
                ( #(#idents,)* ) => {
                    #(::gc::Trace::trace(#idents, _collector);)*
                }
            }
        }
        Fields::Unit => {
            quote! { => {} }
        }
    }
}

#[test]
fn intended_targets() {
    let tests = trybuild::TestCases::new();
    tests.pass("tests/ui/*.rs");
}
