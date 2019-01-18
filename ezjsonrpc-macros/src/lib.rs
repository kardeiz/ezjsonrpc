#![recursion_limit = "2048"]

extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::Span;
use proc_macro_hack::proc_macro_hack;
use quote::quote;

use heck::*;
use syn::*;

#[proc_macro_attribute]
pub fn jsonrpc_methods(attrs: TokenStream, item: TokenStream) -> TokenStream {
    let mut item_impl = parse_macro_input!(item as ItemImpl);

    if item_impl.trait_.is_some() {
        panic!("Must not be implementing some trait");
    }

    let dummy_const = if let &Type::Path(ref tp) = item_impl.self_ty.as_ref() {
        let name =
            tp.path.segments.last().unwrap().value().ident.to_string().to_shouty_snake_case();
        Ident::new(&format!("_IMPL_JSONRPC_CALLS_FOR_{}", name), Span::call_site())
    } else {
        panic!("Couldn't derive name");
    };

    let mut new_methods = Vec::new();

    for method in item_impl.items.iter_mut().filter_map(|x| match *x {
        ImplItem::Method(ref mut y) => Some(y),
        _ => None
    }) {
        let to_match: Path = parse_quote!(jsonrpc);

        let (jsonrpc_attrs, preserved_attrs): (Vec<Attribute>, Vec<Attribute>) =
            method.attrs.drain(..).partition(|x| x.path == to_match);

        method.attrs = preserved_attrs;

        let params = &method
            .sig
            .decl
            .inputs
            .iter()
            .filter_map(|x| match *x {
                FnArg::Captured(ref y) => Some(y),
                _ => None
            })
            .filter_map(|x| match x.pat {
                Pat::Ident(ref y) => Some(y),
                _ => None
            })
            .map(|x| &x.ident)
            .collect::<Vec<_>>();

        let wrapped_fn_name = Ident::new(
            &format!(
                "jsonrpc_call_{}",
                method.sig.ident.to_string().trim_left_matches("r#").to_owned()
            ),
            Span::call_site()
        );

        let item_fn_ident = &method.sig.ident;
        let item_fn_ident_s = &item_fn_ident.to_string();

        let param_names = &params.iter().map(|x| x.to_string()).collect::<Vec<_>>();

        let extract_positional = extract_positional(param_names.len());

        let extract_named = extract_named(param_names.len());

        let out: ImplItem = {
            if param_names.len() == 0 {
                parse_quote!{
                    pub fn #wrapped_fn_name() -> (&'static str, Box<_ezjsonrpc::Method<Self>>) {
                        let rt = |state: &Self, params: Option<_ezjsonrpc::exp::serde_json::Value>| {
                            if params.as_ref()
                                .map(|x| x.as_object().map(|y| !y.is_empty()).unwrap_or(false) ||
                                    x.as_array().map(|y| !y.is_empty()).unwrap_or(false) )
                                .unwrap_or(false) {
                                Err(_ezjsonrpc::Error::invalid_method_parameters())
                            } else {
                                Ok(Self::#item_fn_ident(state))
                            }
                        };
                        (#item_fn_ident_s, rt.into())
                    }
                }
            } else {
                parse_quote!{
                    pub fn #wrapped_fn_name() -> (&'static str, Box<_ezjsonrpc::Method<Self>>) {
                        let rt = |state: &Self, params: Option<_ezjsonrpc::exp::serde_json::Value>| {
                            match params {
                                Some(_ezjsonrpc::exp::serde_json::Value::Object(map)) => {
                                    #extract_named
                                    if let Ok((#(#params),*)) = extract(map, #(#param_names),*) {
                                        return Ok(Self::#item_fn_ident(state, #(#params),*));
                                    }
                                },
                                Some(_ezjsonrpc::exp::serde_json::Value::Array(vals)) => {
                                    #extract_positional
                                    if let Ok((#(#params),*)) = extract(vals) {
                                        return Ok(Self::#item_fn_ident(state, #(#params),*));
                                    }
                                },
                                _ => {}
                            }
                            return Err(_ezjsonrpc::Error::invalid_method_parameters());
                        };
                        (#item_fn_ident_s, rt.into())
                    }
                }
            }
        };

        new_methods.push(out);
    }

    let mut item_impl_cloned = item_impl.clone();

    item_impl_cloned.items = new_methods;

    let out = quote!{
        #item_impl
        const #dummy_const: () = {
            extern crate ezjsonrpc as _ezjsonrpc;
            #item_impl_cloned
        };
    };

    out.into()
}

#[proc_macro_hack]
pub fn methods(item: TokenStream) -> TokenStream {
    use syn::parse::Parser;

    let paths = <punctuated::Punctuated<Path, token::Comma>>::parse_terminated.parse(item).unwrap();

    let parts = paths
        .iter()
        .map(|path| {
            let mut so_path = path.clone();
            {
                let mut last_seg = so_path.segments.last_mut().unwrap();
                last_seg.value_mut().ident = Ident::new(
                    &format!(
                        "jsonrpc_call_{}",
                        last_seg.value().ident.to_string().trim_left_matches("r#").to_owned()
                    ),
                    Span::call_site()
                );
            }
            let so_path = &so_path;
            quote!(#so_path())
        })
        .collect::<Vec<_>>();

    let out = quote!(vec![#(#parts),*]);

    out.into()
}

fn extract_positional(up_to: usize) -> proc_macro2::TokenStream {
    let tys =
        (0..up_to).map(|i| Ident::new(&format!("T{}", i), Span::call_site())).collect::<Vec<_>>();
    let gen = tys
        .iter()
        .map(|x| quote!(#x: _ezjsonrpc::exp::serde::de::DeserializeOwned))
        .collect::<Vec<_>>();

    let ts =
        (0..up_to).map(|i| Ident::new(&format!("t{}", i), Span::call_site())).collect::<Vec<_>>();

    let mut ts_rev = ts.clone();
    ts_rev.reverse();

    let exprs = (0..up_to)
        .map(|i| quote!(_ezjsonrpc::exp::serde_json::from_value(vals.pop().unwrap())?))
        .collect::<Vec<_>>();

    quote!{
        fn extract<#(#gen),*>(mut vals: Vec<_ezjsonrpc::exp::serde_json::Value>) -> Result<(#(#tys),*), _ezjsonrpc::exp::serde_json::Error> {
            if vals.len() != #up_to {
                return Err(_ezjsonrpc::exp::serde::de::Error::custom("Invalid vec"));
            }
            let (#(#ts_rev),*) = (#(#exprs),*);
            Ok((#(#ts),*))
        }
    }
}

fn extract_named(up_to: usize) -> proc_macro2::TokenStream {
    let tys =
        (0..up_to).map(|i| Ident::new(&format!("T{}", i), Span::call_site())).collect::<Vec<_>>();
    let gen = tys
        .iter()
        .map(|x| quote!(#x: _ezjsonrpc::exp::serde::de::DeserializeOwned))
        .collect::<Vec<_>>();

    let ts =
        (0..up_to).map(|i| Ident::new(&format!("t{}", i), Span::call_site())).collect::<Vec<_>>();

    let names =
        (0..up_to).map(|i| Ident::new(&format!("n{}", i), Span::call_site())).collect::<Vec<_>>();

    let names_and_tys = names.iter().map(|x| quote!(#x: &'static str)).collect::<Vec<_>>();

    let mains = ts
        .iter()
        .zip(names.iter())
        .map(|(t, n)| {
            quote!{
                let #t = if let Some(val) = map.remove(#n) {
                    _ezjsonrpc::exp::serde_json::from_value(val)?
                } else {
                    return Err(_ezjsonrpc::exp::serde::de::Error::custom("Invalid map"));
                };
            }
        })
        .collect::<Vec<_>>();

    quote!{
        fn extract<#(#gen),*>(mut map: _ezjsonrpc::exp::serde_json::Map<String, _ezjsonrpc::exp::serde_json::Value>, #(#names_and_tys),*)
            -> Result<(#(#tys),*), _ezjsonrpc::exp::serde_json::Error> {
            #(#mains)*
            Ok((#(#ts),*))
        }
    }
}
