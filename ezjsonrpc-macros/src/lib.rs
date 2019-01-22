#![recursion_limit = "2048"]

extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::Span;
use proc_macro_hack::proc_macro_hack;
use quote::quote;

use syn::*;

#[proc_macro_attribute]
pub fn jsonrpc_method(attrs: TokenStream, item: TokenStream) -> TokenStream {
    
    let method = parse_macro_input!(item as ImplItemMethod);

    let attrs = parse_macro_input!(attrs as AttributeArgs);

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

    let params_fn_name = Ident::new(
        &format!(
            "jsonrpc_params_for_{}",
            method.sig.ident.to_string().trim_left_matches("r#").to_owned()
        ),
        Span::call_site()
    );

    let item_fn_ident = &method.sig.ident;

    let mut item_fn_ident_s = item_fn_ident.to_string();

    if let Some(rename) = attrs.iter()
        .filter_map(|x| match x { NestedMeta::Meta(y) => Some(y), _ => None })
        .filter_map(|x| match x { Meta::NameValue(y) => Some(y), _ => None })
        .find(|x| x.ident == "rename" )
        .and_then(|x| match x.lit { Lit::Str(ref y) => Some(y), _ => None }) {
        item_fn_ident_s = rename.value();
    }

    let mut mappings = attrs.iter()
        .filter_map(|x| match x { NestedMeta::Meta(y) => Some(y), _ => None })
        .filter_map(|x| match x { Meta::List(y) => Some(y), _ => None })
        .filter(|x| x.ident == "rename_args" )
        .flat_map(|x| x.nested.iter() )
        .filter_map(|x| match x { NestedMeta::Meta(y) => Some(y), _ => None })
        .filter_map(|x| match x { Meta::NameValue(y) => Some(y), _ => None })
        .filter_map(|x| match x.lit { Lit::Str(ref y) => Some((x.ident.to_string(), y.value())), _ => None })
        .collect::<std::collections::HashMap<String, String>>();
    
    let param_names = &params.iter().map(|id| {
        let name = id.to_string();
        mappings.remove(&name).unwrap_or_else(|| name)
    }).collect::<Vec<_>>();

    let extract_positional = extract_positional(param_names.len());
    let extract_named = extract_named(param_names.len());

    let params_fn: ImplItemMethod = {
        if param_names.len() == 0 {
            parse_quote!{
                pub fn #params_fn_name() -> (&'static str, Box<std::any::Any>, std::marker::PhantomData<Self>) {
                    extern crate ezjsonrpc as _ezjsonrpc;
                    let rt = |state: &Self, params: Option<_ezjsonrpc::exp::serde_json::Value>| {
                        if params.as_ref()
                            .map(|x| x.as_object().map(|y| !y.is_empty()).unwrap_or(false) ||
                                x.as_array().map(|y| !y.is_empty()).unwrap_or(false) )
                            .unwrap_or(false) {
                            Err(_ezjsonrpc::Error::invalid_params())
                        } else {
                            Ok(Self::#item_fn_ident(state))
                        }
                    };

                    let boxed_method = _ezjsonrpc::BoxedMethod::from(rt);

                    (#item_fn_ident_s, Box::new(boxed_method) as Box<std::any::Any>, std::marker::PhantomData::<Self>)
                }
            }
        } else {
            parse_quote!{
                pub fn #params_fn_name() -> (&'static str, Box<std::any::Any>, std::marker::PhantomData<Self>) {
                    extern crate ezjsonrpc as _ezjsonrpc;
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
                        return Err(_ezjsonrpc::Error::invalid_params());
                    };

                    let boxed_method = _ezjsonrpc::BoxedMethod::from(rt);

                    (#item_fn_ident_s, Box::new(boxed_method) as Box<std::any::Any>, std::marker::PhantomData::<Self>)
                }
            }
        }
    };

    let out = quote!{
        #method
        #params_fn
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
                        "jsonrpc_params_for_{}",
                        last_seg.value().ident.to_string().trim_left_matches("r#").to_owned()
                    ),
                    Span::call_site()
                );
            }
            let so_path = &so_path;
            quote!({
                extern crate ezjsonrpc as _ezjsonrpc;
                let (name, any_box, phantom_data) = #so_path();
                (name, _ezjsonrpc::utils::specify(any_box, phantom_data))
            })
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
        .map(|_| quote!(_ezjsonrpc::exp::serde_json::from_value(vals.pop().unwrap())?))
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
