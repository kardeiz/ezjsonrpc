#![recursion_limit="2048"]

extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::Span;
use proc_macro_hack::proc_macro_hack;
use quote::quote;

use syn::*;
use heck::*;

#[proc_macro_attribute]
pub fn jsonrpc_method(attrs: TokenStream, item: TokenStream) -> TokenStream { 
    
    // let item_cloned = item.clone();

    let attr_args = parse_macro_input!(attrs as AttributeArgs);
    let item_fn = parse_macro_input!(item as ItemFn);

    let params = item_fn.decl.inputs.iter()
        .filter_map(|x| match *x { FnArg::Captured(ref y) => Some(y), _ => None })
        .filter_map(|x| match x.pat { Pat::Ident(ref y) => Some(y), _ => None })
        .map(|x| &x.ident )
        .collect::<Vec<_>>();

    let so_name = Ident::new(
        &format!(
            "jsonrpc_method_{}",
            item_fn.ident.to_string().trim_left_matches("r#").to_owned()
        ).to_camel_case(),
        Span::call_site()
    );


    let dummy_const = Ident::new(
        &format!("_IMPL_CALLER_FOR_{}", so_name.to_string()),
        Span::call_site()
    );

    let item_fn_ident = &item_fn.ident;

    let item_fn_ident_s = item_fn_ident.to_string();

    let item_fn_ident_s = &item_fn_ident_s;

    let params = &params;

    let param_names = params.iter().map(|x| x.to_string() ).collect::<Vec<_>>();    

    let param_names = &param_names;

    let params_len = params.len();

    let extract_positional = {

        let tys = (0..params_len).map(|i| Ident::new(&format!("T{}", i), Span::call_site())).collect::<Vec<_>>();
        let gen = tys.iter().map(|x| quote!(#x: _ezjsonrpc::exp::serde::de::DeserializeOwned) ).collect::<Vec<_>>();

        let ts = (0..params_len).map(|i| Ident::new(&format!("t{}", i), Span::call_site())).collect::<Vec<_>>();

        let mut ts_rev = ts.clone();
        ts_rev.reverse();

        let exprs = (0..params_len).map(|i| quote!(_ezjsonrpc::exp::serde_json::from_value(vals.pop().unwrap())?) ).collect::<Vec<_>>();

        quote!{ 
            fn extract_positional<#(#gen),*>(mut vals: Vec<_ezjsonrpc::exp::Value>) -> Result<(#(#tys),*), _ezjsonrpc::exp::serde_json::Error> {
                if vals.len() != #params_len {
                    return Err(_ezjsonrpc::exp::serde::de::Error::custom("Invalid vec"));
                }
                let (#(#ts_rev),*) = (#(#exprs),*);
                Ok((#(#ts),*))
            }
            
        }        
    };

    let extract_named = {

        let tys = (0..params_len).map(|i| Ident::new(&format!("T{}", i), Span::call_site())).collect::<Vec<_>>();
        let gen = tys.iter().map(|x| quote!(#x: _ezjsonrpc::exp::serde::de::DeserializeOwned) ).collect::<Vec<_>>();

        let ts = (0..params_len).map(|i| Ident::new(&format!("t{}", i), Span::call_site())).collect::<Vec<_>>();

        let names = (0..params_len).map(|i| Ident::new(&format!("n{}", i), Span::call_site())).collect::<Vec<_>>();

        let names_and_tys = names.iter().map(|x| quote!(#x: &'static str) ).collect::<Vec<_>>();

        let mains = ts.iter().zip(names.iter())
            .map(|(t, n)| quote!{
                let #t = if let Some(val) = map.remove(#n) {
                    _ezjsonrpc::exp::serde_json::from_value(val)?
                } else {
                    return Err(_ezjsonrpc::exp::serde::de::Error::custom("Invalid map"));
                };
            })
            .collect::<Vec<_>>();

        quote!{ 
            fn extract_named<#(#gen),*>(mut map: _ezjsonrpc::exp::serde_json::Map<String, _ezjsonrpc::exp::Value>, #(#names_and_tys),*) 
                -> Result<(#(#tys),*), _ezjsonrpc::exp::serde_json::Error> {                               
                #(#mains)*
                Ok((#(#ts),*))
            }
            
        }        
    };

    let out_no_params = quote!{
        #item_fn
        pub struct #so_name;
        const #dummy_const: () = {
            extern crate ezjsonrpc as _ezjsonrpc;

            impl _ezjsonrpc::Method for #so_name {
                fn name(&self) -> &'static str { #item_fn_ident_s }
            }

            impl _ezjsonrpc::Caller for #so_name {
                fn request(&self, request: _ezjsonrpc::Request) -> _ezjsonrpc::RequestFuture {
                    if request.params.as_ref()
                        .map(|x| x.as_object().map(|y| !y.is_empty()).unwrap_or(false) || 
                            x.as_array().map(|y| !y.is_empty()).unwrap_or(false) )
                        .unwrap_or(false) {
                        request.handle(_ezjsonrpc::exp::future_err::<(), _>(_ezjsonrpc::Error::invalid_method_parameters()))
                    } else {
                        request.handle(#item_fn_ident())
                    }
                }
                fn notification(&self, mut notification: _ezjsonrpc::Notification) -> _ezjsonrpc::NotificationFuture {
                    if notification.params.as_ref()
                        .map(|x| x.as_object().map(|y| !y.is_empty()).unwrap_or(false) || 
                            x.as_array().map(|y| !y.is_empty()).unwrap_or(false) )
                        .unwrap_or(false) {
                        notification.handle(_ezjsonrpc::exp::future_err::<(), _>(_ezjsonrpc::Error::invalid_method_parameters()))
                    } else {
                        notification.handle(#item_fn_ident())
                    }
                }
            }
        };
    };

    let out_with_params = quote!{
        #item_fn
        pub struct #so_name;
        const #dummy_const: () = {
            extern crate ezjsonrpc as _ezjsonrpc;

            impl _ezjsonrpc::Method for #so_name {
                fn name(&self) -> &'static str { #item_fn_ident_s }
            }

            impl _ezjsonrpc::Caller for #so_name {
                fn request(&self, mut request: _ezjsonrpc::Request) -> _ezjsonrpc::RequestFuture {
                    match request.params.take() {
                        Some(value) => {
                            match value {
                                _ezjsonrpc::exp::Value::Object(map) => {
                                    #extract_named
                                    if let Ok((#(#params),*)) = extract_named(map, #(#param_names),*) {
                                        return request.handle(#item_fn_ident(#(#params),*));
                                    }
                                },
                                _ezjsonrpc::exp::Value::Array(vals) => {
                                    #extract_positional
                                    if let Ok((#(#params),*)) = extract_positional(vals) {
                                        return request.handle(#item_fn_ident(#(#params),*));
                                    }
                                },
                                _ => {}
                            }
                        },
                        _ => {}
                    }
                    request.handle(_ezjsonrpc::exp::future_err::<(), _>(_ezjsonrpc::Error::invalid_method_parameters()))
                }
                fn notification(&self, mut notification: _ezjsonrpc::Notification) -> _ezjsonrpc::NotificationFuture {
                    match notification.params.take() {
                        Some(value) => {
                            match value {
                                _ezjsonrpc::exp::Value::Object(map) => {
                                    #extract_named
                                    if let Ok((#(#params),*)) = extract_named(map, #(#param_names),*) {
                                        return notification.handle(#item_fn_ident(#(#params),*));
                                    }
                                },
                                _ezjsonrpc::exp::Value::Array(vals) => {
                                    #extract_positional
                                    if let Ok((#(#params),*)) = extract_positional(vals) {
                                        return notification.handle(#item_fn_ident(#(#params),*));
                                    }
                                },
                                _ => {}
                            }
                        },
                        _ => {}
                    }
                    notification.handle(_ezjsonrpc::exp::future_err::<(), _>(_ezjsonrpc::Error::invalid_method_parameters()))
                }
            }

        };
    };

    let out = if params.is_empty() {
        out_no_params
    } else {
        out_with_params
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
                        "jsonrpc_method_{}",
                        last_seg.value().ident.to_string().trim_left_matches("r#").to_owned()
                    ).to_camel_case(),
                    Span::call_site()
                );
            }
            let so_path = &so_path;
            quote!(Box::new(#so_path))
        })
        .collect::<Vec<_>>();

    let out = quote!(vec![#(#parts),*]);

    out.into()
}