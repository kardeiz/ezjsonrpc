#![recursion_limit = "2048"]

#[macro_use]
extern crate serde_derive;

pub use ezjsonrpc_macros::*;

use std::{borrow::Cow, collections::HashMap};

use futures::{
    future::{ok as future_ok, Either as EitherFuture},
    Future, IntoFuture
};

use proc_macro_hack::proc_macro_hack;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde_json::Value;

#[proc_macro_hack]
pub use ezjsonrpc_macros::methods;

#[derive(Default, Debug)]
pub struct V2;

impl Serialize for V2 {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where S: Serializer {
        "2.0".serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for V2 {
    fn deserialize<D>(deserializer: D) -> Result<V2, D::Error>
    where D: Deserializer<'de> {
        let s: std::borrow::Cow<str> = Deserialize::deserialize(deserializer)?;
        if s == "2.0" {
            Ok(V2)
        } else {
            Err(serde::de::Error::custom("Could not deserialize V2"))
        }
    }
}

#[derive(Debug, Clone)]
pub enum Id {
    Num(i64),
    Str(Cow<'static, str>),
    Null
}

impl From<i64> for Id {
    fn from(t: i64) -> Self { Id::Num(t) }
}

impl From<String> for Id {
    fn from(t: String) -> Self { Id::Str(t.into()) }
}

impl From<&'static str> for Id {
    fn from(t: &'static str) -> Self { Id::Str(t.into()) }
}

impl Default for Id {
    fn default() -> Id { Id::Null }
}

impl Serialize for Id {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where S: Serializer {
        match *self {
            Id::Num(ref num) => num.serialize(serializer),
            Id::Str(ref s) => s.serialize(serializer),
            Id::Null => serializer.serialize_none()
        }
    }
}

impl<'de> Deserialize<'de> for Id {
    fn deserialize<D>(deserializer: D) -> Result<Id, D::Error>
    where D: Deserializer<'de> {
        #[derive(Serialize, Deserialize)]
        #[serde(untagged)]
        pub enum PresentId {
            Num(i64),
            Str(Cow<'static, str>)
        }

        #[derive(Serialize, Deserialize)]
        pub struct WrappedPresentId(Option<PresentId>);

        let out = match WrappedPresentId::deserialize(deserializer)? {
            WrappedPresentId(Some(PresentId::Num(num))) => Id::Num(num),
            WrappedPresentId(Some(PresentId::Str(s))) => Id::Str(s),
            WrappedPresentId(None) => Id::Null
        };

        Ok(out)
    }
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(untagged)]
pub enum Request {
    HavingId { jsonrpc: V2, method: Cow<'static, str>, params: Option<Value>, id: Id },
    Notification { jsonrpc: V2, method: Cow<'static, str>, params: Option<Value> }
}

impl Request {
    pub fn is_notification(&self) -> bool {
        match self {
            Request::HavingId { .. } => false,
            Request::Notification { .. } => true
        }
    }

    pub fn method(&self) -> &str {
        match self {
            Request::HavingId { ref method, .. } => method,
            Request::Notification { ref method, .. } => method
        }
    }

    pub fn params_mut(&mut self) -> &mut Option<Value> {
        match self {
            Request::HavingId { ref mut params, .. } => params,
            Request::Notification { ref mut params, .. } => params
        }
    }

    pub fn with_id<I: Into<Id>>(self, id: I) -> Self {
        match self {
            Request::HavingId { jsonrpc, method, params, .. } => {
                Request::HavingId { jsonrpc, method, params, id: id.into() }
            }
            Request::Notification { jsonrpc, method, params } => {
                Request::HavingId { jsonrpc, method, params, id: id.into() }
            }
        }
    }

    pub fn with_method<I: Into<Cow<'static, str>>>(self, method: I) -> Self {
        match self {
            Request::HavingId { jsonrpc, params, id, .. } => {
                Request::HavingId { jsonrpc, method: method.into(), params, id }
            }
            Request::Notification { jsonrpc, params, .. } => {
                Request::Notification { jsonrpc, method: method.into(), params }
            }
        }
    }

    pub fn with_params<I: Into<Value>>(self, params: I) -> Self {
        match self {
            Request::HavingId { jsonrpc, method, id, .. } => {
                Request::HavingId { jsonrpc, method, params: Some(params.into()), id }
            }
            Request::Notification { jsonrpc, method, .. } => {
                Request::Notification { jsonrpc, method, params: Some(params.into()) }
            }
        }
    }

    pub fn having_id() -> Self {
        Request::HavingId { jsonrpc: V2, method: "".into(), params: None, id: Id::Null }
    }

    pub fn notification() -> Self {
        Request::Notification { jsonrpc: V2, method: "".into(), params: None }
    }
}

#[derive(Serialize, Deserialize, Default, Debug)]
pub struct Error {
    pub code: i64,
    pub message: String,
    pub data: Option<Value>
}

impl Error {
    pub fn invalid_params() -> Self {
        Error { code: -32602, message: "Invalid params".into(), data: None }
    }

    pub fn method_not_found() -> Self {
        Error { code: -32601, message: "Method not found".into(), data: None }
    }

    pub fn parse_error() -> Self {
        Error { code: -32700, message: "Parse error".into(), data: None }
    }

    pub fn invalid_request() -> Self {
        Error { code: -32600, message: "Invalid Request".into(), data: None }
    }
}

impl<T: std::fmt::Display> From<T> for Error {
    fn from(t: T) -> Self { Error { message: format!("{}", t), ..Error::default() } }
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(untagged)]
pub enum Response {
    Result { jsonrpc: V2, result: Value, id: Id },
    Error { jsonrpc: V2, error: Error, id: Id },
    Empty
}

impl Response {
    fn result(result: Value, opt_id: Option<Id>) -> Self {
        opt_id
            .map(|id| Response::Result { jsonrpc: V2, result, id })
            .unwrap_or_else(|| Response::Empty)
    }

    fn error(error: Error, opt_id: Option<Id>) -> Self {
        opt_id
            .map(|id| Response::Error { jsonrpc: V2, error, id })
            .unwrap_or_else(|| Response::Empty)
    }
}

type MethodLike<T> =
    Box<Fn(&T, Option<Value>) -> Box<Future<Item = Value, Error = Error> + Send> + Send + Sync>;

pub struct BoxedMethod<T>(pub MethodLike<T>);

impl<H, I, S, E, T> From<H> for BoxedMethod<T>
where
    I: IntoFuture<Item = S, Error = E> + 'static + Send,
    I::Future: 'static + Send,
    S: serde::Serialize,
    E: Into<Error>,
    H: Fn(&T, Option<Value>) -> Result<I, Error> + Sync + Send + 'static
{
    fn from(t: H) -> Self {
        BoxedMethod(Box::new(
            move |state: &T,
                  params: Option<Value>|
                  -> Box<Future<Item = Value, Error = Error> + Send> {
                let rt = t(state, params).into_future().and_then(|s| {
                    s.into_future().map_err(|e| e.into()).and_then(|r| {
                        serde_json::to_value(r)
                            .into_future()
                            .map_err(|e| Error::from(e.to_string()))
                    })
                });
                Box::new(rt)
            }
        ))
    }
}

impl<T> Method<T> for BoxedMethod<T> {
    fn call(
        &self,
        state: &T,
        params: Option<Value>
    ) -> Box<Future<Item = Value, Error = Error> + Send>
    {
        (&self.0)(state, params)
    }
}

pub trait Method<T> {
    fn call(
        &self,
        state: &T,
        params: Option<Value>
    ) -> Box<Future<Item = Value, Error = Error> + Send>;
}

#[derive(Deserialize)]
#[serde(untagged)]
pub enum OneOrManyValues {
    Many(Vec<Value>),
    One(Value)
}

#[derive(Serialize)]
#[serde(untagged)]
pub enum ResponseObjects {
    One(Response),
    Many(Vec<Response>),
    None
}

impl From<Response> for ResponseObjects {
    fn from(t: Response) -> Self {
        match t {
            Response::Empty => ResponseObjects::None,
            t => ResponseObjects::One(t)
        }
    }
}

impl From<Vec<Response>> for ResponseObjects {
    fn from(t: Vec<Response>) -> Self {
        let t = t
            .into_iter()
            .filter_map(|r| match r {
                Response::Empty => None,
                t => Some(t)
            })
            .collect::<Vec<_>>();
        if t.is_empty() {
            return ResponseObjects::None;
        }
        ResponseObjects::Many(t)
    }
}

pub trait Service {
    fn call(&self, req: Request) -> Box<Future<Item = ResponseObjects, Error = ()> + Send>;
    fn batch(
        &self,
        requests: Vec<Request>
    ) -> Box<Future<Item = ResponseObjects, Error = ()> + Send>;
    fn request_from_bytes(
        &self,
        bytes: &[u8]
    ) -> Box<Future<Item = ResponseObjects, Error = ()> + Send>;
}

pub struct Server<T> {
    state: T,
    pub methods: HashMap<String, Box<Method<T>>>
}

impl<T> Server<T> {
    pub fn new(state: T) -> Self { Server { state, methods: HashMap::new() } }

    pub fn with_methods(mut self, named_methods: Vec<(&'static str, Box<Method<T>>)>) -> Self {
        for (name, method) in named_methods {
            self.methods.insert(name.into(), method);
        }
        self
    }

    fn inner_call(&self, req: Request) -> impl Future<Item = Response, Error = ()> {

        let (opt_id, method, params) = match req {
            Request::Notification { method, params, .. } => (None, method, params),
            Request::HavingId { method, params, id, .. } => (Some(id), method, params)
        };

        if let Some(method) = self.methods.get(method.as_ref()) {
            let rt = method.call(&self.state, params).then(|fut| match fut {
                Ok(val) => future_ok(Response::result(val, opt_id)),
                Err(e) => future_ok(Response::error(e, opt_id))
            });
            EitherFuture::A(rt)
        } else {
            let rt = future_ok(Response::error(Error::method_not_found(), opt_id));
            EitherFuture::B(rt)
        }
    }

    fn inner_batch(
        &self,
        requests: Vec<Request>
    ) -> impl Future<Item = Vec<Response>, Error = ()> 
    {
        use futures::stream::Stream;
        futures::stream::futures_unordered(requests.into_iter().map(|r| self.inner_call(r))).collect()
    }

}


impl<T: 'static> Service for Server<T> {
    fn call(&self, req: Request) -> Box<Future<Item = ResponseObjects, Error = ()> + Send> {
        Box::new(self.inner_call(req).map(ResponseObjects::from))
    }

    fn batch(
        &self,
        requests: Vec<Request>
    ) -> Box<Future<Item = ResponseObjects, Error = ()> + Send>
    {
        Box::new(self.inner_batch(requests).map(ResponseObjects::from))
    }

    fn request_from_bytes(
        &self,
        bytes: &[u8]
    ) -> Box<Future<Item = ResponseObjects, Error = ()> + Send>
    {
        if let Ok(mr) = serde_json::from_slice::<OneOrManyValues>(bytes) {
            match mr {
                OneOrManyValues::One(val) => {
                    return Box::new(match serde_json::from_value::<Request>(val) {
                        Ok(rn) => EitherFuture::A(self.call(rn)),
                        Err(_) => EitherFuture::B(future_ok(
                            Response::error(Error::invalid_request(), Some(Id::Null)).into()
                        ))
                    });
                }
                OneOrManyValues::Many(vals) => {
                    if vals.is_empty() {
                        return Box::new(future_ok(
                            Response::error(Error::invalid_request(), Some(Id::Null)).into()
                        ));
                    }

                    let (okays, errs) = vals
                        .into_iter()
                        .map(serde_json::from_value::<Request>)
                        .partition::<Vec<Result<Request, serde_json::Error>>, _>(|x| x.is_ok());

                    let errs = errs
                        .into_iter()
                        .map(|_| Response::error(Error::invalid_request(), Some(Id::Null)))
                        .collect::<Vec<_>>();

                    return Box::new(
                        self.inner_batch(okays.into_iter().flat_map(|x| x).collect())
                            .map(|mut rs| {
                                rs.extend(errs);
                                rs.into()
                            })
                    );
                }
            }
        }

        Box::new(future_ok(Response::error(Error::parse_error(), Some(Id::Null)).into()))
    }
}

pub mod utils {
    use std::{any::Any, marker::PhantomData};

    pub fn specify<T: 'static>(any_box: Box<Any>, _: PhantomData<T>) -> Box<super::Method<T>> {
        any_box.downcast::<super::BoxedMethod<T>>().unwrap() as Box<super::Method<T>>
    }
}

pub mod exp {
    pub use futures;
    pub use serde;
    pub use serde_json;
}
