#![recursion_limit = "2048"]

#[macro_use]
extern crate serde_derive;

pub use ezjsonrpc_macros::*;

use std::borrow::{Borrow, Cow};

use futures::{
    future::{ok as future_ok, Either as EitherFuture},
    Future
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
pub enum RequestObject {
    Request { jsonrpc: V2, method: Cow<'static, str>, params: Option<Value>, id: Id },
    Notification { jsonrpc: V2, method: Cow<'static, str>, params: Option<Value> }
}

impl RequestObject {
    pub fn with_id<I: Into<Id>>(self, id: I) -> Self {
        match self {
            RequestObject::Request { jsonrpc, method, params, .. } => {
                RequestObject::Request { jsonrpc, method, params, id: id.into() }
            }
            RequestObject::Notification { jsonrpc, method, params } => {
                RequestObject::Request { jsonrpc, method, params, id: id.into() }
            }
        }
    }

    pub fn with_method<I: Into<Cow<'static, str>>>(self, method: I) -> Self {
        match self {
            RequestObject::Request { jsonrpc, params, id, .. } => {
                RequestObject::Request { jsonrpc, method: method.into(), params, id }
            }
            RequestObject::Notification { jsonrpc, params, .. } => {
                RequestObject::Notification { jsonrpc, method: method.into(), params }
            }
        }
    }

    pub fn with_params<I: Into<Value>>(self, params: I) -> Self {
        match self {
            RequestObject::Request { jsonrpc, method, id, .. } => {
                RequestObject::Request { jsonrpc, method, params: Some(params.into()), id }
            }
            RequestObject::Notification { jsonrpc, method, .. } => {
                RequestObject::Notification { jsonrpc, method, params: Some(params.into()) }
            }
        }
    }

    pub fn request() -> Self {
        RequestObject::Request { jsonrpc: V2, method: "".into(), params: None, id: Id::Null }
    }

    pub fn notification() -> Self {
        RequestObject::Notification { jsonrpc: V2, method: "".into(), params: None }
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
    fn call(&self, req: RequestObject) -> Box<Future<Item = ResponseObjects, Error = ()> + Send>;
    fn batch(
        &self,
        requests: Vec<RequestObject>
    ) -> Box<Future<Item = ResponseObjects, Error = ()> + Send>;
    fn request_from_bytes(
        &self,
        bytes: &[u8]
    ) -> Box<Future<Item = ResponseObjects, Error = ()> + Send>;
}

type Method<T> = fn(&T, Option<Value>) -> Box<Future<Item = Value, Error = Error> + Send>;
type MethodMatcher<T> = fn(&str) -> Option<Method<T>>;

pub struct Server<T, S> {
    state: S,
    method_matcher: MethodMatcher<T>
}

impl<T, S> Server<T, S>
where S: Borrow<T>
{
    fn empty_matcher(_: &str) -> Option<Method<T>> { None }

    pub fn new(state: S) -> Self { Server { state, method_matcher: Self::empty_matcher } }

    pub fn with_methods(self, method_matcher: MethodMatcher<T>) -> Self {
        let Server { state, .. } = self;

        Server { state, method_matcher }
    }

    fn inner_call(&self, req: RequestObject) -> impl Future<Item = Response, Error = ()> {
        let (opt_id, method, params) = match req {
            RequestObject::Notification { method, params, .. } => (None, method, params),
            RequestObject::Request { method, params, id, .. } => (Some(id), method, params)
        };

        if let Some(method) = (self.method_matcher)(method.as_ref()) {
            let rt = method(self.state.borrow(), params).then(|fut| match fut {
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
        requests: Vec<RequestObject>
    ) -> impl Future<Item = Vec<Response>, Error = ()>
    {
        use futures::stream::Stream;
        futures::stream::futures_unordered(requests.into_iter().map(|r| self.inner_call(r)))
            .collect()
    }
}

impl<T: 'static, S: 'static> Service for Server<T, S>
where S: Borrow<T>
{
    fn call(&self, req: RequestObject) -> Box<Future<Item = ResponseObjects, Error = ()> + Send> {
        Box::new(self.inner_call(req).map(ResponseObjects::from))
    }

    fn batch(
        &self,
        requests: Vec<RequestObject>
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
                    return Box::new(match serde_json::from_value::<RequestObject>(val) {
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
                        .map(serde_json::from_value::<RequestObject>)
                        .partition::<Vec<Result<RequestObject, serde_json::Error>>, _>(|x| {
                            x.is_ok()
                        });

                    let errs = errs
                        .into_iter()
                        .map(|_| Response::error(Error::invalid_request(), Some(Id::Null)))
                        .collect::<Vec<_>>();

                    return Box::new(
                        self.inner_batch(okays.into_iter().flat_map(|x| x).collect()).map(
                            |mut rs| {
                                rs.extend(errs);
                                rs.into()
                            }
                        )
                    );
                }
            }
        }

        Box::new(future_ok(Response::error(Error::parse_error(), Some(Id::Null)).into()))
    }
}

pub mod utils {

    use super::Error;
    use futures::{Future, IntoFuture};

    pub fn finish_callable<I, S, E>(
        i: I
    ) -> Box<Future<Item = serde_json::Value, Error = Error> + Send>
    where
        I: IntoFuture<Item = S, Error = E> + 'static + Send,
        I::Future: 'static + Send,
        S: serde::Serialize,
        E: Into<Error> {
        let rt = i.into_future().map_err(|e| e.into()).and_then(|r| {
            serde_json::to_value(r).into_future().map_err(|e| Error {
                code: 1,
                message: e.to_string(),
                data: None
            })
        });
        Box::new(rt)
    }
}

pub mod exp {
    pub use futures;
    pub use serde;
    pub use serde_json;
}
