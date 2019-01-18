#![recursion_limit = "2048"]

#[macro_use]
extern crate serde_derive;

pub use ezjsonrpc_macros::*;

use std::collections::HashMap;

use futures::{
    future::{err as future_err, ok as future_ok, Either as EitherFuture},
    Future, IntoFuture
};

// use serde_derive::{ Deserialize, Serialize };
use proc_macro_hack::proc_macro_hack;
use serde::{de::DeserializeOwned, Deserialize, Deserializer, Serialize, Serializer};
use serde_json::{json, value::RawValue, Value};

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

impl From<i64> for Id {
    fn from(t: i64) -> Self { Id::Num(t) }
}

impl From<String> for Id {
    fn from(t: String) -> Self { Id::String(t) }
}

impl Default for Id {
    fn default() -> Id { Id::Null }
}

#[derive(Debug, Clone)]
pub enum Id {
    Num(i64),
    String(String),
    Null
}

impl Serialize for Id {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where S: Serializer {
        match *self {
            Id::Num(ref num) => num.serialize(serializer),
            Id::String(ref s) => s.serialize(serializer),
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
            String(String)
        }

        #[derive(Serialize, Deserialize)]
        pub struct WrappedPresentId(Option<PresentId>);

        let out = match WrappedPresentId::deserialize(deserializer)? {
            WrappedPresentId(Some(PresentId::Num(num))) => Id::Num(num),
            WrappedPresentId(Some(PresentId::String(s))) => Id::String(s),
            WrappedPresentId(None) => Id::Null
        };

        Ok(out)
    }
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(untagged)]
pub enum Request {
    HavingId { jsonrpc: V2, method: String, params: Option<Value>, id: Id },
    Notification { jsonrpc: V2, method: String, params: Option<Value> }
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
            Request::HavingId { jsonrpc, method, params, id: _ } => {
                Request::HavingId { jsonrpc, method, params, id: id.into() }
            }
            Request::Notification { jsonrpc, method, params } => {
                Request::HavingId { jsonrpc, method, params, id: id.into() }
            }
        }
    }

    pub fn with_method<I: Into<String>>(self, method: I) -> Self {
        match self {
            Request::HavingId { jsonrpc, method: _, params, id } => {
                Request::HavingId { jsonrpc, method: method.into(), params, id: id.into() }
            }
            Request::Notification { jsonrpc, method: _, params } => {
                Request::Notification { jsonrpc, method: method.into(), params }
            }
        }
    }

    pub fn with_params<I: Into<Value>>(mut self, params: I) -> Self {
        match self {
            Request::HavingId { jsonrpc, method, params: _, id } => {
                Request::HavingId { jsonrpc, method, params: Some(params.into()), id: id.into() }
            }
            Request::Notification { jsonrpc, method, params: _ } => {
                Request::Notification { jsonrpc, method, params: Some(params.into()) }
            }
        }
    }

    pub fn having_id() -> Self {
        Request::HavingId { jsonrpc: V2, method: String::new(), params: None, id: Id::Null }
    }

    pub fn notification() -> Self {
        Request::Notification { jsonrpc: V2, method: String::new(), params: None }
    }
}

#[derive(Serialize, Deserialize, Default, Debug)]
pub struct Error {
    pub code: i64,
    pub message: String,
    pub data: Option<Value>
}

impl Error {
    pub fn invalid_method_parameters() -> Self {
        Error { code: -32602, message: "Invalid params".into(), data: None }
    }

    pub fn method_does_not_exist() -> Self {
        Error { code: -32601, message: "Method not found".into(), data: None }
    }

    pub fn invalid_json_received() -> Self {
        Error { code: -32700, message: "Parse error".into(), data: None }
    }

    pub fn invalid_request_received() -> Self {
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

pub struct MethodContainer<H>(H);

impl<H, T> Method<T> for MethodContainer<H>
where H: Fn(&T, Option<Value>) -> Box<Future<Item = Value, Error = Error> + Send> + Send + Sync
{
    fn call(
        &self,
        state: &T,
        params: Option<Value>
    ) -> Box<Future<Item = Value, Error = Error> + Send>
    {
        (&self.0)(state, params)
    }
}

impl<H, I, S, E, T> From<H> for Box<Method<T>>
where
    I: IntoFuture<Item = S, Error = E> + 'static + Send,
    I::Future: 'static + Send,
    S: serde::Serialize,
    E: Into<Error>,
    H: Fn(&T, Option<Value>) -> Result<I, Error> + Sync + Send + 'static
{
    fn from(t: H) -> Self {
        Box::new(MethodContainer(
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

pub trait Method<T> {
    fn call(
        &self,
        state: &T,
        params: Option<Value>
    ) -> Box<Future<Item = Value, Error = Error> + Send>;
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
}

#[derive(Deserialize)]
#[serde(untagged)]
pub enum OneOrManyMaybeRequests {
    Many(Vec<Value>),
    One(Value)
}

#[derive(Serialize)]
#[serde(untagged)]
pub enum NoneOrOneOrManyResponses {
    One(Response),
    Many(Vec<Response>),
    Empty
}

impl From<Response> for NoneOrOneOrManyResponses {
    fn from(t: Response) -> Self { NoneOrOneOrManyResponses::One(t) }
}

impl From<Vec<Response>> for NoneOrOneOrManyResponses {
    fn from(t: Vec<Response>) -> Self { NoneOrOneOrManyResponses::Many(t) }
}

pub trait Service {
    fn call(&self, req: Request) -> Box<Future<Item = Response, Error = ()> + Send>;
    fn batch(&self, requests: Vec<Request>)
        -> Box<Future<Item = Vec<Response>, Error = ()> + Send>;
    fn request_from_bytes(
        &self,
        bytes: &[u8]
    ) -> Box<Future<Item = NoneOrOneOrManyResponses, Error = ()> + Send>;
}

impl<T> Service for Server<T> {
    fn call(&self, req: Request) -> Box<Future<Item = Response, Error = ()> + Send> {
        let (opt_id, method, params) = match req {
            Request::Notification { method, params, .. } => (None, method, params),
            Request::HavingId { method, params, id, .. } => (Some(id), method, params)
        };

        if let Some(method) = self.methods.get(&method) {
            let rt = method.call(&self.state, params).then(|fut| match fut {
                Ok(val) => Ok(opt_id
                    .map(|id| Response::Result { jsonrpc: V2, result: val, id })
                    .unwrap_or_else(|| Response::Empty)),
                Err(e) => Ok(opt_id
                    .map(|id| Response::Error { jsonrpc: V2, error: e, id })
                    .unwrap_or_else(|| Response::Empty))
            });
            Box::new(rt)
        } else {
            let rt = Ok(opt_id
                .map(|id| Response::Error {
                    jsonrpc: V2,
                    error: Error::method_does_not_exist(),
                    id
                })
                .unwrap_or_else(|| Response::Empty))
            .into_future();
            Box::new(rt)
        }
    }

    fn batch(
        &self,
        requests: Vec<Request>
    ) -> Box<Future<Item = Vec<Response>, Error = ()> + Send>
    {
        use futures::stream::Stream;
        let out = futures::stream::futures_unordered(requests.into_iter().map(|r| self.call(r)))
            .filter_map(|re| match re {
                Response::Empty => None,
                t => Some(t)
            })
            .collect();
        Box::new(out)
    }

    fn request_from_bytes(
        &self,
        bytes: &[u8]
    ) -> Box<Future<Item = NoneOrOneOrManyResponses, Error = ()> + Send>
    {
        if let Ok(mr) = serde_json::from_slice::<OneOrManyMaybeRequests>(bytes) {
            match mr {
                OneOrManyMaybeRequests::One(val) => {
                    return Box::new(match serde_json::from_value::<Request>(val) {
                        Ok(rn) => EitherFuture::A(self.call(rn).map(|re| match re {
                            Response::Empty => NoneOrOneOrManyResponses::Empty,
                            t => NoneOrOneOrManyResponses::One(t)
                        })),
                        Err(_) => EitherFuture::B(
                            Ok(Response::Error {
                                jsonrpc: V2,
                                error: Error::invalid_request_received(),
                                id: Id::Null
                            }
                            .into())
                            .into_future()
                        )
                    });
                }
                OneOrManyMaybeRequests::Many(vals) => {
                    if vals.is_empty() {
                        return Box::new(
                            Ok(Response::Error {
                                jsonrpc: V2,
                                error: Error::invalid_request_received(),
                                id: Id::Null
                            }
                            .into())
                            .into_future()
                        );
                    }
                    let (okays, errs): (
                        Vec<Result<Request, serde_json::Error>>,
                        Vec<Result<Request, serde_json::Error>>
                    ) = vals
                        .into_iter()
                        .map(|v| serde_json::from_value::<Request>(v))
                        .partition(|x| x.is_ok());

                    let mut errs = errs
                        .into_iter()
                        .map(|_| Response::Error {
                            jsonrpc: V2,
                            error: Error::invalid_request_received(),
                            id: Id::Null
                        })
                        .collect::<Vec<_>>();
                    return Box::new(self.batch(okays.into_iter().flat_map(|x| x).collect()).map(
                        |mut rs| {
                            rs.extend(errs);
                            NoneOrOneOrManyResponses::Many(rs)
                        }
                    ));
                }
            }
        }

        Box::new(
            Ok(Response::Error {
                jsonrpc: V2,
                error: Error::invalid_json_received(),
                id: Id::Null
            }
            .into())
            .into_future()
        )
    }
}

// impl Request {

// }

// pub struct Server<T> {
//     inner: T
// }

// pub trait MethodCall {

// }

// impl From<Response> for ResponseOrEmpty {
//     fn from(t: Response) -> Self { ResponseOrEmpty::Response(t) }
// }

// impl From<()> for ResponseOrEmpty {
//     fn from(t: ()) -> Self { ResponseOrEmpty::Empty }
// }

// #[derive(Serialize, Deserialize, Debug)]
// #[serde(untagged)]
// pub enum Response {
//     Result(ResultResponse),
//     Error(ErrorResponse)
// }

// impl From<ResultResponse> for Response {
//     fn from(t: ResultResponse) -> Self { Response::Result(t) }
// }

// impl From<ErrorResponse> for Response {
//     fn from(t: ErrorResponse) -> Self { Response::Error(t) }
// }

// #[derive(Serialize, Deserialize, Default, Debug)]
// pub struct ResultResponse {
//     jsonrpc: V2,
//     pub result: Value,
//     pub id: Id
// }

// impl ResultResponse {
//     pub fn with_result(mut self, result: Value) -> Self {
//         self.result = result;
//         self
//     }

//     pub fn with_id<I: Into<Id>>(mut self, id: I) -> Self {
//         self.id = id.into();
//         self
//     }
// }

// #[derive(Serialize, Deserialize, Default, Debug)]
// pub struct ErrorResponse {
//     jsonrpc: V2,
//     pub error: Error,
//     pub id: Id
// }

// impl ErrorResponse {
//     pub fn with_error<I: Into<Error>>(mut self, error: I) -> Self {
//         self.error = error.into();
//         self
//     }

//     pub fn with_id<I: Into<Id>>(mut self, id: I) -> Self {
//         self.id = id.into();
//         self
//     }
// }

// #[derive(Serialize, Deserialize, Default, Debug)]
// pub struct Error {
//     pub code: i64,
//     pub message: String,
//     pub data: Option<Value>
// }

// impl Error {

//     pub fn invalid_method_parameters() -> Self {
//         Error { code: -32602, message: "Invalid params".into(), data: None }
//     }

//     pub fn method_does_not_exist() -> Self {
//         Error { code: -32601, message: "Method not found".into(), data: None }
//     }

//     pub fn invalid_json_received() -> Self {
//         Error { code: -32700, message: "Parse error".into(), data: None }
//     }

//     pub fn invalid_request_received() -> Self {
//         Error { code: -32600, message: "Invalid Request".into(), data: None }
//     }

// }

// impl<T: std::fmt::Display> From<T> for Error {
//     fn from(t: T) -> Self { Error { message: format!("{}", t), ..Error::default() } }
// }

// impl Request {
//     pub fn handle<S, I, E>(self, fut_like: I) -> Box<exp::Future<Item = ResultResponse, Error = ErrorResponse> + Send>
//     where
//         S: serde::Serialize,
//         E: Into<Error>,
//         I: IntoFuture<Item = S, Error = E>,
//         I::Future: 'static + Send {
//         let id_cloned = self.id.clone();
//         let out = fut_like
//             .into_future()
//             .map_err(|e| e.into())
//             .and_then(|t| {
//                 serde_json::to_value(t).into_future().map_err(|e| Error::from(e))
//             })
//             .map_err(move |e| ErrorResponse { error: e, id: id_cloned, ..ErrorResponse::default() })
//             .map(move |t| ResultResponse { result: t, id: self.id, ..ResultResponse::default() });
//         Box::new(out)
//     }
// }

// impl Notification {
//     pub fn handle<S, I, E>(self, fut_like: I) -> Box<exp::Future<Item = (), Error = ()> + Send>
//     where
//         S: serde::Serialize,
//         E: Into<Error>,
//         I: IntoFuture<Item = S, Error = E>,
//         I::Future: 'static + Send {
//         let out = fut_like.into_future().map(|t| ()).map_err(|e| ());
//         Box::new(out)
//     }
// }

// pub type RequestFuture = Box<exp::Future<Item = ResultResponse, Error = ErrorResponse> + Send>;
// pub type NotificationFuture = Box<exp::Future<Item = (), Error = ()> + Send>;

// pub trait Caller: Send + Sync {

//     fn call<I: Into<RequestOrNotification>>(&self, request_or_notification: I) -> Box<Future<Item = ResponseOrEmpty, Error = ()> + Send>
//         where Self: Sized {
//         let request_or_notification = request_or_notification.into();
//         match request_or_notification {
//             RequestOrNotification::Request(request) => Box::new(self.request(request)
//                 .then(|fut| match fut {
//                     Ok(r) => Ok::<ResponseOrEmpty, ()>(Response::from(r).into()),
//                     Err(e) => Ok::<ResponseOrEmpty, ()>(Response::from(e).into()) })),
//             RequestOrNotification::Notification(notification) =>
//                 Box::new(self.notification(notification).map(|r| r.into() ))
//         }
//     }

//     fn request(&self, request: Request) -> RequestFuture;
//     fn notification(&self, notification: Notification) -> NotificationFuture;
// }

// pub trait Method: Caller {
//     fn name(&self) -> &'static str;
// }

// #[derive(Deserialize)]
// #[serde(untagged)]
// pub enum OneOrManyMaybeRequests {
//     Many(Vec<Value>),
//     One(Value),
// }

// #[derive(Deserialize, Serialize)]
// #[serde(untagged)]
// pub enum MaybeOneOrManyResponses {
//     One(Response),
//     Many(Vec<Response>),
//     Empty
// }

// impl From<Response> for MaybeOneOrManyResponses {
//     fn from(t: Response) -> Self { MaybeOneOrManyResponses::One(t) }
// }

// impl From<Vec<Response>> for MaybeOneOrManyResponses {
//     fn from(t: Vec<Response>) -> Self { MaybeOneOrManyResponses::Many(t) }
// }

// #[derive(Default)]
// pub struct Server {
//     pub methods: HashMap<String, Box<Method>>
// }

// impl Server {
//     pub fn with_methods(mut self, methods: Vec<Box<Method>>) -> Self {
//         for method in methods {
//             self.methods.insert(method.name().into(), method);
//         }
//         self
//     }

//     pub fn batch(&self, requests: Vec<RequestOrNotification>) -> Box<Future<Item = Vec<Response>, Error = ()> + Send> {
//         use futures::stream::Stream;
//         let out = futures::stream::futures_unordered(requests.into_iter().map(|r| self.call(r) ))
//             .filter_map(|re| match re { ResponseOrEmpty::Response(r) => Some(r), ResponseOrEmpty::Empty => None })
//             .collect();
//         Box::new(out)
//     }

//     pub fn request_from_bytes(&self, bytes: &[u8]) -> Box<Future<Item = MaybeOneOrManyResponses, Error = ()> + Send> {
//         if let Ok(mr) = serde_json::from_slice::<OneOrManyMaybeRequests>(bytes) {
//             match mr {
//                 OneOrManyMaybeRequests::One(val) => {
//                     return Box::new(match serde_json::from_value::<RequestOrNotification>(val) {
//                         Ok(rn) => EitherFuture::A(self.call(rn).map(|re| match re {
//                             ResponseOrEmpty::Response(r) => MaybeOneOrManyResponses::One(r),
//                             ResponseOrEmpty::Empty => MaybeOneOrManyResponses::Empty
//                         })),
//                         Err(_) =>
//                             EitherFuture::B(
//                                 Ok(Response::Error(ErrorResponse::default().with_error(Error::invalid_request_received())).into()).into_future())
//                     });
//                 },
//                 OneOrManyMaybeRequests::Many(vals) => {
//                     println!("{:?}", &vals);
//                     let (okays, errs): (Vec<Result<RequestOrNotification, serde_json::Error>>, Vec<Result<RequestOrNotification, serde_json::Error>>) = vals.into_iter().map(|v|
//                         serde_json::from_value::<RequestOrNotification>(v) ).partition(|x| x.is_ok());
//                     println!("OK: {:?}, Err: {:?}", &okays, &errs);
//                     let mut errs = errs.into_iter().map(|_|
//                         Response::Error(ErrorResponse::default().with_error(Error::invalid_request_received()))).collect::<Vec<_>>();
//                     return Box::new(self.batch(okays.into_iter().flat_map(|x| x).collect())
//                         .map(|mut rs| {
//                             rs.extend(errs);
//                             MaybeOneOrManyResponses::Many(rs)
//                         }));
//                 }
//             }
//         }

//         Box::new(Ok(Response::Error(ErrorResponse::default().with_error(Error::invalid_json_received())).into()).into_future())

//     }
// }

// impl Caller for Server {
//     fn request(&self, request: Request) -> Box<exp::Future<Item = ResultResponse, Error = ErrorResponse> + Send> {
//         if let Some(method) = self.methods.get(&request.method) {
//             method.request(request)
//         } else {
//             Box::new(exp::futures::future::err(ErrorResponse { error: Error::method_does_not_exist(), id: request.id, ..ErrorResponse::default() }))
//         }
//     }
//     fn notification(&self, notification: Notification) -> Box<exp::Future<Item = (), Error = ()> + Send> {
//         if let Some(method) = self.methods.get(&notification.method) {
//             method.notification(notification)
//         } else {
//             Box::new(exp::futures::future::err(()))
//         }
//     }
// }

// pub mod utils {
//     use crate::{Error, ErrorResponse, Id, ResultResponse};
//     use futures::{
//         future::{err as future_err, ok as future_ok, Either as EitherFuture},
//         Future, IntoFuture
//     };
//     use serde::de::{DeserializeOwned, Error as DeError};
//     use serde_json::{to_value, Map, Value};

// }

pub mod exp {
    pub use futures;
    pub use serde;
    pub use serde_json;
}

// pub trait ExtractPositionalParams: Sized {
//     fn extract(v: Value) -> Result<Self, serde_json::Error>;
// }

// pub trait ExtractNamedParams<S>: Sized {
//     fn extract(names: S, v: Value) -> Result<Self, serde_json::Error>;
// }

// impl<T1: DeserializeOwned> ExtractPositionalParams for (T1,) {
//     fn extract(v: Value) -> Result<Self, serde_json::Error> {
//         let (t1,) = serde_json::from_value(v)?;
//         Ok(t1)
//     }
// }

// impl<T1: DeserializeOwned, T2: DeserializeOwned> ExtractPositionalParams for (T1, T2) {
//     fn extract(v: Value) -> Result<Self, serde_json::Error> {
//         let (t1, t2) = serde_json::from_value(v)?;
//         Ok((t1, t2))
//     }
// }

// impl<T1: DeserializeOwned> ExtractNamedParams<(&'static str,)> for (T1,) {
//     fn extract(names: (&'static str,), v: Value) -> Result<Self, serde_json::Error> {
//         let (t1,) = serde_json::from_value(v)?;
//         Ok(t1)
//     }
// }
