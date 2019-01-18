#[macro_use]
extern crate ezjsonrpc;

use ezjsonrpc::*;

use serde_json::json;

// use serde_derive::{Serialize, Deserialize};

// #[derive(Deserialize, Debug)]
// #[serde(untagged)]
// pub enum MaybeOneOrMany {
//     Many(Vec<serde_json::Value>),
//     One(serde_json::Value),
// }

// #[jsonrpc_method]
// fn add<T>(x: u32, y: T) -> Result<u32, Box<std::error::Error + Send + Sync>> where T: Clone {
//     Ok(x + y)
// }

// #[jsonrpc_method]
// fn add() -> Result<u32, Box<std::error::Error + Send + Sync>> {
//     Ok(1 + 3)
// }

// #[jsonrpc_method]
// fn subtract(x: u32, y: u32) -> Result<u32, Box<std::error::Error + Send + Sync>> { Ok(x - y) }

// #[jsonrpc_method]
// fn add<T>(x: u32, y: u32) -> Result<u32, Box<std::error::Error + Send + Sync>> where T: Clone { Ok(x + y) }

fn foo() -> Result<impl serde::Serialize, ()> { Ok(String::from("HI")) }

pub struct Foo(u32);

#[jsonrpc_methods]
impl Foo {
    // extern crate ezjsonrpc as _ezjsonrpc;
    #[jsonrpc(rename = "adder", rename_args(x = "x.x", y = "y.y"))]
    fn add(&self, x: u32, y: u32) -> Result<u32, Box<std::error::Error + Send + Sync>> {
        // println!("{:?}", x + y + self.0);
        Ok(x + y + self.0)
    }

    #[jsonrpc(rename = "adder", rename_args(x = "x.x", y = "y.y"))]
    fn subtract(&self, x: u32, y: u32) -> Result<u32, Box<std::error::Error + Send + Sync>> {
        // println!("{:?}", x + y + self.0);
        Ok(x - y)
    }
}

fn main() {
    use futures::Future;

    let server = Server::new(Foo(23)).with_methods(methods![Foo::add, Foo::subtract]);

    // let mut req_1 = Request::having_id()
    //     .with_id(1)
    //     .with_method("add")
    //     .with_params(vec![1, 2]);

    // let mut req_2 = Request::having_id()
    //     .with_id(2)
    //     .with_method("subtract")
    //     .with_params(vec![2, 1]);

    let res_fut = server
        // .batch(vec![req_1, req_2])
        .request_from_bytes(br#"[1]"#)
        // .batch(vec![req_1.into(), req_2.into(), req_3.into()])
        .map(|r| println!("{}", serde_json::to_string_pretty(&r).unwrap()))
        .map_err(|e| println!("{:?}", &e));

    tokio::run(res_fut);

    // println!("{}", serde_json::to_string_pretty(&req).unwrap());
}
