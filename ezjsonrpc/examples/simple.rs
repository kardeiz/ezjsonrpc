#[macro_use]
extern crate ezjsonrpc;

use ezjsonrpc::*;

use serde_json::json;

use serde_derive::{Serialize, Deserialize};

#[derive(Deserialize, Debug)]
#[serde(untagged)]
pub enum MaybeOneOrMany {
    Many(Vec<serde_json::Value>),
    One(serde_json::Value),
}



// #[jsonrpc_method]
// fn add<T>(x: u32, y: T) -> Result<u32, Box<std::error::Error + Send + Sync>> where T: Clone {
//     Ok(x + y)
// }

// #[jsonrpc_method]
// fn add() -> Result<u32, Box<std::error::Error + Send + Sync>> {
//     Ok(1 + 3)
// }

#[jsonrpc_method]
fn subtract(x: u32, y: u32) -> Result<u32, Box<std::error::Error + Send + Sync>> { Ok(x - y) }

#[jsonrpc_method]
fn add(x: u32, y: u32) -> Result<u32, Box<std::error::Error + Send + Sync>> { Ok(x + y) }

fn main() {

    // let x: MaybeOneOrMany = serde_json::from_str("{\"hello\": 23 }").unwrap();

    // println!("{:?}", &x);


    use futures::Future;

    let server = Server::default()
        .with_methods(methods![add, subtract]);

    let mut req_1 = Request::default()
        .with_id(1)
        .with_method("subtract")
        .with_params(vec![22, 1]);

    // let mut req_2 = Request::default()
    //     .with_id(2)
    //     .with_method("add")
    //     .with_params(vec![22, 1]);

    // let mut req_3 = Request::default()
    //     .with_method("added")
    //     .with_params(vec![200000000, 1]);

    let req_or_not = serde_json::from_str::<serde_json::Value>(r#"{"method": "add", "params": [1, 2], "jsonrpc": "2.0", "id": "1"}"#).unwrap();

    let req_or_not = serde_json::from_value::<Request>(req_or_not);

    println!("{:?}", req_or_not);


    let res_fut = server
        // .call(req_1)
        .request_from_bytes(br#"[{"method": "add", "params": [1, 2], "jsonrpc": "2.0", "id": "1"}]"#)
        // .batch(vec![req_1.into(), req_2.into(), req_3.into()])
        .map(|r| println!("{}", serde_json::to_string_pretty(&r).unwrap()))
        .map_err(|e| println!("{:?}", &e));

    tokio::run(res_fut);

    // println!("{}", serde_json::to_string_pretty(&req).unwrap());
}
