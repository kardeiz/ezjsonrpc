use ezjsonrpc::*;
use std::sync::Arc;

#[derive(Default)]
pub struct State {
    count: std::sync::atomic::AtomicUsize
}

impl State {
    #[jsonrpc_method]
    fn count(&self) -> Result<String, Error> {
        Ok(format!(
            "Count is: {}",
            self.count.fetch_add(1, std::sync::atomic::Ordering::SeqCst) + 1
        ))
    }

    #[jsonrpc_method(rename = "unreliable-add", rename_args(x = "a", y = "b"))]
    fn unreliable_add(&self, x: u32, y: u32) -> Result<u32, &'static str> {
        Ok(x + y + self.count.load(std::sync::atomic::Ordering::SeqCst) as u32)
    }
}

fn main() {
    use futures::Future;

    let server =
        Server::new(State::default()).with_methods(methods![State::unreliable_add, State::count]);

    let req_1 = RequestObject::notification().with_method("count");
    let req_2 = RequestObject::request().with_id(1).with_method("count");
    let req_3 =
        RequestObject::request().with_id(2).with_method("unreliable-add").with_params(vec![1, 2]);
    let req_4 = RequestObject::request()
        .with_id(3)
        .with_method("unreliable-add")
        .with_params(serde_json::json!{{ "a": 20, "b": 30}});

    let res_fut = server
        .batch(vec![req_1, req_2, req_3, req_4])
        .map(|r| println!("{}", serde_json::to_string_pretty(&r).unwrap()))
        .map_err(|e| println!("{:?}", &e));

    tokio::run(res_fut);
}
