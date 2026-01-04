use lzscr_jsonrpc::{Message, Request};
use serde::de::DeserializeOwned;
use serde::Serialize;
use serde_json::Value;
use std::collections::HashMap;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("jsonrpc error: {0}")]
    JsonRpc(#[from] lzscr_jsonrpc::Error),

    #[error("unknown method: {0}")]
    UnknownMethod(String),

    #[error("invalid params: {0}")]
    InvalidParams(String),

    #[error("handler error: {0}")]
    Handler(String),
}

impl Error {
    fn to_jsonrpc_error(&self) -> (i64, String) {
        // JSON-RPC 2.0:
        // -32602: Invalid params
        // -32601: Method not found
        // -32603: Internal error
        match self {
            Error::UnknownMethod(m) => (-32601, format!("Method not found: {m}")),
            Error::InvalidParams(msg) => (-32602, format!("Invalid params: {msg}")),
            Error::JsonRpc(e) => (-32603, format!("Internal error: {e}")),
            Error::Handler(msg) => (-32000, msg.clone()),
        }
    }
}

type HandlerFn = Box<dyn Fn(Value) -> Result<Value, Error> + Send + Sync + 'static>;

#[derive(Default)]
pub struct Router {
    handlers: HashMap<String, HandlerFn>,
}

impl Router {
    pub fn new() -> Self {
        Self { handlers: HashMap::new() }
    }

    pub fn method<P, R, F>(&mut self, method: impl Into<String>, f: F)
    where
        P: DeserializeOwned,
        R: Serialize,
        F: Fn(P) -> Result<R, Error> + Send + Sync + 'static,
    {
        let method = method.into();
        self.handlers.insert(
            method,
            Box::new(move |params: Value| {
                let parsed: P = serde_json::from_value(params)
                    .map_err(|e| Error::InvalidParams(e.to_string()))?;
                let result = f(parsed)?;
                serde_json::to_value(result).map_err(|e| Error::Handler(e.to_string()))
            }),
        );
    }

    pub fn handle(&self, req: Request) -> Message {
        let id = req.id.clone();
        let result = (|| {
            let handler = self
                .handlers
                .get(&req.method)
                .ok_or_else(|| Error::UnknownMethod(req.method.clone()))?;

            let params = req.params.unwrap_or(Value::Null);
            handler(params)
        })();

        match result {
            Ok(v) => lzscr_jsonrpc::stdio::ok(id, v),
            Err(e) => {
                let (code, message) = e.to_jsonrpc_error();
                lzscr_jsonrpc::stdio::err(id, code, message)
            }
        }
    }

    pub fn dispatch_message(&self, msg: Message) -> Option<Message> {
        match msg {
            Message::Request(req) => Some(self.handle(req)),
            Message::Notification(_) => None,
            Message::Response(_) => None,
        }
    }
}

pub fn serve_stdio(router: &Router) -> Result<(), Error> {
    lzscr_jsonrpc::stdio::serve_stdio(|msg| Ok(router.dispatch_message(msg))).map_err(Error::from)
}

#[derive(Debug, Clone, Serialize, serde::Deserialize)]
pub struct InitializeParams {
    pub protocol_version: Option<String>,
    pub client_info: Option<ClientInfo>,
}

#[derive(Debug, Clone, Serialize, serde::Deserialize)]
pub struct ClientInfo {
    pub name: String,
    pub version: Option<String>,
}

#[derive(Debug, Clone, Serialize, serde::Deserialize)]
pub struct InitializeResult {
    pub server_info: ServerInfo,
}

#[derive(Debug, Clone, Serialize, serde::Deserialize)]
pub struct ServerInfo {
    pub name: String,
    pub version: Option<String>,
}

pub fn default_router() -> Router {
    let mut r = Router::new();

    r.method("initialize", |_: InitializeParams| {
        Ok(InitializeResult {
            server_info: ServerInfo {
                name: "lzscr-mcp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            },
        })
    });

    r
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn router_handles_unknown_method() {
        let r = Router::new();
        let msg = r.handle(Request {
            jsonrpc: "2.0".to_string(),
            id: lzscr_jsonrpc::Id::Num(1),
            method: "nope".to_string(),
            params: None,
        });

        let lzscr_jsonrpc::Message::Response(resp) = msg else {
            panic!("expected response");
        };

        let lzscr_jsonrpc::ResponsePayload::Error { error } = resp.payload else {
            panic!("expected error payload");
        };

        assert_eq!(error.code, -32601);
    }
}
