use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::io::{self, BufRead, Write};

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("io: {0}")]
    Io(#[from] io::Error),

    #[error("invalid frame: {0}")]
    InvalidFrame(String),

    #[error("json: {0}")]
    Json(#[from] serde_json::Error),

    #[error("invalid message: {0}")]
    InvalidMessage(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Id {
    Null,
    Num(i64),
    Str(String),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Request {
    pub jsonrpc: String,
    pub id: Id,
    pub method: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub params: Option<Value>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Notification {
    pub jsonrpc: String,
    pub method: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub params: Option<Value>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ErrorObject {
    pub code: i64,
    pub message: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub data: Option<Value>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum ResponsePayload {
    Result { result: Value },
    Error { error: ErrorObject },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Response {
    pub jsonrpc: String,
    pub id: Id,
    #[serde(flatten)]
    pub payload: ResponsePayload,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Message {
    Request(Request),
    Notification(Notification),
    Response(Response),
}

pub fn parse_message(v: Value) -> Result<Message, Error> {
    let obj = v
        .as_object()
        .ok_or_else(|| Error::InvalidMessage("expected JSON object".into()))?;

    // method present => request or notification
    if let Some(method) = obj.get("method") {
        let method = method
            .as_str()
            .ok_or_else(|| Error::InvalidMessage("method must be string".into()))?
            .to_string();

        let jsonrpc = obj
            .get("jsonrpc")
            .and_then(|x| x.as_str())
            .unwrap_or("2.0")
            .to_string();

        let params = obj.get("params").cloned();

        if obj.contains_key("id") {
            let id: Id = serde_json::from_value(obj.get("id").cloned().unwrap_or(Value::Null))?;
            return Ok(Message::Request(Request {
                jsonrpc,
                id,
                method,
                params,
            }));
        }

        return Ok(Message::Notification(Notification {
            jsonrpc,
            method,
            params,
        }));
    }

    // otherwise: response
    if obj.contains_key("result") || obj.contains_key("error") {
        let resp: Response = serde_json::from_value(v)?;
        return Ok(Message::Response(resp));
    }

    Err(Error::InvalidMessage(
        "expected JSON-RPC message (request/notification/response)".into(),
    ))
}

/// JSON-RPC 2.0 framing compatible with MCP/LSP-style stdio.
///
/// Reads/writes payloads with headers:
///
/// `Content-Length: <n>\r\n\r\n<payload>`
pub mod stdio {
    use super::*;

    pub fn read_message<R: BufRead>(r: &mut R) -> Result<Value, Error> {
        let mut content_length: Option<usize> = None;

        loop {
            let mut line = String::new();
            let n = r.read_line(&mut line)?;
            if n == 0 {
                return Err(Error::InvalidFrame("EOF while reading headers".into()));
            }

            let line = line.trim_end_matches(['\r', '\n']);
            if line.is_empty() {
                break;
            }

            let (k, v) = line
                .split_once(':')
                .ok_or_else(|| Error::InvalidFrame(format!("invalid header line: {line}")))?;
            if k.eq_ignore_ascii_case("Content-Length") {
                let v = v.trim();
                content_length = Some(v.parse::<usize>().map_err(|e| {
                    Error::InvalidFrame(format!("invalid Content-Length '{v}': {e}"))
                })?);
            }
        }

        let len = content_length.ok_or_else(|| Error::InvalidFrame("missing Content-Length".into()))?;
        let mut buf = vec![0u8; len];
        r.read_exact(&mut buf)?;
        Ok(serde_json::from_slice(&buf)?)
    }

    pub fn write_message<W: Write>(w: &mut W, v: &Value) -> Result<(), Error> {
        let payload = serde_json::to_vec(v)?;
        write!(w, "Content-Length: {}\r\n\r\n", payload.len())?;
        w.write_all(&payload)?;
        w.flush()?;
        Ok(())
    }

    pub fn write_msg<W: Write>(w: &mut W, msg: &Message) -> Result<(), Error> {
        let v = serde_json::to_value(msg)?;
        write_message(w, &v)
    }

    /// A minimal blocking stdio loop.
    ///
    /// - Requests are passed to `handle_request` and any returned response is written.
    /// - Notifications are passed to `handle_notification`.
    pub fn serve_stdio<H>(mut handler: H) -> Result<(), Error>
    where
        H: FnMut(Message) -> Result<Option<Message>, Error>,
    {
        let stdin = io::stdin();
        let stdout = io::stdout();
        let mut r = io::BufReader::new(stdin.lock());
        let mut w = io::BufWriter::new(stdout.lock());

        loop {
            let v = read_message(&mut r)?;
            let msg = super::parse_message(v)?;
            if let Some(out) = handler(msg)? {
                write_msg(&mut w, &out)?;
            }
        }
    }

    /// Convenience: create a JSON-RPC response with a result.
    pub fn ok(id: Id, result: Value) -> Message {
        Message::Response(Response {
            jsonrpc: "2.0".into(),
            id,
            payload: ResponsePayload::Result { result },
        })
    }

    /// Convenience: create a JSON-RPC response with an error.
    pub fn err(id: Id, code: i64, message: impl Into<String>) -> Message {
        Message::Response(Response {
            jsonrpc: "2.0".into(),
            id,
            payload: ResponsePayload::Error {
                error: ErrorObject {
                    code,
                    message: message.into(),
                    data: None,
                },
            },
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn content_length_roundtrip() {
        let msg = Message::Request(Request {
            jsonrpc: "2.0".into(),
            id: Id::Num(1),
            method: "ping".into(),
            params: Some(serde_json::json!({"x": 1})),
        });

        let v = serde_json::to_value(&msg).unwrap();
        let mut out: Vec<u8> = vec![];
        stdio::write_message(&mut out, &v).unwrap();

        let mut r = io::BufReader::new(out.as_slice());
        let v2 = stdio::read_message(&mut r).unwrap();
        let m2 = parse_message(v2).unwrap();

        match m2 {
            Message::Request(r) => {
                assert_eq!(r.method, "ping");
                assert_eq!(r.id, Id::Num(1));
            }
            other => panic!("unexpected: {other:?}"),
        }
    }
}
