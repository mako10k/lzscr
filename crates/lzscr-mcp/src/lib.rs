//! MCP server thin wrapper.
//!
//! This crate intentionally keeps only a small entry-point wrapper around
//! `lzscr-jsonrpc` stdio framing/loop. Higher-level MCP protocol handling is
//! expected to live outside of Rust (or in a dedicated crate), per repository
//! policy.

pub use lzscr_jsonrpc::{
    Error as JsonRpcError, ErrorObject, Id, Message, Notification, Request, Response,
    ResponsePayload,
};

/// Serve JSON-RPC messages over stdio using MCP/LSP-compatible framing.
///
/// This is a thin wrapper around `lzscr_jsonrpc::stdio::serve_stdio`.
pub fn serve_stdio<H>(handler: H) -> Result<(), JsonRpcError>
where
    H: FnMut(Message) -> Result<Option<Message>, JsonRpcError>,
{
    lzscr_jsonrpc::stdio::serve_stdio(handler)
}
