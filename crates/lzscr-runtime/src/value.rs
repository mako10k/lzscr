//! Runtime value types for lzscr evaluation.

use crate::{Env, EvalError, ThunkKind, ThunkState};
use lzscr_ast::ast::{Expr, Pattern};
use std::sync::Arc;

#[derive(Debug, Clone)]
pub enum Value {
    Unit,
    Int(i64),
    Float(f64),
    Str(RtStr),
    Char(i32),
    Symbol(u32),
    Ctor { name: String, args: Vec<Value> },
    List(Vec<Value>),
    Tuple(Vec<Value>),
    Record(std::collections::BTreeMap<String, Value>),
    ModeMap { fields: std::collections::BTreeMap<String, Value>, default: Option<Box<Value>> },
    Native { arity: usize, args: Vec<Value>, f: fn(&Env, &[Value]) -> Result<Value, EvalError> },
    Closure { param: Pattern, body: Expr, env: Env },
    Raised(Box<Value>),
    Thunk { state: std::rc::Rc<std::cell::RefCell<ThunkState>>, kind: ThunkKind },
}

// Runtime string: shared UTF-8 buffer with slice (start,len)
#[derive(Debug, Clone)]
pub struct RtStr {
    pub data: Arc<Vec<u8>>, // whole buffer
    pub start: usize,       // byte offset
    pub len: usize,         // byte length
}

impl RtStr {
    pub fn as_bytes(&self) -> &[u8] {
        &self.data[self.start..self.start + self.len]
    }
    // Removed inherent to_string to avoid shadowing Display
    pub fn eq_str(&self, s: &str) -> bool {
        self.as_bytes() == s.as_bytes()
    }
    pub fn char_count(&self) -> usize {
        std::str::from_utf8(self.as_bytes())
            .map(|s| s.chars().count())
            .unwrap_or_else(|_| String::from_utf8_lossy(self.as_bytes()).chars().count())
    }
    fn char_boundaries(s: &str) -> Vec<usize> {
        // boundaries[ci] = byte index at char index ci; includes trailing s.len()
        let mut b: Vec<usize> = s.char_indices().map(|(i, _)| i).collect();
        b.push(s.len());
        b
    }
    pub fn slice_chars(&self, start_chars: usize, len_chars: usize) -> Option<RtStr> {
        let s = std::str::from_utf8(self.as_bytes()).ok()?;
        let boundaries = RtStr::char_boundaries(s);
        let total_chars = boundaries.len() - 1;
        if start_chars > total_chars {
            return None;
        }
        if start_chars + len_chars > total_chars {
            return None;
        }
        let begin_b = boundaries[start_chars];
        let end_b = boundaries[start_chars + len_chars];
        Some(RtStr {
            data: self.data.clone(),
            start: self.start + begin_b,
            len: end_b.saturating_sub(begin_b),
        })
    }
}

impl std::fmt::Display for RtStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match std::str::from_utf8(self.as_bytes()) {
            Ok(s) => f.write_str(s),
            Err(_) => f.write_str(&String::from_utf8_lossy(self.as_bytes())),
        }
    }
}

impl PartialEq for RtStr {
    fn eq(&self, other: &Self) -> bool {
        self.as_bytes() == other.as_bytes()
    }
}
impl Eq for RtStr {}
impl PartialEq<String> for RtStr {
    fn eq(&self, other: &String) -> bool {
        self.as_bytes() == other.as_bytes()
    }
}
impl PartialEq<&str> for RtStr {
    fn eq(&self, other: &&str) -> bool {
        self.as_bytes() == other.as_bytes()
    }
}
