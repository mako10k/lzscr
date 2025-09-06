//! lzscr-types: HM type inference (scaffold)

pub mod plan {
    /// Placeholder marker so the crate builds. Real implementation will follow.
    pub fn ready() -> bool { true }
}

pub mod api {
    /// Public, stable-ish facade for early tests. Will return pretty-printed type or error.
    /// For now, NYI.
    pub fn infer_program(_src: &str) -> Result<String, String> {
        Err("NYI: type inference not implemented".to_string())
    }
}
