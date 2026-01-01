#!/usr/bin/env bash
set -euo pipefail

cd /workspaces/lzscr

echo "[postCreate] rustc: $(rustc --version)"
echo "[postCreate] cargo: $(cargo --version)"
echo "[postCreate] node:  $(node --version)"
echo "[postCreate] npm:   $(npm --version)"

# Pre-fetch deps to make first build/test faster in Codespaces.
# Keep it lightweight: just metadata resolution.
cargo fetch

# Optional: install wasm target etc. (not required for this repo right now).

echo "[postCreate] Done. Try: cargo test"
