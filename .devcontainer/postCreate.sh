#!/usr/bin/env bash
set -euo pipefail

cd /workspaces/lzscr

echo "[postCreate] rustc: $(rustc --version)"
echo "[postCreate] cargo: $(cargo --version)"

if command -v node >/dev/null 2>&1; then
	echo "[postCreate] node:  $(node --version)"
else
	echo "[postCreate] node:  (not installed)"
fi

if command -v npm >/dev/null 2>&1; then
	echo "[postCreate] npm:   $(npm --version)"
else
	echo "[postCreate] npm:   (not installed)"
fi

# Pre-fetch deps to make first build/test faster in Codespaces.
# Keep it lightweight: just metadata resolution.
cargo fetch

# Optional: install wasm target etc. (not required for this repo right now).

echo "[postCreate] Done. Try: cargo test"
