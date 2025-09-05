This folder contains CI workflows. Main workflow is `ci.yml`.
- `build`: fmt, clippy, test with caching
- `audit`: cargo-audit (weekly and on push), non-blocking
- `deny`: cargo-deny (licenses/bans), non-blocking
- `coverage`: cargo-tarpaulin run, uploads cobertura.xml
