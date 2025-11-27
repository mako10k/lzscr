#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage: lzscr-run-stdin.sh [options] [-- <lzscr-cli args>]

Read a program from stdin, write it to a temporary .lzscr file, execute it
with lzscr-cli, and clean up the temporary file automatically.

Options:
  -s, --stdlib-dir <path>  Override the stdlib directory (default: repo stdlib)
  -k, --keep-temp          Keep the generated file instead of deleting it
  -h, --help               Show this message

Additional lzscr-cli flags can be passed after -- (or after the script options
if there are no conflicts). Set LZSCR_CLI_BIN to point to a specific lzscr-cli
binary; otherwise the script prefers target/debug/lzscr-cli and falls back to
`cargo run -q -p lzscr-cli`.
EOF
}

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
stdlib_dir="$repo_root/stdlib"
keep_temp=0
cli_args=()

while [[ $# -gt 0 ]]; do
  case "$1" in
    -s|--stdlib-dir)
      [[ $# -ge 2 ]] || { echo "error: --stdlib-dir requires a path" >&2; exit 1; }
      stdlib_dir="$2"
      shift 2
      ;;
    -k|--keep-temp)
      keep_temp=1
      shift 1
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    --)
      shift
      cli_args+=("$@")
      break
      ;;
    *)
      cli_args+=("$@")
      break
      ;;
  esac
done

if [[ ! -d "$stdlib_dir" ]]; then
  echo "error: stdlib directory not found: $stdlib_dir" >&2
  exit 1
fi

if [[ -t 0 ]]; then
  echo "error: expected program on stdin" >&2
  exit 1
fi

mkdir -p "${TMPDIR:-/tmp}"
tmp_file="$(mktemp "${TMPDIR:-/tmp}/lzscr_stdin.XXXXXX.lzscr")"

cleanup() {
  if [[ -n "${tmp_file:-}" ]]; then
    if [[ $keep_temp -eq 1 ]]; then
      echo "Temp file kept at $tmp_file" >&2
    else
      rm -f "$tmp_file"
    fi
  fi
}
trap cleanup EXIT

cat > "$tmp_file"

runner_uses_cargo=0
if [[ -n "${LZSCR_CLI_BIN:-}" ]]; then
  runner=("$LZSCR_CLI_BIN")
elif [[ -x "$repo_root/target/debug/lzscr-cli" ]]; then
  runner=("$repo_root/target/debug/lzscr-cli")
else
  runner=(cargo run -q -p lzscr-cli)
  runner_uses_cargo=1
fi

common_args=(--file "$tmp_file" --stdlib-dir "$stdlib_dir")

if [[ $runner_uses_cargo -eq 1 ]]; then
  if [[ ${#cli_args[@]} -gt 0 ]]; then
    "${runner[@]}" -- "${common_args[@]}" "${cli_args[@]}"
  else
    "${runner[@]}" -- "${common_args[@]}"
  fi
else
  if [[ ${#cli_args[@]} -gt 0 ]]; then
    "${runner[@]}" "${common_args[@]}" "${cli_args[@]}"
  else
    "${runner[@]}" "${common_args[@]}"
  fi
fi
