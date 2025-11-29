#!/usr/bin/env python3
"""Verify every stdlib module has a purity classification entry."""
from __future__ import annotations

import argparse
import sys
from pathlib import Path


def parse_table(readme_path: Path) -> dict[str, dict[str, str]]:
    text = readme_path.read_text(encoding="utf-8")
    heading = "## Purity Classification"
    try:
        start = text.index(heading)
    except ValueError as exc:  # pragma: no cover - developer ergonomics
        raise SystemExit(f"{heading!r} section missing in {readme_path}") from exc

    lines = text[start:].splitlines()[1:]
    table_rows: list[str] = []
    collecting = False
    for line in lines:
        stripped = line.strip()
        if stripped.startswith("|"):
            table_rows.append(stripped)
            collecting = True
            continue
        if collecting and stripped:
            # Any non-table content after the table means we are done.
            break
        if collecting and not stripped:
            # Blank line terminates the table as well.
            break

    if len(table_rows) < 2:
        raise SystemExit(
            "Purity classification table appears to be missing rows; "
            "ensure it stays under the heading."
        )

    # Drop header and separator rows.
    data_rows = [row for row in table_rows[2:] if row.strip("|").strip()]
    entries: dict[str, dict[str, str]] = {}
    for row in data_rows:
        cells = [cell.strip() for cell in row.strip("|").split("|")]
        if not cells:
            continue
        module_cell = cells[0]
        if not module_cell:
            continue
        module_name = module_cell.strip()
        if module_name.startswith("`") and module_name.endswith("`"):
            module_name = module_name[1:-1]
        if not module_name:
            continue
        entries[module_name] = {
            "status": cells[1] if len(cells) > 1 else "",
            "notes": cells[2] if len(cells) > 2 else "",
            "follow_up": cells[3] if len(cells) > 3 else "",
        }
    return entries


def find_stdlib_modules(stdlib_dir: Path) -> set[str]:
    modules: set[str] = set()
    for file_path in stdlib_dir.rglob("*.lzscr"):
        rel = file_path.relative_to(stdlib_dir)
        modules.add(rel.as_posix())
    return modules


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--stdlib",
        type=Path,
        default=Path(__file__).resolve().parents[1] / "stdlib",
        help="Path to the stdlib directory (default: repo stdlib)",
    )
    parser.add_argument(
        "--readme",
        type=Path,
        default=Path(__file__).resolve().parents[1] / "stdlib" / "readme.md",
        help="Path to the stdlib README that hosts the classification table",
    )
    args = parser.parse_args(argv)

    declared = parse_table(args.readme)
    files = find_stdlib_modules(args.stdlib)

    declared_set = set(declared.keys())
    missing_entries = sorted(files - declared_set)
    stale_entries = sorted(declared_set - files)

    issues: list[str] = []
    if missing_entries:
        issues.append(
            "Modules missing from classification table:\n  - "
            + "\n  - ".join(missing_entries)
        )
    if stale_entries:
        issues.append(
            "Stale table entries (files removed or renamed):\n  - "
            + "\n  - ".join(stale_entries)
        )

    empty_status = sorted(name for name, meta in declared.items() if not meta["status"].strip())
    if empty_status:
        issues.append(
            "Modules missing a status value:\n  - " + "\n  - ".join(empty_status)
        )

    if issues:
        print("Stdlib purity classification check failed:\n" + "\n\n".join(issues), file=sys.stderr)
        return 1

    print(f"Stdlib purity classification covers {len(files)} module(s).")
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main(sys.argv[1:]))
