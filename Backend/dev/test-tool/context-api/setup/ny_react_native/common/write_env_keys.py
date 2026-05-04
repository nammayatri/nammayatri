#!/usr/bin/env python3
"""Multi-key ``.env*`` upserter — generalises ``write_local_base_url.py``
to handle any number of ``KEY=VALUE`` pairs in a single pass.

Useful when one app needs several env vars stamped at once (e.g. provider
needs ``BASE_URL``, ``UI_BASE_URL``, ``PLASMA_API_KEY``, ``PLASMA_URL``).
Walks every non-template ``.env``/``.env.*`` file under ``<app_dir>`` at
depth ≤ 2, just like the existing single-key writer, so react-native-config
finds the value in whichever variant-specific file it picks at build time.

Usage:
    python3 -m setup.ny_react_native.common.write_env_keys <app_dir> KEY1=VAL1 KEY2=VAL2 ...

Each ``KEY=VAL`` pair is upserted: replaces the existing ``^KEY=`` line if
present, appends otherwise. Values are wrapped in double quotes verbatim;
embed your own quotes in the value if you need a literal unquoted form.
"""
from __future__ import annotations

import re
import sys
from pathlib import Path
from typing import Iterable

EXAMPLE_SUFFIXES = {".example", ".template", ".sample"}


def _is_env_candidate(p: Path) -> bool:
    if not p.is_file():
        return False
    name = p.name
    if name != ".env" and not name.startswith(".env."):
        return False
    # Skip example / template files — leave those untouched.
    for suf in EXAMPLE_SUFFIXES:
        if name.endswith(suf):
            return False
    return True


def iter_env_files(app_dir: Path) -> Iterable[Path]:
    """Yield each non-template ``.env*`` file at depth ≤ 2 under
    ``app_dir``, skipping ``node_modules``. Shared with
    ``write_local_base_url.py`` so both helpers see the same files."""
    if not app_dir.is_dir():
        return
    seen: set[Path] = set()
    candidates = [
        *app_dir.iterdir(),
        *(
            p
            for sub in app_dir.iterdir()
            if sub.is_dir() and sub.name != "node_modules"
            for p in sub.iterdir()
        ),
    ]
    for path in candidates:
        if "node_modules" in path.parts:
            continue
        if not _is_env_candidate(path) or path in seen:
            continue
        seen.add(path)
        yield path


def upsert_keys(app_dir: Path, pairs: list[tuple[str, str]]) -> None:
    """For each env file, set every (key, val) — replace ``^KEY=`` line
    if present, append otherwise. If no env files exist, create
    ``<app_dir>/.env`` with all the pairs as the seed contents."""
    if not app_dir.is_dir():
        print(
            f"ny-react-native: WARN no {app_dir} — skipping env-key write",
            file=sys.stderr,
        )
        return

    files = list(iter_env_files(app_dir))
    touched_any = False
    for path in files:
        text = path.read_text(encoding="utf-8")
        new_text = text
        changed_keys: list[str] = []
        for key, val in pairs:
            replacement = f'{key}="{val}"'
            line_pat = re.compile(rf"^{re.escape(key)}=.*$", re.MULTILINE)
            if line_pat.search(new_text):
                new_text2 = line_pat.sub(replacement, new_text)
            else:
                sep = "" if new_text.endswith("\n") or not new_text else "\n"
                new_text2 = new_text + sep + replacement + "\n"
            if new_text2 != new_text:
                changed_keys.append(key)
                new_text = new_text2
        if new_text != text:
            path.write_text(new_text, encoding="utf-8")
        keys_str = ", ".join(f"{k}=…" for k, _ in pairs)
        print(f"ny-react-native: stamped {keys_str} in {path}")
        touched_any = True

    if not touched_any:
        # No env files at all — create the default one.
        default_env = app_dir / ".env"
        body = "".join(f'{key}="{val}"\n' for key, val in pairs)
        default_env.write_text(body, encoding="utf-8")
        keys_str = ", ".join(f"{k}=…" for k, _ in pairs)
        print(
            f"ny-react-native: created {default_env} with {keys_str}"
        )


def _parse_pairs(args: list[str]) -> list[tuple[str, str]]:
    out: list[tuple[str, str]] = []
    for arg in args:
        if "=" not in arg:
            raise SystemExit(
                f"write_env_keys.py: argument {arg!r} must be in KEY=VALUE form"
            )
        key, _, val = arg.partition("=")
        if not key:
            raise SystemExit(
                f"write_env_keys.py: empty key in argument {arg!r}"
            )
        out.append((key, val))
    return out


def main(argv: list[str]) -> int:
    if len(argv) < 3:
        print(
            "usage: write_env_keys.py <app-dir> KEY1=VAL1 [KEY2=VAL2 ...]",
            file=sys.stderr,
        )
        return 2
    pairs = _parse_pairs(argv[2:])
    upsert_keys(Path(argv[1]), pairs)
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
