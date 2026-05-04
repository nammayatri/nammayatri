#!/usr/bin/env python3
"""Set/replace ``BASE_URL`` in EVERY ``.env*`` file under the app dir
(and its ``./env/`` subdir) so the app talks to our local backend instead
of the upstream prod URL the repo ships with.

react-native-config picks ONE env file at build time based on the
variant (typically ``.env.development`` for ``*Dev*`` variants).
Patching only ``.env`` leaves the others untouched and the build uses
prod URLs. Patching every candidate is idempotent and bulletproof.

For provider, ``setup_config.sh`` regenerates ``.env`` from a template,
so this MUST be called AFTER ``setup_config.sh`` runs.

Usage:
    python3 -m setup.ny_react_native.common.write_local_base_url <app_dir> <base_url>
"""
from __future__ import annotations

import re
import sys
from pathlib import Path

EXAMPLE_SUFFIXES = {".example", ".template", ".sample"}
BASE_URL_LINE = re.compile(r"^BASE_URL=.*$", re.MULTILINE)


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


def write_local_base_url(app_dir: Path, base_url: str) -> None:
    if not app_dir.is_dir():
        print(
            f"ny-react-native: WARN no {app_dir} — skipping BASE_URL write",
            file=sys.stderr,
        )
        return
    found = 0
    seen: set[Path] = set()
    # Walk depth ≤ 2 (matches the original `find -maxdepth 2`).
    for path in [*app_dir.iterdir(), *(p for sub in app_dir.iterdir() if sub.is_dir() and sub.name != "node_modules" for p in sub.iterdir())]:
        if "node_modules" in path.parts:
            continue
        if not _is_env_candidate(path) or path in seen:
            continue
        seen.add(path)
        text = path.read_text(encoding="utf-8")
        replacement = f'BASE_URL="{base_url}"'
        if BASE_URL_LINE.search(text):
            new_text = BASE_URL_LINE.sub(replacement, text)
        else:
            new_text = text + ("" if text.endswith("\n") or not text else "\n") + replacement + "\n"
        if new_text != text:
            path.write_text(new_text, encoding="utf-8")
        print(f'ny-react-native: set BASE_URL="{base_url}" in {path}')
        found += 1
    if found == 0:
        # No env files at all — create the default one.
        default_env = app_dir / ".env"
        default_env.write_text(f'BASE_URL="{base_url}"\n', encoding="utf-8")
        print(
            f'ny-react-native: created {default_env} with BASE_URL="{base_url}"'
        )


def main(argv: list[str]) -> int:
    if len(argv) != 3:
        print(
            "usage: write_local_base_url.py <app-dir> <base-url>",
            file=sys.stderr,
        )
        return 2
    write_local_base_url(Path(argv[1]), argv[2])
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
