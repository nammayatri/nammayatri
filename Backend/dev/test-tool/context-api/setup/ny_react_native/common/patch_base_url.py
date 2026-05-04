#!/usr/bin/env python3
"""Patch the hardcoded ``consumerBaseUrl`` in every per-brand default
config file under ``src-v2/systems/configs/defaults/appDefaultConfig/``,
plus the master ``defaultAppSystemConfig.ts``.

The app reads MMKV first, falls back to ``.env``; but on EVERY launch
``App.tsx`` calls ``setBaseUrl(appConfig.constants.consumerBaseUrl)``
which re-poisons MMKV with the prod URL hardcoded in the brand defaults.
Patching ``.env`` alone is not enough — ``pm clear`` delays the failure
by one launch but the next ``setBaseUrl`` writes the prod URL back.

Usage:
    python3 -m setup.ny_react_native.common.patch_base_url <app_dir> <local_url>
"""
from __future__ import annotations

import re
import sys
from pathlib import Path

PATTERN = re.compile(r"(consumerBaseUrl\s*:\s*)(['\"])([^'\"]*)\2")


def patch_consumer_base_url_defaults(app_dir: Path, local_url: str) -> None:
    targets: list[Path] = []
    defaults_dir = app_dir / "src-v2/systems/configs/defaults/appDefaultConfig"
    if defaults_dir.is_dir():
        targets.extend(sorted(p for p in defaults_dir.iterdir() if p.suffix == ".ts"))
    master = app_dir / "src-v2/systems/configs/defaults/defaultAppSystemConfig.ts"
    if master.is_file():
        targets.append(master)

    patched = 0
    for t in targets:
        try:
            src = t.read_text(encoding="utf-8")
        except OSError:
            continue
        new_src, n = PATTERN.subn(
            lambda m: f"{m.group(1)}'{local_url}'", src
        )
        if n > 0 and new_src != src:
            t.write_text(new_src, encoding="utf-8")
            print(
                f"  patched {t.relative_to(app_dir)} "
                f"({n} match{'es' if n != 1 else ''})"
            )
            patched += 1
    print(f"consumerBaseUrl override applied to {patched} file(s)")


def main(argv: list[str]) -> int:
    if len(argv) != 3:
        print(
            "usage: patch_base_url.py <app-dir> <local-url>",
            file=sys.stderr,
        )
        return 2
    patch_consumer_base_url_defaults(Path(argv[1]), argv[2])
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
