#!/usr/bin/env python3
"""Set placeholder AdMob app IDs in ``app.json`` for
``react-native-google-mobile-ads``. Belt-and-suspenders alongside the
debug AndroidManifest overlay — keeps the plugin happy and silences its
config-time warning. Idempotent: only writes if the keys are absent.

Usage:
    python3 -m setup.ny_react_native.common.admob_app_ids <app_dir>
"""
from __future__ import annotations

import json
import sys
from pathlib import Path

KEY = "react-native-google-mobile-ads"
ANDROID_TEST_ID = "ca-app-pub-3940256099942544~3347511713"
IOS_TEST_ID     = "ca-app-pub-3940256099942544~1458002511"


def ensure_admob_app_ids(app_dir: Path) -> None:
    app_json = app_dir / "app.json"
    if not app_json.is_file():
        print(
            f"ny-react-native: WARN no {app_json} — skipping ad-mob id injection"
        )
        return
    data = json.loads(app_json.read_text())
    existing = data.get(KEY) or {}
    changed = False
    if not existing.get("android_app_id"):
        existing["android_app_id"] = ANDROID_TEST_ID
        changed = True
    if not existing.get("ios_app_id"):
        existing["ios_app_id"] = IOS_TEST_ID
        changed = True
    if changed:
        data[KEY] = existing
        app_json.write_text(json.dumps(data, indent=2) + "\n")
        print(f"  set {KEY}.android_app_id / ios_app_id in {app_json}")
    else:
        print(f"  ad-mob ids already present in {app_json}")


def main(argv: list[str]) -> int:
    if len(argv) != 2:
        print("usage: admob_app_ids.py <app-dir>", file=sys.stderr)
        return 2
    ensure_admob_app_ids(Path(argv[1]))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
