#!/usr/bin/env python3
"""Write a placeholder ``google-services.json`` into the consumer/provider
``android/app/`` directory so the ``com.google.gms.google-services`` Gradle
plugin can resolve a client matching whichever flavor.applicationId is being
built. Real Firebase calls fail at runtime — dev placeholder only.

Usage:
    python3 -m setup.ny_react_native.common.google_services_json <target_dir>

Where ``<target_dir>`` is e.g. ``data/ny-react-native/consumer/android/app``.
The file ``google-services.json`` is created inside that dir if missing;
existing files are kept untouched.
"""
from __future__ import annotations

import hashlib
import json
import sys
from pathlib import Path

PACKAGE_NAMES = [
    "com.mobility.movingtech", "com.mobility.movingtech.debug",
    "in.juspay.nammayatri", "in.juspay.nammayatri.debug",
    "net.openkochi.yatri", "net.openkochi.yatri.debug",
    "in.mobility.manayatri", "in.mobility.manayatri.debug",
    "in.juspay.jatrisaathi", "in.juspay.jatrisaathi.debug",
    "in.mobility.lynx", "in.mobility.lynx.debug",
    "in.mobility.bharattaxi", "in.mobility.bharattaxi.debug",
    "in.mobility.bharatTaxi", "in.mobility.bharatTaxi.debug",
    "in.juspay.nammayatripartner", "in.juspay.nammayatripartner.debug",
    "in.juspay.jatrisaathidriver", "in.juspay.jatrisaathidriver.debug",
    "com.mobility.movingtechdriver", "com.mobility.movingtechdriver.debug",
    "in.mobility.odishayatripartner", "in.mobility.odishayatripartner.debug",
    "in.mobility.keralasavaari", "in.mobility.keralasavaari.debug",
]


def _client(pkg: str) -> dict:
    h = hashlib.sha1(pkg.encode()).hexdigest()[:16]
    return {
        "client_info": {
            "mobilesdk_app_id": f"1:000000000000:android:{h}",
            "android_client_info": {"package_name": pkg},
        },
        "oauth_client": [],
        "api_key": [{"current_key": "AIzaSyDUMMY-not-functional"}],
        "services": {"appinvite_service": {"other_platform_oauth_client": []}},
    }


def write_google_services_json(target_dir: Path) -> None:
    target = target_dir / "google-services.json"
    if target.is_file():
        print(f"ny-react-native: keeping existing {target}")
        return
    print(
        f"ny-react-native: writing dummy {target} "
        "(Firebase calls will not work — dev placeholder)"
    )
    data = {
        "project_info": {
            "project_number": "000000000000",
            "project_id": "ny-rn-dev-placeholder",
            "storage_bucket": "ny-rn-dev-placeholder.appspot.com",
        },
        "client": [_client(p) for p in PACKAGE_NAMES],
        "configuration_version": "1",
    }
    target_dir.mkdir(parents=True, exist_ok=True)
    target.write_text(json.dumps(data, indent=2))
    print(f"  wrote {target} with {len(PACKAGE_NAMES)} client entries")


def main(argv: list[str]) -> int:
    if len(argv) != 2:
        print(
            "usage: google_services_json.py <android-app-dir>",
            file=sys.stderr,
        )
        return 2
    write_google_services_json(Path(argv[1]))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
