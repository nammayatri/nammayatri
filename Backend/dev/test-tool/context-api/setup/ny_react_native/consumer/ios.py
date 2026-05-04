"""Consumer (rider) iOS prebuild — same shape as ``android.py`` but
tailored for the Xcode/CocoaPods path. Build/install/launch still
runs from ``_build_runner.sh``."""
from __future__ import annotations

from pathlib import Path

from ..common import (
    google_services_json,
    google_services_plist,
    log,
    patch_base_url,
    write_local_base_url,
)


def prebuild(app_dir: Path, base_url: str = "http://localhost:8013/v2") -> None:
    log.info("consumer/ios · prebuild")
    google_services_json.write_google_services_json(app_dir / "android" / "app")
    google_services_plist.write_google_services_plist(app_dir / "ios")
    write_local_base_url.write_local_base_url(app_dir, base_url)
    patch_base_url.patch_consumer_base_url_defaults(app_dir, base_url)
