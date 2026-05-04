"""Provider (driver) iOS prebuild."""
from __future__ import annotations

from pathlib import Path

from ..common import (
    google_services_json,
    google_services_plist,
    log,
    write_local_base_url,
)


def prebuild(app_dir: Path, base_url: str = "http://localhost:8016/ui") -> None:
    log.info("provider/ios · prebuild")
    google_services_json.write_google_services_json(app_dir / "android" / "app")
    google_services_plist.write_google_services_plist(app_dir / "ios")
    # NB: caller must invoke this AFTER provider's setup_config.sh
    write_local_base_url.write_local_base_url(app_dir, base_url)
