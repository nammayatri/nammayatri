"""Provider (driver) Android prebuild. ``setup_config.sh`` (in the
provider checkout) regenerates ``.env`` from a template, so
``write_local_base_url`` MUST be called AFTER it runs — ``_build_runner.sh``
handles that ordering today."""
from __future__ import annotations

from pathlib import Path

from ..common import (
    admob_app_ids,
    google_services_json,
    google_services_plist,
    local_properties,
    log,
    write_local_base_url,
)


def prebuild(app_dir: Path, base_url: str = "http://localhost:8016/ui") -> None:
    log.info("provider/android · prebuild")
    local_properties.write_provider(app_dir)
    google_services_json.write_google_services_json(app_dir / "android" / "app")
    google_services_plist.write_google_services_plist(app_dir / "ios")
    admob_app_ids.ensure_admob_app_ids(app_dir)
    # NB: caller must invoke this AFTER provider's setup_config.sh
    write_local_base_url.write_local_base_url(app_dir, base_url)
