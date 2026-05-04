"""Consumer (rider) Android prebuild + build/install/launch.

Currently the orchestration lives in ``_build_runner.sh`` (extracted
verbatim from the original bash). This module is a Python-side
namespace that future iterations will fill in by porting the relevant
phases out of the bash. For now it documents the helpers used by the
consumer-android path and exposes a ``prebuild`` entry point that the
build runner could call out to."""
from __future__ import annotations

from pathlib import Path

from ..common import (
    admob_app_ids,
    google_services_json,
    google_services_plist,
    local_properties,
    log,
    patch_base_url,
    write_local_base_url,
)


def prebuild(app_dir: Path, base_url: str = "http://localhost:8013/v2") -> None:
    """Run every idempotent prebuild data-write needed before
    ``react-native run-android``. Safe to call multiple times — every
    helper checks for existing files and only fills in placeholders."""
    log.info("consumer/android · prebuild")
    local_properties.write_consumer(app_dir)
    google_services_json.write_google_services_json(app_dir / "android" / "app")
    google_services_plist.write_google_services_plist(app_dir / "ios")
    admob_app_ids.ensure_admob_app_ids(app_dir)
    write_local_base_url.write_local_base_url(app_dir, base_url)
    patch_base_url.patch_consumer_base_url_defaults(app_dir, base_url)
