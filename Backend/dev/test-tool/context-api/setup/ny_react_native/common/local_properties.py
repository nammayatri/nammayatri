"""Write ``android/local.properties`` for consumer + provider.

Consumer picks up the static template at
``setup/ny_react_native/templates/consumer-local.properties.tmpl`` and
substitutes ``__ANDROID_HOME__``. Provider's spec is small enough to
inline — only differs in URL / merchant defaults."""
from __future__ import annotations

import os
from pathlib import Path

from . import log
from .paths import TEMPLATES_DIR

CONSUMER_TMPL = TEMPLATES_DIR / "consumer-local.properties.tmpl"


def _sdk_dir() -> str:
    return os.environ.get(
        "ANDROID_HOME",
        str(Path.home() / "Library/Android/sdk"),
    )


def write_consumer(app_dir: Path) -> None:
    target = app_dir / "android" / "local.properties"
    if target.is_file():
        log.info(f"keeping existing {target}")
        return
    log.info(f"writing default {target}")
    body = CONSUMER_TMPL.read_text().replace("__ANDROID_HOME__", _sdk_dir())
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_text(body)


def write_provider(app_dir: Path) -> None:
    target = app_dir / "android" / "local.properties"
    if target.is_file():
        log.info(f"keeping existing {target}")
        return
    log.info(f"writing default {target}")
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_text(
        f"""sdk.dir={_sdk_dir()}
MAPS_API_KEY=""
CONFIG_URL_DRIVER="http://localhost:8016"
CONFIG_URL_USER="http://localhost:8013"
"""
    )
