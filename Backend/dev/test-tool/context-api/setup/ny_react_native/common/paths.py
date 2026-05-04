"""Project / repo path resolution. Reads env vars set by the bash shim
(or by server.py). Centralised so every other module asks here instead
of redo'ing path math."""
from __future__ import annotations

import os
from pathlib import Path

# This file lives at:
#   nammayatri/Backend/dev/test-tool/context-api/setup/ny_react_native/common/paths.py
# Walking parents:
#   [0] common  [1] ny_react_native  [2] setup  [3] context-api
#   [4] test-tool  [5] dev  [6] Backend  [7] nammayatri (project root)
THIS = Path(__file__).resolve()
REPO_ROOT = THIS.parents[7]            # …/nammayatri (the project root)
SETUP_DIR = THIS.parents[1]            # …/setup/ny_react_native
TEMPLATES_DIR = SETUP_DIR / "templates"
DATA_DIR = REPO_ROOT / "data"
DEFAULT_NY_RN_DIR = DATA_DIR / "ny-react-native"


def ny_rn_dir() -> Path:
    """Where the ny-react-native checkout lives. ``$NY_RN_PATH`` overrides
    the default so a dev with a working clone elsewhere can point at it."""
    override = os.environ.get("NY_RN_PATH")
    return Path(override).expanduser() if override else DEFAULT_NY_RN_DIR


def app_dir(app: str) -> Path:
    """``app`` is one of {customer, driver}. customer → consumer/, driver
    → provider/. (The dashboard / API uses customer/driver; the on-disk
    layout uses consumer/provider — this helper smooths over the gap.)"""
    if app == "customer":
        return ny_rn_dir() / "consumer"
    if app == "driver":
        return ny_rn_dir() / "provider"
    raise ValueError(f"unknown app {app!r} (expected customer|driver)")
