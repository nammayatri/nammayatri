"""Android emulator preflight: boot the first available AVD if none is
running, then wait until adb sees it."""
from __future__ import annotations

import os
import subprocess
import time
from pathlib import Path

from . import adb, log, sh


def _emulator_bin() -> Path | None:
    sdk = os.environ.get("ANDROID_HOME") or os.environ.get("ANDROID_SDK_ROOT")
    candidates: list[Path] = []
    if sdk:
        candidates.append(Path(sdk) / "emulator" / "emulator")
    candidates.append(Path.home() / "Library/Android/sdk/emulator/emulator")
    candidates.append(Path.home() / "Android/Sdk/emulator/emulator")
    for c in candidates:
        if c.is_file() and os.access(c, os.X_OK):
            return c
    return None


def list_avds() -> list[str]:
    binp = _emulator_bin()
    if binp is None:
        return []
    rc, out, _ = sh.capture([str(binp), "-list-avds"], timeout=10)
    if rc != 0:
        return []
    return [n for n in (line.strip() for line in out.splitlines()) if n]


def ensure_booted(boot_timeout_s: int = 90) -> None:
    if adb.has_emulator():
        log.info("existing Android emulator detected")
        return
    binp = _emulator_bin()
    avds = list_avds()
    if binp is None or not avds:
        log.err(
            "no Android emulator binary or AVDs available "
            "(install Android Studio + create an AVD)"
        )
        raise SystemExit(3)
    avd = avds[0]
    log.info(f"booting Android emulator {avd}")
    # Detached so the AVD survives this script's exit.
    subprocess.Popen(
        [str(binp), "-avd", avd, "-no-snapshot-load"],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
        start_new_session=True,
    )
    deadline = time.time() + boot_timeout_s
    while time.time() < deadline:
        if adb.has_emulator():
            log.info(f"emulator {avd} is online")
            return
        time.sleep(2)
    log.err(f"emulator {avd} did not come up within {boot_timeout_s}s")
    raise SystemExit(3)
