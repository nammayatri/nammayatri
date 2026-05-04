"""iOS Simulator preflight: boot one if none is running and capture the
booted UDID so we can pass ``--udid`` to ``react-native run-ios``. Without
that, RN CLI auto-picks the first connected device — and if a real iPhone
is paired/plugged-in, the build fails at code-signing with no useful log."""
from __future__ import annotations

import json

from . import log, sh


def is_any_booted() -> bool:
    rc, out, _ = sh.capture(["xcrun", "simctl", "list", "devices", "booted"])
    return rc == 0 and "Booted" in out


def first_booted_udid() -> str | None:
    rc, out, _ = sh.capture(
        ["xcrun", "simctl", "list", "devices", "booted", "-j"],
    )
    if rc != 0:
        return None
    try:
        data = json.loads(out)
    except json.JSONDecodeError:
        return None
    for runtime in data.get("devices", {}).values():
        for d in runtime:
            if d.get("state") == "Booted":
                return d.get("udid")
    return None


def first_available_udid() -> str | None:
    rc, out, _ = sh.capture(
        ["xcrun", "simctl", "list", "devices", "available", "-j"],
    )
    if rc != 0:
        return None
    try:
        data = json.loads(out)
    except json.JSONDecodeError:
        return None
    for runtime in data.get("devices", {}).values():
        for d in runtime:
            if d.get("isAvailable"):
                return d.get("udid")
    return None


def ensure_booted() -> str | None:
    """Boot any available Simulator if none is running. Returns the
    booted UDID (best-effort; may be ``None`` if Xcode is not installed)."""
    rc, _, _ = sh.capture(["xcrun", "--version"])
    if rc != 0:
        log.err("xcrun not on PATH (Xcode required for iOS)")
        raise SystemExit(3)

    if not is_any_booted():
        udid = first_available_udid()
        if not udid:
            log.err("no iOS Simulator devices available")
            raise SystemExit(3)
        log.info(f"booting iOS Simulator {udid}")
        sh.run_ok(["xcrun", "simctl", "boot", udid])
        sh.run_ok(["open", "-a", "Simulator"])
    else:
        log.info("existing iOS Simulator detected")

    booted = first_booted_udid()
    if booted:
        log.info(
            f"targeting iOS Simulator {booted} "
            "(skipping any connected real device)"
        )
    else:
        log.warn(
            "could not resolve booted Simulator UDID — RN CLI may pick a "
            "real device and fail at code-signing"
        )
    return booted
