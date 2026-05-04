"""adb wrappers — pin the right binary, list devices, apply reverses.

The launcher uses ``adb`` for: emulator preflight, port-forward setup
between emulator-localhost and host:8013/8016, and per-app logcat tailing.
nix's ``adb`` and the SDK's ``$ANDROID_HOME/platform-tools/adb`` start
DIFFERENT daemons; whichever runs last wins, breaking the other's view.
We pin to the SDK adb so it lines up with the user's Studio /
emulator command."""
from __future__ import annotations

import os
from pathlib import Path

from . import sh

REVERSE_PORTS = (8013, 8016, 8081, 8082)


def resolve_bin() -> str:
    sdk = os.environ.get("ANDROID_HOME") or os.environ.get("ANDROID_SDK_ROOT")
    candidates: list[Path] = []
    if sdk:
        candidates.append(Path(sdk) / "platform-tools" / "adb")
    candidates.append(Path.home() / "Library/Android/sdk/platform-tools/adb")
    candidates.append(Path.home() / "Android/Sdk/platform-tools/adb")
    for c in candidates:
        if c.is_file() and os.access(c, os.X_OK):
            return str(c)
    return "adb"


def serials() -> list[str]:
    rc, out, _ = sh.capture([resolve_bin(), "devices"], timeout=5)
    if rc != 0:
        return []
    result = []
    for line in out.splitlines()[1:]:
        line = line.strip()
        if not line or "\tdevice" not in line:
            continue
        result.append(line.split("\t", 1)[0])
    return result


def has_emulator() -> bool:
    return any(s.startswith("emulator-") for s in serials())


def apply_reverses(label: str = "") -> None:
    """Re-apply ``adb reverse tcp:N tcp:N`` for every port in
    REVERSE_PORTS. Idempotent — adbd silently replaces existing entries."""
    adb = resolve_bin()
    for s in serials():
        for port in REVERSE_PORTS:
            sh.run_ok(
                [adb, "-s", s, "reverse", f"tcp:{port}", f"tcp:{port}"],
            )
    rc, out, _ = sh.capture([adb, "reverse", "--list"], timeout=3)
    suffix = f" ({label})" if label else ""
    print(f"ny-react-native: adb reverse wired{suffix} — current forwards:")
    for line in (out or "").splitlines():
        print(f"  {line}")
