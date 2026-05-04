#!/usr/bin/env python3
"""Point the provider Android app's gRPC client at the local
notification-service (process-compose entry `notification-service`,
gRPC on :50051) instead of the upstream prod default
``grpc.moving.tech``.

Two on-disk references in the provider:
  1. android/app/src/main/res/xml/remote_config_defaults.xml — JSON
     blob inside ``<value>{"enabled":true,"address":"…","port":…}</value>``
     that Firebase Remote Config seeds defaults with on first launch.
     Without rewriting this, the app remembers the prod URL across
     installs (Remote Config persists in shared prefs).
  2. android/app/src/main/java/in/juspay/mobility/services/GRPCService.kt
     — hardcoded fallback used when Remote Config has nothing yet.

iOS has no native gRPC client in the provider source — only the
CocoaPods-linked gRPC libs — so this helper is Android-only.

Usage:
    python3 -m setup.ny_react_native.common.grpc_local_url <provider_app_dir>

The launcher's adb-reverse heartbeat in server.py already forwards
:50051 from the emulator's localhost back to the host machine.
"""
from __future__ import annotations

import re
import sys
from pathlib import Path

LOCAL_HOST = "localhost"
LOCAL_PORT = 50051


def patch_remote_config_xml(app_dir: Path) -> bool:
    target = app_dir / "android" / "app" / "src" / "main" / "res" / "xml" / "remote_config_defaults.xml"
    if not target.is_file():
        return False
    text = target.read_text(encoding="utf-8")
    # Match the grpc_config <value>...</value> body. The defaults file
    # uses single-line JSON, so a permissive substring rewrite is enough.
    pat = re.compile(
        r'(<value>\{"enabled":\s*\w+\s*,\s*"address":\s*")[^"]+(",\s*"port":\s*)\d+(\}</value>)'
    )
    new_text, n = pat.subn(
        rf'\g<1>{LOCAL_HOST}\g<2>{LOCAL_PORT}\g<3>',
        text,
    )
    if n == 0:
        # Defensive: file structure changed upstream.
        print(
            f"  WARN no grpc_config <value> match in {target} — leaving as-is",
            file=sys.stderr,
        )
        return False
    if new_text != text:
        target.write_text(new_text, encoding="utf-8")
        print(f"  patched grpc_config in {target.relative_to(app_dir)} → {LOCAL_HOST}:{LOCAL_PORT}")
    else:
        print(f"  grpc_config already set to {LOCAL_HOST}:{LOCAL_PORT} in {target.relative_to(app_dir)}")
    return True


def patch_grpc_service_kt(app_dir: Path) -> bool:
    target = (
        app_dir / "android" / "app" / "src" / "main" / "java"
        / "in" / "juspay" / "mobility" / "services" / "GRPCService.kt"
    )
    if not target.is_file():
        return False
    text = target.read_text(encoding="utf-8")
    addr_pat = re.compile(r'(private\s+var\s+grpcAddress\s*=\s*")[^"]+(")')
    port_pat = re.compile(r'(private\s+var\s+grpcPort\s*=\s*)\d+')
    # Also rewrite the inline fallback in the Remote-Config getter
    # (`?: "grpc.moving.tech"`) so even if Firebase fetches succeed but
    # return the upstream value, the app falls back to localhost.
    fallback_pat = re.compile(r'(getString\("address"\)\s*\?:\s*")grpc\.moving\.tech(")')

    new_text, n1 = addr_pat.subn(rf'\g<1>{LOCAL_HOST}\g<2>', text)
    new_text, n2 = port_pat.subn(rf'\g<1>{LOCAL_PORT}', new_text)
    new_text, n3 = fallback_pat.subn(rf'\g<1>{LOCAL_HOST}\g<2>', new_text)
    if n1 + n2 + n3 == 0:
        print(
            f"  WARN no grpcAddress/grpcPort patterns in {target} — leaving as-is",
            file=sys.stderr,
        )
        return False
    if new_text != text:
        target.write_text(new_text, encoding="utf-8")
        print(
            f"  patched GRPCService.kt → {LOCAL_HOST}:{LOCAL_PORT} "
            f"(address={n1}, port={n2}, fallback={n3})"
        )
    else:
        print(
            f"  GRPCService.kt already pointing at {LOCAL_HOST}:{LOCAL_PORT}"
        )
    return True


def point_grpc_at_local(app_dir: Path) -> None:
    print(
        f"ny-react-native: provider · pointing gRPC at "
        f"{LOCAL_HOST}:{LOCAL_PORT} (notification-service)"
    )
    patch_remote_config_xml(app_dir)
    patch_grpc_service_kt(app_dir)


def main(argv: list[str]) -> int:
    if len(argv) != 2:
        print("usage: grpc_local_url.py <provider-app-dir>", file=sys.stderr)
        return 2
    point_grpc_at_local(Path(argv[1]))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
