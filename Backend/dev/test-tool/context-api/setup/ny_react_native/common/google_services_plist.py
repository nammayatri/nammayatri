#!/usr/bin/env python3
"""iOS counterpart of ``google_services_json``: drop a placeholder
``GoogleService-Info.plist`` into ``consumer/ios/`` and into every
``consumer/ios/<Variant>/Plist/`` directory we find.

The ``consumer/README.md`` (step 4.2.5) mandates this file in both spots;
without it the ``[Firebase] copy GoogleService-Info.plist`` build phase
fails xcodebuild with code 65 before any compile starts. Existing files
are kept untouched so a real plist takes precedence.

The ``BUNDLE_ID`` field MUST match the variant's actual
``PRODUCT_BUNDLE_IDENTIFIER``: Firebase aborts hard
(``+[FIRApp configure]`` raises ``NSException`` → ``SIGABRT``) when the
plist's ``BUNDLE_ID`` does not match the running app's bundle id, so
the build "succeeds" but the app crashes <2s after launch. Pass the
resolved bundle id as the optional second arg so the active variant's
plist matches what xcodebuild will stamp into the ``.app``.

Usage:
    python3 -m setup.ny_react_native.common.google_services_plist <ios_dir> [<bundle_id>]
"""
from __future__ import annotations

import plistlib
import re
import sys
from pathlib import Path

PLIST: dict = {
    "CLIENT_ID":           "000000000000-aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa.apps.googleusercontent.com",
    "REVERSED_CLIENT_ID":  "com.googleusercontent.apps.000000000000-aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    # API_KEY format must satisfy FIRInstallations' validator:
    #   ^AIza[A-Za-z0-9_-]{35}$  (39 chars total).
    # Earlier "AIzaSyDUMMY-not-functional-iOS" was only 30 chars — failed
    # validation in +[FIRInstallations validateAPIKey:] → NSException →
    # SIGABRT during +[FIRApp configure], crashing the app <2s after
    # launch with no JS bundle ever evaluating. Pad to 35 chars in the
    # tail so the format check passes; Firebase still fails any actual
    # network call but the app at least gets past install.
    "API_KEY":             "AIzaabcdefghijklmnopqrstuvwxyzABCDEFGHI",
    "GCM_SENDER_ID":       "000000000000",
    "PLIST_VERSION":       "1",
    # BUNDLE_ID is overwritten at runtime by the runner with the variant's
    # actual PRODUCT_BUNDLE_IDENTIFIER — see write_google_services_plist().
    # The placeholder here is only used if the resolver fails (e.g.
    # xcodebuild not on PATH).
    "BUNDLE_ID":           "in.juspay.nammayatri.dev",
    "PROJECT_ID":          "ny-rn-dev-placeholder",
    "STORAGE_BUCKET":      "ny-rn-dev-placeholder.appspot.com",
    "IS_ADS_ENABLED":      False,
    "IS_ANALYTICS_ENABLED": False,
    "IS_APPINVITE_ENABLED": True,
    "IS_GCM_ENABLED":      True,
    "IS_SIGNIN_ENABLED":   True,
    # GOOGLE_APP_ID format must satisfy FirebaseCore's local regex
    # `^1:\d+:ios:[a-fA-F0-9]+$`. The previous "dummy0…" suffix failed
    # validation (`d`, `u`, `m`, `y` aren't hex digits) and Firebase
    # aborted with `[FirebaseCore][I-COR000009] The GOOGLE_APP_ID … is
    # invalid` → uncaught NSException → SIGABRT during +[FIRApp configure].
    # All-zero hex passes the format check; Firebase still fails any
    # network call but the app at least gets past the configure step.
    "GOOGLE_APP_ID":       "1:000000000000:ios:0000000000000000",
    "DATABASE_URL":        "https://ny-rn-dev-placeholder.firebaseio.com",
}


def _is_stale_dummy(cur: dict, target_bundle_id: str | None) -> bool:
    """Decide whether an existing plist is a previously-written dummy that
    needs refreshing. A real production plist (a download from the Firebase
    console) has a real PROJECT_ID — never our placeholder — so we never
    touch those. A dummy from an earlier runner version is stale if its
    BUNDLE_ID doesn't match the active variant, OR its GOOGLE_APP_ID is in
    the old non-hex format (FIRApp's I-COR000009), OR its API_KEY does not
    satisfy FIRInstallations' format check (caused SIGABRT < 2s into
    launch on every dummy plist written before this fix)."""
    if cur.get("PROJECT_ID") != PLIST["PROJECT_ID"]:
        return False  # real prod plist — leave alone
    if target_bundle_id and cur.get("BUNDLE_ID") != target_bundle_id:
        return True
    app_id = str(cur.get("GOOGLE_APP_ID", ""))
    # Firebase: ^1:\d+:ios:[a-fA-F0-9]+$
    if not re.match(r"^1:\d+:ios:[a-fA-F0-9]+$", app_id):
        return True
    # FIRInstallations: ^AIza[A-Za-z0-9_-]{35}$
    api_key = str(cur.get("API_KEY", ""))
    if not re.match(r"^AIza[A-Za-z0-9_-]{35}$", api_key):
        return True
    return False


def _variant_dirs(ios_dir: Path) -> list[Path]:
    """Discover per-variant directories under <ios_dir>/.

    The two repos use different layouts:
      consumer/ios/<Variant>/Plist/Info.plist  (Plist subdir)
      provider/ios/<Variant>/{Debug,Release}/Info.plist  (config subdirs)

    We treat any immediate child of <ios_dir> that contains an Info.plist
    within depth 2 as a variant directory. This filters out build/ Pods/
    fastlane/ scripts/ etc. (they have no Info.plist), avoiding stray
    plists in non-variant dirs while covering both layouts."""
    if not ios_dir.is_dir():
        return []
    out: list[Path] = []
    for child in sorted(ios_dir.iterdir()):
        if not child.is_dir():
            continue
        # Skip artifact/build dirs by name — cheap and explicit.
        if child.name in {"Pods", "build", "fastlane", "node_modules",
                          "scripts", "Frameworks", "Common", "AppConfigs",
                          "assets", "Assets"}:
            continue
        if child.suffix in {".xcodeproj", ".xcworkspace",
                            ".framework", ".bundle", ".xcframework"}:
            continue
        # Variant dirs always carry an Info.plist somewhere within depth-2
        # — either directly (provider/<Variant>/Info.plist),
        # in a Plist/ subdir (consumer/<Variant>/Plist/Info.plist), or in
        # a Debug/Release config subdir (provider/<Variant>/Debug/Info.plist).
        has_info = (child / "Info.plist").is_file()
        if not has_info:
            for sub in ("Plist", "Debug", "Release", "Internal"):
                if (child / sub / "Info.plist").is_file():
                    has_info = True
                    break
        if has_info:
            out.append(child)
    return out


def write_google_services_plist(ios_dir: Path, bundle_id: str | None = None) -> None:
    if not ios_dir.is_dir():
        return
    targets: list[Path] = [ios_dir / "GoogleService-Info.plist"]
    for variant in _variant_dirs(ios_dir):
        # Provider layout: GoogleService-Info.plist sits at the variant root
        # (alongside Debug/Release subdirs) — the build phase references
        # `<Variant>/GoogleService-Info.plist`.
        targets.append(variant / "GoogleService-Info.plist")
        # Consumer layout: build phase references
        # `<Variant>/Plist/GoogleService-Info.plist`. Drop the file there
        # too so the same writer covers both repos. If the Plist/ subdir
        # doesn't exist we just don't write into it (don't manufacture an
        # empty Plist/ in the provider tree).
        plist_subdir = variant / "Plist"
        if plist_subdir.is_dir():
            targets.append(plist_subdir / "GoogleService-Info.plist")
    plist = dict(PLIST)
    if bundle_id:
        plist["BUNDLE_ID"] = bundle_id
    written = overwritten = kept = 0
    for t in targets:
        if t.is_file():
            try:
                with open(t, "rb") as f:
                    cur = plistlib.load(f)
            except Exception:
                cur = {}
            if not _is_stale_dummy(cur, bundle_id):
                kept += 1
                continue
            overwritten += 1
        # Don't auto-mkdir for non-existent parents — variant root dirs
        # always exist (we discovered them); we only ever skip parent
        # creation in the path-walking logic above.
        if not t.parent.is_dir():
            continue
        with open(t, "wb") as f:
            plistlib.dump(plist, f)
        written += 1
    bundle_note = f" bundle_id={bundle_id}" if bundle_id else ""
    print(
        f"ny-react-native: GoogleService-Info.plist written={written} "
        f"overwritten={overwritten} kept={kept} base={ios_dir}{bundle_note}"
    )


def main(argv: list[str]) -> int:
    if len(argv) not in (2, 3):
        print(
            "usage: google_services_plist.py <ios-dir> [<bundle-id>]",
            file=sys.stderr,
        )
        return 2
    bundle_id = argv[2] if len(argv) == 3 else None
    write_google_services_plist(Path(argv[1]), bundle_id)
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
