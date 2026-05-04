"""ny-react-native launcher orchestrator.

Driven by env vars (set by test-context-api before Popen):
    NY_RN_APP       customer | driver | both
    NY_RN_PLATFORM  android | ios
    NY_RN_VARIANT   <Brand>  (e.g. lynx, NammaYatri, BharatTaxi)
    NY_RN_PATH      optional explicit checkout path; default = <repo>/data/ny-react-native

Pipeline:
    1. configure_git_ssh                     [outside nix shell]
    2. clone_or_pull + sync_submodules       [outside nix shell, our git]
    3. submodule sanity check (per app)      [outside nix shell]
    4. exec _build_runner.sh inside nix shell (Node 22, yarn, jdk17, git)

The outer wrapper (``ny-react-native-setup.sh``) sets env vars and ``exec``s
``python3 -m setup.ny_react_native``. We do steps 1–3 directly and then
hand off to the inner bash via ``os.execvpe`` so test-context-api keeps
streaming the build log line-by-line into its per-app buffer.
"""
from __future__ import annotations

import os
import sys
from pathlib import Path

# Make sibling packages importable when invoked as
#   python3 -m setup.ny_react_native
# from the context-api directory. When invoked via ``python3 path/file.py``
# the parent dir won't be on sys.path; resolve it manually.
_HERE = Path(__file__).resolve().parent
if str(_HERE.parents[1]) not in sys.path:
    sys.path.insert(0, str(_HERE.parents[1]))

from setup.ny_react_native.common import git_repo, log, paths, ssh  # noqa: E402

VALID_APPS = {"customer", "driver", "both"}
VALID_PLATFORMS = {"android", "ios"}


def _validate_env() -> tuple[str, str, str]:
    app = os.environ.get("NY_RN_APP", "customer")
    platform = os.environ.get("NY_RN_PLATFORM", "android")
    variant = os.environ.get("NY_RN_VARIANT", "debug")
    if app not in VALID_APPS:
        log.err(f"NY_RN_APP must be customer|driver|both, got {app!r}")
        raise SystemExit(2)
    if platform not in VALID_PLATFORMS:
        log.err(f"NY_RN_PLATFORM must be android|ios, got {platform!r}")
        raise SystemExit(2)
    return app, platform, variant


def _check_ios_submodules(ny_rn_dir: Path, apps: list[str]) -> None:
    """The nammayatri-ios submodule is required for iOS builds. On the
    current upstream branch it ships only ``mobility-customer/`` (no
    ``mobility-driver/``).

    Important asymmetry: the consumer (rider) Podfile DOES reference
    ``nammayatri-ios/mobility-customer/`` for its podspecs, so we must
    fail loud if that's missing. The provider (driver) Podfile pulls
    ALL its pods from the ``ny-cocoapods-specs`` private spec repo
    instead — it does not reference any sibling subdir under
    ``nammayatri-ios/``. So for provider builds we only check that the
    submodule itself populated (cheap sanity), not that any per-app
    subdir exists."""
    if os.environ.get("NY_RN_PLATFORM") != "ios":
        return
    sub_root = ny_rn_dir / "nammayatri-ios"
    if not sub_root.is_dir() or not any(sub_root.iterdir()):
        print("")
        log.err(
            f"nammayatri-ios/ submodule did not populate under {ny_rn_dir}"
        )
        print(f"  iOS builds need this submodule for at least the spec source repo.")
        print(f"  Re-clone the parent repo over SSH so submodule auth carries through:")
        print(
            f"    rm -rf {ny_rn_dir} && git clone --recurse-submodules "
            f"git@github.com:nammayatri/ny-react-native {ny_rn_dir}"
        )
        print("  Or set NY_RN_PATH=…/your-checkout if you have one.")
        raise SystemExit(5)
    # Consumer-only: also assert the per-app podspec subdir.
    if "consumer" in apps:
        sub = "mobility-customer"
        sub_dir = sub_root / sub
        if not sub_dir.is_dir() or not any(sub_dir.iterdir()):
            print("")
            log.err(
                f"nammayatri-ios/{sub} is missing or empty under {ny_rn_dir}"
            )
            print(f"  The consumer iOS Podfile expects podspecs under nammayatri-ios/{sub}/.")
            print(f"  Re-clone the parent repo over SSH so submodule auth carries through:")
            print(
                f"    rm -rf {ny_rn_dir} && git clone --recurse-submodules "
                f"git@github.com:nammayatri/ny-react-native {ny_rn_dir}"
            )
            print("  Or set NY_RN_PATH=…/your-checkout if you have one.")
            print(
                "  If the submodule populated but this dir is genuinely absent,"
            )
            print(f"  the branch may not ship {sub} yet.")
            raise SystemExit(5)


def _exec_build_runner(apps: list[str]) -> None:
    """Hand off to ``_build_runner.sh`` inside a nix shell with modern
    Node + Yarn + JDK 17 on PATH. This is the original ``bash -c '…'``
    body, lifted into its own file. We pass everything via env vars so
    the inner script does not need positional args."""
    runner = paths.SETUP_DIR / "_build_runner.sh"
    if not runner.is_file():
        log.err(f"build runner missing: {runner}")
        raise SystemExit(6)

    env = {
        **os.environ,
        "NY_RN_APPS": " ".join(apps),
        "REPO_ROOT": str(paths.REPO_ROOT),
        "NY_RN_DIR": str(paths.ny_rn_dir()),
    }

    # ``nix shell`` resolves flake inputs by name via --inputs-from. We
    # reuse the project flake's nixpkgs-unstable input the same way the
    # control-center launcher does.
    cmd = [
        "nix", "shell",
        "--inputs-from", str(paths.REPO_ROOT),
        "nixpkgs-unstable#nodejs_22",
        "nixpkgs-unstable#yarn",
        "nixpkgs-unstable#git",
        "nixpkgs-unstable#jdk17",
        # gh — for the dashboard's git-refs picker. Used by server.py's
        # /api/git/refs to enrich the local git data with the remote
        # branch list. Bundling it here keeps the binary on PATH for any
        # in-build hooks that want it too.
        "nixpkgs-unstable#gh",
        "--command", "bash", str(runner),
    ]
    os.execvpe(cmd[0], cmd, env)


def main() -> int:
    app, platform, variant = _validate_env()
    ny_rn_dir = paths.ny_rn_dir()
    log.info(f"app={app} platform={platform} variant={variant}")
    log.info(f"dir={ny_rn_dir}")

    apps = (
        ["consumer"] if app == "customer"
        else ["provider"] if app == "driver"
        else ["consumer", "provider"]
    )

    ssh.configure_git_ssh()
    git_repo.clone_or_pull(ny_rn_dir)
    # Optional: a specific ref (branch name or commit SHA) requested by
    # the dashboard's launcher dropdown. Checkout BEFORE submodule init
    # so the submodule pointers from that ref are the ones that get
    # resolved (different branches may pin different submodule SHAs).
    requested_ref = os.environ.get("NY_RN_REF", "").strip()
    if requested_ref:
        git_repo.checkout_ref(ny_rn_dir, requested_ref)
    git_repo.sync_submodules(ny_rn_dir)
    _check_ios_submodules(ny_rn_dir, apps)

    _exec_build_runner(apps)
    return 0  # unreachable — execvpe replaces this process


if __name__ == "__main__":
    sys.exit(main())
