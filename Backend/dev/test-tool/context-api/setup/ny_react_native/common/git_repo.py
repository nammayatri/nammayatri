"""Clone-or-pull the ny-react-native repo (with submodules) into the
default location, or whichever ``NY_RN_PATH`` points at.

We clone over SSH (``git@github.com:…``) rather than HTTPS so the credential
context carries through to the private ``nammayatri-ios`` submodule —
HTTPS clones fail at the submodule step the moment the user lacks creds,
which surfaced as 'submodule update failed' before this was forced on."""
from __future__ import annotations

from pathlib import Path

from . import log, sh, ssh

GIT_URL = "git@github.com:nammayatri/ny-react-native"


def clone_or_pull(target: Path) -> None:
    if (target / ".git").is_dir():
        log.info(f"pulling latest in {target}")
        # Don't bail if pull fails (offline / divergent); we can still
        # build the existing checkout.
        rc = sh.run_ok(["git", "-C", str(target), "pull", "--ff-only"])
        if rc != 0:
            log.info("pull failed, continuing with existing checkout")
        return

    log.info(f"cloning {GIT_URL} into {target} (recursive)")
    target.parent.mkdir(parents=True, exist_ok=True)
    rc = sh.run_ok(
        ["git", "clone", "--recurse-submodules", GIT_URL, str(target)]
    )
    if rc != 0:
        log.err("clone failed.")
        ssh.ssh_diagnostics()
        raise SystemExit(3)


def sync_submodules(target: Path) -> None:
    log.info("syncing submodules")
    sh.run_ok(["git", "-C", str(target), "submodule", "sync", "--recursive"])
    rc = sh.run_ok(
        ["git", "-C", str(target), "submodule", "update", "--init", "--recursive"]
    )
    if rc != 0:
        print("")
        log.err("submodule init failed.")
        ssh.ssh_diagnostics()
        raise SystemExit(4)


def checkout_ref(target: Path, ref: str) -> None:
    """Fetch + check out an arbitrary ref (branch name or commit SHA).
    The bash wrapper has set ``NY_RN_REF=<value>`` and we run after the
    initial clone-or-pull. We do a fresh ``git fetch origin <ref>`` so
    branches that landed since the last pull are still resolvable, then
    detach onto FETCH_HEAD. Submodules are re-synced after by the caller."""
    log.info(f"checking out ref {ref!r} in {target}")
    rc = sh.run_ok(["git", "-C", str(target), "fetch", "origin", ref, "--tags"])
    if rc != 0:
        # Some hosts reject `git fetch origin <sha>`; retry by SHA via a
        # broader fetch so we still resolve commit-SHA refs.
        log.info(f"direct fetch failed; falling back to `git fetch origin`")
        sh.run_ok(["git", "-C", str(target), "fetch", "origin", "--tags"])
    rc = sh.run_ok(
        ["git", "-C", str(target), "checkout", "--detach", "FETCH_HEAD"]
    )
    if rc != 0:
        # FETCH_HEAD may not point at our ref if the fallback fetch ran.
        # Try checking out the literal ref name as a last resort.
        rc = sh.run_ok(["git", "-C", str(target), "checkout", "--detach", ref])
        if rc != 0:
            log.err(f"could not check out ref {ref!r}")
            raise SystemExit(8)
    sh.run_ok(["git", "-C", str(target), "log", "-1", "--oneline"])
