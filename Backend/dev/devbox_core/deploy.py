"""Code sync: copy a nammayatri checkout into a devbox workspace.

rsync (with the shared exclude list), then a throwaway git repo on the devbox
plus Backend/.ci-cache-sha naming the MinIO build cache to restore.

Moved from test-tool/local-api/server.py so the devbox workflow can sync too.
"""
from __future__ import annotations

import shlex
import subprocess
from typing import Callable

from . import ssh, workspace

# Build artefacts: never copied, and exactly what `cabal clean` wipes on the
# stack host (see remote_cabal_clean) — one list so the two can't drift.
BUILD_STATE_EXCLUDES = [
    "dist-newstyle",
    "dist",
    "_build",
    "result", "result-*",
    ".cabal-dir",
    ".hie",
    "hie",
    ".nix-deps",
    ".ci-project-root",
    ".ci-cabal-dir",
    ".ci-cache-sha",
    "cabal.project.local",
]

# Everything cabal clean wipes — the build-state list minus files a developer
# may have hand-written (cabal.project.local is theirs, not a build artefact).
CABAL_CLEAN_TARGETS = [e for e in BUILD_STATE_EXCLUDES if e != "cabal.project.local"]

# Per-machine state: never copied, and never deleted by cabal clean either.
# data/ holds the stack host's own published port map (data/ports.json)
# plus its service state, so excluding it keeps a redeploy from clobbering them.
MACHINE_STATE_EXCLUDES = [
    "data", "node_modules", ".direnv",
    ".git",
    # The source machine's devbox pin. The stack's pre-flight reads it first to
    # pick its registry key (scripts.nix), so a copy would make a workspace act
    # as the source's own workspace; without it the key comes from the folder
    # name /tmp/<key>/nammayatri, which is what every workspace wants.
    ".devbox-id.json",
    "*.log",
    ".stack-state.json",
    "Frontend/android-native", "Frontend/ios",
    "Frontend/build", "Frontend/dist",
]

# Single source of truth for what never leaves the source machine: the rsync
# --exclude list, and (in local-api) the filter for the workspace fingerprint.
EXCLUDES = BUILD_STATE_EXCLUDES + MACHINE_STATE_EXCLUDES


def cache_commit(project_root: str, max_n: int = 30) -> str:
    """The nearest ancestor commit whose build CI pushed to MinIO, or "".

    Needs git history: main and the minio-pushed/* tags are fetched here
    (best effort), and up to max_n commits back from the merge-base with main
    are checked. A shallow checkout finds nothing → cabal builds from scratch."""
    def _git(args, timeout=20):
        return subprocess.run(
            ["git", "-C", project_root, *args],
            capture_output=True, text=True, timeout=timeout,
        )

    # 1. best-effort fetch (works offline with stale refs; failures are non-fatal)
    for fetch_args in (
        ["fetch", "--quiet", "origin",
         "refs/tags/minio-pushed/*:refs/tags/minio-pushed/*"],
        ["fetch", "--quiet", "origin", "main"],
    ):
        try:
            _git(fetch_args, timeout=30)
        except Exception:  # noqa: BLE001
            pass

    # 2. merge-base with main (fallback HEAD)
    base = "HEAD"
    try:
        mb = _git(["merge-base", "origin/main", "HEAD"], timeout=15)
        if mb.returncode == 0 and mb.stdout.strip():
            base = mb.stdout.strip()
    except Exception:  # noqa: BLE001
        pass

    # 3. walk back and return the NEAREST commit carrying a minio-pushed tag —
    #    the single commit whose build cache cache-restore should pull. CI tags a
    #    commit once its build is pushed to MinIO. If none of the recent ancestors
    #    is tagged, return "" and cabal builds from scratch.
    walk = []
    try:
        lg = _git(["log", "--format=%H", "-n", str(max_n), base], timeout=15)
        if lg.returncode == 0:
            walk = lg.stdout.split()
    except Exception:  # noqa: BLE001
        pass

    for sha in walk:
        try:
            r = _git(["rev-parse", "-q", "--verify",
                      f"refs/tags/minio-pushed/{sha}"], timeout=10)
            if r.returncode == 0:
                return sha
        except Exception:  # noqa: BLE001
            pass
    return ""


def _ssh_base(t: ssh.Target) -> list:
    """The plain ssh options rsync and the git-init step use."""
    cmd = [
        "ssh",
        "-o", "StrictHostKeyChecking=accept-new",
        "-o", "BatchMode=yes",
        "-o", "ConnectTimeout=10",
        "-p", str(t.port),
    ]
    if t.identity:
        cmd += ["-i", t.identity]
    return cmd


def rsync_argv(src_dir: str, t: ssh.Target, remote_dir: str, dry_run: bool = False) -> list:
    """rsync src_dir/ → user@host:remote_dir/ with --delete and the exclude list."""
    excludes = []
    for ex in EXCLUDES:
        excludes += ["--exclude", ex]

    # Use --rsync-path to create the destination directory on the remote before
    # rsync starts — avoids a separate SSH round-trip for mkdir.
    rsync_path = f"mkdir -p {shlex.quote(remote_dir)} && rsync"

    return (
        ["rsync", "-az", "--delete", "--progress", "--stats"]
        + (["--dry-run"] if dry_run else [])
        + ["-e", " ".join(_ssh_base(t)), "--rsync-path", rsync_path]
        + excludes
        + [f"{src_dir.rstrip('/')}/", f"{t.user}@{t.host}:{remote_dir}/"]
    )


def git_init_script(remote_dir: str, commit: str) -> str:
    """Remote command run after rsync: a fresh one-commit git repo (the build
    expects one) and Backend/.ci-cache-sha = the cache commit."""
    git_ignore_cmd = (
        "printf '%s\\n' 'dist-newstyle/' 'hie/' '.hie/' '.nix-deps/' "
        "'.cabal-dir/' '.ci-project-root' '.ci-cabal-dir' '.ci-cache-sha' "
        "> .git/info/exclude"
    )
    return (
        f"cd {shlex.quote(remote_dir)} && "
        f"rm -rf .git && git init -q && {git_ignore_cmd} && git add -A && "
        f"GIT_AUTHOR_NAME=deploy GIT_AUTHOR_EMAIL=deploy@deploy "
        f"GIT_COMMITTER_NAME=deploy GIT_COMMITTER_EMAIL=deploy@deploy "
        f"git commit -q -m deploy --allow-empty 2>/dev/null || true; "
        f"mkdir -p Backend && printf '%s' {shlex.quote(commit)} "
        f"> Backend/.ci-cache-sha"
    )


def post_ssh_argv(t: ssh.Target, remote_dir: str, commit: str) -> list:
    """ssh argv that runs git_init_script on the devbox."""
    return _ssh_base(t) + [f"{t.user}@{t.host}", git_init_script(remote_dir, commit)]


def sync(src_dir: str, t: ssh.Target, name: str, owner: dict | None = None,
         dry_run: bool = False, out: Callable[[str], None] = print) -> dict:
    """register → rsync → git init. Streams rsync output to `out`.

    Returns {"dir", "cacheCommit"}; raises RuntimeError when a step fails."""
    entry = workspace.register(t, name, owner)
    remote_dir = entry["dir"]

    commit = cache_commit(src_dir)
    out(f"[sync] minio cache commit: {commit}" if commit else
        "[sync] no minio-pushed cache commit found — cabal will build from scratch")

    argv = rsync_argv(src_dir, t, remote_dir, dry_run=dry_run)
    out(f"[sync] {' '.join(shlex.quote(a) for a in argv)}")
    proc = subprocess.Popen(argv, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                            text=True, errors="replace")
    for line in proc.stdout:
        out(line.rstrip("\n"))
    if proc.wait() != 0:
        raise RuntimeError(f"rsync failed (exit {proc.returncode})")

    if dry_run:
        out("[sync] dry run — nothing copied, git init skipped")
    else:
        r = subprocess.run(post_ssh_argv(t, remote_dir, commit),
                           capture_output=True, text=True, timeout=300)
        if r.returncode != 0:
            raise RuntimeError(f"git init on the devbox failed: {(r.stderr or '').strip()}")
        out("[sync] git init done")
    return {"dir": remote_dir, "cacheCommit": commit}
