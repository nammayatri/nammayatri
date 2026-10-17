"""Workspace data (config-sync): pick a published version and load it.

The workspace's own test-context-api does the import; this module picks the
version and talks to it through the devbox front desk
(<Workspace URL>/test-context-api/...).

The published version lists live in an S3 bucket that only answers the
office's whitelisted internet line. Devboxes are on it; a laptop on the
office Wi-Fi may not be. So the lists are read through the devbox (curl over
SSH) whenever a devbox is known.

Moved from test-tool/local-api/server.py (helpers) and RemoteStackPanel.tsx
(the wait loop) so the devbox workflow can load data without a browser.
"""
from __future__ import annotations

import json
import os
import re
import shlex
import time
import urllib.error
import urllib.request
from typing import Callable

from . import ssh

SOURCES = ("master", "prod", "prod_international")
# A front-desk Workspace URL: http://<host>:<port>/<workspace-id>, nothing else.
WS_URL_RE = re.compile(r"^http://[A-Za-z0-9.\-]+:\d+/[A-Za-z0-9_-]+$")
# Where DB Manager publishes <source>_to_local/metadata.json. Same env vars and
# default as context-api's CONFIG_SYNC_PUBLIC_BASE.
PUBLIC_BASE = (
    os.getenv("CONFIG_SYNC_CLOUDFRONT_URL", "")
    or os.getenv("CONFIG_SYNC_PUBLIC_BUCKET_URL",
                 "https://backend-ny-config-sync.s3.ap-south-1.amazonaws.com")
)


def metadata_url(source: str) -> str:
    return f"{PUBLIC_BASE.rstrip('/')}/{source}_to_local/metadata.json"


def published_versions(t: ssh.Target | None, source: str) -> tuple[list, str | None]:
    """(versions, error) from <source>_to_local/metadata.json.

    Fetched on the devbox t (curl over SSH); with t=None, from this machine.
    A 404 means nothing is published for that source yet: no versions, no error."""
    url = metadata_url(source)
    if t is None:
        try:
            with urllib.request.urlopen(url, timeout=10) as r:
                return json.loads(r.read()).get("available_versions") or [], None
        except urllib.error.HTTPError as e:
            return [], (None if e.code == 404 else f"HTTP {e.code} for {url}")
        except Exception as e:
            return [], f"cannot reach {url}: {e}"
    try:
        # Body, then the status code on a line of its own.
        r = ssh.run(t, f"curl -sS --max-time 10 -w '\\n%{{http_code}}' {shlex.quote(url)}", timeout=25)
    except Exception as e:
        return [], f"cannot ask {t.host} for {url}: {e}"
    body, _, code = (r.stdout or "").rstrip("\n").rpartition("\n")
    if r.returncode != 0 or not code.isdigit():
        return [], f"{t.host} cannot reach {url}: {(r.stderr or '').strip() or f'exit {r.returncode}'}"
    if code == "404":
        return [], None
    if code != "200":
        return [], f"HTTP {code} for {url} (from {t.host})"
    try:
        return json.loads(body).get("available_versions") or [], None
    except ValueError:
        return [], f"not JSON from {url}"


def pick_version(versions: list) -> dict | None:
    """'Latest, but safe': the newest verified version, else the newest one not
    marked not_stable. None for an empty list (the import then falls back to
    config_transfer.py's pinned version)."""
    usable = [v for v in versions if isinstance(v, dict) and isinstance(v.get("version"), int)]
    for keep in (lambda v: v.get("status") == "verified",
                 lambda v: v.get("status") != "not_stable"):
        pool = [v for v in usable if keep(v)]
        if pool:
            return max(pool, key=lambda v: v["version"])
    return None


def ctx(ws: str, path: str, body: dict | None = None, timeout: float = 10.0) -> dict:
    """Call the workspace's test-context-api at <ws>/test-context-api<path>.

    `unreachable: True` means the front desk answered but the stack behind it
    is not up yet (or the dev-box is off) — the caller should wait and retry."""
    if not WS_URL_RE.match(ws or ""):
        return {"error": f"not a workspace URL: {ws!r}"}
    data = json.dumps(body).encode() if body is not None else None
    req = urllib.request.Request(
        f"{ws}/test-context-api{path}", data=data,
        method="POST" if data is not None else "GET",
        headers={"Content-Type": "application/json"})
    try:
        with urllib.request.urlopen(req, timeout=timeout) as r:
            return json.loads(r.read() or b"{}")
    except urllib.error.HTTPError as e:
        if e.code in (502, 503, 504):
            return {"unreachable": True, "error": f"stack not up yet (HTTP {e.code})"}
        try:
            msg = json.loads(e.read()).get("error")
        except Exception:
            msg = None
        return {"error": msg or f"HTTP {e.code}", "httpStatus": e.code}
    except Exception as e:
        return {"unreachable": True, "error": f"workspace not reachable: {e}"}


def sync_and_wait(ws: str, source: str, version: str, force: bool = False,
                  up_timeout: int = 1800, done_timeout: int = 1800,
                  out: Callable[[str], None] = print) -> dict:
    """Load source/version (e.g. "master", "v3") into the workspace and wait.

    Same rules as the 7070 card's Load data stage: wait for the workspace to
    answer, skip when it already holds that version (unless force), start the
    import, wait for a new run to finish.
    Returns {"state": "done"|"skipped", "detail": …}; raises RuntimeError on failure."""
    if source not in SOURCES:
        raise ValueError(f"unknown source {source!r}")
    if not re.fullmatch(r"v\d+", version or ""):
        raise ValueError(f"invalid version {version!r} (expected e.g. 'v3')")
    label = f"{source} {version}"

    out("[data] waiting for the stack…")
    up_by = time.time() + up_timeout
    st = ctx(ws, "/api/config-sync/status", timeout=5)
    while st.get("unreachable") and time.time() < up_by:
        time.sleep(5)
        st = ctx(ws, "/api/config-sync/status", timeout=5)
    if st.get("unreachable"):
        raise RuntimeError(f"stack did not come up within {up_timeout // 60} min")

    held = st.get("last_synced") or {}
    if not force and not st.get("running") and held.get("from") == source and held.get("version") == version:
        out(f"[data] already {label}")
        return {"state": "skipped", "detail": f"already {label}"}

    out(f"[data] syncing {label}…")
    before = st.get("started_at")
    r = ctx(ws, "/api/config-sync/import", {"from": source, "version": version})
    if r.get("error"):
        raise RuntimeError(r["error"])

    # Status may still show the previous run for a moment: wait for a new
    # started_at, then for it to stop running.
    done_by = time.time() + done_timeout
    while True:
        time.sleep(3)
        st = ctx(ws, "/api/config-sync/status", timeout=5)
        if not (st.get("unreachable") or st.get("running") or st.get("started_at") == before):
            break
        if time.time() > done_by:
            raise RuntimeError(f"sync did not finish within {done_timeout // 60} min")
    if st.get("error"):
        raise RuntimeError(st["error"])
    out(f"[data] done {label}")
    return {"state": "done", "detail": label}
