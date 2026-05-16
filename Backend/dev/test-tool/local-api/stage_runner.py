"""Stage DAG runner for launcher specs.

A stage is a unit of shell work. Stages declare `needs:` for ordering, a
`lifecycle:` of one-shot or long-running, and `invalidatedBy:` keys that
say *what changes make this stage stale*. The runner spawns each stage in
a PTY, fans out the output to SSE subscribers, and persists a per-stage
state record (status, hash, exit code, started/finished) so the dashboard
can survive restarts.

This module deliberately re-implements PTY plumbing rather than reaching
into server.py's remote-session globals — it keeps the new subsystem
isolated and easy to delete if the spec idea is abandoned.
"""
from __future__ import annotations

import errno
import fcntl
import json
import os
import pty
import queue as _queue
import select
import shlex
import signal
import struct
import subprocess
import termios
import threading
import time
import uuid
from collections import deque
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional

from spec_loader import PROJECT_ROOT, build_ctx, template
import input_store

STATE_ROOT = PROJECT_ROOT / "data" / "launcher-state"
SESSION_RING = 40000  # max chunks buffered per stage for SSE replay (5x previous)

# Friendly aliases → nixpkgs attribute paths. Anything not in this map is
# forwarded verbatim, so specs can also use raw nixpkgs attrs (e.g. "ruby_3_3").
NIX_TOOL_MAP: Dict[str, str] = {
    "node18":    "nodejs_18",
    "node20":    "nodejs_20",
    "node22":    "nodejs_22",
    "java11":    "jdk11",
    "java17":    "jdk17",
    "java21":    "jdk21",
    "yarn":      "yarn",
    "pnpm":      "pnpm",
    "adb":       "android-tools",
    "gradle":    "gradle",
    "watchman":  "watchman",
    "python3":   "python3",
    "ruby":      "ruby",
    "git":       "git",
    "cocoapods": "cocoapods",
}


def _nix_packages(tools: Optional[List[str]]) -> List[str]:
    return [NIX_TOOL_MAP.get(t, t) for t in (tools or []) if t]


def _wrap_with_nix(cmd: str, tools: Optional[List[str]]) -> str:
    """If the spec lists `tools:`, run `cmd` inside `nix-shell -p ...` so the
    requested toolchain (correct node, JDK, adb, …) is on PATH for every stage.
    No-ops when `tools` is empty or `cmd` is blank."""
    pkgs = _nix_packages(tools)
    if not pkgs or not (cmd or "").strip():
        return cmd
    pkg_args = " ".join(shlex.quote(p) for p in pkgs)
    return f"nix-shell --quiet -p {pkg_args} --run {shlex.quote(cmd)}"


def _state_dir(slug: str) -> Path:
    d = STATE_ROOT / slug
    d.mkdir(parents=True, exist_ok=True)
    return d


def _state_path(slug: str) -> Path:
    return _state_dir(slug) / "state.json"


def _load_state(slug: str) -> Dict[str, Any]:
    p = _state_path(slug)
    if not p.is_file():
        return {"stages": {}, "ports": {}}
    try:
        return json.loads(p.read_text())
    except Exception:  # noqa: BLE001
        return {"stages": {}, "ports": {}}


def _save_state(slug: str, st: Dict[str, Any]) -> None:
    p = _state_path(slug)
    tmp = p.with_suffix(".tmp")
    tmp.write_text(json.dumps(st, indent=2, default=str))
    tmp.replace(p)


# ── In-memory live sessions ─────────────────────────────────────────────────
# A session represents one (slug, stage-id) currently or recently running.
# Long-running stages keep their session alive until explicitly stopped or
# the process exits; one-shots clean up after exit.

_sessions: Dict[str, dict] = {}      # session_id → session
_by_stage: Dict[str, str] = {}       # f"{slug}/{stage_id}" → session_id (the *current* live one)
_runner_lock = threading.Lock()


def _set_pty_size(fd: int, rows: int, cols: int) -> None:
    try:
        fcntl.ioctl(fd, termios.TIOCSWINSZ,
                    struct.pack("HHHH", max(1, rows), max(1, cols), 0, 0))
    except OSError:
        pass


def _make_session(slug: str, stage_id: str, command: str, cwd: Path,
                  env: Dict[str, str], cols: int, rows: int) -> dict:
    sid = uuid.uuid4().hex
    return {
        "id": sid,
        "slug": slug,
        "stage": stage_id,
        "command": command,
        "cwd": str(cwd),
        "running": False,
        "exit_code": None,
        "started_at": None,
        "finished_at": None,
        "buf": deque(maxlen=SESSION_RING),  # bytes ring
        "subscribers": [],  # list[queue.Queue]
        "lock": threading.Lock(),
        "proc": None,
        "master_fd": -1,
        "cols": cols,
        "rows": rows,
        "env": env,
    }


def _pty_reader(session: dict) -> None:
    fd = session["master_fd"]
    try:
        while True:
            try:
                r, _, _ = select.select([fd], [], [], 1.0)
            except (OSError, ValueError):
                break
            if fd not in r:
                if session["proc"] and session["proc"].poll() is not None:
                    break
                continue
            try:
                chunk = os.read(fd, 4096)
            except OSError as e:
                if e.errno == errno.EIO:
                    break
                if e.errno == errno.EAGAIN:
                    continue
                break
            if not chunk:
                break
            with session["lock"]:
                session["buf"].append(chunk)
                subs = list(session["subscribers"])
            for q in subs:
                try:
                    q.put_nowait(chunk)
                except Exception:  # noqa: BLE001
                    pass
    finally:
        try:
            session["proc"].wait(timeout=10)
        except Exception:  # noqa: BLE001
            pass
        with session["lock"]:
            session["running"] = False
            session["exit_code"] = session["proc"].returncode if session["proc"] else None
            session["finished_at"] = time.time()
            subs = list(session["subscribers"])
        for q in subs:
            try:
                q.put_nowait(None)
            except Exception:  # noqa: BLE001
                pass
        try:
            os.close(fd)
        except OSError:
            pass


def _spawn_session(session: dict) -> None:
    master_fd, slave_fd = pty.openpty()
    _set_pty_size(master_fd, session["rows"], session["cols"])
    full_env = {**os.environ, **session["env"]}
    proc = subprocess.Popen(
        ["bash", "-lc", session["command"]],
        cwd=session["cwd"],
        stdin=slave_fd,
        stdout=slave_fd,
        stderr=slave_fd,
        env=full_env,
        preexec_fn=os.setsid,
        close_fds=True,
    )
    os.close(slave_fd)
    session["master_fd"] = master_fd
    session["proc"] = proc
    session["running"] = True
    session["started_at"] = time.time()
    threading.Thread(target=_pty_reader, args=(session,), daemon=True).start()


def _kill_session(session: dict) -> bool:
    proc = session.get("proc")
    if not proc:
        return False
    if proc.poll() is not None:
        return False
    try:
        os.killpg(os.getpgid(proc.pid), signal.SIGTERM)
    except (OSError, ProcessLookupError):
        pass
    deadline = time.time() + 5
    while proc.poll() is None and time.time() < deadline:
        time.sleep(0.1)
    if proc.poll() is None:
        try:
            os.killpg(os.getpgid(proc.pid), signal.SIGKILL)
        except (OSError, ProcessLookupError):
            pass
    return True


# ── Invalidation hashes ─────────────────────────────────────────────────────
# A stage's fingerprint is the concatenation of resolved invalidation values:
#   - "source.ref"      → current spec source.ref
#   - "ports.X"         → resolved port number
#   - "inputs.X"        → current input value (or its file hash)
#   - "<path-or-glob>"  → mtime/size of files matching it
# When the fingerprint changes vs the persisted one, the stage is stale.

def _invalidation_fingerprint(spec: dict, stage: dict, ctx: Dict[str, Any]) -> str:
    import hashlib
    h = hashlib.sha256()
    h.update(stage["id"].encode())
    h.update(b"\x00")
    for key in stage.get("invalidatedBy", []):
        h.update(key.encode())
        h.update(b"=")
        bucket, _, name = key.partition(".")
        if bucket == "source" and name:
            h.update(str((spec.get("source") or {}).get(name, "")).encode())
        elif bucket == "ports" and name:
            h.update(str(ctx.get("ports", {}).get(name, "")).encode())
        elif bucket == "inputs" and name:
            h.update(str(ctx.get("inputs", {}).get(name, "")).encode())
        else:
            base = Path(ctx.get("destDir") or PROJECT_ROOT)
            for p in sorted(base.glob(key)):
                try:
                    st = p.stat()
                    h.update(f"{p}|{int(st.st_mtime)}|{st.st_size}".encode())
                except OSError:
                    pass
        h.update(b"\x00")
    return h.hexdigest()


# ── Public API ──────────────────────────────────────────────────────────────

def session_for(slug: str, stage_id: str) -> Optional[dict]:
    with _runner_lock:
        sid = _by_stage.get(f"{slug}/{stage_id}")
        if sid:
            return _sessions.get(sid)
        return None


def get_session(session_id: str) -> Optional[dict]:
    with _runner_lock:
        return _sessions.get(session_id)


def stage_status(slug: str, spec: dict, stage: dict) -> dict:
    """Compute the live status snapshot of one stage."""
    persisted = _load_state(slug).get("stages", {}).get(stage["id"], {})
    inputs = input_store.load(slug)
    ports = _load_state(slug).get("ports", {})
    ctx = build_ctx(spec, inputs, ports)
    current_fp = _invalidation_fingerprint(spec, stage, ctx)
    stale = persisted.get("fingerprint") != current_fp
    live = session_for(slug, stage["id"])
    state = "idle"
    if live and live.get("running"):
        state = "running"
    elif persisted.get("last_exit") == 0:
        state = "stale" if stale else "ready"
    elif persisted.get("last_exit") is not None:
        state = "failed"
    return {
        "id": stage["id"],
        "state": state,
        "lifecycle": stage.get("lifecycle", "one-shot"),
        "stale": stale,
        "lastExit": persisted.get("last_exit"),
        "startedAt": persisted.get("started_at") if not live else live.get("started_at"),
        "finishedAt": persisted.get("finished_at") if not live else live.get("finished_at"),
        "sessionId": live["id"] if live else None,
    }


def run_stage(slug: str, spec: dict, stage_id: str, force: bool = False,
              cols: int = 120, rows: int = 30) -> dict:
    """Spawn one stage. Returns the session record (or an error dict)."""
    stage = next((s for s in spec["stages"] if s["id"] == stage_id), None)
    if stage is None:
        return {"error": f"unknown stage {stage_id!r}"}

    inputs = input_store.load(slug)
    state = _load_state(slug)
    port_overrides = state.get("ports", {})
    ctx = build_ctx(spec, inputs, port_overrides)
    fp = _invalidation_fingerprint(spec, stage, ctx)

    # Cache skip for one-shots that succeeded and are not stale.
    persisted = state.get("stages", {}).get(stage_id, {})
    if (not force
            and stage.get("lifecycle", "one-shot") == "one-shot"
            and persisted.get("fingerprint") == fp
            and persisted.get("last_exit") == 0):
        return {"skipped": True, "reason": "fresh", "fingerprint": fp}

    # If already running, return existing session.
    existing = session_for(slug, stage_id)
    if existing and existing.get("running") and not force:
        return {"sessionId": existing["id"], "attached": True}
    if existing and existing.get("running") and force:
        _kill_session(existing)

    # Resolve source builtin separately.
    if stage.get("builtin") == "source":
        return _run_source_stage(slug, spec, stage, fp)

    tools = spec.get("tools") or []

    # Apply hooks.preStart once per stage launch (best effort).
    for cmd in (spec.get("hooks") or {}).get("preStart", []) or []:
        cmd = _wrap_with_nix(template(cmd, ctx), tools)
        try:
            subprocess.run(["bash", "-lc", cmd], check=False, timeout=60)
        except Exception:  # noqa: BLE001
            pass

    # Apply adb reverses if declared.
    _apply_adb_reverses(spec, ctx, tools)

    command = _wrap_with_nix(template(stage.get("run") or "", ctx), tools)
    env = {k: template(v, ctx) for k, v in (spec.get("env") or {}).items()}
    cwd = _resolve_cwd(spec, stage, ctx)
    sess = _make_session(slug, stage_id, command, cwd, env, cols, rows)
    sess["fingerprint"] = fp
    with _runner_lock:
        _sessions[sess["id"]] = sess
        _by_stage[f"{slug}/{stage_id}"] = sess["id"]
    try:
        _spawn_session(sess)
    except Exception as e:  # noqa: BLE001
        with _runner_lock:
            _sessions.pop(sess["id"], None)
            if _by_stage.get(f"{slug}/{stage_id}") == sess["id"]:
                _by_stage.pop(f"{slug}/{stage_id}", None)
        return {"error": f"spawn failed: {e}"}

    # On exit, persist the result.
    def _watch_exit(s=sess, _fp=fp):
        proc = s["proc"]
        proc.wait()
        state = _load_state(slug)
        state.setdefault("stages", {})[stage_id] = {
            "fingerprint": _fp,
            "last_exit": proc.returncode,
            "started_at": s["started_at"],
            "finished_at": time.time(),
        }
        _save_state(slug, state)

    threading.Thread(target=_watch_exit, daemon=True).start()
    return {"sessionId": sess["id"], "started": True, "fingerprint": fp}


def _resolve_cwd(spec: dict, stage: dict, ctx: Dict[str, Any]) -> Path:
    dest = ctx.get("destDir") or str(PROJECT_ROOT)
    base = Path(dest)
    if not base.exists():
        base.mkdir(parents=True, exist_ok=True)
    sub = stage.get("cwd")
    if not sub:
        return base
    sub = template(sub, ctx)
    p = (base / sub).resolve()
    p.mkdir(parents=True, exist_ok=True)
    return p


def _apply_adb_reverses(spec: dict, ctx: Dict[str, Any], tools: Optional[List[str]] = None) -> None:
    names = spec.get("adbReverse") or []
    if not names:
        return
    ports = ctx.get("ports") or {}
    for n in names:
        port = ports.get(n)
        if not port:
            continue
        inner = f"adb reverse tcp:{port} tcp:{port}"
        wrapped = _wrap_with_nix(inner, tools)
        try:
            subprocess.run(
                ["bash", "-lc", wrapped],
                check=False, timeout=30,
                stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
            )
        except Exception:  # noqa: BLE001
            pass


def _run_source_stage(slug: str, spec: dict, stage: dict, fp: str) -> dict:
    """Built-in source-sync: clone or fast-forward; no-op when destDir is a symlink."""
    src = spec.get("source") or {}
    url = src.get("url")
    ref = src.get("ref") or "main"
    dest_rel = src.get("destDir")
    if not (url and dest_rel):
        return {"skipped": True, "reason": "no source"}
    dest = PROJECT_ROOT / dest_rel
    dest.parent.mkdir(parents=True, exist_ok=True)
    log_lines: List[str] = []

    def _run(cmd: List[str], cwd: Optional[Path] = None) -> int:
        try:
            cp = subprocess.run(cmd, cwd=str(cwd) if cwd else None,
                                capture_output=True, text=True, timeout=300)
            if cp.stdout:
                log_lines.append(cp.stdout)
            if cp.stderr:
                log_lines.append(cp.stderr)
            return cp.returncode
        except Exception as e:  # noqa: BLE001
            log_lines.append(f"ERROR: {e}")
            return 1

    if dest.is_symlink():
        log_lines.append(f"destDir {dest} is a symlink → local-path mode, skipping clone")
        rc = 0
    elif not dest.exists():
        rc = _run(["git", "clone", url, str(dest)])
        if rc == 0:
            rc = _run(["git", "checkout", ref], cwd=dest)
    else:
        rc = _run(["git", "fetch", "origin"], cwd=dest)
        if rc == 0:
            rc = _run(["git", "checkout", ref], cwd=dest)
        if rc == 0:
            _run(["git", "pull", "--ff-only"], cwd=dest)

    state = _load_state(slug)
    state.setdefault("stages", {})[stage["id"]] = {
        "fingerprint": fp,
        "last_exit": rc,
        "started_at": time.time(),
        "finished_at": time.time(),
        "log": "\n".join(log_lines)[-4000:],
    }
    _save_state(slug, state)
    return {"builtin": True, "exitCode": rc, "log": "\n".join(log_lines)[-4000:]}


def stop_stage(slug: str, stage_id: str) -> bool:
    sess = session_for(slug, stage_id)
    if not sess:
        return False
    return _kill_session(sess)


def list_sessions(slug: Optional[str] = None) -> List[dict]:
    with _runner_lock:
        out = []
        for s in _sessions.values():
            if slug and s["slug"] != slug:
                continue
            out.append({
                "id": s["id"],
                "slug": s["slug"],
                "stage": s["stage"],
                "running": s.get("running"),
                "exitCode": s.get("exit_code"),
                "startedAt": s.get("started_at"),
                "finishedAt": s.get("finished_at"),
            })
        return out


# ── Source override (local path) ────────────────────────────────────────────

def set_local_path(slug: str, spec: dict, local_path: Optional[str]) -> dict:
    """Repoint the launcher's destDir at a user-chosen local path via symlink,
    or revert to the default cloned path when local_path is None/empty."""
    src = spec.get("source") or {}
    dest_rel = src.get("destDir")
    if not dest_rel:
        return {"error": "spec has no source.destDir"}
    dest = PROJECT_ROOT / dest_rel
    dest.parent.mkdir(parents=True, exist_ok=True)
    if dest.is_symlink() or dest.exists():
        if dest.is_symlink():
            dest.unlink()
        elif dest.is_dir() and not local_path:
            return {"error": f"refusing to remove existing dir {dest}; move it aside manually"}
    if local_path:
        target = Path(local_path).expanduser().resolve()
        if not target.exists():
            return {"error": f"local path does not exist: {target}"}
        os.symlink(str(target), str(dest))
        return {"linked": str(target)}
    return {"reset": True}
