"""Log source multiplexer for launcher specs.

Spec entries: `logs: [{name, kind: stage|file|cmd|http, ...}]`. This module
turns each kind into a stream of bytes that can be fanned out over SSE,
reusing the same subscriber/queue pattern as stage_runner.
"""
from __future__ import annotations

import os
import select
import shlex
import subprocess
import threading
import time
import urllib.error
import urllib.request
from pathlib import Path
from typing import Dict, List, Optional

import stage_runner
from spec_loader import PROJECT_ROOT, build_ctx, template
import input_store


def _resolve_log_def(spec: dict, name: str) -> Optional[dict]:
    for l in spec.get("logs", []):
        if l["name"] == name:
            return l
    return None


def _ctx_for(slug: str, spec: dict) -> dict:
    state = stage_runner._load_state(slug)
    return build_ctx(spec, input_store.load(slug), state.get("ports", {}))


def open_stream(slug: str, spec: dict, name: str) -> Optional[dict]:
    """Returns a dict {bytes_iter, closer} for the named log source, or None."""
    ldef = _resolve_log_def(spec, name)
    if ldef is None:
        return None
    kind = ldef["kind"]
    if kind == "stage":
        return _stage_stream(slug, ldef)
    if kind == "file":
        ctx = _ctx_for(slug, spec)
        return _file_stream(template(ldef.get("path") or "", ctx),
                            follow=bool(ldef.get("follow", True)))
    if kind == "cmd":
        ctx = _ctx_for(slug, spec)
        return _cmd_stream(template(ldef.get("cmd") or "", ctx))
    if kind == "http":
        ctx = _ctx_for(slug, spec)
        return _http_stream(template(ldef.get("url") or "", ctx))
    return None


def _stage_stream(slug: str, ldef: dict) -> dict:
    stage_id = ldef.get("stage")
    sess = stage_runner.session_for(slug, stage_id) if stage_id else None

    import queue
    q: "queue.Queue" = queue.Queue()

    def closer():
        if sess:
            with sess["lock"]:
                try:
                    sess["subscribers"].remove(q)
                except ValueError:
                    pass

    def gen():
        if not sess:
            yield f"[no live session for stage {stage_id!r}]\n".encode()
            return
        with sess["lock"]:
            for chunk in list(sess["buf"]):
                yield chunk
            sess["subscribers"].append(q)
        try:
            while True:
                chunk = q.get()
                if chunk is None:
                    return
                yield chunk
        finally:
            closer()

    return {"iter": gen(), "close": closer}


def _file_stream(path_str: str, follow: bool) -> dict:
    path = Path(path_str)
    stopped = {"v": False}

    def closer():
        stopped["v"] = True

    def gen():
        # Wait briefly for the file to appear (some logs are written by stages).
        wait_until = time.time() + 5
        while not path.exists() and time.time() < wait_until and not stopped["v"]:
            time.sleep(0.5)
        if not path.exists():
            yield f"[file not found: {path}]\n".encode()
            return
        try:
            with path.open("rb") as f:
                # Replay tail.
                try:
                    f.seek(-8000, os.SEEK_END)
                    f.read(1)
                except OSError:
                    f.seek(0)
                else:
                    f.readline()
                while not stopped["v"]:
                    chunk = f.read(4096)
                    if chunk:
                        yield chunk
                    elif follow:
                        time.sleep(0.5)
                    else:
                        return
        except OSError as e:
            yield f"[read error: {e}]\n".encode()

    return {"iter": gen(), "close": closer}


def _cmd_stream(cmd: str) -> dict:
    proc = subprocess.Popen(
        ["bash", "-lc", cmd],
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        bufsize=0,
        start_new_session=True,
    )
    stopped = {"v": False}

    def closer():
        stopped["v"] = True
        if proc.poll() is None:
            try:
                os.killpg(os.getpgid(proc.pid), 15)
            except OSError:
                pass

    def gen():
        try:
            while True:
                if stopped["v"]:
                    return
                chunk = proc.stdout.read(4096)
                if not chunk:
                    if proc.poll() is not None:
                        return
                    time.sleep(0.2)
                    continue
                yield chunk
        finally:
            closer()

    return {"iter": gen(), "close": closer}


def _http_stream(url: str) -> dict:
    """One-shot HTTP GET, body streamed line-by-line. Not a long-poll."""
    stopped = {"v": False}

    def closer():
        stopped["v"] = True

    def gen():
        try:
            with urllib.request.urlopen(url, timeout=10) as r:
                while True:
                    if stopped["v"]:
                        return
                    chunk = r.read(4096)
                    if not chunk:
                        return
                    yield chunk
        except (urllib.error.URLError, OSError) as e:
            yield f"[http error: {e}]\n".encode()

    return {"iter": gen(), "close": closer}
