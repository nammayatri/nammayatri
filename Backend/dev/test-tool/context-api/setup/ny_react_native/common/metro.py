"""Metro bundler lifecycle.

We launch Metro as a sibling process (not via ``yarn start``) so we keep
direct ownership of the PID — the dashboard's stop endpoint walks the
process tree to kill it cleanly. ``CI=true`` + ``</dev/null`` removes
RN CLI's keypress-on-stdin handler that otherwise exits the process the
moment our caller's stdin hits EOF.

``--reset-cache`` is on by default so Metro re-transforms with the
just-written ``.env`` values; otherwise react-native-config inlines a
stale BASE_URL from a cached bundle from a previous launch."""
from __future__ import annotations

import os
import socket
import subprocess
import time
from pathlib import Path

from . import log, sh


def find_free_port(start: int = 8081, hops: int = 50) -> int:
    p = start
    for _ in range(hops):
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
            try:
                s.bind(("127.0.0.1", p))
            except OSError:
                p += 1
                continue
            return p
    return start


def metro_log_path(app: str) -> Path:
    return Path(f"/tmp/ny-rn-metro-{app}.log")


def start(app: str, port: int, app_dir: Path) -> int:
    """Spawn Metro for ``app`` on ``port``. Returns the child PID."""
    logp = metro_log_path(app)
    logp.parent.mkdir(parents=True, exist_ok=True)
    devnull = open(os.devnull, "rb")
    out = open(logp, "wb")
    env = {**os.environ, "CI": "true"}
    p = subprocess.Popen(
        ["npx", "react-native", "start", "--port", str(port), "--reset-cache"],
        cwd=str(app_dir),
        env=env,
        stdin=devnull,
        stdout=out,
        stderr=subprocess.STDOUT,
        start_new_session=True,
    )
    log.info(
        f"started Metro for {app} (pid {p.pid}, port {port}, log {logp})"
    )
    return p.pid


def wait_ready(port: int, timeout_s: int = 60) -> bool:
    """Poll Metro's status endpoint until it answers, or timeout."""
    log.info(f"waiting for Metro on :{port} to be ready…")
    deadline = time.time() + timeout_s
    while time.time() < deadline:
        rc, _, _ = sh.capture(
            ["curl", "-fsS", "-o", "/dev/null",
             f"http://127.0.0.1:{port}/status"],
            timeout=2,
        )
        if rc == 0:
            log.info(f"Metro :{port} ready")
            return True
        time.sleep(1)
    log.warn(f"Metro :{port} did not respond within {timeout_s}s")
    return False


def tail_log(app: str, lines: int = 40) -> None:
    logp = metro_log_path(app)
    if not logp.is_file():
        return
    print(f"── tail of {logp} ──")
    rc, out, _ = sh.capture(["tail", "-n", str(lines), str(logp)])
    for line in out.splitlines():
        print(f"  metro({app}): {line}")
