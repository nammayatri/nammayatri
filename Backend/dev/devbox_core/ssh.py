"""SSH helpers: build the ssh command, run a script on a devbox, check a port.

Moved from test-tool/local-api/server.py so the devbox workflow can use them too.
"""
from __future__ import annotations

import os
import shlex
import socket
import subprocess
from dataclasses import dataclass

# Tried in order when no key is given. Paths keep "~" so callers can expand them.
KEY_CANDIDATES = ["~/.ssh/id_ed25519", "~/.ssh/id_rsa", "~/.ssh/id_ecdsa"]


@dataclass(frozen=True)
class Target:
    """Where to SSH: user@host:port, optionally with a specific private key."""
    user: str
    host: str
    port: int = 22
    identity: str | None = None


def find_key() -> str | None:
    """Path of the private key to use, or None.

    DEVBOX_SSH_KEY_FILE wins when set (the devbox workflow writes its key there);
    otherwise the first existing key from KEY_CANDIDATES."""
    from_env = os.environ.get("DEVBOX_SSH_KEY_FILE", "").strip()
    if from_env and os.path.isfile(from_env):
        return from_env
    for k in KEY_CANDIDATES:
        path = os.path.expanduser(k)
        if os.path.isfile(path):
            return path
    return None


def argv(t: Target, tty: bool = False) -> list:
    """The ssh command line for t, ready to append a remote command to."""
    cmd = ["ssh"]
    if tty:
        cmd += ["-tt"]
    cmd += [
        "-o", "StrictHostKeyChecking=accept-new",
        "-o", "ServerAliveInterval=15",
        "-o", "GSSAPIAuthentication=no",
        "-o", "ConnectTimeout=10",
        "-o", "BatchMode=yes",
        "-p", str(t.port),
    ]
    if t.identity:
        cmd += ["-i", t.identity]
    cmd += [f"{t.user}@{t.host}" if t.user else t.host]
    return cmd


def run(t: Target, script: str, timeout: int = 20) -> subprocess.CompletedProcess:
    """Run a bash script on t; returns the finished process (text stdout/stderr)."""
    return subprocess.run(argv(t) + [f"bash -c {shlex.quote(script)}"],
                          capture_output=True, text=True, timeout=timeout)


def tcp_open(host: str, port: int = 22, timeout: float = 3.0) -> bool:
    """True when something is listening on host:port.

    Only the network path is checked, not key auth: a devbox whose key is not
    authorized yet is still reachable."""
    if not host:
        return False
    try:
        with socket.create_connection((host, port), timeout=timeout):
            return True
    except OSError:
        return False
