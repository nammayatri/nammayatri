"""Subprocess helpers. Three shapes:

- ``run(cmd, ...)`` — fire and wait, raise on non-zero (the bash ``set -e`` analog).
- ``run_ok(cmd, ...)`` — fire and wait, return the exit code, never raise.
- ``capture(cmd, ...)`` — run quietly, return ``(rc, stdout, stderr)``.

Stdout/stderr stream to our process by default so ``log.info`` lines and
child output interleave correctly for the dashboard's per-app log buffer.
"""
from __future__ import annotations

import os
import subprocess
from pathlib import Path
from typing import Mapping, Sequence


def _decode_cmd(cmd: Sequence[str] | str) -> list[str] | str:
    return cmd if isinstance(cmd, str) else list(cmd)


def run(
    cmd: Sequence[str] | str,
    *,
    cwd: str | Path | None = None,
    env: Mapping[str, str] | None = None,
    shell: bool = False,
    check: bool = True,
) -> int:
    """Streams stdout/stderr to our own; raises CalledProcessError on
    non-zero unless ``check=False``."""
    rc = subprocess.run(
        _decode_cmd(cmd),
        cwd=str(cwd) if cwd else None,
        env={**os.environ, **dict(env)} if env else None,
        shell=shell,
        check=False,
    ).returncode
    if check and rc != 0:
        raise subprocess.CalledProcessError(rc, cmd)
    return rc


def run_ok(
    cmd: Sequence[str] | str,
    *,
    cwd: str | Path | None = None,
    env: Mapping[str, str] | None = None,
    shell: bool = False,
) -> int:
    return run(cmd, cwd=cwd, env=env, shell=shell, check=False)


def capture(
    cmd: Sequence[str] | str,
    *,
    cwd: str | Path | None = None,
    env: Mapping[str, str] | None = None,
    shell: bool = False,
    timeout: float | None = None,
) -> tuple[int, str, str]:
    """Run the command, return ``(rc, stdout, stderr)``. Both streams
    captured as text; nothing is printed by us."""
    p = subprocess.run(
        _decode_cmd(cmd),
        cwd=str(cwd) if cwd else None,
        env={**os.environ, **dict(env)} if env else None,
        shell=shell,
        capture_output=True,
        text=True,
        timeout=timeout,
        check=False,
    )
    return p.returncode, p.stdout, p.stderr
