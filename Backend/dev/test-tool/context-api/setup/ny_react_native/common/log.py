"""Tiny log helpers — single-line stdout prints with the
``ny-react-native:`` prefix the dashboard scrapes for the readiness
sentinel. All log lines are unbuffered so test-context-api can stream
them into the per-app log buffer line-by-line."""
from __future__ import annotations

import sys

PREFIX = "ny-react-native:"


def info(msg: str) -> None:
    print(f"{PREFIX} {msg}", flush=True)


def warn(msg: str) -> None:
    print(f"{PREFIX} WARN {msg}", flush=True)


def err(msg: str) -> None:
    print(f"{PREFIX} ERROR {msg}", file=sys.stderr, flush=True)


def section(title: str) -> None:
    """A visually-separated divider used between phases."""
    print("", flush=True)
    print(f"{PREFIX} ── {title} ──", flush=True)
