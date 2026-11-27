"""Machine-readable progress lines for whoever runs devbox_core (the devbox
workflow; SCC reads them back from the GitHub Actions log).

  DEVBOX_STAGE  {"stage": "sync", "state": "started"}
  DEVBOX_STAGE  {"stage": "sync", "state": "done", "ms": 82000}
  DEVBOX_STAGE  {"stage": "data", "state": "failed", "ms": 900, "error": "…"}
  DEVBOX_RESULT {"devbox": …, "workspace": …, "backend_url": …, "cc_url": …}

Stage names match what SCC shows: preparing, sync, build, start, data, cc, ready.
"""
from __future__ import annotations

import json
import time

STAGES = ("preparing", "sync", "build", "start", "data", "cc", "ready")


def stage(name: str, state: str, **extra) -> None:
    print("DEVBOX_STAGE " + json.dumps({"stage": name, "state": state, **extra}), flush=True)


def result(fields: dict) -> None:
    print("DEVBOX_RESULT " + json.dumps(fields), flush=True)


def _escape(text: str, prop: bool = False) -> str:
    """GitHub workflow-command escaping: % CR LF always, : and , in properties."""
    text = text.replace("%", "%25").replace("\r", "%0D").replace("\n", "%0A")
    if prop:
        text = text.replace(":", "%3A").replace(",", "%2C")
    return text


def annotate(title: str, msg: str) -> None:
    """A GitHub Actions error annotation (shown at the top of the run, with the
    whole reason) plus the full message in the log. The first line is what SCC's
    poller reads back for the release page."""
    lines = msg.strip().splitlines() or [title]
    print(f"::error title={_escape(title, prop=True)}::{_escape(chr(10).join(lines[:15]))}", flush=True)
    print(msg, flush=True)


class Stage:
    """with Stage("sync"): …  → prints started, then done or failed (with ms)."""

    def __init__(self, name: str):
        self.name = name
        self.t0 = 0.0
        self.closed = False

    def __enter__(self) -> "Stage":
        self.t0 = time.time()
        stage(self.name, "started")
        return self

    def _ms(self) -> int:
        return int((time.time() - self.t0) * 1000)

    def skip(self, why: str) -> None:
        stage(self.name, "skipped", ms=self._ms(), detail=why)
        self.closed = True

    def __exit__(self, exc_type, exc, tb) -> bool:
        if self.closed:
            return False
        if exc is None:
            stage(self.name, "done", ms=self._ms())
        else:
            stage(self.name, "failed", ms=self._ms(), error=str(exc).splitlines()[0] if str(exc) else exc_type.__name__)
        return False
