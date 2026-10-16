"""Whole-workspace flows: spawn (bring one up) and stop (remove it).

These are what the devbox workflow runs. Each step prints DEVBOX_STAGE lines
(output.py) under the stage names SCC shows, and spawn ends with DEVBOX_RESULT.
"""
from __future__ import annotations

import os
import shlex
import time

from . import cc, data, deploy, fleet, output, ssh, stack, workspace

CC_READY_TIMEOUT = 900   # npm install on a fresh workspace can take a while


def _pick_target(user: str | None, host: str | None, key: str | None) -> tuple[ssh.Target, str]:
    """The devbox to use: the given host, else the least-loaded reachable one."""
    if host:
        return ssh.Target(user or "", host, 22, key or ssh.find_key()), host
    base_host, base_port = fleet.base_api()
    res = fleet.fetch_machines(base_host, base_port)
    if res.get("error"):
        raise RuntimeError(res["error"])
    m = fleet.pick(fleet.devboxes(res.get("machines", [])))
    if m is None:
        raise RuntimeError("no reachable dev-box")
    return ssh.Target(m.get("user") or "", m["bestIp"], 22, key or ssh.find_key()), m.get("name", m["bestIp"])


def spawn(name: str, src: str, owner: dict | None, *, host: str | None = None,
          user: str | None = None, key: str | None = None, source: str | None = None,
          version: str | None = None, cc_info: dict | None = None,
          ready_timeout: int = 3600) -> dict:
    """pick → register → sync → stack start → wait ready → data → control centre.

    Stops at the first failing step (its stage is printed as failed and the
    error is raised); the workspace is left as it is for a look, and `stop`
    cleans it up. Returns the DEVBOX_RESULT fields."""
    with output.Stage("preparing"):
        t, devbox = _pick_target(user, host, key)
        workspace.register(t, name, owner)
    wdir = workspace.workspace_dir(name)

    with output.Stage("sync"):
        synced = deploy.sync(src, t, name, owner)

    # Build and Start are one stack run (cache-restore → cabal-build → services):
    # "build" ends and "start" begins when the services start.
    phase = {"now": "build", "t0": time.time()}
    ms = lambda: int((time.time() - phase["t0"]) * 1000)  # noqa: E731
    output.stage("build", "started")

    def on_stage(s: str) -> None:
        if s == "services" and phase["now"] == "build":
            output.stage("build", "done", ms=ms())
            phase.update(now="start", t0=time.time())
            output.stage("start", "started")

    try:
        stack.start(t, wdir)
        ready = stack.wait_ready(t, name, wdir, timeout=ready_timeout, on_stage=on_stage)
    except Exception as e:
        output.stage(phase["now"], "failed", ms=ms(), error=str(e).splitlines()[0] if str(e) else type(e).__name__)
        raise
    on_stage("services")             # in case ready came before "services" was seen
    output.stage("start", "done", ms=ms())

    data_label = ""
    with output.Stage("data") as st:
        if not source:
            st.skip("no data source chosen")
        else:
            if not version:
                versions, err = data.published_versions(t, source)
                if err:
                    raise RuntimeError(err)
                pick = data.pick_version(versions)
                if not pick:
                    raise RuntimeError(f"no usable {source} version published")
                version = f"v{pick['version']}"
            ws = f"http://{t.host}:{stack.FRONTDESK_PORT}/{name}"
            data.sync_and_wait(ws, source, version)
            data_label = f"{source} {version}"

    cc_url = ""
    with output.Stage("cc") as st:
        p = stack.ports(t, wdir)
        r = cc.start(t, wdir, p.get("rider-app"), p.get("dynamic-offer-driver-app"), cc_info)
        if r.get("error") == "no control-center/ in the workspace":
            st.skip("no control-center/ in the source")
        elif r.get("error"):
            raise RuntimeError(r["error"])
        else:
            deadline = time.time() + CC_READY_TIMEOUT
            while r.get("state") != "running":
                if r.get("state") in ("exited", "stopped") or time.time() > deadline:
                    tail = "\n".join((r.get("log") or [])[-15:])
                    raise RuntimeError(f"control centre {r.get('state')}\n{tail}")
                time.sleep(10)
                r = cc.status(t, wdir)
            cc_url = r.get("url", "")

    fields = {
        "devbox": devbox, "host": t.host, "user": t.user, "workspace": name,
        "backend_url": f"http://{t.host}:{stack.FRONTDESK_PORT}/{name}",
        "rider_url": ready["urls"][0], "driver_url": ready["urls"][1],
        "cc_url": cc_url, "data": data_label, "cache_commit": synced.get("cacheCommit", ""),
    }
    output.stage("ready", "done")
    output.result(fields)
    return fields


def stop(t: ssh.Target, name: str, owner: dict | None) -> dict:
    """Remove a whole workspace: control centre → stack → folder → registry entry.

    The owner is checked first, before anything is stopped. A workspace that
    is already gone is not an error."""
    entry = (workspace.read_registry(t).get("users") or {}).get(name)
    wdir = workspace.workspace_dir(name)
    folder = os.path.dirname(wdir)            # /tmp/<name>
    if entry is not None and not workspace.same_owner(entry, owner):
        raise workspace.OwnerMismatch(
            f"{name} belongs to {entry.get('owner') or 'no owner (made by hand)'}")
    if entry is None:
        left = ssh.run(t, f"test -e {shlex.quote(folder)} && echo yes || echo no").stdout.strip()
        if left == "yes":
            raise RuntimeError(f"{folder} exists but {name} is not registered — "
                               f"not removing a folder whose owner is unknown")
        return {"stopped": False, "note": "already gone"}

    r = cc.stop(t, wdir)
    if r.get("error"):
        raise RuntimeError(r["error"])
    s = stack.stop(t, wdir)
    rm = ssh.run(t, f"rm -rf {shlex.quote(folder)}", timeout=300)
    if rm.returncode != 0:
        raise RuntimeError(f"could not remove {folder}: {(rm.stderr or '').strip()}")
    workspace.release(t, name, owner)
    return {"stopped": True, "stackWasRunning": s.get("wasRunning"),
            "killedLeftovers": s.get("killedLeftovers"), "removed": folder}
