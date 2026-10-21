"""Whole-workspace flows: spawn (bring one up), result (what is up) and stop.

The devbox workflow runs one step per SCC stage (pick → sync → stack → data →
control centre → result) so GitHub reports each stage live; `spawn` runs the
same steps in one go for use by hand, printing DEVBOX_STAGE lines (output.py)
and ending with DEVBOX_RESULT.
"""
from __future__ import annotations

import os
import shlex
import time

from . import cc, data, deploy, fleet, output, ssh, stack, workspace

CC_READY_TIMEOUT = 900   # npm install on a fresh workspace can take a while


def _has_frontdesk(host: str) -> bool:
    """Workspace URLs go through the devbox front desk; without it nothing answers."""
    return ssh.tcp_open(host, stack.FRONTDESK_PORT)


def pick_target(user: str | None, host: str | None,
                key: str | None) -> tuple[ssh.Target, str, str]:
    """The devbox to use: the given host, else the least-loaded reachable one
    that has a front desk. Returns (target, devbox name, url host): the target
    is how we reach it (the overlay address when it answers), the url host is
    its office address, which testers' phones can open."""
    if host:
        if not _has_frontdesk(host):
            raise RuntimeError(f"{host} has no front desk on :{stack.FRONTDESK_PORT} "
                               f"(Caddy + devbox-frontdesk-sync), so workspace URLs cannot answer")
        return ssh.Target(user or "", host, 22, key or ssh.find_key()), host, host
    base_host, base_port = fleet.base_api()
    res = fleet.fetch_machines(base_host, base_port)
    if res.get("error"):
        raise RuntimeError(res["error"])
    boxes = [m for m in fleet.devboxes(res.get("machines", []))
             if m.get("bestIp") and _has_frontdesk(m["bestIp"])]
    m = fleet.pick(boxes)
    if m is None:
        raise RuntimeError(f"no reachable dev-box with a front desk on :{stack.FRONTDESK_PORT}")
    return (ssh.Target(m.get("user") or "", m["bestIp"], 22, key or ssh.find_key()),
            m.get("name", m["bestIp"]), m.get("localIp") or m["bestIp"])


def _cc_url(c: dict, url_host: str) -> str:
    return f"http://{url_host}:{c['port']}" if c.get("state") == "running" and c.get("port") else ""


def result_fields(t: ssh.Target, name: str, devbox: str = "", url_host: str = "") -> dict:
    """The DEVBOX_RESULT fields, read from the workspace as it is now: its URLs,
    the control centre's URL, the data it holds and the build cache it used.
    Needs nothing from earlier steps, so a workflow can call it last. Raises
    RuntimeError when the devbox cannot be reached or the workspace is not there.

    "host" is how we reach the devbox (Stop uses it again); the URLs use
    url_host (default: the same), the address testers open them on."""
    if name not in (workspace.read_registry(t).get("users") or {}):
        raise RuntimeError(f"no workspace {name!r} on {t.host}")
    url_host = url_host or t.host
    wdir = workspace.workspace_dir(name)
    ws = f"http://{t.host}:{stack.FRONTDESK_PORT}/{name}"
    held = (data.ctx(ws, "/api/config-sync/status", timeout=5).get("last_synced") or {})
    c = cc.status(t, wdir)
    commit = ssh.run(t, f"cat {shlex.quote(wdir)}/Backend/.ci-cache-sha 2>/dev/null").stdout.strip()
    rider, driver = stack.ready_urls(url_host, name)
    return {
        "devbox": devbox or t.host, "host": t.host, "user": t.user, "workspace": name,
        "backend_url": f"http://{url_host}:{stack.FRONTDESK_PORT}/{name}",
        "rider_url": rider, "driver_url": driver,
        "cc_url": _cc_url(c, url_host),
        "data": f"{held.get('from')} {held.get('version')}" if held.get("from") else "",
        "cache_commit": commit,
    }


def spawn(name: str, src: str, owner: dict | None, *, host: str | None = None,
          user: str | None = None, key: str | None = None, source: str | None = None,
          version: str | None = None, cc_info: dict | None = None,
          ready_timeout: int = 3600) -> dict:
    """pick → register → sync → stack start → wait ready → data → control centre.

    Stops at the first failing step (its stage is printed as failed and the
    error is raised); the workspace is left as it is for a look, and `stop`
    cleans it up. Returns the DEVBOX_RESULT fields."""
    with output.Stage("preparing"):
        t, devbox, url_host = pick_target(user, host, key)
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
        stack.wait_ready(t, name, wdir, timeout=ready_timeout, on_stage=on_stage)
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
            cc_url = _cc_url(cc.wait_running(t, wdir, timeout=CC_READY_TIMEOUT), url_host)

    rider, driver = stack.ready_urls(url_host, name)
    fields = {
        "devbox": devbox, "host": t.host, "user": t.user, "workspace": name,
        "backend_url": f"http://{url_host}:{stack.FRONTDESK_PORT}/{name}",
        "rider_url": rider, "driver_url": driver,
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
