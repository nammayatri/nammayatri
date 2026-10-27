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


def _not_public(devboxes: str) -> str:
    return (f"public front desk (:{stack.PUBLIC_PORT}) not live in hopper on {devboxes}, so "
            f"testers' phones cannot reach it — is hopper-agent running there, and was "
            f"nix-the-builder's front desk setup re-run?")


def _hopper_name(t: ssh.Target) -> str:
    """The devbox's hostname — the name hopper (and the fleet) know it by."""
    return ssh.run(t, "hostname").stdout.strip()


def _login(m: dict, key: str | None, skip: str = "") -> ssh.Target:
    """Log in on the first of the machine's routes that takes our key (office,
    then tailnet, then VPN), leaving out `skip`; raise naming every address tried."""
    tried = []
    for r in m.get("routes") or []:
        if r["ip"] == skip:
            continue
        t = ssh.Target(m.get("user") or "", r["ip"], 22, key or ssh.find_key())
        err = ssh.login_error(t)
        if not err:
            return t
        tried.append(f"{r['route']} {r['ip']}: {err}")
    raise RuntimeError(f"cannot log in to {m.get('name')}: " + ("; ".join(tried) or "no route"))


def pick_target(user: str | None, host: str | None,
                key: str | None) -> tuple[ssh.Target, str, str]:
    """The devbox to use: the given host, else the least-loaded reachable one
    that has a front desk and is published to hopper. Returns (target, devbox
    name, url host): the target is the first address that takes our key (office
    first, then the overlay), the devbox name is the one hopper's gateway URLs
    use, the url host is its office address, for the office-only URLs."""
    if host:
        if not _has_frontdesk(host):
            raise RuntimeError(f"{host} has no front desk on :{stack.FRONTDESK_PORT} "
                               f"(Caddy + devbox-frontdesk-sync), so workspace URLs cannot answer")
        t = ssh.Target(user or "", host, 22, key or ssh.find_key())
        devbox = _hopper_name(t) or host
        if not stack.is_public(devbox):
            raise RuntimeError(_not_public(devbox))
        return t, devbox, host
    base_host, base_port = fleet.base_api()
    res = fleet.fetch_machines(base_host, base_port)
    if res.get("error"):
        raise RuntimeError(res["error"])
    boxes = [m for m in fleet.devboxes(res.get("machines", []))
             if m.get("bestIp") and _has_frontdesk(m["bestIp"])]
    if not boxes:
        raise RuntimeError(f"no reachable dev-box with a front desk on :{stack.FRONTDESK_PORT}")
    public = [m for m in boxes if stack.is_public(m.get("name", ""))]
    m = fleet.pick(public)
    if m is None:
        raise RuntimeError(_not_public(", ".join(b.get("name", b["bestIp"]) for b in boxes)))
    return _login(m, key), m["name"], m.get("localIp") or m["bestIp"]


def _cc_url(c: dict, url_host: str, public: str) -> str:
    """Through hopper's gateway when the control centre runs under its public
    path (cc.py CC_BASE); else its office address, as before."""
    if c.get("state") != "running" or not c.get("port"):
        return ""
    if c.get("base"):
        return f"{public}/control-centre/"
    return f"http://{url_host}:{c['port']}"


def result_fields(t: ssh.Target, name: str, devbox: str = "", url_host: str = "") -> dict:
    """The DEVBOX_RESULT fields, read from the workspace as it is now: its URLs,
    the control centre's URL, the data it holds and the build cache it used.
    Needs nothing from earlier steps, so a workflow can call it last. Raises
    RuntimeError when the devbox cannot be reached or the workspace is not there.

    "host" is how we reach the devbox (Stop uses it again). backend_url and the
    rider/driver URLs go through hopper's gateway, so testers open them on any
    network; office_url and cc_url use url_host (default: the same as host)."""
    if name not in (workspace.read_registry(t).get("users") or {}):
        raise RuntimeError(f"no workspace {name!r} on {t.host}")
    url_host = url_host or t.host
    devbox = devbox or _hopper_name(t) or t.host
    wdir = workspace.workspace_dir(name)
    held = (data.ctx(stack.office_base(t.host, name), "/api/config-sync/status",
                     timeout=5).get("last_synced") or {})
    c = cc.status(t, wdir)
    commit = ssh.run(t, f"cat {shlex.quote(wdir)}/Backend/.ci-cache-sha 2>/dev/null").stdout.strip()
    public = stack.public_base(devbox, name)
    rider, driver = stack.ready_urls(public)
    return {
        "devbox": devbox, "host": t.host, "user": t.user, "workspace": name,
        "backend_url": public,
        "rider_url": rider, "driver_url": driver,
        "office_url": stack.office_base(url_host, name),
        "cc_url": _cc_url(c, url_host, public),
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
            data.sync_and_wait(stack.office_base(t.host, name), source, version)
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
            cc_url = _cc_url(cc.wait_running(t, wdir, timeout=CC_READY_TIMEOUT), url_host,
                             stack.public_base(devbox, name))

    public = stack.public_base(devbox, name)
    rider, driver = stack.ready_urls(public)
    fields = {
        "devbox": devbox, "host": t.host, "user": t.user, "workspace": name,
        "backend_url": public,
        "rider_url": rider, "driver_url": driver,
        "office_url": stack.office_base(url_host, name),
        "cc_url": cc_url, "data": data_label, "cache_commit": synced.get("cacheCommit", ""),
    }
    output.stage("ready", "done")
    output.result(fields)
    return fields


def reach(t: ssh.Target) -> ssh.Target:
    """t when its address takes our key; else the same dev-box on another of its
    addresses (office first), found in service discovery by any of them."""
    err = ssh.login_error(t)
    if not err:
        return t
    res = fleet.fetch_machines(*fleet.base_api())
    m = next((m for m in fleet.devboxes(res.get("machines", []))
              if t.host in (m.get("localIp"), m.get("overlayIp"), m.get("awsIp"))), None)
    if m is None:
        raise RuntimeError(f"cannot log in to {t.host} ({err}) and service discovery "
                           f"knows no other address for it")
    return _login({**m, "user": t.user or m.get("user")}, t.identity, skip=t.host)


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
