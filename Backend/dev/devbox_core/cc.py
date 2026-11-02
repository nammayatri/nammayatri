"""Control centre on the devbox, as part of a workspace's stack.

The workspace's control-center/ (rsynced with the backend: it sits inside the
repo dir, gitignored) runs as the stack's control-centre process
(Backend/nix/services/nammayatri.nix): a Vite dev server on the workspace's
resolved control-centre port, whose /api/bap|bpp proxy reaches the workspace's
rider-app / driver-app as localhost. The workspace's caddy serves it at
/control-centre/, so testers open it through hopper's gateway on any network:
<gateway>/api/<devbox>/<PUBLIC_PORT>/<workspace>/control-centre/. It starts and
stops with the stack, so there is nothing to start or stop here; this module
only reports on it.

Workspaces started before that ran it detached next to the stack, with its
pid/port in <workspace>/data/control-center.json; stop() still cleans one up.
"""
from __future__ import annotations

import json
import shlex
import time

from . import ssh

STATE = "data/control-center.json"   # the old detached control centre only

# Runs on the devbox: prints this workspace's control-centre state as JSON.
STATUS_PY = r'''
import json, os, sys, urllib.request, urllib.error
d = sys.argv[1]
out = {"state": "stopped", "checkoutPresent": os.path.isdir(os.path.join(d, "control-center"))}
try:
    port = (json.load(open(os.path.join(d, "data/ports.json"))).get("ports") or {}).get("control-centre")
except Exception:
    port = None
try:
    prefix = open(os.path.join(d, "data/cc-prefix")).read().strip()
except Exception:
    prefix = ""
if port and out["checkoutPresent"]:
    out["port"] = int(port)
    out["base"] = prefix + "/control-centre/" if prefix else ""
    ready = False
    try:
        urllib.request.urlopen("http://127.0.0.1:%s%s/control-centre/" % (port, prefix), timeout=2)
        ready = True
    except urllib.error.HTTPError:
        ready = True              # vite answered, just not 200
    except Exception:
        pass
    out["state"] = "running" if ready else "starting"
print(json.dumps(out))
'''


def status(t: ssh.Target, wdir: str) -> dict:
    """{"state": stopped|starting|running|unknown, "port", "base", "url", …}.

    starting = the stack has a control-centre port but Vite does not answer
    yet (npm install on a fresh workspace takes a while)."""
    try:
        r = ssh.run(t, f"python3 -c {shlex.quote(STATUS_PY)} {shlex.quote(wdir)}", timeout=15)
        st = json.loads(r.stdout) if r.returncode == 0 and r.stdout.strip() else \
            {"state": "unknown", "error": (r.stderr or "").strip() or f"exit {r.returncode}"}
    except Exception as e:  # noqa: BLE001
        st = {"state": "unknown", "error": str(e)}
    if st.get("port"):
        st["url"] = f"http://{t.host}:{st['port']}{st.get('base') or ''}"
    return st


def start(t: ssh.Target, wdir: str, rider_port: int | None = None,
          driver_port: int | None = None, info: dict | None = None) -> dict:
    """The stack starts the control centre itself; this only checks that it
    will. Returns the status, or {"error"} when there is no control-center/
    checkout or the running stack predates the control-centre process.
    (The ports and info arguments are kept for older callers.)"""
    st = status(t, wdir)
    if st.get("error"):
        return st
    if not st.get("checkoutPresent"):
        return {"error": "no control-center/ in the workspace"}
    if not st.get("port"):
        return {"error": "this workspace's stack has no control-centre process "
                         "(started before it was added) - restart the stack"}
    return st


def wait_running(t: ssh.Target, wdir: str, timeout: int = 900, interval: int = 10) -> dict:
    """Wait until the control centre answers (npm install on a fresh workspace
    can take a while). Returns its status; raises RuntimeError when it is not
    part of the stack or the timeout passes."""
    deadline = time.time() + timeout
    r = status(t, wdir)
    while r.get("state") != "running":
        if r.get("state") == "stopped" or time.time() > deadline:
            raise RuntimeError(f"control centre {r.get('state')} - see the control-centre "
                               f"process in the stack log")
        time.sleep(interval)
        r = status(t, wdir)
    return r


def stop(t: ssh.Target, wdir: str) -> dict:
    """Stop a control centre left by the old detached start (its process group
    from data/control-center.json), so nothing is left behind. The stack's own
    control-centre process stops with the stack.

    Returns the status afterwards, or {"error"}."""
    d = shlex.quote(wdir)
    script = f"""
cd {d} || exit 0
S={STATE}
[ -f "$S" ] || exit 0
PID=$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1]))["pid"])' "$S" 2>/dev/null || true)
if [ -n "$PID" ]; then
  kill -TERM -- "-$PID" 2>/dev/null || kill -TERM "$PID" 2>/dev/null || true
  for i in 1 2 3 4 5 6 7 8 9 10; do kill -0 -- "-$PID" 2>/dev/null || break; sleep 0.5; done
  kill -KILL -- "-$PID" 2>/dev/null || true
fi
rm -f "$S"
"""
    try:
        r = ssh.run(t, script, timeout=20)
    except Exception as e:  # noqa: BLE001
        return {"error": f"could not stop control-center on the dev-box: {e}"}
    if r.returncode != 0:
        return {"error": (r.stderr or "").strip() or f"stop exited {r.returncode}"}
    return status(t, wdir)
