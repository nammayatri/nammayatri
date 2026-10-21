"""Control centre on the devbox, next to a workspace's stack.

The workspace's control-center/ (rsynced with the backend: it sits inside the
repo dir, gitignored) runs as a Vite dev server: its /api/bap|bpp proxy reaches
the workspace's rider-app / driver-app as localhost, and anyone on the office
network can open http://<devbox>:<port>. Started detached (setsid) so it does
not die with the SSH session; its pid/port live in <workspace>/data/, which
rsync never touches.

Moved from test-tool/local-api/server.py so the devbox workflow can start it.
"""
from __future__ import annotations

import json
import shlex
import time

from . import ssh

PORTS = range(5300, 5320)   # clear of the stack's own resolved 5xxx ports
STATE = "data/control-center.json"
LOG = "data/control-center.log"

# Runs on the devbox: prints this workspace's control-center state as JSON.
STATUS_PY = r'''
import json, os, sys, urllib.request, urllib.error
d = sys.argv[1]
out = {"state": "stopped", "checkoutPresent": os.path.isdir(os.path.join(d, "control-center"))}
try:
    s = json.load(open(os.path.join(d, "data/control-center.json")))
except Exception:
    s = None
if s:
    out.update(s)
    try:
        os.kill(int(s["pid"]), 0)
        alive = True
    except Exception:
        alive = False
    ready = False
    if alive:
        try:
            urllib.request.urlopen("http://127.0.0.1:%s/" % s["port"], timeout=2)
            ready = True
        except urllib.error.HTTPError:
            ready = True          # vite answered, just not 200
        except Exception:
            pass
    out["state"] = "running" if ready else ("starting" if alive else "exited")
try:
    with open(os.path.join(d, "data/control-center.log"), errors="replace") as f:
        out["log"] = f.read().splitlines()[-40:]
except Exception:
    out["log"] = []
print(json.dumps(out))
'''


def status(t: ssh.Target, wdir: str) -> dict:
    """{"state": stopped|starting|running|exited|unknown, "port", "url", "log", …}."""
    try:
        r = ssh.run(t, f"python3 -c {shlex.quote(STATUS_PY)} {shlex.quote(wdir)}", timeout=15)
        st = json.loads(r.stdout) if r.returncode == 0 and r.stdout.strip() else \
            {"state": "unknown", "error": (r.stderr or "").strip() or f"exit {r.returncode}"}
    except Exception as e:  # noqa: BLE001
        st = {"state": "unknown", "error": str(e)}
    if st.get("port"):
        st["url"] = f"http://{t.host}:{st['port']}"
    return st


def start(t: ssh.Target, wdir: str, rider_port: int, driver_port: int,
          info: dict | None = None) -> dict:
    """Start control-center on a free port in PORTS, pointed at this workspace's
    rider-app / driver-app. `info` (branch, commit, dirty, deployHash) is saved
    with the pid/port so status can show what is running.

    Returns the status (unchanged when already starting/running), or {"error"}."""
    current = status(t, wdir)
    if current.get("state") in ("starting", "running"):
        return current
    if not current.get("checkoutPresent"):
        return {"error": "no control-center/ in the workspace"}
    info = info or {}
    d = shlex.quote(wdir)
    script = f"""
set -e
cd {d}
mkdir -p data
PORT=""
for p in {' '.join(str(p) for p in PORTS)}; do
  ss -ltn | grep -q ":$p " || {{ PORT=$p; break; }}
done
[ -n "$PORT" ] || {{ echo "no free port in {PORTS.start}-{PORTS.stop - 1}" >&2; exit 4; }}
cd control-center
VITE_BAP_URL=http://localhost:{int(rider_port)} VITE_BPP_URL=http://localhost:{int(driver_port)} CC_PORT=$PORT \\
  setsid nohup nix shell --inputs-from {d} nixpkgs-unstable#nodejs_22 --command bash -c \\
    'npm install --no-audit --no-fund && exec npm run dev -- --host 0.0.0.0 --port "$CC_PORT" --strictPort' \\
  > ../{LOG} 2>&1 < /dev/null &
PID=$!
cd ..
printf '{{"pid": %s, "port": %s, "startedAt": %s, "riderPort": {int(rider_port)}, "driverPort": {int(driver_port)}, "branch": %s, "commit": %s, "dirty": {json.dumps(bool(info.get('dirty')))}, "deployHash": %s}}\\n' \\
  "$PID" "$PORT" "$(date +%s)" {shlex.quote(json.dumps(info.get('branch') or ''))} \\
  {shlex.quote(json.dumps(info.get('commit') or ''))} \\
  {shlex.quote(json.dumps(info.get('deployHash') or ''))} > {STATE}
echo "$PORT"
"""
    try:
        r = ssh.run(t, script, timeout=30)
    except Exception as e:  # noqa: BLE001
        return {"error": f"could not start control-center on the dev-box: {e}"}
    if r.returncode != 0:
        return {"error": (r.stderr or r.stdout or "").strip() or f"start exited {r.returncode}"}
    return status(t, wdir)


def wait_running(t: ssh.Target, wdir: str, timeout: int = 900, interval: int = 10) -> dict:
    """Wait until the control centre answers (npm install on a fresh workspace
    can take a while). Returns its status; raises RuntimeError with the last
    log lines when it exits or the timeout passes."""
    deadline = time.time() + timeout
    r = status(t, wdir)
    while r.get("state") != "running":
        if r.get("state") in ("exited", "stopped") or time.time() > deadline:
            tail = "\n".join((r.get("log") or [])[-15:])
            raise RuntimeError(f"control centre {r.get('state')}\n{tail}")
        time.sleep(interval)
        r = status(t, wdir)
    return r


def stop(t: ssh.Target, wdir: str) -> dict:
    """Stop exactly the process group started above, so nothing is left behind.

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
