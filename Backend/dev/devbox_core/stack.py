"""The backend stack in a devbox workspace, run in the background.

Same command the 7070 card runs in its terminal, but detached (setsid nohup)
and without process-compose's TUI (-t=false), so a runner with no terminal can
start it, follow it through its log and stop it:

  <workspace>/data/stack.log     everything the stack prints, one
                                 "[<process>\t] …" line per service line
  <workspace>/data/stack.json    {"pid": …}  pid = process group id

The stack itself restores the MinIO build cache, builds, then starts the
services (cache-restore → cabal-build → services in nix/services/nammayatri.nix).
"""
from __future__ import annotations

import json
import os
import shlex
import time
import urllib.parse
import urllib.request
from typing import Callable

from . import ssh

FRONTDESK_PORT = 8888
# The front desk's public-only site (rider + driver APIs), which the devbox
# announces to hopper; testers reach it on any network through hopper's gateway:
#   <GATEWAY_URL>/api/<devbox>/<PUBLIC_PORT>/<workspace>/rider-app/...
PUBLIC_PORT = 8889
GATEWAY_URL = os.environ.get("HOPPER_GATEWAY_URL",
                             "https://service-discovery.devenv.movingtech.net").rstrip("/")
STACK_COMMAND = ("cd Backend && nix develop .#backend -c "
                 ", run-mobility-stack-dev-on-available-ports -t=false")
# A non-login ssh shell lacks the nix profile on PATH.
_PATH = 'export PATH="$HOME/.nix-profile/bin:/nix/var/nix/profiles/default/bin:$PATH"'
LOG = "data/stack.log"
STATE = "data/stack.json"
SCRIPT = "data/stack-start.sh"

# What the log shows, in the order the stack gets there. The stage is the
# last one whose marker has appeared.
STAGES = [
    ("preflight", "Pre-flight"),
    ("cache-restore", "[cache-restore"),
    ("build", "[cabal-build"),
    ("services", "[rider-app-exe"),
]
BUILD_FAILED = "cabal: Failed to build"

# Services that must answer through the front desk before the stack counts as
# ready: <name>/<path> under http://<host>:8888/<workspace>/.
READY_PATHS = ["rider-app/v2", "dynamic-offer-driver-app/ui"]


def _alive_sh() -> str:
    """Bash: $pid from the state file, $alive = yes/no."""
    return (f"pid=$(jq -r '.pid // empty' {STATE} 2>/dev/null || true); "
            'alive=no; [ -n "$pid" ] && kill -0 "$pid" 2>/dev/null && alive=yes; ')


def start(t: ssh.Target, wdir: str, timeout: int = 30) -> dict:
    """Start the stack detached. Does nothing when it is already running.

    Returns {"started": bool, "pid": int}."""
    script = f"""set -e
cd {shlex.quote(wdir)}
mkdir -p data
{_alive_sh()}
if [ "$alive" = yes ]; then echo "{{\\"started\\": false, \\"pid\\": $pid}}"; exit 0; fi
cat > {SCRIPT} <<'EOF'
{_PATH}
cd {shlex.quote(wdir)}
exec bash -c {shlex.quote(STACK_COMMAND)}
EOF
setsid nohup bash {SCRIPT} > {LOG} 2>&1 < /dev/null &
pid=$!
echo "{{\\"pid\\": $pid}}" > {STATE}
echo "{{\\"started\\": true, \\"pid\\": $pid}}"
"""
    r = ssh.run(t, script, timeout=timeout)
    if r.returncode != 0 or not r.stdout.strip():
        raise RuntimeError(f"stack start failed on {t.host}: {(r.stderr or '').strip() or f'exit {r.returncode}'}")
    return json.loads(r.stdout.strip().splitlines()[-1])


def status(t: ssh.Target, wdir: str, timeout: int = 20) -> dict:
    """{"alive": bool, "pid": int|None, "stage": str, "buildFailed": bool}.

    stage is "none" before the stack was started, "nix-eval" while Nix prepares
    the shell (the log has lines but no pre-flight yet), then the STAGES names."""
    checks = "".join(
        f"grep -aqF {shlex.quote(marker)} {LOG} 2>/dev/null && stage={name}; "
        for name, marker in STAGES)
    script = (f"cd {shlex.quote(wdir)} 2>/dev/null || {{ echo '{{\"alive\": false, \"pid\": null, "
              f"\"stage\": \"none\", \"buildFailed\": false, \"missing\": true}}'; exit 0; }}; "
              + _alive_sh() + f"stage=none; [ -s {LOG} ] && stage=nix-eval; " + checks +
              f"bf=false; grep -aqF {shlex.quote(BUILD_FAILED)} {LOG} 2>/dev/null && bf=true; "
              'echo "{\\"alive\\": $([ $alive = yes ] && echo true || echo false), '
              '\\"pid\\": ${pid:-null}, \\"stage\\": \\"$stage\\", \\"buildFailed\\": $bf}"')
    r = ssh.run(t, script, timeout=timeout)
    if r.returncode != 0 or not r.stdout.strip():
        raise RuntimeError(f"stack status failed on {t.host}: {(r.stderr or '').strip() or f'exit {r.returncode}'}")
    return json.loads(r.stdout.strip().splitlines()[-1])


def tail(t: ssh.Target, wdir: str, n: int = 30, timeout: int = 20) -> str:
    """Last n lines of the stack log (carriage returns turned into new lines)."""
    r = ssh.run(t, f"tr '\\r' '\\n' < {shlex.quote(wdir)}/{LOG} 2>/dev/null | tail -n {int(n)}",
                timeout=timeout)
    return r.stdout


def ports(t: ssh.Target, wdir: str, timeout: int = 15) -> dict:
    """The workspace's resolved ports ({service: port}) from data/ports.json,
    which the stack's pre-flight writes. {} when the stack has not got there."""
    r = ssh.run(t, f"cat {shlex.quote(wdir)}/data/ports.json 2>/dev/null", timeout=timeout)
    try:
        return {k: int(v) for k, v in (json.loads(r.stdout).get("ports") or {}).items()}
    except (ValueError, AttributeError):
        return {}


def _answers(url: str) -> bool:
    try:
        with urllib.request.urlopen(url, timeout=5) as resp:
            return resp.status < 500
    except urllib.error.HTTPError as e:
        return e.code < 500 and e.code != 404
    except Exception:
        return False


def office_base(host: str, name: str) -> str:
    """The workspace through the front desk: office network / tailnet only."""
    return f"http://{host}:{FRONTDESK_PORT}/{name}"


def public_base(devbox: str, name: str) -> str:
    """The workspace through hopper's gateway: any network. `devbox` is the name
    hopper knows it by (its hostname, same as the fleet's machine name)."""
    return f"{GATEWAY_URL}/api/{devbox}/{PUBLIC_PORT}/{name}"


def is_public(devbox: str) -> bool:
    """hopper lists the devbox's public front desk, seen in the last 30s — i.e.
    its gateway will forward to it (it 404s otherwise)."""
    url = f"{GATEWAY_URL}/service/list?magicDNS={urllib.parse.quote(devbox)}"
    try:
        with urllib.request.urlopen(url, timeout=10) as resp:
            services = json.load(resp)
    except Exception:
        return False
    return any(s.get("magic_dns_name") == devbox and s.get("port") == PUBLIC_PORT
               and s.get("public") for s in services)


def ready_urls(base: str) -> list:
    return [f"{base}/{p}" for p in READY_PATHS]


def wait_ready(t: ssh.Target, name: str, wdir: str, timeout: int = 3600,
               interval: int = 15, out: Callable[[str], None] = print,
               on_stage: Callable[[str], None] | None = None,
               until: str = "ready") -> dict:
    """Follow the stack until rider-app and driver-app answer through the front
    desk (until="ready"), or only until the services start, i.e. the build is
    over (until="services"). Prints each stage change (and passes it to
    on_stage). Raises RuntimeError (with the last log lines) when the stack
    dies, the build fails, or the timeout passes."""
    deadline = time.time() + timeout
    seen = ""
    while True:
        st = status(t, wdir)
        if st["stage"] != seen:
            seen = st["stage"]
            out(f"[stack] stage: {seen}")
            if on_stage:
                on_stage(seen)
        if st.get("buildFailed"):
            raise RuntimeError("build failed\n" + tail(t, wdir, 40))
        if not st["alive"]:
            raise RuntimeError("stack is not running\n" + tail(t, wdir, 40))
        if until == "services" and seen == "services":
            return {"services": True}
        urls = ready_urls(office_base(t.host, name))
        if seen == "services" and all(_answers(u) for u in urls):
            out("[stack] stage: ready")
            return {"ready": True, "urls": urls}
        if time.time() > deadline:
            raise RuntimeError(f"not ready after {timeout}s (stage {seen})\n" + tail(t, wdir, 40))
        time.sleep(interval)


def stop(t: ssh.Target, wdir: str, grace: int = 30, timeout: int = 90) -> dict:
    """Stop the stack: TERM to its process group (process-compose then stops
    every service), wait up to `grace` seconds, then KILL whatever still runs
    inside the workspace folder.

    Returns {"wasRunning": bool, "killedLeftovers": int}."""
    d = shlex.quote(wdir.rstrip("/"))
    script = f"""cd {d} 2>/dev/null || {{ echo '{{"wasRunning": false, "killedLeftovers": 0}}'; exit 0; }}
{_alive_sh()}
# Leave the folder so this shell is not one of the processes "inside" it.
cd /
inside() {{ for c in /proc/[0-9]*; do w=$(readlink "$c/cwd" 2>/dev/null) || continue
  case "$w" in {d}|{d}/*) echo "${{c#/proc/}}";; esac; done; }}
was=false
if [ "$alive" = yes ]; then was=true; kill -TERM -- "-$pid" 2>/dev/null || true; fi
for i in $(seq {int(grace)}); do
  [ -z "$(inside)" ] && ! {{ [ -n "$pid" ] && kill -0 "$pid" 2>/dev/null; }} && break
  sleep 1
done
n=0; for p in $(inside); do kill -KILL "$p" 2>/dev/null && n=$((n+1)); done
rm -f {d}/{STATE}
echo "{{\\"wasRunning\\": $was, \\"killedLeftovers\\": $n}}"
"""
    r = ssh.run(t, script, timeout=timeout)
    if r.returncode != 0 or not r.stdout.strip():
        raise RuntimeError(f"stack stop failed on {t.host}: {(r.stderr or '').strip() or f'exit {r.returncode}'}")
    return json.loads(r.stdout.strip().splitlines()[-1])
