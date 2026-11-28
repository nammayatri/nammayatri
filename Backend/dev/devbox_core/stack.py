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
import re
import shlex
import subprocess
import time
import urllib.parse
import urllib.request
from typing import Callable

from . import ssh

FRONTDESK_PORT = 8888
# The port app builds use through hopper's gateway; testers reach it on any network:
#   <GATEWAY_URL>/api/<devbox>/<PUBLIC_PORT>/<workspace>/rider-app/...
# The front desk itself: the devbox announces it to hopper (nix-the-builder
# FRONTDESK_PUBLISH_MAIN=1) and hopper's nginx lets only the testers' apps
# through (hopper deploy/sd-nginx/devbox-8888-gate.conf).
PUBLIC_PORT = FRONTDESK_PORT
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

# process-compose's REST API is on this unix socket in the workspace; curl reads
# per-service state from it (no process-compose binary needed on the ssh PATH).
PC_SOCKET = "data/process-compose.sock"
# The services a release needs. Only these fail a release: the rest of the stack
# (mock-registry, location-tracking, notification-service, consumers, test tools)
# restarts by design (availability.restart = always / on_failure in
# nix/services/nammayatri.nix) while its dependencies warm up, so its restarts are
# not a failure.
# One-shot steps fail when they exit non-zero.
CORE_ONESHOT = ("cabal-build", "nammayatri-init", "dashboard-migrations")
# The two app servers (restart = always) are also restarted by design: the liveness
# probe kills a slow start (migrations on a fresh workspace), and config-sync restarts
# them through the process-compose API. Those are kills (a signal: exit -1, or 128+N).
# Only a real crash (the program exits with 1..127) counts, and only once the app has
# restarted RESTART_LIMIT times.
CORE_SERVERS = ("rider-app-exe", "dynamic-offer-driver-app-exe")
CORE = CORE_ONESHOT + CORE_SERVERS
RESTART_LIMIT = 3
# Consecutive ssh timeouts tolerated while polling before the run gives up.
SSH_RETRIES = 5
# The stages after which process-compose (and so its socket) is up.
WATCHED_STAGES = ("cache-restore", "build", "services")
# What a failure in each stage is called at the top of the GitHub run.
TITLES = {
    "none": "Stack did not start",
    "nix-eval": "Stack setup failed",
    "preflight": "Stack setup failed",
    "cache-restore": "Build failed",
    "build": "Build failed",
    "services": "Start failed",
}


class StackFailure(RuntimeError):
    """A failed stack, with the stage it failed in (for the run's error title)."""

    def __init__(self, stage: str, msg: str):
        super().__init__(msg)
        self.stage = stage

    @property
    def title(self) -> str:
        return TITLES.get(self.stage, "Release failed")


HINTS = [
    ("Expression doesn't match annotation",
     "dhall config doesn't match the code — check Backend/dhall-configs/dev/<service>.dhall"),
    ('sqlState = "25001"', "a DB migration uses CONCURRENTLY inside a multi-statement file (or a stale shared-kernel)"),
    ("SqlError", "a DB migration failed — see the last 'Executing:' line above"),
    ("Text file busy", "two services built the same binary at once (process-compose race)"),
    ("address already in use", "a port is already taken (AddrInUse)"),
]


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


def services(t: ssh.Target, wdir: str, timeout: int = 15) -> list:
    """Every process-compose service as {name, status, is_ready, restarts,
    exit_code, is_running}, read over the workspace's process-compose socket.
    [] before the socket exists (the stack has not reached services yet)."""
    r = _run_soft(t, f"curl -s --max-time 8 --unix-socket {shlex.quote(wdir)}/{PC_SOCKET} "
                     "http://localhost/processes 2>/dev/null", timeout=timeout)
    try:
        return json.loads(r).get("data") or []
    except (ValueError, AttributeError):
        return []


def _run_soft(t: ssh.Target, script: str, timeout: int) -> str:
    """stdout of an ssh call that only adds detail (progress, service list, log
    lines): a slow or broken ssh gives "" instead of an exception, so it can never
    fail a build that is fine. The next poll tries again."""
    try:
        return ssh.run(t, script, timeout=timeout).stdout or ""
    except (subprocess.TimeoutExpired, OSError):
        return ""


def _exited_bad(p: dict) -> bool:
    """Stopped and not running, with a non-zero exit code."""
    return (not p.get("is_running")
            and p.get("exit_code") not in (0, None)
            and p.get("status") in ("Completed", "Error", "Failed"))


def _crashed(p: dict) -> bool:
    """The last exit was the program failing (1..127), not a kill by a signal
    (-1 or 128+N: liveness probe, a restart through the API, a manual stop)."""
    code = p.get("exit_code")
    return isinstance(code, int) and 0 < code < 128


def _root_failure(procs: list) -> tuple:
    """The service that actually broke, and the core services left Skipped
    because of it. Returns (bad|None, [skipped core names]).

    Broke = a CORE_ONESHOT step exited non-zero; or a CORE_SERVERS app crashed
    (exit 1..127) after restarting RESTART_LIMIT times; or a core service was
    Skipped because something it waits on failed: then the failed service is
    reported (whatever it is), else the skipped core service itself. Other
    services' restarts and failures are ignored."""
    by_name = {p.get("name"): p for p in procs}
    skipped = [n for n in CORE if (by_name.get(n) or {}).get("status") == "Skipped"]
    for name in CORE_ONESHOT:
        p = by_name.get(name)
        if p and _exited_bad(p):
            return p, skipped
    for name in CORE_SERVERS:
        p = by_name.get(name)
        if p and _crashed(p) and ((p.get("restarts") or 0) >= RESTART_LIMIT or _exited_bad(p)):
            return p, skipped
    if skipped:
        cause = next((p for p in procs if p.get("status") != "Skipped" and _exited_bad(p)), None)
        return (cause or by_name[skipped[0]]), skipped
    return None, skipped


def service_errors(t: ssh.Target, wdir: str, name: str, n: int = 20, timeout: int = 20) -> str:
    """The error-ish lines of one service, so a failure report shows that
    service's reason, not the whole stack's noise. Reads the service's own log
    (process-compose writes <name>.log in the stack's working dir, Backend/),
    else its "[<name>" lines in the stack log. When no line looks like an error,
    the last n lines instead."""
    d = shlex.quote(wdir)
    pat = "'error|exception|Executing:|match annotation|SqlError|busy|in use'"
    lines = f"| tr '\\r' '\\n' | sed 's/\\x1b\\[[0-9;]*m//g'"
    r = _run_soft(t, f"""cd {d} 2>/dev/null || exit 0
src=""
for f in Backend/{shlex.quote(name)}.log {shlex.quote(name)}.log; do [ -s "$f" ] && {{ src=$f; break; }}; done
if [ -n "$src" ]; then all=$(tail -n 5000 "$src" {lines}); else all=$(grep -aF {shlex.quote("[" + name)} {LOG} 2>/dev/null | tail -n 5000 {lines}); fi
errs=$(printf '%s\\n' "$all" | grep -aiE {pat} | tail -n {int(n)})
if [ -n "$errs" ]; then printf '%s\\n' "$errs"; else printf '%s\\n' "$all" | tail -n {int(n)}; fi""",
                  timeout=timeout)
    return r.strip()


def _failure_message(t: ssh.Target, wdir: str, bad: dict, skipped: list) -> str:
    errs = service_errors(t, wdir, bad["name"])
    hint = _hint_for(errs)
    if bad.get("status") == "Skipped":
        what = "did not start (something it waits on failed)"
    elif _exited_bad(bad):
        what = f"exited with code {bad.get('exit_code')}"
    else:
        what = f"keeps crashing ({bad.get('restarts', 0)} restarts)"
    msg = f"{bad['name']} {what}"
    if hint:
        msg += f"\nHint: {hint}"
    if errs:
        msg += "\n" + errs
    others = [n for n in skipped if n != bad["name"]]
    if others:
        msg += "\nDid not start because of it: " + ", ".join(others)
    return msg


def _hint_for(text: str) -> str | None:
    """The first HINT whose pattern appears in `text`, else None."""
    return next((h for pat, h in HINTS if pat in text), None)


def build_progress(t: ssh.Target, wdir: str, timeout: int = 15) -> str:
    """A one-line summary of where the build is: the cache result and the newest
    '[N of M] Compiling <pkg>' line. '' when there is nothing to report yet.
    Greps first and cleans only the matching line, so the poll stays cheap
    however big the log grows."""
    pat = shlex.quote("cache-restore: (cache hit|cache miss|no cache|done)|of [0-9]+] Compiling")
    r = _run_soft(t,
                  f"grep -aE 'Compiling|cache-restore:' {shlex.quote(wdir)}/{LOG} 2>/dev/null | tail -n 20"
                  " | tr '\\r' '\\n' | sed 's/\\x1b\\[[0-9;]*m//g'"
                  f" | grep -aE {pat} | tail -n 1",
                  timeout=timeout)
    line = r.strip()
    if not line:
        return ""
    # drop the "[cache-restore\t] " / "[cabal-build\t] " prefix, then a trailing
    # GHC reason like " [Foo.Bar changed]" so a compile line reads "[N of M] Compiling X".
    piece = line.split("] ", 1)[-1].strip()
    return re.sub(r"\s*\[[^\]]*\]\s*$", "", piece)


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
    on_stage). Raises StackFailure (a RuntimeError naming the stage, with the
    reason) when the stack dies, the build fails, a core service crashes, or
    the timeout passes."""
    deadline = time.time() + timeout
    seen = ""
    last_progress = ""
    ssh_misses = 0
    while True:
        try:
            st = status(t, wdir)
            ssh_misses = 0
        except subprocess.TimeoutExpired:
            ssh_misses += 1
            if ssh_misses >= SSH_RETRIES:
                raise StackFailure(seen or "none", f"lost ssh to {t.host}: {ssh_misses} status checks in a row timed out")
            out(f"[stack] ssh to {t.host} timed out, retrying")
            time.sleep(interval)
            continue
        if st["stage"] != seen:
            seen = st["stage"]
            out(f"[stack] stage: {seen}")
            if on_stage:
                on_stage(seen)
        if st.get("buildFailed"):
            raise StackFailure("build", "cabal build failed\n" + tail(t, wdir, 40))
        if not st["alive"]:
            raise StackFailure(seen, "the stack stopped\n" + tail(t, wdir, 40))
        # While building: show the cache result and the compile count, so Build
        # is not a silent 14-minute block.
        if seen in ("cache-restore", "build"):
            prog = build_progress(t, wdir)
            if prog and prog != last_progress:
                last_progress = prog
                out(f"[stack] {prog}")
        # Watch the core services from the moment process-compose is up. A crashed
        # init step (stage stays "build": the apps never print) or a crash-looping
        # app leaves the process group alive, so without this the loop would wait
        # out the full timeout. Fail fast with that service's reason.
        if seen in WATCHED_STAGES:
            bad, skipped = _root_failure(services(t, wdir))
            if bad is not None:
                raise StackFailure(seen, _failure_message(t, wdir, bad, skipped))
        if until == "services" and seen == "services":
            return {"services": True}
        urls = ready_urls(office_base(t.host, name))
        if seen == "services" and all(_answers(u) for u in urls):
            out("[stack] stage: ready")
            return {"ready": True, "urls": urls}
        if time.time() > deadline:
            raise StackFailure(seen, f"not ready after {timeout}s (stage {seen})\n" + tail(t, wdir, 40))
        time.sleep(interval)


def stop(t: ssh.Target, wdir: str, grace: int = 30, timeout: int | None = None) -> dict:
    """Stop the stack: TERM to its process group (process-compose then stops
    every service), wait up to `grace` seconds, then KILL whatever still runs
    inside the workspace folder. The ssh call gets grace + 60 seconds unless
    `timeout` says otherwise.

    Returns {"wasRunning": bool, "killedLeftovers": int}."""
    d = shlex.quote(wdir.rstrip("/"))
    script = f"""cd {d} 2>/dev/null || {{ echo '{{"wasRunning": false, "killedLeftovers": 0}}'; exit 0; }}
{_alive_sh()}
# Leave the folder so this shell is not one of the processes "inside" it.
cd /
# One find over /proc, not one readlink per process: on a busy dev-box (~800
# processes) the per-process loop took ~2.5 s, so `grace` rounds of it outran
# the ssh timeout and the stop run failed, leaving the release at STOPPING.
inside() {{ find /proc -mindepth 2 -maxdepth 2 -name cwd -printf '%h %l\\n' 2>/dev/null |
  while read -r p w; do case "$w" in {d}|{d}/*) echo "${{p#/proc/}}";; esac; done; }}
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
    r = ssh.run(t, script, timeout=timeout or grace + 60)
    if r.returncode != 0 or not r.stdout.strip():
        raise RuntimeError(f"stack stop failed on {t.host}: {(r.stderr or '').strip() or f'exit {r.returncode}'}")
    return json.loads(r.stdout.strip().splitlines()[-1])
