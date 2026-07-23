#!/usr/bin/env python3
"""
Test Context API + CORS Proxy

1. Serves test context from local DB (merchants, riders, drivers, tokens)
2. Scans integration-test collections and environments
3. Captures per-API service log deltas
4. Proxies API calls to rider-app/driver-app with CORS headers

Endpoints:
  GET  /api/context              → All test context data
  GET  /api/riders               → Available riders
  GET  /api/drivers              → Available drivers
  GET  /api/merchants            → Available merchants
  GET  /api/variants             → Vehicle variants
  GET  /api/collections          → Scan integration-test collection dirs
  GET  /api/collection/<dir>/<f> → Serve raw Postman collection JSON
  POST /api/logs/start           → Start tail -f on all service logs, returns token
  POST /api/logs/stop            → Stop tails, return captured log text
  ANY  /proxy/rider/*            → Proxy to rider-app (localhost:8013)
  ANY  /proxy/driver/*           → Proxy to driver-app (localhost:8016)
  ANY  /proxy/provider-dashboard/*  → Proxy to provider-dashboard (localhost:8018)

Port: 7082
"""

import json
import shutil
import sys
import os
import subprocess
import threading
import time
import base64
import uuid
import errno
import fcntl
import struct
import select as _select
from pathlib import Path
from http.server import HTTPServer, BaseHTTPRequestHandler, ThreadingHTTPServer
from urllib.parse import urlparse, parse_qs
import re
import signal
import urllib.request
import urllib.error

try:
    import pty as _pty
    import termios as _termios
except Exception:
    _pty = None
    _termios = None


_terminal_sessions: dict = {}
_terminal_lock = threading.Lock()


def _terminal_spawn(cols: int, rows: int, cwd: str, shell: str):
    """Fork a PTY-backed child running `shell`. Returns (session_id, pid, fd)."""
    if _pty is None or _termios is None:
        raise RuntimeError("pty/termios not available on this platform")
    pid, fd = _pty.fork()
    if pid == 0:
        try:
            try:
                os.setsid()
            except Exception:
                pass
            try:
                fcntl.ioctl(0, _termios.TIOCSWINSZ,
                            struct.pack("HHHH", rows, cols, 0, 0))
            except Exception:
                pass
            try:
                os.chdir(cwd)
            except Exception:
                pass
            env = os.environ.copy()
            env["TERM"] = "xterm-256color"
            env.setdefault("LANG", env.get("LANG", "en_US.UTF-8"))
            os.execvpe(shell, [shell, "-l"], env)
        except Exception as e:
            sys.stderr.write(f"pty exec failed: {e}\n")
            os._exit(127)
    fcntl.fcntl(fd, fcntl.F_SETFL, fcntl.fcntl(fd, fcntl.F_GETFL) | os.O_NONBLOCK)
    sid = uuid.uuid4().hex
    with _terminal_lock:
        _terminal_sessions[sid] = {
            "fd": fd,
            "pid": pid,
            "cwd": cwd,
            "shell": shell,
            "lock": threading.Lock(),
            "closed": False,
        }
    return sid, pid, fd


def _terminal_close(sid: str):
    with _terminal_lock:
        sess = _terminal_sessions.pop(sid, None)
    if not sess:
        return None
    sess["closed"] = True
    fd = sess["fd"]
    pid = sess["pid"]
    try:
        os.killpg(os.getpgid(pid), signal.SIGTERM)
    except Exception:
        try:
            os.kill(pid, signal.SIGTERM)
        except Exception:
            pass
    deadline = time.time() + 1.0
    while time.time() < deadline:
        try:
            wpid, _st = os.waitpid(pid, os.WNOHANG)
            if wpid == pid:
                break
        except ChildProcessError:
            break
        except Exception:
            break
        time.sleep(0.05)
    try:
        os.killpg(os.getpgid(pid), signal.SIGKILL)
    except Exception:
        try:
            os.kill(pid, signal.SIGKILL)
        except Exception:
            pass
    try:
        os.close(fd)
    except Exception:
        pass
    try:
        os.waitpid(pid, os.WNOHANG)
    except Exception:
        pass
    return sess

# Flush print() to the process-compose log file in real time (not at exit).
sys.stdout.reconfigure(line_buffering=True)
sys.stderr.reconfigure(line_buffering=True)


def redis_cmd(*args):
    """Run a redis-cli command against the cluster, fallback to single-node."""
    cluster_cmd = ["redis-cli", "--cluster",
                   "call", "localhost:30001"] + list(args)
    r = subprocess.run(cluster_cmd, capture_output=True, text=True, timeout=10)
    if r.returncode == 0:
        return True, r.stdout
    # fallback single-node
    r2 = subprocess.run(["redis-cli"] + list(args),
                        capture_output=True, text=True, timeout=10)
    return r2.returncode == 0, r2.stdout


PORT = 7082

VICTORIA_METRICS_URL = os.environ.get("VICTORIA_METRICS_URL", "")
# Base URL of VictoriaMetrics for read queries (without /api/v1 suffix).
# Defaults to localhost:8428 — the port the nix process binds when running locally.
VICTORIA_METRICS_QUERY_URL = os.environ.get("VICTORIA_METRICS_QUERY_URL", "http://127.0.0.1:8428")

# ── Paths ──
SCRIPT_DIR = Path(__file__).resolve().parent
COLLECTIONS_DIR = SCRIPT_DIR.parent.parent / "integration-tests" / "collections"
PROJECT_ROOT = SCRIPT_DIR.parent.parent.parent.parent  # nammayatri/

# Resolved ports from the devbox registry slice (.users[DEVBOX_KEY].ports, set
# by the run-mobility-stack-dev preflight); fall back to base ports.nix if absent.
DEVBOX_REGISTRY_FILE = os.environ.get("DEVBOX_REGISTRY_FILE", "/tmp/devbox-registry.json")
PORTS_BASE_PATH = PROJECT_ROOT / "Backend" / "nix" / "services" / "ports.nix"

# Compiled once. Matches lines like `  rider-app = 8013;` in ports.nix.
_PORTS_LINE_RE = re.compile(r"^\s*([a-zA-Z][a-zA-Z0-9_-]*)\s*=\s*(\d+)\s*;")


# ${VAR:default} resolver — mirror of config-sync's _expand_env_templates.
# Used by the newman webhook runner to pre-process Postman env files before
# invoking newman (which doesn't expand ${...} syntax natively). Local_*
# env files carry the templates; Master_* files don't, but the helper is
# idempotent so feeding either through it is safe.
_ENV_TEMPLATE_RE = re.compile(r"\$\{([A-Z_][A-Z0-9_]*)(?::([^}]*))?\}")


def _expand_env_templates(node):
    if isinstance(node, dict):
        return {k: _expand_env_templates(v) for k, v in node.items()}
    if isinstance(node, list):
        return [_expand_env_templates(v) for v in node]
    if isinstance(node, str):
        return _ENV_TEMPLATE_RE.sub(
            lambda m: os.environ.get(m.group(1), m.group(2) or ""),
            node,
        )
    return node


def _materialize_env_for_newman(src_path: Path) -> Path:
    """Resolve ${VAR:default} in a Postman env file, write to /tmp, return the
    resolved path. Caller deletes after newman finishes. Returns the original
    path unchanged if expansion is a no-op (file has no template literals)."""
    try:
        data = json.loads(src_path.read_text())
    except (json.JSONDecodeError, OSError):
        return src_path
    resolved = _expand_env_templates(data)
    if resolved == data:
        return src_path
    out = Path("/tmp") / f"{src_path.stem}.resolved.json"
    out.write_text(json.dumps(resolved, ensure_ascii=False))
    return out


def _read_ports_from_registry():
    """{ 'service-name': port, ... } from this checkout's devbox-registry slice,
    or None if the registry / key / entry isn't available."""
    key = os.environ.get("DEVBOX_KEY", "")
    if not key:
        return None
    try:
        with open(DEVBOX_REGISTRY_FILE) as f:
            reg = json.load(f)
        ports = (reg.get("users", {}).get(key) or {}).get("ports")
        if isinstance(ports, dict) and ports:
            return {k: int(v) for k, v in ports.items()}
    except (OSError, ValueError, TypeError):
        pass
    return None


def _read_ports_from_nix(path):
    """{ 'service-name': port, ... } parsed from a ports.nix-format file."""
    out = {}
    try:
        for line in path.read_text().splitlines():
            m = _PORTS_LINE_RE.match(line)
            if m:
                out[m.group(1)] = int(m.group(2))
    except OSError:
        pass
    return out


def _read_ports_table():
    """Return { 'source': ..., 'ports': { 'service-name': 1234, ... } }.

    Prefers this checkout's devbox-registry.json slice (the DYNAMICALLY resolved
    ports — same source nammayatri.nix reads), falling back to the base
    Backend/nix/services/ports.nix. Used by the dashboard for runtime port
    discovery — see GET /api/ports — and by _svc_port()."""
    reg_ports = _read_ports_from_registry()
    if reg_ports:
        return {"source": f"{DEVBOX_REGISTRY_FILE}[{os.environ.get('DEVBOX_KEY', '')}]",
                "ports": reg_ports}
    src = (str(PORTS_BASE_PATH.relative_to(PROJECT_ROOT))
           if PORTS_BASE_PATH.is_relative_to(PROJECT_ROOT) else str(PORTS_BASE_PATH))
    return {"source": src, "ports": _read_ports_from_nix(PORTS_BASE_PATH)}


def _svc_port(name, default):
    """Resolved port for a service by its ports.nix name, with a fallback.

    The stack resolves each dev's ports DYNAMICALLY (e.g. rider-app 8013 -> 9013)
    and records them in devbox-registry.json. Anything that talks to a backend
    service must read the resolved value, not the canonical base port — otherwise
    it connects to a port nothing is listening on (the cause of the endless
    '[startup] waiting for: rider-app:8013 Connection refused' loop)."""
    try:
        return _read_ports_table().get("ports", {}).get(name, default)
    except Exception:
        return default

# Whitelisted GitHub repos that the /api/git/refs endpoint can introspect.
# Each entry maps "<owner>/<name>" → the local checkout path under data/
# (gitignored). Adding a new launcher? Add an entry here so its modal can
# offer a branch / commit picker.
KNOWN_REPOS: dict = {
    "nammayatri/ny-react-native": PROJECT_ROOT / "data" / "ny-react-native",
    "nammayatri/control-center":  PROJECT_ROOT / "data" / "control-center",
}

# ── Service log files for per-API capture ──
# Haskell service logs (/tmp) — EulerHS logger writes here
SERVICE_LOGS = {
    "rider-app": Path("/tmp/rider-app.log"),
    "rider-app-eul": Path("/tmp/rider-app-eul.log"),
    "driver-app": Path("/tmp/dynamic-offer-driver-app.log"),
    "driver-app-eul": Path("/tmp/dynamic-offer-driver-app-eul.log"),
    "beckn-gateway": Path("/tmp/beckn-gateway.log"),
    "search-result-aggregator": Path("/tmp/search-result-aggregator.log"),
    "producer": Path("/tmp/producer.log"),
    "rider-producer": Path("/tmp/rider-producer.log"),
    "provider-dashboard": Path("/tmp/provider-dashboard.log"),
    "provider-dashboard-eul": Path("/tmp/provider-dashboard-eul.log"),
    "rider-dashboard": Path("/tmp/rider-dashboard.log"),
    "rider-dashboard-eul": Path("/tmp/rider-dashboard-eul.log"),
}
MAX_LOG_DELTA_BYTES = 64 * 1024  # 64KB per service

# Use the DYNAMICALLY-resolved ports (devbox-registry.json slice), falling back
# to the canonical base ports only if the resolved table is unavailable.
RIDER_URL = os.environ.get("RIDER_URL") or f"http://localhost:{_svc_port('rider-app', 8013)}"
DRIVER_URL = os.environ.get("DRIVER_URL") or f"http://localhost:{_svc_port('dynamic-offer-driver-app', 8016)}"

# ── Config-sync (replaces the standalone config-sync process) ──
CONFIG_SYNC_DIR = PROJECT_ROOT / "Backend" / "dev" / "config-sync"
# Allowed --from values. The actual S3 bucket / per-direction version is owned
# by config_transfer.py (DEFAULT_S3_PUBLIC_BUCKET + DEFAULT_FETCH_VERSIONS), so
# there is only one source of truth for the fetch URL.
CONFIG_SYNC_ENVS = ("master", "prod", "prod_international")
CONFIG_SYNC_DEFAULT_FROM = os.environ.get("CONFIG_SYNC_DEFAULT_FROM", "prod")
CONFIG_SYNC_MAX_LOG_LINES = 4000

CONFIG_SYNC_MARKER_PATH = PROJECT_ROOT / "data" / "config-sync" / "metadata.json"


def _load_last_synced():
    """Read <repo-root>/data/config-sync/metadata.json so /api/config-sync/status
    reports `last_synced` correctly even after a context-api restart."""
    try:
        with CONFIG_SYNC_MARKER_PATH.open("r") as f:
            data = json.load(f)
        if data.get("from") and data.get("synced_at"):
            return {"from": data["from"], "finished_at": data["synced_at"]}
    except (FileNotFoundError, json.JSONDecodeError, OSError):
        pass
    return None


_config_sync_state = {
    "running": False,
    "from": None,
    "started_at": None,
    "finished_at": None,
    "exit_code": None,
    "error": None,
    "log": [],
    "last_synced": _load_last_synced(),
}
_config_sync_lock = threading.Lock()


TOLL_DASHBOARD_ACCESS_SQL = (
    PROJECT_ROOT / "Backend" / "dev" / "local-testing-data" / "toll-dashboard-access.sql"
)
PROVIDER_DASHBOARD_SEED_SQL = (
    PROJECT_ROOT / "Backend" / "dev" / "local-testing-data" / "provider-dashboard.sql"
)


def _execute_sql_file(path: Path) -> tuple[bool, str | None]:
    """Run one SQL file in a single autocommit connection. Returns (ok, error)."""
    if not path.is_file():
        return False, f"file not found: {path}"
    import psycopg2
    try:
        conn = psycopg2.connect(**DB_CONFIG)
        conn.autocommit = True
        try:
            with conn.cursor() as cur:
                cur.execute(path.read_text())
        finally:
            conn.close()
        return True, None
    except Exception as e:
        return False, str(e)


def seed_toll_dashboard_access() -> dict:
    """Idempotent toll dashboard access_matrix + provider-dashboard token/merchant_access."""
    results: dict[str, object] = {"ok": True, "files": []}
    for label, sql_path in (
        ("toll-dashboard-access", TOLL_DASHBOARD_ACCESS_SQL),
        ("provider-dashboard", PROVIDER_DASHBOARD_SEED_SQL),
    ):
        ok, err = _execute_sql_file(sql_path)
        results["files"].append({"name": label, "ok": ok, "error": err})
        if not ok:
            results["ok"] = False
    return results


def _apply_local_testing_data():
    """Apply Backend/dev/local-testing-data/*.sql after config-sync + feature-migrations.
    These files seed test users/admin tokens etc. They were moved out of postgres
    initialDatabases so dev/ddl-migrations runs against empty tables (SET NOT NULL passes).
    Idempotent: each file is wrapped in its own transaction; failures are logged but
    do not abort the rest of the batch (so a single broken file doesn't take down
    everything)."""
    ltd_dir = PROJECT_ROOT / "Backend" / "dev" / "local-testing-data"
    if not ltd_dir.is_dir():
        return
    files = sorted(p for p in ltd_dir.iterdir() if p.suffix == ".sql")
    if not files:
        return
    print(f"  \033[96m[local-testing-data]\033[0m applying {len(files)} file(s)")
    import psycopg2
    conn = psycopg2.connect(**DB_CONFIG)
    conn.autocommit = False
    try:
        for f in files:
            cur = conn.cursor()
            try:
                cur.execute(f.read_text())
                conn.commit()
                print(f"  \033[96m[local-testing-data]\033[0m ok: {f.name}")
            except Exception as e:
                conn.rollback()
                print(f"  \033[96m[local-testing-data]\033[0m FAILED {f.name}: {e}")
            finally:
                cur.close()
    finally:
        conn.close()


def _restart_haskell_services():
    """Kill rider/driver/mock-registry so process-compose restarts them and they
    re-read boot-time config from the freshly-synced DB."""
    proj = str(PROJECT_ROOT)
    for exe in ("rider-app-exe", "dynamic-offer-driver-app-exe"):
        try:
            r = subprocess.run(["pgrep", "-f", f"{proj}.*{exe}"],
                               capture_output=True, text=True, timeout=5)
            for pid in r.stdout.strip().splitlines()[:1]:
                if pid:
                    subprocess.run(["kill", pid], timeout=5)
                    print(f"  \033[96m[config-sync]\033[0m killed {exe} (PID {pid}) — process-compose will restart")
        except Exception as e:
            print(f"  \033[96m[config-sync]\033[0m restart {exe} failed: {e}")
    mock_port = _svc_port("mock-registry", 8020)
    try:
        r = subprocess.run(["lsof", "-ti", f":{mock_port}"],
                           capture_output=True, text=True, timeout=5)
        for pid in r.stdout.strip().splitlines()[:1]:
            if pid:
                subprocess.run(["kill", pid], timeout=5)
                print(f"  \033[96m[config-sync]\033[0m killed mock-registry (PID {pid}, port {mock_port})")
    except Exception as e:
        print(f"  \033[96m[config-sync]\033[0m restart mock-registry failed: {e}")
    time.sleep(3)


def run_config_sync(from_env: str, restart_services: bool = True, force_fetch: bool = False):
    """Run `config_transfer.py import --from <env> --to local --fetch`.
    Streams stdout into _config_sync_state['log']. Restarts haskell services on success.

    The fetch URL is resolved entirely inside config_transfer.py from its
    DEFAULT_S3_PUBLIC_BUCKET + DEFAULT_FETCH_VERSIONS (overridable via
    CONFIG_SYNC_CLOUDFRONT_URL / CONFIG_SYNC_FETCH_URL_<DIRECTION> env vars).

    force_fetch=True passes `--force-fetch` so the importer ignores any
    previously-patched local bundle and re-downloads from S3."""
    if from_env not in CONFIG_SYNC_ENVS:
        msg = f"unknown env '{from_env}'. Available: {list(CONFIG_SYNC_ENVS)}"
        with _config_sync_lock:
            _config_sync_state["error"] = msg
        return
    with _config_sync_lock:
        if _config_sync_state["running"]:
            return
        _config_sync_state.update({
            "running": True, "from": from_env,
            "started_at": time.time(), "finished_at": None,
            "exit_code": None, "error": None, "log": [],
        })

    def _append_log(line: str):
        with _config_sync_lock:
            _config_sync_state["log"].append(line)
            if len(_config_sync_state["log"]) > CONFIG_SYNC_MAX_LOG_LINES:
                del _config_sync_state["log"][:-CONFIG_SYNC_MAX_LOG_LINES]

    try:
        # Sequence (per design):
        #   1. import prod data (config_transfer.py import --skip-feature-migrations)
        #   2. apply Backend/dev/local-testing-data/*.sql
        #   3. run dev/feature-migrations/*.sql  (config_transfer.py import --only-feature-migrations)
        #   4. restart rider/driver/mock-registry so they re-read boot-time config
        #
        # Why this order:
        #   - prod import populates merchants, vehicle configs, etc.
        #   - local-testing-data adds fixed-UUID test persons (juspay_admin etc.)
        #     that feature-migrations like 0001-dashboard-access-setup.sql reference
        #     by hard-coded UUID. Without this seed, feature-migrations FK-fail.
        #   - feature-migrations sit on top of both layers.

        def _run(label, cmd_args):
            full = [sys.executable, "-u", "config_transfer.py"] + cmd_args
            _append_log(f"$ {' '.join(full)}  (cwd={CONFIG_SYNC_DIR})  [{label}]")
            p = subprocess.Popen(
                full, cwd=str(CONFIG_SYNC_DIR),
                stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                text=True, bufsize=1,
                env={**os.environ, "PYTHONUNBUFFERED": "1",
                     "CONFIG_SYNC_LOCAL_DB_PORT": str(_svc_port("db-primary", 5434))},
            )
            for line in p.stdout:
                line = line.rstrip()
                print(f"  \033[96m[config-sync {from_env} {label}]\033[0m {line}")
                _append_log(line)
            p.wait()
            return p.returncode

        # Step 1: prod import (no feature-migrations yet).
        import_args = [
            "import", "--from", from_env, "--to", "local",
            "--fetch",
            "--skip-feature-migrations",
        ]
        if force_fetch:
            import_args.append("--force-fetch")
        rc = _run("import", import_args)
        with _config_sync_lock:
            _config_sync_state["exit_code"] = rc
        if rc != 0:
            with _config_sync_lock:
                _config_sync_state["error"] = f"config_transfer.py import exited with {rc}"
            return

        # Step 2: seed test persons / tokens / etc.
        _apply_local_testing_data()
        _append_log("Applied Backend/dev/local-testing-data/*.sql.")

        # Step 3: feature-migrations on top of imported + seeded state.
        rc = _run("feature-migrations", [
            "import", "--from", from_env, "--to", "local",
            "--only-feature-migrations",
        ])
        with _config_sync_lock:
            _config_sync_state["exit_code"] = rc
        if rc != 0:
            with _config_sync_lock:
                _config_sync_state["error"] = f"feature-migrations exited with {rc}"
            return

        # Step 4: restart so haskell services see the new config.
        if restart_services:
            _restart_haskell_services()
            _append_log("Restarted rider/driver/mock-registry to pick up synced config.")

        # Step 5: persist marker + update in-memory last_synced so future runs
        # from same env can skip the whole pipeline. Marker lives at
        # <repo-root>/data/config-sync/metadata.json — survives context-api
        # restarts; the in-memory copy is what /api/config-sync/status returns.
        synced_at = time.time()
        try:
            CONFIG_SYNC_MARKER_PATH.parent.mkdir(parents=True, exist_ok=True)
            tmp = CONFIG_SYNC_MARKER_PATH.with_suffix(".json.tmp")
            with tmp.open("w") as f:
                json.dump({
                    "from": from_env,
                    "to": "local",
                    "synced_at": synced_at,
                    "source": "context-api/run_config_sync",
                    "force_fetch": bool(force_fetch),
                }, f, indent=2, sort_keys=True)
            tmp.replace(CONFIG_SYNC_MARKER_PATH)
            _append_log(f"Wrote sync marker: {CONFIG_SYNC_MARKER_PATH}")
        except OSError as e:
            _append_log(f"WARN: failed to write sync marker: {e}")
        with _config_sync_lock:
            _config_sync_state["last_synced"] = {"from": from_env, "finished_at": synced_at}
    except Exception as e:
        with _config_sync_lock:
            _config_sync_state["error"] = str(e)
        _append_log(f"ERROR: {e}")
    finally:
        with _config_sync_lock:
            _config_sync_state["running"] = False
            _config_sync_state["finished_at"] = time.time()


def trigger_config_sync(from_env: str, force_fetch: bool = False):
    """Kick off a config-sync in a daemon thread. Returns True if accepted, False if one is already running."""
    with _config_sync_lock:
        if _config_sync_state["running"]:
            return False
    threading.Thread(
        target=run_config_sync,
        kwargs={"from_env": from_env, "force_fetch": force_fetch},
        daemon=True,
    ).start()
    return True


# ── Control-center launcher ──
# Drives Backend/dev/test-tool/context-api/setup/control_center/frontend/setup.sh —
# clones the control-center repo on first launch, runs npm install, then
# `npm run dev`. The dashboard polls /api/control-center/status (mirrors
# config-sync polling) and surfaces a Launch / Check Status / Open
# Control Center button.
CONTROL_CENTER_SETUP_SCRIPT = (
    PROJECT_ROOT / "Backend" / "dev" / "test-tool" / "context-api"
    / "setup" / "control_center" / "frontend" / "setup.sh"
)
CONTROL_CENTER_URL = os.environ.get("CONTROL_CENTER_URL", "http://localhost:5173")
CONTROL_CENTER_MAX_LOG_LINES = 4000

_control_center_state = {
    "running": False,        # the setup script process is alive
    "ready": False,           # vite is serving on CONTROL_CENTER_URL
    "url": CONTROL_CENTER_URL,
    "started_at": None,
    "finished_at": None,
    "exit_code": None,
    "error": None,
    "log": [],
    "pid": None,
}
_control_center_lock = threading.Lock()
_control_center_proc = None  # type: ignore[var-annotated]


# ── UI state (dropdowns / form selections) ────────────────────────
# In-memory bag of UI selections (sync source env, control-center ref,
# per-app native-launcher platform + variant, …) so that dashboard reloads
# don't lose what the user just picked. Survives across browser refreshes
# but NOT across test-context-api restarts (it's a deliberately ephemeral
# session cache; on a fresh server boot the launcher's own defaults take
# over again). Process-launch state lives in the dedicated *_state dicts
# above — this is purely cosmetic UI choices.
#
# Schema is open (any JSON-serialisable value) so the dashboard can extend
# it without server-side changes; the GET endpoint just dumps the dict and
# the PUT endpoint merges. Don't store secrets here — the file/socket has
# no auth and any process on the host can read it.
_ui_state: dict = {}
_ui_state_lock = threading.Lock()


def _probe_control_center_ready() -> bool:
    """Best-effort liveness probe of vite dev server. Any 2xx/3xx/4xx counts as
    'something HTTP is up' — vite returns 200 on /, 404 only on bogus paths."""
    try:
        req = urllib.request.Request(CONTROL_CENTER_URL, method="GET")
        with urllib.request.urlopen(req, timeout=1) as r:
            return 200 <= r.status < 500
    except urllib.error.HTTPError as e:
        return 200 <= e.code < 500
    except Exception:
        return False


def run_control_center_setup(ref: str | None = None):
    """Run control-center-setup.sh, streaming its stdout into _control_center_state['log'].
    Sets ready=True once vite is reachable; the process keeps running until torn down.

    ``ref`` is an optional git ref (branch name or commit SHA) — if set,
    the setup script checks that out before ``npm install`` / vite."""
    global _control_center_proc

    with _control_center_lock:
        if _control_center_state["running"]:
            return
        _control_center_state.update({
            "running": True, "ready": False,
            "started_at": time.time(), "finished_at": None,
            "exit_code": None, "error": None, "log": [], "pid": None,
        })

    def _append_log(line: str):
        with _control_center_lock:
            _control_center_state["log"].append(line)
            if len(_control_center_state["log"]) > CONTROL_CENTER_MAX_LOG_LINES:
                del _control_center_state["log"][:-CONTROL_CENTER_MAX_LOG_LINES]

    try:
        if not CONTROL_CENTER_SETUP_SCRIPT.is_file():
            raise FileNotFoundError(f"setup script not found: {CONTROL_CENTER_SETUP_SCRIPT}")

        ref_str = f" CC_REF={ref}" if ref else ""
        _append_log(f"${ref_str} bash {CONTROL_CENTER_SETUP_SCRIPT}")
        env_vars = {**os.environ, "PYTHONUNBUFFERED": "1"}
        if ref:
            env_vars["CC_REF"] = ref
        p = subprocess.Popen(
            ["bash", str(CONTROL_CENTER_SETUP_SCRIPT)],
            cwd=str(PROJECT_ROOT),
            stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
            text=True, bufsize=1,
            env=env_vars,
            # New session so /api/control-center/stop can kill the whole
            # process tree (script + npm + vite) via os.killpg, not just
            # the bash wrapper.
            start_new_session=True,
        )
        _control_center_proc = p
        with _control_center_lock:
            _control_center_state["pid"] = p.pid

        # Probe vite readiness in a parallel thread while we drain the log.
        def _ready_watcher():
            while True:
                with _control_center_lock:
                    if not _control_center_state["running"]:
                        return
                if _probe_control_center_ready():
                    with _control_center_lock:
                        if not _control_center_state["ready"]:
                            _control_center_state["ready"] = True
                            _append_log(f"control-center: ready at {CONTROL_CENTER_URL}")
                    return
                time.sleep(2)
        threading.Thread(target=_ready_watcher, daemon=True).start()

        for line in p.stdout:
            line = line.rstrip()
            print(f"  \033[95m[control-center]\033[0m {line}")
            _append_log(line)
        rc = p.wait()
        with _control_center_lock:
            _control_center_state["exit_code"] = rc
            if rc != 0:
                _control_center_state["error"] = f"control-center-setup.sh exited with {rc}"
    except Exception as e:
        with _control_center_lock:
            _control_center_state["error"] = str(e)
        _append_log(f"ERROR: {e}")
    finally:
        with _control_center_lock:
            _control_center_state["running"] = False
            _control_center_state["ready"] = False
            _control_center_state["finished_at"] = time.time()
        _control_center_proc = None


def trigger_control_center(ref: str | None = None):
    """Kick off control-center setup in a daemon thread. Returns True if accepted,
    False if one is already running."""
    with _control_center_lock:
        if _control_center_state["running"]:
            return False
    # Reap leftover vite / npm / setup.sh from a prior run that crashed
    # before its teardown trap fired. See _cleanup_stale_control_center
    # — defined further down in the file alongside the ny-rn helper.
    _cleanup_stale_control_center()
    threading.Thread(target=run_control_center_setup, args=(ref,), daemon=True).start()
    return True


# ── ny-react-native launcher ──
# Drives Backend/dev/test-tool/context-api/setup/ny_react_native/setup.sh —
# clones the nammayatri/ny-react-native repo, installs deps, builds the
# requested apps (consumer / provider / both) for android/ios, boots an
# emulator if none is running, and installs+launches via Metro on 8088
# (and 8089 for "both"). Metro defaults dodge every reservation in
# Backend/nix/services/ports.nix — see NY_RN_REVERSE_PORTS below.
NY_RN_SETUP_SCRIPT = (
    PROJECT_ROOT / "Backend" / "dev" / "test-tool" / "context-api"
    / "setup" / "ny_react_native" / "setup.sh"
)
NY_RN_VALID_APPS = {"customer", "driver"}


def _resolve_adb_bin() -> str:
    """Pick the right `adb` binary for this host. Prefer the Android SDK
    one the user actually launched the emulator with — that adb spawns the
    server emulator-5554 is registered with. nix-shell's adb (different
    binary, possibly different version) starts its OWN daemon on 5037 and
    sees no devices, which is why the logcat tabs end up empty.

    Resolution order:
      1. $ANDROID_HOME/platform-tools/adb    (most reliable)
      2. ~/Library/Android/sdk/platform-tools/adb (macOS Android Studio default)
      3. ~/Android/Sdk/platform-tools/adb    (Linux Android Studio default)
      4. plain `adb` from PATH (last resort)
    """
    candidates = []
    sdk = os.environ.get("ANDROID_HOME") or os.environ.get("ANDROID_SDK_ROOT")
    if sdk:
        candidates.append(os.path.join(sdk, "platform-tools", "adb"))
    candidates.append(os.path.expanduser("~/Library/Android/sdk/platform-tools/adb"))
    candidates.append(os.path.expanduser("~/Android/Sdk/platform-tools/adb"))
    for c in candidates:
        if os.path.isfile(c) and os.access(c, os.X_OK):
            return c
    return "adb"
NY_RN_VALID_PLATFORMS = {"android", "ios"}
NY_RN_MAX_LOG_LINES = 4000
NY_RN_READY_SENTINEL = "ny-react-native: launched"

# Variant lists are brand/flavor names — productFlavors in android/app/build.gradle
# (consumer + provider). At runtime we try to parse the actual gradle file from
# the cloned repo and fall back to these defaults. The strings are PascalCase
# (consumer) / camelCase (provider) per the upstream convention.
NY_RN_DEFAULT_VARIANTS = {
    "customer": ["Bridge", "NammaYatri", "Yatri", "ManaYatri", "YatriSathi", "Lynx", "BharatTaxi"],
    "driver":   ["nammaYatri", "jatriSaathi", "bridge", "manaYatri", "yatri", "lynx", "bharatTaxi"],
}
NY_RN_DEFAULT_VARIANT_BY_APP = {"customer": "Bridge", "driver": "nammaYatri"}


def _ny_rn_repo_dir() -> Path:
    """Where the launcher script clones-or-pulls the repo. Mirrors the script."""
    override = os.environ.get("NY_RN_PATH")
    if override:
        return Path(override)
    return PROJECT_ROOT / "data" / "ny-react-native"


def _parse_gradle_product_flavors(gradle_text: str) -> list:
    """Best-effort extraction of `productFlavors { foo { … } bar { … } }` flavor
    names from a Groovy build.gradle. Returns an empty list on any parse hiccup
    so callers can fall back to the hardcoded default list.

    Multi-dimension flavors (e.g. env: dev/prod + brand: bridge/lynx) confuse a
    naive scan because we get a flat list mixing both dimensions. The launcher
    only cares about the BRAND dimension — combining a brand with an env+
    buildType yields the actual gradle variant (e.g. `lynxDevDebug`). We filter
    out names that look like environments so the dropdown surfaces brands only.
    Picking an env from the dropdown produces a non-existent gradle task
    (e.g. `installProdDevDebug`)."""
    import re
    m = re.search(r"productFlavors\s*\{", gradle_text)
    if not m:
        return []
    # Walk braces from the match to find the matching close.
    start = m.end()
    depth = 1
    i = start
    while i < len(gradle_text) and depth > 0:
        c = gradle_text[i]
        if c == "{":
            depth += 1
        elif c == "}":
            depth -= 1
        i += 1
    block = gradle_text[start:i - 1]

    dsl_helpers = {"manifestPlaceholders", "resValue", "buildConfigField", "signingConfig"}
    # Names that look like env-dimension flavors (build environment, not brand).
    # Combining one of these with `DevDebug` or similar produces a non-existent
    # gradle variant. Case-insensitive match.
    env_names = {
        "dev", "prod", "staging", "production", "master", "qa", "test",
        "internal", "stage", "uat", "preprod", "alpha", "beta",
    }

    flavors = []
    for fm in re.finditer(r"(?m)^\s*([A-Za-z_][A-Za-z0-9_]*)\s*\{", block):
        name = fm.group(1)
        if name in dsl_helpers:
            continue
        if name.lower() in env_names:
            continue
        flavors.append(name)
    return flavors


def _ny_rn_detect_variants(app: str) -> list:
    """If the repo is cloned, parse <app>/android/app/build.gradle for flavor
    names. Otherwise return the hardcoded default list."""
    sub = "consumer" if app == "customer" else "provider"
    gradle = _ny_rn_repo_dir() / sub / "android" / "app" / "build.gradle"
    if gradle.is_file():
        try:
            flavors = _parse_gradle_product_flavors(gradle.read_text())
            if flavors:
                return flavors
        except Exception:
            pass
    return list(NY_RN_DEFAULT_VARIANTS[app])

# Per-app launcher state — customer and driver run independently, each
# with its own Metros, gradle build, log buffer, and lifecycle. They share
# only the on-disk repo clone and the emulator.
def _make_ny_rn_state():
    return {
        "running": False,
        "ready": False,
        "app": None,
        "platform": None,
        "variant": None,
        "apps_built": [],
        "started_at": None,
        "finished_at": None,
        "exit_code": None,
        "error": None,
        "log": [],
        "pid": None,
    }


_ny_rn_states = {a: _make_ny_rn_state() for a in NY_RN_VALID_APPS}
_ny_rn_locks = {a: threading.Lock() for a in NY_RN_VALID_APPS}
_ny_rn_procs: dict = {a: None for a in NY_RN_VALID_APPS}


# ── Git ref discovery ───────────────────────────────────────────────────
# Used by both /api/ny-react-native/refs and /api/control-center/refs to
# populate the dropdown of branches + recent commits. Reads the local
# checkout (if cloned), then enriches with remote branch list via `gh`
# if available. Caller may pass ?q= for substring filtering.
def _git_repo_refs(repo_path: Path, github_repo: str | None = None,
                   q: str | None = None, branch_limit: int = 80,
                   commit_limit: int = 30) -> dict:
    """Return a dict describing the available refs for a repo. Shape:
        {
          "cloned": bool,
          "default_branch": "main",
          "current": {"branch": "...", "sha": "...", "subject": "...", "date": "..."},
          "branches": [ {name, sha, subject?, date?}, ... ],
          "commits":  [ {sha, subject, author, date}, ... ],
        }
    All keys are always present; lists may be empty when not cloned."""
    out: dict = {
        "cloned": False,
        "default_branch": "main",
        "current": None,
        "branches": [],
        "commits": [],
    }
    needle = (q or "").strip().lower() or None

    def _matches(s: str) -> bool:
        return (needle is None) or (needle in s.lower())

    # Local data — only available if cloned.
    if (repo_path / ".git").is_dir():
        out["cloned"] = True
        # Current branch + HEAD commit metadata.
        head_cmd = subprocess.run(
            ["git", "-C", str(repo_path), "rev-parse", "--abbrev-ref", "HEAD"],
            capture_output=True, text=True, timeout=4,
        )
        if head_cmd.returncode == 0:
            cur_branch = (head_cmd.stdout or "").strip()
            log_cmd = subprocess.run(
                ["git", "-C", str(repo_path), "log", "-1",
                 "--pretty=%H%x09%s%x09%cI", "HEAD"],
                capture_output=True, text=True, timeout=4,
            )
            cur_sha = cur_subj = cur_date = ""
            if log_cmd.returncode == 0 and log_cmd.stdout.strip():
                parts = log_cmd.stdout.rstrip("\n").split("\t")
                if len(parts) >= 3:
                    cur_sha, cur_subj, cur_date = parts[0], parts[1], parts[2]
            out["current"] = {
                "branch": cur_branch,
                "sha": cur_sha,
                "subject": cur_subj,
                "date": cur_date,
            }

        # Recent commits on the current branch — used to populate the
        # commit search list. Cheap and always available.
        log_recent = subprocess.run(
            ["git", "-C", str(repo_path), "log", f"-{commit_limit}",
             "--pretty=%H%x09%s%x09%an%x09%cI", "HEAD"],
            capture_output=True, text=True, timeout=6,
        )
        if log_recent.returncode == 0:
            for line in (log_recent.stdout or "").splitlines():
                p = line.split("\t")
                if len(p) >= 4:
                    sha, subj, author, date = p[0], p[1], p[2], p[3]
                    if _matches(sha) or _matches(subj) or _matches(author):
                        out["commits"].append({
                            "sha": sha, "subject": subj,
                            "author": author, "date": date,
                        })

        # Local branch list (refs that exist on disk).
        br_cmd = subprocess.run(
            ["git", "-C", str(repo_path), "for-each-ref",
             "--format=%(refname:short)%09%(objectname)%09%(committerdate:iso8601-strict)",
             "refs/heads/", "refs/remotes/origin/"],
            capture_output=True, text=True, timeout=6,
        )
        seen_names: set[str] = set()
        if br_cmd.returncode == 0:
            for line in (br_cmd.stdout or "").splitlines():
                p = line.split("\t")
                if len(p) < 2:
                    continue
                name = p[0]
                # Strip the redundant `origin/` prefix from remote-tracking
                # refs so the UI shows e.g. `feature/x` not `origin/feature/x`.
                if name.startswith("origin/"):
                    name = name[len("origin/"):]
                if name == "HEAD" or name in seen_names:
                    continue
                if not _matches(name):
                    continue
                seen_names.add(name)
                out["branches"].append({
                    "name": name,
                    "sha": p[1],
                    "date": p[2] if len(p) > 2 else "",
                })

    # Optional `gh` enrichment for the remote branch list. Only fires if
    # `gh` is on PATH AND the caller named the github repo. We don't fail
    # the request if gh is missing or unauthenticated — just skip.
    if github_repo and shutil.which("gh"):
        try:
            gh = subprocess.run(
                ["gh", "api", f"repos/{github_repo}",
                 "--jq", ".default_branch"],
                capture_output=True, text=True, timeout=8,
            )
            if gh.returncode == 0 and gh.stdout.strip():
                out["default_branch"] = gh.stdout.strip()
        except Exception:
            pass
        try:
            gh = subprocess.run(
                ["gh", "api", f"repos/{github_repo}/branches",
                 "--paginate", "--jq",
                 ".[] | {name: .name, sha: .commit.sha}"],
                capture_output=True, text=True, timeout=20,
            )
            if gh.returncode == 0:
                existing = {b["name"] for b in out["branches"]}
                # gh emits newline-delimited JSON when paginated.
                for raw_line in (gh.stdout or "").splitlines():
                    raw_line = raw_line.strip()
                    if not raw_line:
                        continue
                    try:
                        b = json.loads(raw_line)
                    except json.JSONDecodeError:
                        continue
                    name = b.get("name", "")
                    if not name or name in existing:
                        continue
                    if not _matches(name):
                        continue
                    out["branches"].append({
                        "name": name, "sha": b.get("sha", ""), "date": "",
                    })
                    if len(out["branches"]) >= branch_limit:
                        break
        except Exception:
            pass

    out["branches"] = out["branches"][:branch_limit]
    return out

# adb-reverse heartbeat ─── adbd drops `adb reverse` mappings whenever the
# device transitions (the emulator app close/reopen lifecycle is enough to
# trigger it). Without these mappings the app's `localhost:8013` requests
# fail with `Network request failed` even though MMKV holds the right URL
# and rider-app is healthy on the host. While a launcher is running/ready
# we periodically re-apply the reverses; idempotent and ~3 ms each.
_ny_rn_reverse_events: dict = {a: None for a in NY_RN_VALID_APPS}
_ny_rn_reverse_threads: dict = {a: None for a in NY_RN_VALID_APPS}
NY_RN_REVERSE_PORTS = [
    8013,   # rider-app HTTP (BAP)
    8016,   # dynamic-offer-driver-app HTTP (BPP)
    # Metro defaults: 8088 (customer) / 8089 (driver). Picked
    # specifically to dodge every reservation in
    # Backend/nix/services/ports.nix — in particular 8081 (LTS),
    # 8082 (kept as buffer), 8085 (nginx), 8091 (mock-payment).
    # find_free_port walks UP from these so spillover lands on
    # equally-unreserved ports.
    8088,
    8089,
    50051,  # notification-service gRPC — provider Android driver app
            # has its grpcAddress patched to localhost:50051 by
            # setup/ny_react_native/common/grpc_local_url.py.
]
NY_RN_REVERSE_INTERVAL_S = 5.0


def _apply_adb_reverses(ports):
    """Re-apply `adb reverse tcp:N tcp:N` for every port in `ports`, against
    every connected emulator. Returns the list of (port, ok) tuples for the
    *first* device — heartbeat callers only need a coarse health signal.
    Silently swallows adb errors (the daemon may be mid-restart)."""
    adb_bin = _resolve_adb_bin()
    try:
        out = subprocess.run([adb_bin, "devices"], capture_output=True, text=True, timeout=3).stdout
    except Exception:
        return []
    serials = []
    for line in out.splitlines()[1:]:
        line = line.strip()
        if not line or "\tdevice" not in line:
            continue
        serials.append(line.split("\t", 1)[0])
    if not serials:
        return []
    results = []
    for serial in serials:
        for p in ports:
            try:
                rc = subprocess.run(
                    [adb_bin, "-s", serial, "reverse", f"tcp:{p}", f"tcp:{p}"],
                    capture_output=True, text=True, timeout=3,
                ).returncode
                if serial == serials[0]:
                    results.append((p, rc == 0))
            except Exception:
                if serial == serials[0]:
                    results.append((p, False))
    return results


def _reverse_heartbeat_loop(app: str, stop_event):
    """Daemon thread body — re-applies adb reverses every NY_RN_REVERSE_INTERVAL_S
    until stop_event is set. Logs a single line on first apply and on any
    failure transition (success→fail or fail→success), to keep the per-app
    log readable."""
    state = _ny_rn_states.get(app)
    lock = _ny_rn_locks.get(app)
    if state is None or lock is None:
        return
    last_ok = None
    while not stop_event.is_set():
        results = _apply_adb_reverses(NY_RN_REVERSE_PORTS)
        ok = bool(results) and all(r for _, r in results)
        if last_ok is None or ok != last_ok:
            mapped = ",".join(str(p) for p, r in results if r) or "(none)"
            msg = f"reverse-heartbeat: applied for {app} ports={mapped} ok={ok}"
            print(f"  \033[94m[ny-react-native:{app}]\033[0m {msg}")
            with lock:
                state["log"].append(msg)
                if len(state["log"]) > NY_RN_MAX_LOG_LINES:
                    del state["log"][:-NY_RN_MAX_LOG_LINES]
            last_ok = ok
        stop_event.wait(NY_RN_REVERSE_INTERVAL_S)


def _start_reverse_heartbeat(app: str):
    """Idempotent — if a heartbeat is already running for this app, leave
    it alone. Otherwise spin up a fresh daemon thread."""
    if app not in _ny_rn_states:
        return
    existing = _ny_rn_reverse_threads.get(app)
    if existing is not None and existing.is_alive():
        return
    ev = threading.Event()
    _ny_rn_reverse_events[app] = ev
    t = threading.Thread(target=_reverse_heartbeat_loop, args=(app, ev), daemon=True)
    _ny_rn_reverse_threads[app] = t
    t.start()


def _stop_reverse_heartbeat(app: str):
    """Signal the heartbeat thread to exit; do NOT join (we don't want
    request handlers to block waiting for the next 5-s tick)."""
    ev = _ny_rn_reverse_events.get(app)
    if ev is not None:
        ev.set()
    _ny_rn_reverse_events[app] = None
    _ny_rn_reverse_threads[app] = None


def _probe_ny_rn_ready(app: str) -> bool:
    """Look for the script's sentinel last log line in the per-app log."""
    lock = _ny_rn_locks.get(app)
    state = _ny_rn_states.get(app)
    if lock is None or state is None:
        return False
    with lock:
        for line in reversed(state["log"][-50:]):
            if NY_RN_READY_SENTINEL in line:
                return True
    return False


def run_ny_rn_setup(app: str, platform: str, variant: str, ref: str | None = None,
                    firebase_override_path: str | None = None):
    """Run ny-react-native-setup.sh for a given app (customer or driver),
    streaming stdout into that app's state log. The script `exec`s into a
    long-running `wait` once Metros are up, so we only declare 'done'
    (running=False) when the user tears it down or the process exits."""
    if app not in _ny_rn_states:
        return
    lock = _ny_rn_locks[app]
    state = _ny_rn_states[app]

    with lock:
        if state["running"]:
            return
        apps_built = ["consumer"] if app == "customer" else ["provider"]
        state.update({
            "running": True, "ready": False,
            "app": app, "platform": platform, "variant": variant,
            "apps_built": apps_built,
            "started_at": time.time(), "finished_at": None,
            "exit_code": None, "error": None, "log": [], "pid": None,
        })

    def _append_log(line: str):
        with lock:
            state["log"].append(line)
            if len(state["log"]) > NY_RN_MAX_LOG_LINES:
                del state["log"][:-NY_RN_MAX_LOG_LINES]

    try:
        if not NY_RN_SETUP_SCRIPT.is_file():
            raise FileNotFoundError(f"setup script not found: {NY_RN_SETUP_SCRIPT}")

        ref_str = f" NY_RN_REF={ref}" if ref else ""
        fb_str = f" NY_RN_FIREBASE_OVERRIDE={firebase_override_path}" if firebase_override_path else ""
        _append_log(f"$ NY_RN_APP={app} NY_RN_PLATFORM={platform} NY_RN_VARIANT={variant}{ref_str}{fb_str} bash {NY_RN_SETUP_SCRIPT}")
        env_vars = {
            **os.environ,
            "NY_RN_APP": app,
            "NY_RN_PLATFORM": platform,
            "NY_RN_VARIANT": variant,
            "PYTHONUNBUFFERED": "1",
        }
        if ref:
            env_vars["NY_RN_REF"] = ref
        if firebase_override_path:
            env_vars["NY_RN_FIREBASE_OVERRIDE"] = firebase_override_path
        p = subprocess.Popen(
            ["bash", str(NY_RN_SETUP_SCRIPT)],
            cwd=str(PROJECT_ROOT),
            stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
            text=True, bufsize=1,
            env=env_vars,
            # New session so /api/ny-react-native/stop can kill the whole
            # process tree (gradle + Metros + adb invocations) via
            # os.killpg, not just the bash wrapper.
            start_new_session=True,
        )
        _ny_rn_procs[app] = p
        with lock:
            state["pid"] = p.pid

        for line in p.stdout:
            line = line.rstrip()
            print(f"  \033[94m[ny-react-native:{app}]\033[0m {line}")
            _append_log(line)
            if NY_RN_READY_SENTINEL in line:
                with lock:
                    state["ready"] = True
                # The setup script applies reverses once at boot; from here
                # on the heartbeat keeps them alive across emulator app
                # close/reopen (which silently wipes the reverse table).
                _start_reverse_heartbeat(app)
        rc = p.wait()
        with lock:
            state["exit_code"] = rc
            if rc != 0:
                state["error"] = f"ny-react-native-setup.sh exited with {rc}"
    except Exception as e:
        with lock:
            state["error"] = str(e)
        _append_log(f"ERROR: {e}")
    finally:
        _stop_reverse_heartbeat(app)
        with lock:
            state["running"] = False
            state["ready"] = False
            state["finished_at"] = time.time()
        _ny_rn_procs[app] = None


# ── Pre-launch stale-process cleanup ───────────────────────────────────
# When a previous run crashed mid-build, was killed externally, or the
# bash teardown trap missed something, stale processes can hang on:
#   - Metro on 8088/8089/etc. (would force the next run to walk to a
#     higher free port; sometimes the stale Metro keeps serving an old
#     bundle ID and confuses the simulator).
#   - xcodebuild for the workspace we're about to rebuild (would
#     contend on DerivedData / SPM cache and stall both builds).
#   - The bash launcher PID we previously tracked but never reaped.
#
# These helpers run BEFORE every trigger_*. They are best-effort and
# never raise — failing to kill a stale process is not a launch blocker;
# at worst the new run finds a busy port and walks to the next.

def _pkill_pattern(pattern: str) -> int:
    """SIGTERM every process whose argv matches ``pattern``, then SIGKILL
    any stragglers ~600 ms later. Returns the count seen on the first
    pass (best-effort; never raises)."""
    try:
        p = subprocess.run(
            ["pgrep", "-f", pattern],
            capture_output=True, text=True, timeout=3,
        )
        pids = [int(x) for x in (p.stdout or "").split() if x.strip().isdigit()]
        for pid in pids:
            try:
                os.kill(pid, signal.SIGTERM)
            except Exception:
                pass
        if pids:
            time.sleep(0.6)
            p2 = subprocess.run(
                ["pgrep", "-f", pattern],
                capture_output=True, text=True, timeout=3,
            )
            still = [int(x) for x in (p2.stdout or "").split() if x.strip().isdigit()]
            for pid in still:
                try:
                    os.kill(pid, signal.SIGKILL)
                except Exception:
                    pass
        return len(pids)
    except Exception:
        return 0


def _free_tcp_port(port: int) -> int:
    """Kill whatever holds tcp:<port> (best-effort). Returns count of
    processes killed. Safe no-op when nothing's listening."""
    try:
        p = subprocess.run(
            ["lsof", "-ti", f"tcp:{port}"],
            capture_output=True, text=True, timeout=3,
        )
        pids = [int(x) for x in (p.stdout or "").split() if x.strip().isdigit()]
        for pid in pids:
            try:
                os.kill(pid, signal.SIGTERM)
            except Exception:
                pass
        if pids:
            time.sleep(0.4)
            p2 = subprocess.run(
                ["lsof", "-ti", f"tcp:{port}"],
                capture_output=True, text=True, timeout=3,
            )
            still = [int(x) for x in (p2.stdout or "").split() if x.strip().isdigit()]
            for pid in still:
                try:
                    os.kill(pid, signal.SIGKILL)
                except Exception:
                    pass
        return len(pids)
    except Exception:
        return 0


def _cleanup_stale_ny_rn(app: str) -> None:
    """Kill anything that could collide with a fresh ny-react-native
    launcher run for ``app``. Two layers:
      1. The PID we tracked from a prior run (process group kill).
      2. Pattern-matched stragglers — Metro for this sub-app, xcodebuild
         for this workspace, gradle daemons rooted in this checkout.
    """
    sub = "consumer" if app == "customer" else "provider"
    print(f"  [ny-react-native:{app}] pre-launch cleanup of stale processes…")

    # 1. Reap the previously-tracked launcher PID, if any.
    with _ny_rn_locks[app]:
        prev = _ny_rn_procs.get(app)
        prev_pid = _ny_rn_states[app].get("pid")
    if prev is not None:
        try:
            _kill_process_group(prev)
        except Exception:
            pass
    if prev_pid:
        try:
            for d in _list_descendants(prev_pid):
                try:
                    os.kill(d, signal.SIGKILL)
                except Exception:
                    pass
            try:
                os.kill(prev_pid, signal.SIGKILL)
            except Exception:
                pass
        except Exception:
            pass

    # 2. Pattern-matched stragglers. Patterns are scoped to this sub
    #    (consumer/provider) so the OTHER launcher's processes are
    #    untouched — important: customer + driver can run concurrently.
    patterns = [
        # Metro started by the build runner for THIS sub. The log path
        # uniquely identifies it.
        rf"react-native start.*ny-rn-metro-{sub}\.log",
        # Common form: Metro launched with cwd=consumer/ or provider/
        rf"react-native start.*--port.*data/ny-react-native/{sub}",
        # xcodebuild for the per-app workspace
        rf"xcodebuild.*data/ny-react-native/{sub}/ios",
        # Pod install / cocoapods child if it stuck on a previous run
        rf"pod install.*data/ny-react-native/{sub}/ios",
        # gradle daemon scoped to this checkout
        rf"GradleDaemon.*data/ny-react-native/{sub}/android",
    ]
    killed = 0
    for pat in patterns:
        killed += _pkill_pattern(pat)

    # 3. Free the standard Metro port for this sub. Defaults are 8088
    #    (customer) / 8089 (driver) — chosen to dodge every reservation
    #    in Backend/nix/services/ports.nix (LTS=8081, nginx=8085, etc.).
    #    The build runner falls through to find_free_port if the
    #    canonical port is busy, but a stale Metro on the canonical
    #    port misleads users (they expect 8088/8089, see 8090, get confused).
    metro_port = 8088 if sub == "consumer" else 8089
    freed = _free_tcp_port(metro_port)

    print(
        f"  [ny-react-native:{app}] cleanup: killed {killed} pattern-match procs, "
        f"freed {freed} on tcp:{metro_port}"
    )


def _cleanup_stale_control_center() -> None:
    """Kill any leftover control-center / vite process from a previous
    run before kicking off a fresh launcher."""
    print("  [control-center] pre-launch cleanup of stale processes…")
    with _control_center_lock:
        prev = _control_center_proc
        prev_pid = _control_center_state.get("pid")
    if prev is not None:
        try:
            _kill_process_group(prev)
        except Exception:
            pass
    if prev_pid:
        try:
            for d in _list_descendants(prev_pid):
                try:
                    os.kill(d, signal.SIGKILL)
                except Exception:
                    pass
            try:
                os.kill(prev_pid, signal.SIGKILL)
            except Exception:
                pass
        except Exception:
            pass

    patterns = [
        # The bash setup script + node child running vite for the cc dir.
        r"control-center/frontend/setup\.sh",
        r"vite.*data/control-center",
        r"npm.*run dev.*data/control-center",
    ]
    killed = 0
    for pat in patterns:
        killed += _pkill_pattern(pat)

    # Free the cc port (5173 is vite's default; bumped via env if needed).
    freed = _free_tcp_port(5173)
    print(
        f"  [control-center] cleanup: killed {killed} pattern-match procs, "
        f"freed {freed} on tcp:5173"
    )


def trigger_ny_rn(app: str, platform: str, variant: str, ref: str | None = None,
                  firebase_override_path: str | None = None):
    """Kick off ny-react-native setup for a single app. Returns True if
    accepted, False if THAT app is already running (the other app may be
    running concurrently — they are independent).

    If `firebase_override_path` is set, the build runner copies that file
    over the in-repo Firebase config (google-services.json for android,
    ios/<variant>/GoogleService-Info.plist for ios) before the build."""
    if app not in _ny_rn_states:
        return False
    with _ny_rn_locks[app]:
        if _ny_rn_states[app]["running"]:
            return False
    # Reap any leftover Metro / xcodebuild / gradle / launcher bash from
    # a prior run that crashed before its teardown trap fired. Cheap
    # (~150 ms typical) and safe — every kill is best-effort.
    _cleanup_stale_ny_rn(app)
    threading.Thread(
        target=run_ny_rn_setup,
        args=(app, platform, variant, ref, firebase_override_path),
        daemon=True,
    ).start()
    return True


def _list_descendants(pid: int) -> list:
    """Return PIDs of every descendant of `pid` (children, grandchildren,
    …). Uses `pgrep -P` recursively so it works without psutil."""
    out_pids = []
    stack = [pid]
    seen = set()
    while stack:
        p = stack.pop()
        if p in seen:
            continue
        seen.add(p)
        try:
            r = subprocess.run(["pgrep", "-P", str(p)],
                               capture_output=True, text=True, timeout=2)
        except Exception:
            continue
        for ln in (r.stdout or "").strip().splitlines():
            try:
                cpid = int(ln.strip())
            except ValueError:
                continue
            out_pids.append(cpid)
            stack.append(cpid)
    return out_pids


def _kill_process_group(proc) -> bool:
    """Tear down `proc` and EVERY descendant. We try three strategies in
    order so that grandchildren that started their own session/group
    (gradle daemon, npm intermediary, emulator children) still get killed:
      1. SIGTERM the whole process group via os.killpg.
      2. SIGTERM every descendant PID found via `pgrep -P` (recursive).
      3. After a 3s grace, SIGKILL the same set.
    Returns True if a kill was attempted."""
    if proc is None or proc.poll() is not None:
        return False
    import signal as _signal
    pid = proc.pid

    try:
        pgid = os.getpgid(pid)
    except Exception:
        pgid = None

    # Snapshot descendants BEFORE signaling so we have the tree even if
    # the parent dies mid-iteration.
    descendants = _list_descendants(pid)

    def _signal_all(sig):
        if pgid:
            try:
                os.killpg(pgid, sig)
            except Exception:
                pass
        for d in descendants:
            try:
                os.kill(d, sig)
            except Exception:
                pass
        try:
            os.kill(pid, sig)
        except Exception:
            pass

    _signal_all(_signal.SIGTERM)

    # Grace period, then SIGKILL anything still alive.
    for _ in range(30):  # ~3s
        if proc.poll() is not None:
            # Re-walk in case grandchildren survived parent death — they
            # are now orphaned but still need killing.
            survivors = _list_descendants(pid)
            if not survivors:
                return True
        time.sleep(0.1)

    # Refresh descendant list (some may have spawned during the grace)
    descendants = list(set(descendants + _list_descendants(pid)))
    _signal_all(_signal.SIGKILL)
    return True


def stop_control_center() -> bool:
    """Tear down a running control-center launcher. Returns True if a kill
    was issued, False if nothing was running."""
    global _control_center_proc
    with _control_center_lock:
        proc = _control_center_proc
        running = _control_center_state["running"]
    if not running or proc is None:
        return False
    return _kill_process_group(proc)


def stop_ny_rn(app: str) -> bool:
    """Tear down a running per-app ny-react-native launcher (kills the script
    + its backgrounded Metros + any in-flight gradle/adb subprocesses)."""
    if app not in _ny_rn_states:
        return False
    with _ny_rn_locks[app]:
        proc = _ny_rn_procs.get(app)
        running = _ny_rn_states[app]["running"]
    if not running or proc is None:
        return False
    return _kill_process_group(proc)


# Paths match each service's readiness_probe in Backend/nix/services/nammayatri.nix.
# Ports come from the resolved table (dynamic per dev) — NOT the base ports.
HASKELL_SERVICE_HEALTH = [
    ("rider-app", _svc_port("rider-app", 8013), "/v2"),
    ("driver-app", _svc_port("dynamic-offer-driver-app", 8016), "/ui"),
    ("mock-registry", _svc_port("mock-registry", 8020), "/"),
]


def _probe_haskell_services():
    """One-shot probe of each haskell service's HTTP health endpoint. Returns:
        { "ready": bool, "services": [{name, port, path, up, status, error?}] }
    Never sleeps. Safe to call from request handlers."""
    import urllib.request
    import urllib.error

    results = []
    all_ok = True
    for name, port, path in HASKELL_SERVICE_HEALTH:
        url = f"http://127.0.0.1:{port}{path}"
        entry = {"name": name, "port": port, "path": path, "up": False, "status": None}
        try:
            with urllib.request.urlopen(url, timeout=2.0) as resp:
                entry["status"] = resp.status
                entry["up"] = 200 <= resp.status < 400
        except urllib.error.HTTPError as e:
            entry["status"] = e.code
            entry["error"] = f"HTTP {e.code}"
        except (urllib.error.URLError, TimeoutError, OSError) as e:
            entry["error"] = str(e.reason if hasattr(e, "reason") else e)
        if not entry["up"]:
            all_ok = False
        results.append(entry)

    return {"ready": all_ok, "services": results}


def _wait_for_haskell_services(timeout_seconds: int = 600) -> bool:
    """Poll until rider-app, driver-app, and mock-registry are all listening,
    AND the dynamic-offer-driver-app schema has finished migrating.

    Returns True when ready, False on timeout. We use this instead of a
    process-compose `process_healthy` dependency because we want the API
    server itself to be reachable immediately for non-sync endpoints (status,
    log tail, manual trigger) — only the auto-sync waits."""
    deadline = time.time() + timeout_seconds
    last_log = 0
    while time.time() < deadline:
        state = _probe_haskell_services()
        if state["ready"]:
            print("  \033[93m[startup]\033[0m haskell services + schema ready")
            return True

        now = time.time()
        if now - last_log > 10:
            missing = [
                f"{s['name']}:{s['port']}{s['path']}"
                + (f" ({s.get('error') or s.get('status')})" if s.get("error") or s.get("status") else "")
                for s in state["services"] if not s["up"]
            ]
            print(f"  \033[93m[startup]\033[0m waiting for: {', '.join(missing)}")
            last_log = now
        time.sleep(2)
    print(f"  \033[93m[startup]\033[0m timeout ({timeout_seconds}s) waiting for services")
    return False


def run_startup_local_testing_data():
    """On server start, wait for haskell services + schema, then apply
    Backend/dev/local-testing-data/*.sql so the dev DB has the seed
    persons / tokens / drivers / vehicles. Does NOT run config-sync — that
    is now strictly user-triggered via the dashboard's "Sync Data" button.

    Disable with RUN_LOCAL_TESTING_DATA_ON_STARTUP=false."""
    if os.environ.get("RUN_LOCAL_TESTING_DATA_ON_STARTUP", "true").lower() in ("0", "false", "no"):
        print("  \033[93m[startup]\033[0m local-testing-data skipped (RUN_LOCAL_TESTING_DATA_ON_STARTUP=false)")
        return

    def _delayed_apply():
        if not _wait_for_haskell_services():
            print("  \033[93m[startup]\033[0m skipping local-testing-data — services never became ready")
            return
        print("  \033[93m[startup]\033[0m applying local-testing-data seed files")
        try:
            _apply_local_testing_data()
        except Exception as e:
            print(f"  \033[93m[startup]\033[0m local-testing-data failed: {e}")

    threading.Thread(target=_delayed_apply, daemon=True).start()

DB_CONFIG = {
    "host": os.environ.get("DB_HOST", "localhost"),
    "port": int(os.environ["DB_PORT"]) if os.environ.get("DB_PORT") else _svc_port("db-primary", 5434),
    "dbname": os.environ.get("DB_NAME", "atlas_dev"),
    "user": os.environ.get("DB_USER", os.environ.get("USER", "atlas")),
    "password": os.environ.get("DB_PASS", ""),
}


def get_conn():
    import psycopg2
    return psycopg2.connect(**DB_CONFIG)


def query(sql, params=()):
    try:
        conn = get_conn()
        conn.autocommit = True
        cur = conn.cursor()
        cur.execute(sql, params)
        cols = [d[0] for d in cur.description] if cur.description else []
        rows = [dict(zip(cols, r)) for r in cur.fetchall()] if cols else []
        conn.close()
        return rows
    except Exception as e:
        return {"error": str(e)}


# ── /api/db/{update,select} helpers ──────────────────────────────────────────
# Narrow setup/diagnostic surface for integration tests. See allow_list.py for
# which tables/columns are addressable. Hard rules enforced below:
#   - /api/db/update WHERE is exactly one column = the table's PK.
#   - Affected rows must be <= 1.
#   - After a successful UPDATE the cache key
#     `{tableCamel}_{pkCol}_{value}{shard-N}` is DEL'd from cluster Redis.
#   - /api/db/select is read-only; tests must NOT use it for assertions.
# Bearer-token auth via TEST_CONTEXT_API_TOKEN env (optional; unset = dev-open).

import allow_list as _tca_allow_list

_TCA_AUTH_TOKEN = os.environ.get("TEST_CONTEXT_API_TOKEN")
_TCA_KV_SHARDS = int(os.environ.get("KV_SHARDS", "128"))
_TCA_SELECT_CAP = 200
_TCA_VALID_IDENT = re.compile(r"^[a-zA-Z][a-zA-Z0-9]*$")
_TCA_CAMEL_BOUNDARY = re.compile(r"(?<!^)(?=[A-Z])")


def _tca_camel_to_snake(name):
    return _TCA_CAMEL_BOUNDARY.sub("_", name).lower()


def _tca_valid_ident(name):
    return isinstance(name, str) and bool(_TCA_VALID_IDENT.match(name))


def _tca_kv_keys_for_all_shards(table_camel, pk_col_camel, pk_value):
    """All possible cache keys for this PK across shards 0..KV_SHARDS-1."""
    base = f"{table_camel}_{pk_col_camel}_{pk_value}"
    return [f"{base}{{shard-{n}}}" for n in range(_TCA_KV_SHARDS)]


def _tca_del_kv_key_all_shards(table_camel, pk_col_camel, pk_value):
    """DEL the PK-keyed KV entry across every possible shard suffix.

    The Haskell side computes shard = f(uuid) % KV_SHARDS where f is opaque
    here; rather than try to mirror that math, issue DEL for every shard
    variant via a single pipelined redis-cli session. Cluster redirects are
    followed by -c. Returns total keys deleted.
    """
    keys = _tca_kv_keys_for_all_shards(table_camel, pk_col_camel, pk_value)
    cmds = "".join(f"DEL {k}\n" for k in keys)
    r = subprocess.run(
        ["redis-cli", "-c", "-h", "localhost", "-p", "30001"],
        input=cmds, capture_output=True, text=True, timeout=15,
    )
    if r.returncode != 0:
        raise RuntimeError(f"redis-cli DEL pipeline failed: {r.stderr.strip()}")
    total = 0
    for line in r.stdout.splitlines():
        line = line.strip()
        if line.startswith("(integer)"):
            try:
                total += int(line.split()[-1])
            except ValueError:
                pass
        else:
            try:
                total += int(line)
            except ValueError:
                pass
    return total


def _tca_auth_ok(handler):
    if not _TCA_AUTH_TOKEN:
        return True
    return handler.headers.get("Authorization", "") == f"Bearer {_TCA_AUTH_TOKEN}"


def _tca_db_update(body):
    from psycopg2 import sql as psql

    table = body.get("table")
    pk = body.get("primaryKey") or {}
    set_map = body.get("set") or {}

    if not isinstance(table, str):
        return {"error": "table is required"}, 400
    spec = _tca_allow_list.get(table)
    if spec is None:
        return {"error": f"table not allow-listed: {table}"}, 400

    pk_col = pk.get("column")
    pk_val = pk.get("value")
    if pk_col != spec["pk"]:
        return {"error": f"primaryKey.column must be {spec['pk']!r} for table {table!r}"}, 400
    if not isinstance(pk_val, str) or not pk_val:
        return {"error": "primaryKey.value is required (string)"}, 400

    if not isinstance(set_map, dict) or not set_map:
        return {"error": "set must be a non-empty object"}, 400
    for col in set_map.keys():
        if not _tca_valid_ident(col):
            return {"error": f"invalid identifier: {col!r}"}, 400
        if not _tca_allow_list.is_updatable(spec, col):
            return {"error": f"column not updatable for {table!r}: {col}"}, 400

    pg_table = spec.get("pg_table") or _tca_camel_to_snake(table)
    set_parts = [
        psql.SQL("{} = {}").format(psql.Identifier(_tca_camel_to_snake(c)), psql.Placeholder())
        for c in set_map.keys()
    ]
    params = list(set_map.values()) + [pk_val]
    sql_q = psql.SQL("UPDATE {sch}.{tbl} SET {sets} WHERE {pkc} = {ph}").format(
        sch=psql.Identifier(spec["schema"]),
        tbl=psql.Identifier(pg_table),
        sets=psql.SQL(", ").join(set_parts),
        pkc=psql.Identifier(_tca_camel_to_snake(pk_col)),
        ph=psql.Placeholder(),
    )

    try:
        conn = get_conn()
        conn.autocommit = True
        cur = conn.cursor()
        cur.execute(sql_q, params)
        rowcount = cur.rowcount
        cur.close()
        conn.close()
    except Exception as e:
        return {"error": f"db error: {e}"}, 500

    if rowcount > 1:
        return {"error": "update affected more than one row; refusing"}, 500

    cache_deleted = None
    cache_key_prefix = None
    if rowcount == 1:
        kv_table = spec.get("kv_table_name") or table
        cache_key_prefix = f"{kv_table}_{pk_col}_{pk_val}{{shard-*}}"
        try:
            cache_deleted = _tca_del_kv_key_all_shards(kv_table, pk_col, pk_val)
        except Exception as e:
            return {
                "updated": rowcount,
                "cacheKeyPattern": cache_key_prefix,
                "cacheDeleteError": f"failed to clear cache key: {e}",
            }, 500

    return {
        "updated": rowcount,
        "cacheKeyPattern": cache_key_prefix,
        "cacheDeleted": cache_deleted,
    }, 200


def _tca_db_select(body):
    from psycopg2 import sql as psql

    table = body.get("table")
    spec = _tca_allow_list.get(table) if isinstance(table, str) else None
    if spec is None:
        return {"error": f"table not allow-listed: {table}"}, 400

    cols = body.get("columns")
    if cols is None:
        cols = ["*"]
    if not isinstance(cols, list) or not cols:
        return {"error": "columns must be a non-empty array"}, 400
    if cols != ["*"]:
        for c in cols:
            if not _tca_valid_ident(c):
                return {"error": f"invalid identifier: {c!r}"}, 400
            if not _tca_allow_list.is_selectable(spec, c):
                return {"error": f"column not selectable for {table!r}: {c}"}, 400

    pk_block = body.get("primaryKey")
    where_block = body.get("where")
    if (pk_block is None) == (where_block is None):
        return {"error": "exactly one of primaryKey or where is required"}, 400

    if pk_block is not None:
        pk_col = pk_block.get("column")
        pk_val = pk_block.get("value")
        if pk_col != spec["pk"]:
            return {"error": f"primaryKey.column must be {spec['pk']!r}"}, 400
        if not isinstance(pk_val, str) or not pk_val:
            return {"error": "primaryKey.value is required"}, 400
        filter_col, filter_val, limit = pk_col, pk_val, 1
    else:
        filter_col = where_block.get("column")
        filter_val = where_block.get("value")
        if not _tca_valid_ident(filter_col):
            return {"error": "where.column required (valid identifier)"}, 400
        if not _tca_allow_list.is_filter_column(spec, filter_col):
            return {"error": f"column not filterable for {table!r}: {filter_col}"}, 400
        limit = body.get("limit")
        if not isinstance(limit, int) or limit <= 0:
            return {"error": "limit (positive int) required for where-mode"}, 400
        if limit > _TCA_SELECT_CAP:
            return {"error": f"limit exceeds cap {_TCA_SELECT_CAP}"}, 400

    pg_table = spec.get("pg_table") or _tca_camel_to_snake(table)
    if cols == ["*"]:
        select_sql = psql.SQL("*")
    else:
        select_sql = psql.SQL(", ").join(
            psql.SQL("{} AS {}").format(
                psql.Identifier(_tca_camel_to_snake(c)), psql.Identifier(c)
            )
            for c in cols
        )
    sql_q = psql.SQL("SELECT {cols} FROM {sch}.{tbl} WHERE {fc} = {ph} LIMIT {lim}").format(
        cols=select_sql,
        sch=psql.Identifier(spec["schema"]),
        tbl=psql.Identifier(pg_table),
        fc=psql.Identifier(_tca_camel_to_snake(filter_col)),
        ph=psql.Placeholder(),
        lim=psql.Literal(limit),
    )

    try:
        conn = get_conn()
        cur = conn.cursor()
        cur.execute(sql_q, [filter_val])
        col_names = [d[0] for d in cur.description] if cur.description else []
        rows = [dict(zip(col_names, r)) for r in cur.fetchall()]
        cur.close()
        conn.close()
    except Exception as e:
        return {"error": f"db error: {e}"}, 500

    return {"rows": rows, "count": len(rows)}, 200


def get_merchants():
    riders = query("""
        SELECT m.id, m.short_id, m.name, m.online_payment,
               moc.id as city_id, moc.city, moc.country, moc.state
        FROM atlas_app.merchant m
        LEFT JOIN atlas_app.merchant_operating_city moc ON moc.merchant_id = m.id
        ORDER BY m.short_id, moc.city
    """)
    drivers = query("""
        SELECT m.id, m.short_id, m.name,
               moc.id as city_id, moc.city, moc.country, moc.currency
        FROM atlas_driver_offer_bpp.merchant m
        LEFT JOIN atlas_driver_offer_bpp.merchant_operating_city moc ON moc.merchant_id = m.id
        ORDER BY m.short_id, moc.city
    """)
    return {"rider_merchants": riders, "driver_merchants": drivers}


def get_riders():
    return query("""
        SELECT p.id as person_id, p.first_name, p.role,
               m.short_id as merchant, moc.city,
               rt.token, rt.verified
        FROM atlas_app.person p
        JOIN atlas_app.merchant m ON m.id = p.merchant_id
        LEFT JOIN atlas_app.merchant_operating_city moc ON moc.id = p.merchant_operating_city_id
        LEFT JOIN atlas_app.registration_token rt ON rt.entity_id = p.id AND rt.verified = true
        WHERE p.role = 'USER' AND rt.token IS NOT NULL
        ORDER BY m.short_id, p.first_name
    """)


def get_drivers():
    return query("""
        SELECT p.id as person_id, p.first_name, p.role,
               m.short_id as merchant, m.id as merchant_id, moc.city, moc.currency,
               rt.token, rt.verified, v.variant as vehicle_variant
        FROM atlas_driver_offer_bpp.person p
        JOIN atlas_driver_offer_bpp.merchant m ON m.id = p.merchant_id
        LEFT JOIN atlas_driver_offer_bpp.merchant_operating_city moc ON moc.id = p.merchant_operating_city_id
        LEFT JOIN atlas_driver_offer_bpp.registration_token rt ON rt.entity_id = p.id AND rt.verified = true
        LEFT JOIN atlas_driver_offer_bpp.vehicle v ON v.driver_id = p.id
        WHERE p.role = 'DRIVER' AND rt.token IS NOT NULL
        ORDER BY m.short_id, p.first_name
    """)


def get_variants(city_id=None):
    sql = """
        SELECT vst.id, vst.service_tier_type, vst.name, vst.seating_capacity,
               vst.is_air_conditioned, vst.is_enabled, vst.priority,
               vst.allowed_vehicle_variant,
               m.short_id as merchant, moc.city, moc.currency
        FROM atlas_driver_offer_bpp.vehicle_service_tier vst
        JOIN atlas_driver_offer_bpp.merchant m ON m.id = vst.merchant_id
        JOIN atlas_driver_offer_bpp.merchant_operating_city moc ON moc.id = vst.merchant_operating_city_id
        WHERE vst.is_enabled = true
    """
    params = ()
    if city_id:
        sql += " AND vst.merchant_operating_city_id = %s"
        params = (city_id,)
    sql += " ORDER BY m.short_id, vst.priority"
    return query(sql, params)


def get_admin_credentials():
    """Return known admin credentials per merchant for the test dashboard.
    These are created in provider-dashboard seed migrations with known email/password."""
    return {
        "MSIL_PARTNER_LOCAL": {"email": "admin@msil.test", "password": "msil1234"},
        "LYNX_PARTNER_LOCAL": {"email": "admin@lynx.test", "password": "lynx1234"},
    }


# ── Collection Scanner ──

ENV_TYPES = ("Local", "Master")


def _read_env_file(f, env_type):
    try:
        env_data = json.loads(f.read_text())
        vals = {v["key"]: v["value"] for v in env_data.get(
            "values", []) if v.get("enabled", True)}
        env_name = f.name.replace("Local_", "").replace("Master_", "").replace(
            ".postman_environment.json", "")
        return {
            "filename": f.name,
            "envType": env_type,
            "envName": env_name,
            "name": env_data.get("name", env_name),
            "city": vals.get("city", ""),
            "state": vals.get("state", ""),
            "merchant": vals.get("dashboard_merchant_id", ""),
            "bapShortId": vals.get("bap_short_id", ""),
            "origin": {"lat": float(vals.get("origin_lat", 0)), "lon": float(vals.get("origin_lon", 0))},
            "destination": {"lat": float(vals.get("dest_lat", 0)), "lon": float(vals.get("dest_lon", 0))},
            "variables": vals,
        }
    except Exception:
        return None


def scan_collections():
    """Walk integration-tests/collections/ and return metadata for each collection group."""
    result = []
    if not COLLECTIONS_DIR.is_dir():
        return result
    for subdir in sorted(COLLECTIONS_DIR.iterdir()):
        if not subdir.is_dir():
            continue
        group = {
            "directory": subdir.name,
            "envTypes": list(ENV_TYPES),
            "environments": [],
            "suites": [],
        }
        for env_type in ENV_TYPES:
            env_dir = subdir / env_type
            if not env_dir.is_dir():
                continue
            for f in sorted(env_dir.iterdir()):
                if not (f.suffix == ".json" and f.name.endswith(".postman_environment.json")):
                    continue
                env = _read_env_file(f, env_type)
                if env is not None:
                    group["environments"].append(env)
        for f in sorted(subdir.iterdir()):
            if not f.is_file() or f.suffix != ".json":
                continue
            if f.name.endswith(".postman_environment.json"):
                continue
            try:
                col_data = json.loads(f.read_text())
                info = col_data.get("info", {})
                group["suites"].append({
                    "filename": f.name,
                    "name": info.get("name", f.stem),
                    "description": info.get("description", ""),
                    "itemCount": len(col_data.get("item", [])),
                })
            except Exception:
                pass
        if group["environments"] or group["suites"]:
            result.append(group)
    return result


def get_collection_file(directory, filename):
    """Return raw Postman collection JSON."""
    path = COLLECTIONS_DIR / directory / filename
    if path.is_file() and path.suffix == ".json":
        return json.loads(path.read_text())
    return None


# ── Service Log Capture (tail -f based) ──

# Global state: active tail processes keyed by a session token
_tail_sessions = {}  # token -> { svc: { proc, lines } }
_tail_lock = threading.Lock()


def _reader_thread(lines_list, proc):
    """Background thread to read lines from tail -f stdout."""
    try:
        for line in proc.stdout:
            lines_list.append(line)
    except (ValueError, OSError):
        pass  # proc closed


def start_log_tails():
    """Start tail -f for each service log. Returns a session token."""
    import uuid
    token = str(uuid.uuid4())[:8]
    session = {}
    for svc, path in SERVICE_LOGS.items():
        if not path.exists():
            continue
        try:
            proc = subprocess.Popen(
                ["tail", "-n", "0", "-f", str(path)],
                stdout=subprocess.PIPE, stderr=subprocess.DEVNULL,
                text=True, bufsize=1
            )
            lines = []
            t = threading.Thread(target=_reader_thread,
                                 args=(lines, proc), daemon=True)
            t.start()
            session[svc] = {"proc": proc, "lines": lines}
        except OSError:
            pass
    with _tail_lock:
        _tail_sessions[token] = session
    return token


def stop_log_tails(token):
    """Stop the tail processes for `token` and return captured logs immediately.

    Grace/settle for async backend writes is the caller's responsibility now —
    the dashboard's step-capture helper opens logs + mock-hits streams together,
    waits a shared grace after the step's HTTP+test-script completes, then
    closes both. Keeping the wait server-side would race the mock-hits stream
    close in the unified flow.
    """
    with _tail_lock:
        session = _tail_sessions.get(token, {})
    if not session:
        with _tail_lock:
            _tail_sessions.pop(token, None)
        return {}

    # Stop and collect immediately — caller has already waited grace.
    with _tail_lock:
        _tail_sessions.pop(token, None)
    logs = {}
    for svc, entry in session.items():
        proc = entry["proc"]
        proc.terminate()
        try:
            proc.wait(timeout=1)
        except subprocess.TimeoutExpired:
            proc.kill()
        text = "".join(entry["lines"]).strip()
        if text:
            if len(text) > MAX_LOG_DELTA_BYTES:
                text = text[-MAX_LOG_DELTA_BYTES:]
            logs[svc] = text
    return logs


def get_full_context():
    return {
        "merchants": get_merchants(),
        "riders": get_riders(),
        "drivers": get_drivers(),
        "variants": get_variants(),
        "admin_credentials": get_admin_credentials(),
    }


# ── Finance Visualization Queries ──

def get_finance_accounts(schema):
    """Get all finance accounts with balances."""
    return query(f"""
        SELECT a.id, a.account_type, a.counterparty_type, a.counterparty_id,
               a.balance, a.currency, a.status
        FROM {schema}.finance_account a
        ORDER BY a.account_type, a.counterparty_type
    """)


def get_finance_ledger_entries(schema, reference_type=None, reference_id=None, counterparty_id=None, limit=50, offset=0):
    """Get finance ledger entries with optional filters."""
    conditions = []
    params = []
    if reference_type:
        conditions.append("le.reference_type = %s")
        params.append(reference_type)
    if reference_id:
        conditions.append("le.reference_id = %s")
        params.append(reference_id)
    if counterparty_id:
        conditions.append(
            "(fa_from.counterparty_id = %s OR fa_to.counterparty_id = %s)")
        params.extend([counterparty_id, counterparty_id])
    where = (" WHERE " + " AND ".join(conditions)) if conditions else ""
    params.extend([limit, offset])
    return query(f"""
        SELECT le.id, le.reference_type, le.reference_id, le.amount, le.currency,
               le.status, le.entry_type, le.created_at,
               fa_from.account_type as from_account_type, fa_from.counterparty_type as from_counterparty,
               fa_to.account_type as to_account_type, fa_to.counterparty_type as to_counterparty
        FROM {schema}.finance_ledger_entry le
        LEFT JOIN {schema}.finance_account fa_from ON fa_from.id = le.from_account_id
        LEFT JOIN {schema}.finance_account fa_to ON fa_to.id = le.to_account_id
        {where}
        ORDER BY le.created_at DESC
        LIMIT %s OFFSET %s
    """, tuple(params))


def get_finance_invoices(schema, issued_to_id=None, invoice_type=None, limit=20, offset=0):
    """Get finance invoices with optional filters."""
    conditions = []
    params = []
    if issued_to_id:
        conditions.append("fi.issued_to_id = %s")
        params.append(issued_to_id)
    if invoice_type:
        conditions.append("fi.invoice_type = %s")
        params.append(invoice_type)
    where = (" WHERE " + " AND ".join(conditions)) if conditions else ""
    params.extend([limit, offset])
    return query(f"""
        SELECT fi.id, fi.invoice_number, fi.invoice_type, fi.status,
               fi.total_amount, fi.subtotal, fi.currency,
               fi.issued_to_type, fi.issued_to_id, fi.issued_to_name,
               fi.issued_by_name, fi.line_items::text as line_items,
               fi.created_at, fi.tax_breakdown
        FROM {schema}.finance_invoice fi
        {where}
        ORDER BY fi.created_at DESC
        LIMIT %s OFFSET %s
    """, tuple(params))


def get_finance_earnings(schema, counterparty_id=None):
    """Wallet-style earnings summary for BPP drivers, matching the shape
    returned by the driver-app /wallet/transactions endpoint.

    Entries where a DRIVER Asset account is the `to` side are additions
    (credits to the driver's wallet); where DRIVER Asset is the `from` side
    are deductions. If counterparty_id is given, scope to that driver;
    otherwise aggregate platform-wide across every DRIVER wallet.
    """
    # Friendly labels — mirror referenceTypeToItemName in
    # Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/UI/DriverWallet.hs
    LABELS = {
        "BaseRide": "Ride Earnings",
        "Tips": "Tips",
        "TollCharges": "Toll Charges",
        "ParkingCharges": "Parking Charges",
        "Topup": "Wallet Top-up",
        "GSTOnline": "GST (Online)",
        "GSTCash": "GST (Cash)",
        "VATOnline": "VAT (Online)",
        "VATCash": "VAT (Cash)",
        "TDSDeductionOnline": "TDS (Online)",
        "TDSDeductionCash": "TDS (Cash)",
        "TDSDeductionCancellation": "TDS (Cancellation)",
        "Payout": "Withdrawal",
        "AirportCashRecharge": "Airport cash recharge (booth)",
        "AirportEntryFee": "Airport Entry Fee",
        "AirportEntryFeeGST": "Airport Entry Fee GST",
        "CommissionOnline": "Platform Commission (Online)",
        "CommissionCash": "Platform Commission (Cash)",
        "DriverCancellationCharges": "Driver Cancellation Charges",
        "CustomerCancellationCharges": "Customer Cancellation Charges",
        "CustomerCancellationGST": "Customer Cancellation GST",
        "WalletIncentive": "Incentive",
        "D2DReferral": "Referral Bonus",
        "DiscountsOnline": "Discounts (Online)",
        "DiscountsCash": "Discounts (Cash)",
        "RideVatOnDiscount": "VAT on Discount",
        "VATAbsorbedOnDiscount": "VAT Absorbed on Discount",
        "VATInput": "VAT Input",
        "CancellationVATInput": "Cancellation VAT Input",
    }

    driver_filter = ""
    params: list = []
    if counterparty_id:
        driver_filter = "AND fa.counterparty_id = %s"
        params = [counterparty_id]

    def grouped(direction_col: str):
        # direction_col is either 'to_account_id' (credits/additions)
        # or 'from_account_id' (debits/deductions).
        sql = f"""
            SELECT le.reference_type,
                   COALESCE(SUM(le.amount), 0) AS total,
                   COUNT(*) AS cnt,
                   MAX(le.currency) AS currency
            FROM {schema}.finance_ledger_entry le
            JOIN {schema}.finance_account fa ON fa.id = le.{direction_col}
            WHERE fa.counterparty_type IN ('DRIVER', 'FLEET_OWNER')
              AND fa.account_type IN ('Liability', 'Control')
              AND le.status = 'SETTLED'
              -- Ignore reversals: drop both the reversal entries and the originals they reversed
              AND le.reversal_of IS NULL
              AND le.id NOT IN (
                SELECT rev.reversal_of FROM {schema}.finance_ledger_entry rev
                WHERE rev.reversal_of IS NOT NULL
              )
              {driver_filter}
            GROUP BY le.reference_type
            ORDER BY total DESC
        """
        rows = query(sql, tuple(params))
        return [] if isinstance(rows, dict) else rows

    add_rows = grouped("to_account_id")
    ded_rows = grouped("from_account_id")

    def to_items(rows):
        out = []
        for r in rows:
            amt = float(r["total"]) if r["total"] is not None else 0.0
            out.append({
                "refType": r["reference_type"],
                "itemName": LABELS.get(r["reference_type"], r["reference_type"]),
                "amount": amt,
                "entries": r["cnt"],
                "currency": r.get("currency"),
            })
        return out

    additions = to_items(add_rows)
    deductions = to_items(ded_rows)
    add_total = sum(i["amount"] for i in additions)
    ded_total = sum(i["amount"] for i in deductions)
    currency = (add_rows[0]["currency"] if add_rows
                else (ded_rows[0]["currency"] if ded_rows else None))

    # Wallet balance (real platform-owed-to-driver): Liability only.
    # Cash-earnings tracker: Control account (no real platform obligation,
    # just cumulative memo of direct rider→driver cash).
    balance = None
    cash_earnings_tracked = None
    if counterparty_id:
        bal_rows = query(
            f"""
            SELECT account_type, balance FROM {schema}.finance_account
            WHERE counterparty_type IN ('DRIVER', 'FLEET_OWNER')
              AND counterparty_id = %s
              AND account_type IN ('Liability', 'Control')
            """,
            (counterparty_id,),
        )
        if bal_rows and not isinstance(bal_rows, dict):
            for row in bal_rows:
                if row["account_type"] == "Liability":
                    balance = float(row["balance"])
                elif row["account_type"] == "Control":
                    cash_earnings_tracked = float(row["balance"])

    return {
        "counterpartyId": counterparty_id,
        "balance": balance,
        "cashEarningsTracked": cash_earnings_tracked,
        "currency": currency,
        "additions": {"totalAmount": add_total, "items": additions},
        "deductions": {"totalAmount": ded_total, "items": deductions},
        "net": add_total - ded_total,
    }


def get_finance_reference_types(schema):
    """Get distinct reference types from ledger entries."""
    return query(f"""
        SELECT DISTINCT reference_type FROM {schema}.finance_ledger_entry ORDER BY reference_type
    """)


def get_finance_dashboard(schema):
    """Full finance dashboard data: accounts, timeline grouped by reference_id, summary stats."""
    accounts = query(f"""
        SELECT a.id, a.account_type, a.counterparty_type, a.counterparty_id,
               a.balance, a.currency, a.status,
               COALESCE((SELECT SUM(le.amount) FROM {schema}.finance_ledger_entry le WHERE le.to_account_id = a.id AND le.amount > 0), 0) as total_credits,
               COALESCE((SELECT SUM(ABS(le.amount)) FROM {schema}.finance_ledger_entry le WHERE le.from_account_id = a.id AND le.amount > 0), 0) as total_debits
        FROM {schema}.finance_account a
        ORDER BY a.account_type, a.counterparty_type
    """)

    # Ledger entries with account info, ordered by time
    entries = query(f"""
        SELECT le.id, le.reference_type, le.reference_id, le.amount, le.currency,
               le.status, le.entry_type, le.created_at,
               le.from_account_id, le.to_account_id,
               COALESCE(fa_from.account_type, 'Unknown') as from_type,
               COALESCE(fa_from.counterparty_type, '') as from_counterparty,
               COALESCE(fa_to.account_type, 'Unknown') as to_type,
               COALESCE(fa_to.counterparty_type, '') as to_counterparty
        FROM {schema}.finance_ledger_entry le
        LEFT JOIN {schema}.finance_account fa_from ON fa_from.id = le.from_account_id
        LEFT JOIN {schema}.finance_account fa_to ON fa_to.id = le.to_account_id
        ORDER BY le.created_at DESC
        LIMIT 500
    """)

    # Invoices
    invoices = query(f"""
        SELECT fi.id, fi.invoice_number, fi.invoice_type, fi.status,
               fi.total_amount, fi.subtotal, fi.currency,
               fi.issued_to_type, fi.issued_to_id, fi.issued_to_name,
               fi.issued_by_name, fi.line_items::text as line_items,
               fi.created_at, fi.tax_breakdown
        FROM {schema}.finance_invoice fi
        ORDER BY fi.created_at DESC
        LIMIT 100
    """)

    # Invoice-to-entry links
    links = query(f"""
        SELECT l.invoice_id, l.ledger_entry_id
        FROM {schema}.finance_invoice_ledger_link l
    """)

    if isinstance(entries, dict) and "error" in entries:
        entries = []
    if isinstance(invoices, dict) and "error" in invoices:
        invoices = []
    if isinstance(links, dict) and "error" in links:
        links = []
    if isinstance(accounts, dict) and "error" in accounts:
        accounts = []

    # Build invoice lookup by entry_id AND by reference_id
    entry_to_invoice = {}
    ref_to_invoice = {}
    invoice_map = {inv["id"]: inv for inv in invoices}
    for lnk in links:
        inv = invoice_map.get(lnk["invoice_id"])
        if inv:
            entry_to_invoice[lnk["ledger_entry_id"]] = inv
    # Also map invoices by issued_to_id for fallback matching
    for inv in invoices:
        ref_to_invoice.setdefault(inv.get("issued_to_id"), []).append(inv)

    # Group entries by reference_id
    from collections import OrderedDict
    groups = OrderedDict()
    for le in entries:
        rid = le["reference_id"] or "unknown"
        if rid not in groups:
            groups[rid] = {"reference_id": rid, "entries": [
            ], "primary_type": None, "earliest": le["created_at"], "invoice": None}
        groups[rid]["entries"].append(le)
        if not groups[rid]["primary_type"]:
            groups[rid]["primary_type"] = le["reference_type"]
        if le["created_at"] and (not groups[rid]["earliest"] or le["created_at"] < groups[rid]["earliest"]):
            groups[rid]["earliest"] = le["created_at"]
        # Attach invoice if linked (via entry link or by reference_id match)
        inv = entry_to_invoice.get(le["id"])
        if inv and not groups[rid]["invoice"]:
            groups[rid]["invoice"] = inv

    timeline = list(groups.values())

    # Summary stats
    active_inv = sum(1 for i in invoices if i.get(
        "status") not in ("Voided", "Cancelled"))
    voided_inv = sum(1 for i in invoices if i.get("status") == "Voided")

    return {
        "accounts": accounts,
        "timeline": timeline,
        "summary": {
            "totalEntries": len(entries),
            "totalInvoices": len(invoices),
            "activeInvoices": active_inv,
            "voidedInvoices": voided_inv,
        },
    }


def clear_finance_data(schema):
    """Truncate every finance-module table in the given schema for a clean slate.
    Uses TRUNCATE ... RESTART IDENTITY CASCADE so FK deps are handled and any
    sequences reset. Tables that don't exist in this schema are skipped."""
    # Every table owned by the finance-kernel storage specs. Keep in sync with
    # Backend/lib/finance-kernel/spec/Storage/*.yaml `tableName` values.
    FINANCE_TABLES = [
        "finance_account",
        "finance_audit_entry",
        "finance_current_state",
        "finance_invoice",
        "finance_invoice_ledger_link",
        "finance_ledger_entry",
        "finance_reconciliation_entry",
        "finance_reconciliation_summary",
        "finance_state_transition",
        "direct_tax_transaction",
        "indirect_tax_transaction",
        "pg_payment_settlement_report",
        "pg_payout_settlement_report",
    ]
    try:
        conn = get_conn()
        conn.autocommit = True
        cur = conn.cursor()
        cur.execute(
            """
            SELECT table_name FROM information_schema.tables
            WHERE table_schema = %s AND table_name = ANY(%s)
            """,
            (schema, FINANCE_TABLES),
        )
        present = [r[0] for r in cur.fetchall()]
        if not present:
            conn.close()
            return {"result": "Success", "message": f"No finance tables in {schema}", "tables": []}
        qualified = ", ".join(f"{schema}.{t}" for t in present)
        cur.execute(f"TRUNCATE TABLE {qualified} RESTART IDENTITY CASCADE")
        conn.close()
        return {"result": "Success", "message": f"Truncated finance tables in {schema}", "tables": present}
    except Exception as e:
        return {"error": str(e)}


# ── Code Coverage Tracking ──────────────────────────────────────────────
# Fetches OpenAPI specs from rider/driver, extracts endpoints + enum schemas,
# records test fingerprints, generates coverage reports.

COVERAGE_DATA_DIR = SCRIPT_DIR / "coverage-data"
COVERAGE_DATA_DIR.mkdir(exist_ok=True)
COVERAGE_SPEC_CACHE = COVERAGE_DATA_DIR / "spec-cache.json"
COVERAGE_RUNS_FILE = COVERAGE_DATA_DIR / "runs.json"
_coverage_lock = threading.Lock()


def _fetch_openapi_spec(service_url: str) -> dict | None:
    """Fetch /openapi JSON from a running service."""
    try:
        req = urllib.request.Request(f"{service_url}/openapi", method="GET")
        req.add_header("Accept", "application/json")
        with urllib.request.urlopen(req, timeout=10) as resp:
            return json.loads(resp.read())
    except Exception as e:
        print(f"  \033[91m[Coverage]\033[0m Failed to fetch openapi from {service_url}: {e}")
        return None


def _extract_enum_fields_from_schema(schema: dict, components: dict, depth: int = 0) -> dict:
    """Recursively extract enum fields from an OpenAPI schema object.
    Returns {fieldName: [enumValues]} for all enum properties.
    Merges enum values across oneOf/anyOf variants (e.g. fareProductType across SearchReq variants)."""
    if depth > 8:
        return {}
    enums = {}

    def _merge_enum(field: str, values: list):
        if field in enums:
            existing = set(str(v) for v in enums[field])
            for v in values:
                if str(v) not in existing:
                    enums[field].append(v)
                    existing.add(str(v))
        else:
            enums[field] = list(values)

    # Resolve $ref
    if "$ref" in schema:
        ref_path = schema["$ref"].replace("#/components/schemas/", "")
        schema = components.get("schemas", {}).get(ref_path, {})

    # Handle allOf/oneOf/anyOf — merge enum values across variants
    for combiner in ("allOf", "oneOf", "anyOf"):
        if combiner in schema:
            for sub in schema[combiner]:
                sub_enums = _extract_enum_fields_from_schema(sub, components, depth + 1)
                for field, values in sub_enums.items():
                    _merge_enum(field, values)

    properties = schema.get("properties", {})
    for field_name, field_schema in properties.items():
        # Resolve field $ref
        resolved = field_schema
        if "$ref" in field_schema:
            ref_path = field_schema["$ref"].replace("#/components/schemas/", "")
            resolved = components.get("schemas", {}).get(ref_path, {})

        if "enum" in resolved:
            _merge_enum(field_name, resolved["enum"])
        elif resolved.get("type") == "boolean":
            _merge_enum(field_name, [True, False])
        # Recurse into nested objects to find deeper enums
        elif resolved.get("type") == "object" or "properties" in resolved or "$ref" in resolved:
            sub_enums = _extract_enum_fields_from_schema(resolved, components, depth + 1)
            for sub_field, sub_values in sub_enums.items():
                _merge_enum(f"{field_name}.{sub_field}", sub_values)

    return enums


def _normalize_path_params(path: str) -> str:
    """Normalize path parameters: {anyName} -> {id}, strip query strings."""
    # Strip query string
    path = path.split("?")[0]
    # Replace named path params with generic {id}
    path = re.sub(r'\{[^}]+\}', '{id}', path)
    return path


def _extract_coverage_spec(openapi: dict, service_name: str) -> dict:
    """Extract endpoints and their scenario-defining enum fields from an OpenAPI spec."""
    components = openapi.get("components", {})
    paths = openapi.get("paths", {})
    endpoints = {}

    for path, methods in paths.items():
        for method, operation in methods.items():
            if method in ("parameters", "summary", "description"):
                continue
            method_upper = method.upper()
            normalized = _normalize_path_params(path)
            key = f"{method_upper} {normalized}"
            # Multiple OpenAPI paths may normalize to the same key — merge enum fields
            if key in endpoints:
                existing = endpoints[key]["enumFields"]
                for f, vals in enum_fields.items():
                    if f in existing:
                        existing[f] = list(set(existing[f] + vals))
                    else:
                        existing[f] = vals
                endpoints[key]["scenarios"] = [f"{f}:{v}" for f, vals in existing.items() for v in vals]
                continue

            # Extract enum fields from request body schema
            enum_fields = {}
            request_body = operation.get("requestBody", {})
            content = request_body.get("content", {})
            # Match any JSON content type (application/json, application/json;charset=utf-8, etc.)
            json_content = {}
            for ct_key, ct_val in content.items():
                if ct_key.startswith("application/json"):
                    json_content = ct_val
                    break
            body_schema = json_content.get("schema", {})
            if body_schema:
                enum_fields = _extract_enum_fields_from_schema(body_schema, components)

            # Extract enum fields from query/path parameters
            for param in operation.get("parameters", []):
                param_schema = param.get("schema", {})
                if "$ref" in param_schema:
                    ref_path = param_schema["$ref"].replace("#/components/schemas/", "")
                    param_schema = components.get("schemas", {}).get(ref_path, {})
                if "enum" in param_schema:
                    enum_fields[param.get("name", "")] = param_schema["enum"]

            # Generate scenario axes from enum fields
            scenarios = []
            for field, values in enum_fields.items():
                for val in values:
                    scenarios.append(f"{field}:{val}")

            endpoints[key] = {
                "method": method_upper,
                "path": path,
                "service": service_name,
                "enumFields": enum_fields,
                "scenarios": scenarios,
                "operationId": operation.get("operationId", ""),
                "summary": operation.get("summary", ""),
                "tags": operation.get("tags", []),
            }

    return endpoints


def get_coverage_spec(force_refresh: bool = False) -> dict:
    """Get the full coverage spec (all endpoints + enums) for rider + driver.
    Caches to disk; pass force_refresh=True to re-fetch from live services."""
    with _coverage_lock:
        if not force_refresh and COVERAGE_SPEC_CACHE.is_file():
            try:
                cached = json.loads(COVERAGE_SPEC_CACHE.read_text())
                # Cache is valid for 1 hour
                if time.time() - cached.get("_cached_at", 0) < 3600:
                    return cached
            except Exception:
                pass

        spec = {"rider": {}, "driver": {}, "_cached_at": time.time()}

        rider_openapi = _fetch_openapi_spec(RIDER_URL)
        if rider_openapi:
            spec["rider"] = _extract_coverage_spec(rider_openapi, "rider")

        driver_openapi = _fetch_openapi_spec(DRIVER_URL)
        if driver_openapi:
            spec["driver"] = _extract_coverage_spec(driver_openapi, "driver")

        spec["summary"] = {
            "rider_endpoints": len(spec["rider"]),
            "driver_endpoints": len(spec["driver"]),
            "total_endpoints": len(spec["rider"]) + len(spec["driver"]),
            "rider_scenarios": sum(len(e["scenarios"]) for e in spec["rider"].values()),
            "driver_scenarios": sum(len(e["scenarios"]) for e in spec["driver"].values()),
        }

        try:
            COVERAGE_SPEC_CACHE.write_text(json.dumps(spec, default=str))
        except Exception:
            pass

        return spec


def _load_coverage_runs() -> dict:
    """Load all recorded coverage runs from disk."""
    if COVERAGE_RUNS_FILE.is_file():
        try:
            return json.loads(COVERAGE_RUNS_FILE.read_text())
        except Exception:
            pass
    return {"runs": []}


def _save_coverage_runs(data: dict):
    """Save coverage runs to disk."""
    try:
        COVERAGE_RUNS_FILE.write_text(json.dumps(data, default=str))
    except Exception as e:
        print(f"  \033[91m[Coverage]\033[0m Failed to save runs: {e}")


def record_coverage_hit(hit: dict):
    """Record a single API call fingerprint.
    hit: {runId, service, method, path, fingerprint: {field: value, ...}, timestamp}
    """
    with _coverage_lock:
        data = _load_coverage_runs()

        run_id = hit.get("runId", "default")
        # Find or create run
        run = None
        for r in data["runs"]:
            if r["id"] == run_id:
                run = r
                break
        if not run:
            run = {
                "id": run_id,
                "startedAt": hit.get("timestamp", time.time()),
                "hits": [],
            }
            data["runs"].append(run)

        run["hits"].append({
            "service": hit.get("service", ""),
            "method": hit.get("method", ""),
            "path": hit.get("path", ""),
            "fingerprint": hit.get("fingerprint", {}),
            "timestamp": hit.get("timestamp", time.time()),
        })
        run["lastHitAt"] = hit.get("timestamp", time.time())

        # Keep only last 50 runs
        if len(data["runs"]) > 50:
            data["runs"] = data["runs"][-50:]

        _save_coverage_runs(data)


def record_coverage_batch(hits: list):
    """Record multiple coverage hits at once."""
    with _coverage_lock:
        data = _load_coverage_runs()

        for hit in hits:
            run_id = hit.get("runId", "default")
            run = None
            for r in data["runs"]:
                if r["id"] == run_id:
                    run = r
                    break
            if not run:
                run = {
                    "id": run_id,
                    "startedAt": hit.get("timestamp", time.time()),
                    "hits": [],
                }
                data["runs"].append(run)

            run["hits"].append({
                "service": hit.get("service", ""),
                "method": hit.get("method", ""),
                "path": hit.get("path", ""),
                "fingerprint": hit.get("fingerprint", {}),
                "timestamp": hit.get("timestamp", time.time()),
            })
            run["lastHitAt"] = hit.get("timestamp", time.time())

        if len(data["runs"]) > 50:
            data["runs"] = data["runs"][-50:]

        _save_coverage_runs(data)


def get_coverage_report(run_ids: list | None = None) -> dict:
    """Generate a coverage report comparing recorded hits against the spec.
    If run_ids is None, uses all runs combined."""
    spec = get_coverage_spec()
    runs_data = _load_coverage_runs()

    # Collect all hits (optionally filtered by run_ids)
    all_hits = []
    selected_runs = []
    for run in runs_data.get("runs", []):
        if run_ids is None or run["id"] in run_ids:
            all_hits.extend(run["hits"])
            selected_runs.append({"id": run["id"], "startedAt": run.get("startedAt"), "hitCount": len(run["hits"])})

    # Build hit index: {service: {"METHOD /path": count}}
    hit_index = {"rider": {}, "driver": {}}
    fingerprint_index = {"rider": {}, "driver": {}}

    # Map hit service names to spec service names
    SERVICE_MAP = {
        "rider": "rider",
        "driver": "driver",
        "lts": "driver",           # LTS endpoints are in driver OpenAPI
        "provider-dashboard": None, # Dashboard endpoints are separate
        "rider-dashboard": None,
        "mock-idfy": None,
        "mock-server": None,
        "internal": None,
    }

    for hit in all_hits:
        raw_service = hit.get("service", "")
        service = SERVICE_MAP.get(raw_service, raw_service)
        if service not in hit_index:
            continue
        # Normalize hit path: strip query string, normalize params
        hit_path = _normalize_path_params(hit["path"])
        key = f"{hit['method']} {hit_path}"
        if key not in hit_index[service]:
            hit_index[service][key] = 0
            fingerprint_index[service][key] = []
        hit_index[service][key] += 1
        if hit.get("fingerprint"):
            fingerprint_index[service][key].append(hit["fingerprint"])

    # Compare against spec
    report = {"rider": {}, "driver": {}}
    totals = {"rider": {"total": 0, "covered": 0, "scenarios_total": 0, "scenarios_covered": 0},
              "driver": {"total": 0, "covered": 0, "scenarios_total": 0, "scenarios_covered": 0}}

    for service in ("rider", "driver"):
        for endpoint_key, endpoint_spec in spec.get(service, {}).items():
            totals[service]["total"] += 1
            hit_count = hit_index[service].get(endpoint_key, 0)
            is_covered = hit_count > 0
            if is_covered:
                totals[service]["covered"] += 1

            # Scenario coverage: check which enum values were actually sent
            scenario_coverage = {}
            fingerprints = fingerprint_index[service].get(endpoint_key, [])
            for field, possible_values in endpoint_spec.get("enumFields", {}).items():
                totals[service]["scenarios_total"] += len(possible_values)
                tested_values = set()
                for fp in fingerprints:
                    if field in fp:
                        tested_values.add(str(fp[field]))
                covered_count = len(tested_values.intersection(set(str(v) for v in possible_values)))
                totals[service]["scenarios_covered"] += covered_count
                scenario_coverage[field] = {
                    "possible": possible_values,
                    "tested": list(tested_values),
                    "coverage": covered_count / len(possible_values) if possible_values else 1.0,
                }

            report[service][endpoint_key] = {
                "covered": is_covered,
                "hitCount": hit_count,
                "scenarioCoverage": scenario_coverage,
                "operationId": endpoint_spec.get("operationId", ""),
                "tags": endpoint_spec.get("tags", []),
            }

    # Overall percentages
    for service in ("rider", "driver"):
        t = totals[service]
        t["endpointCoverage"] = (t["covered"] / t["total"] * 100) if t["total"] > 0 else 0
        t["scenarioCoverage"] = (t["scenarios_covered"] / t["scenarios_total"] * 100) if t["scenarios_total"] > 0 else 0

    return {
        "totals": totals,
        "report": report,
        "runs": selected_runs,
        "totalHits": len(all_hits),
    }


def get_coverage_chains(run_id: str | None = None, time_window_s: float = 5.0) -> dict:
    """Parse service logs to detect internal API chains triggered during a test run.
    Groups internal requests by time proximity to identify call chains."""
    runs_data = _load_coverage_runs()

    # Get time bounds for the run
    target_run = None
    if run_id:
        for r in runs_data.get("runs", []):
            if r["id"] == run_id:
                target_run = r
                break

    # Parse logs for Request&Response entries
    log_pattern = re.compile(
        r'Request&Response\] \|> Request: RequestInfo \{requestMethod = "(\w+)", '
        r'rawPathInfo = "([^"]+)"'
    )
    timestamp_pattern = re.compile(r'"timestamp":"([^"]+)"')

    chains = []
    service_hits = {}  # {service: [{method, path, timestamp_str}]}

    for service_name, log_path in SERVICE_LOGS.items():
        if not log_path.is_file():
            continue
        # Only read rider-app and driver-app logs for chain detection
        if service_name not in ("rider-app", "driver-app"):
            continue

        service_hits[service_name] = []
        try:
            # Read last 500KB of the log for recent entries
            file_size = log_path.stat().st_size
            read_start = max(0, file_size - 512 * 1024)
            with open(log_path, "r") as f:
                if read_start > 0:
                    f.seek(read_start)
                    f.readline()  # skip partial line
                for line in f:
                    m = log_pattern.search(line)
                    ts_m = timestamp_pattern.search(line)
                    if m and ts_m:
                        method = m.group(1)
                        path = m.group(2)
                        ts = ts_m.group(1)
                        # Skip health checks
                        if path in ("/v2", "/ui", "/healthcheck"):
                            continue
                        service_hits[service_name].append({
                            "method": method,
                            "path": path,
                            "timestamp": ts,
                        })
        except Exception as e:
            print(f"  \033[91m[Coverage]\033[0m Error reading {log_path}: {e}")

    # Group into chains by time proximity
    # A chain is: a user-facing hit on one service triggers hits on other services within time_window_s
    all_entries = []
    for svc, hits in service_hits.items():
        for h in hits:
            all_entries.append({**h, "service": svc})

    # Sort by timestamp
    all_entries.sort(key=lambda x: x["timestamp"])

    # Simple chain grouping: consecutive entries within time_window_s of each other
    if all_entries:
        current_chain = [all_entries[0]]
        for entry in all_entries[1:]:
            # Simple heuristic: if entries are close in time, they're part of the same chain
            current_chain.append(entry)
            if len(current_chain) > 20:
                chains.append(current_chain)
                current_chain = []
        if current_chain:
            chains.append(current_chain)

    # Summarize: for each user-facing endpoint, what internal endpoints were triggered
    chain_summary = []
    for chain in chains[-50:]:  # Last 50 chains
        if len(chain) < 2:
            continue
        chain_summary.append({
            "trigger": {"service": chain[0]["service"], "method": chain[0]["method"], "path": chain[0]["path"]},
            "internal": [{"service": e["service"], "method": e["method"], "path": e["path"]} for e in chain[1:]],
            "timestamp": chain[0]["timestamp"],
        })

    return {
        "chains": chain_summary,
        "serviceHitCounts": {svc: len(hits) for svc, hits in service_hits.items()},
    }


class ContextHandler(BaseHTTPRequestHandler):
    def log_message(self, format, *args):
        print(f"  \033[93m[Context API]\033[0m {args[0]}")

    def handle_one_request(self):
        try:
            super().handle_one_request()
        except BrokenPipeError:
            pass  # Client disconnected

    def _cors_headers(self):
        self.send_header("Access-Control-Allow-Origin", "*")
        self.send_header("Access-Control-Allow-Methods",
                         "GET, POST, PUT, DELETE, OPTIONS")
        self.send_header("Access-Control-Allow-Headers", "*")

    def _send_json(self, data, status=200):
        try:
            self.send_response(status)
            self.send_header("Content-Type", "application/json")
            self._cors_headers()
            self.end_headers()
            self.wfile.write(json.dumps(data, default=str).encode())
        except BrokenPipeError:
            pass  # Client disconnected before response was fully written

    def do_OPTIONS(self):
        self.send_response(200)
        self._cors_headers()
        self.end_headers()

    def _proxy(self, method):
        parsed = urlparse(self.path)
        path = parsed.path

        # Determine target — rider uses /v2 prefix, driver uses /ui prefix, lts direct, fleet direct
        LTS_URL = os.environ.get("LTS_URL", "http://localhost:8081")
        PROVIDER_DASHBOARD_URL = os.environ.get(
            "PROVIDER_DASHBOARD_URL", "http://localhost:8018")
        MOCK_IDFY_URL = os.environ.get(
            "MOCK_IDFY_URL", "http://localhost:6235")
        MOCK_SERVER_URL = os.environ.get(
            "MOCK_SERVER_URL", "http://localhost:8080")
        RIDER_DASHBOARD_URL = os.environ.get(
            "RIDER_DASHBOARD_URL", "http://localhost:8017")
        if path.startswith("/proxy/rider-dashboard/"):
            target_base = RIDER_DASHBOARD_URL
            target_path = path[len("/proxy/rider-dashboard"):]
        elif path.startswith("/proxy/mock-server/"):
            target_base = MOCK_SERVER_URL
            target_path = path[len("/proxy/mock-server"):]
        elif path.startswith("/proxy/mock-idfy/"):
            target_base = MOCK_IDFY_URL
            target_path = path[len("/proxy/mock-idfy"):]
        elif path.startswith("/proxy/rider-raw/"):
            target_base = RIDER_URL
            target_path = path[len("/proxy/rider-raw"):]
        elif path.startswith("/proxy/rider/"):
            target_base = RIDER_URL
            target_path = "/v2" + path[len("/proxy/rider"):]
        elif path.startswith("/proxy/lts-raw/"):
            target_base = LTS_URL
            target_path = path[len("/proxy/lts-raw"):]
        elif path.startswith("/proxy/lts/"):
            target_base = LTS_URL
            target_path = "/ui" + path[len("/proxy/lts"):]
        elif path.startswith("/proxy/provider-dashboard/"):
            target_base = PROVIDER_DASHBOARD_URL
            target_path = path[len("/proxy/provider-dashboard"):]
        elif path.startswith("/proxy/driver-raw/"):
            target_base = DRIVER_URL
            target_path = path[len("/proxy/driver-raw"):]
        elif path.startswith("/proxy/driver/"):
            target_base = DRIVER_URL
            target_path = "/ui" + path[len("/proxy/driver"):]
        elif path.startswith("/proxy/juspay-payment/"):
            target_base = os.environ.get("MOCK_SERVER_URL", "http://localhost:8091/")
            target_path = path[len("/proxy/juspay-payment"):]
        else:
            return False

        _xpt = self.headers.get("X-Proxy-Target", "").strip()
        if _xpt:
            try:
                _xp = urlparse(_xpt)
                if _xp.scheme in ("http", "https") and _xp.netloc:
                    target_base = f"{_xp.scheme}://{_xp.netloc}"
            except Exception:
                pass

        target_url = f"{target_base}{target_path}"
        if parsed.query:
            target_url += f"?{parsed.query}"

        # Read request body
        content_len = int(self.headers.get("Content-Length", 0))
        body = self.rfile.read(content_len) if content_len > 0 else None

        # Forward headers (except Host)
        fwd_headers = {}
        for key in self.headers:
            if key.lower() not in ("host", "origin", "referer", "x-proxy-target"):
                fwd_headers[key] = self.headers[key]

        try:
            req = urllib.request.Request(
                target_url, data=body, headers=fwd_headers, method=method)
            _t0 = time.time()
            with urllib.request.urlopen(req, timeout=30) as resp:
                resp_body = resp.read()
                upstream_ms = int((time.time() - _t0) * 1000)
                if resp.headers.get("Content-Encoding", "") == "gzip":
                    import gzip as _gzip
                    resp_body = _gzip.decompress(resp_body)
                self.send_response(resp.status)
                self.send_header("Content-Type", resp.headers.get("Content-Type", "application/json"))
                self.send_header("X-Upstream-Latency-Ms", str(upstream_ms))
                self.send_header("X-Proxy-Url", target_url)
                self._cors_headers()
                self.end_headers()
                self.wfile.write(resp_body)
        except urllib.error.HTTPError as e:
            _t0 = time.time()
            resp_body = e.read()
            upstream_ms = int((time.time() - _t0) * 1000)
            if e.headers.get("Content-Encoding", "") == "gzip":
                import gzip as _gzip
                try:
                    resp_body = _gzip.decompress(resp_body)
                except Exception:
                    pass
            self.send_response(e.code)
            self.send_header("Content-Type", e.headers.get("Content-Type", "application/json"))
            self.send_header("X-Upstream-Latency-Ms", str(upstream_ms))
            self.send_header("X-Proxy-Url", target_url)
            self._cors_headers()
            self.end_headers()
            self.wfile.write(resp_body)
        except Exception as e:
            self._send_json({"error": str(e), "proxyUrl": target_url}, 502)

        return True

    def _read_json_body(self):
        content_len = int(self.headers.get("Content-Length", 0))
        if content_len > 0:
            return json.loads(self.rfile.read(content_len))
        return {}

    def _handle(self, method):
        parsed = urlparse(self.path)
        path = parsed.path.rstrip("/")

        # Proxy requests
        if path.startswith("/proxy/"):
            return self._proxy(method)

        # POST API endpoints
        if method == "POST" and path == "/api/logs/start":
            token = start_log_tails()
            self._send_json({"token": token})
            return True

        if method == "POST" and path == "/api/logs/stop":
            body = self._read_json_body()
            token = body.get("token", "")
            logs = stop_log_tails(token)
            self._send_json({"logs": logs})
            return True

        if method == "POST" and path == "/api/terminal/start":
            body = self._read_json_body() or {}
            cols = int(body.get("cols") or 80)
            rows = int(body.get("rows") or 24)
            cwd = body.get("cwd") or str(PROJECT_ROOT)
            shell = body.get("shell") or os.environ.get("SHELL", "/bin/bash")
            try:
                sid, pid, _fd = _terminal_spawn(cols, rows, cwd, shell)
                self._send_json({"session": sid, "pid": pid})
            except Exception as e:
                self._send_json({"error": str(e)}, 500)
            return True

        if method == "GET" and path == "/api/terminal/stream":
            qs = parse_qs(parsed.query)
            sid = (qs.get("session") or [""])[0]
            with _terminal_lock:
                sess = _terminal_sessions.get(sid)
            if not sess:
                self._send_json({"error": "unknown session"}, 404)
                return True
            try:
                self.send_response(200)
                self.send_header("Content-Type", "text/event-stream")
                self.send_header("Cache-Control", "no-cache")
                self.send_header("X-Accel-Buffering", "no")
                self.send_header("Connection", "keep-alive")
                self._cors_headers()
                self.end_headers()
            except Exception:
                return True
            fd = sess["fd"]
            pid = sess["pid"]
            exit_code = None
            try:
                while True:
                    if sess.get("closed"):
                        break
                    try:
                        r, _w, _x = _select.select([fd], [], [], 0.5)
                    except (OSError, ValueError):
                        break
                    if r:
                        try:
                            chunk = os.read(fd, 4096)
                        except OSError as e:
                            if e.errno in (errno.EAGAIN, errno.EWOULDBLOCK):
                                continue
                            chunk = b""
                        if not chunk:
                            try:
                                _wpid, status = os.waitpid(pid, os.WNOHANG)
                                if _wpid == pid:
                                    exit_code = os.WEXITSTATUS(status) if os.WIFEXITED(status) else -1
                            except Exception:
                                pass
                            break
                        try:
                            payload = json.dumps({"b64": base64.b64encode(chunk).decode("ascii")})
                            self.wfile.write(f"data: {payload}\n\n".encode("utf-8"))
                            self.wfile.flush()
                        except (BrokenPipeError, ConnectionResetError):
                            return True
                    else:
                        try:
                            wpid, status = os.waitpid(pid, os.WNOHANG)
                            if wpid == pid:
                                exit_code = os.WEXITSTATUS(status) if os.WIFEXITED(status) else -1
                                try:
                                    chunk = os.read(fd, 65536)
                                    if chunk:
                                        payload = json.dumps({"b64": base64.b64encode(chunk).decode("ascii")})
                                        self.wfile.write(f"data: {payload}\n\n".encode("utf-8"))
                                        self.wfile.flush()
                                except Exception:
                                    pass
                                break
                        except ChildProcessError:
                            break
                        try:
                            self.wfile.write(b": ping\n\n")
                            self.wfile.flush()
                        except (BrokenPipeError, ConnectionResetError):
                            return True
            finally:
                try:
                    end_payload = json.dumps({"exit": exit_code})
                    self.wfile.write(f"event: end\ndata: {end_payload}\n\n".encode("utf-8"))
                    self.wfile.flush()
                except Exception:
                    pass
                with _terminal_lock:
                    s = _terminal_sessions.pop(sid, None)
                if s:
                    try:
                        os.close(s["fd"])
                    except Exception:
                        pass
                    try:
                        os.waitpid(s["pid"], os.WNOHANG)
                    except Exception:
                        pass
            return True

        if method == "POST" and path == "/api/terminal/input":
            body = self._read_json_body() or {}
            sid = body.get("session", "")
            with _terminal_lock:
                sess = _terminal_sessions.get(sid)
            if not sess:
                self._send_json({"error": "unknown session"}, 400)
                return True
            try:
                data = base64.b64decode(body.get("data", "") or "")
            except Exception as e:
                self._send_json({"error": f"bad base64: {e}"}, 400)
                return True
            try:
                with sess["lock"]:
                    written = 0
                    while written < len(data):
                        try:
                            n = os.write(sess["fd"], data[written:])
                            if n <= 0:
                                break
                            written += n
                        except BlockingIOError:
                            _select.select([], [sess["fd"]], [], 1.0)
                self._send_json({"ok": True})
            except Exception as e:
                self._send_json({"error": str(e)}, 500)
            return True

        if method == "POST" and path == "/api/terminal/resize":
            body = self._read_json_body() or {}
            sid = body.get("session", "")
            cols = int(body.get("cols") or 80)
            rows = int(body.get("rows") or 24)
            with _terminal_lock:
                sess = _terminal_sessions.get(sid)
            if not sess:
                self._send_json({"error": "unknown session"}, 400)
                return True
            try:
                fcntl.ioctl(sess["fd"], _termios.TIOCSWINSZ,
                            struct.pack("HHHH", rows, cols, 0, 0))
                self._send_json({"ok": True})
            except Exception as e:
                self._send_json({"error": str(e)}, 500)
            return True

        if method == "POST" and path == "/api/terminal/kill":
            body = self._read_json_body() or {}
            sid = body.get("session", "")
            sess = _terminal_close(sid)
            if not sess:
                self._send_json({"error": "unknown session"}, 400)
                return True
            self._send_json({"ok": True})
            return True

        if method == "GET" and path == "/api/config-sync/envs":
            self._send_json({
                "envs": list(CONFIG_SYNC_ENVS),
                "default": CONFIG_SYNC_DEFAULT_FROM,
            })
            return True

        if method == "GET" and path == "/api/ui-state":
            # Returns the entire ui-state dict. The dashboard reads this on
            # mount so dropdowns can be restored to whatever the user picked
            # before the page reload (sync source, control-center ref, native
            # app platform/variant for each launcher slot, …). Empty dict on
            # a freshly-booted server — the dashboard then falls back to its
            # own defaults from the launcher option endpoints.
            with _ui_state_lock:
                self._send_json(dict(_ui_state))
            return True

        if method in ("PUT", "POST") and path == "/api/ui-state":
            # Body is a flat dict; merge each key into the in-memory store.
            # Setting a key to null deletes it (so the dashboard can reset a
            # field without having to know whether it was previously set).
            # Returns the merged state for the dashboard to verify against.
            body = self._read_json_body()
            if not isinstance(body, dict):
                self._send_json({"error": "body must be a JSON object"}, 400)
                return True
            with _ui_state_lock:
                for k, v in body.items():
                    if v is None:
                        _ui_state.pop(k, None)
                    else:
                        _ui_state[k] = v
                merged = dict(_ui_state)
            self._send_json(merged)
            return True

        if method == "GET" and path == "/api/config-sync/status":
            with _config_sync_lock:
                st = dict(_config_sync_state)
                st["log"] = st["log"][-300:]
            self._send_json(st)
            return True

        if method == "POST" and path == "/api/config-sync/import":
            body = self._read_json_body()
            from_env = body.get("from", CONFIG_SYNC_DEFAULT_FROM)
            force_fetch = bool(body.get("forceFetch", False))
            if from_env not in CONFIG_SYNC_ENVS:
                self._send_json({"error": f"unknown env '{from_env}'", "envs": list(CONFIG_SYNC_ENVS)}, 400)
                return True
            accepted = trigger_config_sync(from_env, force_fetch=force_fetch)
            if not accepted:
                self._send_json({"error": "another config-sync is already running"}, 409)
                return True
            self._send_json({"started": True, "from": from_env, "forceFetch": force_fetch})
            return True

        if method in ("GET", "POST") and path == "/api/integration-tests/seed-toll-dashboard":
            self._send_json(seed_toll_dashboard_access())
            return True

        if method in ("GET", "POST") and path == "/api/local-testing-data/reapply":
            try:
                _apply_local_testing_data()
                self._send_json({"ok": True})
            except Exception as e:
                self._send_json({"ok": False, "error": str(e)}, 500)
            return True

        if method == "GET" and path == "/api/control-center/status":
            with _control_center_lock:
                st = dict(_control_center_state)
                st["log"] = st["log"][-300:]
            # Refresh `ready` cheaply on each status read so the dashboard
            # learns about vite coming up between log emissions.
            if st["running"] and not st["ready"]:
                if _probe_control_center_ready():
                    with _control_center_lock:
                        _control_center_state["ready"] = True
                    st["ready"] = True
            self._send_json(st)
            return True

        if method == "POST" and path == "/api/control-center/start":
            body = self._read_json_body() or {}
            ref = (body.get("ref") or "").strip() or None
            accepted = trigger_control_center(ref=ref)
            if not accepted:
                self._send_json({"error": "control-center is already running"}, 409)
                return True
            self._send_json({"started": True, "url": CONTROL_CENTER_URL, "ref": ref})
            return True


        if method == "POST" and path == "/api/control-center/stop":
            killed = stop_control_center()
            self._send_json({"stopped": killed})
            return True

        if method == "GET" and path == "/api/ny-react-native/options":
            consumer_variants = _ny_rn_detect_variants("customer")
            provider_variants = _ny_rn_detect_variants("driver")
            self._send_json({
                "apps": sorted(NY_RN_VALID_APPS),
                "platforms": sorted(NY_RN_VALID_PLATFORMS),
                "variants_by_app": {
                    "customer": consumer_variants,
                    "driver": provider_variants,
                },
                "defaults": {
                    "platform": "android",
                    "variant_by_app": dict(NY_RN_DEFAULT_VARIANT_BY_APP),
                },
            })
            return True

        if method == "GET" and path == "/api/ny-react-native/status":
            # ?app=customer|driver  → that app only.  No app → both, keyed.
            from urllib.parse import parse_qs as _pq
            qs = _pq(parsed.query)
            req_app = qs.get("app", [None])[0]

            def _snapshot(a: str) -> dict:
                with _ny_rn_locks[a]:
                    st = dict(_ny_rn_states[a])
                    st["log"] = st["log"][-300:]
                if st["running"] and not st["ready"] and _probe_ny_rn_ready(a):
                    with _ny_rn_locks[a]:
                        _ny_rn_states[a]["ready"] = True
                    st["ready"] = True
                return st

            if req_app in NY_RN_VALID_APPS:
                self._send_json(_snapshot(req_app))
            else:
                self._send_json({a: _snapshot(a) for a in NY_RN_VALID_APPS})
            return True

        if method == "POST" and path == "/api/ny-react-native/start":
            body = self._read_json_body()
            app = body.get("app", "customer")
            platform = body.get("platform", "android")
            variant = body.get("variant") or NY_RN_DEFAULT_VARIANT_BY_APP.get(app, "Bridge")
            ref = (body.get("ref") or "").strip() or None
            if app not in NY_RN_VALID_APPS:
                self._send_json({"error": f"invalid app '{app}'", "valid": sorted(NY_RN_VALID_APPS)}, 400)
                return True
            if platform not in NY_RN_VALID_PLATFORMS:
                self._send_json({"error": f"invalid platform '{platform}'", "valid": sorted(NY_RN_VALID_PLATFORMS)}, 400)
                return True

            # Optional Firebase config override. Caller passes:
            #   firebase_override: { filename: "google-services.json"|"GoogleService-Info.plist",
            #                        content_base64: "<file bytes b64>" }
            # We validate filename+size, decode, write to /tmp, and pass the path
            # via env to the build runner. The runner backs up the in-repo file
            # before copying and restores it on teardown.
            firebase_override_path: str | None = None
            fb = body.get("firebase_override")
            if isinstance(fb, dict) and fb.get("content_base64"):
                import base64 as _b64
                fb_filename = (fb.get("filename") or "").strip()
                expected = (
                    "google-services.json" if platform == "android"
                    else "GoogleService-Info.plist"
                )
                if fb_filename != expected:
                    self._send_json({
                        "error": f"firebase_override filename must be '{expected}' for platform '{platform}', got {fb_filename!r}",
                    }, 400)
                    return True
                try:
                    raw = _b64.b64decode(fb["content_base64"], validate=True)
                except Exception as e:
                    self._send_json({"error": f"firebase_override content_base64 invalid: {e}"}, 400)
                    return True
                # Sanity caps — these files are tiny in practice.
                if len(raw) == 0 or len(raw) > 256 * 1024:
                    self._send_json({
                        "error": f"firebase_override size {len(raw)}B out of range (must be 1B..256KB)",
                    }, 400)
                    return True
                # Write to a deterministic per-(app,platform) path so repeat
                # uploads overwrite cleanly.
                ext = "json" if platform == "android" else "plist"
                firebase_override_path = f"/tmp/ny-rn-firebase-{app}-{platform}.{ext}"
                try:
                    with open(firebase_override_path, "wb") as f:
                        f.write(raw)
                except OSError as e:
                    self._send_json({"error": f"could not write firebase override: {e}"}, 500)
                    return True

            accepted = trigger_ny_rn(
                app, platform, variant, ref=ref,
                firebase_override_path=firebase_override_path,
            )
            if not accepted:
                self._send_json({"error": f"{app} launcher is already running"}, 409)
                return True
            self._send_json({
                "started": True, "app": app, "platform": platform,
                "variant": variant, "ref": ref,
                "firebase_override": firebase_override_path,
            })
            return True

        if method == "GET" and path == "/api/git/refs":
            # Common endpoint for any whitelisted GitHub repo. UI passes
            # `?repo=<owner>/<name>&q=<search>`; we look up the local
            # checkout path from KNOWN_REPOS and merge local git data with
            # `gh api` enrichment. Whitelisting prevents arbitrary path
            # traversal and unbounded gh API calls.
            from urllib.parse import parse_qs as _pq
            qs = _pq(parsed.query)
            repo = (qs.get("repo", [""])[0]).strip()
            q = (qs.get("q", [""])[0]) or None
            if not repo or repo not in KNOWN_REPOS:
                self._send_json({
                    "error": f"unknown repo {repo!r}",
                    "valid": sorted(KNOWN_REPOS.keys()),
                }, 400)
                return True
            self._send_json(_git_repo_refs(
                KNOWN_REPOS[repo],
                github_repo=repo,
                q=q,
            ))
            return True

        if method == "POST" and path == "/api/ny-react-native/stop":
            body = self._read_json_body()
            app = body.get("app", "")
            if app not in NY_RN_VALID_APPS:
                self._send_json({"error": f"invalid app '{app}'", "valid": sorted(NY_RN_VALID_APPS)}, 400)
                return True
            killed = stop_ny_rn(app)
            self._send_json({"stopped": killed, "app": app})
            return True

        if method == "POST" and path == "/api/ny-react-native/clear-cache":
            # Local helper: subprocess.run with capture, returns (rc, out, err).
            def sh_capture(cmd, timeout=8):
                try:
                    p = subprocess.run(
                        cmd, capture_output=True, text=True, timeout=timeout,
                    )
                    return p.returncode, p.stdout or "", p.stderr or ""
                except Exception as e:
                    return -1, "", str(e)
            # Wipe the running app's MMKV / shared-prefs / Documents so a
            # fresh launch doesn't reuse a poisoned BASE_URL or stale
            # auth token. Per-platform mechanism:
            #   Android: `adb shell pm clear <package>` — clears app
            #            data including MMKV files in /data/data/<pkg>/.
            #   iOS:     delete the MMKV dir under the app data container
            #            (Library/Application Support/MMKV/ — verified
            #            via `xcrun simctl get_app_container … data`).
            #            simctl uninstall would be more thorough but
            #            also drops the install — too aggressive for a
            #            "clear cache" UX. After delete we terminate
            #            the process so the next launch reads fresh.
            body = self._read_json_body() or {}
            app = body.get("app", "")
            if app not in NY_RN_VALID_APPS:
                self._send_json({
                    "error": f"invalid app '{app}'",
                    "valid": sorted(NY_RN_VALID_APPS),
                }, 400)
                return True
            with _ny_rn_locks[app]:
                state = dict(_ny_rn_states[app])
            platform = (state.get("platform") or "").lower()
            variant = state.get("variant") or ""
            sub = "consumer" if app == "customer" else "provider"

            if platform == "android":
                # Resolve the active flavor's applicationId — provider
                # uses LynxDriverDevDebug → in.mobility.lynxdriver.debug,
                # consumer uses LynxDevDebug → in.mobility.international,
                # etc. Cheap probe: walk `pm list packages -3` on the
                # connected emulator and pick the most recent install
                # (the one we just launched). Falls back to a list of
                # known per-variant ids if discovery fails.
                adb_bin = _resolve_adb_bin()
                pkg = None
                rc, out, _err = sh_capture([
                    adb_bin, "shell", "dumpsys", "activity", "activities",
                ])
                if rc == 0:
                    m = re.search(
                        r"(in\.[\w.]+)/[A-Za-z][\w.]*Activity",
                        out,
                    )
                    if m:
                        pkg = m.group(1)
                if not pkg:
                    self._send_json({
                        "error": "could not resolve current app package",
                        "hint": "run the app first; we read the foreground activity",
                    }, 503)
                    return True
                rc_clear, _, err_clear = sh_capture(
                    [adb_bin, "shell", "pm", "clear", pkg],
                    timeout=10,
                )
                if rc_clear != 0:
                    self._send_json({
                        "error": f"pm clear failed: {err_clear.strip() or rc_clear}",
                        "package": pkg,
                    }, 500)
                    return True
                self._send_json({
                    "cleared": True,
                    "platform": "android",
                    "package": pkg,
                    "hint": "App is stopped. Tap the icon to relaunch with empty MMKV.",
                })
                return True

            if platform == "ios":
                # Booted simulator UDID
                rc, out, _err = sh_capture(
                    ["xcrun", "simctl", "list", "devices", "booted", "-j"],
                    timeout=4,
                )
                udid = None
                if rc == 0:
                    try:
                        sim_data = json.loads(out or "{}")
                        for runtime in sim_data.get("devices", {}).values():
                            for d in runtime:
                                if d.get("state") == "Booted":
                                    udid = d.get("udid")
                                    break
                            if udid:
                                break
                    except Exception:
                        pass
                if not udid:
                    self._send_json({"error": "no booted iOS Simulator"}, 503)
                    return True
                # Get bundle id from variant. The launcher already wrote
                # the GoogleService-Info.plist with the variant's bundle
                # id, so we re-resolve via xcodebuild against a known
                # workspace location. Cheaper: probe `simctl listapps`
                # and pick a 3rd-party app whose CFBundleName matches
                # the variant's expected display name.
                rc, out, _err = sh_capture(
                    ["xcrun", "simctl", "listapps", udid],
                    timeout=8,
                )
                bundle_id = None
                if rc == 0:
                    # listapps returns plist text. Find third-party apps
                    # only (skip Apple system bundles) and match the
                    # CFBundleName against the variant.
                    needle = (variant or "").lower()
                    for blob in re.findall(
                        r'"([\w.\-]+)"\s*=\s*\{[^}]*?CFBundleName\s*=\s*"([^"]+)";'
                        r'[^}]*?ApplicationType\s*=\s*"User";',
                        out,
                        flags=re.S,
                    ):
                        bid, name = blob[0], blob[1]
                        if needle and needle in name.lower():
                            bundle_id = bid
                            break
                if not bundle_id:
                    self._send_json({
                        "error": "could not resolve iOS bundle id from variant",
                        "variant": variant,
                        "hint": "is the app actually installed on the simulator?",
                    }, 503)
                    return True
                # Resolve the data container, delete MMKV + Documents.
                rc, container, err = sh_capture(
                    ["xcrun", "simctl", "get_app_container", udid, bundle_id, "data"],
                    timeout=4,
                )
                if rc != 0 or not container.strip():
                    self._send_json({
                        "error": f"get_app_container failed: {err.strip() or rc}",
                        "bundle_id": bundle_id,
                    }, 500)
                    return True
                container_path = Path(container.strip())
                # MMKV writes to several sub-locations across versions:
                # Library/Application Support/MMKV/, and Library/MMKV/.
                # Also wipe Library/Preferences/<bundle>.plist and
                # Documents (where react-native-config sometimes seeds).
                wiped: list[str] = []
                for sub_path in [
                    "Library/Application Support/MMKV",
                    "Library/MMKV",
                    "Documents",
                    f"Library/Preferences/{bundle_id}.plist",
                ]:
                    target = container_path / sub_path
                    if target.exists():
                        try:
                            if target.is_dir():
                                shutil.rmtree(target)
                            else:
                                target.unlink()
                            wiped.append(sub_path)
                        except Exception as e:
                            wiped.append(f"{sub_path} (FAILED: {e})")
                # Terminate the running app so next launch sees fresh state.
                sh_capture(
                    ["xcrun", "simctl", "terminate", udid, bundle_id],
                    timeout=4,
                )
                self._send_json({
                    "cleared": True,
                    "platform": "ios",
                    "bundle_id": bundle_id,
                    "container": str(container_path),
                    "wiped": wiped,
                    "hint": "App is terminated. Tap the icon to relaunch with empty MMKV.",
                })
                return True

            self._send_json({
                "error": f"unknown platform '{platform}' for app '{app}'",
                "hint": "is the launcher actually running?",
            }, 503)
            return True

        if method == "POST" and path == "/api/ny-react-native/open-debugger":
            # Tell Metro to open the React Native DevTools (Hermes
            # inspector) in the user's default browser. Metro itself
            # exposes POST /open-debugger which spawns a Chrome window
            # attached to the JS runtime — that gives FULL untruncated
            # console output (the iOS NSLog ~1024-byte clip the unified
            # log hands us in the Console / Network tabs is bypassed
            # entirely because we read straight from the JS runtime).
            #
            # Body: {"app": "customer"|"driver"}. We discover the Metro
            # port by grepping `/tmp/ny-rn-metro-<sub>.log` for the line
            # `Starting dev server on http://localhost:NNNN` — Metro
            # always logs this exactly once at startup.
            body = self._read_json_body() or {}
            app = body.get("app", "")
            if app not in NY_RN_VALID_APPS:
                self._send_json({
                    "error": f"invalid app '{app}'",
                    "valid": sorted(NY_RN_VALID_APPS),
                }, 400)
                return True
            sub = "consumer" if app == "customer" else "provider"
            # Discover Metro's port from the live process list. Earlier we
            # scraped /tmp/ny-rn-metro-<sub>.log for "Starting dev server
            # on http://localhost:NNNN" but newer Metro versions never
            # print that exact line — they show a boxed banner instead,
            # and the file is mostly ANSI control codes. Every Metro is
            # spawned by the launcher as `react-native start --port NNNN`
            # inside `data/ny-react-native/<sub>/node_modules/...` so we
            # can recover the port directly from `ps`, which is stable
            # across Metro versions and doesn't depend on log format.
            port = None
            try:
                ps = subprocess.run(
                    ["ps", "-eo", "pid=,command="],
                    capture_output=True, text=True, timeout=3,
                )
                marker = f"data/ny-react-native/{sub}/"
                # Newest match wins (latest restart); we find all and pick max-pid.
                candidates = []
                for line in ps.stdout.splitlines():
                    if marker in line and "react-native" in line and "start" in line:
                        m = re.search(r"--port\s+(\d+)", line)
                        if m:
                            try:
                                pid_str = line.strip().split(None, 1)[0]
                                candidates.append((int(pid_str), int(m.group(1))))
                            except (ValueError, IndexError):
                                pass
                if candidates:
                    candidates.sort()  # by pid ascending; last is newest
                    port = candidates[-1][1]
            except Exception as e:
                self._send_json({"error": f"scanning processes: {e}"}, 500)
                return True
            if port is None:
                self._send_json({
                    "error": "could not find a running Metro process for this app",
                    "hint": (
                        f"expected `react-native start --port NNNN` with "
                        f"`data/ny-react-native/{sub}/` in its path. "
                        "Is the launcher still running, or has Metro crashed?"
                    ),
                }, 503)
                return True
            try:
                import urllib.request as _ur
                import json as _json
                import uuid as _uuid
                import webbrowser as _wb
                from urllib.parse import urlencode as _urlencode
                # Resolve a debugger target via Metro's /json listing, then
                # POST /open-debugger with `device` + `page` (this Metro
                # version rejects appId-only requests with 404 "Unable to
                # find debugger target"). A fresh `launchId` per click
                # forces Metro to spawn a new debugger-frontend window so
                # close+reopen works — without it Metro silently reuses
                # the dead session from the last closed Chrome tab.
                # If Metro's POST fails for any reason we fall back to
                # opening the target's `devtoolsFrontendUrl` directly,
                # which always works as long as Metro is reachable.
                target = None
                targets = []
                try:
                    with _ur.urlopen(f"http://localhost:{port}/json", timeout=3) as r:
                        targets = _json.loads(r.read().decode("utf-8", "replace"))
                except Exception:
                    targets = []
                if isinstance(targets, list) and targets:
                    # Prefer the Fusebox-capable JS runtime (the modern RN
                    # debugger target); fall back to the first listed.
                    fusebox = [
                        t for t in targets
                        if isinstance(t, dict)
                        and t.get("reactNative", {}).get("capabilities", {}).get("prefersFuseboxFrontend")
                    ]
                    target = (fusebox or targets)[0]

                if target is None:
                    self._send_json({
                        "opened": False,
                        "port": port,
                        "app": app,
                        "error": "no debugger targets in Metro /json — is the app actually running and connected?",
                        "fallback_url": f"http://localhost:{port}/json",
                    }, 503)
                    return True

                # Extract device id + page number from the target's
                # `webSocketDebuggerUrl` (format: ws://host/inspector/debug?device=<id>&page=<n>).
                ws = target.get("webSocketDebuggerUrl") or ""
                dev_m = re.search(r"device=([^&]+)", ws)
                page_m = re.search(r"page=(\d+)", ws)
                device_id = dev_m.group(1) if dev_m else None
                page_num = page_m.group(1) if page_m else None
                app_id = target.get("appId") or target.get("description")
                launch_id = _uuid.uuid4().hex

                opened_via_metro = False
                metro_err = None
                if device_id and page_num:
                    qs = {
                        "device": device_id,
                        "page": page_num,
                        "launchId": launch_id,
                    }
                    if app_id:
                        qs["appId"] = app_id
                    try:
                        req = _ur.Request(
                            f"http://localhost:{port}/open-debugger?{_urlencode(qs)}",
                            method="POST",
                        )
                        with _ur.urlopen(req, timeout=5) as r:
                            _ = r.read()
                        opened_via_metro = True
                    except Exception as e:
                        metro_err = str(e)

                if not opened_via_metro:
                    # Fallback: open the devtoolsFrontendUrl directly in
                    # the user's default browser. This always works as
                    # long as Metro is reachable; we lose the launchId
                    # cache-busting but the URL itself is fresh per click.
                    frontend = target.get("devtoolsFrontendUrl") or ""
                    if frontend:
                        full = f"http://localhost:{port}{frontend}"
                        try:
                            _wb.open(full, new=2)
                            self._send_json({
                                "opened": True,
                                "via": "direct-url",
                                "port": port,
                                "app": app,
                                "appId": app_id,
                                "url": full,
                                "metro_open_debugger_error": metro_err,
                                "hint": "Metro's /open-debugger refused; opened the inspector URL directly.",
                            })
                            return True
                        except Exception as e:
                            self._send_json({
                                "opened": False,
                                "port": port,
                                "app": app,
                                "error": f"webbrowser.open failed: {e}",
                                "metro_open_debugger_error": metro_err,
                                "fallback_url": full,
                                "hint": "Open this URL in Chrome manually.",
                            }, 502)
                            return True
                    self._send_json({
                        "opened": False,
                        "port": port,
                        "app": app,
                        "error": metro_err or "no devtoolsFrontendUrl in target",
                        "fallback_url": f"http://localhost:{port}/json",
                    }, 502)
                    return True

                self._send_json({
                    "opened": True,
                    "via": "metro-open-debugger",
                    "port": port,
                    "app": app,
                    "appId": app_id,
                    "device": device_id,
                    "page": page_num,
                    "launchId": launch_id,
                    "hint": "Look for a new Chrome window with React Native DevTools.",
                })
            except Exception as e:
                self._send_json({
                    "opened": False,
                    "port": port,
                    "app": app,
                    "error": str(e),
                    "fallback_url": f"http://localhost:{port}/json",
                }, 500)
            return True

        if method == "GET" and path == "/api/ny-react-native/metro-log":
            # Tail /tmp/ny-rn-metro-{consumer|provider}.log so the dashboard
            # can stream Metro's own output (bundle compile progress, bundle
            # requests, transform errors) while the app is running. Build
            # log stops growing after the setup script prints its ready
            # sentinel; Metro keeps producing output continuously.
            from urllib.parse import parse_qs as _pq
            qs = _pq(parsed.query)
            req_app = qs.get("app", ["customer"])[0]
            try:
                lines = max(50, min(2000, int(qs.get("lines", ["400"])[0])))
            except ValueError:
                lines = 400
            sub = "consumer" if req_app == "customer" else "provider" if req_app == "driver" else None
            if sub is None:
                self._send_json({"error": f"invalid app '{req_app}'", "lines": []}, 400)
                return True
            path_str = f"/tmp/ny-rn-metro-{sub}.log"
            try:
                if not os.path.isfile(path_str):
                    self._send_json({"app": req_app, "lines": [], "error": f"{path_str} does not exist (Metro not started yet?)"})
                    return True
                with open(path_str, "rb") as f:
                    # Read the tail efficiently — seek to end, read back.
                    f.seek(0, 2)
                    size = f.tell()
                    chunk = min(size, max(64 * 1024, lines * 200))
                    f.seek(size - chunk)
                    data = f.read().decode("utf-8", errors="replace")
                tail = data.splitlines()[-lines:]
                self._send_json({"app": req_app, "lines": tail, "raw_total": len(tail)})
            except Exception as e:
                self._send_json({"app": req_app, "lines": [], "error": str(e)}, 500)
            return True

        if method == "GET" and path == "/api/ny-react-native/_adb-diag":
            # Diagnostic: shows which adb is being used, what `adb devices`
            # returns, and the head/tail of `adb logcat -d -t 5`. Useful
            # when the Console/Network/All tabs are empty.
            adb_bin = _resolve_adb_bin()
            try:
                devs = subprocess.run([adb_bin, "devices", "-l"],
                                      capture_output=True, text=True, timeout=5)
                logc = subprocess.run([adb_bin, "logcat", "-d", "-t", "5"],
                                      capture_output=True, text=True, timeout=5)
                self._send_json({
                    "adb_bin": adb_bin,
                    "android_home": os.environ.get("ANDROID_HOME"),
                    "devices_stdout": devs.stdout,
                    "devices_stderr": devs.stderr,
                    "logcat_sample_stdout": (logc.stdout or "").splitlines()[:20],
                    "logcat_sample_stderr": logc.stderr,
                    "logcat_returncode": logc.returncode,
                })
            except Exception as e:
                self._send_json({"error": str(e), "adb_bin": adb_bin}, 500)
            return True

        if method == "GET" and path == "/api/ny-react-native/logcat":
            # Tail device logcat for one of three categories. We pull a
            # WIDE window of unfiltered logcat and post-filter by regex —
            # adb tag filters miss too many cases (RN 0.79 + Hermes log
            # under different tags than RN 0.71; OkHttp variants, etc.).
            #
            # Platform-aware: Android uses `adb logcat`; iOS uses
            # `xcrun simctl spawn <udid> log show`. The dashboard passes
            # `?app=customer|driver` so we can resolve the platform from
            # the launcher state (set when the user clicked Start).
            from urllib.parse import parse_qs as _pq
            import re as _re
            qs = _pq(parsed.query)
            log_type = (qs.get("type", ["console"])[0]).lower()
            app_q = (qs.get("app", [""])[0]).lower() or None
            try:
                lines = max(50, min(2000, int(qs.get("lines", ["400"])[0])))
            except ValueError:
                lines = 400

            # Decide platform. If the dashboard sent ?app=, prefer that
            # state's platform. Otherwise fall back to "android" so the
            # legacy Android-only call site still works.
            platform = "android"
            variant = None
            if app_q in _ny_rn_states:
                with _ny_rn_locks[app_q]:
                    st = _ny_rn_states[app_q]
                    platform = (st.get("platform") or "android").lower()
                    variant = st.get("variant")

            console_patterns = [
                _re.compile(r"ReactNativeJS", _re.I),
                _re.compile(r"ReactNative", _re.I),
                _re.compile(r"\bHermes", _re.I),
                _re.compile(r"AndroidRuntime"),
                _re.compile(r"\bRNNativeLog\b"),
                _re.compile(r"\bChromiumNet\b"),
                _re.compile(r"\bjsi\b", _re.I),
                _re.compile(r"\b(WARN|ERROR|INFO|DEBUG)\b.*?\b(JS|JSI|JSC|console)\b", _re.I),
                _re.compile(r"unhandled\s+(promise|exception)", _re.I),
                _re.compile(r"FATAL EXCEPTION"),
            ]
            network_patterns = [
                _re.compile(r"OkHttp", _re.I),
                _re.compile(r"okhttp", _re.I),
                _re.compile(r"\bRetrofit\b", _re.I),
                _re.compile(r"\bVolley\b", _re.I),
                _re.compile(r"\bURLConnection\b", _re.I),
                _re.compile(r"\bApolloHttp", _re.I),
                _re.compile(r"\bHttpRequest\b", _re.I),
                _re.compile(r"\bHttpClient\b", _re.I),
                _re.compile(r"\bnetworking\b", _re.I),
                _re.compile(r"\bWebSocket\b", _re.I),
                _re.compile(r"https?://"),
                _re.compile(r"\b(GET|POST|PUT|DELETE|PATCH)\s+/"),
                _re.compile(r"\bRequest\s+(URL|Method|Header)", _re.I),
                _re.compile(r"\bResponse\s+(URL|Code|Header)", _re.I),
            ]

            # ── iOS branch: xcrun simctl log show ────────────────
            if platform == "ios":
                # Find the booted Simulator UDID. There may be multiple,
                # pick the first one matching what the launcher used.
                udid = None
                try:
                    sim = subprocess.run(
                        ["xcrun", "simctl", "list", "devices", "booted", "-j"],
                        capture_output=True, text=True, timeout=4,
                    )
                    if sim.returncode == 0:
                        sim_data = json.loads(sim.stdout or "{}")
                        for runtime in sim_data.get("devices", {}).values():
                            for d in runtime:
                                if d.get("state") == "Booted":
                                    udid = d.get("udid")
                                    break
                            if udid:
                                break
                except Exception:
                    pass

                if not udid:
                    self._send_json({
                        "type": log_type, "lines": [], "raw_total": 0,
                        "diag": {"reason": "no booted iOS Simulator found"},
                    })
                    return True

                # Process name = PascalCase variant. The Lynx scheme builds
                # a binary called "Lynx"; NammaYatri → "NammaYatri", etc.
                proc_name = (variant[:1].upper() + variant[1:]) if variant else "Lynx"

                # Window: ~2 minutes. log show is hard-capped at 4096 lines
                # of output by default — should be plenty for a dev tail.
                # Predicate filters by process name so we don't drown in
                # system noise. Console / network categories use the same
                # post-filter regex as Android.
                predicate = f'process == "{proc_name}"'
                ios_cmd = [
                    "xcrun", "simctl", "spawn", udid,
                    "log", "show",
                    "--last", "2m",
                    "--info", "--debug",
                    "--style", "compact",
                    "--predicate", predicate,
                ]
                try:
                    iproc = subprocess.run(ios_cmd, capture_output=True, text=True, timeout=12)
                    iraw = (iproc.stdout or "").splitlines()
                    ierr = (iproc.stderr or "").strip()

                    if log_type == "all":
                        imatched = iraw
                    elif log_type == "console":
                        imatched = [
                            ln for ln in iraw
                            if any(p.search(ln) for p in console_patterns)
                            or "ReactNativeJS" in ln
                            or "[javascript]" in ln.lower()
                            or "<Notice>" in ln
                            or "<Error>" in ln
                        ]
                    elif log_type == "network":
                        imatched = [
                            ln for ln in iraw
                            if any(p.search(ln) for p in network_patterns)
                            or "CFNetwork" in ln
                            or "NSURLSession" in ln
                            or "boringssl" in ln.lower()
                        ]
                    else:
                        imatched = iraw

                    diag = None
                    if not iraw:
                        diag = {
                            "platform": "ios",
                            "udid": udid,
                            "predicate": predicate,
                            "process_name": proc_name,
                            "log_show_stderr": ierr,
                            "log_show_rc": iproc.returncode,
                            "hint": (
                                "Empty result usually means the process name does not match. "
                                "Confirm with: xcrun simctl spawn " + udid +
                                ' launchctl list | grep -i ' + proc_name
                            ),
                        }
                    self._send_json({
                        "type": log_type,
                        "lines": imatched[-lines:],
                        "raw_total": len(iraw),
                        "error": ierr if iproc.returncode != 0 else None,
                        "diag": diag,
                    })
                    return True
                except subprocess.TimeoutExpired:
                    self._send_json({
                        "type": log_type, "lines": [],
                        "error": "xcrun simctl log show timed out (12s)",
                    }, 504)
                    return True
                except FileNotFoundError:
                    self._send_json({
                        "type": log_type, "lines": [],
                        "error": "xcrun not on PATH",
                    }, 500)
                    return True

            # ── Android branch: adb logcat ───────────────────────
            adb_bin = _resolve_adb_bin()
            try:
                # Pull a generous window unfiltered. -t with no tag args
                # gives us the last N raw entries. We then regex-filter.
                proc = subprocess.run(
                    [adb_bin, "logcat", "-d", "-v", "time", "-t", str(max(2000, lines * 6))],
                    capture_output=True, text=True, timeout=12,
                )
                raw = (proc.stdout or "").splitlines()
                err = (proc.stderr or "").strip()

                if log_type == "all":
                    matched = raw
                elif log_type == "console":
                    matched = [ln for ln in raw if any(p.search(ln) for p in console_patterns)]
                elif log_type == "network":
                    matched = [ln for ln in raw if any(p.search(ln) for p in network_patterns)]
                else:
                    matched = raw

                # When logcat returns nothing, include diagnostics inline so
                # the user can see why (wrong adb, no devices, daemon clash).
                diag = None
                if not raw:
                    try:
                        devs = subprocess.run([adb_bin, "devices", "-l"],
                                              capture_output=True, text=True, timeout=4)
                        diag = {
                            "adb_bin": adb_bin,
                            "devices": (devs.stdout or "").strip(),
                            "devices_err": (devs.stderr or "").strip(),
                            "logcat_stderr": err,
                            "logcat_rc": proc.returncode,
                        }
                    except Exception as de:
                        diag = {"adb_bin": adb_bin, "error": str(de)}

                self._send_json({
                    "type": log_type,
                    "lines": matched[-lines:],
                    "raw_total": len(raw),
                    "error": err if proc.returncode != 0 else None,
                    "diag": diag,
                })
            except subprocess.TimeoutExpired:
                self._send_json({"type": log_type, "lines": [], "error": "adb logcat timed out (12s)"}, 504)
            except FileNotFoundError:
                self._send_json({"type": log_type, "lines": [], "error": "adb not on PATH"}, 500)
            except Exception as e:
                self._send_json({"type": log_type, "lines": [], "error": str(e)}, 500)
            return True

        if method == "POST" and path == "/api/redis/flushall":
            try:
                # `subprocess` is imported at module scope; importing again
                # inside this function would make Python treat the name as
                # local for the ENTIRE function — breaking every other
                # subprocess.run call earlier in _handle (logcat, adb-diag,
                # metro-log, etc.) with UnboundLocalError.
                result = subprocess.run(
                    ["redis-cli", "--cluster", "call",
                        "localhost:30001", "flushall"],
                    capture_output=True, text=True, timeout=10
                )
                if result.returncode == 0:
                    self._send_json(
                        {"result": "ok", "output": result.stdout.strip()})
                else:
                    # Fallback to single-node flush (non-cluster local setup)
                    result2 = subprocess.run(
                        ["redis-cli", "-p", "6379", "flushall"],
                        capture_output=True, text=True, timeout=10
                    )
                    if result2.returncode == 0:
                        self._send_json(
                            {"result": "ok", "output": result2.stdout.strip(), "mode": "single-node"})
                    else:
                        self._send_json(
                            {"error": result.stderr.strip() or result2.stderr.strip()}, 500)
            except Exception as e:
                self._send_json({"error": str(e)}, 500)

        if method == "POST" and path == "/api/finance/clear-all":
            body = self._read_json_body()
            side = body.get("side", "bpp")
            schema = "atlas_app" if side == "bap" else "atlas_driver_offer_bpp"
            result = clear_finance_data(schema)
            self._send_json(result)
            return True

        if method == "POST" and path == "/api/inflate-distance":
            body = self._read_json_body()
            ride_id = body.get("rideId")
            multiplier = body.get("multiplier", 3)
            if not ride_id:
                self._send_json({"error": "rideId required"}, 400)
                return True
            try:
                conn = get_conn()
                conn.autocommit = True
                cur = conn.cursor()
                cur.execute("""
                    UPDATE atlas_driver_offer_bpp.ride r
                    SET traveled_distance = COALESCE(
                        (SELECT estimated_distance FROM atlas_driver_offer_bpp.booking WHERE id = r.booking_id), 5000
                    ) * %s
                    WHERE r.id = %s
                """, (multiplier, ride_id))
                rows = cur.rowcount
                conn.close()
                if rows > 0:
                    self._send_json(
                        {"result": "Success", "rowsUpdated": rows, "multiplier": multiplier})
                else:
                    self._send_json(
                        {"error": f"No ride found with id {ride_id}"}, 404)
            except Exception as e:
                self._send_json({"error": str(e)}, 500)
            return True

        # ── Coverage API ──
        if method == "GET" and path == "/api/coverage/spec":
            from urllib.parse import parse_qs
            qs = parse_qs(parsed.query)
            force = qs.get("refresh", ["0"])[0] == "1"
            self._send_json(get_coverage_spec(force_refresh=force))
            return True

        if method == "POST" and path == "/api/coverage/record":
            body = self._read_json_body()
            if isinstance(body, list):
                record_coverage_batch(body)
            else:
                record_coverage_hit(body)
            self._send_json({"recorded": True})
            return True

        if method == "GET" and path == "/api/coverage/report":
            from urllib.parse import parse_qs
            qs = parse_qs(parsed.query)
            run_ids = qs.get("runId", None)
            self._send_json(get_coverage_report(run_ids))
            return True

        if method == "GET" and path == "/api/coverage/chains":
            from urllib.parse import parse_qs
            qs = parse_qs(parsed.query)
            run_id = qs.get("runId", [None])[0]
            self._send_json(get_coverage_chains(run_id))
            return True

        if method == "GET" and path == "/api/coverage/runs":
            data = _load_coverage_runs()
            # Return run summaries without full hit data
            runs = []
            for r in data.get("runs", []):
                runs.append({
                    "id": r["id"],
                    "startedAt": r.get("startedAt"),
                    "lastHitAt": r.get("lastHitAt"),
                    "hitCount": len(r.get("hits", [])),
                })
            self._send_json({"runs": runs})
            return True

        if method == "DELETE" and path == "/api/coverage/runs":
            with _coverage_lock:
                _save_coverage_runs({"runs": []})
            self._send_json({"cleared": True})
            return True

        if method == "POST" and path in ("/api/db/update", "/api/db/select"):
            if not _tca_auth_ok(self):
                self._send_json({"error": "unauthorized"}, 401)
                return True
            body = self._read_json_body() or {}
            payload, status = (
                _tca_db_update(body) if path.endswith("/update") else _tca_db_select(body)
            )
            self._send_json(payload, status)
            return True

        # GET API endpoints (POST also allowed for metrics/push and webhook)
        _post_allowed = {
            "/api/metrics/push", "/api/webhook/run-all", "/metrics/push", "/webhook/run-all",
            "/api/db/update", "/api/db/select",
            "/api/integration-tests/seed-toll-dashboard",
            "/api/local-testing-data/reapply",
        }
        if method != "GET" and not path.startswith("/proxy/") and path not in _post_allowed:
            self._send_json({"error": "method not allowed"}, 405)
            return True

        if path == "/api/context":
            self._send_json(get_full_context())
        elif path == "/api/riders":
            self._send_json(get_riders())
        elif path == "/api/drivers":
            self._send_json(get_drivers())
        elif path == "/api/merchants":
            self._send_json(get_merchants())
        elif path.startswith("/api/variants"):
            parts = path.split("/")
            city_id = parts[3] if len(parts) > 3 else None
            self._send_json(get_variants(city_id))
        elif path == "/api/collections":
            self._send_json(scan_collections())
        elif path.startswith("/api/collection/"):
            parts = path.split("/")
            if len(parts) >= 5:
                directory = parts[3]
                filename = "/".join(parts[4:])
                data = get_collection_file(directory, filename)
                if data:
                    self._send_json(data)
                else:
                    self._send_json({"error": "collection not found"}, 404)
            else:
                self._send_json(
                    {"error": "usage: /api/collection/<dir>/<file>"}, 400)
        elif path == "/api/load-test/tokens":
            tokens_dir = Path(__file__).parent.parent.parent.parent / "load-test" / "tokens"
            try:
                riders  = json.loads((tokens_dir / "riderTokens.json").read_text())
                drivers = json.loads((tokens_dir / "driverTokens.json").read_text())
                self._send_json({
                    "riders": riders,
                    "drivers": drivers,
                    "riderCount": len(riders),
                    "driverCount": len(drivers),
                })
            except Exception as e:
                self._send_json({"error": str(e), "riders": [], "drivers": [], "riderCount": 0, "driverCount": 0}, 500)
            return
        elif path == "/api/health":
            self._send_json({"status": "ok"})
        elif path == "/api/services-ready":
            self._send_json(_probe_haskell_services())
        # ── Runtime port discovery ──
        # The dashboard fetches /api/ports on first paint to learn the
        # per-user port mapping (after resolve-ports.sh has overlaid the
        # base ports.nix with a free-port offset). Without this, CRA bakes
        # the default ports into the build and the dashboard can't reach
        # remapped services.
        elif path == "/api/ports":
            self._send_json(_read_ports_table())
        # ── Finance Dashboard ──
        elif path == "/api/finance/dashboard":
            from urllib.parse import parse_qs
            qs = parse_qs(parsed.query)
            side = qs.get("side", ["bpp"])[0]
            schema = "atlas_app" if side == "bap" else "atlas_driver_offer_bpp"
            self._send_json(get_finance_dashboard(schema))
        # ── Finance Visualization (granular) ──
        elif path == "/api/finance/accounts":
            from urllib.parse import parse_qs
            qs = parse_qs(parsed.query)
            side = qs.get("side", ["bpp"])[0]
            schema = "atlas_app" if side == "bap" else "atlas_driver_offer_bpp"
            self._send_json(get_finance_accounts(schema))
        elif path == "/api/finance/ledger":
            from urllib.parse import parse_qs
            qs = parse_qs(parsed.query)
            side = qs.get("side", ["bpp"])[0]
            schema = "atlas_app" if side == "bap" else "atlas_driver_offer_bpp"
            self._send_json(get_finance_ledger_entries(
                schema,
                reference_type=qs.get("referenceType", [None])[0],
                reference_id=qs.get("referenceId", [None])[0],
                counterparty_id=qs.get("counterpartyId", [None])[0],
                limit=int(qs.get("limit", [50])[0]),
                offset=int(qs.get("offset", [0])[0]),
            ))
        elif path == "/api/finance/invoices":
            from urllib.parse import parse_qs
            qs = parse_qs(parsed.query)
            side = qs.get("side", ["bpp"])[0]
            schema = "atlas_app" if side == "bap" else "atlas_driver_offer_bpp"
            self._send_json(get_finance_invoices(
                schema,
                issued_to_id=qs.get("issuedToId", [None])[0],
                invoice_type=qs.get("invoiceType", [None])[0],
                limit=int(qs.get("limit", [20])[0]),
                offset=int(qs.get("offset", [0])[0]),
            ))
        elif path == "/api/finance/earnings":
            from urllib.parse import parse_qs
            qs = parse_qs(parsed.query)
            schema = "atlas_driver_offer_bpp"  # earnings only on BPP
            self._send_json(get_finance_earnings(
                schema,
                counterparty_id=qs.get("counterpartyId", [None])[0],
            ))
        elif path == "/api/finance/reference-types":
            from urllib.parse import parse_qs
            qs = parse_qs(parsed.query)
            side = qs.get("side", ["bpp"])[0]
            schema = "atlas_app" if side == "bap" else "atlas_driver_offer_bpp"
            self._send_json(get_finance_reference_types(schema))
        # ── Metrics read (proxy to VictoriaMetrics for dashboard charts) ────
        # GETs only — query / query_range / labels / series. Forwards the
        # query string verbatim. Same shape as Prometheus's HTTP API so the
        # dashboard can treat this endpoint as a drop-in.
        elif method == "GET" and path.startswith("/api/metrics/v1/"):
            vm_path = path[len("/api/metrics"):]  # → "/v1/query_range"
            target = f"{VICTORIA_METRICS_QUERY_URL}/api{vm_path}"
            qs = parsed.query
            if qs:
                target = f"{target}?{qs}"
            try:
                with urllib.request.urlopen(target, timeout=15) as r:
                    body = r.read()
                    self.send_response(r.status)
                    self.send_header("Content-Type", r.headers.get("Content-Type", "application/json"))
                    self.send_header("Content-Length", str(len(body)))
                    self._cors_headers()
                    self.end_headers()
                    self.wfile.write(body)
            except urllib.error.HTTPError as e:
                self._send_json({"error": f"VM {e.code}", "detail": e.read().decode("utf-8", "ignore")}, e.code)
            except Exception as e:
                self._send_json({"error": "VM unreachable", "detail": str(e)}, 502)
            return True

        # ── Metrics push (from dashboard after suite completes) ──────────────
        elif method == "POST" and path in ("/api/metrics/push", "/metrics/push"):
            body = self._read_json_body()
            version_id = body.get("versionId", "unknown")
            collection = body.get("collection", "unknown")
            env = body.get("env", "Master")
            suite = body.get("suite", "unknown")
            steps = body.get("steps", [])
            all_passed = body.get("allPassed", False)
            result = push_suite_metrics(version_id, collection, env, suite, steps, all_passed)
            push_ok = result[0] if result else False
            push_msg = result[1] if result else "skipped"
            suite_label = f"{collection}-{env}-{suite}".replace(" ", "_")
            self._send_json({"ok": push_ok, "pushed": push_ok, "msg": push_msg, "suite": suite_label, "versionId": version_id})
            return True

        # ── Webhook: trigger run-all Master collections ───────────────────
        elif method == "POST" and path in ("/api/webhook/run-all", "/webhook/run-all"):
            body = self._read_json_body()
            version_id = body.get("versionId", "unknown")
            if not version_id or version_id == "unknown":
                self._send_json({"error": "versionId is required"}, 400)
                return True
            import uuid as _uuid, time as _time
            run_id = f"run-{int(_time.time())}-{str(_uuid.uuid4())[:8]}"
            t = threading.Thread(
                target=_run_all_master_webhook,
                args=(run_id, version_id),
                daemon=True
            )
            t.start()
            self._send_json({"runId": run_id, "versionId": version_id, "status": "started"})
            return True

        elif method == "GET" and path in ("/api/webhook/run-all/status", "/webhook/run-all/status"):
            from urllib.parse import parse_qs
            qs = parse_qs(parsed.query)
            run_id = qs.get("runId", [None])[0]
            if not run_id:
                self._send_json({"error": "runId required"}, 400)
                return True
            with _webhook_lock:
                state = _webhook_runs.get(run_id)
            if not state:
                self._send_json({"error": "run not found"}, 404)
                return True
            self._send_json(state)
            return True

        else:
            self._send_json({"error": "not found"}, 404)
        return True

    def do_GET(self):
        self._handle("GET")

    def do_POST(self):
        self._handle("POST")

    def do_PUT(self):
        self._handle("PUT")

    def do_DELETE(self):
        self._handle("DELETE")


# ── Metrics / VictoriaMetrics ─────────────────────────────────────────────────

def _push_metrics_to_vm(lines: list[str]) -> tuple[bool, str]:
    if not VICTORIA_METRICS_URL:
        return False, "VICTORIA_METRICS_URL not set — metrics disabled"
    payload = "\n".join(lines) + "\n"
    try:
        req = urllib.request.Request(
            VICTORIA_METRICS_URL,
            data=payload.encode("utf-8"),
            headers={"Content-Type": "text/plain"},
            method="POST",
        )
        with urllib.request.urlopen(req, timeout=10) as r:
            r.read()
        return True, f"OK — {len(lines)} metrics sent to {VICTORIA_METRICS_URL}"
    except Exception as e:
        msg = str(e)
        if "Name or service not known" in msg or "nodename nor servname" in msg:
            msg = (f"VictoriaMetrics unreachable ({VICTORIA_METRICS_URL}) — "
                   "this is expected when running locally outside K8s. "
                   "Set VICTORIA_METRICS_URL to push from local.")
        return False, msg


def push_suite_metrics(version_id: str, collection: str, env: str, suite: str,
                       steps: list[dict], all_passed: bool):
    import time as _time
    ts_ms = int(_time.time() * 1000)
    suite_label = f"{collection}-{env}-{suite}".replace(" ", "_")
    status_str = "pass" if all_passed else "fail"
    labels_base = f'version="{version_id}",collection="{collection}",env="{env}",suite="{suite_label}"'

    lines = [
        # Always-on counter so the dashboard can chart pass/fail rate per suite.
        # Emitted as a 1-sample gauge; Grafana / dashboard sums by status over a
        # window. (True counters require monotonic state we don't have here.)
        f'integration_test_suite_runs{{{labels_base},status="{status_str}"}} 1 {ts_ms}'
    ]

    if all_passed:
        total_ms = 0
        for step in steps:
            elapsed = step.get("elapsed_ms", 0)
            total_ms += elapsed
            api_name = f"{step.get('method','?')} {step.get('path','?')}".replace('"', "'")
            lines.append(
                f'integration_test_api_latency_ms{{{labels_base},api="{api_name}"}} '
                f'{elapsed} {ts_ms}'
            )
        lines.append(
            f'integration_test_suite_latency_ms{{{labels_base}}} '
            f'{total_ms} {ts_ms}'
        )

    ok, msg = _push_metrics_to_vm(lines)
    status = 'PASS' if all_passed else 'FAIL'
    if ok:
        print(f"  [metrics] ✓ suite={suite_label} status={status} version={version_id} — {msg}")
    else:
        print(f"  [metrics] ✗ suite={suite_label} status={status} version={version_id} — {msg}")
    return ok, msg


# ── Webhook: run all Master collections ───────────────────────────────────────

_webhook_runs: dict = {}  # runId → { status, progress, errors }
_webhook_lock = threading.Lock()


def _run_all_master_webhook(run_id: str, version_id: str):
    import subprocess as _sp, time as _time

    with _webhook_lock:
        _webhook_runs[run_id] = {"status": "running", "total": 0, "done": 0,
                                  "passed": 0, "failed": 0, "errors": []}

    results = []
    for coll_dir in sorted(COLLECTIONS_DIR.iterdir()):
        if not coll_dir.is_dir():
            continue
        master_dir = coll_dir / "Master"
        if not master_dir.is_dir():
            continue
        env_files = sorted(master_dir.glob("Master_*.postman_environment.json"))
        coll_files = [f for f in sorted(coll_dir.glob("*.json"))
                      if not f.name.endswith(".postman_environment.json")]
        for env_file in env_files:
            city = env_file.stem.split("_", 1)[1] if "_" in env_file.stem else env_file.stem
            for coll_file in coll_files:
                with _webhook_lock:
                    _webhook_runs[run_id]["total"] += 1

    total = _webhook_runs[run_id]["total"]

    for coll_dir in sorted(COLLECTIONS_DIR.iterdir()):
        if not coll_dir.is_dir():
            continue
        master_dir = coll_dir / "Master"
        if not master_dir.is_dir():
            continue
        env_files = sorted(master_dir.glob("Master_*.postman_environment.json"))
        coll_files = [f for f in sorted(coll_dir.glob("*.json"))
                      if not f.name.endswith(".postman_environment.json")]

        for env_file in env_files:
            city = env_file.stem.replace("Master_", "")
            for coll_file in coll_files:
                suite_name = coll_dir.name
                flow_name = coll_file.stem
                report_file = Path("/tmp") / f"ny_webhook_{run_id}_{suite_name}_{flow_name}_{city}.json"
                resolved_env_file = _materialize_env_for_newman(env_file)
                try:
                    proc = _sp.run(
                        ["newman", "run", str(coll_file), "-e", str(resolved_env_file),
                         "--bail", "--timeout-request", "60000",
                         "--reporters", "json",
                         "--reporter-json-export", str(report_file)],
                        capture_output=True, text=True, timeout=300
                    )
                    passed = proc.returncode == 0
                    steps = []
                    if report_file.exists():
                        try:
                            rpt = json.loads(report_file.read_text())
                            for ex in rpt.get("run", {}).get("executions", []):
                                item = ex.get("item", {})
                                resp = ex.get("response", {})
                                url = resp.get("responseTime", 0)
                                req = ex.get("request", {})
                                req_url = req.get("url", {})
                                raw = req_url.get("raw", "") if isinstance(req_url, dict) else str(req_url)
                                # strip host, keep path
                                try:
                                    from urllib.parse import urlparse as _up
                                    path = _up(raw).path or raw
                                except Exception:
                                    path = raw
                                steps.append({
                                    "name": item.get("name", "?"),
                                    "method": req.get("method", "GET"),
                                    "path": path,
                                    "elapsed_ms": resp.get("responseTime", 0),
                                })
                            report_file.unlink(missing_ok=True)
                        except Exception:
                            pass
                    push_suite_metrics(version_id, suite_name, "Master", flow_name,
                                       steps, passed)
                    with _webhook_lock:
                        _webhook_runs[run_id]["done"] += 1
                        if passed:
                            _webhook_runs[run_id]["passed"] += 1
                        else:
                            _webhook_runs[run_id]["failed"] += 1
                            _webhook_runs[run_id]["errors"].append(
                                f"{suite_name}/{flow_name}/{city}")
                except Exception as e:
                    with _webhook_lock:
                        _webhook_runs[run_id]["done"] += 1
                        _webhook_runs[run_id]["failed"] += 1
                        _webhook_runs[run_id]["errors"].append(
                            f"{suite_name}/{flow_name}/{city}: {e}")
                finally:
                    # Remove the materialized env-file (only if we wrote one)
                    if resolved_env_file != env_file:
                        try: resolved_env_file.unlink(missing_ok=True)
                        except OSError: pass

    with _webhook_lock:
        _webhook_runs[run_id]["status"] = "done"
    print(f"  [webhook] run {run_id} complete: "
          f"{_webhook_runs[run_id]['passed']}/{total} passed")


def main():
    port = PORT
    for i, arg in enumerate(sys.argv):
        if arg == "--port" and i + 1 < len(sys.argv):
            port = int(sys.argv[i + 1])

    # ThreadingHTTPServer (stdlib since 3.7) spawns one daemon thread
    # per request. Without it, EVERY request serialises through the
    # accept loop — long-running endpoints (config-sync streaming the
    # SQL apply log, /api/ny-react-native/status mid-build, the
    # control-center launcher streaming vite output) block other
    # callers like /api/collections, which then time out at the
    # browser's default 30s.
    #
    # GIL note: this doesn't parallelise CPU work, but every endpoint
    # we serve is IO-bound (subprocess stdout reads, file polls,
    # subprocess.run, urllib calls, JSON parse) — IO releases the GIL,
    # so threading delivers real concurrency for our workload.
    #
    # Thread safety: every shared mutable global (`_ny_rn_states`,
    # `_control_center_state`, `_config_sync_state`, log buffers,
    # status_store) is already guarded by its own `threading.Lock` /
    # `threading.RLock`. New routes adding shared state must follow
    # that discipline.
    server = ThreadingHTTPServer(("0.0.0.0", port), ContextHandler)
    # Daemon = True so threads don't block process shutdown when the
    # user Ctrl-C's the server.
    server.daemon_threads = True
    print(
        f"\n  \033[93m📋 Context API + CORS Proxy on http://localhost:{port} (threaded)\033[0m")
    print(
        f"  DB: {DB_CONFIG['user']}@{DB_CONFIG['host']}:{DB_CONFIG['port']}/{DB_CONFIG['dbname']}")
    print(f"  Proxy: /proxy/rider/* → {RIDER_URL}")
    print(f"  Proxy: /proxy/driver/* → {DRIVER_URL}")
    print(f"  Config-sync envs: {list(CONFIG_SYNC_ENVS)}  (default: {CONFIG_SYNC_DEFAULT_FROM})\n")

    run_startup_local_testing_data()

    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print("\nShutdown.")
        server.server_close()


if __name__ == "__main__":
    main()
