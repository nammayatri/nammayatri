#!/usr/bin/env python3
"""
Local System API

Hosts the repo launcher endpoints:
  - control-center launcher (/api/control-center/{status,start,stop})
  - ny-react-native launcher (/api/ny-react-native/{options,status,start,stop,
    clear-cache,open-debugger,metro-log,_adb-diag,logcat})
  - git ref picker (/api/git/refs)

Port: 7083
"""

import fnmatch
import hashlib
import json
import shutil
import sys
import os
import subprocess
import threading
import time
from pathlib import Path
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from urllib.parse import urlparse
import urllib.parse
import re
import shlex
import signal
import urllib.request
import urllib.error

# ── Generic spec-driven launcher (additive: coexists with control-center /
# ny-react-native handlers below; the legacy handlers are scheduled for
# removal once the spec-driven Tools panel is validated end-to-end).
import spec_loader
import stage_runner
import log_sources
import input_store

# ── Load-test execution engine ───────────────────────────────────────────────
_LT_SERVICE_DIR = Path(__file__).resolve().parent.parent / "load-test-service"
if str(_LT_SERVICE_DIR) not in sys.path:
    sys.path.insert(0, str(_LT_SERVICE_DIR))
try:
    import runner as _lt_runner
    _LT_AVAILABLE = True
except Exception as _lt_err:
    _LT_AVAILABLE = False
    _lt_runner = None  # type: ignore

try:
    import locust_runner as _lt_locust_runner
    _LT_LOCUST_AVAILABLE = True
except Exception:
    _LT_LOCUST_AVAILABLE = False
    _lt_locust_runner = None  # type: ignore

sys.stdout.reconfigure(line_buffering=True)
sys.stderr.reconfigure(line_buffering=True)

PORT = 7083

# ── Paths ──
SCRIPT_DIR = Path(__file__).resolve().parent
PROJECT_ROOT = SCRIPT_DIR.parent.parent.parent.parent  # nammayatri/

KNOWN_REPOS: dict = {
    "nammayatri/ny-react-native": PROJECT_ROOT / "data" / "ny-react-native",
    "nammayatri/control-center":  PROJECT_ROOT / "data" / "control-center",
}

# ── Control-center launcher ──
CONTROL_CENTER_SETUP_SCRIPT = (
    PROJECT_ROOT / "Backend" / "dev" / "test-tool" / "context-api"
    / "setup" / "control_center" / "frontend" / "setup.sh"
)
CONTROL_CENTER_URL = os.environ.get("CONTROL_CENTER_URL", "http://localhost:5173")
CONTROL_CENTER_MAX_LOG_LINES = 4000

_control_center_state = {
    "running": False,
    "ready": False,
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


def _browse_folder(initial: str) -> dict:
    """Open a native folder-picker dialog and return the absolute path.

    Falls back across platforms: AppleScript on macOS, zenity/kdialog on Linux.
    """
    try:
        start = os.path.abspath(os.path.expanduser(initial or "~"))
    except Exception:
        start = os.path.expanduser("~")
    if not os.path.isdir(start):
        start = os.path.expanduser("~")

    plat = sys.platform
    try:
        if plat == "darwin":
            script = (
                'set theFolder to choose folder with prompt "Select local checkout"'
                f' default location (POSIX file "{start}")\n'
                "POSIX path of theFolder"
            )
            res = subprocess.run(
                ["osascript", "-e", script],
                capture_output=True, text=True, timeout=300,
            )
            if res.returncode != 0:
                return {"path": None, "cancelled": True}
            picked = res.stdout.strip().rstrip("/")
            return {"path": picked or None}
        for cmd in (
            ["zenity", "--file-selection", "--directory", f"--filename={start}/"],
            ["kdialog", "--getexistingdirectory", start],
        ):
            if shutil.which(cmd[0]):
                res = subprocess.run(cmd, capture_output=True, text=True, timeout=300)
                if res.returncode != 0:
                    return {"path": None, "cancelled": True}
                picked = res.stdout.strip().rstrip("/")
                return {"path": picked or None}
        return {"path": None, "error": "no folder picker available on this platform"}
    except subprocess.TimeoutExpired:
        return {"path": None, "error": "folder picker timed out"}
    except Exception as e:  # noqa: BLE001
        return {"path": None, "error": str(e)}


def _probe_control_center_ready() -> bool:
    try:
        req = urllib.request.Request(CONTROL_CENTER_URL, method="GET")
        with urllib.request.urlopen(req, timeout=1) as r:
            return 200 <= r.status < 500
    except urllib.error.HTTPError as e:
        return 200 <= e.code < 500
    except Exception:
        return False


def run_control_center_setup(ref: str | None = None):
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
            start_new_session=True,
        )
        _control_center_proc = p
        with _control_center_lock:
            _control_center_state["pid"] = p.pid

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
    with _control_center_lock:
        if _control_center_state["running"]:
            return False
    _cleanup_stale_control_center()
    threading.Thread(target=run_control_center_setup, args=(ref,), daemon=True).start()
    return True


# ── ny-react-native launcher ──
NY_RN_SETUP_SCRIPT = (
    PROJECT_ROOT / "Backend" / "dev" / "test-tool" / "context-api"
    / "setup" / "ny_react_native" / "setup.sh"
)
NY_RN_VALID_APPS = {"customer", "driver"}


def _resolve_adb_bin() -> str:
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

NY_RN_DEFAULT_VARIANTS = {
    "customer": ["Bridge", "NammaYatri", "Yatri", "ManaYatri", "YatriSathi", "Lynx", "BharatTaxi"],
    "driver":   ["nammaYatri", "jatriSaathi", "bridge", "manaYatri", "yatri", "lynx", "bharatTaxi"],
}
NY_RN_DEFAULT_VARIANT_BY_APP = {"customer": "Bridge", "driver": "nammaYatri"}


def _ny_rn_repo_dir() -> Path:
    override = os.environ.get("NY_RN_PATH")
    if override:
        return Path(override)
    return PROJECT_ROOT / "data" / "ny-react-native"


def _parse_gradle_product_flavors(gradle_text: str) -> list:
    import re
    m = re.search(r"productFlavors\s*\{", gradle_text)
    if not m:
        return []
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


def _git_repo_refs(repo_path: Path, github_repo: str | None = None,
                   q: str | None = None, branch_limit: int = 80,
                   commit_limit: int = 30) -> dict:
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

    if (repo_path / ".git").is_dir():
        out["cloned"] = True
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


_ny_rn_reverse_events: dict = {a: None for a in NY_RN_VALID_APPS}
_ny_rn_reverse_threads: dict = {a: None for a in NY_RN_VALID_APPS}
NY_RN_REVERSE_PORTS = [
    8013,
    8016,
    8088,
    8089,
    50051,
]
NY_RN_REVERSE_INTERVAL_S = 5.0


def _apply_adb_reverses(ports):
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
    ev = _ny_rn_reverse_events.get(app)
    if ev is not None:
        ev.set()
    _ny_rn_reverse_events[app] = None
    _ny_rn_reverse_threads[app] = None


def _probe_ny_rn_ready(app: str) -> bool:
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


def _pkill_pattern(pattern: str) -> int:
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
    sub = "consumer" if app == "customer" else "provider"
    print(f"  [ny-react-native:{app}] pre-launch cleanup of stale processes…")

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

    patterns = [
        rf"react-native start.*ny-rn-metro-{sub}\.log",
        rf"react-native start.*--port.*data/ny-react-native/{sub}",
        rf"xcodebuild.*data/ny-react-native/{sub}/ios",
        rf"pod install.*data/ny-react-native/{sub}/ios",
        rf"GradleDaemon.*data/ny-react-native/{sub}/android",
    ]
    killed = 0
    for pat in patterns:
        killed += _pkill_pattern(pat)

    metro_port = 8088 if sub == "consumer" else 8089
    freed = _free_tcp_port(metro_port)

    print(
        f"  [ny-react-native:{app}] cleanup: killed {killed} pattern-match procs, "
        f"freed {freed} on tcp:{metro_port}"
    )


def _cleanup_stale_control_center() -> None:
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
        r"control-center/frontend/setup\.sh",
        r"vite.*data/control-center",
        r"npm.*run dev.*data/control-center",
    ]
    killed = 0
    for pat in patterns:
        killed += _pkill_pattern(pat)

    freed = _free_tcp_port(5173)
    print(
        f"  [control-center] cleanup: killed {killed} pattern-match procs, "
        f"freed {freed} on tcp:5173"
    )


def trigger_ny_rn(app: str, platform: str, variant: str, ref: str | None = None,
                  firebase_override_path: str | None = None):
    if app not in _ny_rn_states:
        return False
    with _ny_rn_locks[app]:
        if _ny_rn_states[app]["running"]:
            return False
    _cleanup_stale_ny_rn(app)
    threading.Thread(
        target=run_ny_rn_setup,
        args=(app, platform, variant, ref, firebase_override_path),
        daemon=True,
    ).start()
    return True


def _list_descendants(pid: int) -> list:
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
    if proc is None or proc.poll() is not None:
        return False
    import signal as _signal
    pid = proc.pid

    try:
        pgid = os.getpgid(pid)
    except Exception:
        pgid = None

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

    for _ in range(30):
        if proc.poll() is not None:
            survivors = _list_descendants(pid)
            if not survivors:
                return True
        time.sleep(0.1)

    descendants = list(set(descendants + _list_descendants(pid)))
    _signal_all(_signal.SIGKILL)
    return True


def stop_control_center() -> bool:
    global _control_center_proc
    with _control_center_lock:
        proc = _control_center_proc
        running = _control_center_state["running"]
    if not running or proc is None:
        return False
    return _kill_process_group(proc)


def stop_ny_rn(app: str) -> bool:
    if app not in _ny_rn_states:
        return False
    with _ny_rn_locks[app]:
        proc = _ny_rn_procs.get(app)
        running = _ny_rn_states[app]["running"]
    if not running or proc is None:
        return False
    return _kill_process_group(proc)


# ── Remote SSH + PTY runner ──
#
# Launches `, run-mobility-stack-dev` (or any command) on a remote host over
# SSH inside a PTY, so the dashboard's xterm can drive it interactively.
# Localhost is special-cased to skip SSH/rsync entirely.
#
# stdlib only: pty / fcntl / termios / struct / subprocess / threading.

import base64 as _base64
import fcntl as _fcntl
import pty as _pty
import select as _select
import struct as _struct
import termios as _termios
import uuid as _uuid
from collections import deque as _deque

# Build artefacts: never copied, and exactly what `cabal clean` wipes on the
# stack host (see remote_cabal_clean) — one list so the two can't drift.
BUILD_STATE_EXCLUDES = [
    "dist-newstyle",
    "dist",
    "_build",
    "result", "result-*",
    ".cabal-dir",
    ".hie",
    "hie",
    ".nix-deps",
    ".ci-project-root",
    ".ci-cabal-dir",
    ".ci-cache-sha",
    "cabal.project.local",
]

# Per-machine state: never copied, and never deleted by cabal clean either.
# Everything cabal clean wipes — the build-state list minus files a developer
# may have hand-written (cabal.project.local is theirs, not a build artefact).
CABAL_CLEAN_TARGETS = [e for e in BUILD_STATE_EXCLUDES if e != "cabal.project.local"]

# Per-machine state: never copied, and never deleted by cabal clean either.
# data/ holds the stack host's own published port map (data/devbox-ports.json)
# plus its service state, so excluding it keeps a redeploy from clobbering them.
MACHINE_STATE_EXCLUDES = [
    "data", "node_modules", ".direnv",
    ".git",
    "*.log",
    ".stack-state.json",
    "Frontend/android-native", "Frontend/ios",
    "Frontend/build", "Frontend/dist",
]

# Single source of truth for what never leaves this machine. Used both as the
# rsync --exclude list AND as the filter for the workspace fingerprint that
# decides whether a deploy is needed — adding an entry here automatically stops
# it from triggering a redeploy.
REMOTE_EXCLUDES = BUILD_STATE_EXCLUDES + MACHINE_STATE_EXCLUDES
REMOTE_DEFAULT_DIR = "/tmp/nammayatri"
REMOTE_DEFAULT_COMMAND = (
    "cd Backend && nix develop .#backend -c , run-mobility-stack-dev"
)
REMOTE_BUFFER_BYTES = 256 * 1024  # last ~256 KB per session for re-attach

# ── SSH key + ServiceDiscovery helpers ──

_SSH_KEY_CANDIDATES = [
    os.path.expanduser("~/.ssh/id_ed25519"),
    os.path.expanduser("~/.ssh/id_rsa"),
    os.path.expanduser("~/.ssh/id_ecdsa"),
]
BASE_API_HOST = os.environ.get("BASE_API_HOST", "34.100.155.111")
BASE_API_PORT = os.environ.get("BASE_API_PORT", "8787")


def _find_ssh_key() -> str | None:
    """Return path to the first existing SSH private key, or None."""
    for k in _SSH_KEY_CANDIDATES:
        if os.path.isfile(k):
            return k
    return None


def _ensure_ssh_key() -> tuple[str, str]:
    """Find or generate an SSH key. Returns (private_key_path, public_key_str)."""
    existing = _find_ssh_key()
    if existing:
        pub = existing + ".pub"
        if os.path.isfile(pub):
            with open(pub) as f:
                return existing, f.read().strip()
        # Private key exists but no .pub — regenerate public from private
        result = subprocess.run(
            ["ssh-keygen", "-y", "-f", existing],
            capture_output=True, text=True, check=True,
        )
        with open(pub, "w") as f:
            f.write(result.stdout.strip() + "\n")
        return existing, result.stdout.strip()

    # No key found — generate ~/.ssh/id_ed25519
    key_path = _SSH_KEY_CANDIDATES[0]
    os.makedirs(os.path.dirname(key_path), exist_ok=True)
    subprocess.run(
        ["ssh-keygen", "-t", "ed25519", "-f", key_path,
         "-N", "", "-C", f"{os.environ.get('USER', 'user')}@nammayatri"],
        capture_output=True, check=True,
    )
    with open(key_path + ".pub") as f:
        return key_path, f.read().strip()


def _fetch_machines() -> dict:
    """Fetch ServiceDiscovery from the base station HTTP API and return
    a list of MachineInfo dicts the frontend can render in a dropdown.

    Calls  GET http://<BASE_API_HOST>:<BASE_API_PORT>/api/status
    which returns:
      { "base": {localIp, awsIp, name, username, resources, usage},
        "workers": [{localIp, awsIp, name, username, type, resources, usage}, …] }

    Transforms that into:
      { "machines": [MachineInfo, …], "myIps": [...] }
    where MachineInfo = {name, role, localIp, awsIp, bestIp, user, type, resources}
    """
    if not BASE_API_HOST:
        return {"machines": [], "myIps": [],
                "error": "BASE_API_HOST not set — cannot reach base station API"}

    url = f"http://{BASE_API_HOST}:{BASE_API_PORT}/api/status"
    try:
        with urllib.request.urlopen(url, timeout=5) as r:
            data = json.loads(r.read().decode())
    except Exception as e:
        return {"machines": [], "myIps": [], "error": f"base station API unreachable: {e}"}

    # Collect this machine's IPs + networks so we can pick the best route.
    import ipaddress
    my_networks: list[tuple[str, ipaddress.IPv4Network]] = []
    try:
        if sys.platform == "darwin":
            out = subprocess.check_output(["ifconfig"], text=True, timeout=3)
            # Parse "inet <ip> netmask <hex>" pairs from ifconfig output
            for m in re.finditer(r"inet (\d+\.\d+\.\d+\.\d+) netmask (0x[0-9a-fA-F]+)", out):
                ip_str, mask_hex = m.group(1), m.group(2)
                if ip_str == "127.0.0.1":
                    continue
                mask_int = int(mask_hex, 16)
                prefix = bin(mask_int).count("1")
                net = ipaddress.ip_network(f"{ip_str}/{prefix}", strict=False)
                my_networks.append((ip_str, net))
        else:
            out = subprocess.check_output(["hostname", "-I"], text=True, timeout=3).strip()
            for ip_str in out.split():
                ip_str = ip_str.strip()
                if ip_str:
                    # Default /24 for Linux (hostname -I doesn't give netmask)
                    net = ipaddress.ip_network(f"{ip_str}/24", strict=False)
                    my_networks.append((ip_str, net))
    except Exception:
        pass
    my_ips = [ip for ip, _ in my_networks]

    def _pick_best(local_ip: str, aws_ip: str) -> str:
        """If any of our IPs is on the same subnet as local_ip, use LAN; else VPN."""
        if not local_ip:
            return aws_ip or local_ip
        try:
            target = ipaddress.ip_address(local_ip)
            for _, net in my_networks:
                if target in net:
                    return local_ip
        except ValueError:
            pass
        return aws_ip or local_ip

    machines: list[dict] = []

    base = data.get("base") or {}
    if base:
        best = _pick_best(base.get("localIp", ""), base.get("awsIp", ""))
        machines.append({
            "name":      base.get("name", "base"),
            "role":      "base",
            "localIp":   base.get("localIp", ""),
            "awsIp":     base.get("awsIp", ""),
            "bestIp":    best,
            "user":      base.get("username", "ubuntu"),
            "type":      base.get("type", ""),
            "resources": base.get("resources", {}),
            "usage":     base.get("usage", {}),
        })

    for w in data.get("workers", []):
        best = _pick_best(w.get("localIp", ""), w.get("awsIp", ""))
        machines.append({
            "name":      w.get("name", ""),
            "role":      "worker",
            "localIp":   w.get("localIp", ""),
            "awsIp":     w.get("awsIp", ""),
            "bestIp":    best,
            "user":      w.get("username", "ubuntu"),
            "type":      w.get("type", ""),
            "resources": w.get("resources", {}),
            "usage":     w.get("usage", {}),
        })

    return {"machines": machines, "myIps": my_ips}


# ── Devbox auto-assignment ────────────────────────────────────────────────────

DEVBOX_ID_FILE = PROJECT_ROOT / ".devbox-id.json"

def _parse_ram_pct(usage: dict) -> float:
    """'27.5Gi (88%)' -> 88.0. Unknown/missing -> 100.0 (least preferred)."""
    try:
        m = re.search(r"\((\d+(?:\.\d+)?)%\)", str((usage or {}).get("ram", "")))
        return float(m.group(1)) if m else 100.0
    except Exception:
        return 100.0


def _devbox_resolve(force_new: bool = False) -> dict:
    """Return this checkout's devbox assignment, creating one if needed."""
    res = _fetch_machines()
    if res.get("error"):
        return {"error": res["error"]}
    devboxes = [m for m in res.get("machines", [])
                if m.get("type") == "dev-box" and m.get("role") == "worker"]
    if not devboxes:
        return {"error": "no dev-box machines registered with the base station"}

    saved = None
    if not force_new and DEVBOX_ID_FILE.is_file():
        try:
            saved = json.loads(DEVBOX_ID_FILE.read_text())
        except Exception:
            saved = None  # corrupt file — regenerate below

    machine = None
    repinned = False
    if saved and saved.get("machine"):
        machine = next((m for m in devboxes if m.get("name") == saved["machine"]), None)
        if machine is None:
            repinned = True  # pinned machine dropped off the fleet — re-pick

    created = False
    if machine is None:
        machine = min(devboxes, key=lambda m: _parse_ram_pct(m.get("usage")))
        local_user = re.sub(r"[^a-z0-9]+", "",
                            (os.environ.get("USER") or "dev").lower()) or "dev"
        mslug = re.sub(r"[^a-z0-9]+", "-", machine["name"].lower()).strip("-")[:24]
        dev_id = f"{local_user}-{mslug}-{_uuid.uuid4().hex[:6]}"
        created = saved is None
        saved = {
            "id": dev_id,
            "machine": machine["name"],
            "sshUser": machine.get("user") or "",
            "localUser": local_user,
            "createdAt": time.strftime("%Y-%m-%dT%H:%M:%S"),
        }

    dev_id = saved["id"]
    host = machine.get("bestIp") or machine.get("localIp") or ""
    ssh_user = machine.get("user") or saved.get("sshUser") or ""
    remote_dir = f"/tmp/{dev_id}/nammayatri"

    # Persist the connection coordinates too: this file is the ONLY local record
    # of where the devbox is. get_devbox_ports() reads host/sshUser/sshPort/
    # remoteDir from here to SSH in and cat the stack's data/devbox-ports.json.
    merged = {**saved, "machine": machine.get("name", saved.get("machine", "")),
              "sshUser": ssh_user, "host": host, "sshPort": 22,
              "remoteDir": remote_dir}
    if merged != saved:
        try:
            DEVBOX_ID_FILE.write_text(json.dumps(merged, indent=2) + "\n")
        except OSError as e:
            return {"error": f"cannot write {DEVBOX_ID_FILE}: {e}"}
        _devbox_ports_cache_clear()

    return {
        "id": dev_id,
        "machine": machine.get("name", ""),
        "host": host,
        "sshUser": ssh_user,
        "port": 22,
        "remoteDir": remote_dir,
        "copyMode": "rsync",
        "resources": machine.get("resources", {}),
        "usage": machine.get("usage", {}),
        "created": created,
        "repinned": repinned,
    }


def _ssh_reachable(user: str, host: str, port: int = 22, timeout: int = 10) -> bool:
    """True when key auth to user@host already works (no prompts)."""
    try:
        test = subprocess.run(
            ["ssh",
             "-o", "StrictHostKeyChecking=accept-new",
             "-o", "BatchMode=yes",
             "-o", "ConnectTimeout=5",
             "-p", str(port),
             f"{user}@{host}", "echo ok"],
            capture_output=True, text=True, timeout=timeout,
        )
        return test.returncode == 0
    except (subprocess.TimeoutExpired, OSError):
        return False


def _relay_authorize_key(target_user: str, target_host: str, pub_key: str) -> str | None:
    """Install pub_key on the target by hopping through a fleet machine we can
    already reach without a password (typically the base station, which usually
    has key access to every worker). Returns the relay's name on success, else
    None — callers fall back to the interactive ssh-copy-id session."""
    fleet = _fetch_machines().get("machines") or []
    quoted = shlex.quote(pub_key)
    install = (
        "mkdir -p ~/.ssh && chmod 700 ~/.ssh && "
        f"grep -qxF {quoted} ~/.ssh/authorized_keys 2>/dev/null || "
        f"echo {quoted} >> ~/.ssh/authorized_keys; "
        "chmod 600 ~/.ssh/authorized_keys"
    )
    for m in fleet:
        relay_host = m.get("bestIp") or m.get("localIp") or ""
        relay_user = m.get("user") or ""
        if not relay_host or not relay_user:
            continue
        if relay_host == target_host:
            continue
        if not _ssh_reachable(relay_user, relay_host):
            continue
        inner = (
            "ssh -o StrictHostKeyChecking=accept-new -o BatchMode=yes "
            f"-o ConnectTimeout=5 {shlex.quote(f'{target_user}@{target_host}')} "
            f"{shlex.quote(install)}"
        )
        try:
            res = subprocess.run(
                ["ssh", "-o", "StrictHostKeyChecking=accept-new", "-o", "BatchMode=yes",
                 "-o", "ConnectTimeout=5", f"{relay_user}@{relay_host}", inner],
                capture_output=True, text=True, timeout=25,
            )
        except (subprocess.TimeoutExpired, OSError):
            continue
        if res.returncode == 0 and _ssh_reachable(target_user, target_host):
            return m.get("name") or relay_host
    return None


def _setup_ssh(host: str, user: str, port: int = 22) -> dict:
    """Ensure we can SSH into the target machine, without asking the user to run
    anything by hand.

    Flow:
      1. Find or generate this machine's SSH key.
      2. Key auth already works → done.
      3. Try to install the key via a fleet machine we can already reach
         passwordlessly (usually the base station).
      4. Otherwise report needs_password — the dashboard then runs ssh-copy-id
         in its own terminal, where the user only types the devbox password.
    """
    key_path, pub_key = _ensure_ssh_key()

    if _ssh_reachable(user, host, port):
        return {
            "status": "ok",
            "message": f"SSH key authorized on {user}@{host}",
            "publicKey": pub_key,
        }

    relay = _relay_authorize_key(user, host, pub_key)
    if relay:
        return {
            "status": "ok",
            "message": f"SSH key installed on {user}@{host} via {relay}",
            "publicKey": pub_key,
            "viaRelay": relay,
        }

    return {
        "status": "needs_password",
        "message": f"SSH key not authorized on {user}@{host} yet — one-time password needed.",
        "command": f"ssh-copy-id -i {key_path} -p {port} {user}@{host}",
        "publicKey": pub_key,
    }


def remote_ssh_copy_id(body: dict) -> dict:
    """Run ssh-copy-id against the target in a PTY session, so the dashboard can
    prompt for the one-time password inline instead of sending the user to a
    terminal. Returns a session id the panel attaches its xterm to."""
    host = (body.get("host") or "").strip()
    user = (body.get("user") or "").strip()
    port = int(body.get("port") or 22)
    if not host or not user:
        return {"error": "host and user are required"}

    key_path, _ = _ensure_ssh_key()
    session = _remote_session_make("ssh-copy-id", host)
    _remote_register(session)

    argv = ["ssh-copy-id",
            "-o", "StrictHostKeyChecking=accept-new",
            "-i", key_path, "-p", str(port), f"{user}@{host}"]
    return _remote_spawn_pty(session, argv, cols=body.get("cols"), rows=body.get("rows"))


# ── Multi-user devbox registry ──
# Stored on the remote host so multiple developers can share a single box
# without stomping on each other's folders or Caddy ports.
REGISTRY_FILE = "/tmp/devbox-registry.json"

# Matches DEC private-mode set/reset: ESC [ ? <nums> h|l   (e.g. \x1b[?1049h,
# \x1b[?1000;1002;1006h). Used to snoop the PTY stream so we can replay the
# *latest* mode state on reattach — the byte ring drops the original startup
# escapes after a while, which leaves xterm out of alt-screen/mouse-tracking
# and the TUI unclickable.
_DECSET_RE = re.compile(rb"\x1b\[\?([\d;]+)([hl])")


def _snoop_modes(modes: dict, chunk: bytes) -> None:
    for m in _DECSET_RE.finditer(chunk):
        action = m.group(2)  # b'h' = set, b'l' = reset
        for n in m.group(1).split(b";"):
            if n:
                try:
                    modes[int(n)] = action
                except ValueError:
                    pass


def _trim_to_escape_boundary(buf: bytes) -> bytes:
    """Drop a leading partial escape sequence from a ring-buffer replay.

    The ring starts wherever the 256 KB window happens to begin, often inside
    an ESC sequence; feeding that to xterm makes it swallow the following
    printable bytes as sequence parameters. Start at the first line break
    instead (or the first ESC, whichever comes first) so the parser resyncs."""
    head = buf[:512]
    nl, esc = head.find(b"\n"), head.find(b"\x1b")
    if nl >= 0 and (esc < 0 or nl < esc):
        return buf[nl + 1:]          # resume after a line break
    if esc > 0:
        return buf[esc:]             # resume at a sequence start (keep the ESC)
    return buf


def _replay_modes_bytes(modes: dict) -> bytes:
    if not modes:
        return b""
    out = bytearray()
    for n, action in modes.items():
        out += b"\x1b[?" + str(n).encode() + action
    return bytes(out)

_remote_sessions: dict = {}        # session_id -> session dict
_remote_sessions_lock = threading.Lock()


def _is_localhost(host: str) -> bool:
    return (host or "").strip() in ("localhost", "127.0.0.1", "::1", "")


# ── Registry helpers ──


def _remote_read_registry(ssh_user: str, host: str, port: int, identity: str | None) -> dict:
    """Read the devbox registry JSON from the remote host via SSH."""
    cmd = f'cat {REGISTRY_FILE} 2>/dev/null || echo \'{{"users": {{}}}}\''
    argv = _ssh_argv(ssh_user, host, port, identity, want_tty=False) + [cmd]
    try:
        result = subprocess.run(argv, capture_output=True, timeout=10)
        if result.returncode == 0 and result.stdout.strip():
            return json.loads(result.stdout)
    except Exception:
        pass
    return {"users": {}}


def _remote_write_registry(ssh_user: str, host: str, port: int, identity: str | None, registry: dict) -> bool:
    """Write the devbox registry JSON to the remote host via SSH."""
    data = json.dumps(registry, indent=2)
    cmd = f"cat > {REGISTRY_FILE}"
    argv = _ssh_argv(ssh_user, host, port, identity, want_tty=False) + [cmd]
    try:
        result = subprocess.run(argv, input=data.encode(), capture_output=True, timeout=10)
        return result.returncode == 0
    except Exception:
        return False


_SAFE_NAME_RE = re.compile(r"^[a-zA-Z0-9_-]+$")


def _register_dev_user(ssh_user: str, host: str, port: int, identity: str | None, dev_name: str) -> dict:
    """Ensure dev_name is in the remote registry; return their entry."""
    if not _SAFE_NAME_RE.match(dev_name):
        raise ValueError(f"devName contains unsafe characters: {dev_name!r}")
    registry = _remote_read_registry(ssh_user, host, port, identity)
    users = registry.get("users", {})

    if dev_name in users:
        return users[dev_name]

    entry = {
        "dir": f"/tmp/{dev_name}/nammayatri",
    }
    users[dev_name] = entry
    registry["users"] = users
    _remote_write_registry(ssh_user, host, port, identity, registry)

    return entry


# ── Devbox port discovery (single source of truth) ──
#
# The stack's run-mobility-stack-dev preflight publishes its resolved port map
# at <workspace>/data/devbox-ports.json on the machine it runs on. Nothing is
# ever mirrored locally: every consumer — the dashboard's port table, Tools →
# Service Ports, launcher-spec ${ports.*} / ${host} — goes through
# get_devbox_ports(), which reads that one file (directly for a local stack,
# over SSH for a devbox, using the coordinates in .devbox-id.json).

DEVBOX_PORTS_RELPATH = "data/devbox-ports.json"
DEVBOX_PORTS_FILE = PROJECT_ROOT / "data" / "devbox-ports.json"
_DEVBOX_PORTS_TTL = 5.0

_devbox_ports_lock = threading.Lock()
_devbox_ports_cache: dict = {"at": 0.0, "key": "", "value": None}


def _devbox_ports_cache_clear() -> None:
    with _devbox_ports_lock:
        _devbox_ports_cache["at"] = 0.0
        _devbox_ports_cache["value"] = None


def _read_devbox_id() -> dict:
    try:
        data = json.loads(DEVBOX_ID_FILE.read_text())
        return data if isinstance(data, dict) else {}
    except (OSError, ValueError):
        return {}


def _shape_ports(payload: dict, source: str, host: str) -> dict:
    ports = payload.get("ports") if isinstance(payload, dict) else None
    if not isinstance(ports, dict) or not ports:
        return {"source": source, "host": host, "ports": {},
                "error": "no ports in devbox-ports.json — is the stack running?"}
    ports = {k: int(v) for k, v in ports.items()}
    return {
        "source": source,
        "host": host,
        "devKey": payload.get("devKey"),
        "dir": payload.get("dir"),
        "ports": ports,
        "caddyRoutes": payload.get("caddyRoutes") or [],
        "caddyPort": payload.get("caddyPort") or ports.get("caddy-reverse-proxy"),
        "contextApiPort": ports.get("test-context-api"),
    }


def get_devbox_ports(force: bool = False, host_override: str | None = None) -> dict:
    """Resolved port map of the stack this checkout is pointed at.

    Returns {source, host, ports, caddyPort, contextApiPort} — or the same shape
    with an "error" and an empty ports map when the stack isn't up yet. Cached
    for _DEVBOX_PORTS_TTL seconds; pass force=True to bypass.

    host_override lets the dashboard ask for a specific stack (it passes
    "localhost" while in Local mode even though a devbox is still pinned in
    .devbox-id.json); everything else comes from .devbox-id.json."""
    now = time.time()
    cache_key = (host_override or "").strip()
    if not force:
        with _devbox_ports_lock:
            cached = _devbox_ports_cache["value"]
            if (cached is not None and _devbox_ports_cache.get("key") == cache_key
                    and now - _devbox_ports_cache["at"] < _DEVBOX_PORTS_TTL):
                return cached

    saved = _read_devbox_id()
    host = (host_override or saved.get("host") or "").strip()

    if _is_localhost(host):
        try:
            result = _shape_ports(json.loads(DEVBOX_PORTS_FILE.read_text()),
                                  str(DEVBOX_PORTS_FILE), "localhost")
        except (OSError, ValueError) as e:
            result = {"source": str(DEVBOX_PORTS_FILE), "host": "localhost",
                      "ports": {}, "error": f"cannot read {DEVBOX_PORTS_FILE}: {e}"}
    else:
        user = (saved.get("sshUser") or "").strip()
        ssh_port = int(saved.get("sshPort") or 22)
        remote_dir = (saved.get("remoteDir")
                      or (f"/tmp/{saved['id']}/nammayatri" if saved.get("id") else ""))
        source = f"{user}@{host}:{remote_dir}/{DEVBOX_PORTS_RELPATH}"
        if not user or not remote_dir:
            result = {"source": source, "host": host, "ports": {},
                      "error": "incomplete .devbox-id.json — resolve the devbox first"}
        else:
            # Fall back to the registry slice for stacks started before the
            # preflight learned to publish data/devbox-ports.json.
            dev_key = saved.get("id") or ""
            fallback = (
                f"jq -c --arg k {shlex.quote(dev_key)} "
                f"'{{devKey: $k, dir: .users[$k].dir, caddyPort: .users[$k].caddyPort, "
                f"ports: .users[$k].ports}}' {REGISTRY_FILE}"
            ) if dev_key else "exit 1"
            argv = _ssh_argv(user, host, ssh_port, _find_ssh_key(), want_tty=False) + [
                f"cat {shlex.quote(f'{remote_dir}/{DEVBOX_PORTS_RELPATH}')} 2>/dev/null || {fallback}"
            ]
            try:
                proc = subprocess.run(argv, capture_output=True, timeout=15)
                if proc.returncode == 0 and proc.stdout.strip():
                    result = _shape_ports(json.loads(proc.stdout), source, host)
                else:
                    err = (proc.stderr or b"").decode(errors="replace").strip()
                    result = {"source": source, "host": host, "ports": {},
                              "error": err or "no data/devbox-ports.json on the devbox — is the stack running?"}
            except Exception as e:
                result = {"source": source, "host": host, "ports": {}, "error": str(e)}

    with _devbox_ports_lock:
        _devbox_ports_cache["at"] = time.time()
        _devbox_ports_cache["key"] = cache_key
        _devbox_ports_cache["value"] = result
    return result


EDITOR_CLI = "code"


def editor_available() -> dict:
    """Whether the `code` shell command exists on this machine — the dashboard
    hides its Open-in-editor button when it doesn't."""
    path = shutil.which(EDITOR_CLI)
    if path:
        return {"available": True, "cli": EDITOR_CLI, "path": path}
    return {
        "available": False,
        "cli": EDITOR_CLI,
        "error": (f"`{EDITOR_CLI}` is not on PATH — open VS Code and run "
                  f"\"Shell Command: Install '{EDITOR_CLI}' command in PATH\" once."),
    }


def open_remote_editor(body: dict) -> dict:
    """Open the stack workspace in VS Code over Remote-SSH.

    `code` accepts `--folder-uri vscode-remote://ssh-remote+<user@host>/<path>`;
    for a local stack we just open the directory."""
    cli = EDITOR_CLI
    host = (body.get("host") or "localhost").strip()
    user = (body.get("user") or "").strip()
    remote_dir = (body.get("remoteDir") or REMOTE_DEFAULT_DIR).strip() or REMOTE_DEFAULT_DIR

    if _is_localhost(host):
        argv = [cli, str(PROJECT_ROOT)]
        target = str(PROJECT_ROOT)
    else:
        if not user:
            return {"error": "user (SSH user) is required for a remote workspace"}
        target = f"vscode-remote://ssh-remote+{user}@{host}{remote_dir}"
        argv = [cli, "--folder-uri", target]

    try:
        proc = subprocess.run(argv, capture_output=True, text=True, timeout=20)
    except FileNotFoundError:
        return editor_available()
    except (subprocess.TimeoutExpired, OSError) as e:
        return {"error": str(e)}

    if proc.returncode != 0:
        return {"error": (proc.stderr or proc.stdout or f"{cli} exited {proc.returncode}").strip()}
    return {"opened": target}


def _remote_session_make(kind: str, host: str) -> dict:
    return {
        "id": _uuid.uuid4().hex[:12],
        "kind": kind,                 # "deploy" | "start"
        "host": host,
        "proc": None,
        "master_fd": None,
        "running": False,
        "exit_code": None,
        "started_at": time.time(),
        "finished_at": None,
        "buf": _deque(maxlen=4000),   # text-line ring (for /status)
        "byte_buf": bytearray(),      # raw bytes ring (for /stream re-attach)
        "modes": {},                  # latest DEC private-mode state: {num: b'h'|b'l'}
        "subscribers": [],            # list[queue.Queue]
        "lock": threading.Lock(),
    }


def _remote_register(session: dict) -> None:
    with _remote_sessions_lock:
        _remote_sessions[session["id"]] = session


def _remote_get(session_id: str) -> dict | None:
    with _remote_sessions_lock:
        return _remote_sessions.get(session_id)


def _remote_pty_reader(session: dict) -> None:
    """Reads bytes from the PTY master, fans out to subscribers + ring buffer."""
    import queue as _queue
    fd = session["master_fd"]
    while True:
        try:
            r, _, _ = _select.select([fd], [], [], 1.0)
        except (OSError, ValueError):
            break
        if not r:
            if session["proc"] and session["proc"].poll() is not None:
                break
            continue
        try:
            chunk = os.read(fd, 8192)
        except OSError:
            break
        if not chunk:
            break
        with session["lock"]:
            session["byte_buf"].extend(chunk)
            if len(session["byte_buf"]) > REMOTE_BUFFER_BYTES:
                del session["byte_buf"][:len(session["byte_buf"]) - REMOTE_BUFFER_BYTES]
            _snoop_modes(session["modes"], chunk)
            # Keep a coarse text log for the /status fallback.
            try:
                text = chunk.decode("utf-8", errors="replace")
            except Exception:
                text = ""
            if text:
                for line in text.splitlines():
                    session["buf"].append(line)
            subs = list(session["subscribers"])
        for q in subs:
            try:
                q.put_nowait(chunk)
            except _queue.Full:
                pass
    # Drain on exit.
    rc = None
    try:
        rc = session["proc"].wait(timeout=5) if session["proc"] else None
    except Exception:
        pass
    with session["lock"]:
        session["running"] = False
        session["exit_code"] = rc
        session["finished_at"] = time.time()
        subs = list(session["subscribers"])
    for q in subs:
        try:
            q.put_nowait(None)  # sentinel: stream ended
        except Exception:
            pass


def _remote_pipe_reader(session: dict, stream) -> None:
    """Non-PTY reader (used by rsync). stream is a subprocess pipe."""
    import queue as _queue
    import re as _re
    _lf_to_crlf = _re.compile(rb"(?<!\r)\n")
    while True:
        chunk = stream.read(4096)
        if not chunk:
            break
        if isinstance(chunk, str):
            chunk = chunk.encode()
        chunk = _lf_to_crlf.sub(b"\r\n", chunk)
        with session["lock"]:
            session["byte_buf"].extend(chunk)
            if len(session["byte_buf"]) > REMOTE_BUFFER_BYTES:
                del session["byte_buf"][:len(session["byte_buf"]) - REMOTE_BUFFER_BYTES]
            for line in chunk.decode("utf-8", errors="replace").splitlines():
                session["buf"].append(line)
            subs = list(session["subscribers"])
        for q in subs:
            try:
                q.put_nowait(chunk)
            except _queue.Full:
                pass
    rc = session["proc"].wait() if session["proc"] else None

    # Run post-rsync git init so Nix sees a git repo (faster store copy)
    if rc == 0 and session.get("post_ssh_argv"):
        try:
            post = subprocess.run(
                session["post_ssh_argv"], capture_output=True, text=True
            )
            msg = f"[deploy] git init: {'ok' if post.returncode == 0 else post.stderr.strip()}"
            with session["lock"]:
                session["buf"].append(msg)
        except Exception as e:
            with session["lock"]:
                session["buf"].append(f"[deploy] git init failed: {e}")

    with session["lock"]:
        session["running"] = False
        session["exit_code"] = rc
        session["finished_at"] = time.time()
        subs = list(session["subscribers"])
    for q in subs:
        try:
            q.put_nowait(None)
        except Exception:
            pass


def _ssh_argv(user: str, host: str, port: int, identity: str | None, want_tty: bool) -> list:
    argv = ["ssh"]
    if want_tty:
        argv += ["-tt"]
    argv += [
        "-o", "StrictHostKeyChecking=accept-new",
        "-o", "ServerAliveInterval=15",
        "-o", "GSSAPIAuthentication=no",
        "-o", "ConnectTimeout=10",
        "-o", "BatchMode=yes",
        "-p", str(port),
    ]
    if identity:
        argv += ["-i", identity]
    if user:
        argv += [f"{user}@{host}"]
    else:
        argv += [host]
    return argv


# ── Service log viewer ────────────────────────────────────────────────────────
# The stack writes each service's log as <workspace>/<name>.log. On a dev-box
# those live on the remote machine (/tmp/<devId>/nammayatri/*.log), out of the
# developer's reach — these two endpoints list them and tail a chosen one over
# SSH (or read them locally when running the stack on this machine).
_LOG_NAME_RE = re.compile(r"^[A-Za-z0-9._-]+\.log$")

def _log_list(body: dict) -> dict:
    """List the *.log files in the running stack's workspace."""
    host = (body.get("host") or "localhost").strip()
    if _is_localhost(host):
        try:
            return {"files": sorted(p.name for p in PROJECT_ROOT.glob("*.log"))}
        except OSError as e:
            return {"error": f"list logs failed: {e}"}
    user = (body.get("user") or "").strip()
    port = int(body.get("port") or 22)
    identity = (body.get("identityFile") or "").strip() or None
    remote_dir = (body.get("remoteDir") or "").strip()
    if not remote_dir:
        return {"error": "remoteDir required for a remote host"}
    cmd = f"ls -1 {shlex.quote(remote_dir)}/*.log 2>/dev/null | xargs -n1 basename 2>/dev/null || true"
    argv = _ssh_argv(user, host, port, identity, want_tty=False) + [cmd]
    try:
        r = subprocess.run(argv, capture_output=True, timeout=10)
        files = sorted(x for x in r.stdout.decode(errors="replace").splitlines() if x.strip())
        return {"files": files}
    except Exception as e:
        return {"error": f"list logs failed: {e}"}


# Byte cap for a "full log" fetch — protects against multi-GB logs (the
# driver-app log can balloon past 1 GB). We return at most the LAST cap bytes.
_LOG_FULL_CAP = 25 * 1024 * 1024


def _log_tail(body: dict) -> dict:
    """Return log content: last N lines (follow mode) or the whole file
    (full mode, capped at the last _LOG_FULL_CAP bytes)."""
    fname = (body.get("file") or "").strip()
    if not _LOG_NAME_RE.match(fname):
        return {"error": f"invalid log file name: {fname!r}"}
    full = bool(body.get("full"))
    lines = max(1, min(int(body.get("lines") or 2000), 200000))
    host = (body.get("host") or "localhost").strip()
    if _is_localhost(host):
        try:
            p = PROJECT_ROOT / fname
            with open(p, "rb") as f:
                f.seek(0, os.SEEK_END)
                size = f.tell()
                if full:
                    start = max(0, size - _LOG_FULL_CAP)
                    f.seek(start)
                    text = f.read().decode(errors="replace")
                    return {"file": fname, "content": text, "truncated": start > 0}
                f.seek(max(0, size - 512 * 1024))
                text = f.read().decode(errors="replace")
                return {"file": fname, "content": "\n".join(text.splitlines()[-lines:])}
        except OSError as e:
            return {"error": f"read failed: {e}"}
    user = (body.get("user") or "").strip()
    port = int(body.get("port") or 22)
    identity = (body.get("identityFile") or "").strip() or None
    remote_dir = (body.get("remoteDir") or "").strip()
    if not remote_dir:
        return {"error": "remoteDir required for a remote host"}
    path = shlex.quote(remote_dir + "/" + fname)
    cmd = (f"tail -c {_LOG_FULL_CAP} {path} 2>/dev/null || true" if full
           else f"tail -n {lines} {path} 2>/dev/null || true")
    argv = _ssh_argv(user, host, port, identity, want_tty=False) + [cmd]
    try:
        r = subprocess.run(argv, capture_output=True, timeout=30)
        content = r.stdout.decode(errors="replace")
        return {"file": fname, "content": content,
                "truncated": full and len(r.stdout) >= _LOG_FULL_CAP}
    except Exception as e:
        return {"error": f"tail failed: {e}"}

def _log_clear(body: dict) -> dict:
    """Truncate one *.log to empty (remote via SSH, or local)."""
    fname = (body.get("file") or "").strip()
    if not _LOG_NAME_RE.match(fname):
        return {"error": f"invalid log file name: {fname!r}"}
    host = (body.get("host") or "localhost").strip()
    if _is_localhost(host):
        try:
            open(PROJECT_ROOT / fname, "w").close()
            return {"cleared": True, "file": fname}
        except OSError as e:
            return {"error": f"clear failed: {e}"}
    user = (body.get("user") or "").strip()
    port = int(body.get("port") or 22)
    identity = (body.get("identityFile") or "").strip() or None
    remote_dir = (body.get("remoteDir") or "").strip()
    if not remote_dir:
        return {"error": "remoteDir required for a remote host"}
    cmd = f": > {shlex.quote(remote_dir + '/' + fname)}"
    argv = _ssh_argv(user, host, port, identity, want_tty=False) + [cmd]
    try:
        r = subprocess.run(argv, capture_output=True, timeout=10)
        if r.returncode == 0:
            return {"cleared": True, "file": fname}
        return {"error": r.stderr.decode(errors="replace").strip() or "clear failed"}
    except Exception as e:
        return {"error": f"clear failed: {e}"}


def _compute_cache_commit(project_root: str, max_n: int = 30) -> str:
    def _git(args, timeout=20):
        return subprocess.run(
            ["git", "-C", project_root, *args],
            capture_output=True, text=True, timeout=timeout,
        )

    # 1. best-effort fetch (works offline with stale refs; failures are non-fatal)
    for fetch_args in (
        ["fetch", "--quiet", "origin",
         "refs/tags/minio-pushed/*:refs/tags/minio-pushed/*"],
        ["fetch", "--quiet", "origin", "main"],
    ):
        try:
            _git(fetch_args, timeout=30)
        except Exception:  # noqa: BLE001
            pass

    # 2. merge-base with main (fallback HEAD)
    base = "HEAD"
    try:
        mb = _git(["merge-base", "origin/main", "HEAD"], timeout=15)
        if mb.returncode == 0 and mb.stdout.strip():
            base = mb.stdout.strip()
    except Exception:  # noqa: BLE001
        pass

    # 3. walk back and return the NEAREST commit carrying a minio-pushed tag —
    #    the single commit whose build cache cache-restore should pull. CI tags a
    #    commit once its build is pushed to MinIO. If none of the recent ancestors
    #    is tagged, return "" and cabal builds from scratch.
    walk = []
    try:
        lg = _git(["log", "--format=%H", "-n", str(max_n), base], timeout=15)
        if lg.returncode == 0:
            walk = lg.stdout.split()
    except Exception:  # noqa: BLE001
        pass

    for sha in walk:
        try:
            r = _git(["rev-parse", "-q", "--verify",
                      f"refs/tags/minio-pushed/{sha}"], timeout=10)
            if r.returncode == 0:
                return sha
        except Exception:  # noqa: BLE001
            pass
    return ""


# ── Start pre-flight: workspace fingerprint + git-history check ───────────────
# Deploy and Cabal Clean are no longer manual buttons. Starting the stack runs a
# pipeline instead:
#   1. fingerprint the workspace (every file rsync would copy, i.e. everything
#      not in REMOTE_EXCLUDES) — if it differs from the last deployed
#      fingerprint for this target, rsync first;
#   2. compare the git HEAD recorded at the last start against the current
#      branch's history — if that commit is no longer reachable (branch switch,
#      rebase, reset), the build tree is stale, so run a cabal clean first.
# The per-target state lives in .stack-state.json at the repo root.
STACK_STATE_FILE = PROJECT_ROOT / ".stack-state.json"


def _excluded_from_deploy(rel: str, name: str) -> bool:
    """True when a path is covered by REMOTE_EXCLUDES (rsync semantics:
    path-less patterns match any component, anchored patterns match a subtree)."""
    for pat in REMOTE_EXCLUDES:
        if "/" in pat:
            if rel == pat or rel.startswith(pat + "/"):
                return True
        elif fnmatch.fnmatch(name, pat):
            return True
    return False


def _workspace_fingerprint() -> str:
    """Hash of (path, size, mtime) for every file rsync would copy.

    Same change-detection inputs rsync itself uses, so the fingerprint moves
    exactly when a deploy would actually transfer something."""
    entries = []
    stack = [""]
    while stack:
        rel = stack.pop()
        base = PROJECT_ROOT / rel if rel else PROJECT_ROOT
        try:
            with os.scandir(base) as it:
                for e in it:
                    child = f"{rel}/{e.name}" if rel else e.name
                    if _excluded_from_deploy(child, e.name):
                        continue
                    try:
                        if e.is_dir(follow_symlinks=False):
                            stack.append(child)
                        elif e.is_file(follow_symlinks=False):
                            st = e.stat(follow_symlinks=False)
                            entries.append((child, st.st_size, st.st_mtime_ns))
                    except OSError:
                        continue
        except OSError:
            continue
    h = hashlib.sha256()
    for rel, size, mtime in sorted(entries):
        h.update(f"{rel}\0{size}\0{mtime}\n".encode())
    return h.hexdigest()


def _git_head() -> str:
    try:
        r = subprocess.run(["git", "-C", str(PROJECT_ROOT), "rev-parse", "HEAD"],
                           capture_output=True, text=True, timeout=15)
        return r.stdout.strip() if r.returncode == 0 else ""
    except Exception:  # noqa: BLE001
        return ""


def _git_in_history(sha: str) -> bool:
    """True when sha is an ancestor of (or equal to) the current HEAD."""
    if not sha:
        return False
    try:
        r = subprocess.run(
            ["git", "-C", str(PROJECT_ROOT), "merge-base", "--is-ancestor", sha, "HEAD"],
            capture_output=True, timeout=20)
        return r.returncode == 0
    except Exception:  # noqa: BLE001
        return False


def _stack_state_key(body: dict) -> str:
    host = (body.get("host") or "localhost").strip()
    if _is_localhost(host):
        return "local"
    user = (body.get("user") or "").strip()
    dev_name = (body.get("devName") or "").strip()
    return f"{user}@{host}/{dev_name}"


def _stack_state_read() -> dict:
    try:
        return json.loads(STACK_STATE_FILE.read_text())
    except Exception:  # noqa: BLE001 — missing or corrupt file: start fresh
        return {}


def _stack_state_write(key: str, patch: dict) -> dict:
    state = _stack_state_read()
    entry = dict(state.get(key) or {})
    entry.update(patch)
    state[key] = entry
    try:
        tmp = STACK_STATE_FILE.with_suffix(".json.tmp")
        tmp.write_text(json.dumps(state, indent=2) + "\n")
        tmp.replace(STACK_STATE_FILE)
    except OSError:
        pass
    return entry


def remote_preflight(body: dict) -> dict:
    """Decide what has to happen before the stack can start on this target."""
    host = (body.get("host") or "localhost").strip()
    copy_mode = (body.get("copyMode") or "rsync").strip()
    key = _stack_state_key(body)
    stored = _stack_state_read().get(key) or {}

    workspace_hash = _workspace_fingerprint()
    stored_workspace = stored.get("workspaceHash") or ""
    git_head = _git_head()
    stored_git = stored.get("gitHead") or ""

    if _is_localhost(host) or copy_mode == "skip":
        needs_deploy, deploy_reason = False, "running locally — no rsync needed"
    elif not stored_workspace:
        needs_deploy, deploy_reason = True, "no deploy recorded for this dev-box yet"
    elif stored_workspace != workspace_hash:
        needs_deploy, deploy_reason = True, "local files changed since the last deploy"
    else:
        needs_deploy, deploy_reason = False, "workspace unchanged since the last deploy"

    if not stored_git:
        needs_clean, clean_reason = False, "first run — recording HEAD as the baseline"
    elif _git_in_history(stored_git):
        needs_clean = False
        clean_reason = f"last run's commit {stored_git[:8]} is in this branch's history"
    else:
        needs_clean = True
        clean_reason = (f"last run's commit {stored_git[:8]} is not in this branch's "
                        f"history — build artifacts are stale")

    _auto_deploy_remember(_deploy_target(body))

    return {
        "key": key,
        "needsDeploy": needs_deploy,
        "deployReason": deploy_reason,
        "needsCabalClean": needs_clean,
        "cabalCleanReason": clean_reason,
        "gitHead": git_head,
        "storedGitHead": stored_git,
        "workspaceHash": workspace_hash,
        "storedWorkspaceHash": stored_workspace,
        "deployedAt": stored.get("deployedAt"),
        "startedAt": stored.get("startedAt"),
        "checkedAt": time.time(),
        "autoDeploy": auto_deploy_status(),
    }


def remote_mark(body: dict) -> dict:
    """Record a completed pipeline stage for this target.

    stage=deploy → snapshot the workspace fingerprint (skips the next rsync
    until a file actually changes); stage=start → snapshot git HEAD (skips the
    next cabal clean while that commit stays in the branch's history)."""
    stage = (body.get("stage") or "").strip()
    key = _stack_state_key(body)
    if stage == "deploy":
        patch = {"workspaceHash": _workspace_fingerprint(), "deployedAt": time.time()}
    elif stage == "start":
        patch = {"gitHead": _git_head(), "startedAt": time.time()}
    else:
        return {"error": f"unknown stage: {stage!r}"}
    return {"key": key, "state": _stack_state_write(key, patch)}


# ── Deploy: one exclusive lock, two callers ──────────────────────────────────
# Deploys run one at a time behind _deploy_lock. The two callers differ only in
# how they wait for it:
#   • Start (run-mobility-stack) blocks on the lock — if the background watcher
#     is mid-rsync, the pipeline waits for it instead of racing a second rsync
#     into the same remote directory;
#   • the background watcher never blocks — it takes the lock only if it is
#     free, otherwise it goes back to sleep and retries on the next tick.
_deploy_lock = threading.Lock()

AUTO_DEPLOY_POLL_SECONDS = 60
AUTO_DEPLOY_TARGET_KEY = "__autoDeploy__"

_auto_deploy_lock = threading.Lock()
_auto_deploy = {
    "target": None,
    "enabled": True,
    "lastCheckAt": None,
    "lastDeployAt": None,
    "lastSession": None,
    "lastResult": None,
}


def _deploy_target(body: dict) -> dict:
    return {
        "host": (body.get("host") or "localhost").strip(),
        "user": (body.get("user") or "").strip(),
        "port": int(body.get("port") or 22),
        "identityFile": (body.get("identityFile") or "").strip() or None,
        "devName": (body.get("devName") or "").strip(),
        "copyMode": (body.get("copyMode") or "rsync").strip(),
    }


def _deployable(target: dict) -> bool:
    return bool(
        target
        and target.get("user")
        and not _is_localhost(target.get("host") or "localhost")
        and (target.get("copyMode") or "rsync") != "skip"
    )


def _auto_deploy_remember(target: dict) -> None:
    """Pin the target the watcher deploys to — the last one the dashboard used."""
    if not _deployable(target):
        return
    with _auto_deploy_lock:
        if _auto_deploy["target"] == target:
            return
        _auto_deploy["target"] = dict(target)
    _stack_state_write(AUTO_DEPLOY_TARGET_KEY, dict(target))


def _auto_deploy_target() -> dict | None:
    with _auto_deploy_lock:
        target = _auto_deploy["target"]
    if target is None:
        target = (_stack_state_read().get(AUTO_DEPLOY_TARGET_KEY) or {}) or None
        if target:
            with _auto_deploy_lock:
                _auto_deploy["target"] = target
    return target if _deployable(target or {}) else None


def auto_deploy_status() -> dict:
    with _auto_deploy_lock:
        status = dict(_auto_deploy)
    status["busy"] = _deploy_lock.locked()
    status["pollSeconds"] = AUTO_DEPLOY_POLL_SECONDS
    return status


def _session_note(session: dict, line: str) -> None:
    with session["lock"]:
        session["buf"].append(line)


def _session_finish(session: dict, code: int) -> None:
    with session["lock"]:
        session["running"] = False
        session["exit_code"] = code
        session["finished_at"] = time.time()


def _deploy_plan(target: dict) -> dict:
    """rsync argv + the post-rsync git-init command for a target.

    Touches the network (dev-user registry lookup), so it runs inside the lock,
    not while building the HTTP response."""
    entry = _register_dev_user(
        target["user"], target["host"], target["port"],
        target["identityFile"], target["devName"],
    )
    remote_dir = entry["dir"]

    ssh_cmd_parts = [
        "ssh",
        "-o", "StrictHostKeyChecking=accept-new",
        "-o", "BatchMode=yes",
        "-o", "ConnectTimeout=10",
        "-p", str(target["port"]),
    ]
    if target["identityFile"]:
        ssh_cmd_parts += ["-i", target["identityFile"]]
    ssh_cmd = " ".join(ssh_cmd_parts)

    excludes = []
    for ex in REMOTE_EXCLUDES:
        excludes += ["--exclude", ex]

    # Use --rsync-path to create the destination directory on the remote before
    # rsync starts — avoids a separate SSH round-trip for mkdir.
    rsync_path = f"mkdir -p {shlex.quote(remote_dir)} && rsync"

    argv = (
        ["rsync", "-az", "--delete", "--progress", "--stats",
         "-e", ssh_cmd, "--rsync-path", rsync_path]
        + excludes
        + [f"{PROJECT_ROOT}/", f"{target['user']}@{target['host']}:{remote_dir}/"]
    )

    cache_commit = _compute_cache_commit(str(PROJECT_ROOT))
    notes = [
        f"[deploy] minio cache commit: {cache_commit}" if cache_commit else
        "[deploy] no minio-pushed cache commit found — cabal will build from scratch"
    ]

    git_ignore_cmd = (
        "printf '%s\\n' 'dist-newstyle/' 'hie/' '.hie/' '.nix-deps/' "
        "'.cabal-dir/' '.ci-project-root' '.ci-cabal-dir' '.ci-cache-sha' "
        "> .git/info/exclude"
    )
    git_init_cmd = (
        f"cd {shlex.quote(remote_dir)} && "
        f"rm -rf .git && git init -q && {git_ignore_cmd} && git add -A && "
        f"GIT_AUTHOR_NAME=deploy GIT_AUTHOR_EMAIL=deploy@deploy "
        f"GIT_COMMITTER_NAME=deploy GIT_COMMITTER_EMAIL=deploy@deploy "
        f"git commit -q -m deploy --allow-empty 2>/dev/null || true; "
        f"mkdir -p Backend && printf '%s' {shlex.quote(cache_commit)} "
        f"> Backend/.ci-cache-sha"
    )
    ssh_base = [
        "ssh", "-o", "StrictHostKeyChecking=accept-new",
        "-o", "BatchMode=yes", "-o", "ConnectTimeout=10",
        "-p", str(target["port"]),
    ]
    if target["identityFile"]:
        ssh_base += ["-i", target["identityFile"]]

    return {
        "argv": argv,
        "notes": notes,
        "post_ssh_argv": ssh_base + [f"{target['user']}@{target['host']}", git_init_cmd],
    }


def _deploy_run(session: dict, target: dict, wait_for_lock: bool) -> bool:
    """Run one rsync deploy under the exclusive deploy lock (blocking).

    wait_for_lock=False returns immediately when another deploy holds the lock —
    that is how the watcher backs off instead of queueing."""
    if not _deploy_lock.acquire(blocking=False):
        if not wait_for_lock:
            _session_note(session, "[deploy] another deploy is in progress — skipped")
            _session_finish(session, 75)
            return False
        _session_note(
            session,
            "[deploy] an auto-deploy is already running — waiting for it to finish…",
        )
        _deploy_lock.acquire()

    try:
        # Snapshot before rsync: anything edited *during* the transfer may not
        # have been picked up, so it must still count as un-deployed.
        workspace_hash = _workspace_fingerprint()
        try:
            plan = _deploy_plan(target)
        except Exception as exc:  # noqa: BLE001
            _session_note(session, f"[deploy] error: {exc}")
            _session_finish(session, 2)
            return False

        try:
            proc = subprocess.Popen(
                plan["argv"],
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                bufsize=0,
                start_new_session=True,
            )
        except FileNotFoundError:
            _session_note(session, "[deploy] error: rsync not on PATH")
            _session_finish(session, 127)
            return False

        with session["lock"]:
            session["proc"] = proc
            session["running"] = True
            session["buf"].append(f"[deploy] {' '.join(plan['argv'])}")
            for note in plan["notes"]:
                session["buf"].append(note)
        session["post_ssh_argv"] = plan["post_ssh_argv"]

        _remote_pipe_reader(session, proc.stdout)  # blocks until rsync exits

        if session["exit_code"] == 0:
            _stack_state_write(
                _stack_state_key(target),
                {"workspaceHash": workspace_hash, "deployedAt": time.time()},
            )
            return True
        return False
    finally:
        _deploy_lock.release()


def remote_deploy(body: dict) -> dict:
    target = _deploy_target(body)
    session = _remote_session_make("deploy", target["host"])
    _remote_register(session)

    if _is_localhost(target["host"]) or target["copyMode"] == "skip":
        with session["lock"]:
            session["running"] = False
            session["exit_code"] = 0
            session["finished_at"] = time.time()
            session["buf"].append(
                "[deploy] localhost or copyMode=skip — no rsync needed."
            )
        return {"session": session["id"], "skipped": True}

    if not target["user"]:
        with session["lock"]:
            session["exit_code"] = 2
            session["finished_at"] = time.time()
            session["buf"].append("[deploy] error: 'user' is required for non-localhost host")
        return {"session": session["id"], "error": "user required"}

    _auto_deploy_remember(target)
    with session["lock"]:
        session["running"] = True

    # The lock wait happens off the request thread so /deploy still answers with
    # a session id immediately; the wait is visible in that session's output.
    threading.Thread(
        target=_deploy_run, args=(session, target, True), daemon=True
    ).start()

    return {"session": session["id"], "skipped": False}


def _auto_deploy_prune() -> None:
    """Keep finished auto-deploy sessions from piling up (one per minute)."""
    with _remote_sessions_lock:
        stale = [
            sid for sid, s in _remote_sessions.items()
            if s["kind"] == "deploy" and s.get("auto") and not s["running"]
            and (s.get("finished_at") or 0) < time.time() - 3600
        ]
        for sid in stale:
            _remote_sessions.pop(sid, None)


def _auto_deploy_tick() -> None:
    target = _auto_deploy_target()
    with _auto_deploy_lock:
        _auto_deploy["lastCheckAt"] = time.time()
    if not target:
        return

    key = _stack_state_key(target)
    stored = _stack_state_read().get(key) or {}
    if not stored.get("workspaceHash"):
        return  # never deployed to this box yet — let Start do the first sync
    if stored["workspaceHash"] == _workspace_fingerprint():
        return

    if _deploy_lock.locked():
        return  # a Start-triggered deploy owns the lock — retry next tick

    _auto_deploy_prune()
    session = _remote_session_make("deploy", target["host"])
    session["auto"] = True
    _remote_register(session)
    with session["lock"]:
        session["running"] = True
        session["buf"].append("[auto-deploy] local files changed — syncing")
    with _auto_deploy_lock:
        _auto_deploy["lastSession"] = session["id"]
        _auto_deploy["lastDeployAt"] = time.time()

    ok = _deploy_run(session, target, wait_for_lock=False)
    with _auto_deploy_lock:
        _auto_deploy["lastResult"] = (
            "ok" if ok else f"failed (exit {session['exit_code']})"
        )


def _auto_deploy_loop() -> None:
    while True:
        time.sleep(AUTO_DEPLOY_POLL_SECONDS)
        try:
            _auto_deploy_tick()
        except Exception as exc:  # noqa: BLE001 — a bad tick must not kill the watcher
            with _auto_deploy_lock:
                _auto_deploy["lastResult"] = f"error: {exc}"


def start_auto_deploy_watcher() -> None:
    threading.Thread(target=_auto_deploy_loop, daemon=True).start()


def remote_clear_data(body: dict) -> dict:
    """Wipe runtime state under <repo>/data (postgres, kafka, metabase, …).

    Runs the existing `, clear-data` mission-control script, auto-answering
    its [y/N] confirmation prompt. Localhost runs the script in the local
    repo; remote hosts run it over SSH inside `remoteDir`.
    """
    host = (body.get("host") or "localhost").strip()
    user = (body.get("user") or "").strip()
    port = int(body.get("port") or 22)
    identity = (body.get("identityFile") or "").strip() or None
    dev_name = (body.get("devName") or "").strip()

    session = _remote_session_make("clear-data", host)
    _remote_register(session)

    # Delete data/ contents directly — no nix evaluation needed.
    inner = (
        "for entry in data/*/; do"
        " [ -d \"$entry\" ] || continue;"
        " name=$(basename \"$entry\");"
        " case \"$name\" in ny-react-native|control-center) echo \"keeping $name\"; continue;; esac;"
        " echo \"rm -rf $entry\"; rm -rf -- \"$entry\";"
        " done;"
        " find data/ -maxdepth 1 -type f -delete 2>/dev/null || true;"
        " echo Done."
    )

    if _is_localhost(host):
        argv = ["bash", "-lc", f"cd {PROJECT_ROOT} && {inner}"]
    else:
        if not user:
            with session["lock"]:
                session["exit_code"] = 2
                session["finished_at"] = time.time()
                session["buf"].append("[clear-data] error: 'user' is required for non-localhost host")
            return {"session": session["id"], "error": "user required"}
        # devName is mandatory — derive remoteDir from the devbox registry.
        entry = _register_dev_user(user, host, port, identity, dev_name)
        remote_dir = entry["dir"]
        remote_cmd = f"export PATH=\"$HOME/.nix-profile/bin:/nix/var/nix/profiles/default/bin:$PATH\"; cd {shlex.quote(remote_dir)} && {inner}"
        argv = _ssh_argv(user, host, port, identity, want_tty=False) + [remote_cmd]

    try:
        proc = subprocess.Popen(
            argv,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            bufsize=0,
            start_new_session=True,
        )
    except FileNotFoundError as e:
        with session["lock"]:
            session["exit_code"] = 127
            session["finished_at"] = time.time()
            session["buf"].append(f"[clear-data] error: {e}")
        return {"session": session["id"], "error": str(e)}

    with session["lock"]:
        session["proc"] = proc
        session["running"] = True
        session["buf"].append(f"[clear-data] {' '.join(argv)}")

    threading.Thread(
        target=_remote_pipe_reader, args=(session, proc.stdout), daemon=True
    ).start()

    return {"session": session["id"]}


def remote_cabal_clean(body: dict) -> dict:
    """Run `cabal clean` inside the Backend directory to clear stale build artifacts.

    Useful after a GHC panic or package-database corruption. Runs non-interactively;
    no PTY needed. Localhost runs directly; remote hosts run it over SSH.
    """
    host = (body.get("host") or "localhost").strip()
    user = (body.get("user") or "").strip()
    port = int(body.get("port") or 22)
    identity = (body.get("identityFile") or "").strip() or None
    dev_name = (body.get("devName") or "").strip()

    session = _remote_session_make("cabal-clean", host)
    _remote_register(session)

    # Clear every build-state entry of the rsync exclude list, at the workspace
    # root and under Backend/ — those paths are never rsynced, so nothing else
    # would ever clean them up on the stack host.
    _targets = " ".join(
        f"./{e} Backend/{e}" for e in CABAL_CLEAN_TARGETS
    )
    inner = f"rm -rf {_targets}"

    if _is_localhost(host):
        argv = ["bash", "-lc", f"cd {PROJECT_ROOT} && {inner}"]
    else:
        if not user:
            with session["lock"]:
                session["exit_code"] = 2
                session["finished_at"] = time.time()
                session["buf"].append("[cabal-clean] error: 'user' is required for non-localhost host")
            return {"session": session["id"], "error": "user required"}
        entry = _register_dev_user(user, host, port, identity, dev_name)
        remote_dir = entry["dir"]
        remote_cmd = f"export PATH=\"$HOME/.nix-profile/bin:/nix/var/nix/profiles/default/bin:$PATH\"; cd {shlex.quote(remote_dir)} && {inner}"
        argv = _ssh_argv(user, host, port, identity, want_tty=False) + [remote_cmd]

    try:
        proc = subprocess.Popen(
            argv,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            bufsize=0,
            start_new_session=True,
        )
    except FileNotFoundError as e:
        with session["lock"]:
            session["exit_code"] = 127
            session["finished_at"] = time.time()
            session["buf"].append(f"[cabal-clean] error: {e}")
        return {"session": session["id"], "error": str(e)}

    with session["lock"]:
        session["proc"] = proc
        session["running"] = True
        session["buf"].append(f"[cabal-clean] {' '.join(argv)}")

    threading.Thread(
        target=_remote_pipe_reader, args=(session, proc.stdout), daemon=True
    ).start()

    return {"session": session["id"]}

def remote_start(body: dict) -> dict:
    host = (body.get("host") or "localhost").strip()
    user = (body.get("user") or "").strip()
    port = int(body.get("port") or 22)
    identity = (body.get("identityFile") or "").strip() or None
    remote_dir = (body.get("remoteDir") or REMOTE_DEFAULT_DIR).strip() or REMOTE_DEFAULT_DIR
    command = (body.get("command") or REMOTE_DEFAULT_COMMAND).strip()
    cols = int(body.get("cols") or 120)
    rows = int(body.get("rows") or 32)

    session = _remote_session_make("start", host)
    _remote_register(session)

    if _is_localhost(host):
        # Run locally inside a PTY. cd to the repo root — the user-supplied
        # command is expected to handle its own `cd Backend && nix develop …`
        # so the same command works for localhost and remote (rsynced) hosts.
        argv = ["bash", "-lc", f"cd {PROJECT_ROOT} && {command}"]
    else:
        if not user:
            with session["lock"]:
                session["exit_code"] = 2
                session["finished_at"] = time.time()
                session["buf"].append("[start] error: 'user' is required for non-localhost host")
            return {"session": session["id"], "error": "user required"}
        _auto_deploy_remember(_deploy_target(body))
        remote_cmd = f"cd {shlex.quote(remote_dir)} && {command}"
        argv = _ssh_argv(user, host, port, identity, want_tty=True) + [remote_cmd]

    master_fd, slave_fd = _pty.openpty()
    try:
        _set_pty_size(master_fd, rows, cols)
    except Exception:
        pass

    try:
        proc = subprocess.Popen(
            argv,
            stdin=slave_fd, stdout=slave_fd, stderr=slave_fd,
            close_fds=True, start_new_session=True,
        )
    except FileNotFoundError as e:
        os.close(master_fd); os.close(slave_fd)
        with session["lock"]:
            session["exit_code"] = 127
            session["finished_at"] = time.time()
            session["buf"].append(f"[start] error: {e}")
        return {"session": session["id"], "error": str(e)}

    os.close(slave_fd)  # parent only needs the master end

    with session["lock"]:
        session["proc"] = proc
        session["master_fd"] = master_fd
        session["running"] = True
        session["cols"] = cols
        session["rows"] = rows
        session["buf"].append(f"[start] {' '.join(argv)}")

    threading.Thread(target=_remote_pty_reader, args=(session,), daemon=True).start()
    return {"session": session["id"], "cols": cols, "rows": rows}


def _remote_spawn_pty(session: dict, argv: list[str], cols=None, rows=None) -> dict:
    """Run argv in a PTY attached to an already-registered session, so the
    dashboard's xterm can drive it (used for interactive one-offs like
    ssh-copy-id, which must be able to prompt for a password)."""
    cols = int(cols or 100)
    rows = int(rows or 24)
    master_fd, slave_fd = _pty.openpty()
    try:
        _set_pty_size(master_fd, rows, cols)
    except Exception:
        pass
    try:
        proc = subprocess.Popen(
            argv,
            stdin=slave_fd, stdout=slave_fd, stderr=slave_fd,
            close_fds=True, start_new_session=True,
        )
    except (FileNotFoundError, OSError) as e:
        os.close(master_fd); os.close(slave_fd)
        with session["lock"]:
            session["exit_code"] = 127
            session["finished_at"] = time.time()
            session["buf"].append(f"[{session['kind']}] error: {e}")
        return {"session": session["id"], "error": str(e)}

    os.close(slave_fd)
    with session["lock"]:
        session["proc"] = proc
        session["master_fd"] = master_fd
        session["running"] = True
        session["cols"] = cols
        session["rows"] = rows
        session["buf"].append(f"[{session['kind']}] {' '.join(argv)}")

    threading.Thread(target=_remote_pty_reader, args=(session,), daemon=True).start()
    return {"session": session["id"], "cols": cols, "rows": rows}


def _set_pty_size(fd: int, rows: int, cols: int) -> None:
    _fcntl.ioctl(
        fd, _termios.TIOCSWINSZ,
        _struct.pack("HHHH", rows, cols, 0, 0),
    )


def remote_input(session_id: str, data_b64: str) -> dict:
    session = _remote_get(session_id)
    if not session or session.get("master_fd") is None:
        return {"error": "no such session"}
    try:
        data = _base64.b64decode(data_b64)
    except Exception:
        return {"error": "invalid base64"}
    try:
        os.write(session["master_fd"], data)
    except OSError as e:
        return {"error": str(e)}
    return {"ok": True}


def remote_resize(session_id: str, cols: int, rows: int) -> dict:
    session = _remote_get(session_id)
    if not session or session.get("master_fd") is None:
        return {"error": "no such session"}
    try:
        _set_pty_size(session["master_fd"], rows, cols)
        with session["lock"]:
            session["cols"] = cols
            session["rows"] = rows
    except Exception as e:
        return {"error": str(e)}
    return {"ok": True}


def remote_stop(session_id: str) -> dict:
    session = _remote_get(session_id)
    if not session:
        return {"error": "no such session"}
    proc = session.get("proc")
    if proc and proc.poll() is None:
        try:
            os.killpg(os.getpgid(proc.pid), 15)
        except Exception:
            try: proc.terminate()
            except Exception: pass
        time.sleep(0.5)
        if proc.poll() is None:
            try: os.killpg(os.getpgid(proc.pid), 9)
            except Exception:
                try: proc.kill()
                except Exception: pass
    if session.get("master_fd") is not None:
        try: os.close(session["master_fd"])
        except Exception: pass
    return {"stopped": True}


def _remote_broadcast(session: dict, chunk: bytes) -> None:
    with session["lock"]:
        subs = list(session["subscribers"])
    for q in subs:
        try:
            q.put_nowait(chunk)
        except Exception:  # noqa: BLE001 — a full/closed subscriber queue is not fatal
            pass


def _remote_force_repaint(session: dict, clear: bool = True) -> bool:
    """Make a full-screen TUI redraw every cell of the browser's terminal.

    A diffing TUI (tcell/process-compose) only writes cells its own model says
    changed, and it has no idea what the browser is actually showing. Anything
    xterm holds that the TUI believes is already blank therefore lingers for
    ever — the stray border column left over from a frame drawn while the
    browser geometry and the PTY still disagreed. Clearing xterm alone makes
    that worse (the TUI won't repaint what it thinks is unchanged), so the
    clear has to be paired with a genuine geometry change, which is the one
    thing that invalidates the TUI's model."""
    fd = session.get("master_fd")
    if fd is None or not session.get("running"):
        return False
    with session["lock"]:
        cols = session.get("cols") or 120
        rows = session.get("rows") or 32
        alt_screen = session["modes"].get(1049) == b"h"
    if clear and alt_screen:
        _remote_broadcast(session, b"\x1b[0m\x1b[2J\x1b[H")
    # Shrink by a row AND a column, hold long enough for the app to render that
    # frame, then restore. Two real changes are needed: a differential renderer
    # coalesces a fast shrink/restore into "no net change" and repaints only
    # diffs. Ctrl+L after, for apps that honour it.
    try:
        _set_pty_size(fd, max(10, rows - 1), max(20, cols - 1))
        time.sleep(0.18)
        _set_pty_size(fd, rows, cols)
    except Exception:  # noqa: BLE001
        pass
    try:
        os.write(fd, b"\x0c")
    except OSError:
        pass
    return True


def remote_repaint(session_id: str) -> dict:
    session = _remote_get(session_id)
    if not session:
        return {"error": "no such session"}
    return {"repainted": _remote_force_repaint(session)}


def remote_status(session_id: str) -> dict:
    session = _remote_get(session_id)
    if not session:
        return {"error": "no such session"}
    with session["lock"]:
        return {
            "id": session["id"],
            "kind": session["kind"],
            "host": session["host"],
            "running": session["running"],
            "exitCode": session["exit_code"],
            "startedAt": session["started_at"],
            "finishedAt": session["finished_at"],
            "lastLines": list(session["buf"])[-300:],
            "cols": session.get("cols"),
            "rows": session.get("rows"),
        }


def remote_sessions_list() -> list:
    with _remote_sessions_lock:
        return [
            {
                "id": s["id"], "kind": s["kind"], "host": s["host"],
                "running": s["running"], "exitCode": s["exit_code"],
                "startedAt": s["started_at"],
                "cols": s.get("cols"), "rows": s.get("rows"),
            }
            for s in _remote_sessions.values()
        ]


class LocalApiHandler(BaseHTTPRequestHandler):
    def log_message(self, format, *args):
        print(f"  \033[93m[Local API]\033[0m {args[0]}")

    def handle_one_request(self):
        try:
            super().handle_one_request()
        except BrokenPipeError:
            pass

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
            pass

    def do_OPTIONS(self):
        self.send_response(200)
        self._cors_headers()
        self.end_headers()

    def _read_json_body(self):
        content_len = int(self.headers.get("Content-Length", 0))
        if content_len > 0:
            return json.loads(self.rfile.read(content_len))
        return {}

    def _handle(self, method):
        parsed = urlparse(self.path)
        path = parsed.path.rstrip("/")

        # GET /api/control-center/status
        if method == "GET" and path == "/api/control-center/status":
            with _control_center_lock:
                st = dict(_control_center_state)
                st["log"] = st["log"][-300:]
            if st["running"] and not st["ready"]:
                if _probe_control_center_ready():
                    with _control_center_lock:
                        _control_center_state["ready"] = True
                    st["ready"] = True
            self._send_json(st)
            return True

        # POST /api/control-center/start
        if method == "POST" and path == "/api/control-center/start":
            body = self._read_json_body() or {}
            ref = (body.get("ref") or "").strip() or None
            accepted = trigger_control_center(ref=ref)
            if not accepted:
                self._send_json({"error": "control-center is already running"}, 409)
                return True
            self._send_json({"started": True, "url": CONTROL_CENTER_URL, "ref": ref})
            return True

        # POST /api/control-center/stop
        if method == "POST" and path == "/api/control-center/stop":
            killed = stop_control_center()
            self._send_json({"stopped": killed})
            return True

        # GET /api/ny-react-native/options
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

        # GET /status  (ny-react-native status)
        if method == "GET" and path == "/api/ny-react-native/status":
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

        # POST /start  (ny-react-native start)
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
                if len(raw) == 0 or len(raw) > 256 * 1024:
                    self._send_json({
                        "error": f"firebase_override size {len(raw)}B out of range (must be 1B..256KB)",
                    }, 400)
                    return True
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

        # GET /api/git/refs
        if method == "GET" and path == "/api/git/refs":
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

        # POST /stop  (ny-react-native stop)
        if method == "POST" and path == "/api/ny-react-native/stop":
            body = self._read_json_body()
            app = body.get("app", "")
            if app not in NY_RN_VALID_APPS:
                self._send_json({"error": f"invalid app '{app}'", "valid": sorted(NY_RN_VALID_APPS)}, 400)
                return True
            killed = stop_ny_rn(app)
            self._send_json({"stopped": killed, "app": app})
            return True

        # POST /clear-cache  (ny-react-native clear-cache)
        if method == "POST" and path == "/api/ny-react-native/clear-cache":
            def sh_capture(cmd, timeout=8):
                try:
                    p = subprocess.run(
                        cmd, capture_output=True, text=True, timeout=timeout,
                    )
                    return p.returncode, p.stdout or "", p.stderr or ""
                except Exception as e:
                    return -1, "", str(e)
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
                rc, out, _err = sh_capture(
                    ["xcrun", "simctl", "listapps", udid],
                    timeout=8,
                )
                bundle_id = None
                if rc == 0:
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

        # POST /open-debugger  (ny-react-native open-debugger)
        if method == "POST" and path == "/api/ny-react-native/open-debugger":
            body = self._read_json_body() or {}
            app = body.get("app", "")
            if app not in NY_RN_VALID_APPS:
                self._send_json({
                    "error": f"invalid app '{app}'",
                    "valid": sorted(NY_RN_VALID_APPS),
                }, 400)
                return True
            sub = "consumer" if app == "customer" else "provider"
            port = None
            try:
                ps = subprocess.run(
                    ["ps", "-eo", "pid=,command="],
                    capture_output=True, text=True, timeout=3,
                )
                marker = f"data/ny-react-native/{sub}/"
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
                    candidates.sort()
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
                target = None
                targets = []
                try:
                    with _ur.urlopen(f"http://localhost:{port}/json", timeout=3) as r:
                        targets = _json.loads(r.read().decode("utf-8", "replace"))
                except Exception:
                    targets = []
                if isinstance(targets, list) and targets:
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

        # GET /metro-log
        if method == "GET" and path == "/api/ny-react-native/metro-log":
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

        # GET /_adb-diag
        if method == "GET" and path == "/api/ny-react-native/_adb-diag":
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

        # GET /logcat
        if method == "GET" and path == "/api/ny-react-native/logcat":
            from urllib.parse import parse_qs as _pq
            import re as _re
            qs = _pq(parsed.query)
            log_type = (qs.get("type", ["console"])[0]).lower()
            app_q = (qs.get("app", [""])[0]).lower() or None
            try:
                lines = max(50, min(2000, int(qs.get("lines", ["400"])[0])))
            except ValueError:
                lines = 400

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

            if platform == "ios":
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

                proc_name = (variant[:1].upper() + variant[1:]) if variant else "Lynx"

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

            adb_bin = _resolve_adb_bin()
            try:
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

        # ── Remote SSH/PTY runner ──

        # POST /api/remote/logs — list *.log files in the stack workspace
        if method == "POST" and path == "/api/remote/logs":
            self._send_json(_log_list(self._read_json_body() or {}))
            return True

        # POST /api/remote/log-tail — last N lines (or full) of a chosen *.log
        if method == "POST" and path == "/api/remote/log-tail":
            self._send_json(_log_tail(self._read_json_body() or {}))
            return True

        # POST /api/remote/log-clear — truncate a chosen *.log to empty
        if method == "POST" and path == "/api/remote/log-clear":
            self._send_json(_log_clear(self._read_json_body() or {}))
            return True

        # POST /api/remote/preflight — does starting need a deploy / cabal clean?
        if method == "POST" and path == "/api/remote/preflight":
            try:
                self._send_json(remote_preflight(self._read_json_body() or {}))
            except Exception as exc:
                self._send_json({"error": f"preflight: {exc}"}, 500)
            return True

        # POST /api/remote/mark — record a completed deploy / start
        if method == "POST" and path == "/api/remote/mark":
            try:
                self._send_json(remote_mark(self._read_json_body() or {}))
            except Exception as exc:
                self._send_json({"error": f"mark: {exc}"}, 500)
            return True

        # POST /api/remote/deploy
        if method == "POST" and path == "/api/remote/deploy":
            try:
                self._send_json(remote_deploy(self._read_json_body() or {}))
            except Exception as exc:
                self._send_json({"error": f"deploy: {exc}"}, 500)
            return True

        # POST /api/remote/start
        if method == "POST" and path == "/api/remote/start":
            try:
                self._send_json(remote_start(self._read_json_body() or {}))
            except Exception as exc:
                self._send_json({"error": f"start: {exc}"}, 500)
            return True

        # POST /api/remote/clear-data
        if method == "POST" and path == "/api/remote/clear-data":
            try:
                self._send_json(remote_clear_data(self._read_json_body() or {}))
            except Exception as exc:
                self._send_json({"error": f"clear-data: {exc}"}, 500)
            return True

        # POST /api/remote/cabal-clean
        if method == "POST" and path == "/api/remote/cabal-clean":
            try:
                self._send_json(remote_cabal_clean(self._read_json_body() or {}))
            except Exception as exc:
                self._send_json({"error": f"cabal-clean: {exc}"}, 500)
            return True

        # POST /api/remote/input
        if method == "POST" and path == "/api/remote/input":
            body = self._read_json_body() or {}
            session_id = (body.get("session") or "").strip()
            self._send_json(remote_input(session_id, body.get("data", "")))
            return True

        # POST /api/remote/resize
        if method == "POST" and path == "/api/remote/resize":
            body = self._read_json_body() or {}
            session_id = (body.get("session") or "").strip()
            cols = int(body.get("cols") or 120)
            rows = int(body.get("rows") or 32)
            self._send_json(remote_resize(session_id, cols, rows))
            return True

        # POST /api/remote/stop  (also accept /kill for parity with terminal API)
        if method == "POST" and path in ("/api/remote/stop", "/api/remote/kill"):
            body = self._read_json_body() or {}
            session_id = (body.get("session") or "").strip()
            try:
                self._send_json(remote_stop(session_id))
            except Exception as exc:
                self._send_json({"error": f"stop: {exc}"}, 500)
            return True

        # POST /api/remote/repaint — force a full TUI redraw at the current size
        if method == "POST" and path == "/api/remote/repaint":
            body = self._read_json_body() or {}
            session_id = (body.get("session") or "").strip()
            self._send_json(remote_repaint(session_id))
            return True

        # GET /api/remote/status?session=<id>
        if method == "GET" and path == "/api/remote/status":
            qs = urllib.parse.parse_qs(parsed.query or "")
            session_id = (qs.get("session", [""])[0]).strip()
            self._send_json(remote_status(session_id))
            return True

        # GET /api/remote/auto-deploy — watcher target + last run
        if method == "GET" and path == "/api/remote/auto-deploy":
            self._send_json(auto_deploy_status())
            return True

        # GET /api/remote/sessions
        if method == "GET" and path == "/api/remote/sessions":
            self._send_json(remote_sessions_list())
            return True

        # GET /api/remote/machines  — fetch ServiceDiscovery machine list
        if method == "GET" and path == "/api/remote/machines":
            self._send_json(_fetch_machines())
            return True

        # GET /api/devbox/resolve[?new=1]
        if method == "GET" and path == "/api/devbox/resolve":
            from urllib.parse import parse_qs as _pq
            q = _pq(urlparse(self.path).query)
            force = (q.get("new", ["0"])[0] or "0") in ("1", "true")
            self._send_json(_devbox_resolve(force_new=force))
            return True

        # GET /api/devbox/ports[?refresh=1][&host=...]
        # Single source of truth for resolved ports: reads data/devbox-ports.json
        # from the stack host (locally, or over SSH using .devbox-id.json).
        if method == "GET" and path == "/api/devbox/ports":
            from urllib.parse import parse_qs as _pq
            q = _pq(urlparse(self.path).query)
            refresh = (q.get("refresh", ["0"])[0] or "0") in ("1", "true")
            host_q = (q.get("host", [""])[0] or "").strip() or None
            self._send_json(get_devbox_ports(force=refresh, host_override=host_q))
            return True

        # POST /api/remote/setup-ssh  — generate key + test connectivity
        if method == "POST" and path == "/api/remote/setup-ssh":
            body = self._read_json_body() or {}
            host = (body.get("host") or "").strip()
            user = (body.get("user") or "").strip()
            port = int(body.get("port") or 22)
            if not host or not user:
                self._send_json({"error": "host and user are required"}, 400)
                return True
            try:
                self._send_json(_setup_ssh(host, user, port))
            except Exception as e:
                self._send_json({"error": str(e)}, 500)
            return True

        # POST /api/remote/ssh-copy-id — PTY session running ssh-copy-id, so the
        # one-time password can be typed in the dashboard terminal.
        if method == "POST" and path == "/api/remote/ssh-copy-id":
            self._send_json(remote_ssh_copy_id(self._read_json_body() or {}))
            return True

        # GET /api/remote/editor-available — is `code` on PATH on this machine?
        if method == "GET" and path == "/api/remote/editor-available":
            self._send_json(editor_available())
            return True

        # POST /api/remote/open-editor — open the workspace in VS Code
        # (Remote-SSH for a devbox, plain folder for a local stack).
        if method == "POST" and path == "/api/remote/open-editor":
            self._send_json(open_remote_editor(self._read_json_body() or {}))
            return True

        # GET /api/remote/stream?session=<id>  (SSE)
        if method == "GET" and path == "/api/remote/stream":
            qs = urllib.parse.parse_qs(parsed.query or "")
            session_id = (qs.get("session", [""])[0]).strip()
            self._remote_stream(session_id)
            return True

        # POST /api/browse-folder  body: {initial?: str}  → {path: str|null}
        if method == "POST" and path == "/api/browse-folder":
            body = self._read_json_body() or {}
            initial = (body.get("initial") or "").strip() or os.path.expanduser("~")
            self._send_json(_browse_folder(initial))
            return True

        # ── Generic spec-driven launcher routes ─────────────────────────
        if self._handle_launcher(method, path, parsed):
            return True

        # ── Load Test ─────────────────────────────────────────────────────
        if path == "/api/load-test/start" and method == "POST":
            body = self._read_json_body() or {}
            engine = body.get("engine", "postman")
            if engine == "locust":
                if not _LT_LOCUST_AVAILABLE:
                    self._send_json({"error": "locust runner unavailable — check load-test-service/locust_runner.py"}, 503)
                    return True
                run_id = _lt_locust_runner.start_run(body)
            else:
                if not _LT_AVAILABLE:
                    self._send_json({"error": "load test runner unavailable"}, 503)
                    return True
                run_id = _lt_runner.start_run(body)
            self._send_json({"runId": run_id})
            return True
        elif path.startswith("/api/load-test/events/") and method == "GET":
            run_id = path[len("/api/load-test/events/"):]
            self._lt_stream(run_id)
            return True
        elif path.startswith("/api/load-test/stop/") and method == "POST":
            run_id = path[len("/api/load-test/stop/"):]
            # Try both runners — only one will have the run_id
            ok = False
            if _LT_AVAILABLE and _lt_runner:
                ok = _lt_runner.stop_run(run_id) or ok
            if _LT_LOCUST_AVAILABLE and _lt_locust_runner:
                ok = _lt_locust_runner.stop_run(run_id) or ok
            self._send_json({"ok": ok})
            return True

        self._send_json({"error": "not found"}, 404)
        return True

    def _remote_stream(self, session_id: str):
        import queue as _queue
        session = _remote_get(session_id)
        if not session:
            self._send_json({"error": "no such session"}, 404)
            return
        try:
            self.send_response(200)
            self.send_header("Content-Type", "text/event-stream")
            self.send_header("Cache-Control", "no-cache")
            self.send_header("X-Accel-Buffering", "no")
            self._cors_headers()
            self.end_headers()
        except BrokenPipeError:
            return

        q: _queue.Queue = _queue.Queue(maxsize=256)
        is_pty = session.get("master_fd") is not None
        with session["lock"]:
            backlog = bytes(session["byte_buf"])
            mode_prefix = _replay_modes_bytes(session["modes"])
            alt_screen = session["modes"].get(1049) == b"h"
            session["subscribers"].append(q)
            running = session["running"]

        def _send_event(chunk: bytes):
            payload = json.dumps({"b64": _base64.b64encode(chunk).decode()})
            self.wfile.write(b"data: " + payload.encode() + b"\n\n")
            self.wfile.flush()

        try:
            # Replay the byte ring so xterm reconstructs the TUI's mode state
            # (alternate screen, mouse tracking, hidden cursor, keypad mode).
            # Without this the new xterm misses the startup mode-setting
            # escapes and renders process-compose's draws into the wrong
            # buffer — rows look highlighted and input doesn't reach the TUI.
            if mode_prefix:
                _send_event(mode_prefix)

            # A full-screen TUI (alt-screen) repaints itself completely after
            # the SIGWINCH pulse below, so replaying the ring buys nothing —
            # and actively hurts: the ring starts mid-stream, so its first
            # escape sequence is usually cut in half and any SGR whose reset
            # fell off the front leaks a background colour onto cells the TUI
            # never repaints (the grey blocks). Hand xterm a clean slate
            # instead. Non-TUI sessions (deploy/cabal-clean output) still get
            # their scrollback, prefixed with an SGR reset for the same reason.
            if alt_screen and is_pty and running:
                _send_event(b"\x1b[0m\x1b[2J\x1b[H")
            elif backlog:
                _send_event(b"\x1b[0m" + _trim_to_escape_boundary(backlog))
            if is_pty and running:
                # The clear above already went to this subscriber, so only the
                # geometry pulse is needed here. The browser re-runs this via
                # /repaint once its own fit has settled — the pulse is only
                # correct at the *final* geometry, and the first fit often is
                # not it.
                _remote_force_repaint(session, clear=False)
            if not running and session.get("exit_code") is not None:
                _send_event(f"\r\n[exit {session['exit_code']}]\r\n".encode())
                return
            while True:
                try:
                    chunk = q.get(timeout=15)
                except _queue.Empty:
                    # SSE keep-alive comment.
                    self.wfile.write(b": ping\n\n")
                    self.wfile.flush()
                    continue
                if chunk is None:
                    rc = session.get("exit_code")
                    _send_event(f"\r\n[exit {rc if rc is not None else '?'}]\r\n".encode())
                    return
                _send_event(chunk)
        except (BrokenPipeError, ConnectionResetError):
            pass
        finally:
            with session["lock"]:
                try:
                    session["subscribers"].remove(q)
                except ValueError:
                    pass

    def _lt_stream(self, run_id: str):
        import queue as _queue
        eq = None
        if _LT_AVAILABLE and _lt_runner:
            eq = _lt_runner.get_queue(run_id)
        if eq is None and _LT_LOCUST_AVAILABLE and _lt_locust_runner:
            eq = _lt_locust_runner.get_queue(run_id)
        if eq is None:
            self._send_json({"error": "run not found or already finished"}, 404)
            return
        try:
            self.send_response(200)
            self.send_header("Content-Type", "text/event-stream")
            self.send_header("Cache-Control", "no-cache")
            self.send_header("X-Accel-Buffering", "no")
            self._cors_headers()
            self.end_headers()
        except BrokenPipeError:
            return
        try:
            while True:
                try:
                    event = eq.get(timeout=15)
                    self.wfile.write(f"data: {json.dumps(event)}\n\n".encode())
                    self.wfile.flush()
                    if event.get("type") in ("run_complete", "error"):
                        break
                except _queue.Empty:
                    self.wfile.write(b": ping\n\n")
                    self.wfile.flush()
        except (BrokenPipeError, ConnectionResetError):
            pass

    # ── Spec-driven launcher endpoints (additive) ────────────────────────────

    def _handle_launcher(self, method: str, path: str, parsed) -> bool:
        if not path.startswith("/api/launcher"):
            return False

        # GET /api/launcher  → list specs (with status summary)
        if method == "GET" and path == "/api/launcher":
            out = []
            for s in spec_loader.get_specs():
                out.append({
                    "name": s["name"],
                    "title": s["title"],
                    "icon": s.get("icon"),
                    "category": s.get("category"),
                    "tags": s.get("tags") or [],
                    "ports": s.get("ports") or [],
                    "domains": s.get("domains") or [],
                })
            self._send_json(out)
            return True

        parts = path.split("/")
        # /api/launcher/{slug}/...
        if len(parts) < 4:
            self._send_json({"error": "bad launcher path"}, 400)
            return True
        slug = parts[3]
        spec = spec_loader.get_spec(slug)
        if spec is None:
            self._send_json({"error": f"unknown launcher {slug!r}"}, 404)
            return True
        tail = "/".join(parts[4:])

        # GET /api/launcher/{slug}
        if method == "GET" and tail == "":
            input_store.ensure_defaults(slug, spec)
            inputs_view = input_store.public_view(slug, spec)
            stages = [stage_runner.stage_status(slug, spec, st) for st in spec["stages"]]
            self._send_json({
                "spec": spec,
                "inputs": inputs_view,
                "stages": stages,
                "missingRequired": input_store.missing_required(slug, spec),
            })
            return True

        # POST /api/launcher/{slug}/inputs
        if method == "POST" and tail == "inputs":
            body = self._read_json_body() or {}
            updated = {}
            by_key = {i["key"]: i for i in spec.get("inputs", [])}
            for key, payload in body.items():
                idef = by_key.get(key)
                if not idef:
                    continue
                try:
                    value = input_store.upsert_input(slug, idef, payload if isinstance(payload, dict) else {"value": payload})
                    updated[key] = bool(value) if idef["type"] in ("file", "secret-text") else value
                except Exception as e:  # noqa: BLE001
                    self._send_json({"error": f"input {key!r}: {e}"}, 400)
                    return True
            self._send_json({"updated": updated, "inputs": input_store.public_view(slug, spec)})
            return True

        # POST /api/launcher/{slug}/source  {ref?: str, localPath?: str|null}
        if method == "POST" and tail == "source":
            body = self._read_json_body() or {}
            if "localPath" in body:
                result = stage_runner.set_local_path(slug, spec, body.get("localPath") or None)
                self._send_json(result)
                return True
            if "ref" in body:
                # Persist the ref override under the spec_loader cache by
                # writing into the spec file? For now write into per-launcher
                # state so the spec file stays canonical.
                state = stage_runner._load_state(slug)
                state["sourceRefOverride"] = (body["ref"] or "").strip() or None
                if state["sourceRefOverride"]:
                    spec["source"]["ref"] = state["sourceRefOverride"]
                stage_runner._save_state(slug, state)
                self._send_json({"ref": spec["source"].get("ref")})
                return True
            self._send_json({"error": "expected ref or localPath"}, 400)
            return True

        # POST /api/launcher/{slug}/workflow/{name}
        if method == "POST" and tail.startswith("workflow/"):
            wf = tail.split("/", 1)[1]
            stages = (spec.get("workflows") or {}).get(wf)
            if not stages:
                self._send_json({"error": f"unknown workflow {wf!r}"}, 404)
                return True
            results = []
            for sid in stages:
                r = stage_runner.run_stage(slug, spec, sid, force=False)
                results.append({"stage": sid, **r})
                if r.get("error"):
                    break
                if r.get("skipped"):
                    continue
                session_id = r.get("sessionId")
                if not session_id:
                    continue
                stage_def = next((s for s in spec["stages"] if s["id"] == sid), None)
                if not stage_def:
                    continue
                wait_r = stage_runner.wait_for_stage(slug, stage_def, session_id)
                results[-1]["wait"] = wait_r
                if wait_r.get("error") or wait_r.get("timeout"):
                    break
                if stage_def.get("lifecycle", "one-shot") == "one-shot" and wait_r.get("exit", 0) != 0:
                    break
                if stage_def.get("lifecycle") == "long-running" and wait_r.get("ready") is False:
                    break
            self._send_json({"workflow": wf, "results": results})
            return True

        # POST /api/launcher/{slug}/stage/{id}  body: {force?: bool}
        if method == "POST" and tail.startswith("stage/"):
            sub = tail.split("/", 1)[1]
            if sub.endswith("/stop"):
                stage_id = sub[:-len("/stop")]
                stopped = stage_runner.stop_stage(slug, stage_id)
                self._send_json({"stopped": stopped, "stage": stage_id})
                return True
            stage_id = sub
            body = self._read_json_body() or {}
            r = stage_runner.run_stage(slug, spec, stage_id, force=bool(body.get("force")))
            self._send_json({"stage": stage_id, **r})
            return True

        # DELETE /api/launcher/{slug}/stages  — reset all stage state
        if method == "DELETE" and tail == "stages":
            stage_runner.reset_all_stages(slug)
            self._send_json({"reset": True, "slug": slug})
            return True

        # GET /api/launcher/{slug}/status
        if method == "GET" and tail == "status":
            stages = [stage_runner.stage_status(slug, spec, st) for st in spec["stages"]]
            self._send_json({"slug": slug, "stages": stages})
            return True

        # GET /api/launcher/{slug}/sessions
        if method == "GET" and tail == "sessions":
            self._send_json(stage_runner.list_sessions(slug))
            return True

        # GET /api/launcher/{slug}/stage/{id}/stream  (SSE — per-stage live log)
        if method == "GET" and tail.startswith("stage/") and tail.endswith("/stream"):
            stage_id = tail[len("stage/"):-len("/stream")]
            self._launcher_stage_stream(slug, stage_id)
            return True

        # GET /api/launcher/{slug}/logs/{name}/stream  (SSE)
        if method == "GET" and tail.startswith("logs/") and tail.endswith("/stream"):
            log_name = tail[len("logs/"):-len("/stream")]
            self._launcher_log_stream(slug, spec, log_name)
            return True

        self._send_json({"error": f"unknown launcher route {tail!r}"}, 404)
        return True

    def _launcher_stage_stream(self, slug: str, stage_id: str) -> None:
        import base64 as _b64
        stream = log_sources.open_stage_stream(slug, stage_id)
        try:
            self.send_response(200)
            self.send_header("Content-Type", "text/event-stream")
            self.send_header("Cache-Control", "no-cache")
            self.send_header("X-Accel-Buffering", "no")
            self._cors_headers()
            self.end_headers()
        except BrokenPipeError:
            stream["close"]()
            return
        try:
            for chunk in stream["iter"]:
                payload = json.dumps({"b64": _b64.b64encode(chunk).decode()})
                self.wfile.write(b"data: " + payload.encode() + b"\n\n")
                self.wfile.flush()
        except (BrokenPipeError, ConnectionResetError):
            pass
        finally:
            stream["close"]()

    def _launcher_log_stream(self, slug: str, spec: dict, log_name: str) -> None:
        import base64 as _b64
        stream = log_sources.open_stream(slug, spec, log_name)
        if stream is None:
            self._send_json({"error": f"unknown log source {log_name!r}"}, 404)
            return
        try:
            self.send_response(200)
            self.send_header("Content-Type", "text/event-stream")
            self.send_header("Cache-Control", "no-cache")
            self.send_header("X-Accel-Buffering", "no")
            self._cors_headers()
            self.end_headers()
        except BrokenPipeError:
            stream["close"]()
            return
        try:
            for chunk in stream["iter"]:
                payload = json.dumps({"b64": _b64.b64encode(chunk).decode()})
                self.wfile.write(b"data: " + payload.encode() + b"\n\n")
                self.wfile.flush()
        except (BrokenPipeError, ConnectionResetError):
            pass
        finally:
            stream["close"]()

    def do_GET(self):
        self._handle("GET")

    def do_POST(self):
        self._handle("POST")

    def do_PUT(self):
        self._handle("PUT")

    def do_DELETE(self):
        self._handle("DELETE")


def main():
    port = PORT
    for i, arg in enumerate(sys.argv):
        if arg == "--port" and i + 1 < len(sys.argv):
            port = int(sys.argv[i + 1])

    server = ThreadingHTTPServer(("0.0.0.0", port), LocalApiHandler)
    server.daemon_threads = True
    start_auto_deploy_watcher()
    print(
        f"\n  \033[93m📋 Local API on http://localhost:{port} (threaded)\033[0m\n")

    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print("\nShutdown.")
        server.server_close()


if __name__ == "__main__":
    main()
