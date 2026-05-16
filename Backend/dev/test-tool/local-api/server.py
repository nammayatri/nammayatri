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

REMOTE_EXCLUDES = [
    ".git", "data", "node_modules", "dist-newstyle",
    "dist", ".direnv", "_build", "result", "result-*",
]
REMOTE_DEFAULT_DIR = "/tmp/nammayatri"
REMOTE_DEFAULT_COMMAND = (
    "cd Backend && nix develop .#backend -c , run-mobility-stack-dev"
)
REMOTE_BUFFER_BYTES = 256 * 1024  # last ~256 KB per session for re-attach

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
        "-p", str(port),
    ]
    if identity:
        argv += ["-i", identity]
    if user:
        argv += [f"{user}@{host}"]
    else:
        argv += [host]
    return argv


def remote_deploy(body: dict) -> dict:
    host = (body.get("host") or "localhost").strip()
    user = (body.get("user") or "").strip()
    port = int(body.get("port") or 22)
    identity = (body.get("identityFile") or "").strip() or None
    remote_dir = (body.get("remoteDir") or REMOTE_DEFAULT_DIR).strip() or REMOTE_DEFAULT_DIR
    copy_mode = (body.get("copyMode") or "rsync").strip()

    session = _remote_session_make("deploy", host)
    _remote_register(session)

    if _is_localhost(host) or copy_mode == "skip":
        with session["lock"]:
            session["running"] = False
            session["exit_code"] = 0
            session["finished_at"] = time.time()
            session["buf"].append(
                "[deploy] localhost or copyMode=skip — no rsync needed."
            )
        return {"session": session["id"], "skipped": True}

    if not user:
        with session["lock"]:
            session["exit_code"] = 2
            session["finished_at"] = time.time()
            session["buf"].append("[deploy] error: 'user' is required for non-localhost host")
        return {"session": session["id"], "error": "user required"}

    target = f"{user}@{host}:{remote_dir}/"
    ssh_cmd_parts = ["ssh", "-o", "StrictHostKeyChecking=accept-new", "-p", str(port)]
    if identity:
        ssh_cmd_parts += ["-i", identity]
    ssh_cmd = " ".join(ssh_cmd_parts)

    excludes = []
    for ex in REMOTE_EXCLUDES:
        excludes += ["--exclude", ex]

    argv = (
        ["rsync", "-az", "--delete", "--info=progress2,stats1", "-e", ssh_cmd]
        + excludes
        + [f"{PROJECT_ROOT}/", target]
    )

    try:
        proc = subprocess.Popen(
            argv,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            bufsize=0,
        )
    except FileNotFoundError:
        with session["lock"]:
            session["exit_code"] = 127
            session["finished_at"] = time.time()
            session["buf"].append("[deploy] error: rsync not on PATH")
        return {"session": session["id"], "error": "rsync not on PATH"}

    with session["lock"]:
        session["proc"] = proc
        session["running"] = True
        session["buf"].append(f"[deploy] {' '.join(argv)}")

    threading.Thread(
        target=_remote_pipe_reader, args=(session, proc.stdout), daemon=True
    ).start()

    return {"session": session["id"], "skipped": False}


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
    remote_dir = (body.get("remoteDir") or REMOTE_DEFAULT_DIR).strip() or REMOTE_DEFAULT_DIR

    session = _remote_session_make("clear-data", host)
    _remote_register(session)

    # `yes y | … , clear-data` answers the confirmation prompt non-interactively.
    inner = "cd Backend && yes y | nix develop .#backend -c , clear-data"

    if _is_localhost(host):
        argv = ["bash", "-lc", f"cd {PROJECT_ROOT} && {inner}"]
    else:
        if not user:
            with session["lock"]:
                session["exit_code"] = 2
                session["finished_at"] = time.time()
                session["buf"].append("[clear-data] error: 'user' is required for non-localhost host")
            return {"session": session["id"], "error": "user required"}
        remote_cmd = f"cd {remote_dir} && {inner}"
        argv = _ssh_argv(user, host, port, identity, want_tty=False) + [remote_cmd]

    try:
        proc = subprocess.Popen(
            argv,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            bufsize=0,
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
        remote_cmd = f"cd {remote_dir} && {command}"
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

        # POST /api/remote/deploy
        if method == "POST" and path == "/api/remote/deploy":
            self._send_json(remote_deploy(self._read_json_body() or {}))
            return True

        # POST /api/remote/start
        if method == "POST" and path == "/api/remote/start":
            self._send_json(remote_start(self._read_json_body() or {}))
            return True

        # POST /api/remote/clear-data
        if method == "POST" and path == "/api/remote/clear-data":
            self._send_json(remote_clear_data(self._read_json_body() or {}))
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
            self._send_json(remote_stop(session_id))
            return True

        # GET /api/remote/status?session=<id>
        if method == "GET" and path == "/api/remote/status":
            qs = urllib.parse.parse_qs(parsed.query or "")
            session_id = (qs.get("session", [""])[0]).strip()
            self._send_json(remote_status(session_id))
            return True

        # GET /api/remote/sessions
        if method == "GET" and path == "/api/remote/sessions":
            self._send_json(remote_sessions_list())
            return True

        # GET /api/remote/stream?session=<id>  (SSE)
        if method == "GET" and path == "/api/remote/stream":
            qs = urllib.parse.parse_qs(parsed.query or "")
            session_id = (qs.get("session", [""])[0]).strip()
            self._remote_stream(session_id)
            return True

        # ── Generic spec-driven launcher routes ─────────────────────────
        if self._handle_launcher(method, path, parsed):
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
            session["subscribers"].append(q)
            running = session["running"]
            cols = session.get("cols") or 120
            rows = session.get("rows") or 32

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
            if backlog:
                _send_event(backlog)
            if is_pty and running:
                fd = session["master_fd"]
                # The replayed frame is stale (it was drawn for whatever size
                # the PTY was when the bytes were captured). Pulse the PTY
                # size to force a SIGWINCH — and follow with Ctrl+L — so the
                # TUI redraws fresh against the now-correctly-moded xterm.
                # We pulse to (cols-1) and back so the kernel sees a real
                # change either way; same-size ioctls don't emit SIGWINCH.
                try:
                    _set_pty_size(fd, rows, max(20, cols - 1))
                    time.sleep(0.05)
                    _set_pty_size(fd, rows, cols)
                except Exception:
                    pass
                try:
                    os.write(fd, b"\x0c")
                except OSError:
                    pass
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

        # GET /api/launcher/{slug}/status
        if method == "GET" and tail == "status":
            stages = [stage_runner.stage_status(slug, spec, st) for st in spec["stages"]]
            self._send_json({"slug": slug, "stages": stages})
            return True

        # GET /api/launcher/{slug}/sessions
        if method == "GET" and tail == "sessions":
            self._send_json(stage_runner.list_sessions(slug))
            return True

        # GET /api/launcher/{slug}/logs/{name}/stream  (SSE)
        if method == "GET" and tail.startswith("logs/") and tail.endswith("/stream"):
            log_name = tail[len("logs/"):-len("/stream")]
            self._launcher_log_stream(slug, spec, log_name)
            return True

        self._send_json({"error": f"unknown launcher route {tail!r}"}, 404)
        return True

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
    print(
        f"\n  \033[93m📋 Local API on http://localhost:{port} (threaded)\033[0m\n")

    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print("\nShutdown.")
        server.server_close()


if __name__ == "__main__":
    main()
