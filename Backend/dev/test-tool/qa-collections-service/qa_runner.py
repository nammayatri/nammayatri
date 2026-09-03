"""
Backend execution engine for ny-qa-automation collections (NY/MSIL/YS).

The dashboard's "Collections (Integration Tests)" tab lists these the same
way it lists every other suite (see context-api/server.py's
_scan_qa_collections()), but routes their actual execution here instead of
running them in-browser, because they rely on pm.execution.setNextRequest()
branching that the in-browser postman-runtime.ts does not implement. Each
collection runs as its own Newman subprocess (qa_newman_runner.js).

A run can also be triggered externally via POST /api/qa-collections/webhook
(see build_webhook_run_config below) — either a single {directory, filename}
passed in the request body, or — if the body is empty — whatever
webhook-config.json lists.

Mirrors the run-registry / SSE-queue pattern already used by
load-test-service/runner.py (start_run / stop_run / get_queue), so
local-api/server.py can wire this up the same way it wires up /api/load-test.
"""

import json
import os
import shutil
import subprocess
import threading
import time
import uuid
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
from queue import Queue

SCRIPT_DIR = Path(__file__).resolve().parent
PROJECT_ROOT = SCRIPT_DIR.parent.parent.parent.parent  # .../nammayatri/

NODE_PATH = shutil.which("node") or "/opt/homebrew/bin/node"
RUNNER_JS = SCRIPT_DIR / "qa_newman_runner.js"

# ny-qa-automation is a private repo, checked out on disk rather than baked
# into any image — resolved fresh on every call (not cached at import time)
# so a sync_repo() clone that just created it is picked up immediately:
#   1. $QA_AUTOMATION_DIR if set (must point at .../src/api_tests)
#   2. <repo-root>/data/ny-qa-automation/src/api_tests — the managed clone
#      sync_repo() creates/pulls, same convention as data/control-center
#   3. <repo-root>/../ny-qa-automation/src/api_tests — a sibling checkout,
#      for anyone who already had it cloned there before this existed
GITHUB_REPO_SSH = "git@github.com:nammayatri/ny-qa-automation.git"


def _qa_repo_root() -> Path:
    env_override = os.environ.get("QA_AUTOMATION_DIR")
    if env_override:
        # env override points at .../src/api_tests; repo root is two levels up.
        return Path(env_override).parent.parent
    data_repo = PROJECT_ROOT / "data" / "ny-qa-automation"
    if data_repo.is_dir():
        return data_repo
    sibling_repo = PROJECT_ROOT.parent / "ny-qa-automation"
    if sibling_repo.is_dir():
        return sibling_repo
    return data_repo  # doesn't exist yet — sync_repo() clones it here


def _qa_dir() -> Path:
    env_override = os.environ.get("QA_AUTOMATION_DIR")
    if env_override:
        return Path(env_override)
    return _qa_repo_root() / "src" / "api_tests"


MAX_TRACKED_RUNS = 20

_runs: dict[str, dict] = {}
_runs_lock = threading.Lock()
_sync_lock = threading.Lock()


def is_available() -> bool:
    return _qa_dir().is_dir()


def sync_repo(timeout: int = 120) -> dict:
    """Clone-or-pull ny-qa-automation on disk. Uses whatever git credentials
    (SSH agent, HTTPS credential helper) are already configured on this
    host/container — nothing here handles auth itself, same as the
    control-center/ny-react-native launcher scripts.
    """
    repo_root = _qa_repo_root()
    with _sync_lock:
        repo_root.parent.mkdir(parents=True, exist_ok=True)
        if (repo_root / ".git").is_dir():
            cmd = ["git", "-C", str(repo_root), "pull", "--ff-only"]
        else:
            cmd = ["git", "clone", GITHUB_REPO_SSH, str(repo_root)]
        try:
            proc = subprocess.run(cmd, capture_output=True, text=True, timeout=timeout)
        except subprocess.TimeoutExpired:
            return {"ok": False, "dir": str(repo_root), "output": f"timed out after {timeout}s running: {' '.join(cmd)}"}
        except OSError as exc:
            return {"ok": False, "dir": str(repo_root), "output": str(exc)}
        output = (proc.stdout or "") + (proc.stderr or "")
        return {"ok": proc.returncode == 0, "dir": str(repo_root), "output": output.strip()}


def _resolve_path(rel: str) -> Path:
    """rel is either 'Local.postman_environment.json' or 'MSIL/Master.postman_environment.json'."""
    return _qa_dir() / rel


WEBHOOK_CONFIG_PATH = SCRIPT_DIR / "webhook-config.json"
DEFAULT_WEBHOOK_ENV_FILE = "Local.postman_environment.json"


def _load_webhook_config() -> dict:
    if not WEBHOOK_CONFIG_PATH.is_file():
        return {"concurrency": 1, "collections": []}
    try:
        cfg = json.loads(WEBHOOK_CONFIG_PATH.read_text())
    except Exception:
        return {"concurrency": 1, "collections": []}
    cfg.setdefault("concurrency", 1)
    cfg.setdefault("collections", [])
    return cfg


def build_webhook_run_config(body: dict) -> dict:
    """Build a start_run() config from a webhook request, in priority order:

    1. Body carries its own `collections` (a full [{directory, filename?,
       envFile?}] list) — run exactly that, ignoring webhook-config.json
       entirely. What an external caller with its own idea of "which flows"
       (e.g. System Control Centre, driven by its own DB config) should send —
       a `filename`-less entry still expands to "every collection in that
       directory" via _expand_collections.
    2. Body is just {"directory": ..., "filename": ...} — run that one
       collection alone (kept for simple single-collection callers).
    3. Empty/other body — run whatever webhook-config.json lists (this
       dashboard's own local "configured" set).
    """
    if body.get("collections"):
        return {
            "collections": body["collections"],
            "envFile": body.get("envFile") or DEFAULT_WEBHOOK_ENV_FILE,
            "concurrency": body.get("concurrency") or 1,
            "triggeredBy": "webhook",
        }
    if body.get("directory") and body.get("filename"):
        return {
            "collections": [{
                "directory": body["directory"],
                "filename": body["filename"],
                "envFile": body.get("envFile"),
            }],
            "envFile": body.get("envFile") or DEFAULT_WEBHOOK_ENV_FILE,
            "concurrency": 1,
            "triggeredBy": "webhook",
        }
    cfg = _load_webhook_config()
    return {
        "collections": cfg.get("collections", []),
        "envFile": cfg.get("envFile") or DEFAULT_WEBHOOK_ENV_FILE,
        "concurrency": cfg.get("concurrency", 1),
        "triggeredBy": "webhook",
    }


MAX_EVENTS_PER_RUN = 2000


def _emit(run: dict, eq: Queue, event: dict):
    """Push an event onto the live SSE queue AND the run's persisted event log,
    so full detail (incl. request/response bodies for failures) survives after
    the SSE stream closes — see get_run_detail()."""
    eq.put(event)
    with run["lock"]:
        events = run["events"]
        events.append(event)
        if len(events) > MAX_EVENTS_PER_RUN:
            del events[: len(events) - MAX_EVENTS_PER_RUN]


def _expand_collections(collections: list) -> list:
    """A {directory} entry with no filename expands to every collection
    currently in that directory — lets webhook-config.json (or SCC's own
    config) say "run all of NY" without naming every file."""
    expanded = []
    for c in collections:
        if c.get("filename"):
            expanded.append(c)
            continue
        directory = c.get("directory", "")
        dir_path = _qa_dir() / directory
        if not dir_path.is_dir():
            continue
        for f in sorted(dir_path.iterdir()):
            if f.is_file() and f.suffix == ".json" and not f.name.endswith(".postman_environment.json"):
                expanded.append({**c, "filename": f.name})
    return expanded


def _run_one_collection(run_id: str, directory: str, filename: str, env_path: Path, eq: Queue, run: dict):
    collection_path = _qa_dir() / directory / filename
    label = Path(filename).stem

    if not collection_path.is_file():
        _emit(run, eq, {"type": "collection_result", "collection": label, "directory": directory, "passed": 0, "failed": 1,
                         "error": f"collection not found: {collection_path}"})
        return 0, 1

    cmd = [NODE_PATH, str(RUNNER_JS), str(collection_path)]
    if env_path is not None:
        cmd.append(str(env_path))

    _emit(run, eq, {"type": "collection_started", "collection": label, "directory": directory})

    try:
        proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True, bufsize=1)
    except OSError as exc:
        _emit(run, eq, {"type": "collection_result", "collection": label, "directory": directory, "passed": 0, "failed": 1, "error": str(exc)})
        return 0, 1

    with run["lock"]:
        run["procs"].append(proc)

    passed = failed = 0
    try:
        for line in proc.stdout:
            line = line.strip()
            if not line:
                continue
            try:
                event = json.loads(line)
            except ValueError:
                continue
            event["collection"] = label
            event["directory"] = directory
            _emit(run, eq, event)
            if event.get("type") == "done":
                passed = event.get("passed", 0)
                failed = event.get("failed", 0)
    finally:
        proc.wait()

    if proc.returncode != 0 and passed == 0 and failed == 0:
        failed = 1

    _emit(run, eq, {"type": "collection_result", "collection": label, "directory": directory, "passed": passed, "failed": failed})
    return passed, failed


def _coordinate(run_id: str, config: dict, eq: Queue, run: dict):
    collections = config.get("collections") or []
    default_env_file = config.get("envFile") or None
    concurrency = max(1, int(config.get("concurrency") or 1))

    started_at = run["started_at"]
    total_passed = total_failed = 0

    _emit(run, eq, {"type": "run_started", "runId": run_id, "collections": len(collections), "concurrency": concurrency})

    try:
        with ThreadPoolExecutor(max_workers=concurrency) as ex:
            futures = [
                ex.submit(
                    _run_one_collection, run_id, c.get("directory", ""), c.get("filename", ""),
                    _resolve_path(c["envFile"]) if c.get("envFile") else (_resolve_path(default_env_file) if default_env_file else None),
                    eq, run,
                )
                for c in collections
                if not run["abort"].is_set()
            ]
            for fut in futures:
                p, f = fut.result()
                total_passed += p
                total_failed += f
    except Exception as exc:
        _emit(run, eq, {"type": "error", "error": str(exc)})
    finally:
        run["status"] = "stopped" if run["abort"].is_set() else ("failed" if total_failed else "passed")
        run["finished_at"] = int(time.time() * 1000)
        run["passed"] = total_passed
        run["failed"] = total_failed
        _emit(run, eq, {
            "type": "run_complete",
            "passed": total_passed,
            "failed": total_failed,
            "durationMs": run["finished_at"] - started_at,
            "aborted": run["abort"].is_set(),
        })
        _evict_old_runs()


def start_run(config: dict) -> str:
    run_id = str(uuid.uuid4())[:8]
    eq: Queue = Queue()
    now = int(time.time() * 1000)
    expanded_collections = _expand_collections(config.get("collections") or [])
    config = {**config, "collections": expanded_collections}
    run = {
        "eq": eq,
        "abort": threading.Event(),
        "lock": threading.Lock(),
        "procs": [],
        "events": [],
        "started_at": now,
        "finished_at": None,
        "triggered_by": config.get("triggeredBy", "ui"),
        "collections": [f"{c.get('directory', '')}/{c.get('filename', '')}" for c in expanded_collections],
        "status": "running",
        "passed": 0,
        "failed": 0,
    }
    with _runs_lock:
        _runs[run_id] = run

    t = threading.Thread(target=_coordinate, args=(run_id, config, eq, run), daemon=True)
    t.start()
    return run_id


def stop_run(run_id: str) -> bool:
    with _runs_lock:
        run = _runs.get(run_id)
    if not run:
        return False
    run["abort"].set()
    with run["lock"]:
        for proc in run["procs"]:
            if proc.poll() is None:
                try:
                    proc.terminate()
                except OSError:
                    pass
    return True


def get_queue(run_id: str):
    with _runs_lock:
        run = _runs.get(run_id)
    return run["eq"] if run else None


def get_run_detail(run_id: str) -> dict | None:
    """Full detail for one run — summary fields plus every event recorded
    (capped at MAX_EVENTS_PER_RUN), including request/response bodies for
    failures. Survives after the live SSE stream closes, for a caller (e.g.
    System Control Centre) that only checks in after the run has finished."""
    with _runs_lock:
        run = _runs.get(run_id)
        if not run:
            return None
        events = list(run["events"])
    return {
        "runId": run_id,
        "status": run["status"],
        "triggeredBy": run["triggered_by"],
        "collections": run["collections"],
        "startedAt": run["started_at"],
        "finishedAt": run["finished_at"],
        "passed": run["passed"],
        "failed": run["failed"],
        "events": events,
    }


def list_runs() -> list:
    with _runs_lock:
        runs = list(_runs.items())
    out = [{
        "runId": run_id,
        "status": run["status"],
        "triggeredBy": run["triggered_by"],
        "collections": run["collections"],
        "startedAt": run["started_at"],
        "finishedAt": run["finished_at"],
        "passed": run["passed"],
        "failed": run["failed"],
    } for run_id, run in runs]
    out.sort(key=lambda r: r["startedAt"], reverse=True)
    return out


def _evict_old_runs():
    with _runs_lock:
        finished = [rid for rid, r in _runs.items() if r["finished_at"] is not None]
        finished.sort(key=lambda rid: _runs[rid]["finished_at"])
        overflow = len(_runs) - MAX_TRACKED_RUNS
        for rid in finished[:max(0, overflow)]:
            _runs.pop(rid, None)
