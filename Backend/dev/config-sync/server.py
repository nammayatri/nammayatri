#!/usr/bin/env python3
"""HTTP wrapper around `config_transfer.py export` / `patch` for k8s utils.

Each /export and /patch starts config_transfer.py as a real subprocess so the
parent can SIGTERM it on demand (Python threads can't be cancelled cleanly).
Stdout is streamed line-by-line into a bounded per-task deque.

Run:
    python server.py                    # listens on 0.0.0.0:8090
    PORT=9000 python server.py

Endpoints:
    GET  /healthz                       liveness
    POST /export                        body: {"from":"...", "parallel":10,
                                               "schemas":[...], "tables":[...]}
    POST /patch                         body: {"from":"...", "to":"...", "s3":true,
                                               "s3_bucket":"...", "s3_prefix":"...",
                                               "schemas":[...]}
    GET  /tasks                         list known tasks (summaries)
    GET  /tasks/<id>                    status + captured stdout
    POST /tasks/<id>/stop               SIGTERM the running task (then SIGKILL after 5s)

Statuses: running | succeeded | failed | cancelled.
"""

import json
import os
import shlex
import signal
import subprocess
import sys
import threading
import time
import uuid
from collections import deque
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path

import config_transfer

VALID_ENVS = config_transfer.VALID_ENVS

SCRIPT_DIR = Path(__file__).resolve().parent
CONFIG_TRANSFER = SCRIPT_DIR / "config_transfer.py"
MARKER_PATH = config_transfer.MARKER_PATH


def _read_marker():
    try:
        with MARKER_PATH.open("r") as f:
            return json.load(f)
    except (FileNotFoundError, json.JSONDecodeError):
        return None

_TASKS = {}
_TASKS_LOCK = threading.Lock()
_LOG_MAX_LINES = 5000


def _new_task(cmd_name, args_dict, argv):
    task_id = uuid.uuid4().hex[:12]
    with _TASKS_LOCK:
        _TASKS[task_id] = {
            "id": task_id,
            "cmd": cmd_name,
            "args": args_dict,
            "argv": argv,
            "status": "running",
            "started_at": time.time(),
            "finished_at": None,
            "returncode": None,
            "log": deque(maxlen=_LOG_MAX_LINES),
            "proc": None,
        }
    return task_id


def _drain_into_log(task_id, proc):
    """Read child stdout line-by-line into the task's deque, then reap."""
    try:
        assert proc.stdout is not None
        for raw in iter(proc.stdout.readline, ""):
            line = raw.rstrip("\n")
            with _TASKS_LOCK:
                t = _TASKS.get(task_id)
                if t is not None:
                    t["log"].append(line)
        proc.stdout.close()
    finally:
        rc = proc.wait()
        with _TASKS_LOCK:
            t = _TASKS[task_id]
            t["returncode"] = rc
            t["finished_at"] = time.time()
            # If the user explicitly hit Stop we already flipped status to
            # 'cancelled' — preserve that even though SIGTERM produced a
            # non-zero exit code.
            if t["status"] != "cancelled":
                t["status"] = "succeeded" if rc == 0 else "failed"
            t["proc"] = None


def _spawn_task(cmd_name, args_dict, argv):
    task_id = _new_task(cmd_name, args_dict, argv)
    env = os.environ.copy()
    # Force unbuffered stdout from the child so log lines reach the deque
    # immediately instead of in 4-KiB chunks at process exit.
    env["PYTHONUNBUFFERED"] = "1"
    proc = subprocess.Popen(
        argv,
        cwd=str(SCRIPT_DIR),
        env=env,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
        bufsize=1,
        # New process group → SIGTERM/SIGKILL hits the whole tree (incl. the
        # passetto-server child that patch starts).
        start_new_session=True,
    )
    with _TASKS_LOCK:
        _TASKS[task_id]["proc"] = proc
        _TASKS[task_id]["pid"] = proc.pid
    threading.Thread(target=_drain_into_log, args=(task_id, proc), daemon=True).start()
    return task_id


def _stop_task(task_id, grace=5.0):
    with _TASKS_LOCK:
        t = _TASKS.get(task_id)
        if not t:
            return None
        proc = t.get("proc")
        if proc is None or proc.poll() is not None:
            return t["status"]
        t["status"] = "cancelled"
        pid = proc.pid

    # SIGTERM the whole process group so passetto-server (if patch spawned
    # it) and any other children get the signal too.
    try:
        os.killpg(os.getpgid(pid), signal.SIGTERM)
    except (ProcessLookupError, PermissionError):
        pass

    def _kill_if_stuck():
        time.sleep(grace)
        if proc.poll() is None:
            try:
                os.killpg(os.getpgid(pid), signal.SIGKILL)
            except (ProcessLookupError, PermissionError):
                pass
    threading.Thread(target=_kill_if_stuck, daemon=True).start()
    return "cancelled"


def _task_view(task):
    return {
        "id": task["id"],
        "cmd": task["cmd"],
        "args": task["args"],
        "status": task["status"],
        "started_at": task["started_at"],
        "finished_at": task["finished_at"],
        "returncode": task["returncode"],
        "pid": task.get("pid"),
        "log": list(task["log"]),
    }


def _export_argv(body):
    src = body.get("from")
    if src not in VALID_ENVS:
        raise ValueError(f"'from' must be one of {sorted(VALID_ENVS)}, got {src!r}")
    argv = [sys.executable, str(CONFIG_TRANSFER), "export", "--from", src]
    parallel = int(body.get("parallel", 4))
    argv += ["--parallel", str(parallel)]
    for s in body.get("schemas") or []:
        argv += ["--schema", str(s)]
    for t in body.get("tables") or []:
        argv += ["--table", str(t)]
    return {"from": src, "parallel": parallel,
            "schemas": body.get("schemas"), "tables": body.get("tables")}, argv


def _patch_argv(body):
    src, dst = body.get("from"), body.get("to")
    if src not in VALID_ENVS:
        raise ValueError(f"'from' must be one of {sorted(VALID_ENVS)}, got {src!r}")
    if dst not in VALID_ENVS:
        raise ValueError(f"'to' must be one of {sorted(VALID_ENVS)}, got {dst!r}")
    argv = [sys.executable, str(CONFIG_TRANSFER), "patch", "--from", src, "--to", dst]
    for s in body.get("schemas") or []:
        argv += ["--schema", str(s)]
    if body.get("s3"):
        argv += ["--s3"]
        if body.get("s3_bucket"):
            argv += ["--s3-bucket", str(body["s3_bucket"])]
        if body.get("s3_prefix"):
            argv += ["--s3-prefix", str(body["s3_prefix"])]
    return {"from": src, "to": dst,
            "schemas": body.get("schemas"),
            "s3": bool(body.get("s3")),
            "s3_bucket": body.get("s3_bucket"),
            "s3_prefix": body.get("s3_prefix")}, argv


class Handler(BaseHTTPRequestHandler):
    def log_message(self, fmt, *args):
        sys.stderr.write("[%s] %s\n" % (self.log_date_time_string(), fmt % args))

    def _cors_headers(self):
        self.send_header("Access-Control-Allow-Origin", "*")
        self.send_header("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
        self.send_header("Access-Control-Allow-Headers", "Content-Type")
        self.send_header("Access-Control-Max-Age", "600")

    def do_OPTIONS(self):
        self.send_response(204)
        self._cors_headers()
        self.send_header("Content-Length", "0")
        self.end_headers()

    def _send_json(self, status, payload):
        body = json.dumps(payload, default=str).encode()
        self.send_response(status)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self._cors_headers()
        self.end_headers()
        self.wfile.write(body)

    def _read_body(self):
        n = int(self.headers.get("Content-Length") or 0)
        if n == 0:
            return {}
        raw = self.rfile.read(n)
        return json.loads(raw) if raw else {}

    def do_GET(self):
        if self.path == "/healthz":
            return self._send_json(200, {"ok": True})
        if self.path == "/sync-marker":
            return self._send_json(200, {"marker": _read_marker()})
        if self.path == "/tasks":
            with _TASKS_LOCK:
                summary = [
                    {k: t[k] for k in ("id", "cmd", "status", "started_at",
                                       "finished_at", "returncode")}
                    for t in _TASKS.values()
                ]
            return self._send_json(200, {"tasks": summary})
        if self.path.startswith("/tasks/"):
            task_id = self.path.split("/tasks/", 1)[1]
            with _TASKS_LOCK:
                task = _TASKS.get(task_id)
                if not task:
                    return self._send_json(404, {"error": f"task {task_id} not found"})
                return self._send_json(200, _task_view(task))
        return self._send_json(404, {"error": f"no such route {self.path}"})

    def do_POST(self):
        # /tasks/<id>/stop has no body
        if self.path.startswith("/tasks/") and self.path.endswith("/stop"):
            task_id = self.path[len("/tasks/"):-len("/stop")]
            new_status = _stop_task(task_id)
            if new_status is None:
                return self._send_json(404, {"error": f"task {task_id} not found"})
            return self._send_json(200, {"id": task_id, "status": new_status})

        try:
            body = self._read_body()
        except json.JSONDecodeError as e:
            return self._send_json(400, {"error": f"invalid json: {e}"})

        if self.path == "/export":
            try:
                args, argv = _export_argv(body)
            except ValueError as e:
                return self._send_json(400, {"error": str(e)})
            task_id = _spawn_task("export", args, argv)
            self.log_message("started export task %s: %s", task_id, shlex.join(argv))
            return self._send_json(202, {"task_id": task_id, "status": "running"})

        if self.path == "/patch":
            try:
                args, argv = _patch_argv(body)
            except ValueError as e:
                return self._send_json(400, {"error": str(e)})
            task_id = _spawn_task("patch", args, argv)
            self.log_message("started patch task %s: %s", task_id, shlex.join(argv))
            return self._send_json(202, {"task_id": task_id, "status": "running"})

        return self._send_json(404, {"error": f"no such route {self.path}"})


def main():
    port = int(os.environ.get("PORT", "8090"))
    host = os.environ.get("HOST", "0.0.0.0")
    print(f"config-sync server listening on http://{host}:{port}")
    ThreadingHTTPServer((host, port), Handler).serve_forever()


if __name__ == "__main__":
    main()
