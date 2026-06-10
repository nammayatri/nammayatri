import json
import os
import shutil
import subprocess
import sys
import threading
import time
import uuid
from pathlib import Path
from queue import Queue

_LT_DIR = Path(__file__).resolve().parent

print(f'[locust_runner] loaded from {_LT_DIR}', flush=True)

_runs: dict = {}
_runs_lock = threading.Lock()


def start_run(body: dict) -> str:
    run_id = uuid.uuid4().hex[:12]
    eq: Queue = Queue()

    worker_count     = int(body.get('workerCount', 1))
    rides_per_worker = int(body.get('ridesPerWorker', 1))
    rider_tokens     = body.get('riderTokens', [])
    driver_tokens    = body.get('driverTokens', [])
    collection_dir   = body.get('collectionDir', '')
    collection_suite = body.get('collectionSuite', '')
    base_env         = body.get('baseEnv', {})

    cfg_path = _LT_DIR / f'.lt_cfg_{run_id}.json'
    cfg_path.write_text(json.dumps({
        'collectionDir':   collection_dir,
        'collectionSuite': collection_suite,
        'baseEnv':         base_env,
        'ridesPerWorker':  rides_per_worker,
        'workerCount':     worker_count,
        'riders':          rider_tokens,
        'drivers':         driver_tokens,
    }))

    env = {**os.environ, 'LT_CFG': str(cfg_path), 'PYTHONUNBUFFERED': '1'}

    total_users = worker_count
    spawn_rate  = max(1, total_users // 5)

    cmd = _locust_cmd() + [
        '-f', str(_LT_DIR / 'locustfile.py'),
        '--headless',
        '-u', str(total_users),
        '-r', str(spawn_rate),
        '--loglevel', 'WARNING',
    ]

    try:
        proc = subprocess.Popen(
            cmd,
            cwd=str(_LT_DIR),
            stdout=subprocess.DEVNULL,
            stderr=subprocess.PIPE,
            text=True,
            env=env,
        )
    except FileNotFoundError as e:
        eq.put({'type': 'error',
                'error': f'locust not found: {e}. Run: pipenv install'})
        cfg_path.unlink(missing_ok=True)
        return run_id

    abort = threading.Event()
    t0 = time.time()

    def _reader():
        try:
            for raw in proc.stderr:
                line = raw.rstrip()
                if line.startswith('LT_EVT:'):
                    try:
                        eq.put(json.loads(line[7:]))
                    except Exception:
                        pass
                elif line:
                    print(f'  [locust] {line}', flush=True)
        except Exception:
            pass
        finally:
            try:
                proc.wait(timeout=30)
            except Exception:
                try:
                    proc.kill()
                except Exception:
                    pass
                try:
                    proc.wait()
                except Exception:
                    pass
            if not abort.is_set():
                duration_ms = int((time.time() - t0) * 1000)
                eq.put({'type': 'run_complete', 'durationMs': duration_ms, 'aborted': False})
            try:
                cfg_path.unlink(missing_ok=True)
            except Exception:
                pass
            with _runs_lock:
                _runs.pop(run_id, None)

    threading.Thread(target=_reader, daemon=True).start()

    with _runs_lock:
        _runs[run_id] = {'proc': proc, 'queue': eq, 'abort': abort}

    return run_id


def stop_run(run_id: str) -> bool:
    with _runs_lock:
        run = _runs.get(run_id)
    if not run:
        return False
    run['abort'].set()
    proc = run['proc']
    if proc.poll() is None:
        proc.terminate()
        time.sleep(0.5)
        if proc.poll() is None:
            proc.kill()
    run['queue'].put({'type': 'run_complete', 'durationMs': 0, 'aborted': True})
    return True


def get_queue(run_id: str):
    with _runs_lock:
        run = _runs.get(run_id)
    return run['queue'] if run else None


def _locust_cmd() -> list:
    if (_LT_DIR / 'Pipfile').exists() and shutil.which('pipenv'):
        return ['pipenv', 'run', 'locust']
    if shutil.which('locust'):
        return ['locust']
    py = shutil.which('python3') or shutil.which('python') or sys.executable
    return [py, '-m', 'locust']
