import copy
import json
import os
import re
import subprocess
import threading
import time
import uuid
from concurrent.futures import ThreadPoolExecutor, wait as futures_wait, ALL_COMPLETED
from pathlib import Path
from queue import Empty, Queue

NODE_PATH = '/opt/homebrew/bin/node'
PM_RUNNER_PATH = os.path.join(os.path.dirname(__file__), 'pm_runner.js')

COLLECTIONS_DIR = Path(__file__).resolve().parent.parent.parent / 'integration-tests' / 'collections'
_ENV_VAR_RE = re.compile(r'\$\{([A-Z_][A-Z0-9_]*)(?::([^}]*))?\}')

SETUP_SERVICES = {'provider-dashboard'}

AUTH_RESET_KEYS = [
    'driver_token', 'driver_id', 'driver_authId',
    'driver2_token', 'driver2_id', 'driver2_authId',
    'rider_token', 'rider_authId',
    'searchId', 'estimateId', 'customer_bookingId',
    'driver_ride_id', 'ride_otp', 'searchTryId',
]

def resolve_vars(template, stores):
    """Replace {{key}} placeholders from stores (collection has priority)."""
    if template is None:
        return None
    s = str(template)

    def _replace(m):
        key = m.group(1)
        col = stores.get('collection', {})
        env = stores.get('environment', {})
        if key in col:
            return str(col[key])
        if key in env:
            return str(env[key])
        return m.group(0)

    return re.sub(r'\{\{(\w+)\}\}', _replace, s)

class _PmProcess:
    """Single persistent pm_runner.js process with a per-process lock."""

    def __init__(self):
        self._lock = threading.Lock()
        self._proc = self._spawn()

    def _spawn(self):
        return subprocess.Popen(
            [NODE_PATH, PM_RUNNER_PATH],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            bufsize=0,
        )

    def run(self, script: str, response_body: str, response_status: int,
            response_headers: dict, stores: dict) -> dict:
        req_line = json.dumps({
            'id': str(uuid.uuid4()),
            'script': script,
            'responseBody': response_body,
            'responseStatus': response_status,
            'responseHeaders': response_headers or {},
            'stores': copy.deepcopy(stores),
        }) + '\n'

        with self._lock:
            try:
                if self._proc.poll() is not None:
                    self._proc = self._spawn()
                self._proc.stdin.write(req_line.encode('utf-8'))
                self._proc.stdin.flush()
                out = self._proc.stdout.readline()
                return json.loads(out.decode('utf-8'))
            except Exception as e:
                try:
                    self._proc.kill()
                except Exception:
                    pass
                self._proc = self._spawn()
                return {'stores': stores, 'assertions': [], 'consoleLogs': [],
                        'error': str(e), 'skipped': False}

    def close(self):
        try:
            self._proc.terminate()
        except Exception:
            pass

class PmNodePool:
    def __init__(self, size: int = 4):
        self._q: Queue[_PmProcess] = Queue()
        for _ in range(max(1, size)):
            self._q.put(_PmProcess())

    def run_script(self, script: str, response_body: str, response_status: int,
                   response_headers: dict, stores: dict) -> dict:
        proc = self._q.get()
        try:
            return proc.run(script, response_body, response_status, response_headers, stores)
        finally:
            self._q.put(proc)

    def close(self):
        while not self._q.empty():
            try:
                self._q.get_nowait().close()
            except Empty:
                break

def _load_env_from_file(collection_dir: str, env_file: str) -> dict:
    """
    Load env vars from a specific postman_environment.json on disk.
    Expands ${VAR:default} patterns from OS environment.
    Returns empty dict on any failure.
    """
    path = COLLECTIONS_DIR / collection_dir / env_file
    if not path.exists():
        for subdir in ('Local', 'Master'):
            candidate = COLLECTIONS_DIR / collection_dir / subdir / env_file
            if candidate.exists():
                path = candidate
                break
        else:
            return {}
    try:
        data = json.loads(path.read_text())
        return {
            v['key']: _ENV_VAR_RE.sub(
                lambda m: os.environ.get(m.group(1), m.group(2) or ''),
                str(v.get('value', '')),
            )
            for v in data.get('values', [])
            if v.get('enabled', True) and v.get('key')
        }
    except Exception:
        return {}

def _worker_offset(worker_id: int, gps_step: float = 0.03, cols: int = 5,
                   lon_dir: int = 1) -> tuple[float, float]:
    """N-column grid spreading north (+lat) and east or west for lon.
    lon_dir=1  → east  (safe for inland cities: Bangalore, Delhi, Kolkata)
    lon_dir=-1 → west  (safe for coastal cities: Chennai — Bay of Bengal is east)
    Default: 5 cols × 0.03° ≈ 3.3 km isolation."""
    d_lat = (worker_id // cols) * gps_step
    d_lon = lon_dir * (worker_id % cols) * gps_step
    return d_lat, d_lon

def build_stores(base_env: dict, rider: dict, driver: dict, worker_id: int) -> dict:
    gps_step = float(base_env.get('load_test_gps_step', '0.03'))
    cols     = int(base_env.get('load_test_gps_cols',  '5'))
    lon_dir  = int(base_env.get('load_test_gps_lon_dir', '1'))
    d_lat, d_lon = _worker_offset(worker_id, gps_step, cols, lon_dir)
    o_lat = float(base_env.get('origin_lat', 0)) + d_lat
    o_lon = float(base_env.get('origin_lon', 0)) + d_lon
    d_la2 = float(base_env.get('dest_lat',   0)) + d_lat
    d_lo2 = float(base_env.get('dest_lon',   0)) + d_lon

    env = {**base_env,
           '_test_rider_number':  rider['phone'],
           '_test_driver_number': driver['phone'],
           'origin_lat': str(o_lat), 'origin_lon': str(o_lon),
           'dest_lat':   str(d_la2), 'dest_lon':   str(d_lo2)}

    col = {'_test_rider_number':  rider['phone'],
           '_test_driver_number': driver['phone'],
           'origin_lat': str(o_lat), 'origin_lon': str(o_lon),
           'dest_lat':   str(d_la2), 'dest_lon':   str(d_lo2)}

    return {'environment': env, 'collection': col}

def _reset_auth(stores: dict) -> None:
    for k in AUTH_RESET_KEYS:
        stores['collection'].pop(k, None)
        stores['environment'].pop(k, None)

import urllib.request
import urllib.error

def _http(method: str, url: str, headers: dict, body: str | None):
    """Returns (status, body_str, headers_dict)."""
    data = body.encode('utf-8') if body else None
    req = urllib.request.Request(url, data=data, method=method.upper())
    for k, v in headers.items():
        if v:
            req.add_header(k, v)
    try:
        with urllib.request.urlopen(req, timeout=30) as r:
            return r.status, r.read().decode('utf-8', errors='replace'), dict(r.headers)
    except urllib.error.HTTPError as e:
        return e.code, e.read().decode('utf-8', errors='replace'), dict(e.headers)
    except Exception as e:
        return 0, str(e), {}

def _detect_variable_delay(script: str, stores: dict) -> int:
    """
    Handle prereq delay patterns that use a pm.environment.get() variable, e.g.:
      const delay = pm.environment.get('booking_wait_ms') || 5000;
    The TypeScript parser only detects literal-number delays; this covers the rest.
    """
    m = re.search(
        r"pm\.(?:environment|globals|variables)\.get\(['\"](\w+)['\"]\)\s*\|\|\s*(\d+)",
        script
    )
    if not m:
        return 0
    var_name, default_ms = m.group(1), int(m.group(2))
    val = (stores.get('environment', {}).get(var_name)
           or stores.get('collection', {}).get(var_name))
    try:
        return int(val) if val else default_ms
    except (TypeError, ValueError):
        return default_ms

_RETRY_ON_EMPTY  = {'Get Nearby Ride Requests (Driver)', 'Get Search Results'}
_RETRY_MAX       = 12
_RETRY_DELAY_S   = 3.0

_VERIFY_AFTER = {
    'Ride Search': 'searchId',
}
_VERIFY_RETRY_MAX   = 8
_VERIFY_RETRY_DELAY = 3.0

def _store_get(stores: dict, key: str) -> str:
    """Return the value of key from collection (priority) or environment."""
    return (stores.get('collection', {}).get(key)
            or stores.get('environment', {}).get(key)
            or '')

def execute_step(step: dict, stores: dict, pool: PmNodePool) -> dict:
    t0 = int(time.time() * 1000)
    step_name = step['name']
    method    = step.get('method', 'GET')
    prereq = step.get('prereqScript')
    if prereq:
        res = pool.run_script(prereq, 'null', 0, {}, stores)
        if res.get('skipped'):
            return {'name': step_name, 'method': method,
                    'url': '', 'status': 0, 'elapsed': 0, 'passed': True,
                    'skipped': True, 'assertions': [], 'error': None,
                    'requestBody': None, 'requestHeaders': {}, 'responseBody': None}
        stores['environment'].update(res['stores'].get('environment', {}))
        stores['collection'].update(res['stores'].get('collection', {}))

    delay_ms = int(step.get('delayMs', 0))
    if delay_ms == 0 and prereq:
        delay_ms = _detect_variable_delay(prereq, stores)
    if delay_ms > 0:
        time.sleep(delay_ms / 1000.0)

    url = resolve_vars(step.get('rawUrl', ''), stores)

    headers = {k: resolve_vars(v, stores) for k, v in step.get('headers', {}).items()
               if resolve_vars(v, stores)}

    body_tmpl = step.get('bodyTemplate')
    body = resolve_vars(body_tmpl, stores) if body_tmpl else None
    if body and 'Content-Type' not in headers and 'content-type' not in headers:
        headers['Content-Type'] = 'application/json'

    status, resp_body, resp_headers = _http(method, url, headers, body)

    assertions: list = []
    script_err = None
    test_scr = step.get('testScript')
    if test_scr:
        res = pool.run_script(test_scr, resp_body, status, resp_headers, stores)
        stores['environment'].update(res['stores'].get('environment', {}))
        stores['collection'].update(res['stores'].get('collection', {}))
        assertions = res.get('assertions', [])
        script_err = res.get('error')

    failed_a = next((a for a in assertions if not a.get('passed')), None)
    passed = failed_a is None and script_err is None

    required_key = _VERIFY_AFTER.get(step_name)
    if required_key and passed and not _store_get(stores, required_key):
        for _kc in range(_VERIFY_RETRY_MAX):
            time.sleep(_VERIFY_RETRY_DELAY)
            url2     = resolve_vars(step.get('rawUrl', ''), stores)
            headers2 = {k: resolve_vars(v, stores) for k, v in step.get('headers', {}).items()
                        if resolve_vars(v, stores)}
            body2    = resolve_vars(body_tmpl, stores) if body_tmpl else None
            if body2 and 'Content-Type' not in headers2 and 'content-type' not in headers2:
                headers2['Content-Type'] = 'application/json'
            status, resp_body, resp_headers = _http(method, url2, headers2, body2)
            if test_scr:
                res_k      = pool.run_script(test_scr, resp_body, status, resp_headers, stores)
                res_k_env  = res_k.get('stores', {}).get('environment', {})
                res_k_col  = res_k.get('stores', {}).get('collection', {})
                assertions = res_k.get('assertions', [])
                script_err = res_k.get('error')
                failed_a   = next((a for a in assertions if not a.get('passed')), None)
                passed     = failed_a is None and script_err is None
                key_found  = res_k_env.get(required_key) or res_k_col.get(required_key)
                if passed and key_found:
                    stores['environment'].update(res_k_env)
                    stores['collection'].update(res_k_col)
                    url = url2
                    break
            else:
                passed = (status == 200)
                if passed:
                    url = url2
                    break

    def _should_retry_empty() -> bool:
        if step_name not in _RETRY_ON_EMPTY:
            return False
        if passed:
            return False
        if status == 200:
            err_lower = (failed_a.get('error') or '').lower() if failed_a else ''
            if 'empty' in err_lower or 'above 0' in err_lower or 'above' in err_lower:
                return True
            try:
                body = json.loads(resp_body or '{}')
                if isinstance(body.get('estimates'), list) and len(body['estimates']) == 0:
                    return True
            except Exception:
                pass
        if status in (400, 404) and step_name == 'Get Search Results':
            return True
        return False

    if _should_retry_empty():
        for _attempt in range(_RETRY_MAX):
            time.sleep(_RETRY_DELAY_S)
            url_r = resolve_vars(step.get('rawUrl', ''), stores)
            status, resp_body, resp_headers = _http(method, url_r, headers, body)
            if test_scr:
                retry_stores = copy.deepcopy(stores)
                res2 = pool.run_script(test_scr, resp_body, status, resp_headers, retry_stores)
                assertions = res2.get('assertions', [])
                script_err  = res2.get('error')
                failed_a    = next((a for a in assertions if not a.get('passed')), None)
                passed      = failed_a is None and script_err is None
                if passed:
                    stores['environment'].update(res2['stores'].get('environment', {}))
                    stores['collection'].update(res2['stores'].get('collection', {}))
                    url = url_r
                    break
            else:
                passed = (status == 200)
                if passed:
                    url = url_r
                    break

    elapsed = int(time.time() * 1000) - t0

    def _trunc(s: str | None, limit: int = 8192) -> str | None:
        if not s:
            return s
        return s[:limit] + (f'\n…[{len(s)-limit} more chars]' if len(s) > limit else '')

    return {
        'name': step_name,
        'method': method,
        'url': url,
        'status': status,
        'elapsed': elapsed,
        'passed': passed,
        'skipped': False,
        'assertions': assertions,
        'error': (failed_a['error'] if failed_a else script_err),
        'requestBody': _trunc(body),
        'requestHeaders': {k: v for k, v in headers.items()},
        'responseBody': _trunc(resp_body),
    }

def _warmup_worker(worker_id: int, rider: dict, driver: dict,
                   warmup_steps: list, base_env: dict,
                   pool: PmNodePool, eq: Queue, abort: threading.Event):
    """Phase 1: activate driver (auth → set location → go online)."""
    def emit(**kw):
        eq.put({'type': 'worker_update', 'workerId': worker_id, **kw})

    emit(status='running', phase='warmup', currentStep='',
         ridesPassed=0, ridesFailed=0, currentRide=0,
         riderPhone=rider['phone'], driverPhone=driver['phone'])

    stores = build_stores(base_env, rider, driver, worker_id)
    for step in warmup_steps:
        if abort.is_set():
            break
        if step.get('service') in SETUP_SERVICES:
            continue
        emit(currentStep=step['name'])
        try:
            execute_step(step, stores, pool)
        except Exception:
            pass  # warmup failures are non-fatal

_CANCEL_BODY = json.dumps({"reasonCode": "OTHER", "reasonStage": "OnAssign"})

def _cancel_booking(stores: dict):
    """Cancel ALL active bookings for the rider. Called on ACTIVE_BOOKING_ALREADY_PRESENT."""
    env   = stores.get('environment', {})
    col   = stores.get('collection', {})
    token = col.get('rider_token') or env.get('rider_token', '')
    base  = env.get('baseUrl_app', '')
    if not token or not base:
        return
    headers = {'token': token, 'Content-Type': 'application/json'}
    try:
        _, list_body, _ = _http(
            'GET', f'{base}/rideBooking/list?limit=10&onlyActive=true&offset=0',
            headers, None,
        )
        for booking in json.loads(list_body).get('list', []):
            bid = booking.get('id')
            if bid:
                _http('POST', f'{base}/rideBooking/{bid}/cancel', headers, _CANCEL_BODY)
    except Exception:
        pass

_RIDER_STATE_KEYS = [
    'rider_token', 'rider_authId',
    'searchId', 'estimateId', 'customer_bookingId',
    'driver_ride_id', 'ride_otp', 'searchTryId',
]

def _ride_worker(worker_id: int, rider: dict, driver: dict,
                 rides_per_worker: int, run_steps: list,
                 base_env: dict, pool: PmNodePool,
                 eq: Queue, abort: threading.Event):
    """Phase 2: run N complete rides for this worker."""
    def emit(**kw):
        eq.put({'type': 'worker_update', 'workerId': worker_id, **kw})

    emit(status='running', phase='ride', currentStep='',
         ridesPassed=0, ridesFailed=0, currentRide=0, totalRides=rides_per_worker,
         riderPhone=rider['phone'], driverPhone=driver['phone'])
    first_rider_idx = next(
        (i for i, s in enumerate(run_steps) if s.get('service') == 'rider'), 0
    )

    rides_passed = rides_failed = 0
    failed_name = failed_status = failed_reason = None

    for ride_idx in range(rides_per_worker):
        if abort.is_set():
            break

        emit(currentRide=ride_idx, currentStep='')
        stores = build_stores(base_env, rider, driver, worker_id)
        _reset_auth(stores)
        stores['collection']['_test_driver_number'] = driver['phone']
        stores['collection']['_test_rider_number']  = rider['phone']

        ride_ok       = True
        restart_count = 0
        step_idx      = 0

        while step_idx < len(run_steps):
            if abort.is_set():
                ride_ok = False
                break

            step = run_steps[step_idx]
            step_idx += 1

            if step.get('service') in SETUP_SERVICES:
                continue

            emit(currentStep=step['name'])
            try:
                result = execute_step(step, stores, pool)

                eq.put({'type': 'step_result', 'workerId': worker_id,
                        'rideIndex': ride_idx, 'step': result})

                if result.get('skipped'):
                    continue

                if not result['passed']:
                    if ('ACTIVE_BOOKING_ALREADY_PRESENT' in (result.get('responseBody') or '')
                            and restart_count < 3):
                        restart_count += 1
                        emit(currentStep='Cancelling stuck booking…')
                        _cancel_booking(stores)
                        for k in _RIDER_STATE_KEYS:
                            stores['environment'].pop(k, None)
                            stores['collection'].pop(k, None)
                        step_idx = first_rider_idx
                        continue

                    ride_ok       = False
                    failed_name   = step['name']
                    failed_status = result['status']
                    failed_reason = result.get('error')
                    break

            except Exception as exc:
                ride_ok       = False
                failed_name   = step['name']
                failed_reason = str(exc)
                eq.put({'type': 'step_result', 'workerId': worker_id,
                        'rideIndex': ride_idx,
                        'step': {'name': step['name'], 'method': step.get('method', ''),
                                 'url': '', 'status': 0, 'elapsed': 0,
                                 'passed': False, 'skipped': False,
                                 'assertions': [], 'error': str(exc),
                                 'requestBody': None, 'requestHeaders': {},
                                 'responseBody': None}})
                break

        if ride_ok:
            rides_passed += 1
        else:
            rides_failed += 1

        emit(ridesPassed=rides_passed, ridesFailed=rides_failed,
             failedStepName=failed_name,
             failedStepStatus=failed_status,
             failedStepReason=failed_reason)

    final = 'stopped' if abort.is_set() else ('passed' if rides_failed == 0 else 'failed')
    emit(status=final, currentStep='',
         finishedAt=int(time.time() * 1000),
         ridesPassed=rides_passed, ridesFailed=rides_failed,
         failedStepName=failed_name,
         failedStepStatus=failed_status,
         failedStepReason=failed_reason)

_runs: dict[str, dict] = {}
_runs_lock = threading.Lock()

def start_run(config: dict) -> str:
    run_id = str(uuid.uuid4())[:8]
    eq = Queue()
    abort = threading.Event()
    started_at = int(time.time() * 1000)

    with _runs_lock:
        _runs[run_id] = {'eq': eq, 'abort': abort, 'started_at': started_at}

    t = threading.Thread(target=_coordinate, args=(run_id, config, eq, abort), daemon=True)
    t.start()
    return run_id

def _coordinate(run_id: str, config: dict, eq: Queue, abort: threading.Event):
    steps          = config['steps']
    worker_count   = int(config['workerCount'])
    rides_pw       = int(config['ridesPerWorker'])
    rider_tokens   = config['riderTokens']
    driver_tokens  = config['driverTokens']
    collection_dir = config.get('collectionDir', '')
    env_file       = config.get('envFile', '')
    disk_env: dict = {}
    if collection_dir and env_file:
        disk_env = _load_env_from_file(collection_dir, env_file)
    base_env = {**disk_env, **config.get('baseEnv', {})}

    count = min(worker_count, len(rider_tokens), len(driver_tokens))

    run_steps = [s for s in steps if s.get('service') not in SETUP_SERVICES]

    first_rider = next((i for i, s in enumerate(run_steps) if s.get('service') == 'rider'), 0)
    warmup_steps = run_steps[:first_rider] if first_rider > 0 else []

    pool = PmNodePool(size=min(count, 8))

    try:
        if warmup_steps and not abort.is_set():
            eq.put({'type': 'phase_change', 'phase': 'warmup'})
            with ThreadPoolExecutor(max_workers=count) as ex:
                futs = [
                    ex.submit(_warmup_worker, i, rider_tokens[i], driver_tokens[i],
                              warmup_steps, base_env, pool, eq, abort)
                    for i in range(count)
                ]
                futures_wait(futs, return_when=ALL_COMPLETED)

        if abort.is_set():
            eq.put({'type': 'run_complete', 'durationMs': 0, 'aborted': True})
            return

        eq.put({'type': 'phase_change', 'phase': 'riding'})
        with ThreadPoolExecutor(max_workers=count) as ex:
            futs = [
                ex.submit(_ride_worker, i, rider_tokens[i], driver_tokens[i],
                          rides_pw, run_steps, base_env, pool, eq, abort)
                for i in range(count)
            ]
            futures_wait(futs, return_when=ALL_COMPLETED)

        started_at = _runs.get(run_id, {}).get('started_at', int(time.time() * 1000))
        eq.put({'type': 'run_complete',
                'durationMs': int(time.time() * 1000) - started_at,
                'aborted': abort.is_set()})

    except Exception as exc:
        eq.put({'type': 'error', 'error': str(exc)})
    finally:
        pool.close()
        with _runs_lock:
            _runs.pop(run_id, None)

def stop_run(run_id: str) -> bool:
    with _runs_lock:
        run = _runs.get(run_id)
    if run:
        run['abort'].set()
        return True
    return False

def get_queue(run_id: str):
    with _runs_lock:
        run = _runs.get(run_id)
    return run['eq'] if run else None
