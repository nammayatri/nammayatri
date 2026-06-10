import json
import os
import re
import sys
import time
import threading
from pathlib import Path

from runner import (
    PmNodePool,
    AUTH_RESET_KEYS,
    SETUP_SERVICES,
    execute_step,
    _http,
)

COLLECTIONS_DIR = Path(__file__).resolve().parent.parent.parent / 'integration-tests' / 'collections'

_pool: PmNodePool | None = None
_pool_lock = threading.Lock()


def _get_pool(size: int = 4) -> PmNodePool:
    global _pool
    with _pool_lock:
        if _pool is None:
            _pool = PmNodePool(size=size)
    return _pool


_PORT_RE = re.compile(r'\$\{([A-Z_][A-Z0-9_]*)(?::([^}]*))?\}')


def _expand(val: str) -> str:
    return _PORT_RE.sub(lambda m: os.environ.get(m.group(1), m.group(2) or ''), val)


def load_env(collection_dir: str, env_subdir: str = 'Local') -> dict:
    env_dir = COLLECTIONS_DIR / collection_dir / env_subdir
    if env_subdir.endswith('.json'):
        candidates = [COLLECTIONS_DIR / collection_dir / env_subdir]
    else:
        candidates = sorted(env_dir.glob('*.json')) if env_dir.exists() else []

    for f in candidates:
        try:
            data = json.loads(f.read_text())
            return {
                v['key']: _expand(str(v.get('value', '')))
                for v in data.get('values', [])
                if v.get('enabled', True) and v.get('key')
            }
        except Exception:
            pass
    return {}


def _infer_service(raw_url: str) -> str:
    if '{{baseURL_BPP_Dashboard' in raw_url or '{{dashboard_base_url' in raw_url:
        return 'provider-dashboard'
    if '{{baseURL_namma_P' in raw_url or '{{baseUrl_lts' in raw_url:
        return 'driver'
    return 'rider'


def _collect_items(items: list, out: list):
    for item in items:
        if 'item' in item:
            _collect_items(item['item'], out)
            continue
        if 'request' not in item:
            continue
        req = item['request']
        url_obj = req.get('url', {})
        raw_url = url_obj.get('raw', '') if isinstance(url_obj, dict) else str(url_obj)

        headers: dict[str, str] = {}
        for h in req.get('header', []):
            if h.get('disabled'):
                continue
            headers[h.get('key', '')] = h.get('value', '')

        body_tmpl = None
        body_obj = req.get('body', {})
        if body_obj and body_obj.get('mode') == 'raw':
            body_tmpl = body_obj.get('raw') or None

        prereq = test_scr = ''
        for ev in item.get('event', []):
            lines = ev.get('script', {}).get('exec', [])
            text = '\n'.join(lines) if isinstance(lines, list) else (lines or '')
            if ev.get('listen') == 'prerequest':
                prereq = text
            elif ev.get('listen') == 'test':
                test_scr = text

        delay_ms = 0
        if prereq:
            m = re.search(r'setTimeout\s*\(\s*function\s*\(\s*\)\s*\{[^}]*\}\s*,\s*(\d+)', prereq)
            if m:
                delay_ms = int(m.group(1))

        out.append({
            'name': item.get('name', '(unnamed)'),
            'method': req.get('method', 'GET').upper(),
            'rawUrl': raw_url,
            'headers': headers,
            'bodyTemplate': body_tmpl,
            'prereqScript': prereq or None,
            'testScript': test_scr or None,
            'delayMs': delay_ms,
            'service': _infer_service(raw_url),
        })


def load_steps(collection_dir: str, collection_suite: str) -> list:
    path = COLLECTIONS_DIR / collection_dir / collection_suite
    data = json.loads(path.read_text())
    steps: list = []
    _collect_items(data.get('item', []), steps)
    return steps


def _csv(val: str) -> list[str]:
    return [v.strip() for v in val.split(',') if v.strip()]


def _nth(lst: list, idx: int, fallback: str = '') -> str:
    return lst[idx % len(lst)] if lst else fallback


def build_worker_env(env: dict, worker_id: int,
                     rider_token: dict | None,
                     driver_token: dict | None) -> dict:
    env = dict(env)

    driver_phones = _csv(env.get('load_test_driver_numbers', ''))
    rider_phones  = _csv(env.get('load_test_rider_numbers', ''))

    driver_phone = (driver_token or {}).get('phone') or _nth(driver_phones, worker_id)
    rider_phone  = (rider_token  or {}).get('phone') or _nth(rider_phones,  worker_id)

    env['_test_driver_number'] = driver_phone
    env['_test_rider_number']  = rider_phone

    reg_nos = _csv(env.get('load_test_reg_nos', ''))
    env['_test_reg_no'] = _nth(reg_nos, worker_id)

    gps_step = float(env.get('load_test_gps_step', '0.03'))
    cols     = int(env.get('load_test_gps_cols',   '5'))
    lon_dir  = int(env.get('load_test_gps_lon_dir', '1'))
    d_lat = (worker_id // cols) * gps_step
    d_lon = lon_dir * (worker_id % cols) * gps_step

    for key, default in (('origin_lat', '0'), ('origin_lon', '0'),
                         ('dest_lat', '0'), ('dest_lon', '0')):
        try:
            env[key] = str(float(env.get(key, default)) + (d_lat if 'lat' in key else d_lon))
        except (ValueError, TypeError):
            pass

    return env, driver_phone, rider_phone


class CollectionWorker:
    def __init__(
        self,
        collection_dir: str,
        collection_suite: str,
        base_env: dict,
        rides_per_worker: int,
        worker_id: int,
        rider_token: dict | None = None,
        driver_token: dict | None = None,
        pool_size: int = 4,
    ):
        self._worker_id        = worker_id
        self._rides_per_worker = rides_per_worker
        self._current_ride     = 0
        self._rides_passed     = 0
        self._rides_failed     = 0
        self._started_at       = int(time.time() * 1000)
        self._done             = False
        self._step_idx         = 0
        self._phase            = 'warmup'

        all_steps = load_steps(collection_dir, collection_suite)

        first_rider = next(
            (i for i, s in enumerate(all_steps) if s.get('service') == 'rider'),
            len(all_steps),
        )
        self._warmup_steps = all_steps[:first_rider]
        self._ride_steps   = list(all_steps)

        env = load_env(collection_dir)
        env.update(base_env)

        env, driver_phone, rider_phone = build_worker_env(
            env, worker_id, rider_token, driver_token
        )

        self._base_env     = env
        self._driver_phone = driver_phone
        self._rider_phone  = rider_phone

        self._first_rider_idx = next(
            (i for i, s in enumerate(self._ride_steps) if s.get('service') == 'rider'),
            0,
        )
        self._ride_restart_count = 0

        self._stores = self._fresh_stores()
        self._pool   = _get_pool(pool_size)

        self._emit_worker_update(
            status='running',
            phase='warmup' if self._warmup_steps else 'ride',
        )

    def _fresh_stores(self) -> dict:
        stores = {'environment': dict(self._base_env), 'collection': {}}
        for k in AUTH_RESET_KEYS:
            stores['environment'].pop(k, None)
        stores['collection']['_test_driver_number'] = self._driver_phone
        stores['collection']['_test_rider_number']  = self._rider_phone
        stores['collection']['_test_reg_no']        = self._base_env.get('_test_reg_no', '')
        return stores

    def tick(self):
        from locust.exception import StopUser

        if self._done:
            raise StopUser()

        if self._phase == 'warmup':
            self._tick_warmup()
            return

        if not self._ride_steps:
            self._done = True
            self._emit_worker_update(status='passed', finishedAt=int(time.time() * 1000))
            raise StopUser()

        if self._step_idx >= len(self._ride_steps):
            self._ride_complete(passed=True)
            if self._done:
                raise StopUser()
            return

        step   = self._ride_steps[self._step_idx]
        result = execute_step(step, self._stores, self._pool)
        self._step_idx += 1

        self._emit('step_result', rideIndex=self._current_ride, step=result)

        if not result.get('passed') and not result.get('skipped'):
            resp_body = result.get('responseBody') or ''

            if 'Add Vehicle' in step['name'] and step.get('service') == 'provider-dashboard':
                return

            if result.get('status') == 429 and 'Location Update' in step['name']:
                return

            if 'DRIVER_ACCOUNT_DISABLED' in resp_body and self._ride_restart_count < 2:
                self._ride_restart_count += 1
                self._enable_driver_direct()
                self._step_idx -= 1
                return

            if ('ACTIVE_BOOKING_ALREADY_PRESENT' in resp_body
                    and self._ride_restart_count < 3):
                self._ride_restart_count += 1
                self._emit_worker_update(status='running', currentStep='Cancelling stuck booking…')
                self._cancel_active_booking()
                _RIDER_KEYS = [
                    'rider_token', 'rider_authId',
                    'searchId', 'estimateId', 'customer_bookingId',
                    'driver_ride_id', 'ride_otp', 'searchTryId',
                ]
                for k in _RIDER_KEYS:
                    self._stores['environment'].pop(k, None)
                    self._stores['collection'].pop(k, None)
                self._step_idx = self._first_rider_idx
                return

            self._ride_complete(passed=False)
            if self._done:
                raise StopUser()

    def _tick_warmup(self):
        if self._step_idx >= len(self._warmup_steps):
            self._phase    = 'ride'
            self._step_idx = 0
            self._stores   = self._fresh_stores()
            self._emit_worker_update(status='running', phase='ride')
            return

        step = self._warmup_steps[self._step_idx]
        self._step_idx += 1
        self._emit_worker_update(status='running', phase='warmup', currentStep=step['name'])
        try:
            result = execute_step(step, self._stores, self._pool)
            if result and not result.get('passed') and not result.get('skipped'):
                self._emit('step_result', rideIndex=-1, step=result)
        except Exception as exc:
            self._emit('step_result', rideIndex=-1, step={
                'name': step['name'], 'passed': False, 'skipped': False,
                'error': str(exc), 'status': 0, 'elapsed': 0,
                'method': step.get('method', ''), 'url': step.get('rawUrl', ''),
                'assertions': [], 'responseBody': None,
            })

    def _enable_driver_direct(self):
        env = self._stores.get('environment', {})
        col = self._stores.get('collection', {})
        token     = col.get('dashboard_token')  or env.get('dashboard_token', '')
        base_url  = env.get('baseURL_BPP_Dashboard_Internal', '')
        merchant  = env.get('dashboard_merchant_id', '')
        city      = env.get('dashboard_city', '')
        driver_id = col.get('driver_id')        or env.get('driver_id', '')
        if not all([token, base_url, merchant, city, driver_id]):
            return
        try:
            _http('POST', f'{base_url}/{merchant}/{city}/driver/{driver_id}/enable',
                  {'token': token, 'Content-Type': 'application/json'}, None)
        except Exception:
            pass

    def _cancel_active_booking(self):
        env    = self._stores.get('environment', {})
        col    = self._stores.get('collection', {})
        token  = col.get('rider_token') or env.get('rider_token', '')
        base   = env.get('baseUrl_app', '')
        if not token or not base:
            return
        headers = {'token': token, 'Content-Type': 'application/json'}
        cancel_body = json.dumps({"reasonCode": "OTHER", "reasonStage": "OnAssign"})
        try:
            _, list_body, _ = _http(
                'GET',
                f'{base}/rideBooking/list?limit=10&onlyActive=true&offset=0',
                headers, None,
            )
            data = json.loads(list_body)
            for booking in data.get('list', []):
                booking_id = booking.get('id')
                if booking_id:
                    _http('POST', f'{base}/rideBooking/{booking_id}/cancel',
                          headers, cancel_body)
        except Exception:
            pass

    def _ride_complete(self, passed: bool):
        if passed:
            self._rides_passed += 1
        else:
            self._rides_failed += 1
        self._current_ride       += 1
        self._step_idx            = 0
        self._ride_restart_count  = 0
        self._stores              = self._fresh_stores()

        if self._current_ride >= self._rides_per_worker:
            self._done = True
            status = 'failed' if self._rides_failed > 0 else 'passed'
            self._emit_worker_update(status=status, finishedAt=int(time.time() * 1000))
        else:
            self._emit_worker_update(status='running')

    def _emit(self, event_type: str, **kwargs):
        print('LT_EVT:' + json.dumps({'type': event_type, 'workerId': self._worker_id, **kwargs}),
              file=sys.stderr, flush=True)

    def _emit_worker_update(self, phase: str | None = None, **kwargs):
        self._emit(
            'worker_update',
            phase=phase or self._phase,
            currentRide=self._current_ride,
            totalRides=self._rides_per_worker,
            ridesPassed=self._rides_passed,
            ridesFailed=self._rides_failed,
            riderPhone=self._rider_phone,
            driverPhone=self._driver_phone,
            startedAt=self._started_at,
            **kwargs,
        )
