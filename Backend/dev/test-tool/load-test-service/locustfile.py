import json
import os
import sys
import threading
from pathlib import Path

from locust import User, constant, events, task
from locust.exception import StopUser

from collection_runner import CollectionWorker

_worker_counter = 0
_worker_counter_lock = threading.Lock()

_cfg: dict = {}


@events.init.add_listener
def _load_config(environment, **kwargs):
    global _cfg
    cfg_path = os.getenv('LT_CFG', '')
    if cfg_path and Path(cfg_path).exists():
        try:
            _cfg = json.loads(Path(cfg_path).read_text())
            cd  = _cfg.get('collectionDir', '?')
            rpw = _cfg.get('ridesPerWorker', 1)
            wc  = _cfg.get('workerCount', 1)
            print(f'[locust] collection={cd}  workers={wc}  rides/worker={rpw}',
                  file=sys.stderr, flush=True)
        except Exception as e:
            print(f'[locust] LT_CFG load error: {e}', file=sys.stderr, flush=True)
    else:
        print('[locust] no LT_CFG — set LT_CFG env var to a config JSON path',
              file=sys.stderr, flush=True)


class WorkflowUser(User):
    wait_time = constant(0)

    def on_start(self):
        global _worker_counter
        with _worker_counter_lock:
            worker_id = _worker_counter
            _worker_counter += 1

        riders  = _cfg.get('riders', [])
        drivers = _cfg.get('drivers', [])
        rider_token  = riders[worker_id % len(riders)]  if riders  else None
        driver_token = drivers[worker_id % len(drivers)] if drivers else None

        self._worker = CollectionWorker(
            collection_dir=_cfg.get('collectionDir', ''),
            collection_suite=_cfg.get('collectionSuite', ''),
            base_env=_cfg.get('baseEnv', {}),
            rides_per_worker=int(_cfg.get('ridesPerWorker', 1)),
            worker_id=worker_id,
            rider_token=rider_token,
            driver_token=driver_token,
            pool_size=max(2, int(_cfg.get('workerCount', 1))),
        )

    @task
    def run_step(self):
        self._worker.tick()
