# /// script
# requires-python = ">=3.11,<3.14"
# dependencies = ["confluent-kafka>=2.3", "redis>=5"]
# ///
"""End-to-end check for the driver-pool-state stream.

Runs a test command (normally a ride suite) while recording two things side by
side, then checks that they agree:

  * every event on the `driver-pool-state` Kafka topic, and
  * every change to each driver's DriverPoolData in LTS Redis
    (`<prefix>driver-pool-data:<driverId>`), polled every --poll-ms.

Checks, per driver:
  1. ts       every event's ts is wall-clock epoch ms from this run
  2. shape    every event has a non-empty `changes` with DriverPoolData field names
  3. coverage every field change seen in LTS appears on the stream with the same
              new value, around the same time
  4. final    folding all events in order gives the same values LTS ends with

Usage (from Backend/dev/integration-tests, inside `nix develop .#backend`):
  uv run driver-pool-state/check_stream.py -- ./run-tests.sh auto NY_Bangalore 01-AutoRideFlow

Exits 1 if any check fails or the test command fails.
"""

import argparse
import json
import subprocess
import sys
import threading
import time
import uuid

import redis
from confluent_kafka import Consumer, KafkaError

# Bumped on every sync or written only by the cold-start builder / migrations;
# never part of a caller's change.
IGNORED_FIELDS = {"lastUpdatedAt", "schemaVersion", "driverId", "merchantOperatingCityId", "totalRides", "safetyPlusEnabled"}


def parse_args():
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--bootstrap", default="127.0.0.1:29092")
    p.add_argument("--topic", default="driver-pool-state")
    p.add_argument("--redis-host", default="localhost")
    p.add_argument("--redis-port", type=int, default=6379)
    p.add_argument("--prefix", default="dynamic-offer-driver-app-lts:", help="LTS Redis key prefix")
    p.add_argument("--poll-ms", type=int, default=100, help="LTS polling interval")
    p.add_argument("--settle-s", type=float, default=5.0, help="keep recording after the command ends")
    p.add_argument("--slack-s", type=float, default=3.0, help="time tolerance when matching LTS changes to events")
    p.add_argument("command", nargs=argparse.REMAINDER, help="test command, after --")
    a = p.parse_args()
    if a.command and a.command[0] == "--":
        a.command = a.command[1:]
    if not a.command:
        p.error("give a test command after --")
    return a


class Recorder:
    def __init__(self, args):
        self.args = args
        self.stop = threading.Event()
        self.events = []  # (recv_time, driverId, ts, changes, raw)
        self.snapshots = {}  # driverId -> [(time, record)]
        self.lock = threading.Lock()
        self.consumer = Consumer(
            {
                "bootstrap.servers": args.bootstrap,
                "group.id": f"driver-pool-state-check-{uuid.uuid4()}",
                "auto.offset.reset": "latest",
                "enable.auto.commit": False,
                "allow.auto.create.topics": True,
            }
        )
        self.assigned = threading.Event()
        self.consumer.subscribe([args.topic], on_assign=lambda c, parts: self.assigned.set())
        self.redis = redis.Redis(host=args.redis_host, port=args.redis_port, decode_responses=True)

    def consume(self):
        while not self.stop.is_set():
            msg = self.consumer.poll(0.2)
            if msg is None:
                continue
            if msg.error():
                if msg.error().code() != KafkaError._PARTITION_EOF:
                    print(f"kafka error: {msg.error()}", file=sys.stderr)
                continue
            raw = msg.value().decode()
            try:
                ev = json.loads(raw)
            except json.JSONDecodeError:
                ev = {}
            with self.lock:
                self.events.append((time.time(), ev.get("driverId"), ev.get("ts"), ev.get("changes"), raw))
        self.consumer.close()

    def poll_lts(self):
        pattern = f"{self.args.prefix}driver-pool-data:*"
        last = {}
        while not self.stop.is_set():
            keys = list(self.redis.scan_iter(pattern, count=1000))
            if keys:
                now = time.time()
                for key, val in zip(keys, self.redis.mget(keys)):
                    if val is None:
                        continue
                    record = json.loads(val)
                    did = key.rsplit(":", 1)[1]
                    comparable = {k: v for k, v in record.items() if k not in IGNORED_FIELDS}
                    if last.get(did) != comparable:
                        last[did] = comparable
                        with self.lock:
                            self.snapshots.setdefault(did, []).append((now, record))
            time.sleep(self.args.poll_ms / 1000)


def check(rec, started, finished, slack):
    failures, notes = [], []
    events_by_driver = {}
    for t, did, ts, changes, raw in rec.events:
        events_by_driver.setdefault(did, []).append((t, ts, changes, raw))

    # 1 + 2: ts and shape
    lo, hi = (started - 60) * 1000, (finished + 60) * 1000
    for t, did, ts, changes, raw in rec.events:
        if did is None or not isinstance(changes, dict):
            failures.append(f"malformed event: {raw[:200]}")
            continue
        if not isinstance(ts, (int, float)) or not lo <= ts <= hi:
            failures.append(f"[{did}] ts {ts} is not wall-clock epoch ms of this run (expected {int(lo)}..{int(hi)})")
        if not changes:
            failures.append(f"[{did}] event with empty changes")
        known = set()
        for _, record in rec.snapshots.get(did, []):
            known |= set(record)
        unknown = set(changes) - known if known else set()
        if unknown:
            failures.append(f"[{did}] fields not in DriverPoolData: {sorted(unknown)}")

    for did, snaps in rec.snapshots.items():
        evs = events_by_driver.get(did, [])
        if not evs and len(snaps) < 2:
            continue  # untouched by this run
        # 3: coverage — every LTS field change has a matching event
        for (t0, before), (t1, after) in zip(snaps, snaps[1:]):
            for field in sorted(set(before) | set(after)):
                if field in IGNORED_FIELDS or before.get(field) == after.get(field):
                    continue
                new = after.get(field)
                hit = any(
                    field in changes and changes[field] == new and t0 - slack <= t <= t1 + slack
                    for t, _, changes, _ in evs
                )
                if not hit:
                    failures.append(f"[{did}] LTS changed {field}: {json.dumps(before.get(field))} -> {json.dumps(new)}, no matching event")
        # 4: final — folded events equal LTS's final values
        folded = {}
        for _, _, changes, _ in sorted(evs, key=lambda e: e[1] or 0):
            folded.update(changes or {})
        final = snaps[-1][1]
        for field, value in folded.items():
            if field in final and final[field] != value:
                failures.append(f"[{did}] final {field}: stream says {json.dumps(value)}, LTS has {json.dumps(final[field])}")
        notes.append(f"[{did}] {len(evs)} events, {len(snaps)} LTS states, {len(folded)} fields published")

    for did in events_by_driver:
        if did not in rec.snapshots:
            notes.append(f"[{did}] {len(events_by_driver[did])} events but no LTS entry (sync skipped this driver)")
    return failures, notes


def main():
    args = parse_args()
    rec = Recorder(args)
    threads = [threading.Thread(target=rec.consume, daemon=True), threading.Thread(target=rec.poll_lts, daemon=True)]
    for th in threads:
        th.start()
    if not rec.assigned.wait(30):
        print("could not subscribe to the topic within 30s; is Kafka up?", file=sys.stderr)
        return 1
    time.sleep(1)  # let LTS polling record the starting state

    started = time.time()
    print(f"$ {' '.join(args.command)}", flush=True)
    code = subprocess.call(args.command)
    finished = time.time()
    time.sleep(args.settle_s)
    rec.stop.set()
    for th in threads:
        th.join(5)

    failures, notes = check(rec, started, finished, args.slack_s)
    print("\n── driver-pool-state stream check ──")
    print(f"events received: {len(rec.events)}  drivers in LTS: {len(rec.snapshots)}")
    for n in notes:
        print("  " + n)
    if code != 0:
        failures.insert(0, f"test command exited with {code}")
    if not rec.events:
        failures.append("no events received on the topic")
    if failures:
        print(f"\nFAIL ({len(failures)}):")
        for f in failures:
            print("  ✗ " + f)
        return 1
    print("\nPASS: every LTS DriverPoolData change was published with the same values")
    return 0


if __name__ == "__main__":
    sys.exit(main())
