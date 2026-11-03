# /// script
# requires-python = ">=3.11,<3.14"
# dependencies = ["confluent-kafka>=2.3", "redis>=5"]
# ///
"""Load test for the SESSIONIZER (+ DRIVER_POOL_STATE writer).

--drivers drivers ping every --ping-every seconds for --minutes minutes in
real time; a share of them go on and off rides or get blocked mid-run, and
every driver sends driver analytics events (pop-ups sent / accepted, ride
assigned / completed, ratings) at random times. Afterwards every
(bucket, city, driver) row of atlas_kafka.driver_analytics_5m is checked
against the reference model of test_sessionizer.py. Each run uses its own
city id, so it never mixes with other data.

Usage (from Backend/dev/integration-tests, stack running):
  uv run sessionizer/load_test.py                                  # 4000 drivers x 10 min
  uv run sessionizer/load_test.py --drivers 1000 --minutes 5       # smaller
Don't run run-tests.sh while this runs: it flushes the writers' Redis.
"""

import argparse
import importlib.util
import random
import sys
import time
import uuid
from pathlib import Path

import redis
from confluent_kafka import Producer


def load_module(name: str):
    spec = importlib.util.spec_from_file_location(name, Path(__file__).with_name(f"{name}.py"))
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


ts = load_module("test_sessionizer")


def say(msg: str):
    print(f"{time.strftime('%H:%M:%S')}  {msg}", flush=True)


def build(args, run_id: str):
    t0_ms = (int(time.time()) + 5) * 1000
    p = ts.Plan(t0_ms, run_id)
    rnd = random.Random(f"load-{run_id}")
    every, end = args.ping_every, args.minutes * 60
    sc = p.scenario("load", "load")
    for _ in range(args.drivers):
        d = p.driver(sc)
        first = rnd.uniform(0, every)                      # spread drivers over the interval
        pings = [first + k * every for k in range(int((end - first) // every) + 1)]
        p.state(0, d, **ts.IDLE)
        kind = rnd.choices(["idle", "ride", "blocked"], [65, 30, 5])[0]
        if kind == "ride" and len(pings) > 6:
            a = rnd.randint(1, len(pings) - 5)
            b = rnd.randint(a + 1, len(pings) - 2)
            p.state(pings[a] + 2, d, onRide=True)
            p.event(pings[a] + 3, d, {"total_bookings": 1, "driver_addition": rnd.choice([0, 10, 20.5])})
            p.state(pings[b] + 2, d, onRide=False)
            p.event(pings[b] + 3, d, {"finished_rides": 1, "gross_earnings": round(rnd.uniform(50, 400), 2)})
            p.event(pings[b] + 4, d, {"rides_with_rating": 1, "rating_sum": rnd.randint(1, 5)})
        elif kind == "blocked" and len(pings) > 3:
            p.state(pings[rnd.randint(1, len(pings) - 2)] + 2, d, blocked=True)
        for _ in range(args.popups_per_driver):
            at = rnd.uniform(1, end)
            p.event(at, d, {"popups_sent": 1})
            if rnd.random() < 0.4:
                p.event(at + rnd.uniform(1, 10), d, {"popups_accepted": 1})
        for s in pings:
            p.ping(s, d)
    return p


def play(p, bootstrap: str):
    prod = Producer({"bootstrap.servers": bootstrap, "broker.address.family": "v4", "linger.ms": 20,
                     "compression.type": "lz4", "queue.buffering.max.messages": 1_000_000})
    msgs = sorted(p.msgs, key=lambda m: m.send_s)
    end = msgs[-1].send_s
    next_report = 30
    for m in msgs:
        wait = p.t0 / 1000 + m.send_s - time.time()
        if wait > 0:
            prod.poll(0)
            time.sleep(wait)
        while True:
            try:
                prod.produce(m.topic, key=m.key.encode() if m.key else None, value=m.value)
                break
            except BufferError:
                prod.poll(0.1)
        prod.poll(0)
        if m.send_s >= next_report:
            say(f"t+{m.send_s:.0f}s of {end:.0f}s")
            next_report += 30
    prod.flush(120)


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--drivers", type=int, default=4000)
    ap.add_argument("--ping-every", type=float, default=10)
    ap.add_argument("--minutes", type=float, default=10)
    ap.add_argument("--popups-per-driver", type=int, default=10)
    ap.add_argument("--bootstrap", default="127.0.0.1:29092")
    ap.add_argument("--clickhouse", default="http://localhost:8123")
    ap.add_argument("--redis-host", default="localhost")
    ap.add_argument("--redis-port", type=int, default=6379)
    ap.add_argument("--redis-db", type=int, default=2)
    ap.add_argument("--timeout", type=int, default=900, help="seconds to wait for the sessionizer flush")
    args = ap.parse_args()

    run_id = uuid.uuid4().hex[:8]
    p = build(args, run_id)
    counts = {t: sum(1 for m in p.msgs if m.topic == t) for t in (ts.STATE_TOPIC, ts.LOCATION_TOPIC, ts.EVENTS_TOPIC)}
    say(f"run {run_id}: {args.drivers} drivers, {args.minutes} min; " + ", ".join(f"{n:,} {t}" for t, n in counts.items()))
    play(p, args.bootstrap)
    last = max(m.send_s for m in p.msgs)
    played = time.time()
    r = redis.Redis(host=args.redis_host, port=args.redis_port, db=args.redis_db, decode_responses=True)
    if not ts.wait_for_flush(r, p.ms(last), args.timeout):
        say("FAIL  the sessionizer did not flush in time")
        return 1
    print()
    say(f"flushed {time.time() - played:.0f}s after the last message")

    expected, _, _, _ = ts.model(p)
    events = ", ".join(f"sum({c}) AS {c}" for c in ts.EVENT_COLUMNS)
    rows = ts.ch(args.clickhouse, f"""
        SELECT toUnixTimestamp(bucket) * 1000 AS b, merchant_operating_city_id AS c, driver_id AS d,
               max(t.online) AS online, maxIf(t.online, onRide) AS on_ride, sum(t.pings) AS pings,
               sumIf(t.pings, state_known = 0) AS pings_unknown_state, {events}
        FROM atlas_kafka.driver_analytics_5m AS t WHERE merchant_operating_city_id = '{p.city}' GROUP BY b, c, d""")
    got = {(int(x["b"]), x["c"], x["d"]): {m: ts.num(x[m]) for m in ts.MEASURES} for x in rows}

    rep = ts.Report()
    for m in ts.MEASURES:
        want = round(sum(v[m] for v in expected.values()), 4)
        have = round(sum(v[m] for v in got.values()), 4)
        rep.check(want == have, f"total {m}: {have}", f"want {want}")
    bad = [k for k in set(expected) | set(got) if expected.get(k) != got.get(k)]
    rep.check(not bad, f"every (bucket, city, driver) row matches the model ({len(expected):,} rows)",
              "\n        ".join(f"{ts.iso(k[0])} {k[2]}  model: {ts.fmt(expected.get(k, {}))}  |  got: {ts.fmt(got.get(k, {}))}"
                                for k in sorted(bad)[:20]))
    print(f"\n{'ALL PASS' if not rep.failures else f'{rep.failures} FAILED'}  (run {run_id})")
    return 1 if rep.failures else 0


if __name__ == "__main__":
    sys.exit(main())
