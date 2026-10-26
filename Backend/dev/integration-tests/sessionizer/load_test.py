# /// script
# requires-python = ">=3.11,<3.14"
# dependencies = ["confluent-kafka>=2.3", "redis>=5"]
# ///
"""Load test for both writers of atlas_kafka.driver_analytics_5m.

Runs two loads at the same time and checks exact totals afterwards:

  A. DRIVER_PERFORMANCE: a burst of drainer CDC messages (pop-ups with
     duplicates and Pulled retractions, ride lifecycles, cancellations, fare
     params, rider bookings, ratings, driver fees) for --perf-drivers drivers,
     produced as fast as possible. Reports how fast the consumer drains it.
  B. SESSIONIZER (+ DRIVER_POOL_STATE writer): --ping-drivers drivers pinging
     every --ping-every seconds for --minutes minutes in real time, a share of
     them going on and off rides or getting blocked mid-run.

Expected values come from the reference models of test_driver_performance.py
and test_sessionizer.py, so every row is checked bucket by bucket. Each run
uses its own city ids, so it never mixes with other data.

Usage (from Backend/dev/integration-tests, stack running):
  uv run sessionizer/load_test.py                       # ~200k pop-ups, ~16k rides, 4000 drivers x 10 min
  uv run sessionizer/load_test.py --perf-drivers 500 --ping-drivers 1000 --minutes 5   # smaller
Don't run run-tests.sh while this runs: it flushes the writers' Redis.
"""

import argparse
import importlib.util
import random
import sys
import threading
import time
import uuid
from collections import defaultdict
from pathlib import Path

import redis
from confluent_kafka import Consumer, ConsumerGroupTopicPartitions, Producer, TopicPartition
from confluent_kafka.admin import AdminClient


def load_module(name: str):
    spec = importlib.util.spec_from_file_location(name, Path(__file__).with_name(f"{name}.py"))
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


dp = load_module("test_driver_performance")
ts = load_module("test_sessionizer")

LOCK = threading.Lock()


def say(msg: str):
    with LOCK:
        print(f"{time.strftime('%H:%M:%S')}  {msg}", flush=True)


# ─── Kafka progress ──────────────────────────────────────────────────────────

class Progress:
    """Committed offsets of a consumer group vs the end of its topics."""

    def __init__(self, bootstrap: str, group: str, topics: list[str]):
        self.admin = AdminClient({"bootstrap.servers": bootstrap, "broker.address.family": "v4"})
        self.cons = Consumer({"bootstrap.servers": bootstrap, "group.id": f"load-test-probe-{uuid.uuid4().hex[:6]}",
                              "broker.address.family": "v4", "enable.auto.commit": False})
        self.group, self.topics = group, topics

    def ends(self) -> dict:
        out = {}
        for t in self.topics:
            md = self.cons.list_topics(t, timeout=10).topics[t]
            for p in md.partitions:
                out[(t, p)] = self.cons.get_watermark_offsets(TopicPartition(t, p), timeout=10)[1]
        return out

    def committed(self) -> dict:
        res = self.admin.list_consumer_group_offsets([ConsumerGroupTopicPartitions(self.group)])
        tps = res[self.group].result(10).topic_partitions
        return {(tp.topic, tp.partition): max(tp.offset, 0) for tp in tps if tp.topic in self.topics}

    def remaining(self, target: dict) -> int:
        c = self.committed()
        return sum(max(0, end - c.get(k, 0)) for k, end in target.items())


# ─── A. driver performance burst ─────────────────────────────────────────────

def perf_driver(base: int, run_id: str, rnd: random.Random, n_srd: int, n_rides: int):
    p = dp.Plan(base, run_id)
    d = p.driver(p.scenario("load", "load"))
    for _ in range(n_srd):
        off = rnd.randint(0, 2700)
        row = p.srd(d, off, status="Active")
        resp = rnd.choices(["Reject", "Pulled", None, "Accept"], [40, 35, 20, 5])[0]
        if resp is None and rnd.random() < 0.5:
            continue                                          # still showing, never updated
        rendered = rnd.randint(1, 5)
        responded = rendered + rnd.randint(1, 20) if resp in ("Accept", "Reject") else None
        p.send("srd", {**row, "status": "Inactive", "response": resp, "updated_at": row["created_at"] + 30,
                       "rendered_at": row["created_at"] + rendered,
                       "responded_at": None if responded is None else row["created_at"] + responded,
                       "is_auto_accepted": dp.b(resp == "Accept" and rnd.random() < 0.1)},
               times=2 if rnd.random() < 0.1 else 1)          # ~10% exact duplicates, as in prod
    for _ in range(n_rides):
        off = rnd.randint(0, 2400)
        rid, bid, fp_ride, fp_quote = (str(uuid.uuid4()) for _ in range(4))
        outcome = rnd.choices(["COMPLETED", "CANCELLED", "INPROGRESS"], [70, 25, 5])[0]
        p.dbooking(bid, off, auto=rnd.random() < 0.05, fp_id=fp_quote)
        if rnd.random() < 0.1:
            p.fp(fp_quote, driver_selected_fare=rnd.choice([10, 20]), customer_extra_fee=rnd.choice([None, 5]))
        p.ride(rid, bid, d, off, "NEW", fp_id=fp_ride)
        if outcome in ("INPROGRESS", "COMPLETED"):
            p.ride(rid, bid, d, off, "INPROGRESS", fp_id=fp_ride, upd=60)
        if outcome == "COMPLETED":
            p.ride(rid, bid, d, off, "COMPLETED", fare=rnd.randint(80, 600), toll=rnd.choice([None, None, 12.5]),
                   fp_id=fp_ride, upd=600, times=rnd.choice([1, 1, 2]))
            if rnd.random() < 0.05:
                p.fp(fp_ride, parking=20.0)
            p.rbooking(bid, off, {"instrumentType": "Cash"} if rnd.random() < 0.85
                       else {"instrumentType": "Card", "instrumentName": "DefaultCardType"})
            if rnd.random() < 0.3:
                p.rating(str(uuid.uuid4()), rid, d, off + 900, rnd.randint(1, 5))
        elif outcome == "CANCELLED":
            p.ride(rid, bid, d, off, "CANCELLED", fp_id=fp_ride, upd=120)
            p.bcr(bid, rid if rnd.random() < 0.9 else None, d, off + 120,
                  rnd.choice(["ByDriver", "ByUser", "ByApplication"]))
    if rnd.random() < 0.5:
        p.fee(str(uuid.uuid4()), d, 0, rnd.choice(["RECURRING_INVOICE", "MANDATE_REGISTRATION"]), 15.0)
    return p


def run_perf(args, run_id: str, result: dict):
    base = int(time.time()) - 50 * 60
    rnd = random.Random(f"perf-{run_id}")
    city = f"perf-test-{run_id}"
    prog = Progress(args.bootstrap, dp.GROUP, list(dp.T.values()))
    prod = Producer({"bootstrap.servers": args.bootstrap, "broker.address.family": "v4", "linger.ms": 20,
                     "compression.type": "lz4", "queue.buffering.max.messages": 1_000_000})
    expected: dict = {}
    sent = 0
    t0 = time.time()
    for i in range(args.perf_drivers):
        p = perf_driver(base, run_id, rnd, args.srds_per_driver, args.rides_per_driver)
        expected.update(dp.norm_rows(dp.model(p)))
        for m in p.msgs:
            while True:
                try:
                    prod.produce(m.topic, key=m.key.encode() if m.key else None, value=m.value)
                    break
                except BufferError:
                    prod.poll(0.1)
            prod.poll(0)
        sent += len(p.msgs)
        if (i + 1) % max(1, args.perf_drivers // 10) == 0:
            say(f"A  produced {sent:,} CDC messages for {i + 1:,} drivers ({sent / (time.time() - t0):,.0f} msg/s)")
    prod.flush(120)
    produced_s = time.time() - t0
    target = prog.ends()
    say(f"A  done producing: {sent:,} messages in {produced_s:.0f}s; waiting for driver-performance to commit them")
    samples = []
    deadline = time.time() + args.timeout
    while time.time() < deadline:
        left = prog.remaining(target)
        samples.append((time.time(), left))
        if left == 0:
            break
        say(f"A  driver-performance: {left:,} messages not yet committed")
        time.sleep(10)
    drained_s = time.time() - t0
    r = redis.Redis(host=args.redis_host, port=args.redis_port, db=args.redis_db, decode_responses=True)
    dp.wait_for_flush(r, int(time.time() * 1000), 180, "A  final flush")
    got = dp.query_rows(args.clickhouse, f"merchant_operating_city_id = '{city}'")
    result.update(perf=dict(sent=sent, produced_s=produced_s, drained_s=drained_s, samples=samples,
                            expected=expected, got=got, city=city, drained=samples and samples[-1][1] == 0))


# ─── B. sessionizer real-time pings ──────────────────────────────────────────

def ping_plan(args, run_id: str):
    t0_ms = (int(time.time()) + 5) * 1000
    p = ts.Plan(t0_ms, run_id)
    rnd = random.Random(f"ping-{run_id}")
    every, end = args.ping_every, args.minutes * 60
    sc = p.scenario("load", "load")
    for _ in range(args.ping_drivers):
        d = p.driver(sc)
        first = rnd.uniform(0, every)                      # spread drivers over the interval
        pings = [first + k * every for k in range(int((end - first) // every) + 1)]
        p.state(0, d, **ts.IDLE)
        kind = rnd.choices(["idle", "ride", "blocked"], [65, 30, 5])[0]
        if kind == "ride" and len(pings) > 6:
            a = rnd.randint(1, len(pings) - 5)
            b = rnd.randint(a + 1, len(pings) - 2)
            p.state(pings[a] + 2, d, onRide=True)          # 2 s after a ping: the writer has it well before the next
            p.state(pings[b] + 2, d, onRide=False)
        elif kind == "blocked" and len(pings) > 3:
            p.state(pings[rnd.randint(1, len(pings) - 2)] + 2, d, blocked=True)
        for s in pings:
            p.ping(s, d)
    return p, sc


def run_pings(args, run_id: str, result: dict):
    p, sc = ping_plan(args, run_id)
    msgs = sorted(p.msgs, key=lambda m: m.send_s)
    pings = sum(1 for m in msgs if m.topic == ts.LOCATION_TOPIC)
    say(f"B  playing {pings:,} pings + {len(msgs) - pings:,} state events for {args.ping_drivers:,} drivers "
        f"over {args.minutes} min ({pings / (args.minutes * 60):,.0f} pings/s)")
    prod = Producer({"bootstrap.servers": args.bootstrap, "broker.address.family": "v4", "linger.ms": 5,
                     "queue.buffering.max.messages": 1_000_000})
    by_second = defaultdict(list)
    for m in msgs:
        by_second[int(m.send_s)].append(m)
    late = 0.0
    for s in sorted(by_second):
        wait = p.t0 / 1000 + s - time.time()
        if wait > 0:
            time.sleep(wait)
        else:
            late = max(late, -wait)
        for m in by_second[s]:
            prod.produce(m.topic, key=m.key.encode() if m.key else None, value=m.value)
        prod.poll(0)
        if s % 60 == 0 and s:
            say(f"B  t+{s}s")
    prod.flush(60)
    say(f"B  done playing (max lateness {late:.1f}s); waiting for the sessionizer flush")
    last_st = max(p.ms(m.send_s) for m in msgs if m.topic == ts.LOCATION_TOPIC)
    r = redis.Redis(host=args.redis_host, port=args.redis_port, db=args.redis_db, decode_responses=True)
    ok = ts.wait_for_flush(r, last_st, args.timeout)
    activity, _, _ = ts.model(p)
    exp = {k: v for k, v in activity.items() if any(v.values())}
    rows = ts.ch(args.clickhouse, f"""
        SELECT toUnixTimestamp(bucket) * 1000 AS b, merchant_operating_city_id AS c, driver_id AS d,
               {", ".join(f"sum({m}) AS {m}" for m in ts.MEASURES)}
        FROM atlas_kafka.driver_analytics_5m WHERE merchant_operating_city_id = '{p.city}' GROUP BY b, c, d
        HAVING online_ms > 0 OR pings > 0""")
    got = {(int(x["b"]), x["c"], x["d"]): {m: int(x[m]) for m in ts.MEASURES} for x in rows}
    result.update(ping=dict(pings=pings, events=len(msgs) - pings, flushed=ok, expected=exp, got=got, late=late))


# ─── report ──────────────────────────────────────────────────────────────────

def compare(name: str, exp: dict, got: dict, measures: list[str], rep) -> None:
    keys = set(exp) | set(got)
    bad = [k for k in keys if exp.get(k) != got.get(k)]
    rep.check(not bad, f"{name}: all {len(exp):,} expected rows match exactly ({len(got):,} rows in ClickHouse)",
              f"{len(bad):,} rows differ, e.g. " + "; ".join(
                  f"{k}: want {exp.get(k)} got {got.get(k)}" for k in sorted(bad, key=str)[:3]))
    for m in measures:
        want = sum((dp.dec(v.get(m)) for v in exp.values()), dp.Decimal(0))
        have = sum((dp.dec(v.get(m)) for v in got.values()), dp.Decimal(0))
        if want or have:
            rep.check(want == have, f"{name}: total {m} = {want}", f"got {have}")


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--bootstrap", default="127.0.0.1:29092")
    ap.add_argument("--clickhouse", default="http://localhost:8123")
    ap.add_argument("--redis-host", default="localhost")
    ap.add_argument("--redis-port", type=int, default=6379)
    ap.add_argument("--redis-db", type=int, default=2)
    ap.add_argument("--perf-drivers", type=int, default=2000)
    ap.add_argument("--srds-per-driver", type=int, default=100)
    ap.add_argument("--rides-per-driver", type=int, default=8)
    ap.add_argument("--ping-drivers", type=int, default=4000)
    ap.add_argument("--ping-every", type=float, default=20.0, help="seconds between a driver's pings (<= 300)")
    ap.add_argument("--minutes", type=float, default=10.0)
    ap.add_argument("--timeout", type=int, default=1800, help="seconds to wait for each consumer to catch up")
    ap.add_argument("--assign-timeout", type=int, default=330)
    args = ap.parse_args()

    run_id = uuid.uuid4().hex[:8]
    say(f"load test {run_id}")
    if not dp.preflight(args):
        sys.exit(1)
    print()
    result: dict = {}
    threads = [threading.Thread(target=run_perf, args=(args, run_id, result)),
               threading.Thread(target=run_pings, args=(args, run_id, result))]
    for t in threads:
        t.start()
    for t in threads:
        t.join()

    rep = dp.Report()
    print("\n── A. driver-performance (CDC burst) ──")
    a = result.get("perf")
    if a:
        rate = a["sent"] / a["drained_s"] if a["drained"] else 0
        print(f"  {a['sent']:,} messages produced in {a['produced_s']:.0f}s; consumer committed all of them "
              f"{a['drained_s']:.0f}s after the start -> ~{rate:,.0f} msg/s end to end"
              if a["drained"] else f"  {a['sent']:,} messages produced; consumer did NOT catch up within --timeout")
        rep.check(a["drained"], "driver-performance consumed and committed every message")
        compare("driver-performance", a["expected"], a["got"], dp.MEASURES, rep)
    else:
        rep.check(False, "driver-performance load did not complete (see errors above)")
    print("\n── B. sessionizer (real-time pings) ──")
    b = result.get("ping")
    if b:
        print(f"  {b['pings']:,} pings and {b['events']:,} state events played (max producer lateness {b['late']:.1f}s)")
        rep.check(b["late"] < 5, "the player kept up with real time (lateness < 5 s)")
        rep.check(b["flushed"], "the sessionizer flushed past the last ping")
        compare("sessionizer", b["expected"], b["got"], ts.MEASURES, rep)
    else:
        rep.check(False, "sessionizer load did not complete (see errors above)")
    print(f"\n{'ALL PASS' if not rep.failures else f'{rep.failures} FAILED'}  (load test {run_id})")
    sys.exit(1 if rep.failures else 0)


if __name__ == "__main__":
    main()
