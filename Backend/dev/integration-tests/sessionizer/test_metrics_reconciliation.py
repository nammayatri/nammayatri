# /// script
# requires-python = ">=3.11,<3.14"
# dependencies = ["confluent-kafka>=2.3", "redis>=5"]
# ///
"""Metrics audit for atlas_kafka.driver_analytics_5m.

Generates a controlled mix of sessionizer (pings + state) and driver-performance
(CDC) events, waits for both consumers to flush, then reconciles:

  1. What we sent — message counts by topic / entity type
  2. Reference model — expected totals per metric column (rules from the spec)
  3. ClickHouse — query-time sums from driver_analytics_5m
  4. State grain — every (bucket, city, driver, state) row for activity metrics;
     every (bucket, city, driver, perf-state) row for performance metrics

Each run uses a fresh run id and city prefix so it does not collide with other data.

Usage (from Backend/dev/integration-tests, stack running):
  uv run sessionizer/test_metrics_reconciliation.py
  uv run sessionizer/test_metrics_reconciliation.py --fast

Do not run run-tests.sh during this test (it flushes Redis).
"""

from __future__ import annotations

import argparse
import importlib.util
import sys
import time
import uuid
from collections import defaultdict
from dataclasses import dataclass, field
from decimal import Decimal
from pathlib import Path

import redis
from confluent_kafka import Producer


def load_module(name: str):
    spec = importlib.util.spec_from_file_location(name, Path(__file__).with_name(f"{name}.py"))
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


dp = load_module("test_driver_performance")
ts = load_module("test_sessionizer")

ACTIVITY = ts.MEASURES
PERF = dp.MEASURES
ALL_MEASURES = ACTIVITY + PERF


@dataclass
class InputLedger:
    """Counts of messages the audit script produces."""
    by_topic: dict[str, int] = field(default_factory=lambda: defaultdict(int))
    pings: int = 0
    state_events: int = 0
    srd: int = 0
    rides: int = 0
    bookings: int = 0
    bcrs: int = 0
    fare_params: int = 0
    ratings: int = 0

    def note_ts(self, m: ts.Msg):
        self.by_topic[m.topic] += 1
        if m.topic == ts.LOCATION_TOPIC:
            self.pings += 1
        elif m.topic == ts.STATE_TOPIC:
            self.state_events += 1

    def note_dp(self, m: dp.Msg):
        self.by_topic[m.topic] += 1
        table = dp.TABLE_OF.get(m.topic)
        if table == "srd":
            self.srd += 1
        elif table == "ride":
            self.rides += 1
        elif table == "booking":
            self.bookings += 1
        elif table == "bcr":
            self.bcrs += 1
        elif table == "fp":
            self.fare_params += 1
        elif table == "rating":
            self.ratings += 1


def build_activity_plan(t0_ms: int, run_id: str) -> tuple[ts.Plan, float, int, InputLedger]:
    """Short sessionizer timeline: every activity column gets a non-zero total."""
    p = ts.Plan(t0_ms, run_id)
    p.city = f"metrics-audit-act-{run_id}"
    ledger = InputLedger()

    sc = p.scenario("audit_idle_onride", "idle + on-ride online and pings",
                    online_ms=40_000, online_on_ride_ms=20_000, pings=4)
    d = p.driver(sc)
    p.state(0, d, **ts.IDLE)
    p.ping(5, d)
    p.ping(25, d)
    p.state(30, d, onRide=True)
    p.ping(35, d)
    p.ping(55, d)

    sc = p.scenario("audit_unknown", "pings with no state -> unknown row",
                    online_ms=20_000, pings=2, pings_unknown_state=1)
    d = p.driver(sc)
    p.ping(5, d)
    p.ping(25, d)

    sc = p.scenario("audit_blocked", "blocked state row",
                    online_ms=20_000, pings=2)
    d = p.driver(sc)
    p.state(0, d, **{**ts.IDLE, "blocked": True})
    p.ping(5, d)
    p.ping(25, d)

    sc = p.scenario("audit_rating_col", "vehicleRating in state columns",
                    online_ms=20_000, pings=2)
    d = p.driver(sc)
    p.state(0, d, **{**ts.IDLE, "vehicleRating": 4.7})
    p.ping(5, d)
    p.ping(25, d)

    last = max(m.send_s for m in p.msgs)
    d = f"st-{run_id}-sentinel"
    p.ping(last + 10, d)
    sentinel_ms = p.ms(last + 10)
    for m in p.msgs:
        ledger.note_ts(m)
    return p, last + 10, sentinel_ms, ledger


def build_perf_plan(base_s: int, run_id: str) -> tuple[dp.Plan, InputLedger]:
    """Compact driver-performance plan: every performance column non-zero somewhere."""
    p = dp.Plan(base_s, run_id)
    p.city = f"metrics-audit-perf-{run_id}"
    ledger = InputLedger()

    sc = p.scenario("audit_popups", "accept + reject + neglected + response + auto",
                    popups_sent=3, popups_accepted=1, popups_rejected=1, popups_neglected=1,
                    response_secs_sum=4, auto_accept_search_requests=1)
    p.phase = 0
    d = p.driver(sc)
    p.state(d, p.t(0) * 1000, **{"mode": "ONLINE", "onRide": False, "selectedServiceTiers": ["AUTO_RICKSHAW"]})
    r_act = p.srd(d, 10, status="Active")
    p.srd(d, 10, "Accept", 1, 5, auto=True, sid=r_act["id"])
    p.srd(d, 20, "Reject", 1, 4)
    p.srd(d, 30, None, 1, None)

    sc = p.scenario("audit_completed", "finished ride + money + rating + auto-accept finished",
                    total_bookings=1, finished_rides=1, gross_earnings=250,
                    driver_addition=15, customer_tip=5, rides_with_rating=1, rating_sum=4,
                    auto_accept_bookings=1, auto_accept_finished_rides=1)
    p.phase = 1
    d = p.driver(sc)
    p.state(d, p.t(40) * 1000, **{"mode": "ONLINE", "onRide": False, "selectedServiceTiers": ["AUTO_RICKSHAW"]})
    rid, bid = dp.ids(2)
    fpq = str(uuid.uuid4())
    p.fp(fpq, driver_selected_fare=15, customer_extra_fee=5)
    p.dbooking(bid, 50, auto=True, fp_id=fpq)
    p.ride(rid, bid, d, 50, "NEW")
    p.ride(rid, bid, d, 50, "COMPLETED", fare=250, upd=120)
    p.rating(str(uuid.uuid4()), rid, d, 50 + 130, 4)

    sc = p.scenario("audit_cancel_auto", "cancelled + auto-accept booking flags",
                    total_bookings=1, cancelled_rides=1, rides_cancelled_by_driver=1,
                    auto_accept_bookings=1, auto_accept_cancelled_rides=1,
                    auto_accept_rides_cancelled_by_driver=1)
    p.phase = 2
    d = p.driver(sc)
    p.state(d, p.t(60) * 1000, **{"mode": "ONLINE", "onRide": True, "selectedServiceTiers": ["AUTO_RICKSHAW"]})
    rid, bid = dp.ids(2)
    p.dbooking(bid, 70, auto=True)
    p.ride(rid, bid, d, 70, "NEW")
    p.ride(rid, bid, d, 70, "CANCELLED", upd=30)
    p.bcr(bid, rid, d, 70 + 30, "ByDriver")

    sc = p.scenario("audit_inprogress", "in-progress ride",
                    total_bookings=1, inprogress_rides=1)
    p.phase = 3
    d = p.driver(sc)
    p.state(d, p.t(80) * 1000, **{"mode": "ONLINE", "onRide": False, "selectedServiceTiers": ["AUTO_RICKSHAW"]})
    rid, bid = dp.ids(2)
    p.dbooking(bid, 90)
    p.ride(rid, bid, d, 90, "NEW")
    p.ride(rid, bid, d, 90, "INPROGRESS", upd=10)

    sc = p.scenario("audit_cancel_user", "cancelled by user on booking id",
                    total_bookings=1, cancelled_rides=1, rides_cancelled_by_user=1,
                    auto_accept_rides_cancelled_by_user=1, auto_accept_bookings=1,
                    auto_accept_cancelled_rides=1)
    p.phase = 4
    d = p.driver(sc)
    p.state(d, p.t(100) * 1000, **{"mode": "ONLINE", "onRide": False, "selectedServiceTiers": ["AUTO_RICKSHAW"]})
    rid, bid = dp.ids(2)
    p.dbooking(bid, 110, auto=True)
    p.ride(rid, bid, d, 110, "NEW")
    p.ride(rid, bid, d, 110, "CANCELLED", upd=20)
    p.bcr(bid, None, d, 110 + 20, "ByUser")

    for m in p.msgs:
        ledger.note_dp(m)
    return p, ledger


def play_ts(plan: ts.Plan, bootstrap: str):
    ts.play(plan, bootstrap)


def play_dp(plan: dp.Plan, bootstrap: str, fast: bool, r: redis.Redis, grace: int, flush_timeout: int):
    prod = Producer({"bootstrap.servers": bootstrap, "linger.ms": 5})
    for ph in sorted({m.phase for m in plan.msgs}):
        msgs = [m for m in plan.msgs if m.phase == ph]
        dp.produce(prod, msgs)
        sent_ms = int(time.time() * 1000)
        print(f"  perf phase {ph}: sent {len(msgs)} messages")
        last = ph == max(m.phase for m in plan.msgs)
        if fast and not last:
            time.sleep(2)
        elif not dp.wait_for_flush(r, sent_ms + grace * 1000, flush_timeout, f"perf phase {ph}"):
            raise RuntimeError(f"driver-performance did not flush after phase {ph}")


def sum_measures(rows: dict, keys, measures: list[str]) -> dict:
    tot = {m: (Decimal(0) if m in dp.MONEY else 0) for m in measures}
    for k in keys:
        if k not in rows:
            continue
        for m in measures:
            v = rows[k].get(m, 0)
            if m in dp.MONEY:
                tot[m] += dp.dec(v)
            else:
                tot[m] += int(v) if not isinstance(v, Decimal) else int(v)
    return dict(tot)


def fmt_val(m: str, v) -> str:
    if m in dp.MONEY:
        return str(dp.dec(v))
    return str(int(v) if v is not None else 0)


def compare_totals(label: str, expected: dict, got: dict, measures: list[str], rep: dp.Report):
    print(f"\n── {label}: column totals (model vs ClickHouse) ──")
    print(f"  {'column':<32} {'model':>14} {'clickhouse':>14} {'ok':>4}")
    for m in measures:
        ev = expected.get(m, 0)
        gv = got.get(m, 0)
        if m in dp.MONEY:
            ok = abs(dp.dec(ev) - dp.dec(gv)) <= Decimal("0.001")
        else:
            ok = int(ev) == int(gv)
        rep.check(ok, f"total {m}", f"model {fmt_val(m, ev)} vs CH {fmt_val(m, gv)}")
        mark = "yes" if ok else "NO"
        print(f"  {m:<32} {fmt_val(m, ev):>14} {fmt_val(m, gv):>14} {mark:>4}")


def sql_driver_ids(drivers: list[str]) -> str:
    return ",".join("'" + d.replace("\\", "\\\\").replace("'", "\\'") + "'" for d in drivers)


def query_activity_driver_rows(url: str, drivers: list[str]) -> dict:
    ids = sql_driver_ids(drivers)
    rows = ts.ch(url, f"""
        SELECT toUnixTimestamp(bucket) * 1000 AS b, merchant_operating_city_id AS c, driver_id AS d,
               {", ".join(f"sum({m}) AS {m}" for m in ACTIVITY)}
        FROM atlas_kafka.driver_analytics_5m WHERE driver_id IN ({ids}) GROUP BY b, c, d
        HAVING online_ms > 0 OR pings > 0 OR pings_unknown_state > 0""")
    return {(int(x["b"]), x["c"], x["d"]): {m: int(x[m]) for m in ACTIVITY} for x in rows}


def query_activity_state_rows(url: str, drivers: list[str]) -> dict:
    ids = sql_driver_ids(drivers)
    rows = ts.ch(url, f"""
        SELECT toUnixTimestamp(bucket) * 1000 AS b, merchant_operating_city_id AS c, driver_id AS d,
               state_known, {", ".join(ts.CHECKED_STATE)},
               {", ".join(f"sum({m}) AS {m}" for m in ACTIVITY)}, max(state_at) AS state_at
        FROM atlas_kafka.driver_analytics_5m WHERE driver_id IN ({ids})
        GROUP BY b, c, d, state_known, {", ".join(ts.CHECKED_STATE)}
        HAVING {" OR ".join(f"{m} > 0" for m in ACTIVITY)}""")

    def got_key(x):
        vals = tuple((c, tuple(x[c]) if isinstance(x[c], list) else x[c]) for c in ts.CHECKED_STATE)
        if int(x["state_known"]) == 0:
            return None if all(v in (None, ()) for _, v in vals) else ("state_known=0 with values",) + vals
        return vals

    return {(int(x["b"]), x["c"], x["d"], got_key(x)): {**{m: int(x[m]) for m in ACTIVITY},
                                                         "state_at": int(x["state_at"])}
            for x in rows}


def compare_state_rows(label: str, expected: dict, got: dict, measures: list[str], rep: dp.Report, limit: int = 15):
    bad = sorted({k for k in set(expected) | set(got) if expected.get(k) != got.get(k)}, key=repr)
    rep.check(not bad, f"{label}: every state row matches for {measures}",
              "\n        ".join(
                  f"{k}  model { {m: expected.get(k, {}).get(m) for m in measures} }  "
                  f"got { {m: got.get(k, {}).get(m) for m in measures} }"
                  for k in bad[:limit]))
    print(f"  {label}: {len(expected)} model rows, {len(got)} CH rows, {len(bad)} mismatches")


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--bootstrap", default="127.0.0.1:29092")
    ap.add_argument("--clickhouse", default="http://localhost:8123")
    ap.add_argument("--redis-host", default="localhost")
    ap.add_argument("--redis-port", type=int, default=6379)
    ap.add_argument("--redis-db", type=int, default=2)
    ap.add_argument("--flush-timeout", type=int, default=660)
    ap.add_argument("--grace", type=int, default=30)
    ap.add_argument("--assign-timeout", type=int, default=330)
    ap.add_argument("--fast", action="store_true", help="skip perf flush waits between phases")
    args = ap.parse_args()

    run_id = uuid.uuid4().hex[:8]
    r = redis.Redis(host=args.redis_host, port=args.redis_port, db=args.redis_db, decode_responses=True)
    rep = dp.Report()

    print(f"metrics audit run {run_id}\n")

    # ── Sessionizer leg ──
    t0 = int(time.time() * 1000) + 3000
    act_plan, _, sentinel_ms, act_ledger = build_activity_plan(t0, run_id)
    act_drivers = [d for sc in act_plan.scenarios for d in sc.drivers]
    print(f"activity: {len(act_plan.msgs)} messages, {len(act_drivers)} drivers, city {act_plan.city}")
    play_ts(act_plan, args.bootstrap)
    if not ts.wait_for_flush(r, sentinel_ms, args.flush_timeout):
        print("FAIL  sessionizer did not flush; is kafka-sessionizer-exe running?")
        return 1
    print("  sessionizer flushed\n")

    exp_act, _, _, exp_act_rows = ts.model(act_plan)
    got_act = query_activity_driver_rows(args.clickhouse, act_drivers)
    exp_act_drivers = {k: v for k, v in exp_act.items() if k[2] in act_drivers}
    compare_totals("activity (driver totals)", sum_measures(exp_act_drivers, exp_act_drivers.keys(), ACTIVITY),
                   sum_measures(got_act, got_act.keys(), ACTIVITY), ACTIVITY, rep)

    exp_act_state = {k: v for k, v in exp_act_rows.items() if k[2] in act_drivers}
    got_act_state = query_activity_state_rows(args.clickhouse, act_drivers)
    compare_state_rows("activity state rows", exp_act_state, got_act_state, ACTIVITY, rep)

    # ── Driver-performance leg ──
    print("\npreflight (driver-performance topics)")
    if not dp.preflight(args):
        return 1
    base = int(time.time()) - 50 * 60
    perf_plan, perf_ledger = build_perf_plan(base, run_id)
    perf_drivers = [d for sc in perf_plan.scenarios for d in sc.drivers]
    print(f"performance: {len(perf_plan.msgs)} messages, {len(perf_drivers)} drivers, city {perf_plan.city}")
    try:
        play_dp(perf_plan, args.bootstrap, args.fast, r, args.grace, args.flush_timeout)
    except RuntimeError as e:
        print(f"FAIL  {e}")
        return 1
    print("  driver-performance flushed\n")

    perf_ids = sql_driver_ids(perf_drivers)
    perf_where = f"driver_id IN ({perf_ids}) OR merchant_operating_city_id = '{perf_plan.city}'"
    exp_perf = dp.norm_rows(dp.model(perf_plan))
    got_perf = dp.query_rows(args.clickhouse, perf_where)
    compare_totals("performance (driver totals)", sum_measures(exp_perf, exp_perf.keys(), PERF),
                   sum_measures(got_perf, got_perf.keys(), PERF), PERF, rep)

    exp_perf_state, _ = dp.state_model(perf_plan)
    got_perf_state, _ = dp.query_state_rows(args.clickhouse, perf_where)
    compare_state_rows("performance state rows", exp_perf_state, got_perf_state, PERF, rep)

    # ── Input ledger ──
    print("\n── input ledger (what we sent) ──")
    print(f"  sessionizer: {act_ledger.pings} pings, {act_ledger.state_events} state events")
    print(f"  performance: {act_ledger.srd} SRD, {act_ledger.rides} ride versions, "
          f"{act_ledger.bookings} bookings, {act_ledger.bcrs} BCR, "
          f"{act_ledger.fare_params} fare_params, {act_ledger.ratings} ratings")
    topics = defaultdict(int)
    for led in (act_ledger, perf_ledger):
        for topic, n in led.by_topic.items():
            topics[topic] += n
    for topic, n in sorted(topics.items()):
        print(f"    {topic}: {n}")

    # Sanity: every performance column should be non-zero in the model for this run
    perf_tot = sum_measures(exp_perf, exp_perf.keys(), PERF)
    for m in PERF:
        rep.check(dp.dec(perf_tot.get(m, 0)) != 0,
                  f"audit plan exercises column {m}",
                  f"model total is {perf_tot.get(m, 0)}")
    act_tot = sum_measures(exp_act_drivers, exp_act_drivers.keys(), ACTIVITY)
    for m in ACTIVITY:
        rep.check(int(act_tot.get(m, 0)) > 0, f"audit plan exercises activity column {m}",
                  f"model total is {act_tot.get(m, 0)}")

    print(f"\n{'ALL PASS' if not rep.failures else f'{rep.failures} FAILED'}  (metrics audit {run_id})")
    return 1 if rep.failures else 0


if __name__ == "__main__":
    sys.exit(main())
