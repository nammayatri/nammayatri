# /// script
# requires-python = ">=3.11,<3.14"
# dependencies = ["confluent-kafka>=2.3", "redis>=5"]
# ///
"""Scenario tests for the DRIVER_POOL_STATE writer and the SESSIONIZER.

Plays scripted timelines straight onto the three input topics, in real time:

  * driver-pool-state                    driver state changes  -> state writer
  * location-updates                     LTS pings             -> sessionizer
  * driver-analytics-events              driver app events     -> sessionizer

Every scenario uses fresh driver ids, so runs never collide. After the last
message the script waits for the sessionizer's next flush, then checks
atlas_kafka.driver_analytics_5m:

  1. activity  per scenario and 5-min bucket: online (a non-OFFLINE ping in
               the bucket), online on a ride, pings, unknown-state pings and
               event counts, against hand-written totals AND against a
               reference model of the rules, per bucket and city (summed over
               the driver's states)
  2. state     the row grain is (bucket, city, driver, state): each ping and
               each event sits in the row of the state in force at its own
               time, and state_at is the history entry time; checked per state
               row against the model (state_known, mode, active, onRide,
               enabled, blocked, subscribed, selectedServiceTiers,
               vehicleRating, registrationNo) plus explicit splits
  3. writer    current-state hash, history entries, TTLs, history trimming,
               registrationNoChangedAt
  4. inputs    malformed messages are skipped without stalling either consumer

With --e2e it instead checks the newest driver from a real ride run
(`./run-tests.sh auto NY_Bangalore 01-AutoRideFlow`): state history
online -> on ride -> off ride, no unknown-state rows, and the ride's events.

Usage (from Backend/dev/integration-tests, with the stack running):
  uv run sessionizer/test_sessionizer.py            # ~12 min: ~6 min timeline + flush wait
  uv run sessionizer/test_sessionizer.py --e2e      # after a ride run

Exits 1 if any check fails.
"""

import argparse
import json
import sys
import time
import urllib.parse
import urllib.request
import uuid
from collections import defaultdict
from dataclasses import dataclass, field
from datetime import UTC, datetime

import redis
from confluent_kafka import Producer

STATE_TOPIC = "driver-pool-state"
LOCATION_TOPIC = "location-updates"
EVENTS_TOPIC = "driver-analytics-events"

# Mirrors Processor.Sessionizer.Processor / Processor.DriverPoolState.Types.
BUCKET_MS = 5 * 60 * 1000
MIN_PLAUSIBLE_MS = 1577836800000          # 2020-01-01: older pings are skipped
MAX_FUTURE_SKEW_MS = 5 * 60 * 1000        # pings further ahead of the wall clock are skipped
STATE_EXPIRY_S = 30 * 24 * 60 * 60
HISTORY_CAP = 100                         # Processor.DriverPoolState.Processor.historyCap

# Event columns checked (Processor.Sessionizer.Processor.eventColumns); gross_earnings is Decimal.
EVENT_COLUMNS = ["popups_sent", "popups_accepted", "popups_pulled", "total_bookings", "finished_rides",
                 "rides_with_rating", "rating_sum", "gross_earnings"]
# Per (bucket, city, driver): online / on_ride are 0 or 1 per bucket; the rest are sums.
MEASURES = ["online", "on_ride", "pings", "pings_unknown_state"] + EVENT_COLUMNS
# Per state row.
ROW_MEASURES = ["online", "pings"] + EVENT_COLUMNS
SHORT = {"online": "online", "on_ride": "on_ride", "pings": "pings", "pings_unknown_state": "pings_unk",
         **{c: c for c in EVENT_COLUMNS}}

IDLE = {"mode": "ONLINE", "active": True, "enabled": True, "blocked": False,
        "onRide": False, "subscribed": True, "selectedServiceTiers": ["AUTO_RICKSHAW"]}

# Mirrors Processor.StateColumns: the driver_analytics_5m state columns (DriverPoolData keys).
STATE_COLUMNS = """
    active onRide enabled blocked subscribed canSwitchToRental canSwitchToInterCity canSwitchToIntraCity
    forwardBatchingEnabled isSpecialLocWarrior isPetModeEnabled chargesEnabled hasAdvanceBooking
    isDisabledReasonFlag hasRideStarted airConditioned mode onRideTripCategory goHomeStatus variant
    rideBillingModel enableForAirport acUsageRestrictionType fleetOwnerId cloudType registrationNo latestScheduledPickup
    selectedServiceTiers selectedAutoAcceptTiers softBlockStiers vehicleTags driverTag tripDistanceMinThreshold
    tripDistanceMaxThreshold maxPickupRadius lastUpdatedAt registrationNoChangedAt airConditionScore vehicleRating latestScheduledBooking
    tollRouteBlockedTill""".split()
# The state columns checked in ClickHouse (grouped by), with their kind.
CHECKED_STATE = {"mode": "text", "active": "bool", "onRide": "bool", "enabled": "bool", "blocked": "bool",
                 "subscribed": "bool", "selectedServiceTiers": "list", "vehicleRating": "float",
                 "registrationNo": "text"}


def iso(ms: int) -> str:
    return datetime.fromtimestamp(ms / 1000, UTC).isoformat(timespec="milliseconds").replace("+00:00", "Z")


def floor_bucket(ms: int) -> int:
    return ms - ms % BUCKET_MS


# ─── timeline ────────────────────────────────────────────────────────────────

@dataclass
class Msg:
    send_s: float          # wall-clock offset from T0 when it is produced
    topic: str
    key: str | None
    value: bytes


@dataclass
class Scenario:
    name: str
    what: str
    expect: dict[str, int] = field(default_factory=dict)  # hand-written totals over the scenario's drivers
    drivers: list[str] = field(default_factory=list)


class Plan:
    def __init__(self, t0_ms: int, run_id: str):
        self.t0 = t0_ms
        self.run_id = run_id
        self.msgs: list[Msg] = []
        self.scenarios: list[Scenario] = []
        self.merchant = "sessionizer-test-merchant"
        self.city = f"sessionizer-test-{run_id}"

    def ms(self, s: float) -> int:
        return self.t0 + round(s * 1000)

    def scenario(self, name, what, **expect) -> Scenario:
        sc = Scenario(name, what, expect)
        self.scenarios.append(sc)
        return sc

    def driver(self, sc: Scenario) -> str:
        d = f"st-{self.run_id}-{len(self.scenarios):02d}-{len(sc.drivers)}-{uuid.uuid4().hex[:6]}"
        sc.drivers.append(d)
        return d

    def state(self, s, d, ts_ms=None, **changes):
        ev = {"driverId": d, "ts": self.ms(s) if ts_ms is None else ts_ms, "changes": changes}
        self.msgs.append(Msg(s, STATE_TOPIC, d, json.dumps(ev).encode()))

    def ping(self, s, d, mode="ONLINE", city=None, send_s=None, key=True):
        st = self.ms(s)
        lu = {"r_id": None, "ts": iso(st), "st": iso(st), "mocid": city or self.city,
              "pt": {"lat": 12.9352, "lon": 77.6245}, "acc": 5.0, "ride_status": "IDLE",
              "speed": None, "da": True, "mode": mode, "m_id": self.merchant}
        self.msgs.append(Msg(s if send_s is None else send_s, LOCATION_TOPIC, d if key else None,
                             json.dumps(lu).encode()))

    def ping_at(self, send_s, d, st_ms=None, ts_ms=None, with_st=True, mode="ONLINE", city=None):
        """A ping with explicit timestamps (st defaults to the send time, ts to st)."""
        st = self.ms(send_s) if st_ms is None else st_ms
        lu = {"r_id": None, "ts": iso(st if ts_ms is None else ts_ms), "mocid": city or self.city,
              "pt": {"lat": 12.9352, "lon": 77.6245}, "acc": 5.0, "ride_status": "IDLE",
              "speed": None, "da": True, "mode": mode, "m_id": self.merchant}
        if with_st:
            lu["st"] = iso(st)
        self.msgs.append(Msg(send_s, LOCATION_TOPIC, d, json.dumps(lu).encode()))

    def event(self, s, d, counts: dict, city=None, ts_ms=None):
        """A driver-analytics-events record (SharedLogic.DriverPool.DriverAnalyticsEvent)."""
        ev = {"driverId": d, "ts": self.ms(s) if ts_ms is None else ts_ms,
              "merchantOperatingCityId": city or self.city, "counts": counts}
        self.msgs.append(Msg(s, EVENTS_TOPIC, d, json.dumps(ev).encode()))

    def raw(self, s, topic, key, value: bytes):
        self.msgs.append(Msg(s, topic, key, value))


def build_plan(t0_ms: int, run_id: str) -> tuple[Plan, float, int]:
    """All scenarios on one timeline. Returns the plan, the offset (s) of the
    5-minute bucket edge the boundary scenarios straddle, and the sentinel's st."""
    p = Plan(t0_ms, run_id)
    edge_ms = floor_bucket(t0_ms + 30_000) + BUCKET_MS   # first edge at least 30 s in
    b = (edge_ms - t0_ms) / 1000

    sc = p.scenario("idle_steady", "4 ONLINE pings 20 s apart, driver idle", pings=4)
    d = p.driver(sc)
    p.state(0, d, **IDLE)
    for s in (5, 25, 45, 65):
        p.ping(s, d)
    # Malformed input, aimed at this driver: none of it may count.
    p.ping(30, d, key=False)                                   # ping without a Kafka key
    p.raw(31, LOCATION_TOPIC, d, b"{not json")                 # undecodable ping
    p.raw(32, STATE_TOPIC, d, b"{not json")                    # undecodable state event
    p.raw(33, EVENTS_TOPIC, d, b"{not json")                   # undecodable event

    sc = p.scenario("two_buckets", "two pings 300 s apart: online in two buckets", online=2, pings=2)
    d = p.driver(sc)
    p.state(0, d, **IDLE)
    p.ping(5, d)
    p.ping(305, d)

    sc = p.scenario("bucket_edge", "pings 10 s either side of a 5-min edge: online in both buckets", online=2, pings=2)
    d = p.driver(sc)
    p.state(b - 20, d, **IDLE)
    p.ping(b - 10, d)
    p.ping(b + 10, d)

    sc = p.scenario("offline_ping", "ONLINE, OFFLINE, ONLINE, ONLINE: the OFFLINE ping counts nowhere", pings=3)
    d = p.driver(sc)
    p.state(0, d, **IDLE)
    p.ping(5, d)
    p.ping(25, d, mode="OFFLINE")
    p.ping(45, d)
    p.ping(65, d)

    sc = p.scenario("only_offline", "only OFFLINE pings: no row at all", pings=0)
    d = p.driver(sc)
    p.state(0, d, **IDLE)
    p.ping(5, d, mode="OFFLINE")
    p.ping(25, d, mode="OFFLINE")

    sc = p.scenario("silent_mode", "SILENT pings count as online", pings=2)
    d = p.driver(sc)
    p.state(0, d, **{**IDLE, "mode": "SILENT"})
    p.ping(5, d, mode="SILENT")
    p.ping(25, d, mode="SILENT")

    sc = p.scenario("on_ride", "driver on a ride", pings=3)
    d = p.driver(sc)
    p.state(0, d, **{**IDLE, "onRide": True})
    for s in (5, 25, 45):
        p.ping(s, d)

    sc = p.scenario("disabled", "disabled driver pinging ONLINE: online, in an enabled=false row", pings=2)
    d = p.driver(sc)
    p.state(0, d, **{**IDLE, "enabled": False})
    p.ping(5, d)
    p.ping(25, d)

    sc = p.scenario("blocked", "blocked driver pinging ONLINE: online, in a blocked=true row", pings=2)
    d = p.driver(sc)
    p.state(0, d, **{**IDLE, "blocked": True})
    p.ping(5, d)
    p.ping(25, d)

    sc = p.scenario("unknown_state", "pings but no state ever published: online, in the state_known = 0 row",
                    pings=2, pings_unknown_state=2)
    d = p.driver(sc)
    p.ping(5, d)
    p.ping(25, d)

    sc = p.scenario("state_change", "assigned a ride between two pings: an idle row and an onRide row", pings=2)
    d = p.driver(sc)
    p.state(0, d, **IDLE)
    p.ping(5, d)
    p.state(60, d, onRide=True)
    p.ping(120, d)

    sc = p.scenario("many_changes", "ride, ride end, block between two pings: only the states at the pings get rows",
                    pings=2)
    d = p.driver(sc)
    p.state(0, d, **IDLE)
    p.ping(5, d)
    p.state(40, d, onRide=True)
    p.state(80, d, onRide=False)
    p.state(120, d, blocked=True)
    p.ping(180, d)

    sc = p.scenario("out_of_order_ping", "a late ping older than the last one is counted", pings=4)
    d = p.driver(sc)
    p.state(0, d, **IDLE)
    p.ping(5, d)
    p.ping(45, d)
    p.ping(25, d, send_s=50)
    p.ping(65, d)

    sc = p.scenario("duplicate_ping", "the same ping delivered twice: counted twice as a ping", pings=4)
    d = p.driver(sc)
    p.state(0, d, **IDLE)
    p.ping(5, d)
    p.ping(25, d)
    p.ping(25, d, send_s=26)
    p.ping(45, d)

    sc = p.scenario("city_change", "driver moves city: each ping in its own city's row", pings=3)
    d = p.driver(sc)
    p.state(0, d, **IDLE)
    p.ping(5, d, city=p.city + "-A")
    p.ping(25, d, city=p.city + "-B")
    p.ping(45, d, city=p.city + "-B")

    sc = p.scenario("ping_mode_decides_online", "state says OFFLINE/inactive but pings say ONLINE: online", pings=2)
    d = p.driver(sc)
    p.state(0, d, **{**IDLE, "mode": "OFFLINE", "active": False})
    p.ping(5, d)
    p.ping(25, d)

    sc = p.scenario("state_columns_stamped", "a change to a state column outside the tagged seven (vehicleRating) "
                    "adds a history entry and a row", pings=2)
    d = p.driver(sc)
    p.state(0, d, **IDLE, vehicleRating=4.5, driverTag=["SafetyCohort#New"])
    p.ping(5, d)
    p.state(15, d, vehicleRating=4.8)
    p.ping(25, d)

    sc = p.scenario("vehicle_change", "vehicle KA01 then KA02: a row each, registrationNoChangedAt = the change time",
                    pings=2)
    d = p.driver(sc)
    p.state(0, d, **IDLE, registrationNo="KA01AB1234")
    p.ping(5, d)
    p.state(15, d, registrationNo="KA02CD5678")
    p.ping(25, d)

    # ── events ──
    sc = p.scenario("events_stamped", "events count at their own time, in the row of the state then",
                    pings=2, popups_sent=1, popups_accepted=1, total_bookings=1, finished_rides=1,
                    gross_earnings=120.25)
    d = p.driver(sc)
    p.state(0, d, **IDLE)
    p.ping(5, d)
    p.event(10, d, {"popups_sent": 1})
    p.event(12, d, {"popups_accepted": 1, "response_secs_sum": 2})
    p.state(20, d, onRide=True)
    p.event(25, d, {"total_bookings": 1, "driver_addition": 10.5})
    p.event(40, d, {"finished_rides": 1, "gross_earnings": 120.25})
    p.ping(45, d)

    sc = p.scenario("event_without_ping", "an event while not pinging: a row with online = 0",
                    rides_with_rating=1, rating_sum=5)
    d = p.driver(sc)
    p.state(0, d, **IDLE)
    p.event(10, d, {"rides_with_rating": 1, "rating_sum": 5})

    sc = p.scenario("event_unknown_state", "an event with no state ever published: the state_known = 0 row",
                    popups_sent=1, popups_pulled=1)
    d = p.driver(sc)
    p.event(10, d, {"popups_sent": 1})
    p.event(20, d, {"popups_pulled": 1})

    sc = p.scenario("event_unknown_column", "a column the table doesn't have is ignored", popups_sent=2)
    d = p.driver(sc)
    p.state(0, d, **IDLE)
    p.event(10, d, {"popups_sent": 1, "no_such_column": 7})
    p.event(11, d, {"popups_sent": 1})

    sc = p.scenario("event_rating_changed", "a changed rating adds only its difference", rides_with_rating=1,
                    rating_sum=4)
    d = p.driver(sc)
    p.state(0, d, **IDLE)
    p.event(10, d, {"rides_with_rating": 1, "rating_sum": 5})
    p.event(20, d, {"rating_sum": -1})

    sc = p.scenario("event_old_state", "an event an hour old takes the state in force then",
                    finished_rides=1)
    d = p.driver(sc)
    p.state(0, d, ts_ms=p.t0 - 3_600_000, **{**IDLE, "onRide": True})
    p.state(1, d, onRide=False)
    p.event(10, d, {"finished_rides": 1}, ts_ms=p.t0 - 3_000_000)

    # ── extreme / edge cases ──
    sc = p.scenario("edge_ping_on_bucket_edge", "a ping exactly on a 5-min edge belongs to the later bucket",
                    online=2, pings=3)
    d = p.driver(sc)
    p.state(b - 30, d, **IDLE)
    for s_ in (b - 20, b, b + 20):
        p.ping(s_, d)

    sc = p.scenario("edge_st_missing_uses_ts", "a ping without st falls back to ts", pings=3)
    d = p.driver(sc)
    p.state(0, d, **IDLE)
    p.ping(5, d)
    p.ping_at(25, d, with_st=False)
    p.ping(45, d)

    sc = p.scenario("edge_future_ping_skipped", "a ping stamped a year ahead is skipped and doesn't block later pings",
                    pings=3)
    d = p.driver(sc)
    p.state(0, d, **IDLE)
    p.ping(5, d)
    p.ping_at(15, d, st_ms=p.ms(15) + 365 * 86400_000)
    p.ping(25, d)
    p.ping(45, d)

    sc = p.scenario("edge_future_ts_without_st", "no st and a device ts a year ahead: skipped too", pings=3)
    d = p.driver(sc)
    p.state(0, d, **IDLE)
    p.ping(5, d)
    p.ping_at(15, d, ts_ms=p.ms(15) + 365 * 86400_000, with_st=False)
    p.ping(25, d)
    p.ping(45, d)

    sc = p.scenario("edge_ancient_pings_skipped", "pings stamped 1970 and 2019 are skipped", pings=2)
    d = p.driver(sc)
    p.state(0, d, **IDLE)
    p.ping(5, d)
    p.ping_at(10, d, st_ms=1000)
    p.ping_at(12, d, st_ms=1577836799000)
    p.ping(25, d)

    sc = p.scenario("edge_first_state_after_ping", "the driver's first state arrives between pings: unknown, then idle",
                    pings=2, pings_unknown_state=1)
    d = p.driver(sc)
    p.ping(5, d)
    p.state(15, d, **IDLE)
    p.ping(25, d)

    sc = p.scenario("edge_change_at_ping_time", "a change stamped exactly at a ping applies to that ping", pings=3)
    d = p.driver(sc)
    p.state(0, d, **IDLE)
    p.ping(5, d)
    p.state(25, d, onRide=True)
    p.ping(25, d)
    p.ping(45, d)

    sc = p.scenario("edge_fifty_changes", "50 ride on/off changes 2 s apart between two pings", pings=2)
    d = p.driver(sc)
    p.state(0, d, **IDLE)
    p.ping(5, d)
    for k in range(50):
        p.state(7 + 2 * k, d, onRide=(k % 2 == 0))
    p.ping(115, d)

    sc = p.scenario("edge_null_mode", "a ping with mode null counts as online (only OFFLINE does not)", pings=3)
    d = p.driver(sc)
    p.state(0, d, **IDLE)
    p.ping_at(5, d, mode=None)
    p.ping(25, d)
    p.ping(45, d)

    sc = p.scenario("edge_rapid_toggles", "ONLINE/OFFLINE every second", pings=3)
    d = p.driver(sc)
    p.state(0, d, **IDLE)
    for s_, mode in ((5, "ONLINE"), (6, "OFFLINE"), (7, "ONLINE"), (8, "ONLINE"), (9, "OFFLINE")):
        p.ping(s_, d, mode=mode)

    sc = p.scenario("edge_burst_200_pings", "200 pings one second apart", pings=200)
    d = p.driver(sc)
    p.state(0, d, **IDLE)
    for s_ in range(5, 205):
        p.ping(s_, d)

    sc = p.scenario("edge_weird_ids", "driver id and city with quotes, backslashes, unicode and 300 chars", pings=2)
    d = f"st-{run_id}-weird-'\"\\-ಕನ್ನಡ-日本-" + "x" * 300
    sc.drivers.append(d)
    city = p.city + "-O'Brien\"\\-ಬೆಂಗಳೂರು"
    p.state(0, d, **IDLE)
    p.ping(5, d, city=city)
    p.ping(25, d, city=city)

    # Writer-only scenarios (no pings).
    sc = p.scenario("non_state_field_change", "a change to a field the sessionizer ignores adds no history entry")
    d = p.driver(sc)
    p.state(0, d, **IDLE)
    p.state(10, d, lat=12.97, lon=77.59, lastUpdatedAt="2026-01-01T00:00:00Z")

    sc = p.scenario("same_state_again", "re-publishing identical state adds no history entry")
    d = p.driver(sc)
    p.state(0, d, **IDLE)
    p.state(10, d, **IDLE)
    p.state(20, d, mode="ONLINE")

    # Topped up past HISTORY_CAP after the run, so the oldest entries are dropped.
    sc = p.scenario("trim_target", "history keeps only the newest HISTORY_CAP entries")
    d = p.driver(sc)
    p.state(0, d, **IDLE)
    p.state(5, d, onRide=True)
    p.state(10, d, onRide=False)

    # Sentinel: the last message; its flush means every scenario has been flushed.
    sc = p.scenario("sentinel", "end marker")
    d = p.driver(sc)
    last = max(m.send_s for m in p.msgs)
    p.ping(last + 10, d)
    return p, b, p.ms(last + 10)


# ─── reference model ─────────────────────────────────────────────────────────

def column_state(cur: dict) -> dict:
    """The snapshot's `state`: the current hash's values for the state columns (those present)."""
    return {k: cur[k] for k in STATE_COLUMNS if k in cur}


def same_entry(a: tuple, b: tuple) -> bool:
    """History entries (ts, state) are equal ignoring lastUpdatedAt, as the writer compares them."""
    def strip(st):
        return {k: v for k, v in st.items() if k != "lastUpdatedAt"}
    return strip(a[1]) == strip(b[1])


def render(kind: str, v):
    """A state value as the writer renders it into its column (lenient: anything else is null / [])."""
    if kind == "bool":
        return v if isinstance(v, bool) else {"True": True, "true": True, "False": False, "false": False}.get(v) \
            if isinstance(v, str) else None
    if kind == "text":
        return None if v is None else v if isinstance(v, str) else json.dumps(v, separators=(",", ":"))
    if kind == "list":
        return [x if isinstance(x, str) else json.dumps(x, separators=(",", ":")) for x in v] if isinstance(v, list) else []
    if kind == "float":
        return float(v) if isinstance(v, (int, float)) and not isinstance(v, bool) else None
    raise ValueError(kind)


def state_key(state: dict | None) -> tuple | None:
    """A row's state as its checked columns render it (lists as tuples); None
    when the writer had no state (state_known = 0)."""
    if state is None:
        return None
    return tuple((c, tuple(v) if isinstance(v, list) else v)
                 for c, k in CHECKED_STATE.items() for v in [render(k, state.get(c))])


def show_state(key: tuple | None) -> str:
    if key is None:
        return "unknown"
    if not isinstance(key[0], tuple):                    # ("state_known=0 with values", ...)
        return repr(key)
    return " ".join(f"{c}={list(v) if isinstance(v, tuple) else v}" for c, v in key if v not in (None, ()))


def on_ride(entry) -> bool:
    return entry is not None and entry[1].get("onRide") is True


def vehicle_of(v):
    """registrationNo as the writer compares it: "" and null both mean no vehicle."""
    return None if v == "" else v


def model(plan: Plan):
    """Expected rows, from the rules as specified, independent of the Haskell code.
    Returns per (bucket, city, driver) activity (over the driver's states), the
    writer's history and current state, and the state rows: (bucket, city,
    driver, state_key) -> measures + state_at (max history entry time credited
    there) + on_ride."""
    history: dict[str, list[tuple[int, dict]]] = defaultdict(list)
    current: dict[str, dict] = defaultdict(dict)
    rows = defaultdict(lambda: {**dict.fromkeys(ROW_MEASURES, 0), "state_at": 0, "on_ride": 0})

    def row(bucket, city, d, entry):
        """entry: the history entry (ts, state) in force, None when unknown."""
        r = rows[(bucket, city, d, state_key(entry[1]) if entry else None)]
        if entry:
            r["state_at"] = max(r["state_at"], entry[0])
        return r

    def in_force(d, at):
        """The newest history entry at or before `at` (None: no state yet)."""
        h = [e for e in history[d] if e[0] <= at]
        return h[-1] if h else None

    def plausible(t, m):
        return MIN_PLAUSIBLE_MS <= t <= plan.ms(m.send_s) + MAX_FUTURE_SKEW_MS

    for m in sorted(plan.msgs, key=lambda m: m.send_s):
        try:
            v = json.loads(m.value)
        except json.JSONDecodeError:
            continue
        if m.topic == STATE_TOPIC:
            cur = current[v["driverId"]]
            changes = dict(v["changes"])
            if "registrationNo" in cur and "registrationNo" in changes \
                    and vehicle_of(cur["registrationNo"]) != vehicle_of(changes["registrationNo"]):
                changes["registrationNoChangedAt"] = v["ts"]
            cur.update(changes)
            entry = (v["ts"], column_state(cur))
            h = history[v["driverId"]]
            if not h or not same_entry(h[-1], entry):
                h.append(entry)
        elif m.topic == EVENTS_TOPIC:
            d, t = v["driverId"], v["ts"]
            if not plausible(t, m):
                continue
            r = row(floor_bucket(t), v["merchantOperatingCityId"], d, in_force(d, t))
            for c in EVENT_COLUMNS:
                r[c] += v["counts"].get(c, 0)
        elif m.key is not None:
            d = m.key
            st = int(datetime.fromisoformat((v.get("st") or v["ts"]).replace("Z", "+00:00")).timestamp() * 1000)
            if not plausible(st, m) or v["mode"] == "OFFLINE":
                continue                                  # skipped by the sessionizer
            entry = in_force(d, st)
            r = row(floor_bucket(st), v["mocid"], d, entry)
            r["pings"] += 1
            r["online"] = 1
            if on_ride(entry):
                r["on_ride"] = 1
    activity = defaultdict(lambda: dict.fromkeys(MEASURES, 0))
    for (b, c, d, sk), r in rows.items():
        a = activity[(b, c, d)]
        a["online"] = max(a["online"], r["online"])
        a["on_ride"] = max(a["on_ride"], r["on_ride"])
        a["pings"] += r["pings"]
        a["pings_unknown_state"] += r["pings"] if sk is None else 0
        for col in EVENT_COLUMNS:
            a[col] += r[col]
    return activity, history, current, {k: {m: r[m] for m in ROW_MEASURES + ["state_at"]} for k, r in rows.items()}


def per_state(rows: dict, d: str) -> dict:
    """A driver's state rows summed over buckets and cities: state_key -> measures + max state_at."""
    out = defaultdict(lambda: {**dict.fromkeys(ROW_MEASURES, 0), "state_at": 0})
    for (_, _, dd, sk), v in rows.items():
        if dd == d:
            for m in ROW_MEASURES:
                out[sk][m] = max(out[sk][m], v[m]) if m == "online" else out[sk][m] + v[m]
            out[sk]["state_at"] = max(out[sk]["state_at"], v["state_at"])
    return dict(out)


def state_checks(rows: dict, by_name: dict, t0: int) -> list[tuple[bool, str, str]]:
    """Hand-written expectations on the state rows (from ClickHouse or from the model)."""
    out = []

    def states(name):
        return per_state(rows, by_name[name])

    def brief(ps):
        return "; ".join(f"{show_state(k)}: {fmt(v)} at={v['state_at'] - t0 if v['state_at'] else 0}"
                         for k, v in ps.items())

    ps = states("state_change")
    split = sorted((dict(k)["onRide"], v["pings"], v["state_at"] - t0) for k, v in ps.items() if k)
    out.append((None not in ps and split == [(False, 1, 0), (True, 1, 60_000)],
                "state_change: an idle row (the t+5 ping) and an onRide row (the t+120 ping, state_at t+60 s)", brief(ps)))
    ps = states("many_changes")
    split = sorted((dict(k)["onRide"], dict(k)["blocked"], v["pings"]) for k, v in ps.items() if k)
    out.append((None not in ps and split == [(False, False, 1), (False, True, 1)],
                "many_changes: rows only for the states at the pings: idle, blocked", brief(ps)))
    ps = states("unknown_state")
    out.append((list(ps) == [None], "unknown_state: every row has state_known = 0 (state columns NULL / [])", brief(ps)))
    ps = states("state_columns_stamped")
    split = sorted((dict(k)["vehicleRating"], v["pings"]) for k, v in ps.items() if k)
    out.append((None not in ps and split == [(4.5, 1), (4.8, 1)],
                "state_columns_stamped: a vehicleRating change between the pings: 4.5 row and 4.8 row", brief(ps)))
    ps = states("vehicle_change")
    split = sorted((dict(k)["registrationNo"], v["pings"]) for k, v in ps.items() if k)
    out.append((None not in ps and split == [("KA01AB1234", 1), ("KA02CD5678", 1)],
                "vehicle_change: a row per vehicle", brief(ps)))
    ps = states("idle_steady")
    out.append((len(ps) == 1 and None not in ps and dict(next(iter(ps)))["vehicleRating"] is None,
                "idle_steady: one state; a state column never published (vehicleRating) is NULL", brief(ps)))
    ps = states("edge_first_state_after_ping")
    split = sorted(((k is not None), v["pings"]) for k, v in ps.items())
    out.append((split == [(False, 1), (True, 1)],
                "edge_first_state_after_ping: unknown row (the t+5 ping), idle row (the t+25 ping)", brief(ps)))
    ps = states("events_stamped")
    split = sorted((dict(k)["onRide"], v["popups_sent"], v["popups_accepted"], v["total_bookings"], v["finished_rides"])
                   for k, v in ps.items() if k)
    out.append((None not in ps and split == [(False, 1, 1, 0, 0), (True, 0, 0, 1, 1)],
                "events_stamped: pop-ups in the idle row, booking and finished ride in the onRide row", brief(ps)))
    ps = states("event_without_ping")
    out.append((len(ps) == 1 and None not in ps and next(iter(ps.values()))["online"] == 0,
                "event_without_ping: one row, online = 0", brief(ps)))
    ps = states("event_old_state")
    split = sorted((dict(k)["onRide"], v["finished_rides"]) for k, v in ps.items() if k)
    out.append((None not in ps and split == [(True, 1)],
                "event_old_state: counted in the onRide state in force an hour ago", brief(ps)))
    return out


# ─── I/O ─────────────────────────────────────────────────────────────────────

def ch(url: str, sql: str) -> list[dict]:
    q = urllib.parse.urlencode({"query": sql + " FORMAT JSONEachRow"})
    with urllib.request.urlopen(f"{url}/?{q}", timeout=30) as r:
        return [json.loads(line) for line in r.read().decode("utf-8", errors="replace").splitlines() if line]


def play(plan: Plan, bootstrap: str):
    prod = Producer({"bootstrap.servers": bootstrap, "linger.ms": 0})
    msgs = sorted(plan.msgs, key=lambda m: m.send_s)
    end = msgs[-1].send_s
    for i, m in enumerate(msgs):
        wait = plan.t0 / 1000 + m.send_s - time.time()
        if wait > 0:
            time.sleep(wait)
        prod.produce(m.topic, key=m.key.encode() if m.key else None, value=m.value)
        prod.flush(10)
        if i % 10 == 0:
            print(f"\r  sent {i + 1}/{len(msgs)}  t+{m.send_s:5.0f}s of {end:.0f}s", end="", flush=True)
    print(f"\r  sent {len(msgs)}/{len(msgs)}  t+{end:.0f}s           ")


def wait_for_flush(r: redis.Redis, covering_ms: int, timeout_s: int):
    """The sessionizer publishes watermark = the oldest record time not flushed yet (now when idle)."""
    deadline = time.time() + timeout_s
    while time.time() < deadline:
        wm = int(r.get("sessionizer:watermark") or 0)
        if wm > covering_ms:
            return True
        print(f"\r  waiting for the sessionizer flush ... {int(deadline - time.time())}s left ", end="", flush=True)
        time.sleep(5)
    return False


def history_of(r: redis.Redis, d: str) -> list[tuple[int, dict]]:
    out = []
    for member, score in r.zrange(f"driver_state:history:{d}", 0, -1, withscores=True):
        text = json.loads(member) if member.startswith('"') else member
        out.append((int(score), json.loads(text.split(":", 1)[1])))
    return out


# ─── checks ──────────────────────────────────────────────────────────────────

class Report:
    def __init__(self):
        self.failures = 0

    def check(self, ok: bool, label: str, detail: str = ""):
        self.failures += 0 if ok else 1
        print(f"  {'PASS' if ok else 'FAIL'}  {label}" + (f"\n        {detail}" if detail and not ok else ""))


def fmt(row: dict) -> str:
    return " ".join(f"{SHORT[k]}={row.get(k, 0)}" for k in MEASURES if row.get(k, 0)) or "nothing"


def num(v) -> float | int:
    """A ClickHouse number (Decimal may come as text), money rounded to 4 places."""
    f = float(v)
    return int(f) if f == int(f) else round(f, 4)


def run_scenarios(args) -> int:
    r = redis.Redis(host=args.redis_host, port=args.redis_port, db=args.redis_db, decode_responses=True)
    run_id = uuid.uuid4().hex[:8]
    t0 = int(time.time() * 1000) + 3000
    plan, edge_s, sentinel_ms = build_plan(t0, run_id)
    exp_activity, exp_history, exp_current, exp_rows = model(plan)
    drivers = [d for sc in plan.scenarios for d in sc.drivers]

    print(f"run {run_id}: {len(plan.scenarios) - 1} scenarios, {len(plan.msgs)} messages, "
          f"bucket edge at t+{edge_s:.0f}s")
    play(plan, args.bootstrap)
    if not wait_for_flush(r, sentinel_ms, args.flush_timeout):
        print("\nFAIL  the sessionizer did not flush in time; is kafka-sessionizer-exe running?")
        return 1
    print()

    ids = ",".join("'" + d.replace("\\", "\\\\").replace("'", "\\'") + "'" for d in drivers)
    events = ", ".join(f"sum({c}) AS {c}" for c in EVENT_COLUMNS)
    rows = ch(args.clickhouse, f"""
        SELECT toUnixTimestamp(bucket) * 1000 AS b, merchant_operating_city_id AS c, driver_id AS d,
               max(t.online) AS online, maxIf(t.online, onRide) AS on_ride, sum(t.pings) AS pings,
               sumIf(t.pings, state_known = 0) AS pings_unknown_state, {events}
        FROM atlas_kafka.driver_analytics_5m AS t WHERE driver_id IN ({ids}) GROUP BY b, c, d""")
    got_activity = {(int(x["b"]), x["c"], x["d"]): {m: num(x[m]) for m in MEASURES} for x in rows}
    state_rows = ch(args.clickhouse, f"""
        SELECT toUnixTimestamp(bucket) * 1000 AS b, merchant_operating_city_id AS c, driver_id AS d,
               state_known, {", ".join(CHECKED_STATE)},
               max(online) AS online, sum(pings) AS pings, {events},
               toUnixTimestamp64Milli(max(state_at)) AS state_at
        FROM atlas_kafka.driver_analytics_5m WHERE driver_id IN ({ids})
        GROUP BY b, c, d, state_known, {", ".join(CHECKED_STATE)}""")

    def got_key(x):
        vals = tuple((c, tuple(x[c]) if isinstance(x[c], list) else x[c]) for c in CHECKED_STATE)
        if int(x["state_known"]) == 0:   # must also have every state column NULL / []
            return None if all(v in (None, ()) for _, v in vals) else ("state_known=0 with values",) + vals
        return vals
    got_rows = {(int(x["b"]), x["c"], x["d"], got_key(x)): {**{m: num(x[m]) for m in ROW_MEASURES},
                                                             "state_at": int(x["state_at"])}
                for x in state_rows}

    rep = Report()
    print("activity (driver_analytics_5m)")
    for sc in plan.scenarios:
        if not sc.expect:
            continue
        ds = set(sc.drivers)
        total = dict.fromkeys(MEASURES, 0)
        for k, v in got_activity.items():
            if k[2] in ds:
                for m in MEASURES:
                    total[m] += v[m]
        # online / on_ride: buckets; checked only where the scenario says (the bucket edge falls anywhere).
        total = {m: round(v, 4) for m, v in total.items() if m in sc.expect or m not in ("online", "on_ride")}
        want = {m: sc.expect.get(m, 0) for m in total}
        rep.check(total == want, f"{sc.name}: {sc.what}", f"want {fmt(want)}\n        got  {fmt(total)}")
        mine_exp = {k: v for k, v in exp_activity.items() if k[2] in ds}
        mine_got = {k: v for k, v in got_activity.items() if k[2] in ds}
        if mine_exp != mine_got:
            rep.check(False, f"{sc.name}: per bucket and city matches the reference model",
                      "\n        ".join(f"{iso(k[0])} {k[1][-2:]}  model: {fmt(mine_exp.get(k, {}))}  |  got: {fmt(mine_got.get(k, {}))}"
                                         for k in sorted(set(mine_exp) | set(mine_got))))
    rep.check(got_activity == {k: v for k, v in exp_activity.items() if k[2] in set(drivers)},
              "every activity row matches the reference model, bucket by bucket")

    print("state rows (driver_analytics_5m, one row per distinct state)")
    by_name = {sc.name: sc.drivers[0] for sc in plan.scenarios if sc.drivers}
    name_of = {d: sc.name for sc in plan.scenarios for d in sc.drivers}
    exp_rows = {k: v for k, v in exp_rows.items() if k[2] in set(drivers)}

    def show(k):
        def one(v):
            return f"{fmt(v)} state_at={v['state_at'] - t0 if v['state_at'] else 0}" if v else "-"
        return (f"{name_of[k[2]]} {iso(k[0])} {k[1][-2:]} [{show_state(k[3])}]"
                f"\n          model: {one(exp_rows.get(k))}\n          got:   {one(got_rows.get(k))}")
    bad = [k for k in sorted(set(exp_rows) | set(got_rows), key=repr) if exp_rows.get(k) != got_rows.get(k)]
    rep.check(not bad, "every state row matches the model: pings by the state at their st, events by the state at "
              "their time, state_at = history entry time (state_known, mode, active, onRide, enabled, blocked, "
              "subscribed, selectedServiceTiers, vehicleRating, registrationNo)", "\n        ".join(map(show, bad[:20])))
    for ok, label, detail in state_checks(got_rows, by_name, t0):
        rep.check(ok, label, f"got {detail}")

    print("state writer (Redis)")
    for name in ("idle_steady", "many_changes", "non_state_field_change", "same_state_again",
                 "state_change", "on_ride", "state_columns_stamped", "vehicle_change"):
        d = by_name[name]
        got = [(t, h.get("state")) for t, h in history_of(r, d)]
        rep.check(got == exp_history[d], f"{name}: history has {len(exp_history[d])} entries at the change times, "
                  "each with its state columns",
                  f"want {[(t - t0, st) for t, st in exp_history[d]]} (ms after t0, state)\n"
                  f"        got  {[(t - t0, st) for t, st in got]}")
    d = by_name["vehicle_change"]
    cur = {k: json.loads(v) for k, v in r.hgetall(f"driver_state:current:{d}").items()}
    rep.check(cur.get("registrationNoChangedAt") == plan.ms(15),
              "vehicle_change: the writer stamps registrationNoChangedAt with the change's ts",
              f"want {plan.ms(15)}\n        got  {cur.get('registrationNoChangedAt')}")
    changed = ch(args.clickhouse, f"""
        SELECT registrationNo, toUnixTimestamp64Milli(any(registrationNoChangedAt)) AS at
        FROM atlas_kafka.driver_analytics_5m WHERE driver_id = '{d}' AND state_known = 1
        GROUP BY registrationNo ORDER BY registrationNo""")
    got_changed = [(x["registrationNo"], int(x["at"]) if x["at"] is not None else None) for x in changed]
    rep.check(got_changed == [("KA01AB1234", None), ("KA02CD5678", plan.ms(15))],
              "vehicle_change: rows carry registrationNoChangedAt (none for the first vehicle)",
              f"got {got_changed}")
    d = by_name["many_changes"]
    cur = {k: json.loads(v) for k, v in r.hgetall(f"driver_state:current:{d}").items()}
    rep.check(all(cur.get(k) == v for k, v in exp_current[d].items()),
              "current-state hash holds every merged field",
              f"want {exp_current[d]}\n        got  {cur}")
    ttls = [r.ttl(f"driver_state:{k}:{d}") for k in ("current", "history")]
    rep.check(all(STATE_EXPIRY_S - 3600 < t <= STATE_EXPIRY_S for t in ttls),
              "current and history keys expire in 30 days", f"ttl {ttls}")
    rep.check(not r.exists(f"driver_state:history:{by_name['unknown_state']}"),
              "a driver with no state events has no history")

    # Trimming: each state change adds an entry and drops the oldest past
    # HISTORY_CAP. Alternating `blocked` makes every event a change.
    d = by_name["trim_target"]
    before = [t for t, _ in exp_history[d]]
    base = int(time.time() * 1000)
    added = [base + i for i in range(HISTORY_CAP - len(before) + 2)]
    prod = Producer({"bootstrap.servers": args.bootstrap})
    for i, ts in enumerate(added):
        prod.produce(STATE_TOPIC, key=d.encode(),
                     value=json.dumps({"driverId": d, "ts": ts, "changes": {"blocked": i % 2 == 0}}).encode())
    prod.flush(10)
    got_ts: list[int] = []
    # The writer tries a DB seed on every event of a driver not in the DB (all test drivers),
    # so it applies these at about one a second.
    for _ in range(480):
        got_ts = [t for t, _ in history_of(r, d)]
        if added[-1] in got_ts:
            break
        time.sleep(0.5)
    want_ts = (before + added)[-HISTORY_CAP:]
    rep.check(got_ts == want_ts,
              f"history capped at the newest {HISTORY_CAP} entries ({len(before)} + {len(added)} -> {HISTORY_CAP})",
              f"want {[t - t0 for t in want_ts]}\n        got  {[t - t0 for t in got_ts]}")

    print(f"\n{'ALL PASS' if not rep.failures else f'{rep.failures} FAILED'}  (run {run_id})")
    return 1 if rep.failures else 0


def run_e2e(args) -> int:
    """Checks the newest driver from a real ride run against the full pipeline."""
    r = redis.Redis(host=args.redis_host, port=args.redis_port, db=args.redis_db, decode_responses=True)
    newest = max(((int(r.zrange(k, -1, -1, withscores=True)[0][1]), k.rsplit(":", 1)[1])
                  for k in r.scan_iter("driver_state:history:*", count=1000)
                  if not k.rsplit(":", 1)[1].startswith("st-")), default=None)
    d = args.driver or (newest[1] if newest else None)
    if not d:
        print("FAIL  no driver in the writer's Redis; run a ride test first")
        return 1
    hist = history_of(r, d)
    last_ms = max(t for t, _ in hist)
    print(f"driver {d}: {len(hist)} history entries; waiting for the flush covering the ride ...")
    if not wait_for_flush(r, last_ms + 30_000, args.flush_timeout):
        print("\nFAIL  the sessionizer did not flush in time")
        return 1
    print()
    rep = Report()
    rep.check(bool(r.hget(f"driver_state:current:{d}", "_seededFromDb")), "writer seeded the driver from the DB")
    flags = [(h["state"].get("mode"), h["state"].get("onRide") is True) for _, h in hist]
    i_online = next((i for i, (m, o) in enumerate(flags) if m == "ONLINE" and not o), None)
    i_ride = next((i for i, (m, o) in enumerate(flags) if o and i_online is not None and i > i_online), None)
    i_off = next((i for i, (m, o) in enumerate(flags) if not o and i_ride is not None and i > i_ride), None)
    rep.check(None not in (i_online, i_ride, i_off), "history goes online -> on ride -> off ride",
              f"(mode, onRide) entries: {flags}")

    act = ch(args.clickhouse, f"""
        SELECT uniqExactIf(bucket, t.online = 1) AS online, uniqExactIf(bucket, t.online = 1 AND onRide) AS on_ride,
               sum(t.pings) AS pings, sumIf(t.pings, state_known = 0) AS pings_unknown_state,
               {", ".join(f"sum({c}) AS {c}" for c in EVENT_COLUMNS)},
               sum(cancelled_rides) AS cancelled, sum(rides_started) AS started
        FROM atlas_kafka.driver_analytics_5m AS t WHERE driver_id = '{d}'""")[0]
    rides_ended = int(act.pop("cancelled")) + int(act["finished_rides"])
    started = int(act.pop("started"))
    act = {m: num(act[m]) for m in MEASURES}
    rep.check(act["pings"] > 0, "the driver's pings were counted", fmt(act))
    rep.check(act["pings_unknown_state"] == 0, "no unknown-state rows: every ping found the writer's state", fmt(act))
    rep.check(act["on_ride"] <= act["online"], "online buckets on a ride <= online buckets", fmt(act))
    rep.check(act["popups_sent"] > 0 and act["total_bookings"] > 0 and started > 0 and rides_ended > 0,
              "the ride's events arrived: pop-up sent, booking, start, end or cancel",
              f"{fmt(act)} started={started} ended={rides_ended}")
    print(f"\n{'ALL PASS' if not rep.failures else f'{rep.failures} FAILED'}  (driver {d})")
    return 1 if rep.failures else 0


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--bootstrap", default="127.0.0.1:29092")
    ap.add_argument("--clickhouse", default="http://localhost:8123")
    ap.add_argument("--redis-host", default="localhost")
    ap.add_argument("--redis-port", type=int, default=6379)
    ap.add_argument("--redis-db", type=int, default=2, help="the writer's Redis DB (driverStateRedisCfg)")
    ap.add_argument("--flush-timeout", type=int, default=660, help="seconds to wait for the sessionizer flush")
    ap.add_argument("--e2e", action="store_true", help="check the newest driver from a real ride run instead")
    ap.add_argument("--driver", help="with --e2e: the driver id to check (default: newest in the writer's Redis)")
    args = ap.parse_args()
    sys.exit(run_e2e(args) if args.e2e else run_scenarios(args))


if __name__ == "__main__":
    main()
