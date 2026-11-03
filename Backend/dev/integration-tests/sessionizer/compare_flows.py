# /// script
# requires-python = ">=3.11,<3.14"
# dependencies = ["confluent-kafka>=2.3", "redis>=5", "psycopg[binary]>=3.1"]
# ///
"""Compares what the stack wrote during a run of ride flows, between two
builds, and checks the sessionizer's numbers against the source data.

Subcommands (compare_flows.sh drives them; each reads the running stack):

  prepare  [--sync-metadata data/config-sync/metadata.json]
           Makes a stack able to run ride flows. A database with no merchants
           gets the same config bundle the metadata file names (config-sync is
           otherwise manual), and atlas_registry.subscriber gets one row per
           local merchant with the key it signs with, plus the gateway.
           Exits 10 after an import: the stack must then be restarted.
  mark     --out pre.json
           Records the time and every Kafka topic's end offsets. Run it just
           before the flows.
  snapshot --pre pre.json --out side.json
           What the run wrote since `mark`: every Postgres row created since
           (atlas_driver_offer_bpp and atlas_app, ids / times / tokens masked),
           Redis key patterns (ids masked) and the driver-pool-data values,
           and per Kafka topic the messages appended and their JSON shapes.
  diff     base.json branch.json
           Table by table, pattern by pattern, topic by topic: what differs.
           Differences the branch is meant to make are listed separately.
  ride-stats --pre pre.json --ride N --flow-rc RC --out rides.jsonl
           One line per repeated ride (compare_flows.sh with RIDES=n): pop-ups,
           dispatch jobs and batches since the mark, and whether each driver's
           pool data holds the vehicle number of their vehicle row.
  ride-report base.jsonl branch.jsonl
           Both sides' rides side by side.
  truth    --pre pre.json
           On a build with the sessionizer: waits for its flush, then compares
           atlas_kafka.driver_analytics_5m per driver with the same metrics
           computed from Postgres (pop-ups, rides, cancellations, ratings,
           fare parameters) and from location-updates (online buckets, pings).

Run one suite per mark: run-tests.sh flushes Redis before each suite, which
drops writes the drainer has not reached yet, so Postgres would miss them.

Exits 1 when `diff` finds an unexpected difference or a snapshot with no ride,
and when `truth` finds a mismatch or a run with no ride.
"""

import argparse
import json
import re
import sys
import time
import urllib.parse
import urllib.request
from collections import Counter, defaultdict
from datetime import UTC, datetime
from decimal import Decimal

import redis
from confluent_kafka import Consumer, TopicPartition
from confluent_kafka.admin import AdminClient

SCHEMAS = ["atlas_driver_offer_bpp", "atlas_app"]
BUCKET_S = 300
# Topics only the branch has; their presence is the point of the change.
NEW_TOPICS = {"driver-pool-state", "driver-analytics-events", "driver-analytics-5m"}
# Redis keys only the branch has (the writer's and the sessionizer's, Redis DB 2).
NEW_KEY_PREFIXES = ("driver_state:", "state_writer:", "sessionizer:")
# `prepare` exit code: config was imported, so the stack must restart. The import's own
# restart only kills the apps still running; one that died during it stays down.
IMPORTED_EXIT = 10
PREPARE_TIMEOUT_S = 1800
# How long ClickHouse's Kafka engine gets to load a flush's rows before a mismatch counts.
CLICKHOUSE_WAIT_S = 120
# The public half of every local signing key (dev/config-sync/assets/patches.json.example).
LOCAL_SIGNING_PUBLIC_KEY = "1tRC4LHVKWeGikY5C8T1FYI9t3oacpa3IFvj8e7aGws="
# driver-pool-data fields the branch changes on purpose: the vehicle number now
# follows vehicle create / upsert.
EXPECTED_DPD_FIELDS = {"registrationNo"}

UUID = re.compile(r"[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}")
ISO_TIME = re.compile(r"\d{4}-\d{2}-\d{2}[T ]\d{2}:\d{2}:\d{2}(\.\d+)?(Z|[+-]\d{2}:?\d{2})?")
LONG_NUM = re.compile(r"\b\d{9,}\b")
VOLATILE_COLUMN = re.compile(r"(^id$|_id$|^short_id|_at$|time|date|otp|token|hash|encrypted|session|"
                             r"request_id|transaction_id|message_id|url|signature|nonce|version|_key$|lat$|lon$|"
                             r"location|distance|duration|eta|geohash|counter|seq|offset|"
                             # generated per run: expiries, the test's random vehicle and phone, codes, links
                             r"_till$|_on$|pay_by|registration_no|vehicle_number|masked_mobile|referral_code|"
                             r"payment_link|sdk_payload)", re.I)
INVOICE_NUMBER = re.compile(r"\b\d{6}-[A-Z]+-\d+\b")
RUN_TOKEN = re.compile(r"(?<![A-Za-z0-9])(?=[A-Za-z0-9+/=]*\d)(?=[A-Za-z0-9+/=]*[A-Za-z])[A-Za-z0-9+/=]{6,}(?![A-Za-z0-9])")


def mask(v):
    """A value with run-specific parts (uuids, times, epoch numbers) replaced."""
    if isinstance(v, str):
        return INVOICE_NUMBER.sub("<inv>", LONG_NUM.sub("<n>", ISO_TIME.sub("<t>", UUID.sub("<id>", v))))
    if isinstance(v, dict):
        return {k: mask(x) for k, x in sorted(v.items())}
    if isinstance(v, list):
        return [mask(x) for x in v]
    return v


def mask_row(row: dict) -> str:
    return json.dumps({k: ("<v>" if VOLATILE_COLUMN.search(k) else mask(v)) for k, v in sorted(row.items())},
                      sort_keys=True, default=str)


def key_pattern(k: str) -> str:
    return re.sub(r"\d+", "#", RUN_TOKEN.sub("*", UUID.sub("*", k)))


def shape(v) -> str:
    """A JSON message's structure: its keys, nested, without values."""
    def walk(x):
        if isinstance(x, dict):
            return {k: walk(y) for k, y in sorted(x.items())}
        if isinstance(x, list):
            return [walk(x[0])] if x else []
        return type(x).__name__
    return json.dumps(walk(v), sort_keys=True)


def ch(url: str, sql: str) -> list[dict]:
    q = urllib.parse.urlencode({"query": sql + " FORMAT JSONEachRow"})
    with urllib.request.urlopen(f"{url}/?{q}", timeout=60) as r:
        return [json.loads(line, parse_float=Decimal) for line in r.read().decode("utf-8", errors="replace").splitlines() if line]


def pg(args):
    import psycopg
    from psycopg.rows import dict_row
    conn = psycopg.connect(host=args.pg_host, port=args.pg_port, dbname=args.pg_db, user=args.pg_user,
                           connect_timeout=5, autocommit=True, row_factory=dict_row)
    conn.execute("SET TIME ZONE 'UTC'")
    return conn


# ─── Kafka ───────────────────────────────────────────────────────────────────

def end_offsets(bootstrap: str) -> dict[str, dict[int, int]]:
    admin = AdminClient({"bootstrap.servers": bootstrap, "broker.address.family": "v4"})
    md = admin.list_topics(timeout=15)
    c = Consumer({"bootstrap.servers": bootstrap, "broker.address.family": "v4", "group.id": "compare-flows-offsets", "enable.auto.commit": False})
    out = {}
    for t, tm in md.topics.items():
        if t.startswith("__"):
            continue
        out[t] = {p: c.get_watermark_offsets(TopicPartition(t, p), timeout=10)[1] for p in tm.partitions}
    c.close()
    return out


def read_range(bootstrap: str, topic: str, start: dict[int, int], end: dict[int, int]):
    """Yields (key, value) of every message of `topic` in [start, end) per partition."""
    todo = {p: (start.get(p, 0), e) for p, e in end.items() if e > start.get(p, 0)}
    if not todo:
        return
    c = Consumer({"bootstrap.servers": bootstrap, "broker.address.family": "v4", "group.id": "compare-flows-read", "enable.auto.commit": False})
    c.assign([TopicPartition(topic, p, s) for p, (s, _) in todo.items()])
    left = {p: e - s for p, (s, e) in todo.items()}
    idle = 0
    while any(n > 0 for n in left.values()) and idle < 20:
        msgs = c.consume(1000, timeout=1)
        idle = 0 if msgs else idle + 1
        for m in msgs:
            if m.error() or m.offset() >= todo[m.partition()][1]:
                continue
            left[m.partition()] -= 1
            yield (m.key().decode("utf-8", "replace") if m.key() else None), m.value()
    c.close()


def load_pre(path: str) -> dict:
    """A mark, with partition numbers back as ints (JSON keeps object keys as text)."""
    pre = json.load(open(path))
    pre["offsets"] = {t: {int(p): o for p, o in parts.items()} for t, parts in pre["offsets"].items()}
    return pre


# ─── mark / snapshot ─────────────────────────────────────────────────────────

def ensure_sessionizer(args) -> bool:
    """The branch's topics and table: creates missing topics (same partition count as
    location-updates) and checks driver_analytics_5m has the event columns."""
    from confluent_kafka.admin import NewTopic
    admin = AdminClient({"bootstrap.servers": args.bootstrap, "broker.address.family": "v4"})
    md = admin.list_topics(timeout=15)
    parts = len(md.topics["location-updates"].partitions) if "location-updates" in md.topics else 1
    missing = [t for t in ("driver-analytics-events", "driver-analytics-5m") if t not in md.topics]
    created = admin.create_topics([NewTopic(t, num_partitions=parts, replication_factor=1) for t in missing]) if missing else {}
    for t, f in created.items():
        f.result()
        print(f"  created topic {t} ({parts} partitions)")
    if "driver-analytics-events" in missing:
        print("  WARN  driver-analytics-events was just created: restart kafka-sessionizer-exe so it reads it from the start")
    cols = {x["name"] for x in ch(args.clickhouse, "SELECT name FROM system.columns WHERE database = 'atlas_kafka' "
                                                   "AND table = 'driver_analytics_5m'")}
    if "popups_pulled" not in cols:
        print("  FAIL  atlas_kafka.driver_analytics_5m has an old schema (the seed runs only on a fresh ClickHouse data dir).\n"
              "        Drop driver_performance_5m, driver_performance_hourly, driver_analytics_5m_mv, driver_analytics_5m_queue\n"
              "        and driver_analytics_5m, then apply Backend/dev/clickhouse/sql-seed/sessionizer-tables.sql.")
        return False
    return True


def http_json(url: str, body: dict | None = None) -> dict:
    data = json.dumps(body).encode() if body is not None else None
    req = urllib.request.Request(url, data=data, headers={"Content-Type": "application/json"})
    with urllib.request.urlopen(req, timeout=30) as r:
        return json.loads(r.read())


def merchant_counts(conn) -> tuple[int, int]:
    return (conn.execute("SELECT count(*) AS n FROM atlas_app.merchant").fetchone()["n"],
            conn.execute("SELECT count(*) AS n FROM atlas_driver_offer_bpp.merchant").fetchone()["n"])


def sync_config(args) -> bool:
    """The config import the dashboard's Sync Data button runs, from the same bundle."""
    try:
        meta = json.load(open(args.sync_metadata))
    except (OSError, json.JSONDecodeError):
        meta = {}
    body = {"from": meta.get("from", "prod"), "version": meta.get("version")}
    print(f"  importing config {body} (the database has no merchants)")
    http_json(f"{args.context_api}/api/config-sync/import", body)
    deadline = time.time() + PREPARE_TIMEOUT_S
    while time.time() < deadline:
        time.sleep(10)
        try:
            st = http_json(f"{args.context_api}/api/config-sync/status")
        except OSError:
            continue
        if not st.get("running"):
            if st.get("error") or st.get("exit_code"):
                print(f"  FAIL  config import: {st.get('error') or st.get('exit_code')}\n" + "\n".join(st.get("log", [])[-15:]))
                return False
            return True
    print("  FAIL  config import did not finish in time")
    return False


def seed_registry(conn) -> int:
    """One row per local merchant with the key it signs with. A row for the same
    subscriber under another key is replaced: nothing can verify against it, and
    two BPP rows would make the gateway forward each search twice."""
    wanted = [(r["key"], r["sid"], "BAP") for r in conn.execute(
        "SELECT bap_unique_key_id AS key, bap_id AS sid FROM atlas_app.merchant")]
    wanted += [(r["key"], r["sid"], "BPP") for r in conn.execute(
        "SELECT unique_key_id AS key, trim(subscriber_id) AS sid FROM atlas_driver_offer_bpp.merchant")]
    wanted.append(("juspay-bg-1-key", "JUSPAY.BG.1", "BG"))
    written = 0
    with conn.transaction():
        for key, sid, typ in wanted:
            found = {r["unique_key_id"] for r in conn.execute(
                "SELECT unique_key_id FROM atlas_registry.subscriber WHERE subscriber_id = %s AND type = %s AND domain = 'MOBILITY'",
                (sid, typ))}
            if found == {key}:
                continue
            conn.execute("DELETE FROM atlas_registry.subscriber WHERE subscriber_id = %s AND type = %s AND domain = 'MOBILITY'",
                         (sid, typ))
            url = "http://localhost:8015/v1" if typ == "BG" else f"http://{sid}"
            conn.execute("""INSERT INTO atlas_registry.subscriber
                              (unique_key_id, subscriber_id, subscriber_url, type, domain, city, country, status,
                               signing_public_key, encr_public_key, valid_from, valid_until, created, updated)
                            VALUES (%s, %s, %s, %s, 'MOBILITY', '{*}', 'India', 'SUBSCRIBED', %s, '',
                                    now() - interval '1 day', now() + interval '5 years', now(), now())""",
                         (key, sid, url, typ, LOCAL_SIGNING_PUBLIC_KEY))
            written += 1
    return written


def cmd_prepare(args) -> int:
    deadline = time.time() + PREPARE_TIMEOUT_S
    conn = None
    while conn is None:      # the schemas exist once the apps have migrated
        try:
            conn = pg(args)
            merchant_counts(conn)
        except Exception as e:
            conn = None
            if time.time() > deadline:
                print(f"  FAIL  database not ready: {e}")
                return 1
            time.sleep(5)
    imported = 0 in merchant_counts(conn)
    if imported and not sync_config(args):
        return 1
    if 0 in merchant_counts(conn):
        print(f"  FAIL  still no merchants after the config import: {merchant_counts(conn)}")
        return 1
    written = seed_registry(conn)
    n = conn.execute("SELECT count(*) AS n FROM atlas_registry.subscriber").fetchone()["n"]
    print(f"  registry: {n} subscribers ({written} written), merchants {merchant_counts(conn)}")
    return IMPORTED_EXIT if imported else 0


def cmd_mark(args) -> int:
    if args.sessionizer and not ensure_sessionizer(args):
        return 1
    pre = {"started_ms": int(time.time() * 1000), "offsets": end_offsets(args.bootstrap)}
    json.dump(pre, open(args.out, "w"))
    print(f"marked {datetime.fromtimestamp(pre['started_ms'] / 1000, UTC).isoformat()} "
          f"({len(pre['offsets'])} topics)")
    return 0


def pg_snapshot(args, since_ms: int) -> dict:
    conn = pg(args)
    since = datetime.fromtimestamp(since_ms / 1000, UTC)
    tables = conn.execute(
        "SELECT table_schema, table_name FROM information_schema.columns "
        "WHERE column_name = 'created_at' AND table_schema = ANY(%s) ORDER BY 1, 2", (SCHEMAS,)).fetchall()
    out = {}
    for t in tables:
        name = f"{t['table_schema']}.{t['table_name']}"
        try:
            rows = conn.execute(f'SELECT * FROM "{t["table_schema"]}"."{t["table_name"]}" WHERE created_at >= %s',
                                (since,)).fetchall()
        except Exception as e:  # views or odd column types
            print(f"  skip {name}: {e}", file=sys.stderr)
            continue
        if rows:
            out[name] = dict(Counter(mask_row(r) for r in rows))
    return out


def redis_snapshot(args) -> dict:
    patterns: Counter = Counter()
    dpd = {}
    for db in range(16):
        r = redis.Redis(host=args.redis_host, port=args.redis_port, db=db, decode_responses=True)
        for k in r.scan_iter(count=1000):
            patterns[f"db{db}:{key_pattern(k)}"] += 1
            if db == 0 and "driver-pool-data:" in k:   # after the app's key prefix
                try:
                    dpd[k.split("driver-pool-data:", 1)[1]] = json.loads(r.get(k) or "null")
                except (json.JSONDecodeError, redis.ResponseError):
                    pass
    try:
        rc = redis.RedisCluster(host=args.redis_host, port=args.redis_cluster_port, decode_responses=True)
        for k in rc.scan_iter(count=1000):
            patterns[f"cluster:{key_pattern(k)}"] += 1
    except Exception as e:
        print(f"  redis cluster not scanned: {e}", file=sys.stderr)
    return {"patterns": dict(patterns), "driver_pool_data": dpd}


def kafka_snapshot(args, pre: dict) -> dict:
    now = end_offsets(args.bootstrap)
    out = {}
    for t, ends in sorted(now.items()):
        start = pre["offsets"].get(t, {})
        n = sum(e - start.get(p, 0) for p, e in ends.items())
        if n <= 0:
            continue
        shapes: Counter = Counter()
        if t != "location-updates":   # high volume and unchanged; counted only
            for _, v in read_range(args.bootstrap, t, start, ends):
                try:
                    shapes[shape(json.loads(v))] += 1
                except (json.JSONDecodeError, TypeError, UnicodeDecodeError):
                    shapes["<not json>"] += 1
        out[t] = {"messages": n, "shapes": dict(shapes)}
    return out


def cmd_snapshot(args) -> int:
    pre = load_pre(args.pre)
    snap = {"pre": pre, "ended_ms": int(time.time() * 1000), "postgres": pg_snapshot(args, pre["started_ms"]),
            "redis": redis_snapshot(args), "kafka": kafka_snapshot(args, pre)}
    json.dump(snap, open(args.out, "w"))
    print(f"snapshot: {len(snap['postgres'])} tables with new rows, {len(snap['redis']['patterns'])} key patterns, "
          f"{len(snap['kafka'])} topics with messages -> {args.out}")
    return 0


# ─── diff ────────────────────────────────────────────────────────────────────

def column_values(rows: Counter, column: str) -> list:
    out: Counter = Counter()
    for row, n in rows.items():
        out[json.dumps(json.loads(row).get(column), sort_keys=True)] += n
    return list(out.items())


def differing_columns(only_a: Counter, only_b: Counter) -> list[str]:
    """Columns whose values differ between the rows only one side has. With an unequal
    row count every column of the extra rows differs: those are named by the count."""
    columns = {c for row in list(only_a) + list(only_b) for c in json.loads(row)}
    return sorted(c for c in columns if Counter(dict(column_values(only_a, c))) != Counter(dict(column_values(only_b, c))))


def cmd_diff(args) -> int:
    a, b = json.load(open(args.base)), json.load(open(args.branch))
    unexpected, expected = [], []
    for name, snap in (("base", a), ("branch", b)):
        if not snap["postgres"].get("atlas_driver_offer_bpp.ride"):
            print(f"FAIL  the {name} run created no ride, so comparing it proves nothing")
            return 1

    print("postgres (rows created during the run, masked)")
    for t in sorted(set(a["postgres"]) | set(b["postgres"])):
        ra, rb = Counter(a["postgres"].get(t, {})), Counter(b["postgres"].get(t, {}))
        if ra == rb:
            continue
        only_a, only_b = ra - rb, rb - ra
        columns = differing_columns(only_a, only_b)
        unexpected.append(f"postgres {t}: base {sum(ra.values())} rows, branch {sum(rb.values())}; columns: {', '.join(columns)}")
        for c in columns[:8]:
            va, vb = column_values(only_a, c), column_values(only_b, c)
            unexpected.append(f"    {c}: base {va[:2]} | branch {vb[:2]}"[:400])
    same = len(set(a["postgres"]) & set(b["postgres"])) - sum(1 for u in unexpected if u.startswith("postgres"))
    print(f"  {same} tables identical")

    pa, pb = a["redis"]["patterns"], b["redis"]["patterns"]
    for k in sorted(set(pa) | set(pb)):
        if pa.get(k) == pb.get(k):
            continue
        line = f"redis {k}: base {pa.get(k, 0)} keys, branch {pb.get(k, 0)}"
        (expected if any(p in k for p in NEW_KEY_PREFIXES) and not pa.get(k) else unexpected).append(line)
    da, db = a["redis"]["driver_pool_data"], b["redis"]["driver_pool_data"]
    for k in sorted(set(da) & set(db)):
        if not isinstance(da[k], dict) or not isinstance(db[k], dict):
            continue
        fields = {f for f in set(da[k]) | set(db[k]) if mask(da[k].get(f)) != mask(db[k].get(f))
                  and not VOLATILE_COLUMN.search(f) and f not in ("lastUpdatedAt",)}
        if fields:
            line = f"redis {k}: fields differ: " + ", ".join(f"{f}: {da[k].get(f)!r} -> {db[k].get(f)!r}" for f in sorted(fields))
            (expected if fields <= EXPECTED_DPD_FIELDS else unexpected).append(line)

    ka, kb = a["kafka"], b["kafka"]
    for t in sorted(set(ka) | set(kb)):
        if t in NEW_TOPICS:
            if t not in ka:
                expected.append(f"kafka {t}: new on the branch, {kb[t]['messages']} messages")
            continue
        xa, xb = ka.get(t, {"messages": 0, "shapes": {}}), kb.get(t, {"messages": 0, "shapes": {}})
        if xa["messages"] != xb["messages"]:
            unexpected.append(f"kafka {t}: base {xa['messages']} messages, branch {xb['messages']}")
        for s in set(xa["shapes"]) ^ set(xb["shapes"]):
            unexpected.append(f"kafka {t}: shape only in {'base' if s in xa['shapes'] else 'branch'}: {s[:300]}")

    print("\nexpected differences (what the branch adds)")
    for line in expected or ["  none"]:
        print(f"  {line}")
    print("\nunexpected differences")
    for line in unexpected or ["  none"]:
        print(f"  {line}")
    print(f"\n{'SAME' if not unexpected else f'{sum(1 for u in unexpected if not u.startswith(chr(32)))} DIFFERENCES'}"
          " (counts and masked rows; a flow run twice can differ in timing-dependent rows, so read each one)")
    return 1 if unexpected else 0


# ─── repeated rides ───────────────────────────────────────────────────────────

def cmd_ride_stats(args) -> int:
    pre = load_pre(args.pre)
    conn = pg(args)
    since = datetime.fromtimestamp(pre["started_ms"] / 1000, UTC)

    def count(table):
        return conn.execute(f"SELECT count(*) AS n FROM atlas_driver_offer_bpp.{table} WHERE created_at >= %s",
                            (since,)).fetchone()["n"]
    ends = end_offsets(args.bootstrap)
    batches = sum(e - pre["offsets"].get("search-try-batch", {}).get(p, 0) for p, e in ends.get("search-try-batch", {}).items())
    pool = redis_snapshot(args)["driver_pool_data"]
    vehicles = {r["d"]: r["reg"] for r in conn.execute(
        "SELECT trim(driver_id) AS d, registration_no AS reg FROM atlas_driver_offer_bpp.vehicle WHERE driver_id = ANY(%s)",
        (list(pool),))}
    held = [(d, v.get("registrationNo"), vehicles[d]) for d, v in pool.items() if isinstance(v, dict) and d in vehicles]
    line = {"ride": args.ride, "flow_rc": args.flow_rc, "popups": count("search_request_for_driver"),
            "jobs": count("scheduler_job"), "rides": count("ride"), "batches": batches,
            "pool_data_vehicle_ok": sum(1 for _, a, b in held if a == b),
            "pool_data_vehicle_wrong": sum(1 for _, a, b in held if a != b)}
    with open(args.out, "a") as f:
        f.write(json.dumps(line) + "\n")
    print(f"  ride {args.ride}: {line}")
    return 0


def cmd_ride_report(args) -> int:
    sides = {n: [json.loads(x) for x in open(p)] for n, p in (("base", args.base), ("branch", args.branch))}
    print(f"{'':32}{'base':>12}{'branch':>12}")

    def row(label, f):
        print(f"{label:32}{str(f(sides['base'])):>12}{str(f(sides['branch'])):>12}")
    row("rides attempted", len)
    row("flows passed", lambda r: sum(1 for x in r if x["flow_rc"] == 0))
    for c in ("popups", "jobs", "batches", "rides"):
        row(f"{c} total", lambda r, c=c: sum(x[c] for x in r))
        row(f"{c} per ride (median)", lambda r, c=c: sorted(x[c] for x in r)[len(r) // 2] if r else "-")
        row(f"{c} per ride (min-max)", lambda r, c=c: f"{min(x[c] for x in r)}-{max(x[c] for x in r)}" if r else "-")
    row("pool data vehicle right", lambda r: sum(x["pool_data_vehicle_ok"] for x in r))
    row("pool data vehicle wrong", lambda r: sum(x["pool_data_vehicle_wrong"] for x in r))
    return 0


# ─── truth ───────────────────────────────────────────────────────────────────

COLUMNS = ["popups_sent", "popups_accepted", "popups_rejected", "popups_pulled", "total_bookings", "rides_started",
           "finished_rides", "cancelled_rides", "rides_cancelled_by_driver", "rides_cancelled_by_user",
           "auto_accept_bookings", "auto_accept_finished_rides", "auto_accept_cancelled_rides",
           "rides_with_rating", "rating_sum", "driver_addition", "customer_tip", "gross_earnings"]
MONEY = {"driver_addition", "customer_tip", "gross_earnings"}


def expected_from_postgres(args, since_ms: int) -> dict[str, Counter]:
    conn = pg(args)
    conn.execute("SET search_path TO atlas_driver_offer_bpp")
    since = datetime.fromtimestamp(since_ms / 1000, UTC)
    exp: dict[str, Counter] = defaultdict(Counter)
    for s in conn.execute("SELECT trim(driver_id) AS d, response FROM search_request_for_driver "
                          "WHERE created_at >= %s AND driver_id <> 'drivers-not-found'", (since,)):
        e = exp[s["d"]]
        e["popups_sent"] += 1
        e["popups_accepted"] += s["response"] == "Accept"
        e["popups_rejected"] += s["response"] == "Reject"
        e["popups_pulled"] += s["response"] == "Pulled"
    for r in conn.execute("""
            SELECT trim(r.driver_id) AS d, r.status, coalesce(r.fare_amount, r.fare) AS fare, r.trip_start_time, b.is_auto_accepted,
                   coalesce(fp.driver_selected_fare_amount, fp.driver_selected_fare) AS driver_selected_fare,
                   coalesce(fp.customer_extra_fee_amount, fp.customer_extra_fee) AS customer_extra_fee, bcr.source
            FROM ride r JOIN booking b ON b.id = r.booking_id
            LEFT JOIN fare_parameters fp ON fp.id = b.fare_parameters_id
            LEFT JOIN LATERAL (SELECT source FROM booking_cancellation_reason x WHERE x.booking_id = b.id LIMIT 1) bcr ON true
            WHERE r.created_at >= %s""", (since,)):
        e = exp[r["d"]]
        auto = r["is_auto_accepted"] is True
        done, cancelled = r["status"] == "COMPLETED", r["status"] == "CANCELLED"
        e["total_bookings"] += 1
        e["auto_accept_bookings"] += auto
        e["rides_started"] += r["trip_start_time"] is not None
        e["finished_rides"] += done
        e["auto_accept_finished_rides"] += auto and done
        e["cancelled_rides"] += cancelled
        e["auto_accept_cancelled_rides"] += auto and cancelled
        e["rides_cancelled_by_driver"] += cancelled and r["source"] == "ByDriver"
        e["rides_cancelled_by_user"] += cancelled and r["source"] == "ByUser"
        e["driver_addition"] += Decimal(r["driver_selected_fare"] or 0)
        e["customer_tip"] += Decimal(r["customer_extra_fee"] or 0)
        e["gross_earnings"] += Decimal(r["fare"] or 0) if done else 0
    for x in conn.execute("SELECT trim(driver_id) AS d, rating_value FROM rating WHERE created_at >= %s", (since,)):
        exp[x["d"]]["rides_with_rating"] += 1
        exp[x["d"]]["rating_sum"] += x["rating_value"]
    return exp


def online_from_pings(args, pre: dict, end_ms: int) -> dict[str, tuple[set, int]]:
    """Per driver: the buckets with a non-OFFLINE ping, and the number of such pings."""
    out: dict[str, tuple[set, int]] = defaultdict(lambda: (set(), 0))
    ends = end_offsets(args.bootstrap).get("location-updates", {})
    for key, v in read_range(args.bootstrap, "location-updates", pre["offsets"].get("location-updates", {}), ends):
        try:
            lu = json.loads(v)
        except (json.JSONDecodeError, TypeError):
            continue
        if not key or lu.get("mode") == "OFFLINE":
            continue
        st = datetime.fromisoformat((lu.get("st") or lu["ts"]).replace("Z", "+00:00")).timestamp()
        if st * 1000 < pre["started_ms"] - 60_000 or st * 1000 > end_ms + 300_000:
            continue
        buckets, n = out[key]
        buckets.add(int(st) - int(st) % BUCKET_S)
        out[key] = (buckets, n + 1)
    return out


def cmd_truth(args) -> int:
    pre = load_pre(args.pre)
    end_ms = int(time.time() * 1000)
    r = redis.Redis(host=args.redis_host, port=args.redis_port, db=args.state_redis_db, decode_responses=True)
    deadline = time.time() + args.flush_timeout
    while int(r.get("sessionizer:watermark") or 0) <= end_ms:
        if time.time() > deadline:
            print("FAIL  the sessionizer did not flush past the run in time; is kafka-sessionizer-exe running?")
            return 1
        print(f"\r  waiting for the sessionizer flush ... {int(deadline - time.time())}s left ", end="", flush=True)
        time.sleep(5)
    print()
    exp = expected_from_postgres(args, pre["started_ms"])
    pings = online_from_pings(args, pre, end_ms)
    drivers = sorted(set(exp) | set(pings))
    if not drivers:
        print("FAIL  no pop-ups, rides or pings in the run window")
        return 1
    if not any(e["total_bookings"] for e in exp.values()):
        print("FAIL  no ride in the run window, so only pings could be checked (did the flow reach a booking?)")
        return 1
    ids = ",".join("'" + d.replace("'", "\\'") + "'" for d in drivers)
    since_bucket = (pre["started_ms"] // 1000 - 60) // BUCKET_S * BUCKET_S
    def stream():
        return {x["driver_id"]: x for x in ch(args.clickhouse, f"""
        SELECT driver_id, uniqExactIf(bucket, t.online = 1) AS online_buckets, sum(t.pings) AS pings,
               {", ".join(f"sum({c}) AS {c}" for c in COLUMNS)}
        FROM atlas_kafka.driver_analytics_5m AS t
        WHERE bucket >= toDateTime({since_bucket}) AND driver_id IN ({ids}) GROUP BY driver_id""")}

    def compare(d, g):
        rows = []
        for c in COLUMNS:
            want, have = exp[d][c], Decimal(str(g.get(c, 0)))
            ok = abs(Decimal(str(want)) - have) < Decimal("0.01") if c in MONEY else int(want) == int(have)
            if not ok or want:
                rows.append((ok, c, want, have))
        buckets, n = pings.get(d, (set(), 0))
        rows.append((len(buckets) == int(g.get("online_buckets", 0)), "online_buckets", len(buckets), g.get("online_buckets", 0)))
        rows.append((n == int(g.get("pings", 0)), "pings", n, g.get("pings", 0)))
        return rows

    # The flush delivers to the driver-analytics-5m topic; ClickHouse's Kafka engine
    # inserts those rows a few seconds later, so allow it time before calling a mismatch.
    deadline = time.time() + CLICKHOUSE_WAIT_S
    got = stream()
    while time.time() < deadline and not all(ok for d in drivers for ok, *_ in compare(d, got.get(d, {}))):
        time.sleep(5)
        got = stream()

    failures = 0
    print(f"{len(drivers)} drivers; per driver: stream (driver_analytics_5m) vs source (Postgres, location-updates)")
    for d in drivers:
        rows = compare(d, got.get(d, {}))
        bad = [x for x in rows if not x[0]]
        failures += len(bad)
        print(f"  {'PASS' if not bad else 'FAIL'}  {d}: " + ", ".join(f"{c}={want}" + ("" if ok else f" (stream {have})")
                                                          for ok, c, want, have in rows if want or not ok))
    print(f"\n{'ALL MATCH' if not failures else f'{failures} MISMATCHES'}")
    return 1 if failures else 0


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--bootstrap", default="127.0.0.1:29092")
    ap.add_argument("--clickhouse", default="http://localhost:8123")
    ap.add_argument("--redis-host", default="localhost")
    ap.add_argument("--redis-port", type=int, default=6379)
    ap.add_argument("--redis-cluster-port", type=int, default=30001)
    ap.add_argument("--state-redis-db", type=int, default=2)
    ap.add_argument("--pg-host", default="localhost")
    ap.add_argument("--pg-port", type=int, default=5434)
    ap.add_argument("--pg-db", default="atlas_dev")
    ap.add_argument("--pg-user", default="atlas_superuser")
    ap.add_argument("--context-api", default="http://localhost:7082")
    sub = ap.add_subparsers(dest="cmd", required=True)
    s = sub.add_parser("prepare")
    s.add_argument("--sync-metadata", default="data/config-sync/metadata.json")
    s = sub.add_parser("mark")
    s.add_argument("--out", required=True)
    s.add_argument("--sessionizer", action="store_true", help="also create the branch's topics and check its table")
    s = sub.add_parser("snapshot")
    s.add_argument("--pre", required=True)
    s.add_argument("--out", required=True)
    s = sub.add_parser("diff")
    s.add_argument("base")
    s.add_argument("branch")
    s = sub.add_parser("ride-stats")
    s.add_argument("--pre", required=True)
    s.add_argument("--ride", type=int, required=True)
    s.add_argument("--flow-rc", type=int, required=True)
    s.add_argument("--out", required=True)
    s = sub.add_parser("ride-report")
    s.add_argument("base")
    s.add_argument("branch")
    s = sub.add_parser("truth")
    s.add_argument("--pre", required=True)
    s.add_argument("--flush-timeout", type=int, default=900)
    args = ap.parse_args()
    sys.exit({"prepare": cmd_prepare, "ride-stats": cmd_ride_stats, "ride-report": cmd_ride_report, "mark": cmd_mark, "snapshot": cmd_snapshot, "diff": cmd_diff, "truth": cmd_truth}[args.cmd](args))


if __name__ == "__main__":
    main()
